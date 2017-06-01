package tcp

import (
	"bufio"
	"errors"
	"net"
	"sync"
)

func createTCPTransport(lAddr string) (*TCPTransport, error) {
	state := &TransPortValid{ValidTransportState{
		_localEndPoints: make(map[EndPointId]*LocalEndPoint, 10),
		_nextEndPointId: 0,
	}}

	tp := &TCPTransport{
		transportAddr: TransportAddr(lAddr),
		transportState: struct {
			value TransportState
			lock  sync.Mutex
		}{
			value: state,
		},
	}

	err := forkServer(lAddr, func(conn net.Conn) {
		tp.handleConnectionRequest(conn)
	})
	if err != nil {
		return nil, err
	}
	return tp, nil
}

//------------------------------------------------------------------------------
// Incoming requests                                                          --
//------------------------------------------------------------------------------
func (tp *TCPTransport) handleConnectionRequest(conn net.Conn) {
	// get endpoint id
	ourEndPointID, err := ReadUint32(conn)
	if err != nil {
		conn.Close()
		return
	}
	// get remote endpoint
	bs, err := ReadWithLen(conn, tcpMaxAddressLength)
	if err != nil {
		conn.Close()
		return
	}
	theirAddress := decodeEndPointAddress(bs)
	println(theirAddress)
	if !checkPeerHost(conn, theirAddress) {
		writeConnectionRequestResponse(ConnectionRequestHostMismatch{}, conn)
		conn.Close()
		return
	}

	// ourAddress := tp.transportAddr.encodeEndPointAddress(ourEndPointID)

	// dispatch to endpoint
	// we need this clojure to avoid dead lock!!!
	ep, err := func() (*LocalEndPoint, error) {
		tp.transportState.lock.Lock()
		defer tp.transportState.lock.Unlock()

		switch ts := tp.transportState.value.(type) {
		case *TransPortValid:
			vst := &ts._1

			if ep, ok := vst._localEndPoints[EndPointId(ourEndPointID)]; ok {
				return ep, nil
			}

			//connection to unknown endpoint
			writeConnectionRequestResponse(ConnectionRequestInvalid{}, conn)
			return nil, errors.New("unknown endpoint")

		default: // TransportClosed
			return nil, errors.New("Transport closed")
		}
	}()

	if err != nil {
		println(err)
		conn.Close()
		return
	}

	ep.handleConnectionRequest(theirAddress, conn)
}

// endpoint handle incoming connection
func (ourEndPoint *LocalEndPoint) handleConnectionRequest(theirAddress *EndPointAddress, conn net.Conn) {
	// This runs in a thread that will never be killed
	theirEndPoint, isNew, err := ourEndPoint.findRemoteEndPoint(*theirAddress, RequestedByThem{})
	if err != nil {
		println(err)
		// writeConnectionRequestResponse(ConnectionRequestCrossed{}, conn)
		conn.Close()
		return
	}

	if !isNew {
		writeConnectionRequestResponse(ConnectionRequestCrossed{}, conn)
		conn.Close()
		return
	}

	vst := &RemoteEndPointValid{ValidRemoteEndPointState{
		remoteConn:           conn,
		_remoteNextConnOutId: firstNonReservedLightweightConnectionId,
		_remoteIncoming:      make(map[LightweightConnectionId]struct{}, 100),
	}}
	writeConnectionRequestResponse(ConnectionRequestAccepted{}, conn)
	ourEndPoint.resolveInit(theirEndPoint, vst)

	handleIncomingMessages(ourEndPoint, theirEndPoint)
}

// | Handle requests from a remote endpoint.
//
// Returns only if the remote party closes the socket or if an error occurs.
// This runs in a thread that will never be killed.
func handleIncomingMessages(ourEndPoint *LocalEndPoint, theirEndPoint *RemoteEndPoint) {
	theirAddress := theirEndPoint.remoteAddress

	// Deal with a premature exit
	prematureExit := func(err error) {
		theirEndPoint.remoteState.lock.Lock()
		defer theirEndPoint.remoteState.lock.Unlock()

		switch theirEndPoint.remoteState.value.(type) {
		case *RemoteEndPointInvalid, *RemoteEndPointInit, RemoteEndPointClosed:
			ourEndPoint.relyViolation("handleIncomingMessages:prematureExi")
		case *RemoteEndPointValid:
			// vst := &st._1
			code := EventConnectionLost{theirAddress}
			ourEndPoint.localQueue <- &ErrorEvent{err, code.String()}
			theirEndPoint.remoteState.value = &RemoteEndPointFailed{err}
		case *RemoteEndPointClosing:
			theirEndPoint.remoteState.value = &RemoteEndPointFailed{err}
		case *RemoteEndPointFailed:
			ourEndPoint.localState.lock.Lock()
			defer ourEndPoint.localState.lock.Unlock()

			if _, ok := ourEndPoint.localState.value.(*LocalEndPointValid); ok {
				code := EventConnectionLost{theirAddress}
				ourEndPoint.localQueue <- &ErrorEvent{err, code.String()}
			}
		}
	}

	sock, err := func() (net.Conn, error) {
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInvalid:
			ourEndPoint.relyViolation("handleIncomingMessages (invalid)")
		case *RemoteEndPointInit:
			ourEndPoint.relyViolation("handleIncomingMessages (init)")
		case *RemoteEndPointValid:
			return st._1.remoteConn, nil
		case *RemoteEndPointClosing:
			return st._2.remoteConn, nil
		case *RemoteEndPointFailed:
			return nil, st._1
		default: //RemoteEndPointClosed
			return nil, errors.New("handleIncomingMessages (already closed)")
		}
		return nil, errors.New("handleIncomingMessages (failed)")
	}()

	if err != nil {
		prematureExit(err)
		return
	}

	// Construct a connection ID
	connId := func(lcid LightweightConnectionId) ConnectionId {
		return createConnectionId(theirEndPoint.remoteId, lcid)
	}

	// The ID of the last connection _we_ created (or 0 for none)
	lastSentId := func(vst *ValidRemoteEndPointState) LightweightConnectionId {
		if vst._remoteNextConnOutId == firstNonReservedLightweightConnectionId {
			return 0
		} else {
			return vst._remoteNextConnOutId - 1
		}
	}

	enqueue := func(e Event) {
		ourEndPoint.localQueue <- e
	}

	// Read a message and output it on the endPoint's channel. By rights we
	// should verify that the connection ID is valid, but this is unnecessary
	// overhead
	readMessage := func(sock net.Conn, lcid LightweightConnectionId) error {
		msg, err := ReadWithLen(sock, tcpMaxReceiveLength)
		if err != nil {
			return err
		}
		event := &Received{connId(lcid), msg}
		enqueue(event)
		return nil
	}

	// Create a new connection
	createdNewConnection := func(lcid LightweightConnectionId) error {
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInvalid:
			ourEndPoint.relyViolation("handleIncomingMessages:createNewConnection (invalid)")
		case *RemoteEndPointInit:
			ourEndPoint.relyViolation("handleIncomingMessages:createNewConnection (init)")
		case *RemoteEndPointValid:
			st._1._remoteLastIncoming = lcid
			st._1._remoteIncoming[lcid] = struct{}{}
		case *RemoteEndPointClosing:
			vst := &st._2
			vst._remoteLastIncoming = lcid
			vst._remoteIncoming[lcid] = struct{}{}
			theirState.value = &RemoteEndPointValid{*vst}
		case *RemoteEndPointFailed:
			return st._1
		case RemoteEndPointClosed:
			ourEndPoint.relyViolation("createNewConnection (closed)")
		}

		enqueue(&ConnectionOpened{connId(lcid), theirAddress})
		return nil
	}

	// Close a connection
	// It is important that we verify that the connection is in fact open,
	// because otherwise we should not decrement the reference count
	closeConnection := func(lcid LightweightConnectionId) error {
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInvalid:
			ourEndPoint.relyViolation("handleIncomingMessages:closeConnection (invalid)")
		case *RemoteEndPointInit:
			ourEndPoint.relyViolation("handleIncomingMessages:closeConnection (init)")
		case *RemoteEndPointValid:
			vst := &st._1
			if _, ok := vst._remoteIncoming[lcid]; ok {
				delete(vst._remoteIncoming, lcid)
			} else {
				return errors.New("Invalid CloseConnection")
			}
		case *RemoteEndPointClosing:
			return errors.New("Invalid CloseConnection request")
		case *RemoteEndPointFailed:
			return st._1
		case RemoteEndPointClosed:
			ourEndPoint.relyViolation("closeConnection (closed)")
		}

		enqueue(&ConnectionClosed{connId(lcid)})
		return nil
	}

	// Close the socket (if we don't have any outgoing connections)
	closeSocket := func(sock net.Conn, lastReceivedId LightweightConnectionId) (bool, error) {
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInvalid:
			ourEndPoint.relyViolation("handleIncomingMessages:closeSocket (invalid)")
		case *RemoteEndPointInit:
			ourEndPoint.relyViolation("handleIncomingMessages:closeSocket (init)")
		case *RemoteEndPointValid:
			vst := &st._1
			for k := range vst._remoteIncoming {
				enqueue(&ConnectionClosed{connId(k)})
			}
			vst._remoteIncoming = make(map[LightweightConnectionId]struct{})
			if uint32(vst._remoteOutgoing) > 0 || uint32(lastReceivedId) != uint32(lastSentId(vst)) {
				return false, nil
			} else {
				ourEndPoint.removeRemoteEndPoint(theirEndPoint)
				//TODO: schedule 1258
				theirState.value = RemoteEndPointClosed{}
				return true, nil
			}
		case *RemoteEndPointClosing:
			vst := &st._2
			if lastReceivedId != lastSentId(vst) {
				return false, nil
			}
			if vst._remoteOutgoing > 0 {
				code := EventConnectionLost{theirAddress}
				msg := "socket closed prematurely by peer"
				enqueue(&ErrorEvent{errors.New(msg), code.String()})
			}
			ourEndPoint.removeRemoteEndPoint(theirEndPoint)
			theirState.value = RemoteEndPointClosed{}
			return true, nil
		case *RemoteEndPointFailed:
			return false, st._1
		case RemoteEndPointClosed:
			ourEndPoint.relyViolation("handleIncomingMessages:closeSocket (closed)")
		}

		return false, nil
	}

	closeRemoteEndPoint := func(vst *ValidRemoteEndPointState) {
		// close incoming connections
		for k := range vst._remoteIncoming {
			enqueue(&ConnectionClosed{connId(k)})
		}
		// report the endpoint as gone if we have any outgoing connections
		if vst._remoteOutgoing > 0 {
			code := &EventConnectionLost{theirAddress}
			enqueue(&ErrorEvent{errors.New(code.String()), "The remote endpoint was closed."})
		}
	}

	closeEndPoint := func() {
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointValid:
			vst := &st._1
			closeRemoteEndPoint(vst)
			theirState.value = RemoteEndPointClosed{}
		case *RemoteEndPointClosing:
			vst := &st._2
			closeRemoteEndPoint(vst)
			theirState.value = RemoteEndPointClosed{}
		}
	}

	// Dispatch
	//
	// If a recv throws an exception this will be caught top-level and
	// 'prematureExit' will be invoked. The same will happen if the remote
	// endpoint is put into a Closed (or Closing) state by a concurrent thread
	// (because a 'send' failed) -- the individual handlers below will throw a
	// user exception which is then caught and handled the same way as an
	// exception thrown by 'recv'.

	//TODO: HERE
	err = func() error {
		for {
			lcid, err := ReadUint32(sock)
			if err != nil {
				return err
			}

			if uint32(lcid) >= uint32(firstNonReservedLightweightConnectionId) {
				readMessage(sock, LightweightConnectionId(lcid))
				continue
			}

			switch decodeControlHeader(uint8(uint32(lcid))).(type) {
			case CreateNewConnection:
				cid, err := ReadUint32(sock)
				if err != nil {
					return err
				}
				err = createdNewConnection(LightweightConnectionId(cid))
				if err != nil {
					return err
				}
				continue
			case CloseConnection:
				cid, err := ReadUint32(sock)
				if err != nil {
					return err
				}
				err = closeConnection(LightweightConnectionId(cid))
				if err != nil {
					return err
				}
				continue
			case CloseSocket:
				i, err := ReadUint32(sock)
				if err != nil {
					return err
				}
				didClose, err := closeSocket(sock, LightweightConnectionId(i))
				if err != nil {
					return err
				}
				if !didClose {
					continue
				}
			case CloseEndPoint:
				ourEndPoint.removeRemoteEndPoint(theirEndPoint)
				closeEndPoint()
			default:
				err := errors.New("Invalid control request")
				return err
			}
		}
	}()

	if err != nil {
		prematureExit(err)
		return
	}
}

// | Create a connection to a remote endpoint
//
// If the remote endpoint is in 'RemoteEndPointClosing' state then we will
// block until that is resolved.
//
// May throw a TransportError ConnectErrorCode exception.
func (ourEndPoint *LocalEndPoint) createConnectionTo(theirAddress EndPointAddress) (*RemoteEndPoint, LightweightConnectionId, error) {
	for {

		theirEndPoint, isNew, err := ourEndPoint.findRemoteEndPoint(theirAddress, RequestedByUs{})
		if err != nil {
			return nil, firstNonReservedLightweightConnectionId, err
		}

		if isNew {
			ourEndPoint.setupRemoteEndPoint(theirEndPoint)
			// continue
		}
		// 'findRemoteEndPoint' will have increased 'remoteOutgoing'
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointValid:
			vst := &st._1
			connId := vst._remoteNextConnOutId
			//TODO schedule
			vst._remoteNextConnOutId = connId + 1
			return theirEndPoint, connId, nil
		case *RemoteEndPointInvalid:
			return nil, 0, errors.New(st._1.String())
		case *RemoteEndPointFailed:
			return nil, 0, st._1
		default:
			ourEndPoint.relyViolation("createConnectionTo")
		}
	}
}

// | Set up a remote endpoint
func (ourEndPoint *LocalEndPoint) setupRemoteEndPoint(theirEndPoint *RemoteEndPoint) (ConnectionRequestResponse, error) {
	ourAddress := ourEndPoint.localAddress
	theirAddress := theirEndPoint.remoteAddress

	sock, rsp, err := socketToEndPoint(ourAddress, theirAddress)
	if err != nil {
		ourEndPoint.resolveInit(theirEndPoint, &RemoteEndPointInvalid{nil, err.Error()})
		return nil, err
	}

	afterAccept := func() {
		go func() {
			defer sock.Close()
			handleIncomingMessages(ourEndPoint, theirEndPoint)
		}()
	}

	theirState := &theirEndPoint.remoteState

	switch rsp.(type) {
	case ConnectionRequestAccepted:
		st := &RemoteEndPointValid{ValidRemoteEndPointState{
			remoteConn:           sock,
			_remoteIncoming:      make(map[LightweightConnectionId]struct{}),
			_remoteNextConnOutId: firstNonReservedLightweightConnectionId,
		}}
		ourEndPoint.resolveInit(theirEndPoint, st)
		afterAccept()
	case ConnectionRequestInvalid:
		defer sock.Close()

		st := &RemoteEndPointInvalid{ConnectNotFound{}, "setupRemoteEndPoint: Invalid endpoint"}
		ourEndPoint.resolveInit(theirEndPoint, st)
	case ConnectionRequestCrossed:
		defer sock.Close()
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInit:
			// putMVar crossed _
		case *RemoteEndPointFailed:
			return rsp, st._1
		default:
			ourEndPoint.relyViolation("setupRemoteEndPoint: Crossed")
		}
	case ConnectionRequestHostMismatch:
		defer sock.Close()

		msg := "setupRemoteEndPoint: Host mismatch "
		st := &RemoteEndPointInvalid{ConnectFailed{}, msg}
		ourEndPoint.resolveInit(theirEndPoint, st)
	}

	return rsp, nil
}

// | Find a remote endpoint. If the remote endpoint does not yet exist we
// create it in Init state. Returns if the endpoint was new, or 'Nothing' if
// it times out.
func (ourEndPoint *LocalEndPoint) findRemoteEndPoint(theirAddress EndPointAddress, findOrigin RequestedBy) (*RemoteEndPoint, bool, error) {
	ourState := &ourEndPoint.localState

	ourState.lock.Lock()
	defer ourState.lock.Unlock()

	switch state := ourState.value.(type) {
	case *LocalEndPointValid:
		vst := &state._1
		if theirEndPoint, ok := vst._localConnections[theirAddress]; ok {
			theirState := &theirEndPoint.remoteState
			//TODO:
			switch p := theirState.value.(type) {
			case *RemoteEndPointInvalid:
				return nil, false, errors.New(p._2)
			case *RemoteEndPointInit:
				return nil, false, errors.New("Already connected")
			case *RemoteEndPointValid:
				return theirEndPoint, false, nil
			case *RemoteEndPointFailed:
				return nil, false, p._1
			default:
				return nil, false, errors.New("unknown remote state")
			}

		} else {
			//TODO:
			theirState := &RemoteEndPointInit{newNotifier(), newNotifier(), findOrigin}
			theirEndPoint = &RemoteEndPoint{
				remoteAddress: theirAddress,
				remoteState: struct {
					value RemoteState
					lock  sync.Mutex
				}{value: theirState},
				remoteId: vst._nextConnInId,
			}
			vst._localConnections[theirAddress] = theirEndPoint
			vst._nextConnInId += 1
			return theirEndPoint, true, nil
		}
	default: // LocalEndPointClosed
		return nil, false, errors.New("Local endpoint closed")
	}
}

// Resolve an endpoint currently in 'Init' state
func (ourEndPoint *LocalEndPoint) resolveInit(theirEndPoint *RemoteEndPoint, newState RemoteState) error {
	theirEndPoint.remoteState.lock.Lock()
	defer theirEndPoint.remoteState.lock.Unlock()

	switch p := theirEndPoint.remoteState.value.(type) {
	case *RemoteEndPointInit:
		switch newState.(type) {
		case RemoteEndPointClosed:
			ourEndPoint.removeRemoteEndPoint(theirEndPoint)
		default:
			theirEndPoint.remoteState.value = newState
		}
		return nil
	case *RemoteEndPointFailed:
		return p._1
	default:
		ourEndPoint.relyViolation("resolveInit")
	}
	theirEndPoint.remoteState.value = newState
	return nil
}

// | Remove reference to a remote endpoint from a local endpoint
//
// If the local endpoint is closed, do nothing
func (ourEndPoint *LocalEndPoint) removeRemoteEndPoint(theirEndPoint *RemoteEndPoint) {
	ourEndPoint.localState.lock.Lock()
	defer ourEndPoint.localState.lock.Unlock()

	switch ourState := ourEndPoint.localState.value.(type) {
	case *LocalEndPointValid:
		vst := &ourState._1
		remoteEndPoint, ok := vst._localConnections[theirEndPoint.remoteAddress]
		if ok {
			if remoteEndPoint.remoteId == theirEndPoint.remoteId {
				delete(vst._localConnections, theirEndPoint.remoteAddress)
			}
		}
	case LocalEndPointClosed:
		return
	}
}

// | Create a new local endpoint
//
// May throw a TransportError NewEndPointErrorCode exception if the transport
// is closed.
func (tp *TCPTransport) createLocalEndPoint(epid EndPointId) (*LocalEndPoint, NewEndPointErrorCode) {
	tp.transportState.lock.Lock()
	defer tp.transportState.lock.Unlock()

	switch ts := tp.transportState.value.(type) {
	case *TransPortValid:
		vst := &ts._1
		endpoints := vst._localEndPoints

		if _, ok := endpoints[epid]; ok {
			// println("endpoint already exist!")
			return nil, NewEndPointFailed{}
		}

		st := &LocalEndPointValid{ValidLocalEndPointState{
			_localNextConnOutId: firstNonReservedLightweightConnectionId,
			_localConnections:   make(map[EndPointAddress]*RemoteEndPoint),
			_nextConnInId:       firstNonReservedHeavyweightConnectionId,
		}}

		endpoints[epid] = &LocalEndPoint{
			localAddress: EndPointAddress{tp.transportAddr, epid},
			localState: struct {
				value LocalEndPointState
				lock  sync.Mutex
			}{value: st},
			localQueue: make(chan Event, 10),
			closeLocalEndPoint: func() error {
				return tp.closeLocalEndPoint(epid)
			},
		}
		return endpoints[epid], nil
	case TransportClosed:
		return nil, NewEndPointFailed{}
	default:
		return nil, NewEndPointFailed{}
	}
}

func (tp *TCPTransport) closeLocalEndPoint(epid EndPointId) error {
	tp.transportState.lock.Lock()
	defer tp.transportState.lock.Unlock()

	endpoints := tp.transportState.value.(*TransPortValid)._1._localEndPoints
	if _, ok := endpoints[epid]; ok {
		// close(ep.connections)
		delete(endpoints, epid)
		return nil
	}
	return errors.New("endpoinyt not exist!")
}

func (ep *LocalEndPoint) connect(remoteEP EndPointAddress) (net.Conn, error) {
	conn, err := net.Dial("tcp", string(remoteEP.TransportAddr))
	if err != nil {
		return nil, err
	}

	// send remote endpoint id and out endpoint address
	bw := bufio.NewWriterSize(conn, 256)
	WriteUint32(uint32(remoteEP.epid), bw)
	myaddr := []byte(ep.localAddress.String())
	WriteUint32(uint32(len(myaddr)), bw)
	bw.Write(myaddr)
	bw.Flush()

	// r, err := ReadUint32(conn)
	// if err != nil {
	// 	defer conn.Close()
	// 	return nil, err
	// }
	// resp := ConnectionRequestResponse(r)
	// if resp != ConnectionRequestAccepted {
	// 	defer conn.Close()
	// 	return nil, errors.New(resp.String())
	// }
	return conn, nil
}

func createConnectionId(hcid HeavyweightConnectionId, lcid LightweightConnectionId) ConnectionId {
	return ConnectionId(uint64(uint32(hcid))<<32 | uint64(uint32(lcid)))
}

//------------------------------------------------------------------------------
// Constants                                                                  --
//------------------------------------------------------------------------------

const (
	// | We reserve a bunch of connection IDs for control messages
	firstNonReservedLightweightConnectionId = LightweightConnectionId(1024)

	// | We reserve some connection IDs for special heavyweight connections
	firstNonReservedHeavyweightConnectionId = HeavyweightConnectionId(1)

	// | Maximum length (in bytes) for a peer's address.
	// If a peer attempts to send an address of length exceeding the limit,
	// the connection will be refused (socket will close).
	tcpMaxAddressLength = 1000

	// | Maximum length (in bytes) to receive from a peer.
	// If a peer attempts to send data on a lightweight connection exceeding
	// the limit, the heavyweight connection which carries that lightweight
	// connection will go down. The peer and the local node will get an
	// EventConnectionLost.
	tcpMaxReceiveLength = 4 * 1024 * 1024
)
