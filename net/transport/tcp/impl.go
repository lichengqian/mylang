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

	handleIncomingMessage(ourEndPoint, theirEndPoint)
}

// | Handle requests from a remote endpoint.
//
// Returns only if the remote party closes the socket or if an error occurs.
// This runs in a thread that will never be killed.
func handleIncomingMessage(ourEndPoint *LocalEndPoint, theirEndPoint *RemoteEndPoint) {
	theirAddress := theirEndPoint.remoteAddress

	sock, err := func() (net.Conn, error) {
		theirEndPoint.remoteState.lock.Lock()
		defer theirEndPoint.remoteState.lock.Unlock()

		switch st := theirEndPoint.remoteState.value.(type) {
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

	if err != nil {
		prematureExit(err)
		return
	}

	// Read a message and output it on the endPoint's channel. By rights we
	// should verify that the connection ID is valid, but this is unnecessary
	// overhead
	readMessage := func(sock net.Conn, lcid LightweightConnectionId) error {
		msg, err := ReadWithLen(sock, tcpMaxReceiveLength)
		if err != nil {
			return err
		}
		connId := createConnectionId(theirEndPoint.remoteId, lcid)
		event := &Received{connId, msg}
		ourEndPoint.localQueue <- event
		return nil
	}

	// Create a new connection
	createdNewConnection := func(lcid LightweightConnectionId) {

	}

	// Close a connection
	// It is important that we verify that the connection is in fact open,
	// because otherwise we should not decrement the reference count
	closeConnection := func(lcid LightweightConnectionId) {

	}

	// Close the socket (if we don't have any outgoing connections)
	closeSocket := func(sock net.Conn, lastReceivedId LightweightConnectionId) (bool, error) {
		return false, nil
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
	for {
		lcid, err := ReadUint32(sock)
		if err != nil {
			prematureExit(err)
			return
		}

		if uint32(lcid) >= uint32(firstNonReservedLightweightConnectionId) {
			readMessage(sock, LightweightConnectionId(lcid))
			continue
		}

		switch decodeControlHeader(lcid).(type) {
		case CreateNewConnection:
			cid, err := ReadUint32(sock)
			if err != nil {
				prematureExit(err)
				return
			}
			createdNewConnection(LightweightConnectionId(cid))
			continue
		case CloseConnection:
			cid, err := ReadUint32(sock)
			if err != nil {
				prematureExit(err)
				return
			}
			closeConnection(LightweightConnectionId(cid))
			continue
		case CloseSocket:
			i, err := ReadUint32(sock)
			if err != nil {
				prematureExit(err)
				return
			}
			didClose, err := closeSocket(sock, LightweightConnectionId(i))
			if err != nil {
				prematureExit(err)
				return
			}
			if !didClose {
				continue
			}
		case CloseEndPoint: //TODO: 1119
			continue
		default:
			err := errors.New("Invalid control request")
			prematureExit(err)
			return
		}
	}
}

// | Find a remote endpoint. If the remote endpoint does not yet exist we
// create it in Init state. Returns if the endpoint was new, or 'Nothing' if
// it times out.
func (ourEndPoint *LocalEndPoint) findRemoteEndPoint(theirAddress EndPointAddress, findOrigin RequestedBy) (*RemoteEndPoint, bool, error) {
	ourEndPoint.localState.lock.Lock()
	defer ourEndPoint.localState.lock.Unlock()

	switch state := ourEndPoint.localState.value.(type) {
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
			theirState := &RemoteEndPointInit{sync.Mutex{}, sync.Mutex{}, findOrigin}
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
