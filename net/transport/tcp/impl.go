package tcp

import (
	"errors"
	"fmt"
	"net"
	"sync"
)

func createTCPTransport(lAddr string) (*TCPTransport, error) {
	state := &TransPortValid{ValidTransportState{
		_localEndPoints: make(map[EndPointId]*LocalEndPoint),
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
// API functions                                                              --
//------------------------------------------------------------------------------

// | Connnect to an endpoint
func (ourEndPoint *LocalEndPoint) apiConnect(theirAddress EndPointAddress) (*LocalConnection, error) {
	//TODO: connect to self 756

	err := ourEndPoint.resetIfBroken(theirAddress)
	if err != nil {
		return nil, err
	}
	theirEndPoint, connId, err := ourEndPoint.createConnectionTo(theirAddress)
	if err != nil {
		return nil, err
	}

	if theirEndPoint == nil {
		panic("apiConnect failed!")
	}

	return &LocalConnection{
		close: func() error {
			return ourEndPoint.apiClose(theirEndPoint, connId)
		},
		send: func(msg []byte) (int, error) {
			return ourEndPoint.apiSend(theirEndPoint, connId, msg)
		},
	}, nil
}

// | Close a connection
func (ourEndPoint *LocalEndPoint) apiClose(theirEndPoint *RemoteEndPoint, connId LightweightConnectionId) error {
	fmt.Println("apiClose:", ourEndPoint.localAddress, "->", theirEndPoint.remoteAddress, connId)
	conn := func() net.Conn {
		theirState := theirEndPoint.remoteState

		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointValid:
			vst := &st._1
			vst._remoteOutgoing--
			fmt.Println("	remoteOutgoing--:", vst._remoteOutgoing)
			//sched action
			return vst.remoteConn
		default:
			return nil
		}
	}()

	if conn != nil {
		sendCloseConnection(uint32(connId), conn)
	}
	ourEndPoint.closeIfUnused(theirEndPoint)
	return nil
}

// | Send data across a connection
func (ourEndPoint *LocalEndPoint) apiSend(theirEndPoint *RemoteEndPoint, connId LightweightConnectionId, msg []byte) (int, error) {
	fmt.Println("apiSend", connId)
	action, err := func() (func(), error) {
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInvalid, *RemoteEndPointInit:
			ourEndPoint.relyViolation("apiSend")
		case *RemoteEndPointValid:
			vst := &st._1
			conn := vst.remoteConn
			return func() {
				connId.sendMsg(msg, conn)
			}, nil
		case *RemoteEndPointClosing, RemoteEndPointClosed:
			ourEndPoint.relyViolation("apiSend")
		case *RemoteEndPointFailed:
			return nil, st._1
		}
		return nil, errors.New("apiSend error")
	}()

	if err != nil {
		return 0, err
	}
	action()
	return len(msg), nil
}

//------------------------------------------------------------------------------
// Incoming requests                                                          --
//------------------------------------------------------------------------------

// | Handle a connection request (that is, a remote endpoint that is trying to
// establish a TCP connection with us)
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
	theirAddress, err := decodeEndPointAddress(bs)
	if err != nil {
		conn.Close()
		return
	}
	fmt.Println("handleConnectionRequest:", theirAddress, "->", ourEndPointID)
	if !checkPeerHost(conn, theirAddress) {
		writeConnectionRequestResponse(ConnectionRequestHostMismatch{}, conn)
		conn.Close()
		return
	}

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
	err := ourEndPoint.resetIfBroken(*theirAddress)
	if err != nil {
		fmt.Println(err)
		conn.Close()
		return
	}
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
		fmt.Println("in prematureExit:", err)
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch theirState.value.(type) {
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
		action := func() func() {
			theirState := &theirEndPoint.remoteState
			theirState.lock.Lock()
			defer theirState.lock.Unlock()

			fmt.Println("closing socket:", theirState.value.String())
			switch st := theirState.value.(type) {
			case *RemoteEndPointInvalid:
				ourEndPoint.relyViolation("handleIncomingMessages:closeSocket (invalid)")
			case *RemoteEndPointInit:
				ourEndPoint.relyViolation("handleIncomingMessages:closeSocket (init)")
			case *RemoteEndPointValid:
				vst := &st._1
				fmt.Println(vst)
				for k := range vst._remoteIncoming {
					enqueue(&ConnectionClosed{connId(k)})
				}
				if uint32(vst._remoteOutgoing) > 0 || uint32(lastReceivedId) != uint32(lastSentId(vst)) {
					fmt.Println("we still have connections, can not close socket", vst._remoteOutgoing)
					vst._remoteIncoming = make(map[LightweightConnectionId]struct{})
					return nil
				} else {
					ourEndPoint.removeRemoteEndPoint(theirEndPoint)
					theirState.value = RemoteEndPointClosed{}
					return func() {
						sendCloseSocket(uint32(vst._remoteLastIncoming), vst.remoteConn)
					}
				}
			case *RemoteEndPointClosing:
				vst := &st._2
				if lastReceivedId != lastSentId(vst) {
					return nil
				}
				if vst._remoteOutgoing > 0 {
					code := EventConnectionLost{theirAddress}
					msg := "socket closed prematurely by peer"
					enqueue(&ErrorEvent{errors.New(msg), code.String()})
				}
				ourEndPoint.removeRemoteEndPoint(theirEndPoint)
				theirState.value = RemoteEndPointClosed{}
				return func() {}
			case *RemoteEndPointFailed:
				fmt.Println("closeSocket:", st._1)
				return nil
			case RemoteEndPointClosed:
				ourEndPoint.relyViolation("handleIncomingMessages:closeSocket (closed)")
			}
			return nil
		}()

		if action != nil {
			action()
			return true, nil
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

	err = func() error {
		for {
			lcid, err := ReadUint32(sock)
			if err != nil {
				fmt.Println("read lcid failed", err)
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
				fmt.Println("closing socket...", i, didClose, err)
				if err != nil {
					return err
				}
				if didClose {
					return nil
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
	}
	fmt.Println("handleIncomingMessages exit!---")
}

// | Create a connection to a remote endpoint
//
// If the remote endpoint is in 'RemoteEndPointClosing' state then we will
// block until that is resolved.
//
// May throw a TransportError ConnectErrorCode exception.
func (ourEndPoint *LocalEndPoint) createConnectionTo(theirAddress EndPointAddress) (*RemoteEndPoint, LightweightConnectionId, error) {
	return ourEndPoint.createConnectionTo_go(theirAddress, nil)
}

func (ourEndPoint *LocalEndPoint) createConnectionTo_go(theirAddress EndPointAddress, rsp ConnectionRequestResponse) (*RemoteEndPoint, LightweightConnectionId, error) {
	theirEndPoint, isNew, err := ourEndPoint.findRemoteEndPoint(theirAddress, RequestedByUs{})
	switch rsp.(type) {
	case ConnectionRequestCrossed:
		func() {
			theirState := &theirEndPoint.remoteState
			theirState.lock.Lock()
			defer theirState.lock.Unlock()

			switch theirState.value.(type) {
			case *RemoteEndPointInit:
				// resovled := st._1
				// notify(resolved)
				ourEndPoint.removeRemoteEndPoint(theirEndPoint)
				theirState.value = RemoteEndPointClosed{}
			}
		}()
	}

	if err != nil {
		return nil, firstNonReservedLightweightConnectionId, err
	}

	if isNew {
		rsp2, err := ourEndPoint.setupRemoteEndPoint(theirEndPoint)
		fmt.Println("createConnectionTo ", theirAddress, rsp2, err)
		if err != nil {
			// return theirEndPoint, firstNonReservedLightweightConnectionId, err
		}
		return ourEndPoint.createConnectionTo_go(theirAddress, rsp2)
	}
	// 'findRemoteEndPoint' will have increased 'remoteOutgoing'
	var action func()
	connId, err := func() (LightweightConnectionId, error) {
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointValid:
			vst := &st._1
			connId := vst._remoteNextConnOutId
			vst._remoteNextConnOutId = connId + 1
			conn := vst.remoteConn
			action = func() {
				sendCreateNewConnection(uint32(connId), conn)
			}
			return connId, nil
		case *RemoteEndPointInvalid:
			return 0, errors.New(st._1.String())
		case *RemoteEndPointFailed:
			return 0, st._1
		}
		return 0, errors.New("createConnectionTo")
	}()

	if action != nil {
		action()
	}
	return theirEndPoint, connId, err
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
	theirEndPoint, isNew, err := func() (*RemoteEndPoint, bool, error) {
		ourState := &ourEndPoint.localState

		ourState.lock.Lock()
		defer ourState.lock.Unlock()

		switch state := ourState.value.(type) {
		case *LocalEndPointValid:
			vst := &state._1
			if theirEndPoint, ok := vst._localConnections[theirAddress]; ok {
				return theirEndPoint, false, nil

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
	}()

	// fmt.Println("findRemoteEndPoint:", findOrigin, theirEndPoint, isNew, err)
	if err != nil || isNew {
		return theirEndPoint, isNew, err
	}

	snapshot := func() RemoteState {
		theirState := &theirEndPoint.remoteState
		theirState.lock.Lock()
		defer theirState.lock.Unlock()

		switch p := theirState.value.(type) {
		case *RemoteEndPointValid:
			vst := &p._1
			switch findOrigin.(type) {
			case RequestedByUs:
				vst._remoteOutgoing++
				fmt.Println("	remoteOutgoing++:", vst._remoteOutgoing)
				return theirState.value
			}
		}
		return theirState.value
	}()

	// fmt.Println("findRemoteEndPoint:snapshot", findOrigin, snapshot)

	switch st := snapshot.(type) {
	case *RemoteEndPointInvalid:
		return nil, false, errors.New(st._2)
	case *RemoteEndPointInit:
		//TODO: 1803
		return nil, false, errors.New("Already connected")
	case *RemoteEndPointValid:
		return theirEndPoint, false, nil
	case *RemoteEndPointClosing:
		//TODO: wait resolved
		return ourEndPoint.findRemoteEndPoint(theirAddress, findOrigin)
	case RemoteEndPointClosed:
		return ourEndPoint.findRemoteEndPoint(theirAddress, findOrigin)
	case *RemoteEndPointFailed:
		return nil, false, st._1
	}
	//this can not happen!
	panic("can not happen")
	return nil, false, nil
}

// | Send a CloseSocket request if the remote endpoint is unused
func (ourEndPoint *LocalEndPoint) closeIfUnused(theirEndPoint *RemoteEndPoint) {
	theirState := &theirEndPoint.remoteState

	action := func() func() {
		theirState.lock.Lock()
		theirState.lock.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointValid:
			vst := &st._1
			if vst._remoteOutgoing == 0 && len(vst._remoteIncoming) == 0 {
				theirState.value = &RemoteEndPointClosing{newNotifier(), *vst}
				conn := vst.remoteConn
				return func() {
					sendCloseSocket(uint32(vst._remoteLastIncoming), conn)
				}
			}
		}
		return nil
	}()

	if action != nil {
		fmt.Println("close unused connection to ", theirEndPoint.remoteAddress)
		action()
	}
}

// | Reset a remote endpoint if it is in Invalid mode
//
// If the remote endpoint is currently in broken state, and
//
//   - a user calls the API function 'connect', or and the remote endpoint is
//   - an inbound connection request comes in from this remote address
//
// we remove the remote endpoint first.
//
// Throws a TransportError ConnectFailed exception if the local endpoint is
// closed.
func (ourEndPoint *LocalEndPoint) resetIfBroken(theirAddress EndPointAddress) error {
	theirEndPoint, err := func() (*RemoteEndPoint, error) {
		ourState := &ourEndPoint.localState
		ourState.lock.Lock()
		defer ourState.lock.Unlock()

		switch st := ourState.value.(type) {
		case *LocalEndPointValid:
			vst := &st._1
			return vst._localConnections[theirAddress], nil
		default:
			return nil, errors.New("EndPoint closed")
		}
	}()

	if err != nil {
		return err
	}

	if theirEndPoint == nil {
		return nil
	}

	theirState := &theirEndPoint.remoteState
	theirState.lock.Lock()
	theirState.lock.Unlock()

	switch theirState.value.(type) {
	case *RemoteEndPointInvalid, *RemoteEndPointFailed:
		ourEndPoint.removeRemoteEndPoint(theirEndPoint)
	}
	return nil
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
func (tp *TCPTransport) createLocalEndPoint(epid EndPointId) (*LocalEndPoint, error) {
	tp.transportState.lock.Lock()
	defer tp.transportState.lock.Unlock()

	switch ts := tp.transportState.value.(type) {
	case *TransPortValid:
		vst := &ts._1
		endpoints := vst._localEndPoints

		if _, ok := endpoints[epid]; ok {
			return nil, errors.New("endpoint already exist")
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
		return nil, errors.New("transport closed")
	default:
		return nil, errors.New("new endpoint failed")
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
