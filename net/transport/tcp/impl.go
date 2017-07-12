package tcp

import (
	"errors"
	"fmt"
	"io"
	"net"
	"sync"
)

func createTCPTransport(lAddr string) (*TCPTransport, error) {
	tp := newTCPTransport(lAddr, defaultTCPParameters)

	err := tp.forkServer(func(conn net.Conn) {
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

// | Close the transport
func (tp *TCPTransport) apiCloseTransport(evs []Event) error {
	return errors.New("not implemented")
}

// | Create a new endpoint
func (tp *TCPTransport) apiNewEndPoint(epid EndPointId, shake ShakeHand) (*EndPoint, error) {
	ourEndPoint, err := tp.createLocalEndPoint(epid, shake)
	if err != nil {
		return nil, err
	}
	return &EndPoint{
		Close: func() error {
			return tp.apiCloseEndPoint([]Event{EndPointClosed{}}, ourEndPoint)
		},
		Dial: func(theirAddress EndPointAddress) (*Connection, error) {
			return tp.transportParams.apiConnect(ourEndPoint, theirAddress)
		},
		Receive: func() Event {
			return <-ourEndPoint.localQueue
		},
		Address: func() EndPointAddress {
			return ourEndPoint.localAddress
		},
	}, nil
}

// | Connnect to an endpoint
func (params *TCPParameters) apiConnect(ourEndPoint *LocalEndPoint, theirAddress EndPointAddress) (*Connection, error) {
	//TODO: connect to self 756

	err := ourEndPoint.resetIfBroken(theirAddress)
	if err != nil {
		return nil, err
	}
	theirEndPoint, connId, err := params.createConnectionTo(ourEndPoint, theirAddress)
	if err != nil {
		return nil, err
	}

	if theirEndPoint == nil {
		panic("apiConnect failed!")
	}

	connAlive := NewBool(true)
	return &Connection{
		Close: func() error {
			return ourEndPoint.apiClose(theirEndPoint, connId, connAlive)
		},
		Send: func(msg []byte) (int, error) {
			return ourEndPoint.apiSend(theirEndPoint, connId, msg, connAlive)
		},
	}, nil
}

// | Force-close the endpoint
func (transport *TCPTransport) apiCloseEndPoint(evs []Event, ourEndPoint *LocalEndPoint) error {
	// Remove the reference from the transport state
	transport.removeLocalEndPoint(ourEndPoint)
	// Close the local endpoint
	ourState := func() *ValidLocalEndPointState {
		st := &ourEndPoint.localState
		st.Lock()
		defer st.Unlock()

		switch state := st.value.(type) {
		case *LocalEndPointValid:
			st.value = LocalEndPointClosed{}
			return &state._1
		}
		return nil
	}()

	// Close the remote socket and return the set of all incoming connections
	tryCloseRemoteSocket := func(theirEndPoint *RemoteEndPoint) {
		// We make an attempt to close the connection nicely
		// (by sending a CloseSocket first)
		closed := &RemoteEndPointFailed{errors.New("apiCloseEndPoint")}

		theirState := &theirEndPoint.remoteState
		theirState.Lock()
		defer theirState.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInit:
			resolved := st._1
			notify(resolved)
			theirState.value = closed
		case *RemoteEndPointValid:
			// Schedule an action to send a CloseEndPoint message and then
			// wait for the socket to actually close (meaning that this
			// end point is no longer receiving from it).
			// Since we replace the state in this MVar with 'closed', it's
			// guaranteed that no other actions will be scheduled after this
			// one.
			vst := &st._1
			vst.sendOn(sendCloseEndPoint)
			tryShutdownSocketBoth(vst.remoteConn)
			// remoteSocketClosed(vst)

			theirState.value = closed
		case *RemoteEndPointClosing:
			resovled := st._1
			vst := &st._2
			notify(resovled)

			// Schedule an action to wait for the socket to actually close (this
			// end point is no longer receiving from it).
			// Since we replace the state in this MVar with 'closed', it's
			// guaranteed that no other actions will be scheduled after this
			// one.
			tryShutdownSocketBoth(vst.remoteConn)
			// remoteSocketClosed(vst)

			theirState.value = closed
		}
	}

	if ourState != nil {
		for _, remoteEndPoint := range ourState._localConnections {
			tryCloseRemoteSocket(remoteEndPoint)
		}
		for _, e := range evs {
			ourEndPoint.localQueue <- e
		}
	}
	return nil
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
	bs, err := ReadWithLen(conn, tp.transportParams.tcpMaxAddressLength)
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
		tp.transportState.Lock()
		defer tp.transportState.Unlock()

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
			return nil, ErrTransportClosed
		}
	}()

	if err != nil {
		println(err)
		conn.Close()
		return
	}

	tp.handleConnectionRequestForEndPoint(ep, theirAddress, conn)
}

// endpoint handle incoming connection
func (tp *TCPTransport) handleConnectionRequestForEndPoint(ourEndPoint *LocalEndPoint, theirAddress *EndPointAddress, conn net.Conn) {
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

	//handshake!
	if ourEndPoint.shakeHand != nil {
		conn, err = ourEndPoint.shakeHand(conn, *theirAddress)
		if err != nil {
			println("shake hand failed", err)
			conn.Close()
			return
		}
	}
	vst := newRemoteEndPointValid(conn)
	writeConnectionRequestResponse(ConnectionRequestAccepted{}, conn)
	ourEndPoint.resolveInit(theirEndPoint, vst)

	go (&vst._1).sendRoutine()
	tp.transportParams.handleIncomingMessages(ourEndPoint, theirEndPoint)
}

// | Handle requests from a remote endpoint.
//
// Returns only if the remote party closes the socket or if an error occurs.
// This runs in a thread that will never be killed.
func (params *TCPParameters) handleIncomingMessages(ourEndPoint *LocalEndPoint, theirEndPoint *RemoteEndPoint) {
	theirAddress := theirEndPoint.remoteAddress

	// Deal with a premature exit
	prematureExit := func(err error) {
		fmt.Println("in prematureExit:", err)
		theirState := &theirEndPoint.remoteState
		theirState.Lock()
		defer theirState.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInvalid, *RemoteEndPointInit, RemoteEndPointClosed:
			ourEndPoint.relyViolation("handleIncomingMessages:prematureExi")
		case *RemoteEndPointValid:
			// vst := &st._1
			code := &EventConnectionLost{theirAddress}
			ourEndPoint.localQueue <- &ErrorEvent{code, err}
			theirEndPoint.remoteState.value = &RemoteEndPointFailed{err}
		case *RemoteEndPointClosing:
			notify(st._1)
			theirEndPoint.remoteState.value = &RemoteEndPointFailed{err}
		case *RemoteEndPointFailed:
			ourEndPoint.localState.Lock()
			defer ourEndPoint.localState.Unlock()

			if _, ok := ourEndPoint.localState.value.(*LocalEndPointValid); ok {
				code := &EventConnectionLost{theirAddress}
				ourEndPoint.localQueue <- &ErrorEvent{code, err}
			}
		}
	}

	sock, err := func() (net.Conn, error) {
		theirState := &theirEndPoint.remoteState
		theirState.Lock()
		defer theirState.Unlock()

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

	// Read a message and output it on the endPoint's channel. By rights we
	// should verify that the connection ID is valid, but this is unnecessary
	// overhead
	readMessage := func(sock net.Conn, lcid LightweightConnectionId) error {
		msg, err := ReadWithLen(sock, params.tcpMaxReceiveLength)
		if err != nil {
			return err
		}
		event := &Received{connId(lcid), msg}
		ourEndPoint.enqueue(event)
		return nil
	}

	// Create a new connection
	createdNewConnection := func(lcid LightweightConnectionId) error {
		theirState := &theirEndPoint.remoteState
		theirState.Lock()
		defer theirState.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInvalid:
			ourEndPoint.relyViolation("handleIncomingMessages:createNewConnection (invalid)")
		case *RemoteEndPointInit:
			ourEndPoint.relyViolation("handleIncomingMessages:createNewConnection (init)")
		case *RemoteEndPointValid:
			st._1._remoteLastIncoming = lcid
			st._1._remoteIncoming[lcid] = struct{}{}
		case *RemoteEndPointClosing:
			// If the endpoint is in closing state that means we send a
			// CloseSocket request to the remote endpoint. If the remote
			// endpoint replies that it created a new connection, it either
			// ignored our request or it sent the request before it got ours.
			// Either way, at this point we simply restore the endpoint to
			// RemoteEndPointValid
			notify(st._1)
			vst := &st._2
			vst._remoteLastIncoming = lcid
			vst._remoteIncoming[lcid] = struct{}{}
			theirState.value = &RemoteEndPointValid{*vst}
		case *RemoteEndPointFailed:
			return st._1
		case RemoteEndPointClosed:
			ourEndPoint.relyViolation("createNewConnection (closed)")
		}

		ourEndPoint.enqueue(&ConnectionOpened{connId(lcid), theirAddress})
		return nil
	}

	// Close a connection
	// It is important that we verify that the connection is in fact open,
	// because otherwise we should not decrement the reference count
	closeConnection := func(lcid LightweightConnectionId) error {
		theirState := &theirEndPoint.remoteState
		theirState.Lock()
		defer theirState.Unlock()

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

		ourEndPoint.enqueue(&ConnectionClosed{connId(lcid)})
		return nil
	}

	// Close the socket (if we don't have any outgoing connections)
	closeSocket := func(sock net.Conn, lastReceivedId LightweightConnectionId) (bool, error) {
		action := func() func() {
			theirState := &theirEndPoint.remoteState
			theirState.Lock()
			defer theirState.Unlock()

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
					ourEndPoint.enqueue(&ConnectionClosed{connId(k)})
				}
				if uint32(vst._remoteOutgoing) > 0 || uint32(lastReceivedId) != uint32(lastSentId(vst)) {
					fmt.Println("we still have connections, can not close socket", vst._remoteOutgoing)
					vst._remoteIncoming = make(map[LightweightConnectionId]struct{})
					return nil
				} else {
					ourEndPoint.removeRemoteEndPoint(theirEndPoint)
					theirState.value = RemoteEndPointClosed{}
					return func() {
						vst.sendOn(func(conn io.Writer) {
							sendCloseSocket(uint32(vst._remoteLastIncoming), conn)
						})
					}
				}
			case *RemoteEndPointClosing:
				// Like above, we need to check if there is a ConnectionCreated
				// message that we sent but that the remote endpoint has not yet
				// received. However, since we are in 'closing' state, the only
				// way this may happen is when we sent a ConnectionCreated,
				// ConnectionClosed, and CloseSocket message, none of which have
				// yet been received. It's sufficient to check that the peer has
				// not seen the ConnectionCreated message. In case they have seen
				// it (so that lastReceivedId == lastSendId vst) then they must
				// have seen the other messages or else they would not have sent
				// CloseSocket.
				// We leave the endpoint in closing state in that case.
				vst := &st._2
				if lastReceivedId != lastSentId(vst) {
					return nil
				}
				if vst._remoteOutgoing > 0 {
					code := &EventConnectionLost{theirAddress}
					msg := "socket closed prematurely by peer"
					ourEndPoint.enqueue(&ErrorEvent{code, errors.New(msg)})
				}
				ourEndPoint.removeRemoteEndPoint(theirEndPoint)
				theirState.value = RemoteEndPointClosed{}
				// Nothing to do, but we want to indicate that the socket
				// really did close.
				notify(st._1)
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
			ourEndPoint.enqueue(&ConnectionClosed{connId(k)})
		}
		// report the endpoint as gone if we have any outgoing connections
		if vst._remoteOutgoing > 0 {
			code := &EventConnectionLost{theirAddress}
			ourEndPoint.enqueue(&ErrorEvent{code, errors.New("The remote endpoint was closed.")})
		}
	}

	closeEndPoint := func() {
		theirState := &theirEndPoint.remoteState
		theirState.Lock()
		defer theirState.Unlock()

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
				//exit for loop
				return nil
			default:
				err := errors.New("Invalid control request")
				return err
			}
		}
	}()

	if err != nil {
		prematureExit(err)
	}
	fmt.Println("handleIncomingMessages exit!---", theirAddress)
}

// | Create a connection to a remote endpoint
//
// If the remote endpoint is in 'RemoteEndPointClosing' state then we will
// block until that is resolved.
//
// May throw a TransportError ConnectErrorCode exception.
func (params *TCPParameters) createConnectionTo(ourEndPoint *LocalEndPoint, theirAddress EndPointAddress) (*RemoteEndPoint, LightweightConnectionId, error) {
	return params.createConnectionTo_go(ourEndPoint, theirAddress, nil)
}

func (params *TCPParameters) createConnectionTo_go(ourEndPoint *LocalEndPoint, theirAddress EndPointAddress, rsp ConnectionRequestResponse) (*RemoteEndPoint, LightweightConnectionId, error) {
	theirEndPoint, isNew, err := ourEndPoint.findRemoteEndPoint(theirAddress, RequestedByUs{})
	switch rsp.(type) {
	case ConnectionRequestCrossed:
		func() {
			theirState := &theirEndPoint.remoteState
			theirState.Lock()
			defer theirState.Unlock()

			switch st := theirState.value.(type) {
			case *RemoteEndPointInit:
				resolved := st._1
				notify(resolved)
				ourEndPoint.removeRemoteEndPoint(theirEndPoint)
				theirState.value = RemoteEndPointClosed{}
			}
		}()
	}

	if err != nil {
		return nil, firstNonReservedLightweightConnectionId, err
	}

	if isNew {
		rsp2, err := params.setupRemoteEndPoint(ourEndPoint, theirEndPoint)
		fmt.Println("createConnectionTo ", theirAddress, rsp2, err)
		if err != nil {
			// return theirEndPoint, firstNonReservedLightweightConnectionId, err
		}
		return params.createConnectionTo_go(ourEndPoint, theirAddress, rsp2)
	}
	// 'findRemoteEndPoint' will have increased 'remoteOutgoing'
	var action func()
	connId, err := func() (LightweightConnectionId, error) {
		theirState := &theirEndPoint.remoteState
		theirState.Lock()
		defer theirState.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointValid:
			vst := &st._1
			connId := vst._remoteNextConnOutId
			vst._remoteNextConnOutId = connId + 1
			action = func() {
				vst.sendOn(func(conn io.Writer) {
					sendCreateNewConnection(uint32(connId), conn)
				})
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
func (params *TCPParameters) setupRemoteEndPoint(ourEndPoint *LocalEndPoint, theirEndPoint *RemoteEndPoint) (ConnectionRequestResponse, error) {
	ourAddress := ourEndPoint.localAddress
	theirAddress := theirEndPoint.remoteAddress

	sock, rsp, err := socketToEndPoint(ourAddress, theirAddress, ourEndPoint.shakeHand)
	if err != nil {
		ourEndPoint.resolveInit(theirEndPoint, &RemoteEndPointInvalid{nil, err.Error()})
		return nil, err
	}

	theirState := &theirEndPoint.remoteState

	switch rsp.(type) {
	case ConnectionRequestAccepted:
		st := newRemoteEndPointValid(sock)
		ourEndPoint.resolveInit(theirEndPoint, st)

		go (&st._1).sendRoutine()
		go func() {
			defer sock.Close()
			params.handleIncomingMessages(ourEndPoint, theirEndPoint)
		}()
	case ConnectionRequestInvalid:
		defer sock.Close()

		st := &RemoteEndPointInvalid{ConnectNotFound{}, "setupRemoteEndPoint: Invalid endpoint"}
		ourEndPoint.resolveInit(theirEndPoint, st)
	case ConnectionRequestCrossed:
		defer sock.Close()
		theirState.Lock()
		defer theirState.Unlock()

		switch st := theirState.value.(type) {
		case *RemoteEndPointInit:
			crossed := st._2
			notify(crossed)
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

		ourState.Lock()
		defer ourState.Unlock()

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
						sync.Mutex
					}{value: theirState},
					remoteId: vst._nextConnInId,
				}
				vst._localConnections[theirAddress] = theirEndPoint
				vst._nextConnInId += 1
				return theirEndPoint, true, nil
			}
		default: // LocalEndPointClosed
			return nil, false, ErrEndPointClosed
		}
	}()

	// fmt.Println("findRemoteEndPoint:", findOrigin, theirEndPoint, isNew, err)
	if err != nil || isNew {
		return theirEndPoint, isNew, err
	}

	snapshot := func() RemoteState {
		theirState := &theirEndPoint.remoteState
		theirState.Lock()
		defer theirState.Unlock()

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

	fmt.Println("findRemoteEndPoint:snapshot", findOrigin, snapshot)

	switch st := snapshot.(type) {
	case *RemoteEndPointInvalid:
		return nil, false, errors.New(st._2)
	case *RemoteEndPointInit:
		//TODO: 1803
		resolved, crossed, initOrigin := st._1, st._2, st._3
		switch findOrigin.(type) {
		case RequestedByUs:
			wait(resolved)
			return ourEndPoint.findRemoteEndPoint(theirAddress, findOrigin)
		case RequestedByThem:
			switch initOrigin.(type) {
			case RequestedByUs:
				if ourEndPoint.localAddress.String() > theirAddress.String() {
					// Wait for the Crossed message
					wait(crossed)
					return theirEndPoint, true, nil
				} else {
					return theirEndPoint, false, nil
				}
			}
		}
		// case RequestedByThem:
		return nil, false, errors.New("Already connected")
	case *RemoteEndPointValid:
		return theirEndPoint, false, nil
	case *RemoteEndPointClosing:
		//TODO: wait resolved
		wait(st._1)
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

func createConnectionId(hcid HeavyweightConnectionId, lcid LightweightConnectionId) ConnectionId {
	return ConnectionId(uint64(uint32(hcid))<<32 | uint64(uint32(lcid)))
}

//------------------------------------------------------------------------------
// Constants                                                                  --
//------------------------------------------------------------------------------

// | Default TCP parameters
var defaultTCPParameters = &TCPParameters{
	tcpMaxAddressLength: 1000,
	tcpMaxReceiveLength: 4 * 1024 * 1024,
}
