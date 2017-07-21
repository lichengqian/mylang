package tcp

import (
	"errors"
	"fmt"
	"net"
	"sync"
)

//------------------------------------------------------------------------------
// API functions                                                              --
//------------------------------------------------------------------------------

// | Close the transport
func (tp *TCPTransport) apiCloseTransport(evs []Event) error {
	return errors.New("not implemented")
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

	go vst.sendRoutine()
	tp.transportParams.handleIncomingMessages(ourEndPoint, theirEndPoint)
}

// | Handle requests from a remote endpoint.
//
// Returns only if the remote party closes the socket or if an error occurs.
// This runs in a thread that will never be killed.
func (params *TCPParameters) handleIncomingMessages(ourEndPoint *LocalEndPoint, theirEndPoint *RemoteEndPoint) {
	theirAddress := theirEndPoint.remoteAddress

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
		ourEndPoint.prematureExit(theirEndPoint, err)
		return
	}

	// Read a message and output it on the endPoint's channel. By rights we
	// should verify that the connection ID is valid, but this is unnecessary
	// overhead
	readMessage := func(sock net.Conn, lcid LightweightConnectionId) error {
		msg, err := ReadWithLen(sock, params.tcpMaxReceiveLength)
		if err != nil {
			return err
		}
		event := &Received{theirEndPoint.connId(lcid), msg}
		ourEndPoint.enqueue(event)
		return nil
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
				err = ourEndPoint.onCreateNewConnection(theirEndPoint, LightweightConnectionId(cid))
				if err != nil {
					return err
				}
				continue
			case CloseConnection:
				cid, err := ReadUint32(sock)
				if err != nil {
					return err
				}
				err = ourEndPoint.onCloseConnection(theirEndPoint, LightweightConnectionId(cid))
				if err != nil {
					return err
				}
				continue
			case CloseSocket:
				i, err := ReadUint32(sock)
				if err != nil {
					return err
				}
				didClose := ourEndPoint.onCloseSocket(theirEndPoint, sock, LightweightConnectionId(i))
				fmt.Println("closing socket...", i, didClose)
				if didClose {
					return nil
				}
			case CloseEndPoint:
				ourEndPoint.removeRemoteEndPoint(theirEndPoint)
				ourEndPoint.onCloseEndPoint(theirEndPoint)
				//exit for loop
				return nil
			default:
				err := errors.New("Invalid control request")
				return err
			}
		}
	}()

	if err != nil {
		ourEndPoint.prematureExit(theirEndPoint, err)
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
	theirEndPoint, err := params.createSocketTo_go(ourEndPoint, theirAddress, nil)
	if err != nil {
		return nil, firstNonReservedLightweightConnectionId, err
	}
	connId, err := theirEndPoint.newConnection()
	return theirEndPoint, connId, err
}

func (params *TCPParameters) createSocketTo(ourEndPoint *LocalEndPoint, theirAddress EndPointAddress) (*RemoteEndPoint, error) {
	return params.createSocketTo_go(ourEndPoint, theirAddress, nil)
}

func (params *TCPParameters) createSocketTo_go(ourEndPoint *LocalEndPoint, theirAddress EndPointAddress, rsp ConnectionRequestResponse) (*RemoteEndPoint, error) {
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
		return nil, err
	}

	if isNew {
		rsp2, err := ourEndPoint.setupRemoteEndPoint(params, theirEndPoint)
		fmt.Println("createConnectionTo ", theirAddress, rsp2, err)
		if err != nil {
			// return theirEndPoint, firstNonReservedLightweightConnectionId, err
		}
		return params.createSocketTo_go(ourEndPoint, theirAddress, rsp2)
	}
	return theirEndPoint, nil
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

//------------------------------------------------------------------------------
// Constants                                                                  --
//------------------------------------------------------------------------------

// | Default TCP parameters
var defaultTCPParameters = &TCPParameters{
	tcpMaxAddressLength: 1000,
	tcpMaxReceiveLength: 4 * 1024 * 1024,
}
