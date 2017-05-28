package tcp

import (
	"bufio"
	"errors"
	"net"
)

func createTCPTransport(lAddr string) (*TCPTransport, error) {
	ln, err := net.Listen("tcp", lAddr)
	if err != nil {
		return nil, err
	}

	state := &TransportState{
		_localEndPoints: make(map[EndPointId]*LocalEndPoint, 10),
		_nextEndPointId: 0,
	}

	tp := &TCPTransport{
		transportAddr:  TransportAddr(lAddr),
		transportState: state,
	}
	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				panic(err)
			}
			go tp.handleConnectionRequest(conn)
		}
	}()
	return tp, nil
}

func (tp *TCPTransport) newLocalEndPoint(epid EndPointId) (*LocalEndPoint, NewEndPointErrorCode) {
	tp.transportState_lock.Lock()
	defer tp.transportState_lock.Unlock()

	endpoints := tp.transportState._localEndPoints

	if _, ok := endpoints[epid]; ok {
		// println("endpoint already exist!")
		return nil, NewEndPointFailed
	}

	st := &LocalEndPointState{}

	endpoints[epid] = &LocalEndPoint{
		localAddress: EndPointAddress{tp.transportAddr, epid},
		localState:   st,
		localQueue:   make(chan Event, 10),
		closeLocalEndPoint: func() error {
			return tp.closeLocalEndPoint(epid)
		},
	}
	return endpoints[epid], 0xFF
}

func (tp *TCPTransport) closeLocalEndPoint(epid EndPointId) error {
	tp.transportState_lock.Lock()
	defer tp.transportState_lock.Unlock()

	endpoints := tp.transportState._localEndPoints
	if ep, ok := endpoints[epid]; ok {
		// close(ep.connections)
		delete(endpoints, epid)
		return nil
	}
	return errors.New("endpoinyt not exist!")
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
	bs, err := ReadWithLen(conn, 1000)
	if err != nil {
		conn.Close()
		return
	}
	theirAddress := decodeEndPointAddress(bs)
	println(theirAddress)
	if !checkPeer(conn, theirAddress) {
		WriteUint32(uint32(ConnectionRequestHostMismatch), conn)
		conn.Close()
		return
	}

	// ourAddress := tp.transportAddr.encodeEndPointAddress(ourEndPointID)

	// dispatch to endpoint
	// we need this clojure to avoid dead lock!!!
	ep, err := func() (*LocalEndPoint, error) {
		tp.transportState_lock.Lock()
		defer tp.transportState_lock.Unlock()

		state := tp.transportState
		if state == nil {
			return nil, errors.New("Transport closed")
		}

		if ep, ok := state._localEndPoints[EndPointId(ourEndPointID)]; ok {
			return ep, nil
		}

		//connection to unknown endpoint
		WriteUint32(uint32(ConnectionRequestInvalid), conn)
		return nil, errors.New("unknown endpoint")
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
	theirEndPoint, err := ourEndPoint.findRemoteEndPoint(*theirAddress, RequestedByThem)
	if err != nil {
		println(err)
		WriteUint32(uint32(ConnectionRequestCrossed), conn)
		conn.Close()
		return
	}

	st := NewRemoteEndPointValid(
		ValidRemoteEndPointState{
			remoteConn: conn,
			// remoteOutgoing: 0,
		})
	//connection accepted
	WriteUint32(uint32(ConnectionRequestAccepted), conn)
	resolveInit(ourEndPoint, theirEndPoint, st)
	theirEndPoint.handleIncomingMessage()
}

// | Handle requests from a remote endpoint.
//
// Returns only if the remote party closes the socket or if an error occurs.
// This runs in a thread that will never be killed.
func handleIncomingMessage(ourEndPoint *LocalEndPoint, theirEndPoint *RemoteEndPoint) {
	sock, err := func() (net.Conn, error) {
		theirEndPoint.remoteState_lock.Lock()
		defer theirEndPoint.remoteState_lock.Unlock()

		//TODO: 1072
		return nil, errors.New("handleIncomingMessages (failed)")
	}()

	// Deal with a premature exit 1329
	prematureExit := func(err error) {

	}

	if err != nil {
		prematureExit(err)
		return
	}

	// Dispatch
	//
	// If a recv throws an exception this will be caught top-level and
	// 'prematureExit' will be invoked. The same will happen if the remote
	// endpoint is put into a Closed (or Closing) state by a concurrent thread
	// (because a 'send' failed) -- the individual handlers below will throw a
	// user exception which is then caught and handled the same way as an
	// exception thrown by 'recv'.

	//TODO: 1101
	for {
		lcid, err := ReadUint32(sock)
		if err != nil {
			prematureExit(err)
			return
		}

	}
}

// | Find a remote endpoint. If the remote endpoint does not yet exist we
// create it in Init state. Returns if the endpoint was new, or 'Nothing' if
// it times out.
func (ourEndPoint *LocalEndPoint) findRemoteEndPoint(theirAddress EndPointAddress, findOrigin RequestedBy) (*RemoteEndPoint, error) {
	ourEndPoint.localState_lock.Lock()
	defer ourEndPoint.localState_lock.Unlock()

	state := ourEndPoint.localState
	if state == nil {
		return nil, errors.New("Local endpoint closed")
	}

	if theirEndPoint, ok := state._localConnections[theirAddress]; ok {
		theirEndPoint.remoteState_lock.Lock()
		defer theirEndPoint.remoteState_lock.Unlock()

		theirState := theirEndPoint.remoteState
		//TODO:
		switch theirState {
		case RemoteEndPointValid:
			return nil, nil
		}
	} else {
		//TODO:
		theirState := nil
		theirEndPoint = &RemoteEndPoint{
			remoteAddress: theirAddress,
			remoteState:   theirState,
			remoteId:      state._nextConnInId,
		}
		state._localConnections[theirAddress] = theirEndPoint
		state._nextConnInId += 1
		return theirEndPoint, nil
	}
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

	r, err := ReadUint32(conn)
	if err != nil {
		defer conn.Close()
		return nil, err
	}
	resp := ConnectionRequestResponse(r)
	if resp != ConnectionRequestAccepted {
		defer conn.Close()
		return nil, errors.New(resp.String())
	}
	return conn, nil
}

// Resolve an endpoint currently in 'Init' state
func resolveInit(ourEndPoint LocalEndPoint, theirEndPoint RemoteEndPoint, newState *RemoteState) {
	theirEndPoint.remoteState_lock.Lock()
	defer theirEndPoint.remoteState_lock.Unlock()

	st := theirEndPoint.remoteState

	theirEndPoint.remoteState = newState
}

//------------------------------------------------------------------------------
// Constants                                                                  --
//------------------------------------------------------------------------------

const (
	firstNonReservedLightweightConnectionId = LightweightConnectionId(1024)
	firstNonReservedHeavyweightConnectionId = HeavyweightConnectionId(1)
)
