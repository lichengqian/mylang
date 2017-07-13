package tcp

import (
	"errors"
	"fmt"
	"net"
	"testing"
)

func SendStr(conn *Connection, msg string) {
	conn.Send([]byte(msg))
}

func assertConnectionOpend(t *testing.T, event Event) *EndPointAddress {
	switch e := event.(type) {
	case *ConnectionOpened:
		fmt.Println(e._2)
		return &e._2
	default:
		t.Fatal("not match ConnectionOpend!")
	}
	return nil
	// if st, ok := event.(*ConnectionOpened); ok {
	// 	xx :=
	// }

}

func testAcceptConn(conn net.Conn) {
	// Initial setup
	ReadUint32(conn)
	ReadWithLen(conn, 1000)

	writeConnectionRequestResponse(ConnectionRequestAccepted{}, conn)
}

// func mockEarlyDisconnect(lAddr string) (EndPointAddress, error) {
// 	handler := func(conn net.Conn) {
// 		// Initial setup
// 		testAcceptConn(conn)

// 		// Server opens  a logical connection
// 		ch, err := ReadUint32(conn)
// 		cheader := decodeControlHeader(uint8(ch))
// 		lcid, err := ReadUint32(conn)
// 		fmt.Println("mock2", cheader, lcid, err)

// 		// Server sends a message
// 		lcid2, err := ReadUint32(conn)
// 		bs, err := ReadWithLen(conn, 1000)
// 		fmt.Println("mock3", lcid2, string(bs), err)

// 		// Reply
// 		sendCreateNewConnection(10002, conn)

// 		WriteUint32(10002, conn)
// 		WriteWithLen([]byte("pong"), conn)

// 		// Close the socket
// 		conn.Close()
// 	}
// 	err := forkServer(lAddr, handler)
// 	return EndPointAddress{TransportAddr(lAddr), EndPointId(1000)}, err
// }

// func mockEarlyCloseSocket(lAddr string) (EndPointAddress, error) {
// 	handler := func(conn net.Conn) {
// 		// Initial setup
// 		testAcceptConn(conn)

// 		// Server opens  a logical connection
// 		ch, err := ReadUint32(conn)
// 		cheader := decodeControlHeader(uint8(ch))
// 		lcid, err := ReadUint32(conn)
// 		fmt.Println("mock2", cheader, lcid, err)

// 		// Server sends a message
// 		lcid2, err := ReadUint32(conn)
// 		bs, err := ReadWithLen(conn, 1000)
// 		fmt.Println("mock3", lcid2, string(bs), err)

// 		// Reply
// 		sendCreateNewConnection(10002, conn)

// 		LightweightConnectionId(10002).sendMsg([]byte("pong"), conn)

// 		// Close the socket
// 		// Send a CloseSocket even though there are still connections *in both
// 		// directions*
// 		sendCloseSocket(1024, conn)
// 		conn.Close()
// 	}
// 	err := forkServer(lAddr, handler)
// 	return EndPointAddress{TransportAddr(lAddr), EndPointId(1000)}, err
// }

func mockUnnecessaryConnect(numThreads int, ourAddress EndPointAddress, theirAddress EndPointAddress, gotAccepted Notifier) {
	for i := 0; i < numThreads; i++ {
		done := newNotifier()
		defer wait(done)

		go func(idx int) {
			defer notify(done)
			sock, rsp, err := socketToEndPoint(ourAddress, theirAddress, nil)
			fmt.Println("mockUnnecessaryConnect:", idx, rsp, err)
			if err != nil {
				return
			}

			switch rsp.(type) {
			case ConnectionRequestAccepted:
				notify(gotAccepted)
			default:
				sock.Close()
			}
		}(i)
	}
	// fmt.Println("exit mockUnnecessaryConnect")
}

func TestRecover(t *testing.T) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("recover from ", r)
		}
		fmt.Println("closing resource")
	}()

	// _acc := 0
	fmt.Println("external address:", MkExternalAddress("127.0.0.1:99"))
	fmt.Println("external address:", MkExternalAddress("0.0.0.0:99"))
	panic(errors.New("hello, error"))
}

// Find a socket between two endpoints
//
// Throws an IO exception if the socket could not be found.
func (transport *TCPTransport) internalSocketBetween(ourAddress EndPointAddress, theirAddress EndPointAddress) (net.Conn, error) {
	ourEndPoint, err := func() (*LocalEndPoint, error) {
		s := &transport.transportState
		s.Lock()
		defer s.Unlock()

		switch st := s.value.(type) {
		case *TransPortValid:
			vst := &st._1
			if ep, ok := vst._localEndPoints[ourAddress.EndPointId]; ok {
				return ep, nil
			} else {
				return nil, errors.New("Local endpoint not found")
			}
		}
		return nil, ErrTransportClosed
	}()

	if err != nil {
		return nil, err
	}

	theirEndPoint, err := func() (*RemoteEndPoint, error) {
		s := &ourEndPoint.localState
		s.Lock()
		defer s.Unlock()

		switch st := s.value.(type) {
		case *LocalEndPointValid:
			vst := &st._1
			if ep, ok := vst._localConnections[theirAddress]; ok {
				return ep, nil
			} else {
				return nil, errors.New("RemoteEndPoint not found")
			}
		}
		return nil, ErrEndPointClosed
	}()

	if err != nil {
		return nil, err
	}

	s := &theirEndPoint.remoteState
	s.Lock()
	defer s.Unlock()

	switch st := s.value.(type) {
	case *RemoteEndPointInit:
		return nil, errors.New("Remote endpoint not yet initialized")
	case *RemoteEndPointValid:
		return st._1.remoteConn, nil
	case *RemoteEndPointClosing:
		return st._2.remoteConn, nil
	case RemoteEndPointClosed:
		return nil, errors.New("Remote endpoint closed")
	case *RemoteEndPointInvalid:
		return nil, errors.New(st._2)
	case *RemoteEndPointFailed:
		return nil, st._1
	}
	return nil, errors.New("can not happen")
}
