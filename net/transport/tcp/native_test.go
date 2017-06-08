package tcp

import (
	"fmt"
	"net"
	"testing"
)

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

func mockEarlyDisconnect(lAddr string) (EndPointAddress, error) {
	handler := func(conn net.Conn) {
		// Initial setup
		testAcceptConn(conn)

		// Server opens  a logical connection
		ch, err := ReadUint32(conn)
		cheader := decodeControlHeader(uint8(ch))
		lcid, err := ReadUint32(conn)
		fmt.Println("mock2", cheader, lcid, err)

		// Server sends a message
		lcid2, err := ReadUint32(conn)
		bs, err := ReadWithLen(conn, 1000)
		fmt.Println("mock3", lcid2, string(bs), err)

		// Reply
		sendCreateNewConnection(10002, conn)

		WriteUint32(10002, conn)
		WriteWithLen([]byte("pong"), conn)

		// Close the socket
		conn.Close()
	}
	err := forkServer(lAddr, handler)
	return EndPointAddress{TransportAddr(lAddr), EndPointId(1000)}, err
}

func mockEarlyCloseSocket(lAddr string) (EndPointAddress, error) {
	handler := func(conn net.Conn) {
		// Initial setup
		testAcceptConn(conn)

		// Server opens  a logical connection
		ch, err := ReadUint32(conn)
		cheader := decodeControlHeader(uint8(ch))
		lcid, err := ReadUint32(conn)
		fmt.Println("mock2", cheader, lcid, err)

		// Server sends a message
		lcid2, err := ReadUint32(conn)
		bs, err := ReadWithLen(conn, 1000)
		fmt.Println("mock3", lcid2, string(bs), err)

		// Reply
		sendCreateNewConnection(10002, conn)

		LightweightConnectionId(10002).sendMsg([]byte("pong"), conn)

		// Close the socket
		// Send a CloseSocket even though there are still connections *in both
		// directions*
		sendCloseSocket(1024, conn)
		conn.Close()
	}
	err := forkServer(lAddr, handler)
	return EndPointAddress{TransportAddr(lAddr), EndPointId(1000)}, err
}

func mockUnnecessaryConnect(numThreads int, ourAddress EndPointAddress, theirAddress EndPointAddress, gotAccepted Notifier) {
	for i := 0; i < numThreads; i++ {
		done := newNotifier()
		defer wait(done)

		go func(idx int) {
			defer notify(done)
			sock, rsp, err := socketToEndPoint(ourAddress, theirAddress)
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
