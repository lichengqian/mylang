package tcp

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net"
	"strconv"
	"strings"
	"sync/atomic"
)

type Action func()
type Notifier chan struct{}

func newNotifier() Notifier {
	return make(chan struct{}, 1)
}

func wait(n Notifier) {
	<-n
}

func notify(n Notifier) {
	close(n)
}

type AtomicBool int32

// NewBool creates an AtomicBool with given default value
func NewBool(ok bool) *AtomicBool {
	ab := new(AtomicBool)
	if ok {
		ab.Set()
	}
	return ab
}

// Set sets the Boolean to true
func (ab *AtomicBool) Set() {
	atomic.StoreInt32((*int32)(ab), 1)
}

// UnSet sets the Boolean to false
func (ab *AtomicBool) UnSet() {
	atomic.StoreInt32((*int32)(ab), 0)
}

// IsSet returns whether the Boolean is true
func (ab *AtomicBool) IsSet() bool {
	return atomic.LoadInt32((*int32)(ab)) == 1
}

func encodeEndPointAddress(ep EndPointAddress) []byte {
	s := ep.String()
	return []byte(s)
}

func decodeEndPointAddress(bs []byte) (*EndPointAddress, error) {
	s := string(bs)
	// fmt.Println("before decode:", s)
	i := strings.LastIndex(s, ":")
	addr := TransportAddr(s[:i])
	epid, err := strconv.Atoi(s[(i + 1):])
	if err != nil {
		return nil, err
	}
	ep := EndPointAddress{addr, EndPointId(uint32(epid))}
	return &ep, nil
}

func WriteUint32(i uint32, w io.Writer) (int, error) {
	var buf [4]byte
	binary.BigEndian.PutUint32(buf[:], uint32(i))
	return w.Write(buf[:])
}

func WriteWithLen(buf []byte, w io.Writer) (int, error) {
	WriteUint32(uint32(len(buf)), w)
	return w.Write(buf[:])
}

func ReadExact(r io.Reader, len uint32) ([]byte, error) {
	// fmt.Println("ReadExact:", len)
	buf := make([]byte, len)
	_, err := io.ReadFull(r, buf[:])
	if err != nil {
		return nil, err
	}
	return buf, nil
}

func ReadUint32(r io.Reader) (uint32, error) {
	var buf [4]byte
	_, err := io.ReadFull(r, buf[:])
	if err != nil {
		return 0, err
	}
	return uint32(binary.BigEndian.Uint32(buf[:])), nil
}

func encodeChannelID(sid ChannelID) []byte {
	var buf [8]byte
	binary.BigEndian.PutUint64(buf[:], uint64(sid))
	return buf[:]
}

func decodeChannelID(b []byte) ChannelID {
	return ChannelID(binary.BigEndian.Uint64(b))
}

func ReadUint64(r io.Reader) (uint64, error) {
	var buf [8]byte
	_, err := io.ReadFull(r, buf[:])
	if err != nil {
		return 0, err
	}
	return binary.BigEndian.Uint64(buf[:]), nil
}

func ReadWithLen(r io.Reader, limit int) ([]byte, error) {
	len, err := ReadUint32(r)
	if err != nil {
		return nil, err
	}
	if len > uint32(limit) {
		return nil, errors.New("limit exceeded")
	}

	buf, err := ReadExact(r, len)
	if err != nil {
		return nil, err
	}
	return buf, nil
}

func splitHostPort(addr string) (host string, port int) {
	host, portStr, err := net.SplitHostPort(addr)
	if err != nil {
		panic(err)
	}
	port, err = strconv.Atoi(portStr)
	if err != nil {
		panic(err)
	}
	return host, port
}

func checkPeerHost(conn net.Conn, epAddr *EndPointAddress) bool {
	//check remote ip and port
	actualHost, _ := splitHostPort(conn.RemoteAddr().String())
	_, port := splitHostPort(string(epAddr.TransportAddr))

	actualAddr := actualHost + ":" + strconv.Itoa(port)
	fmt.Println("checkPeerHost ", actualAddr)
	epAddr.TransportAddr = TransportAddr(actualAddr)
	return true
}

func writeConnectionRequestResponse(rsp ConnectionRequestResponse, w io.Writer) (int, error) {
	return WriteUint32(uint32(rsp.tagConnectionRequestResponse()), w)
}

// net function
func (transport *TCPTransport) forkServer(handler func(net.Conn)) error {
	lAddr := transport.transportAddr
	ln, err := net.Listen("tcp", string(lAddr))
	if err != nil {
		return err
	}

	actualAddr := ln.Addr().String()
	fmt.Println("transport linsten on :", actualAddr)
	transport.transportAddr = TransportAddr(actualAddr)

	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				panic(err)
			}
			go handler(conn)
		}
	}()
	return nil
}

// | Establish a connection to a remote endpoint
//
// Maybe throw a TransportError
//
// If a socket is created and returned (Right is given) then the caller is
// responsible for eventually closing the socket and filling the MVar (which
// is empty). The MVar must be filled immediately after, and never before,
// the socket is closed.
func socketToEndPoint(ourAddress EndPointAddress, theirAddress EndPointAddress, shake ShakeHand) (net.Conn, ConnectionRequestResponse, error) {
	sock, err := net.Dial("tcp", string(theirAddress.TransportAddr))
	if err != nil {
		return nil, nil, err
	}

	//TODO:1663
	WriteUint32(uint32(theirAddress.EndPointId), sock)
	//write our address
	WriteWithLen(encodeEndPointAddress(ourAddress), sock)
	//handshake
	if shake != nil {
		sock, err = shake(sock, theirAddress)
		if err != nil {
			return nil, nil, err
		}
	}
	response, err := ReadUint32(sock)
	if err != nil {
		defer sock.Close()
		return nil, nil, err
	}
	rsp := decodeConnectionRequestResponse(uint8(response))
	return sock, rsp, nil
}

// for test only
func socketToEndPoint_(ourAddress EndPointAddress, theirAddress EndPointAddress) (net.Conn, error) {
	sock, rsp, err := socketToEndPoint(ourAddress, theirAddress, nil)
	if err != nil {
		return nil, err
	}
	switch rsp.(type) {
	case ConnectionRequestAccepted:
		return sock, nil
	default:
		defer sock.Close()
		return nil, errors.New(rsp.String())
	}
}

//-----------------------------------------------------------------------------
// network utils                                                             --
//-----------------------------------------------------------------------------

func tryShutdownSocketBoth(conn net.Conn) {
	conn.Close()
}

func sendCreateNewConnection(lcid uint32, w io.Writer) {
	WriteUint32(uint32(CreateNewConnection{}.tagControlHeader()), w)
	WriteUint32(lcid, w)
}

func sendCloseConnection(lcid uint32, w io.Writer) {
	fmt.Println("sending CloseConnection:", lcid)
	WriteUint32(uint32(CloseConnection{}.tagControlHeader()), w)
	WriteUint32(lcid, w)
}

func sendCloseSocket(i uint32, w io.Writer) {
	fmt.Println("sending CloseSocket:", i)
	WriteUint32(uint32(CloseSocket{}.tagControlHeader()), w)
	WriteUint32(i, w)
}

func sendCloseEndPoint(w io.Writer) {
	fmt.Println("sending CloseEndPoint")
	WriteUint32(uint32(CloseEndPoint{}.tagControlHeader()), w)
}

func (lcid LightweightConnectionId) sendMsg(msg []byte, w io.Writer) {
	WriteUint32(uint32(lcid), w)
	WriteWithLen(msg, w)
}

func recvControlHeader(r io.Reader) (ControlHeader, error) {
	n, err := ReadUint32(r)
	if err != nil {
		return nil, err
	}
	return decodeControlHeader(uint8(n)), nil
}

//-----------------------------------------------------------------------------
// Debugging                                                                 --
//-----------------------------------------------------------------------------

func (ourEndPoint *LocalEndPoint) relyViolation(str string) {
	fmt.Println(str + " RELY violation")
	panic(str + " RELY violation")
}

func (vst *ValidRemoteEndPointState) String() string {
	return fmt.Sprintf("ValidRemoteEndPointState: outgoing=%d, incoming=%d", vst._remoteOutgoing, len(vst._remoteIncoming))
}

// LocalNode & LocalSwitch
