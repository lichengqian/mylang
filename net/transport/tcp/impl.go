package tcp

import (
	"bufio"
	"encoding/binary"
	"errors"
	"io"
	"net"
)

type TransportAddr string

type EndPointAddress struct {
	TransportAddr
	epid EndPointId
}

func createTCPTransport(lAddr string) (*TCPTransport, error) {
	ln, err := net.Listen("tcp", lAddr)
	if err != nil {
		return nil, err
	}

	state := &TransportState{
		_localEndPoints: make(map[EndPointId]*LocalEndPoint, 10),
	}

	tp := &TCPTransport{
		transportAddr:  TransportAddr(lAddr),
		listener:       ln,
		transportState: state,
	}
	go tp.transportRoutine()
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

	endpoints[epid] = &LocalEndPoint{
		localAddress: EndPointAddress{tp.transportAddr, epid},
		connections:  make(chan net.Conn, 10),
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
		close(ep.connections)
		delete(endpoints, epid)
		return nil
	}
	return errors.New("endpoinyt not exist!")
}

func (tp *TCPTransport) handleConnectionRequest(conn net.Conn) {
	// get endpoint id
	ourEndPointID, err := ReadUint32(conn)
	if err != nil {
		conn.Close()
		return
	}
	// get remote endpoint
	theirEndPoint, err := ReadWithLen(conn, 1000)
	if err != nil {
		conn.Close()
		return
	}

	println(theirEndPoint)
	// dispatch to endpoint
	tp.transportState_lock.Lock()
	defer tp.transportState_lock.Unlock()

	state := tp.transportState
	if ep, ok := state._localEndPoints[EndPointId(ourEndPointID)]; ok {
		//connection accepted
		WriteUint32(uint32(ConnectionRequestAccepted), conn)
		ep.connections <- conn
	} else {
		//connection to unknown endpoint
		WriteUint32(uint32(ConnectionRequestInvalid), conn)
		conn.Close()
	}
}

func (tp *TCPTransport) transportRoutine() {
	for {
		conn, err := tp.listener.Accept()
		if err != nil {
			panic(err)
		}
		tp.handleConnectionRequest(conn)
	}
}

func WriteUint32(i uint32, w io.Writer) (int, error) {
	var buf [4]byte
	binary.BigEndian.PutUint32(buf[:], uint32(i))
	return w.Write(buf[:])
}

func ReadExact(r io.Reader, len uint32) ([]byte, error) {
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

func ReadWithLen(r io.Reader, limit uint32) ([]byte, error) {
	len, err := ReadUint32(r)
	if err != nil {
		return nil, err
	}
	if len > limit {
		return nil, errors.New("limit exceeded")
	}

	buf, err := ReadExact(r, len)
	if err != nil {
		return nil, err
	}
	return buf, nil
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
