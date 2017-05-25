package tcp

import (
	"errors"
	"fmt"
	"net"
)

type Transport interface {
	Listen(EndPointId) (EndPoint, error)
}

type EndPoint interface {
	net.Listener
	Dial(remoteEP EndPointAddress) (net.Conn, error)
}

func CreateTransport(lAddr string) (Transport, error) {
	return createTCPTransport(lAddr)
}

func (addr *EndPointAddress) String() string {
	return fmt.Sprintf("%s:%d", addr.TransportAddr, addr.epid)
}

func (tp *TCPTransport) Listen(epid EndPointId) (EndPoint, error) {
	ep, err := tp.newLocalEndPoint(epid)
	if ep == nil {
		// println(err.String())
		return nil, errors.New(err.String())
	}
	return ep, nil
}

func (ep *LocalEndPoint) Accept() (net.Conn, error) {
	conn, more := <-ep.connections
	if more {
		// never fail
		return conn, nil
	}
	return nil, errors.New("endpint closed!")
}

func (ep *LocalEndPoint) Close() error {
	// println()
	return ep.closeLocalEndPoint()
}

func (ep *LocalEndPoint) Addr() net.Addr {
	return &ep.localAddress
}

func (ep *LocalEndPoint) Dial(remoteEP EndPointAddress) (net.Conn, error) {
	return ep.connect(remoteEP)
}

func (addr *EndPointAddress) Network() string {
	return "tcp"
}
