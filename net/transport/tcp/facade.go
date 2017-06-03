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
	// return createTCPTransport(lAddr)
	return nil, errors.New("not implemented")
}

func (addr EndPointAddress) String() string {
	return fmt.Sprintf("%s:%d", addr.TransportAddr, addr.epid)
}

func (ep *LocalEndPoint) Receive() Event {
	return <-ep.localQueue
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
