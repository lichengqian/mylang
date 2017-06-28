package tcp

import (
	"fmt"
	"net"
)

type EndPointAddress struct {
	TransportAddr
	EndPointId
}

type ShakeHand func(net.Conn, EndPointAddress) (net.Conn, error)

func NewEndPointAddress(lAddr string, ep int) EndPointAddress {
	return EndPointAddress{TransportAddr(lAddr), EndPointId(ep)}
}

type Transport struct {
	Close       func() error
	NewEndPoint func(EndPointId, ShakeHand) (*EndPoint, error)
}

type EndPoint struct {
	Close func() error
	// | Create a new lightweight connection.
	Dial func(remoteEP EndPointAddress) (*Connection, error)
	// | Endpoints have a single shared receive queue.
	Receive func() Event
	// | EndPointAddress of the endpoint.
	Address func() EndPointAddress
}

type Connection struct {
	Close func() error
	Send  func([]byte) (int, error)
}

func (conn *Connection) Write(bs []byte) (int, error) {
	return conn.Send(bs)
}

func CreateTransport(lAddr string) (*Transport, error) {
	transport, err := createTCPTransport(lAddr)
	if err != nil {
		return nil, err
	}
	return transport.ToTransport(), nil
}

func (transport *TCPTransport) ToTransport() *Transport {
	return &Transport{
		Close: func() error {
			return transport.apiCloseTransport([]Event{})
		},
		NewEndPoint: func(epid EndPointId, shake ShakeHand) (*EndPoint, error) {
			return transport.apiNewEndPoint(epid, shake)
		},
	}
}

func (addr EndPointAddress) String() string {
	return fmt.Sprintf("%s:%d", addr.TransportAddr, addr.EndPointId)
}
