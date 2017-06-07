package tcp

import (
	"errors"
	"fmt"
)

type TransportAddr string

type EndPointAddress struct {
	TransportAddr
	epid EndPointId
}

func newEndPointAddress(lAddr string, ep int) EndPointAddress {
	return EndPointAddress{TransportAddr(lAddr), EndPointId(ep)}
}

type Transport struct {
	Close       func() error
	NewEndPoint func(EndPointId) (*EndPoint, error)
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
	Write func([]byte) (int, error)
}

func Send(conn *Connection, msg string) error {
	_, err := conn.Write([]byte(msg))
	return err
}

func CreateTransport(lAddr string) (*Transport, error) {
	transport, err := createTCPTransport(lAddr)
	if err != nil {
		return nil, err
	}
	return &Transport{
		Close: func() error {
			return errors.New("not implemented")
		},
		NewEndPoint: func(epid EndPointId) (*EndPoint, error) {
			return transport.apiNewEndPoint(epid)
		},
	}, nil
}

func (addr EndPointAddress) String() string {
	return fmt.Sprintf("%s:%d", addr.TransportAddr, addr.epid)
}
