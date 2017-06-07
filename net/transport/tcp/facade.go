package tcp

import (
	"errors"
	"fmt"
	"io"
	"net"
)

type TransportAddr string

type EndPointAddress struct {
	TransportAddr
	epid EndPointId
}

func newEndPointAddress(lAddr string, ep int) EndPointAddress {
	return EndPointAddress{TransportAddr(lAddr), EndPointId(ep)}
}

type Transport interface {
	io.Closer
	NewEndPoint(EndPointId) (EndPoint, error)
}

type EndPoint interface {
	io.Closer
	// | Create a new lightweight connection.
	Dial(remoteEP EndPointAddress) (*Connection, error)
	// | Endpoints have a single shared receive queue.
	Receive() Event
	// | EndPointAddress of the endpoint.
	Address() EndPointAddress
}

type Connection struct {
	Close func() error
	Write func([]byte) (int, error)
}

func Send(conn *Connection, msg string) error {
	_, err := conn.Write([]byte(msg))
	return err
}

func CreateTransport(lAddr string) (Transport, error) {
	return createTCPTransport(lAddr)
	// return nil, errors.New("not implemented")
}

func (tp *TCPTransport) NewEndPoint(epid EndPointId) (EndPoint, error) {
	return tp.createLocalEndPoint(epid)
}

func (addr EndPointAddress) String() string {
	return fmt.Sprintf("%s:%d", addr.TransportAddr, addr.epid)
}

func (tp *TCPTransport) Close() error {
	return errors.New("not implemented")
}

func (ep *LocalEndPoint) Address() EndPointAddress {
	return ep.localAddress
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

func (ep *LocalEndPoint) Dial(remoteEP EndPointAddress) (*Connection, error) {
	return ep.apiConnect(remoteEP)
}

func (addr *EndPointAddress) Network() string {
	return "tcp"
}
