package tcp

import (
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
	Send  func([]byte) (int, error)
}

func (conn *Connection) Write(bs []byte) (int, error) {
	return conn.Write(bs)
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
		NewEndPoint: func(epid EndPointId) (*EndPoint, error) {
			return transport.apiNewEndPoint(epid)
		},
	}
}

func (addr EndPointAddress) String() string {
	return fmt.Sprintf("%s:%d", addr.TransportAddr, addr.epid)
}

// LocalNode / LocalSwitch interface

func (transport *Transport) NewLocalNode() (*LocalNode, error) {
	return newLocalNode(transport)
}

func (localNode *LocalNode) NewLocalSwitch(sid SwitchID) (*LocalSwitch, error) {
	return newLocalSwitch(localNode, sid)
}

func (localSwitch *LocalSwitch) Receive() Message {
	return <-localSwitch.switchQueue
}

func (localSwitch *LocalSwitch) SendPayload(to EndPointAddress, payload []byte) error {
	return sendPayload(localSwitch, to, payload)
}

func (localSwitch *LocalSwitch) Close() error {
	return closeLocalSwitch(localSwitch)
}
