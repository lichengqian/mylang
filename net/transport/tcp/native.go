package tcp

import (
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

func (tp TransportAddr) encodeEndPointAddress(epid EndPointId) *EndPointAddress {
	return EndPointAddress{tp, epid}
}

func decodeEndPointAddress(bs []byte) *EndPointAddress {
	//TODO: decode endpoint address
	return nil
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

func checkPeer(conn net.Conn, epAddr *EndPointAddress) bool {
	//TODO: check remote ip and port
	return true
}