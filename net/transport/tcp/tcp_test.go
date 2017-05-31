package tcp

import (
	"errors"
	"sync"
	"testing"
)

type S struct {
	state struct {
		value Event
		lock  sync.RWMutex
	}
}

func testEvent(e Event) {
	switch pe := e.(type) {
	case EndPointClosed:
		println(pe.tagEvent(), pe.String())
	case *ErrorEvent:
		println(pe.tagEvent(), pe._1, pe.String())
		// pe._1 += "!"
	default:
		println("unknown Event")
	}
}

func TestError(t *testing.T) {
	ps := &S{struct {
		value Event
		lock  sync.RWMutex
	}{value: EndPointClosed{}}}

	ps.state.lock.Lock()
	defer ps.state.lock.Unlock()

	testEvent(ps.state.value)
	ps.state.value = &EndPointClosed{}
	testEvent(ps.state.value)
	// testE(EC{"ecmsg"}) .    compiler error
	ps.state.value = &ErrorEvent{errors.New("err"), "ecmsg"}
	testEvent(ps.state.value)
	// pc.msg = "ecmsg2"
	testEvent(ps.state.value)
	ps.state.value = nil
	testEvent(ps.state.value)

	lcid := LightweightConnectionId(1)
	hcid := HeavyweightConnectionId(1)
	cid := createConnectionId(hcid, lcid)
	println(cid)
}

const (
	testAddr = "127.0.0.1:9999"
)

func TestTransport(t *testing.T) {
	tp, err := createTCPTransport(testAddr)
	if err != nil {
		t.Error(err)
		return
	}

	ep, errCode := tp.createLocalEndPoint(1000)
	if errCode != nil {
		t.Error(errors.New(errCode.String()))
		return
	}

	println("testing create endpoint on exist endpoit id")
	_, errCode = tp.createLocalEndPoint(1000)
	if errCode.String() != "NewEndPointFailed" {
		t.Error(errors.New(errCode.String()))
		return
	}

	println("testing connect to noexist endpoint")

	ep.Close()
	// conn, err := ep.Dial(EndPointAddress{testAddr, 900})
	// if err.Error() != "ConnectionRequestInvalid" {
	// 	t.Error(err)
	// 	return
	// }

	// println("testing connect to valid endpoint")
	// conn, err = ep.Dial(EndPointAddress{testAddr, 1000})
	// if err != nil {
	// 	t.Error(err)
	// 	return
	// }
	// println(conn.LocalAddr)

	// println("closing endpoint...")
	// if err = ep.Close(); err != nil {
	// 	t.Error(err)
	// 	return
	// }

	// println("connecting to a closed endpoint")
	// conn, err = ep.Dial(EndPointAddress{testAddr, 1000})
	// if err.Error() != "ConnectionRequestInvalid" {
	// 	t.Error(err)
	// 	return
	// }
}
