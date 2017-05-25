package tcp

import "testing"

func TestError(t *testing.T) {
	err2 := NewEndPointFailed
	println(err2.String())

}

const (
	testAddr = "127.0.0.1:9999"
)

func TestTransport(t *testing.T) {
	tp, err := CreateTransport(testAddr)
	if err != nil {
		t.Error(err)
		return
	}

	ep, err := tp.Listen(1000)
	if err != nil {
		t.Error(err)
		return
	}

	go ep.Accept()

	println("testing create endpoint on exist endpoit id")
	_, err = tp.Listen(1000)
	if err.Error() != "NewEndPointFailed" {
		t.Error(err)
		return
	}

	println("testing connect to noexist endpoint")

	conn, err := ep.Dial(EndPointAddress{testAddr, 900})
	if err.Error() != "ConnectionRequestInvalid" {
		t.Error(err)
		return
	}

	println("testing connect to valid endpoint")
	conn, err = ep.Dial(EndPointAddress{testAddr, 1000})
	if err != nil {
		t.Error(err)
		return
	}
	println(conn.LocalAddr)

	println("closing endpoint...")
	if err = ep.Close(); err != nil {
		t.Error(err)
		return
	}

	println("connecting to a closed endpoint")
	conn, err = ep.Dial(EndPointAddress{testAddr, 1000})
	if err.Error() != "ConnectionRequestInvalid" {
		t.Error(err)
		return
	}
}
