(ns tcp)

(enum ControlHeader
    CreateNewConnection
    CloseConnection
    CloseSocket
    CloseEndPoint
    ProbeSocket
    ProbeSocketAct)

(enum ConnectionRequestResponse
    "Response sent by /B/ to /A/ when /A/ tries to connect"
    ConnectionRequestAccepted
    ConnectionRequestInvalid
    ConnectionRequestCrossed
    ConnectionRequestHostMismatch)

