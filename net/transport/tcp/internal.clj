(ns tcp)

(enum ControlHeader
    CreateNewConnection
    CloseConnection
    CloseSocket
    CloseEndPoint
    ProbeSocket
    ProbeSocketAct)

(enum ConnectionRequestResponse
    ConnectionRequestAccepted
    ConnectionRequestInvalid
    ConnectionRequestCrossed
    ConnectionRequestHostMismatch)

