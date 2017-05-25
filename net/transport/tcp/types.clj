(ns tcp)

(type EndPointId UInt32)

(enum NewEndPointErrorCode
    NewEndPointInsufficientResources
    NewEndPointFailed)

(struct TCPTransport
    transportAddr TransportAddr
    listener Listener
    *transportState TransportState)

(struct TransportState
    _localEndPoints (Map EndPointId LocalEndPoint)
    _nextEndPointId EndPointId)

(struct LocalEndPoint
    localAddress EndPointAddress
    connections  (Chan Conn)
    closeLocalEndPoint (-> Error))

(enum ConnectionRequestResponse
    ConnectionRequestAccepted
    ConnectionRequestInvalid
    ConnectionRequestCrossed
    ConnectionRequestHostMismatch)

; (enum ConnectErrorCode
;     ConnectNotFound
;     ConnectInsufficientResources
;     ConnectTimeout
;     ConnectFailed)

; (enum RemoteState
;     (RemoteEndPointInvalid ConnectErrorCode String)
;     (RemoteEndPointInit Lock Lock RequestedBy)
;     (RemoteEndPointValid RemoteEndPointState)
;     (RemoteEndPointClosing Lock RemoteEndPointState)
;     RemoteEndPointClosed
;     (RemoteEndPointFailed Error))

; (struct RemoteEndPointState
;     remoteConn Conn
;     remoteSendLock Lock
;     )

; (struct LocalEndPointState
;     localConnections (Map EndPointAddress RemoteEndPoint))

; (struct RemoteEndPoint
;     remoteAddress EndPointAddress
;     *remoteState RemoteState)

; (enum RequestedBy
;     RequestedByUs
;     RequestedByThem)

