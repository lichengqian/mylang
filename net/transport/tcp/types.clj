(ns tcp)

(type EndPointId UInt32)
(type LightweightConnectionId UInt32)
(type HeavyweightConnectionId UInt32)
(type ConnectionId UInt64)

(struct TCPTransport
    transportAddr TransportAddr
    transportState (MVar TransportState))

(enum TransportState
    (TransPortValid ValidTransportState)
    TransportClosed)

(struct ValidTransportState
    _localEndPoints (Map EndPointId LocalEndPoint)
    _nextEndPointId EndPointId)

(struct LocalEndPoint
    localAddress EndPointAddress
    localState  (MVar LocalEndPointState)
    localQueue   (Chan Event)
    closeLocalEndPoint "func() error")

(enum LocalEndPointState
    (LocalEndPointValid ValidLocalEndPointState)
    LocalEndPointClosed)
    
(struct ValidLocalEndPointState
    _localNextConnOutId   LightweightConnectionId
    _nextConnInId       HeavyweightConnectionId
    _localConnections   (Map EndPointAddress RemoteEndPoint))

(struct RemoteEndPoint
    remoteAddress EndPointAddress
    remoteState (MVar RemoteState)
    remoteId    HeavyweightConnectionId)

(enum RequestedBy
    RequestedByUs
    RequestedByThem)

(enum RemoteState
    (RemoteEndPointInvalid ConnectErrorCode String)
    (RemoteEndPointInit Lock Lock RequestedBy)
    (RemoteEndPointValid ValidRemoteEndPointState)
    (RemoteEndPointClosing Lock ValidRemoteEndPointState)
    RemoteEndPointClosed
    (RemoteEndPointFailed Error))

(struct ValidRemoteEndPointState
    _remoteOutgoing LightweightConnectionId
    _remoteIncoming (Set LightweightConnectionId)
    _remoteLastIncoming LightweightConnectionId
    _remoteNextConnOutId LightweightConnectionId
    remoteConn Conn
    remoteSendLock Lock)
    


(enum Event
    "Event on an endpoint.
    test multiline comment"
    (Received ConnectionId ByteString)
    (ConnectionClosed ConnectionId)
    (ConnectionOpened ConnectionId EndPointAddress)
    EndPointClosed
    (ErrorEvent Error string))

(enum ConnectErrorCode
    ConnectNotFound
    ConnectInsufficientResources
    ConnectTimeout
    ConnectFailed)

(enum NewEndPointErrorCode
    NewEndPointInsufficientResources
    NewEndPointFailed)

(enum EventErrorCode
    "Error codes used when reporting errors to endpoints (through receive)"
    EventEndPointFailed
    EventTransportFailed
    (EventConnectionLost EndPointAddress))

; (native :golang " 
; type test string
;     ")


; (fn test []
;     (println "hello")
;     [(not 1) nil])
