(ns tcp)

(type EndPointId UInt32)
(type LightweightConnectionId UInt32)
(type HeavyweightConnectionId UInt32)
(type ConnectionId UInt64)

(enum NewEndPointErrorCode
    NewEndPointInsufficientResources
    NewEndPointFailed)

(struct TCPTransport
    transportAddr TransportAddr
    *transportState TransportState)

(struct TransportState
    _localEndPoints (Map EndPointId LocalEndPoint)
    _nextEndPointId EndPointId)

(struct LocalEndPoint
    localAddress EndPointAddress
    *localState  LocalEndPointState
    localQueue   (Chan Event)
    closeLocalEndPoint (-> (IO Error)))

(enum Event
    "Event on an endpoint."
    (Received ConnectionId ByteString)
    (ConnectionClosed ConnectionId)
    (ConnectionOpened ConnectionId EndPointAddress)
    EndPointClosed
    (ErrorEvent string))

(struct LocalEndPointState
    _localNextConnOutId   LightweightConnectionId
    _nextConnInId       HeavyweightConnectionId
    _localConnections (Map EndPointAddress RemoteEndPoint))

(struct RemoteEndPoint
    remoteAddress EndPointAddress
    *remoteState RemoteState
    remoteId    HeavyweightConnectionId)

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
    remoteSendLock Lock
    )

(enum RequestedBy
    RequestedByUs
    RequestedByThem)

(enum ConnectErrorCode
    ConnectNotFound
    ConnectInsufficientResources
    ConnectTimeout
    ConnectFailed)

(fn test []
    (println "hello")
    [(not 1) nil])