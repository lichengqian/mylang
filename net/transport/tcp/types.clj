
(type TransportAddr String)

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

(defmacro withValidTransportState! [transport vst & body]
    (let [st (symbol (str "&" transport ".transportState"))
          v (symbol "st.value")]
        `(do
            (let st ~st)
            (lock! st)
            (match ~v
                [TransPortValid ~vst]
                (do ~@body)))))

(struct ValidTransportState
    _localEndPoints (Map EndPointId *LocalEndPoint)
    _nextEndPointId EndPointId)

(struct LocalEndPoint
    localAddress EndPointAddress
    localState  (MVar LocalEndPointState)
    localQueue   (Chan Event)
    shakeHand  ShakeHand)

(impl ^*TCPTransport transport
    (defn removeLocalEndPoint 
        " | Remove reference to a local endpoint from the transport state

 Does nothing if the transport is closed"
        [^*LocalEndPoint ourEndPoint]
        (withValidTransportState! transport vst
            (let epid ourEndPoint.localAddress.EndPointId)
            (dissoc vst._localEndPoints epid))))

(enum LocalEndPointState
    (LocalEndPointValid ValidLocalEndPointState)
    LocalEndPointClosed)
    
(defmacro withValidLocalEndPointState! [ourEndPoint vst & body]
    (let [st (symbol (str "&" ourEndPoint ".localState"))
          v (symbol "st.value")]
        `(do
            (let st ~st)
            (lock! st)
            (match ~v
                [LocalEndPointValid ~vst]
                (do ~@body)))))

(struct ValidLocalEndPointState
    _localNextConnOutId   LightweightConnectionId
    _nextConnInId       HeavyweightConnectionId
    _localConnections   (Map EndPointAddress *RemoteEndPoint))

(struct RemoteEndPoint
    remoteAddress EndPointAddress
    remoteState (MVar RemoteState)
    remoteId    HeavyweightConnectionId
    remoteScheduled     (Chan Action))

(impl ^*LocalEndPoint ourEndPoint
    (defn removeRemoteEndPoint 
        " | Remove reference to a remote endpoint from a local endpoint

 If the local endpoint is closed, do nothing"
        [^*RemoteEndPoint theirEndPoint]
        (withValidLocalEndPointState! ourEndPoint vst
            (dissoc vst._localConnections theirEndPoint.remoteAddress))))

(enum RequestedBy
    RequestedByUs
    RequestedByThem)

(enum RemoteState
    (RemoteEndPointInvalid ConnectErrorCode String)
    (RemoteEndPointInit Notifier Notifier RequestedBy)
    (RemoteEndPointValid ValidRemoteEndPointState)
    (RemoteEndPointClosing Notifier ValidRemoteEndPointState)
    RemoteEndPointClosed
    (RemoteEndPointFailed Error))

(struct ValidRemoteEndPointState
    _remoteOutgoing LightweightConnectionId
    _remoteIncoming (Set LightweightConnectionId)
    _remoteLastIncoming LightweightConnectionId
    _remoteNextConnOutId LightweightConnectionId
    remoteConn Conn
    remoteSendLock Lock)
    

;;; transport definition

(enum Event
    "Event on an endpoint."
    (Received ConnectionId ByteString)
    (ConnectionClosed ConnectionId)
    (ConnectionOpened ConnectionId EndPointAddress)
    EndPointClosed
    (ErrorEvent EventErrorCode Error))

; (enum NewEndPointErrorCode
;     "Errors during the creation of an endpoint"
;     NewEndPointInsufficientResources
;     NewEndPointFailed)

(enum ConnectErrorCode
    "Connection failure"
    ConnectNotFound
    ConnectInsufficientResources
    ConnectTimeout
    ConnectFailed)

(enum EventErrorCode
    "Error codes used when reporting errors to endpoints (through receive)"
    EventEndPointFailed
    EventTransportFailed
    (EventConnectionLost EndPointAddress))

;;; internal
(enum ControlHeader
    CreateNewConnection
    CloseConnection
    CloseSocket
    CloseEndPoint)
    ; ProbeSocket
    ; ProbeSocketAct)

(enum ConnectionRequestResponse
    "Response sent by /B/ to /A/ when /A/ tries to connect"
    ConnectionRequestAccepted
    ConnectionRequestInvalid
    ConnectionRequestCrossed
    ConnectionRequestHostMismatch)

(deferr
    TransportClosed "Transport closed"
    EndPointClosed  "EndPoint closed"
    ConnectionClosed "Connection closed")
    
(defmacro encode! [n]
    (cl-format nil "func encode~A(n ~A) uint8 { return n.tag~A() }"
        n n n))

(defmacro decode! [n]
    (cl-format nil "func decode~A(tag uint8) ~A {~% switch tag { ~{case ~D: return ~A{}~%~} default: return nil~% }~%}" 
        n n (interleave (range) (get-enum n))))

(defmacro message! [& names]
    (for [n names]
        `(do (encode! ~n)
            (decode! ~n))))

(message! ControlHeader ConnectionRequestResponse)
