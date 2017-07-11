(import "io")
(import "bufio")
(import "time")

(type TransportAddr String)

(type EndPointId UInt32)
(type LightweightConnectionId UInt32)
(type HeavyweightConnectionId UInt32)
(type ConnectionId UInt64)

(struct TCPTransport
    transportAddr TransportAddr
    transportState (MVar TransportState)
    transportParams *TCPParameters)

(enum TransportState
    (TransPortValid ValidTransportState)
    TransportClosed)

(struct ValidTransportState
    _localEndPoints (Map EndPointId *LocalEndPoint)
    _nextEndPointId EndPointId)

(struct LocalEndPoint
    localAddress EndPointAddress
    localState  (MVar LocalEndPointState)
    localQueue   (Chan Event)
    shakeHand  ShakeHand)

(enum LocalEndPointState
    (LocalEndPointValid ValidLocalEndPointState)
    LocalEndPointClosed)
    
(struct ValidLocalEndPointState
    _localNextConnOutId   LightweightConnectionId
    _nextConnInId       HeavyweightConnectionId
    _localConnections   (Map EndPointAddress *RemoteEndPoint))

;;; REMOTE ENDPOINTS

(struct RemoteEndPoint
    remoteAddress EndPointAddress
    remoteState (MVar RemoteState)
    remoteId    HeavyweightConnectionId
    remoteScheduled     (Chan Action))

(enum RequestedBy
    RequestedByUs
    RequestedByThem)

(enum RemoteState
    ;; | Invalid remote endpoint (for example, invalid address)
    (RemoteEndPointInvalid ConnectErrorCode String)
    ;; | The remote endpoint is being initialized
    (RemoteEndPointInit Notifier Notifier RequestedBy)
    ;; | "Normal" working endpoint
    (RemoteEndPointValid ValidRemoteEndPointState)
    ;; | The remote endpoint is being closed (garbage collected)
    (RemoteEndPointClosing Notifier ValidRemoteEndPointState)
    ;; | The remote endpoint has been closed (garbage collected)
    RemoteEndPointClosed
    ;; | The remote endpoint has failed, or has been forcefully shutdown
    ;; using a closeTransport or closeEndPoint API call
    (RemoteEndPointFailed Error))

(struct ValidRemoteEndPointState
    _remoteOutgoing LightweightConnectionId
    _remoteIncoming (Set LightweightConnectionId)
    _remoteLastIncoming LightweightConnectionId
    _remoteNextConnOutId LightweightConnectionId
    remoteConn Conn
    ; remoteSendLock Lock
    ;; for batch send
    sendQueue    (Chan Sender)  ; send queue
    flushTimer   *ThrottleTimer ; flush writes as necessary but throttled.
    bufWriter   *bufio.Writer)

;;; Parameters for setting up the TCP transport
(struct TCPParameters
    ;; | Maximum length (in bytes) for a peer's address.
    ;; If a peer attempts to send an address of length exceeding the limit,
    ;; the connection will be refused (socket will close).
    tcpMaxAddressLength UInt32
    ;; | Maximum length (in bytes) to receive from a peer.
    ;; If a peer attempts to send data on a lightweight connection exceeding
    ;; the limit, the heavyweight connection which carries that lightweight
    ;; connection will go down. The peer and the local node will get an
    ;; EventConnectionLost.
    tcpMaxReceiveLength UInt32)

;;; macros

(defmacro withValidTransportState! [transport vst & body]
    (let [st (symbol (str "&" transport ".transportState"))
          v (symbol "st.value")]
        `(do
            (let st ~st)
            (lock! st)
            (match ~v
                [TransPortValid ~vst]
                (do ~@body)))))

(def defaultEndPointQueueCapacity 4096)

(impl ^*TCPTransport transport
    (defn removeLocalEndPoint 
        " | Remove reference to a local endpoint from the transport state

 Does nothing if the transport is closed"
        [^*LocalEndPoint ourEndPoint]
        (withValidTransportState! transport *vst
            (let epid ourEndPoint.localAddress.EndPointId)
            (dissoc vst._localEndPoints epid))))

(defmacro withValidLocalEndPointState! [ourEndPoint vst & body]
    (let [st (symbol (str "&" ourEndPoint ".localState"))
          v (symbol "st.value")]
        `(do
            (let st ~st)
            (lock! st)
            (match ~v
                [LocalEndPointValid ~vst]
                (do ~@body)))))

(impl ^*LocalEndPoint ourEndPoint
    (defn removeRemoteEndPoint 
        " | Remove reference to a remote endpoint from a local endpoint

 If the local endpoint is closed, do nothing"
        [^*RemoteEndPoint theirEndPoint]
        (withValidLocalEndPointState! ourEndPoint *vst
            (dissoc vst._localConnections theirEndPoint.remoteAddress))))

(def minWriteBufferSize 65536)
(def flushThrottleMS 100)

(type Sender "func (io.Writer)")

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
    
(defmacro message! [n]
    `(do (encode! ~n)
        (decode! ~n)))

(message! ControlHeader)
(message! ConnectionRequestResponse)

;;; struct methods
(impl ^*LocalEndPoint ourEndPoint
    (defn enqueue 
        [^Event e]
        (>! ourEndPoint.localQueue e)))


(impl ^*ValidRemoteEndPointState vst
    (defn sendOn 
        "| Send a payload over a heavyweight connection (thread safe)"
        [^Sender sender]
        ; (lock! vst.remoteSendLock)
        ; (sender vst.bufWriter)
        (>! vst.sendQueue sender))
        ; (vst.flushTimer.Set))

    (defn flush []
        ; (lock! vst.remoteSendLock)
        (println "flushing...")
        (let err (vst.bufWriter.Flush))
        (when (not= err nil)
            (println "warn flush failed " err)))
                
    (defn sendRoutine
        "batch send"
        []
        (forever
            (alt!
                vst.flushTimer.Ch ([_] (vst.flush))
                vst.sendQueue ([sender]
                               (sender vst.bufWriter)
                               (vst.flushTimer.Set))))))
            

;;; constructor
(defn newTransportState ^TransportState []
  (return
    (&TransPortValid.
        (map->ValidTransportState {_nextEndPointId 0}))))

(defn newTCPTransport ^*TCPTransport 
    [^string lAddr ^*TCPParameters params]
    (let state (newTransportState))
    (return
        (map->&TCPTransport {transportAddr (TransportAddr lAddr)
                             transportState (^TransportState newMVar state)
                             transportParams params})))

(defn newLocalEndPointState ^LocalEndPointState []
  (return
    (&LocalEndPointValid.
        (map->ValidLocalEndPointState {_localNextConnOutId firstNonReservedLightweightConnectionId,
                                       _nextConnInId       firstNonReservedHeavyweightConnectionId}))))
        
;; | We reserve a bunch of connection IDs for control messages
(def firstNonReservedLightweightConnectionId 
    (LightweightConnectionId 1024))

;; | We reserve some connection IDs for special heavyweight connections
(def firstNonReservedHeavyweightConnectionId 
    (HeavyweightConnectionId 1))
  
(defn newRemoteEndPointValid ^*RemoteEndPointValid [^Conn conn]
    (return
      (&RemoteEndPointValid.
        (map->ValidRemoteEndPointState {remoteConn conn
                                        _remoteNextConnOutId firstNonReservedLightweightConnectionId
                                        sendQueue (native "make(chan Sender, 1000)")
                                        flushTimer (NewThrottleTimer "flush" flushThrottleMS*time.Millisecond)
                                        bufWriter (bufio.NewWriterSize conn minWriteBufferSize)}))))

;;;------------------------------------------------------------------------------
;;; API functions                                                              --
;;;------------------------------------------------------------------------------

(impl ^*LocalEndPoint ourEndPoint
    (defn apiClose
        "| Close a connection"
        ^Error [^*RemoteEndPoint theirEndPoint ^LightweightConnectionId connId ^*AtomicBool connAlive]
        (println "apiClose:", ourEndPoint.localAddress, "->", theirEndPoint.remoteAddress, connId)
        (let st ((fn ^*ValidRemoteEndPointState []
                        (let theirState &theirEndPoint.remoteState)
                        (matchMVar! theirState
                            [RemoteEndPointValid *vst]
                            (if (connAlive.IsSet)
                                (do
                                    (connAlive.UnSet)
                                    vst._remoteOutgoing--
                                    (println "	remoteOutgoing--:" vst._remoteOutgoing)
                                    (return vst))
                                (return nil)))
                        (return nil))))
        
        (when (not (nil? st))
            (st.sendOn (fn [^io.Writer conn]
                           (sendCloseConnection (uint32 connId) conn))))
        (ourEndPoint.closeIfUnused theirEndPoint)
        (return nil)))
