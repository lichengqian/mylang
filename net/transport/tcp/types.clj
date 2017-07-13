(import "io")
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
    bufWriter   BufferedOutputStream)

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
            (.remove vst._localEndPoints epid))))

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
            (.remove vst._localConnections theirEndPoint.remoteAddress))))

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
        (let err (.flush vst.bufWriter))
        (when (not= err nil)
            (println "warn flush failed " err))))
                
(impl ^*RemoteEndPointValid st
    (defn sendRoutine
        "batch send"
        []
        (let vst &st._1)
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
                                        bufWriter (BufferedOutputStream. conn minWriteBufferSize)}))))

(defn createTCPTransport
    ^*TCPTransport
    [^string lAddr]
    (let tp (newTCPTransport lAddr defaultTCPParameters))
    (<- (tp.forkServer tp.handleConnectionRequest))
    (return tp))

;;;------------------------------------------------------------------------------

;;; API functions                                                              --
;;;------------------------------------------------------------------------------

;;; | Create a new endpoint
(impl ^*TCPTransport tp
    (defn apiNewEndPoint
        ^*EndPoint
        [^EndPointId epid, ^ShakeHand shake]
        (<- ourEndPoint (tp.createLocalEndPoint epid shake))
        (return (map->&EndPoint {Close (fn ^Error []
                                            (return (tp.apiCloseEndPoint 
                                                        (native "[]Event{EndPointClosed{}}")
                                                        ourEndPoint)))
                                 Dial (fn ^"*Connection, error" [^EndPointAddress theirAddress]
                                            (return (tp.transportParams.apiConnect ourEndPoint theirAddress)))
                                 Receive (fn ^Event []
                                            (return (<! ourEndPoint.localQueue)))
                                 Address (fn ^EndPointAddress []
                                            (return ourEndPoint.localAddress))}))))

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
        (return nil))

    (defn apiSend
        "| Send data across a connection"
        ^"int, error"
        [^*RemoteEndPoint theirEndPoint 
         ^LightweightConnectionId connId
         ^ByteString msg
         ^*AtomicBool connAlive]
        (println "apiSend" connId (count msg))
        (let theirState &theirEndPoint.remoteState)
        (matchMVar! theirState
            [RemoteEndPointInvalid]
            (return 0 (errors.New "apiSend RemoteEndPointInvalid"))
            [RemoteEndPointInit]
            (return 0 (errors.New "apiSend RemoteEndPointInit"))
            [RemoteEndPointClosing]
            (if (connAlive.IsSet)
                (return 0 (errors.New "apiSend RemoteEndPointClosing"))
                (return 0 ErrConnectionClosed))
            RemoteEndPointClosed
            (if (connAlive.IsSet)
                (return 0 (errors.New "apiSend RemoteEndPointClosed"))
                (return 0 ErrConnectionClosed))
            [RemoteEndPointFailed err]
            (if (connAlive.IsSet)
                (return 0 err)
                (return 0 ErrConnectionClosed))
            [RemoteEndPointValid *vst]
            (if (connAlive.IsSet)
                (do
                    (vst.sendOn
                        (fn [^io.Writer conn]
                            (connId.sendMsg msg conn)))
                    (return (count msg) nil))
                (return 0 ErrConnectionClosed)))
        (return 0 (errors.New "apiSend error")))

    (defn closeIfUnused
        "| Send a CloseSocket request if the remote endpoint is unused"
        [^*RemoteEndPoint theirEndPoint]
        (let theirState &theirEndPoint.remoteState)
        (matchMVar! theirState
            [RemoteEndPointValid *vst]
            (when (and  (= vst._remoteOutgoing 0)
                        (= (count vst._remoteIncoming) 0))
                (set theirState.value
                    (&RemoteEndPointClosing. (newNotifier) *vst))
                (println "close unused connection to ", theirEndPoint.remoteAddress)
                (vst.sendOn
                    (fn [^io.Writer conn]
                        (sendCloseSocket (uint32 vst._remoteLastIncoming) conn))))))

    (defn getRemoteEndPoint
        ^"*RemoteEndPoint, error"
        [^EndPointAddress theirAddress]
        (let ourState &ourEndPoint.localState)
        (matchMVar! ourState
            [LocalEndPointValid *vst]
            (return (get vst._localConnections theirAddress) nil))
        (return nil ErrEndPointClosed))
    ;; | Reset a remote endpoint if it is in Invalid mode
    ;;
    ;; If the remote endpoint is currently in broken state, and
    ;;
    ;;   - a user calls the API function 'connect', or and the remote endpoint is
    ;;   - an inbound connection request comes in from this remote address
    ;;
    ;; we remove the remote endpoint first.
    ;;
    ;; Throws a TransportError ConnectFailed exception if the local endpoint is
    ;; closed.
    (defn resetIfBroken
        [^EndPointAddress theirAddress]
        (<- theirEndPoint
            (ourEndPoint.getRemoteEndPoint theirAddress))
        (when (not (nil? theirEndPoint))
            (let theirState &theirEndPoint.remoteState)
            (matchMVar! theirState
                [RemoteEndPointInvalid]
                (ourEndPoint.removeRemoteEndPoint theirEndPoint)
                [RemoteEndPointFailed e]
                (do
                    (println "resetIfBroken" e)
                    (ourEndPoint.removeRemoteEndPoint theirEndPoint))))
        (return nil))
            
    ;; Resolve an endpoint currently in 'Init' state
    (defn resolveInit ^Error
        [^*RemoteEndPoint theirEndPoint, ^RemoteState newState]
        (let theirState &theirEndPoint.remoteState
            setState (fn []
                        (match newState
                            RemoteEndPointClosed
                            (ourEndPoint.removeRemoteEndPoint theirEndPoint)

                            (set theirState.value newState))))

        (matchMVar! theirState
            [RemoteEndPointInit resolved]
            (do
                (notify resolved)
                (setState)
                (return nil))
            [RemoteEndPointFailed e]
            (return e))
        (return (errors.New "resolveInit")))

    ;; | Set up a remote endpoint
    (defn setupRemoteEndPoint
        ^"ConnectionRequestResponse, error"
        [^*TCPParameters params, ^*RemoteEndPoint theirEndPoint]
        (let ourAddress ourEndPoint.localAddress
             theirAddress theirEndPoint.remoteAddress
             [sock rsp err] (socketToEndPoint ourAddress theirAddress ourEndPoint.shakeHand))
        (when (not (nil? err))
            (ourEndPoint.resolveInit theirEndPoint (&RemoteEndPointInvalid. nil (err.Error)))
            (return nil err))
        
        (let theirState &theirEndPoint.remoteState)
        (match rsp
            ConnectionRequestAccepted
            (do
                (let st (newRemoteEndPointValid sock))
                (ourEndPoint.resolveInit theirEndPoint st)

                (go (st.sendRoutine))
                (go (try (params.handleIncomingMessages ourEndPoint theirEndPoint)
                         (finally (sock.Close)))))
            
            ConnectionRequestInvalid
            (try (let st (&RemoteEndPointInvalid. (ConnectNotFound.) "setupRemoteEndPoint: Invalid endpoint"))
                 (ourEndPoint.resolveInit theirEndPoint st)
                 (finally (sock.Close)))

            ConnectionRequestCrossed
            (try (matchMVar! theirState
                    [RemoteEndPointInit _ crossed _]
                    (notify crossed)
                    [RemoteEndPointFailed e]
                    (return rsp, e)

                    (ourEndPoint.relyViolation "setupRemoteEndPoint: Crossed"))
                 (finally (sock.Close)))

            ConnectionRequestHostMismatch
            (try (let msg "setupRemoteEndPoint: Host mismatch "
                      st (&RemoteEndPointInvalid. (ConnectFailed.) msg))
                 (ourEndPoint.resolveInit theirEndPoint st)
                 (finally (sock.Close))))

        (return rsp nil)))

(impl ^*TCPTransport tp
    ;;; | Create a new local endpoint
    ;;;
    ;;; May throw a TransportError NewEndPointErrorCode exception if the transport
    ;;; is closed.
    (defn createLocalEndPoint
        ^"*LocalEndPoint, error"
        [^EndPointId epid, ^ShakeHand shake]
        (let tpState &tp.transportState)
        (matchMVar! tpState
            [TransPortValid vst]
            (do
                (let endpoints vst._localEndPoints)
                (when (contains? endpoints epid)
                    (return nil (errors.New "endpoint already exist")))
                (.put endpoints epid
                    (map->&LocalEndPoint {localAddress (EndPointAddress. tp.transportAddr epid)
                                          localState (^LocalEndPointState newMVar (newLocalEndPointState))
                                          localQueue (native "make(chan Event, defaultEndPointQueueCapacity)")
                                          shakeHand shake}))
                (return (get endpoints epid) nil)))
        (return nil ErrTransportClosed)))
