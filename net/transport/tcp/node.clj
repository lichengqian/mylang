
(type ChannelID UInt64)


(struct LocalNode
    localEndPoint   *EndPoint
    localState      (MVar LocalNodeState)
    localCtrlChan   (Chan NCMsg))


(enum LocalNodeState
    (LocalNodeValid ValidLocalNodeState)
    LocalNodeClosed)


(type OutgoingConnectionMap (Map ChannelID (Map EndPointAddress *Connection)))


(struct ValidLocalNodeState
    localSwitches       (Map ChannelID *LocalChannel)
    ;; | Outgoing connections
    localConnections    OutgoingConnectionMap)


(defmacro withValidLocalNodeState! [node vst & body]
    (let [st (symbol (str "&" node ".localState"))
          v (symbol "st.value")]
        `(let* [st ~st]
            (lock! st)
            (match ~v
                [LocalNodeValid ~vst]
                (do ~@body)))))


(impl ^*LocalNode localNode
    (defn getLocalChannel ^*LocalChannel [^ChannelID chanID]
        (withValidLocalNodeState! localNode vst
            (return (get vst.localSwitches chanID)))

            ; LocalNodeClosed
        (return nil))

    (defn getLocalConnection ^*Connection
        [^ChannelID from ^EndPointAddress to]
        (withValidLocalNodeState! localNode vst
            (return
                (get-in vst.localConnections [from to])))
        
        (return nil)))


(def defaultChannelQueueCapacity 4096)


(struct LocalChannel
    channelID       ChannelID
    localNode       *LocalNode
    Queue           (Chan Message)
    onConnect       (fn [EndPointAddress]))


;;;------------------------------------------------------------------------------
;;; Messages                                                                   --
;;;------------------------------------------------------------------------------
(struct Message
    From        EndPointAddress
    Payload     ByteString)


(defn NewLocalNode ^*LocalNode [^*Transport transport, ^ShakeHand shake]
    (<- endpoint (transport.NewEndPoint 0 shake))
    (let 
        st (LocalNodeValid. (map->ValidLocalNodeState 
                                {localConnections (newOutgoingConnectionMap)}))
        node (map->LocalNode
                {localEndPoint endpoint
                 localState (^LocalNodeState newMVar &st)})

        stopNC  (fn []
                    (>! node.localCtrlChan (NCMsg. (node.localEndPoint.Address) (SigShutdown.)))))

    ;; Once the NC terminates, the endpoint isn't much use,
    (go (try (nodeController &node)
             (finally (node.localEndPoint.Close))))

    ;; whilst a closed/failing endpoint will terminate the NC
    (go (try (handleNodeMessages &node)
             (finally (stopNC))))

    (return &node))


(defn NewLocalChannel ^*LocalChannel
    [^*LocalNode localNode, ^ChannelID sid, ^"func (EndPointAddress)" onConnect]

    (withValidLocalNodeState! localNode vst
        (let localChannel (map->LocalChannel {channelID sid, 
                                              localNode localNode,
                                              Queue (^Message chan defaultChannelQueueCapacity)
                                              onConnect onConnect})) 
                            
        (when (== nil (get vst.localSwitches sid))
            (.put vst.localSwitches sid &localChannel)
            (return &localChannel))                     
        (throw (str "local channel exist!" sid)))

    ; LocalNodeClosed
    (throw "local node closed"))


(defn CloseLocalChannel
    [^*LocalChannel localChannel]

    (withValidLocalNodeState! localChannel.localNode vst
        (let localSwitch_ (get vst.localSwitches localChannel.channelID))
        (if (nil? localSwitch_)
            (throw "local switch closed")
            (do
                (.remove vst.localSwitches localChannel.channelID)
                (.remove vst.localConnections localChannel.channelID)
                (return nil))))
    ; LocalNodeClosed
    (throw "local node closed"))
    
;;;------------------------------------------------------------------------------
;;; Handle incoming messages                                                   --
;;;------------------------------------------------------------------------------

(struct IncomingConnection
    theirAddress        EndPointAddress
    theirTarget         IncomingTarget)


(enum IncomingTarget
    Uninit
    (ToChannel *LocalChannel)
    ToNode)


(type IncomingConnectionMap (Map EndPointAddress (Set ConnectionId)))


(struct ConnectionState
    incoming        (Map ConnectionId *IncomingConnection)
    incomingFrom    IncomingConnectionMap)


(defn initConnectionState ^*ConnectionState []
    (return
        (map->&ConnectionState
            {incomingFrom (newIncomingConnectionMap)})))


(defn handleNodeMessages [^*LocalNode localNode]
    (let 
        st (initConnectionState)
         
        invalidRequest  (fn [^ConnectionId cid ^String msg]
                            (.remove  st.incoming, cid))

        onConnectionOpened  (fn [^ConnectionId cid, ^EndPointAddress ep]
                                (.put st.incoming cid (&IncomingConnection. ep (Uninit.)))
                                (assoc-in st.incomingFrom [ep] cid))

        onConnectionClosed  (fn [^ConnectionId cid]
                                (let pConn (get st.incoming cid))
                                (if (nil? pConn)
                                    (invalidRequest cid "closed unknown connection")
                                    (do
                                        (.remove st.incoming cid)
                                        (.remove (get st.incomingFrom pConn.theirAddress) cid))))
        
        onReceived  (fn [^ConnectionId cid, ^ByteString payload]
                        (let pConn (get st.incoming cid))
                        (if (nil? pConn)
                            (invalidRequest cid "message received from an unknown connection")

                            (match pConn.theirTarget
                                Uninit
                                (do
                                    (let 
                                        pSwitch (localNode.getLocalChannel
                                                    (decodeChannelID payload)))
                                    
                                    (if (nil? pSwitch)
                                        (.remove  st.incoming cid)
                                        (do
                                            (.put st.incoming cid 
                                                (&IncomingConnection. pConn.theirAddress (&ToChannel. pSwitch)))
                                            ;; call onConnect callback
                                            (when (not (nil? pSwitch.onConnect))
                                                (pSwitch.onConnect pConn.theirAddress)))))

                                [ToChannel pSwitch]
                                (do
                                    (println pSwitch.channelID payload)
                                    (>! pSwitch.Queue 
                                        (Message. pConn.theirAddress payload))))))

        onErrorEvent    (fn ^Bool [^EventErrorCode errcode ^Error err]
                            (println errcode err)
                            (match errcode
                                [EventConnectionLost addr]
                                (do
                                    (native
                                        "for cid, _ := range st.incomingFrom[addr] {"
                                        "   delete(st.incoming, cid)"
                                        "}")
                                    (.remove  st.incomingFrom addr))

                                EventEndPointFailed (return true)
                                EventTransportFailed (return true))
                            (return false)))

    (println "handling node message...") 
    (forever
        (let event (localNode.localEndPoint.Receive))
        (match event
            [ConnectionOpened cid ep]
            (onConnectionOpened cid ep)

            [Received cid payload]  
            (onReceived cid payload)

            [ConnectionClosed cid]
            (onConnectionClosed cid)

            [ErrorEvent errcode err]
            (do
                (let exit (onErrorEvent errcode err))
                (when exit
                    return))
            
            EndPointClosed 
            return)))

;;;------------------------------------------------------------------------------
;;; Message sending                                                            --
;;;------------------------------------------------------------------------------

(defn DialTo
    [^*LocalChannel localChannel, ^EndPointAddress to]
    (let node localChannel.localNode)
    (<- _ (connBetween node localChannel.channelID to))
    (return nil))


(defn SendPayload
    [^*LocalChannel LocalChannel ^EndPointAddress to ^ByteString payload]
    (let node LocalChannel.localNode)
    (<- conn (connBetween node LocalChannel.channelID to))
    (<- bytes (conn.Send payload))
    (println bytes)
    ; (>! node.localCtrlChan (NCMsg. to (&Died. to (DiedDisconnect.))))
    (return nil))


(defn setupConnBetween ^*Connection
    [^*LocalNode node ^ChannelID from ^EndPointAddress to]
    (<- conn (node.localEndPoint.Dial to))
    (<- nbytes (conn.Write (encodeChannelID from)))
    (when (== nbytes 8)
        (lock! node.localState)
        (match node.localState.value
            [LocalNodeValid vst]
            (assoc-in vst.localConnections [from to] conn))
        (return conn))
    (throw "conn failed"))


(defn connBetween ^*Connection
    [^*LocalNode node ^ChannelID from ^EndPointAddress to]
    (let conn 
        (node.getLocalConnection from to))
    
    (when (nil? conn)
        (<- newconn (setupConnBetween node from to))
        (return newconn))
    (return conn))

;;;------------------------------------------------------------------------------
;;; Node controller internal data types                                        --
;;;------------------------------------------------------------------------------
(native
    "type Identifier interface {
     tagIdentifier() uint8
    }
    func (addr EndPointAddress) tagIdentifier() uint8 {
     return 0
    }")


;;; | Why did a channel die?
(enum DiedReason
    DiedDisconnect
    DiedNodeDown)


;;; | Messages to the node controller
(struct NCMsg
    ctrlMsgSender       Identifier
    ctrlMsgSignal       Signal)


;;; | Signals to the node controller (see 'NCMsg')
(enum Signal
    (Died Identifier DiedReason)
    (Kill ChannelID String)
    SigShutdown)


;;;------------------------------------------------------------------------------
;;; Top-level access to the node controller                                    --
;;;------------------------------------------------------------------------------

(defn nodeController [^*LocalNode node]
    (forever
        (let msg (<! node.localCtrlChan))

        (match msg.ctrlMsgSignal
            [Died ident reason]
            (println "Died:" ident reason)

            SigShutdown
            (do
                (node.localEndPoint.Close)
                return))))

;;;------------------------------------------------------------------------------
;;; Internal data types                                                        --
;;;------------------------------------------------------------------------------
