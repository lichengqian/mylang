

(type ChannelID UInt64)

(struct LocalNode
    localEndPoint *EndPoint
    localState  (MVar LocalNodeState)
    localCtrlChan (Chan NCMsg))

(enum LocalNodeState
    (LocalNodeValid ValidLocalNodeState)
    LocalNodeClosed)

(type OutgoingConnectionMap (Map ChannelID (Map EndPointAddress *Connection)))

(defrecord ValidLocalNodeState
    [^"map[ChannelID]*LocalChannel" localSwitches
    ;; | Outgoing connections
     ^OutgoingConnectionMap localConnections])

(defrecord LocalChannel
    [^ChannelID channelID
     ^*LocalNode localNode
     ^"chan Message" Queue])
    ; switchState (MVar LocalSwitchState))

; (struct LocalSwitchState
;     switchQueue  (Chan Message))

;;;------------------------------------------------------------------------------
;;; Messages                                                                   --
;;;------------------------------------------------------------------------------
(defrecord Message
    [^EndPointAddress From
     ^ByteString Payload])

(defn NewLocalNode ^*LocalNode [^*Transport transport]
    (<- endpoint (transport.NewEndPoint 0))
    (let 
        st (LocalNodeValid. (map->ValidLocalNodeState 
                                {localSwitches (native "make(map[ChannelID]*LocalChannel)")
                                 localConnections (newOutgoingConnectionMap)}))
        node (map->LocalNode
                {localEndPoint endpoint
                 localState (^LocalNodeState newMVar &st)})

        stopNC  (fn []
                    (>! node.localCtrlChan (NCMsg. (node.localEndPoint.Address) (SigShutdown.)))))

    ;; Once the NC terminates, the endpoint isn't much use,
    (go (finally (nodeController &node)
                 (node.localEndPoint.Close)))

    ;; whilst a closed/failing endpoint will terminate the NC
    (go (finally (handleNodeMessages &node)
                 (stopNC)))

    (return &node))

(defmacro withValidLocalNodeState! [st vst & body]
    (let [v (symbol (str st ".value"))]
        `(match ~v
            [LocalNodeValid ~vst]
            (do ~@body))))

(defn NewLocalChannel ^*LocalChannel
    [^*LocalNode localNode, ^ChannelID sid]
    (let st &localNode.localState)
    (lock! st)

    (withValidLocalNodeState! st vst
        (let localChannel (LocalChannel. sid localNode 
                            (^Message chan 10)))
        (when (== nil (get vst.localSwitches sid))
            (assoc vst.localSwitches sid &localChannel)
            (return &localChannel))                     
        (throw (str "local channel exist!" sid)))

    ; LocalNodeClosed
    (throw "local node closed"))

(defn CloseLocalChannel
    [^*LocalChannel localChannel]
    (let st &localChannel.localNode.localState)
    (lock! st)

    (withValidLocalNodeState! st vst
        (let localSwitch_ (get vst.localSwitches localChannel.channelID))
        (if (nil? localSwitch_)
            (throw "local switch closed")
            (do
                (dissoc vst.localSwitches localChannel.channelID)
                (dissoc vst.localConnections localChannel.channelID)
                (return))))
    ; LocalNodeClosed
    (throw "local node closed"))
    
;;;------------------------------------------------------------------------------
;;; Handle incoming messages                                                   --
;;;------------------------------------------------------------------------------

(defrecord IncomingConnection
    [^EndPointAddress theirAddress
     ^IncomingTarget  theirTarget])

(enum IncomingTarget
    Uninit
    (ToChannel *LocalChannel)
    ToNode)

(type IncomingConnectionMap (Map EndPointAddress (Set ConnectionId)))

(defrecord ConnectionState
    [^"map[ConnectionId]*IncomingConnection" incoming
     ^IncomingConnectionMap incomingFrom])

(defn initConnectionState ^*ConnectionState []
    (native
        "return &ConnectionState {"
        "   incoming: make(map[ConnectionId]*IncomingConnection),"
        "   incomingFrom: newIncomingConnectionMap(),"
        "}"))

(defn handleNodeMessages [^*LocalNode localNode]
    (let 
        st (initConnectionState)
         
        invalidRequest 
        (fn [^ConnectionId cid ^String msg]
            (dissoc  st.incoming, cid))

        onConnectionOpened
        (fn [^ConnectionId cid, ^EndPointAddress ep]
            (assoc st.incoming cid (&IncomingConnection. ep (Uninit.)))
            (assoc-in st.incomingFrom [ep] cid))

        onConnectionClosed 
        (fn [^ConnectionId cid]
            (let pConn (get st.incoming cid))
            (if (nil? pConn)
                (invalidRequest cid "closed unknown connection")
                (do
                    (dissoc st.incoming cid)
                    (dissoc (get st.incomingFrom pConn.theirAddress) cid))))
        
        onReceived
        (fn [^ConnectionId cid, ^ByteString payload]
            (let pConn (get st.incoming cid))
            (if (nil? pConn)
                (invalidRequest cid "message received from an unknown connection")

                (match pConn.theirTarget
                    Uninit
                    (do
                        (let 
                            switchid (decodeChannelID payload)
                            nst &localNode.localState
                            pSwitch (^*LocalChannel withMVar nst 
                                        (match nst.value
                                            [LocalNodeValid vst]
                                            (return (get vst.localSwitches switchid)))

                                            ; LocalNodeClosed
                                        (return nil)))
                        
                        (if (nil? pSwitch)
                            (dissoc  st.incoming cid)
                            (assoc st.incoming cid (&IncomingConnection. pConn.theirAddress (&ToChannel. pSwitch)))))

                    [ToChannel pSwitch]
                    (do
                        (println pSwitch.channelID payload)
                        (>! pSwitch.Queue 
                            (Message. pConn.theirAddress payload))))))

        onErrorEvent
        (fn ^Bool [^EventErrorCode errcode ^Error err]
            (println errcode err)
            (match errcode
                [EventConnectionLost addr]
                (do
                    (native
                        "for cid, _ := range st.incomingFrom[addr] {"
                        "   delete(st.incoming, cid)"
                        "}")
                    (dissoc  st.incomingFrom addr))

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
    (return))

(defn SendPayload
    [^*LocalChannel LocalChannel ^EndPointAddress to ^ByteString payload]
    (let node LocalChannel.localNode)
    (<- conn (connBetween node LocalChannel.channelID to))
    (<- bytes (conn.Send payload))
    (println bytes)
    (>! node.localCtrlChan (NCMsg. to (&Died. to (DiedDisconnect.))))
    (return))

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
        (^*Connection withMVar node.localState
            (match node.localState.value
                [LocalNodeValid vst]
                (return
                    (get-in vst.localConnections [from to])))
            (return nil)))
    
    (when (nil? conn)
        (<- newconn (setupConnBetween node from to))
        (return newconn))
    (return conn))

;;;------------------------------------------------------------------------------
;;; Node controller internal data types                                        --
;;;------------------------------------------------------------------------------
(native
    "type Identifier interface {"
    " tagIdentifier() uint8"
    "}"
    "func (addr EndPointAddress) tagIdentifier() uint8 {"
    " return 0"
    "}")

;;; | Why did a switch die?
(enum DiedReason
    DiedDisconnect
    DiedNodeDown)

;;; | Messages to the node controller
(defrecord NCMsg
    [^Identifier ctrlMsgSender
     ^Signal     ctrlMsgSignal])

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
