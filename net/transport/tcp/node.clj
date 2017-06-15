
(type SwitchID UInt64)

(struct LocalNode
    localEndPoint *EndPoint
    localState  (MVar LocalNodeState)
    localCtrlChan (Chan NCMsg))

(enum LocalNodeState
    (LocalNodeValid ValidLocalNodeState)
    LocalNodeClosed)

; (defrecord FromTo
;     [^SwitchID from
;      ^EndPointAddress to])

(defrecord ValidLocalNodeState
    [^"map[SwitchID]*LocalSwitch" localSwitches
    ;; | Outgoing connections
     ^"map[SwitchID]map[EndPointAddress]*Connection" localConnections])

(defrecord LocalSwitch
    [^SwitchID switchID
     ^*LocalNode switchNode
     ^"chan Message" switchQueue])
    ; switchState (MVar LocalSwitchState))

; (struct LocalSwitchState
;     switchQueue  (Chan Message))

;;;------------------------------------------------------------------------------
;;; Messages                                                                   --
;;;------------------------------------------------------------------------------
(defrecord Message
    [^EndPointAddress msgFrom
     ^ByteString msgPayload])

(defn newLocalNode ^*LocalNode [^*Transport transport]
    (<- endpoint (transport.NewEndPoint 0))
    (let localNode (createBareLocalNode endpoint))
    (return localNode))

(defn createBareLocalNode ^*LocalNode [^*EndPoint endpoint]
    (let 
        st (LocalNodeValid. (map->ValidLocalNodeState 
                                {localSwitches (native "make(map[SwitchID]*LocalSwitch)")
                                 localConnections (native "make(map[SwitchID]map[EndPointAddress]*Connection)")}))
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

(defn newLocalSwitch ^*LocalSwitch
    [^*LocalNode localNode, ^SwitchID sid]
    (let st &localNode.localState)
    (lock! st)
    (match st.value
        [LocalNodeValid vst]
        (do
            (let localSwitch (LocalSwitch. sid localNode (^Message chan 10)))
            (assoc vst.localSwitches sid &localSwitch)
            (return &localSwitch)))                    

    ; LocalNodeClosed
    (throw "local node closed"))

(defn closeLocalSwitch
    [^*LocalSwitch localSwitch]
    (let st &localSwitch.switchNode.localState)
    (lock! st)
    (match st.value
        [LocalNodeValid vst]
        (do
            (let localSwitch_ (get vst.localSwitches localSwitch.switchID))
            (if (nil? localSwitch_)
                (throw "local switch closed")
                (do
                    (dissoc vst.localSwitches localSwitch.switchID)
                    (dissoc vst.localConnections localSwitch.switchID)
                    (return)))))
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
    (ToSwitch *LocalSwitch)
    ToNode)

(defrecord ConnectionState
    [^"map[ConnectionId]*IncomingConnection" incoming
     ^"map[EndPointAddress]map[ConnectionId]struct{}" incomingFrom])

(defn initConnectionState ^*ConnectionState []
    (native
        "return &ConnectionState {"
        "   incoming: make(map[ConnectionId]*IncomingConnection),"
        "   incomingFrom: make(map[EndPointAddress]map[ConnectionId]struct{}),"
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
            (native
                "if data, ok := st.incomingFrom[ep]; ok {"
                    "data[cid] = struct{}{}"
                "} else {"
                    "data = make(map[ConnectionId]struct{})"
                    "st.incomingFrom[ep] = data"
                    "data[cid] = struct{}{}"
                "}"))

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
                            switchid (decodeSwitchID payload)
                            nst &localNode.localState
                            pSwitch (^*LocalSwitch withMVar nst 
                                        (match nst.value
                                            [LocalNodeValid vst]
                                            (return (get vst.localSwitches switchid)))

                                            ; LocalNodeClosed
                                        (return nil)))
                        
                        (if (nil? pSwitch)
                            (dissoc  st.incoming cid)
                            (assoc st.incoming cid (&IncomingConnection. pConn.theirAddress (&ToSwitch. pSwitch)))))

                    [ToSwitch pSwitch]
                    (do
                        (println pSwitch.switchID payload)
                        (>! pSwitch.switchQueue 
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

(defn sendPayload
    [^*LocalSwitch localSwitch ^EndPointAddress to ^ByteString payload]
    (let node localSwitch.switchNode)
    (<- conn (connBetween node localSwitch.switchID to))
    (<- bytes (conn.Send payload))
    (println bytes)
    (>! node.localCtrlChan (NCMsg. to (&Died. to (DiedDisconnect.))))
    (return))

(defn setupConnBetween ^*Connection
    [^*LocalNode node ^SwitchID from ^EndPointAddress to]
    (<- conn (node.localEndPoint.Dial to))
    (<- nbytes (conn.Write (encodeSwitchID from)))
    (when (== nbytes 8)
        (lock! node.localState)
        (match node.localState.value
            [LocalNodeValid vst]
            (vst.setLocalConnection  from to conn))
        (return conn))
    (throw "conn failed"))

(defn connBetween ^*Connection
    [^*LocalNode node ^SwitchID from ^EndPointAddress to]
    (let conn 
        (^*Connection withMVar node.localState
            (match node.localState.value
                [LocalNodeValid vst]
                (return
                    (vst.getLocalConnection from to)))
            (return nil)))
    
    (when (nil? conn)
        (<- newconn (setupConnBetween node from to))
        (return newconn))
    (return conn))

; (for! [x [0 1 2]]
;     (println x))

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
    (Kill SwitchID String)
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

