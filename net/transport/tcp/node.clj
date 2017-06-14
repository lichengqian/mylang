
(type SwitchID UInt64)

(struct LocalNode
    localEndPoint *EndPoint
    localState  (MVar LocalNodeState))

(enum LocalNodeState
    (LocalNodeValid ValidLocalNodeState)
    LocalNodeClosed)

(defrecord FromTo
    [^SwitchID from
     ^EndPointAddress to])

(defrecord ValidLocalNodeState
    [^"map[SwitchID]*LocalSwitch" localSwitches
    ;; | Outgoing connections
     ^"map[FromTo]*Connection" localConnections])

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
    (go (handleNodeMessages localNode))
    (return localNode))

(defn createBareLocalNode ^*LocalNode [^*EndPoint endpoint]
    (let 
        st (LocalNodeValid. (map->ValidLocalNodeState 
                                {localSwitches (native "make(map[SwitchID]*LocalSwitch)")
                                 localConnections (native "make(map[FromTo]*Connection)")}))
        node (map->LocalNode
                {localEndPoint endpoint
                 localState (^LocalNodeState newMVar &st)}))
    (return &node))

;;;------------------------------------------------------------------------------
;;; Handle incoming messages                                                   --
;;;------------------------------------------------------------------------------

(defrecord IncomingConnection
    [^EndPointAddress theirAddress
     ^IncomingTarget  theirTarget])

(enum IncomingTarget
    Uninit
    (ToSwitch *LocalSwitch))

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
    (loop []
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
    (connBetween localSwitch.switchNode localSwitch.switchID to))

(defn setupConnBetween ^*Connection
    [^*LocalNode node ^SwitchID from ^EndPointAddress to]
    (<- conn (node.localEndPoint.Dial to))
    (<- nbytes (conn.Write (encodeSwitchID from)))
    (when (== nbytes 8)
        (withMVar node.localState
            (match node.localState.value
                [LocalNodeValid vst]
                (assoc vst.localConnections (FromTo. from to)
                    conn))))
    (throw "conn failed"))

(defn connBetween ^*Connection
    [^*LocalNode node ^SwitchID from ^EndPointAddress to]
    (let conn 
        (^*Connection withMVar node.localState
            (match node.localState.value
                [LocalNodeValid vst]
                (return
                    (get vst.localConnections (FromTo. from to))))
            (return nil)))
    
    (if (nil? conn)
        (do
            (<- newconn (setupConnBetween node from to))
            (return newconn))
        (return conn)))

