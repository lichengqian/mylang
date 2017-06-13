
(type SwitchID UInt64)

(struct LocalNode
    localEndPoint *EndPoint
    localState  (MVar LocalNodeState))

(enum LocalNodeState
    (LocalNodeValid ValidLocalNodeState)
    LocalNodeClosed)

(defrecord ValidLocalNodeState
    [^"map[SwitchID]*LocalSwitch" localSwitches   
     ^"map[SwitchID]*Connection" localConnections])

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
                                 localConnections (native "make(map[SwitchID]*Connection)")}))
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
    (ToSwitch SwitchID))

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
    (let st (initConnectionState)
         invalidRequest (fn [^ConnectionId cid ^String msg]
                            (delete st.incoming, cid))

         deleteConn (fn [^ConnectionId cid]
                       (delete st.incoming cid)
                       (native 
                           "// TODO"
                           "// hello")))

    (loop []
        (println "handling node message...")
        (let event (localNode.localEndPoint.Receive))
        (match event
            [ConnectionOpened cid ep]
            (do
                (assoc st.incoming cid (&IncomingConnection. ep (Uninit.)))
                (native
                    "if data, ok := st.incomingFrom[ep]; ok {"
                        "data[cid] = struct{}{}"
                    "} else {"
                        "data = make(map[ConnectionId]struct{})"
                        "st.incomingFrom[ep] = data"
                        "data[cid] = struct{}{}"
                    "}"))
                ; (recur))

            [Received cid payload]
            (do
                (let pConn (get st.incoming cid))
                (if (nil? pConn)
                    (invalidRequest cid "message received from an unknown connection")

                    (match pConn.theirTarget
                        Uninit
                        (do
                            (let switchid (decodeSwitchID payload))
                            (println "uinit"))

                        [ToSwitch sid]
                        (println sid payload))))

            [ConnectionClosed cid]
            (do
                (let pConn (get st.incoming cid))
                (if (nil? pConn)
                    (invalidRequest cid "closed unknown connection")
                    (deleteConn cid)))

            [ErrorEvent err msg]
            (println err msg)
                
            EndPointClosed
            return)))

