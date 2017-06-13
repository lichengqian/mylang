
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
            (delete st.incoming, cid))

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
                    (delete st.incoming cid)
                    (delete (get st.incomingFrom pConn.theirAddress) cid))))
        
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
                            (delete st.incoming cid)
                            (assoc st.incoming cid (&IncomingConnection. pConn.theirAddress (&ToSwitch. pSwitch)))))

                    [ToSwitch pSwitch]
                    (do
                        (println pSwitch.switchID payload)
                        (>! pSwitch.switchQueue 
                            (Message. pConn.theirAddress payload))))))

        onErrorEvent
        (fn [^EventErrorCode errcode ^Error err]
            (println errcode err)))

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
            (onErrorEvent errcode err)
                
            EndPointClosed
            return)))

