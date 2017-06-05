(ns tcp)

;;; Test that the server gets a ConnectionClosed message when the client closes
;;; the socket without sending an explicit control message to the server first)
(test earlyDisconnect
    (let 
        clientAddr (chan EndPointAddress 1)
        serverAddr (chan EndPointAddress 1)
        serverDone (newNotifier))
    (println "testEarlyDisconnect")

    (let-fn server
        (println "server")
        (<- tp (CreateTransport "127.0.0.1:9999"))
        (<- ep (tp.NewEndPoint 1000))
        (println "server" (ep.Address))
        (serverAddr<- (ep.Address))
        ;; TEST 1: they connect to us, then drop the connection
        (do
            (let event (ep.Receive))
            (println "server" event)
            (assertConnectionOpend t event)
            (let event2 (ep.Receive))
            (println "server2" event2))

        ;; TEST 2: after they dropped their connection to us, we now try to
        ;; establish a connection to them. This should re-establish the broken
        ;; TCP connection.)
        (<-clientAddr theirAddr)
        (println "server" "Trying to connect to client" theirAddr)
        (<- conn (ep.Dial theirAddr))
        ; (println "server" conn)

        ;; TEST 3: To test the connection, we do a simple ping test; as before,
        ;; however, the remote client won't close the connection nicely but just
        ;; closes the socket)
        (do
            (Send conn "ping")

            (let event3 (ep.Receive))
            (println "server3" event3)
                
            (let event4 (ep.Receive))
            (println "server4" event4)

            (let event5 (ep.Receive))
            (println "server5" event5))
        
        ;; TEST 4: A subsequent send on an already-open connection will now break
        (Send conn "ping2")

        (notify serverDone)
        (println "server exist"))

    (let-fn client
        (println "client")
        (<- ourAddr (mockListener "127.0.0.1:8888"))
        (println "client" ourAddr)

        (clientAddr<- ourAddr)
        (<-serverAddr theirAddr)
        ;; Connect to the server
        (<- sock (socketToEndPoint_ ourAddr theirAddr))
        ;; Open a new connection
        (createNewConnection 10002 sock)

        ;; Close the socket without closing the connection explicitly
        ;; The server should receive an error event)
        (sock.Close)
        (println "client exit"))

    (go (server))
    (go (client))
    (wait serverDone))
