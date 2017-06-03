(ns tcp)

;;; Test that the server gets a ConnectionClosed message when the client closes
;;; the socket without sending an explicit control message to the server first)
(test earlyDisconnect
    (let 
        clientAddr (chan EndPointAddress 1)
        serverAddr (chan EndPointAddress 1)
        serverDone (newNotifier))
    (tlog "testEarlyDisconnect")


    (let-fn server
        (tlog "server")
        (<- tp (createTCPTransport "127.0.0.1:9999"))
        (<- ep (tp.createLocalEndPoint 1000))
        (println ep.localAddress)
        (serverAddr<- ep.localAddress)
        ;; TEST 1: they connect to us, then drop the connection
        (do
            (let event (ep.Receive))
            (println event)
            (assertConnectionOpend t event)
            (let event2 (ep.Receive))
            (println event2))

        ;; TEST 2: after they dropped their connection to us, we now try to
        ;; establish a connection to them. This should re-establish the broken
        ;; TCP connection.)
        (<-clientAddr theirAddr)
        (println "Trying to connect to client" theirAddr)
        (<- conn (ep.connect theirAddr))
        (println conn)

        ;; TEST 3: To test the connection, we do a simple ping test; as before,
        ;; however, the remote client won't close the connection nicely but just
        ;; closes the socket)
        (do
            (send conn "ping")

            (let event3 (ep.Receive))
            (println event3)
                
            (let event4 (ep.Receive))
            (println event4)

            (let event5 (ep.Receive))
            (println event5))

        
        ;; TEST 4: A subsequent send on an already-open connection will now break
        (send conn "ping2")

        (notify serverDone)
        (println "server exist"))
    (let-fn client
        (tlog "client")
        (<- ourAddr (mockListener "127.0.0.1:8888"))
        (println ourAddr)

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
