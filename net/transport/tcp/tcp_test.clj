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
        (<-clientAddr theirAddr)

        ;; TEST 1: they connect to us, then drop the connection
        (do
            (let event (ep.Receive))
            (println "server" event)
            (assertConnectionOpend t event)
            (let event2 (ep.Receive))
            (println "want2 ErrorEvent" event2))

        ;; TEST 2: after they dropped their connection to us, we now try to
        ;; establish a connection to them. This should re-establish the broken
        ;; TCP connection.)
        (println "server" "Trying to connect to client" theirAddr)
        (<- conn (ep.Dial theirAddr))
        ; (println "server" conn)

        ;; TEST 3: To test the connection, we do a simple ping test; as before,
        ;; however, the remote client won't close the connection nicely but just
        ;; closes the socket)
        (do
            (Send conn "ping")

            (let event3 (ep.Receive))
            (println "want3 ConnectionOpened" event3)
                
            (let event4 (ep.Receive))
            (println "want4 Received" event4)

            (let event5 (ep.Receive))
            (println "want5 ErrorEvent" event5))
        
        ;; TEST 4: A subsequent send on an already-open connection will now break
        (Send conn "ping2")

        (notify serverDone)
        (println "server exist"))

    (let-fn client
        (println "client")
        (<- ourAddr (mockEarlyDisconnect "127.0.0.1:8888"))
        (println "client" ourAddr)

        (clientAddr<- ourAddr)
        (<-serverAddr theirAddr)
        ;; Connect to the server
        (<- sock (socketToEndPoint_ ourAddr theirAddr))
        ;; Open a new connection
        (sendCreateNewConnection 10002 sock)

        ;; Close the socket without closing the connection explicitly
        ;; The server should receive an error event)
        (sock.Close)
        (println "client exit"))

    (go (server))
    (go (client))
    (wait serverDone))

;;; Test the behaviour of a premature CloseSocket request
(test earlyCloseSocket
    (let 
        clientAddr (chan EndPointAddress 1)
        serverAddr (chan EndPointAddress 1)
        serverDone (newNotifier))
    (println "testEarlyCloseSocket")
    
    (let-fn server
        (println "server")
        (<- tp (CreateTransport "127.0.0.1:9999"))
        (<- ep (tp.NewEndPoint 1000))
        (println "server" (ep.Address))
        (serverAddr<- (ep.Address))
        (<-clientAddr theirAddr)
        
        ;; TEST 1: they connect to us, then send a CloseSocket. Since we don't
        ;; have any outgoing connections, this means we will agree to close the
        ;; socket)
        (do
            (let event (ep.Receive))
            (println "want ConnectionOpened" event)
            (assertConnectionOpend t event)
            (let event2 (ep.Receive))
            (println "want2 ConnectionClosed" event2))
            
        ;; TEST 2: after they dropped their connection to us, we now try to
        ;; establish a connection to them. This should re-establish the broken
        ;; TCP connection.
        (println "server" "Trying to connect to client" theirAddr)
        (<- conn (ep.Dial theirAddr))
        
        ;; TEST 3: To test the connection, we do a simple ping test; as before,
        ;; however, the remote client won't close the connection nicely but just
        ;; sends a CloseSocket -- except that now we *do* have outgoing
        ;; connections, so we won't agree and hence will receive an error when
        ;; the socket gets closed
        (do
            (Send conn "ping")

            (let event3 (ep.Receive))
            (println "want3 ConnectionOpened" event3)
                
            (let event4 (ep.Receive))
            (println "want4 Received" event4)

            (let event5 (ep.Receive))
            (println "want5 ConnectionClosed" event5)
            
            (let event6 (ep.Receive))
            (println "want6 ErrorEvent" event6))
            
        ;; TEST 4: A subsequent send on an already-open connection will now break
        (Send conn "ping2")

        (notify serverDone)
        (println "server exist"))

    (let-fn client
        (println "client")
        (<- ourAddr (mockEarlyCloseSocket "127.0.0.1:8888"))
        (println "client" ourAddr)

        (clientAddr<- ourAddr)
        (<-serverAddr theirAddr)
        ;; Connect to the server
        (<- sock (socketToEndPoint_ ourAddr theirAddr))
        ;; Open a new connection
        (println "client create connection 10003")
        (sendCreateNewConnection 10003 sock)

        ;; Send a CloseSocket without sending a closeconnecton
        ;; The server should still receive a ConnectionClosed message
        (println "client close socket 0")
        (sendCloseSocket 0 sock)
        (sock.Close)
        (println "client exit"))

    (go (server))
    (go (client))
    (wait serverDone))

;;; Test the creation of a transport with an invalid address
(test invalidAddress
    (let [_ err] (CreateTransport "invalidHostName:9999"))
    (println err))

;;; Test connecting to invalid or non-existing endpoints
(test invalidConnect
    (<- tp (CreateTransport "127.0.0.1:9999"))
    (<- ep (tp.NewEndPoint 1000))

    ;; Syntax connect, but invalid hostname (TCP address lookup failure)
    (let [conn2, err] (ep.Dial (newEndPointAddress "invalidAddress", 0)))
    (println conn2 err)

    ;; TCP address correct, but nobody home at that address
    (let [conn3, err] (ep.Dial (newEndPointAddress "127.0.0.1:9000", 0)))
    (println conn3 err)

    ;; Valid TCP address but invalid endpoint number
    (let [conn4, err] (ep.Dial (newEndPointAddress "127.0.0.1:9999", 900)))
    (println conn4 err))
