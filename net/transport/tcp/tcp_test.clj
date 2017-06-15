;;; Test that the server gets a ConnectionClosed message when the client closes
;;; the socket without sending an explicit control message to the server first)
(deftest earlyDisconnect
    (let 
        clientAddr (^EndPointAddress chan 1)
        serverAddr (^EndPointAddress chan 1)
        serverDone (newNotifier))
    (println "testEarlyDisconnect")

    (go
        (println "server")
        (<- tp (CreateTransport "127.0.0.1:9999"))
        (<- ep (tp.NewEndPoint 1000))
        (println "server" (ep.Address))
        (>! serverAddr (ep.Address))
        (let theirAddr (<! clientAddr))

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
            (SendStr conn "ping")

            (let event3 (ep.Receive))
            (println "want3 ConnectionOpened" event3)
                
            (let event4 (ep.Receive))
            (println "want4 Received" event4)

            (let event5 (ep.Receive))
            (println "want5 ErrorEvent" event5))
        
        ;; TEST 4: A subsequent send on an already-open connection will now break
        (SendStr conn "ping2")

        (notify serverDone)
        (println "server exist"))

    (go
        (println "client")
        (<- ourAddr (mockEarlyDisconnect "127.0.0.1:8888"))
        (println "client" ourAddr)

        (>! clientAddr ourAddr)
        (let theirAddr (<! serverAddr))
        ;; Connect to the server
        (<- sock (socketToEndPoint_ ourAddr theirAddr))
        ;; Open a new connection
        (sendCreateNewConnection 10002 sock)

        ;; Close the socket without closing the connection explicitly
        ;; The server should receive an error event)
        (sock.Close)
        (println "client exit"))

    (wait serverDone))

;;; Test the behaviour of a premature CloseSocket request
(deftest earlyCloseSocket
    (let 
        clientAddr (^EndPointAddress chan 1)
        serverAddr (^EndPointAddress chan 1)
        serverDone (newNotifier))
    (println "testEarlyCloseSocket")
    
    (go     ; server
        (println "server")
        (<- tp (CreateTransport "127.0.0.1:9999"))
        (<- ep (tp.NewEndPoint 1000))
        (println "server" (ep.Address))
        (>! serverAddr (ep.Address))
        (let theirAddr (<! clientAddr))
        
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
            (SendStr conn "ping")

            (let event3 (ep.Receive))
            (println "want3 ConnectionOpened" event3)
                
            (let event4 (ep.Receive))
            (println "want4 Received" event4)

            (let event5 (ep.Receive))
            (println "want5 ConnectionClosed" event5)
            
            (let event6 (ep.Receive))
            (println "want6 ErrorEvent" event6))
            
        ;; TEST 4: A subsequent send on an already-open connection will now break
        (SendStr conn "ping2")

        (notify serverDone)
        (println "server exist"))

    (go     ; client
        (println "client")
        (<- ourAddr (mockEarlyCloseSocket "127.0.0.1:8888"))
        (println "client" ourAddr)

        (>! clientAddr ourAddr)
        (let theirAddr (<! serverAddr))
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

    (wait serverDone))

;;; Test the creation of a transport with an invalid address
(deftest invalidAddress
    (let [_ err] (CreateTransport "invalidHostName:9999"))
    (println err))

;;; Test connecting to invalid or non-existing endpoints
(deftest invalidConnect
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

;;; | Test that an endpoint can ignore CloseSocket requests (in "reality" this)
;;; would happen when the endpoint sends a new connection request before
;;; receiving an (already underway) CloseSocket request
(deftest ignoreCloseSocket
    (let 
        clientAddr (^EndPointAddress chan 1)
        serverAddr (^EndPointAddress chan 1)
        clientDone (newNotifier)
        serverDone (newNotifier)
        connectionEstablished (newNotifier))
    (println "testIgnoreCloseSocket")

    (<- transport (CreateTransport "127.0.0.1:9999"))

    ;; server
    (go
        (println "server")
        (<- endpoint (transport.NewEndPoint 1000))
        (let ourAddress (endpoint.Address))
        (>! serverAddr ourAddress)
        (let theirAddress (<! clientAddr))

        ;; Wait for the client to set up the TCP connection to us
        (wait connectionEstablished)

        ;; Connect then disconnect to the client
        (<- conn (endpoint.Dial theirAddress))
        (conn.Close)

        ;; At this point the server will have sent a CloseSocket request to the
        ;; client, which however ignores it, instead it requests and closes
        ;; another connection
        (let event (endpoint.Receive))
        (println "Waiting for ConnectionOpened" event)
        (let event2 (endpoint.Receive))
        (println "Waiting for ConnectionClosed" event2)

        (println "server Done")
        (notify serverDone))

    ;; client
    (go
        (println "client")
        (<- endpoint (transport.NewEndPoint 2000))
        (let ourAddress (endpoint.Address))
        (>! clientAddr ourAddress)
        (let theirAddress (<! serverAddr))

        ;; Connect to the server
        (<- sock (socketToEndPoint_ ourAddress theirAddress))
        (notify connectionEstablished)

        ;; Server connects to us, and then closes the connection
        (<- cheader (recvControlHeader sock))
        (<- lcid (ReadUint32 sock))
        (println "want CreatedNewConnection:" cheader lcid)
        
        (<- cheader2 (recvControlHeader sock))
        (<- lcid2 (ReadUint32 sock))
        (println "want CloseConnection:" cheader2 lcid2)

        ;; Server will now send a CloseSocket request as its refcount reached 0
        (<- cheader3 (recvControlHeader sock))
        (<- lcid3 (ReadUint32 sock))
        (println "want CloseSocket:" cheader3 lcid3)

        ;; But we ignore it and request another connection in the other direction
        (println "Ignore it, requesting another connection")
        (sendCreateNewConnection 1024 sock)

        ;; Close it again
        (println "Closing socket")
        (sendCloseSocket 1024 sock)
        (sock.Close)

        (println "client Done")
        (notify clientDone))

    (wait clientDone)
    (wait serverDone))

;;; | Like 'testIgnoreSocket', but now the server requests a connection after the
;;; client closed their connection. In the meantime, the server will have sent a
;;; CloseSocket request to the client, and must block until the client responds.)
(deftest blockAfterCloseSocket
    (let 
        clientAddr (^EndPointAddress chan 1)
        serverAddr (^EndPointAddress chan 1)
        clientDone (newNotifier)
        serverDone (newNotifier)
        connectionEstablished (newNotifier))
    (println "testBlockAfterCloseSocket")

    (<- transport (CreateTransport "127.0.0.1:9999"))

    ;; server
    (go
        (println "server")
        (<- endpoint (transport.NewEndPoint 1000))
        (let ourAddress (endpoint.Address))
        (>! serverAddr ourAddress)
        (let theirAddress (<! clientAddr))

        ;; Wait for the client to set up the TCP connection to us
        (wait connectionEstablished)

        ;; Connect then disconnect to the client
        (<- conn (endpoint.Dial theirAddress))
        (conn.Close)

        ;; At this point the server will have sent a CloseSocket request to the
        ;; client, and must block until the client responds
        (<- conn2 (endpoint.Dial theirAddress))

        (println "serverDone" conn2)
        (notify serverDone))

    ;; client
    (go
        (println "client")
        (<- endpoint (transport.NewEndPoint 2000))
        (let ourAddress (endpoint.Address))
        (>! clientAddr ourAddress)
        (let theirAddress (<! serverAddr))

        ;; Connect to the server
        (<- sock (socketToEndPoint_ ourAddress theirAddress))
        (notify connectionEstablished)

        ;; Server connects to us, and then closes the connection
        (<- cheader (recvControlHeader sock))
        (<- lcid (ReadUint32 sock))
        (println "want CreatedNewConnection:" cheader lcid)
        
        (<- cheader2 (recvControlHeader sock))
        (<- lcid2 (ReadUint32 sock))
        (println "want CloseConnection:" cheader2 lcid2)

        ;; Server will now send a CloseSocket request as its refcount reached 0
        (<- cheader3 (recvControlHeader sock))
        (<- lcid3 (ReadUint32 sock))
        (println "want CloseSocket:" cheader3 lcid3)

        (let unblocked (newNotifier))
        ;; We should not hear from the server until we unblock him by
        ;; responding to the CloseSocket request (in this case, we)
        ;; respond by sending a ConnectionRequest
        (go
            (ReadUint32 sock)
            (wait unblocked)
            (println "clientDone")
            (notify clientDone))
        
        (sleep 1000)
        (println "Client ignores close socket and sends connection request")
        (println "This should unblock the server")
        (notify unblocked)
        (sendCreateNewConnection 1024 sock))

    (wait clientDone)
    (wait serverDone))
    

;;; | Test what happens when a remote endpoint sends a connection request to our
;;; transport for an endpoint it already has a connection to
(deftest unnecessaryConnect
    (let 
        numThreads 10
        serverAddr (^EndPointAddress chan 1)
        clientDone (newNotifier))
    (println "testUnnecessaryConnect")

    (go
        (<- transport (CreateTransport "127.0.0.1:9999"))
        (<- endpoint (transport.NewEndPoint 1000))
        ;; Since we're lying about the server's address, we have to manually
        ;; construct the proper address. If we used its actual address, the clients
        ;; would try to resolve "128.0.0.1" and then would fail due to invalid
        ;; address.)
        (>! serverAddr (endpoint.Address)))

    (go
        ;; We pick an address < 128.0.0.1 so that this is not rejected purely because of the "crossed" check
        (let ourAddress (newEndPointAddress "127.0.0.1:8888" 1000))

        ;; We should only get a single 'Accepted' reply
        (let gotAccepted (newNotifier)
             addr (<! serverAddr))
        (mockUnnecessaryConnect numThreads ourAddress addr gotAccepted)
        (wait gotAccepted)
        (notify clientDone))

    (wait clientDone))

;;; | Test that we can create "many" transport instances
;;; TODO

;;; | Test what happens when the transport breaks completely)
;;; TODO

;;; | Test that a second call to 'connect' might succeed even if the first
;;; failed. This is a TCP specific test rather than an endpoint specific test
;;; because we must manually create the endpoint address to match an endpoint we
;;; have yet to set up.
;;; Then test that we get a connection lost message after the remote endpoint
;;; suddenly closes the socket, and that a subsequent 'connect' allows us to
;;; re-establish a connection to the same endpoint
;;; TODO

(deftest invalidCloseConnection
    (<- internal (createTCPTransport "127.0.0.1:9999"))
    (let 
        transport (internal.ToTransport)
        serverDone (newNotifier)
        clientDone (newNotifier)
        serverAddr (^EndPointAddress chan 1))

    ;; server
    (go 
        (<- endpoint (transport.NewEndPoint 1000))
        (>! serverAddr (endpoint.Address))

        (let event (endpoint.Receive))
        (println "want ConnectionOpened" event)
        
        ;; At this point the client sends an invalid request, so we terminate the
        ;; connection
        (let event2 (endpoint.Receive))
        (println "want ErrorEvent" event2)
        
        (notify serverDone))

    ;; client
    (go
        (<- endpoint (transport.NewEndPoint 2000))
        (let
            ourAddr (endpoint.Address)
            theirAddr (<! serverAddr))
        ;; Connect so that we have a TCP connection)
        (endpoint.Dial theirAddr)

        ;; Get a handle on the TCP connection and manually send an invalid CloseConnection request
        (<- sock (internal.internalSocketBetween ourAddr theirAddr))
        (sendCloseConnection 12345 sock)

        (notify clientDone))
    
    (wait serverDone)
    (wait clientDone))

;;; | Ensure that an end point closes up OK even if the peer disobeys the
;;;   protocol.)
(deftest closeEndPoint
    (let
        serverAddr (^EndPointAddress chan 1)
        serverDone (newNotifier))
    
    ;; A server which accepts one connection and then attempts to close the
    ;; end point.)
    (go
        (<- transport (CreateTransport "127.0.0.1:9999"))
        (<- ep (transport.NewEndPoint 1000))
        (>! serverAddr (ep.Address))

        (let event (ep.Receive))
        (println "want ConnectionOpened" event)

        (ep.Close)
        (notify serverDone))

    ;; A nefarious client which connects to the server then stops responding.
    (go
        (let 
            ourAddr (newEndPointAddress "127.0.0.1:8888" 100)
            theirAddr (<! serverAddr))
        (<- sock (socketToEndPoint_ ourAddr theirAddr))
        (sendCreateNewConnection 1024 sock)
        (wait serverDone)
        (sock.Close))

    (wait serverDone))
