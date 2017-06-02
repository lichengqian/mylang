(ns tcp)

;;; Test that the server gets a ConnectionClosed message when the client closes
;;; the socket without sending an explicit control message to the server first)
(test earlyDisconnect
    (let 
        ; clientAddr (newNotifier)
        ; serverAddr (newNotifier)
        serverDone (newNotifier))
    (tlog "testEarlyDisconnect")

    (let-fn server
        (tlog "server")
        (<- tp (createTCPTransport "127.0.0.1:9999"))
        (<- ep (tp.createLocalEndPoint 1000))
        (ep.Close)
        (notify serverDone))
    (let-fn client
        (tlog "client"))
    (go (server))
    (go (client))
    (wait serverDone))
