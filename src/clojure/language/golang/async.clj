(in-ns 'language.golang)

(defmethod emit-special [::golang 'go]
  [_ [_ & body]]
  (if (= 1 (count body))
      (str "go " (emit-do body))
      (str  "go func() {\n"
            (emit-do body)
            "}()\n")))

(defemit-special ::golang
  'chan [t n] (str "make(chan " (emit t) ", " (emit n) ")")
  '<! [c] (str "<-" (emit c))
  '>! [c v] (str (emit c) "<- " (emit v)))

(defmethod go-call 'sleep
    [_ s]
    (println "sleep " s)
    (add-import "time")
    (str "time.Sleep(" s " * time.Millisecond)"))
    
(defmethod go-call 'newMVar
    [f v]
    (println "newMVar " v)
    (add-import "sync")
    (str
        (->> 
            (str "value " (emit-type (typeof f))
                "\n sync.Mutex")
            braceln
            (str "struct"))
        (->> 
            (str "value: " 
                (emit v)
                ",")
            braceln)))

(defmethod go-call 'withMVar
    [f v & body]
    (println "withMVar " v)
    (let [sig (with-meta [] (meta f))
          prefix (str v ".Lock()\n defer " v ".Unlock()\n")
          code (list* (list 'native prefix) body)]
        (str
            (emit `(fn ~sig ~@code))
            "()\n")))

