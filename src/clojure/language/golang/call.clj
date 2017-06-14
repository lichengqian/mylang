(in-ns 'language.golang)

;; We would like to be able to add source comments for each argument of a
;; function inline, but this is not possible (only works in a |, || or &&
;; pipeline).
(defmethod emit-function-call ::golang
  [name & args]
  ; (println (str name) args)
  (cond
    (string/ends-with? (str name) ".")    ; enum constructor
    (->> args
        (map emit)
        (string/join ", ")
        brace
        (str (substring (str name) 0 (- (count (str name)) 1))))

    (string/starts-with? (str name) "map->")  ; struct constructor 
    (do
      (println (into [] (first args)))
      (->> (into [] (first args))
          (map (fn [[k v]] (str (emit k) ": " (emit v) ",\n")))
          string/join
          braceln
          (str (substring (str name) 5))))
        
    :else
    (case (str name)
      "sleep" (do
                  (add-import "time")
                  (str "time.Sleep(" (first args) " * time.Millisecond)"))

      "nil?" (str (emit (first args))
                  " == nil")

      "some?" (str (emit (first args))
                  " /= nil")

      "get"   (let [mv (first args)
                    k  (second args)]
                  (str (emit mv) (bracket (emit k))))

      "assoc" (let [mv (first args)
                    kvs (partition 2 (rest args))
                    emit-kv (fn [[k v]]
                                (str  (emit mv) 
                                      (bracket (emit k)) 
                                      " = "
                                      (emit v)
                                      "\n"))]
                  (->> kvs
                    (map emit-kv)
                    string/join))

      "newMVar" (do
                    (add-import "sync")
                    (str
                      (->> 
                          (str "value " (emit-type (typeof name))
                              "\n sync.Mutex")
                          braceln
                          (str "struct"))
                      (->> 
                          (str "value: " 
                              (emit (first args))
                              ",")
                          braceln)))

      "withMVar" (do
                    (str
                      (->>  ; func () RET 
                          (typeof name)
                          emit-type
                          (str "func () "))
                      (->   ; body
                          (first args)
                          emit
                          (#(str % ".Lock()\n"
                                 "defer " % ".Unlock()\n"
                                 (emit-do (rest args))))
                          braceln)
                      "()\n"))

      (if (seq args)
        (->> args
          (map emit)
          (string/join ", ")
          paren
          (str (emit name)))
        ; (str (emit name) "(" (reduce str (interpose "," args)) ")")
        (str (emit name) "()")))))

