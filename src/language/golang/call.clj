(in-ns 'language.golang)

(defmethod go-call 'nil?
    [_ v]
    (str (emit v) 
        " == nil"))

(defmethod go-call 'some?
    [_ v]
    (str (emit v) 
        " != nil"))

(defmethod go-call 'count
    [_ v]
    (str "len" 
        (paren (emit v)))) 

(defmethod go-call 'set
    [_ v expr]
    (str (emit v) " = " (emit expr) "\n"))

;; map op 
(defmethod go-call 'get
    [_ m k]
    (str (emit m) 
        (bracket (emit k))))

(defmethod go-call 'assoc
    [_ m & kvs]
    ; (println "calling assoc......")
    (->> (partition 2 kvs)
        (map (fn [[k v]]
                (str  (emit m)
                    (bracket (emit k))
                    " = "
                    (emit v)
                    "\n")))
        string/join))

(defmethod go-call 'dissoc
    [_ m k]
    (str "delete" 
        (paren 
            (str (emit m)
                ", "
                (emit k)))))

(defmethod go-call 'defer
    [_ & expr]
    (if (= 1 (count expr))
        (str "defer " (emit (first expr)))
        (str "defer func() " 
            (brace (emit-do expr))
            "()")))

(defmethod go-call 'forever
    [_ & body]
    (str "for "
        (->> body
            emit-do
            braceln)))

(defn- go-call-default
    [name args]
    (if (seq args)
        (->> args
          (map emit)
          (string/join ", ")
          paren
          (str (emit name)))
        ; (str (emit name) "(" (reduce str (interpose "," args)) ")")
        (str (emit name) "()")))

(defmethod go-call :default
    [name & args]
    (go-call-default name args))

;; We would like to be able to add source comments for each argument of a
;; function inline, but this is not possible (only works in a |, || or &&
;; pipeline).
(defmethod emit-function-call ::golang
  [name & args]
;   (println "function-call:" name args)
  (if (symbol? name)
    (cond
        (string/ends-with? (str name) ".")    ; enum constructor
        (->> args
            (map emit)
            (string/join ", ")
            brace
            (str (.substring (str name) 0 (- (count (str name)) 1))))

        (string/starts-with? (str name) "map->")  ; struct constructor 
        (do
            ; (println (into [] (first args)))
            (->> (into [] (first args))
                (map (fn [[k v]] (str (emit k) ": " (emit v) ",\n")))
                string/join
                braceln
                (str (.substring (str name) 5))))
            
        :else
        (apply go-call name args))
    
    (go-call-default name args)))
