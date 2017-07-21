(in-ns 'language.golang)

;; golang native keyword
(defmethod go-call 'go:case
    [f c & body]
    (str "case "
        (emit c)
        ": \n"
        (emit-do body)))

(defmethod go-call 'go:default
    [f & body]
    (str "default:\n"
        (emit-do body)))

(defmethod go-call 'go:select
    [f & body]
    (str "select"
        (braceln (emit-do body))))

(defmethod go-call 'go:switch
    [f e & body]
    (as-> e ?
        (m/fmap emit ?)
        (string/join "; " ?)
        (str "switch " ?
            (braceln (emit-do body)))))

(defmethod go-call 'type
    [_ v]
    (str (emit v) 
        ".(type)"))

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
(defmethod go-call 'contains?
    [_ m k]
    (str "_, ok := "(emit m) 
        (bracket (emit k))
        "; ok"))

(defmethod go-call 'get
    [_ m k]
    (str (emit m) 
        (bracket (emit k))))

;;; native map support

(defmethod dot-call '.put
  [_ m k v]
  (cl-format nil "~A[~A] = ~A\n"
    (emit m) (emit k) (emit v)))


(defmethod dot-call '.remove
  [_ m k]
  (cl-format nil "delete(~A, ~A)"
    (emit m) (emit k)))


;;; native set support

(defmethod dot-call '.add
  [_ m k]
  (cl-format nil "~A[~A] = struct{}{}\n"
    (emit m) (emit k)))


;;; native OutputStream support

(defmethod dot-call '.flush
  [_ stream]
  (str (emit stream) ".Flush()"))

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

;; native ctor support

(defmethod ctor-call 'BufferedOutputStream
  [_ & args]
  (add-import "bufio")
  (cl-format nil "bufio.NewWriterSize(~{~A~^, ~})"
    (map emit args)))

;; TODO: enum constructor ?
(defmethod ctor-call :default
  [c & args]
  (->> args
            (map emit)
            (string/join ", ")
            brace
            (str (str c))))

;; We would like to be able to add source comments for each argument of a
;; function inline, but this is not possible (only works in a |, || or &&
;; pipeline).
(defmethod emit-function-call ::golang
  [name & args]
;   (println "function-call:" name args)
  (if (symbol? name)
    (cond
        (string/ends-with? (str name) ".")    ; ctor call
        (apply ctor-call 
            (.substring (str name) 0 (- (count (str name)) 1))
            args)

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
