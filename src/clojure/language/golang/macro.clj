(in-ns 'language.golang)

(defmethod emit-macro-call [::golang 'for!]
  [_ seq-exprs body-expr]
  (println "for!" body-expr)
  (let [[_f & _args] body-expr
        _inner_c `(list '~_f ~@_args)
        _c (list 'for seq-exprs
              _inner_c)]
      (println _inner_c)
      (println _c)
      (println (eval _c))
      (->> _c
        eval
        (into [])
        emit-do)))

(defmethod emit-macro-call [::golang 'decode!]
  [_ enum-name]
  (.macro_decode golang
    enum-name 
    (get-enum enum-name)))

(defmethod emit-macro-call [::golang 'encode!]
  [_ enum-name]
  (.macro_encode golang
    enum-name 
    (get-enum enum-name)))

(defmethod emit-macro-call [::golang 'matchMVar!]
  [f v & body]
  (let [mv (symbol (str (name v) ".value"))]
    `(withMVar ~v
          (match ~mv ~@body))))

(defmethod emit-macro-call [::golang 'lock!]
  [_ v]
  (let [_lock (str v ".Lock()")
        _unlock (str "defer " v ".Unlock()")]
      `(native ~_lock ~_unlock)))

(defmethod emit-special [::golang 'defmacro] 
  [_ [ _ n sig & body]]
  (let [code
          `(defmethod emit-macro-call [::golang '~n]
              [~'_ ~@sig] ~@body)]
    (println code) 
    (eval code)
    ""))
      

