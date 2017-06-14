(in-ns 'language.golang)

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
