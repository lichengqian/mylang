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
  (emit
    `(withMVar ~v
            (match ~v ~@body))))
