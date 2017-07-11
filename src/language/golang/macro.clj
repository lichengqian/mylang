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

(defmethod emit-macro-call [::golang 'matchMVar!]
  [f v & body]
  (let [f2 (with-meta 'withMVar (meta f))
        mv (symbol (str (name v) ".value"))]
    `(~f2 ~v
          (match ~mv ~@body))))

(defmethod emit-macro-call [::golang 'lock!]
  [_ vv]
  (let [v (emit vv)
        _lock (str v ".Lock()")
        _unlock (str "defer " v ".Unlock()")]
      `(native ~_lock ~_unlock)))

