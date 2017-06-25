(in-ns 'language.golang)

;;; return function
(defn- emit-return-default [v]
  (str "return " (emit v)))

(def ^:dynamic *return* emit-return-default)

(defmethod emit-special [::golang 'return]
  [_ [_ v]]
  (*return* v))

(def ^:dynamic *throw* (fn [v] (str "return " (emit v))))

(defmethod emit-special [::golang 'throw] 
  [_ [ _ err]]
  (add-import "errors")
  (*throw* err))

;;; defn / fn support 

(defn- emit-function-decl
  [sig body]
  (println (meta sig))
  (with-local-vars [has-err false]
    (letfn [(check-error-return? [body]
                (prewalk #(if (or (= '<- %) (= 'throw %)) 
                              (do
                                (var-set has-err true)
                                %)
                              %)
                        body))
            (emit-function-sig
              [sig]
              (let [ret-type (if @has-err [(typeof sig), 'Error] [(typeof sig)])
                    args (->> sig
                          (map emit-arg)
                          (string/join ", ")
                          paren)]
                  (str args " " 
                      (emit-type (filterv some? ret-type)))))

            (error-code []
              (if (nil? (typeof sig))
                "return err\n"
                "return nil, err\n"))

            (emit-return [v]
              (str "return " (emit v)
                (if (nil? (typeof sig))
                  "\n"
                  ", nil")))

            (emit-throw [v]
              (str "return "
                (if (nil? (typeof sig))
                  ""
                  "nil,")
                (if (symbol? v)
                  (emit v) 
                  (cl-format nil "errors.New(~A)"
                      (emit v)))))

            (emit-body [body]
              (if @has-err
                (with-bindings {#'*error-code* (error-code)
                                #'*return* emit-return
                                #'*throw*  emit-throw}
                  (emit-do body))
                (with-bindings {#'*return* emit-return-default}
                  (emit-do body))))]

      (check-error-return? body)
      ; (println @has-err "has error return for sig : " sig)
      (str (emit-function-sig sig) " {\n"
          (emit-body body)
          "}"))))
  
(defmethod emit-function ::golang
  [name doc? sig body]
  (assert (symbol? name))
  (str (emit-doc doc?)
      "func " name
      (emit-function-decl sig body)
      "\n\n"))

(defmethod emit-special [::golang 'fn] 
  [_ [ _ sig & body]]
  (str "func "
      (emit-function-decl sig body)))

