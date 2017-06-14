(in-ns 'language.golang)

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

            (emit-body [body]
              (if @has-err
                (with-bindings {#'*error-code* (error-code)
                                #'*return* emit-return}
                  (emit-do body))
                (emit-do body)))]

      (check-error-return? body)
      (str (emit-function-sig sig) " {\n"
          (emit-body body)
          "}\n\n"))))
  
(defmethod emit-function ::golang
  [name doc? sig body]
  (assert (symbol? name))
  (str (emit-doc doc?)
      "func " name
      (emit-function-decl sig body)))

(defmethod emit-special [::golang 'throw] 
  [_ [ _ err]]
  (str "return " (emit err)))

(defmethod emit-special [::golang 'fn] 
  [_ [ _ sig & body]]
  (str "func "
      (emit-function-decl sig body)))

