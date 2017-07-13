(in-ns 'language.golang)

;;; return function
(defn- emit-return-default [& v]
  (->> v
    (m/fmap emit)
    (cl-format nil "return ~{~A~^, ~}")))

(def ^:dynamic *return* emit-return-default)
(def ^:dynamic *self* "")

(defmethod emit-special [::golang 'return]
  [_ [_ & v]]
  (apply *return* v))

(def ^:dynamic *throw* (fn [v] (str "return " (emit v))))

(defmethod emit-special [::golang 'throw] 
  [_ [ _ err]]
  (add-import "errors")
  (*throw* err))

;;; defn / fn support 

(defn- emit-function-decl
  [sig body]
  ; (println (meta sig) body)
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

            (emit-return [& v]
              (str (apply emit-return-default v)
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
              ; (println "emit-body" body)
              (if @has-err
                (with-bindings {#'*error-code* (error-code)
                                #'*return* emit-return
                                #'*throw*  emit-throw}
                  ; (println "@has-err")
                  (emit-do body))
                (with-bindings {#'*return* emit-return-default}
                  ; (println "no err" (type (first body)))
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
      "func " *self* name
      (emit-function-decl sig body)
      "\n\n"))

(defmethod emit-special [::golang 'fn] 
  [_ [ _ sig & body]]
  (str "func "
      (emit-function-decl sig body)))

;; impl support 
;; (impl ^StructType structVar (defn...))
(defmethod emit-special [::golang 'impl] 
  [_ [ _ self & decls]]
  (with-bindings {#'*self* (paren (emit-arg self))}
    (emit-do decls)))


(defmethod type-call 'fn
  [_ sig]
  (cl-format nil "func (~{~A~^, ~}) ~A"
    (map emit-type sig)
    (emit-type (typeof sig))))
