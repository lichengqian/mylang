(ns language.golang.compiler
  (:require [clojure.core.match :refer [match]]
            [language.compiler :refer :all]
            [language.golang.native :refer :all]))

(defmulti -compile (fn [ast] (:op ast)))


(defmethod -compile :const
  [{:keys [type val]}]
  (case type
    :string (str "\"" val "\"")
    (code-block "~A" val)))


(defmethod -compile :var
  [{:keys [form var] :as ast}]
  (cond
    (native? ast) (var-get var)
    :else form))


(defmethod -compile :with-meta
  [{:keys [expr]}]
  (-compile expr))


(defmethod -compile :static-call
  [{:keys [class method args]}]
  (match [class method]
    ['clojure.lang.Numbers 'add] (apply code-block "~A + ~A"
                                  (map -compile args))
    ['clojure.lang.Numbers 'minus] (apply code-block "~A - ~A"
                                     (map -compile args))
    ['clojure.lang.Numbers 'multiply] (apply code-block "~A * ~A"
                                       (map -compile args))))


(defmethod -compile :do
  [{:keys [statements ret env]}]
  (code-block-with-keys (if (= :ctx/return (:context env))
                         "~{~A\n~}return ~A"
                         "~{~A\n~}~A")
    :statements (mapv -compile statements)
    :ret (-compile ret)))


(defn compile-fn-param
  [{:keys [form tag]}]
  (code-block "~A ~A" form tag))


(defn compile-fn-method
  ([ast] (compile-fn-method "" ast))
  ([name {:keys [params body]}]
   (code-block-with-keys "func~A(~{~A~^, ~}) {\n~A}"
     :name (str " "name) 
     :params (mapv compile-fn-param params)
     :body (-compile body))))


(defn compile-fn
  [name {:keys [methods]}]
  (compile-fn-method name (first methods)))

(defmethod -compile :fn
  [{:keys [methods]}]
  (compile-fn-method (first methods)))


(defmethod -compile :def
  [{:keys [name init]}]
  ; (println "---init-- ")
  ; (pprint init)
  (match [(:op init)]
    [:with-meta] (compile-fn name (:expr init))
    :else (code-block "const ~A = ~A\n"
            name
            (-compile init))))


(defmethod -compile :binding
  [{:keys [form init]}]
  (code-block "~A := ~A"
    form
    (-compile init)))


(defmethod -compile :local
  [{:keys [form]}]
  form)


(defmethod -compile :let
  [{:keys [bindings body]}]
  (code-block-with-keys "~{~A\n~}~A"
    :bindings (mapv -compile bindings)
    :body (-compile body)))


(defmethod -compile :invoke
  [{:keys [fn args]}]
  (cond
    (native? fn)   ;; 支持native函数调用
    (apply (:var fn) -compile args)

    :else
    (apply code-block "~A(~@{~A~^, ~})"
      (-compile fn)
      (map -compile args))))
    

(defmethod -compile :default
  [{:keys [op children]}]
  (code-block "no compiler for ~A ~A!"
    op children))
