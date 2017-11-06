(ns language.golang.compiler
  (:require [clojure.core.match :refer [match]]
            [language.target :refer :all]
            [language.golang.target :refer :all]))

(defmulti -compile (fn [ast] (:op ast)))


(defmethod -compile :const
  [{:keys [type val]}]
  (code-block "~A" val))


(defmethod -compile :var
  [{:keys [form]}]
  form)


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
  (code-block (if (= :ctx/return (:context env))
                "~{~A\n~}return ~A"
                "~{~A\n~}~A")
    (map -compile statements)
    (-compile ret)))


(defn compile-fn-param
  [{:keys [form tag]}]
  (code-block "~A ~A" form tag))


(defn compile-fn-method
  ([ast] (compile-fn-method "" ast))
  ([name {:keys [params body]}]
   (code-block "func~A(~{~A~^, ~}) {\n~A}"
     (str " "name) 
     (map compile-fn-param params)
     (-compile body))))


(defn compile-fn
  [name {:keys [methods]}]
  (compile-fn-method name (first methods)))

(defmethod -compile :fn
  [{:keys [methods]}]
  (compile-fn-method (first methods)))


(defmethod -compile :def
  [{:keys [name init]}]
  (println "---init-- ")
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
  (code-block "~{~A\n~}~A"
    (map -compile bindings)
    (-compile body)))


(defmethod -compile :invoke
  [{:keys [fn args]}]
  (code-block "~A(~{~A~^, ~})"
    (-compile fn)
    (map -compile args)))

(defmethod -compile :default
  [{:keys [op children]}]
  (code-block "no compiler for ~A ~A!"
    op children))
