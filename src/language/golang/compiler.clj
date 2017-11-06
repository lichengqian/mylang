(ns language.golang.compiler
  (:require [language.target :refer :all]
            [language.golang.target :refer :all]))

(defmulti -compile (fn [ast] (:op ast)))


(defmethod -compile :const
  [{:keys [type val]}]
  (code-block "~A" val))


(defmethod -compile :with-meta
  [{:keys [expr]}]
  (-compile expr))


(defmethod -compile :def
  [{:keys [name init]}]
  (code-block "const ~A = ~A\n"
    name
    (-compile init)))


(defmethod -compile :binding
  [{:keys [form init]}]
  (code-block "~A := ~A"
    form
    (-compile init)))


(defmethod -compile :let
  [{:keys [bindings body]}]
  (code-block "~{~A\n~}"
    (map -compile bindings)))


(defmethod -compile :invoke
  [{:keys [fn args]}]
  (code-block "~A()"
    (-compile fn)))

(defmethod -compile :default
  [{:keys [op children]}]
  (code-block "no compiler for ~A ~A!"
    op children))
