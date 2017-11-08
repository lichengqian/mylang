(ns language.golang.analyzer
  (:require [language.analyzer :refer :all]))

(defn native-expr
  [form]
  (ns-resolve 'language.golang.native form))


;;;; golang host analyzer
(defn analyze-golang-expr
  {:pass-info {:walk :post :depends #{}}}
  [{:keys [op target form tag env class] :as ast}]
  (case op
    :maybe-class
    (if-let [var (native-expr form)]
      (assoc ast :op :var
                 :var var)
      ast)

    ast))


(def analyze
  (-> default-passes
    (conj #'analyze-golang-expr)
    (build-analyzer)))
