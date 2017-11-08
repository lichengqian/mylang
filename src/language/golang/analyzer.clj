(ns language.golang.analyzer
  (:require [language.analyzer :refer :all]
            [language.golang.native :as native]))

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


(def native-mapper
  {#'println #'native/fmt-Println})

(defn clj->golang
  {:pass-info {:walk :post :depends #{}}}
  [{:keys [op target form tag env class] :as ast}]
  (case op
    :var
    (if-let [var (native-mapper (:var ast))]
      (assoc ast :var var)
      ast)

    ast))



(def analyze
  (-> default-passes
    (conj #'analyze-golang-expr #'clj->golang)
    (build-analyzer)))
