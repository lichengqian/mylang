(ns language.analyzer 
  (:import (clojure.lang Symbol IPersistentVector IPersistentMap IPersistentSet ISeq IType IRecord)))

(defmulti -analyze-form (fn [form] (class form)))

(declare analyze-seq
         analyze-const)

(def ^:dynamic analyze-form
  -analyze-form)

(defmethod -analyze-form Symbol
    [form]
    { :op :symbol
      :form form})

(defmethod -analyze-form IPersistentVector
  [form]
  { :op :vector
    :form form
    :items (mapv analyze-form form)
    :children [:items]})

(defmethod -analyze-form IPersistentMap
  [form]
  (let [[keys vals] (reduce-kv (fn [[keys vals] k v]
                                [(conj keys k) (conj vals v)])
                              [[] []] form)
        ks (mapv analyze-form keys)
        vs (mapv analyze-form vals)]
    { :op :map
      :form form
      :keys ks
      :vals vs
      :children [:keys :vals]}))

(defmethod -analyze-form IPersistentSet
  [form]
  { :op :set
    :form form
    :items (mapv analyze-form form)
    :children [:items]})

(defmethod -analyze-form ISeq
  [form]
  (if-let [form (seq form)]
    (analyze-seq form)
    (analyze-const form)))

(defmethod -analyze-form :default
  [form]
  (analyze-const form))

(defn analyze-const
  [form]
  { :op :const
    :form form
    :literal? true
    :val form})

(defn analyze-seq
  [[f & args :as form]]
  (when (nil? f)
    (throw (ex-info "Can't call nil"
                    {:form form})))
  { :op :invoke
    :form form
    :fn (analyze-form f)
    :args (mapv analyze-form args)
    :children [:fn :args]})
