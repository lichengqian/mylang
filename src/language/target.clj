(ns language.target
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.analyzer.ast :as ast]))

(defrecord CodeBlock [format-str args])

(defmethod print-method CodeBlock
  [cb w]
  (print-method 
    (apply cl-format nil (:format-str cb) (:args cb))
    w))


(defn code-block
  [format-str & args]
  (-> (CodeBlock. format-str (vec args))
    (assoc :children [:args])))


(defrecord Type [name package])

(defmethod print-method Type
  [t w]
  (print-method (str (:name t)) w))


(defmacro primitive-types
  [& names]
  `(do
     ~@(for [name names]
          `(def ~name (Type. ~(str name) nil)))))

(defn collect-tags
  [key ast]
  (->> (ast/nodes ast)
    (map key)
    (filter some?)
    (into #{})))
