(ns language.emit
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.analyzer.jvm :as ast]))

(defn parse-ast
  [form]
  (ast/analyze form))

  
(defn emit
  [ast emitter]
  ((get emitter (:op ast) (get emitter nil)) ast emitter))


(defn emit-undefined
  [ast emitter]
  (println "undefined" ast))


(def base-emitter
  { nil     #'emit-undefined})
    


