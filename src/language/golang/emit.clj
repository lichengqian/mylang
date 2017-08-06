(ns language.golang.emit
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.analyzer.jvm :as ast]
            [language.emit :refer [parse-ast emit base-emitter]]))
    
(defn emit-const
  [ast emitter]
  (str (:val ast)))


(defn emit-def
  [{:keys [name init]} emitter]
  (cl-format nil "const ~A = ~A" name (emit init emitter)))


(def go-emitter
  { :def    #'emit-def
    :const  #'emit-const})


(defn test-emit
  [form]  
  (let [ast (parse-ast form)]
    (println ast)
    (emit ast (merge base-emitter go-emitter))))  


