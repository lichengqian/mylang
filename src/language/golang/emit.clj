(ns language.golang.emit
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.analyzer.jvm :as ast]
            [language.emitter :refer [parse-ast emit base-emitter]]))
    
(defn emit-const
  [{:keys [type val]} emitter]
  (case type
    :string (str "\"" val "\"")
    (str val)))


(defn- emit-fn
  ([name ast emitter]
   (str "func " name " "
       (emit-fn ast emitter)))
  ([ast emitter]
   (str "fn")))


(defn emit-def
  [{:keys [name init]} emitter]
  (cond
    (= (get-in init [:expr :op]) :fn)  (emit-fn name (:expr init) emitter)
    :else               (cl-format nil "const ~A = ~A" name (emit init emitter))))


(def go-emitter
  { :def    #'emit-def
    :const  #'emit-const})


(defn test-emit
  [form]  
  (let [ast (parse-ast form)]
    (println ast)
    (emit ast (merge base-emitter go-emitter))))  


