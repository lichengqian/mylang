(ns language.emitter
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.jvm :as ast]))

(defn- my-macroexpand-1
  ([form] (my-macroexpand-1 form (ast/empty-env)))
  ([form env]
   (println "expand " form)
   form))


(defn- my-create-var
  [& args]
  (println "create var" args)
  (apply ast/create-var args))


(defn parse-ast
  [form]
  (ast/analyze form (ast/empty-env)
    {:bindings {#'ana/macroexpand-1 my-macroexpand-1
                #'ana/create-var    my-create-var}  
     :passes-opts {}}))

  
(defn emit
  [ast emitter]
  ((get emitter (:op ast) (get emitter nil)) ast emitter))


(defn emit-undefined
  [ast emitter]
  (throw
    (ex-info
      (format "Don't know how to handle op %s." (:op ast))
      ast)))


(def base-emitter
  { nil     #'emit-undefined})
    


