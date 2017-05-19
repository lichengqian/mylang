(ns clj-compiler
    (:require [clojure.edn :as edn]
            [clojure.string :as str])
    (:use [mylang]
          [language.common]
          [language.golang]))

(defn read-forms [path]
    (-> path
        slurp
        (#(str "[" % "\n]"))
        edn/read-string))

(defn output-with-context [content]
    ; (println *compiler-context*)
    (let [_ns (:ns *compiler-context*)
          _imports (into #{} (:import *compiler-context*))
          _out (str (emit-ns _ns)
                    (apply str (map #(str % "\n") _imports))
                    "\n"
                    content)]
        _out))

(defn- _compile [path]
    (binding [*compiler-context* {:import #{}}]
        (let [forms (read-forms path)
              _ (println "read ok" forms)
              out (emit-script (identity forms))]
            (output-with-context out))))

(defn clj-compile 
    "编译一个clj文件，生成同名的golang文件"
    ([path]
        (println (_compile path)))
    ([path target]
        (spit target (_compile path))))
