(ns clj-compiler
    (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
    (:use [mylang]
          [language.common]
          [language.golang]))

(defn read-forms [path]
    (-> path
        slurp
        (#(str "[" % "\n]"))    ; 最外层包一个中括号，可以解析为一个edn vector，回车是为了防止文件最后一行有注释!
        edn/read-string))

(defn output-with-context [content]
    ; (println *compiler-context*)
    (let [_ns (:ns *compiler-context*)
          _imports (into #{} (:import *compiler-context*))
        ;   _ (println _imports)
          _out (str (emit-ns _ns)
                    (apply str (map #(str % "\n") _imports))
                    "\n"
                    content)]
        _out))

(defn- _compile [path]
    (binding [*compiler-context* {:import #{}}]
        (set-ns (default-go-ns path))
        (let [forms (read-forms path)
            ;   _ (println "read ok" forms)
              out (emit-script (identity forms))]
            (output-with-context out))))

(defn clj-compile 
    "编译一个clj文件，生成同名的golang文件"
    ([path]
     (println (_compile path)))
    ([path target]
     (spit target (_compile path))))

(defn go-target
    [path]
    (str/replace path #"clj" "go"))

(defn go-compile
    "编译一个clj文件，生成同名的golang文件,并格式化"
    ([path]
     (go-compile path (go-target path)))
            
    ([path target]
     (set-language :language.golang/golang)
     (spit target (_compile path))
     (sh "gofmt" "-w" target)))

(defn go-make
    "批量编译一个目录下的clj文件"
    [path]
    (doseq [f (.listFiles (io/file path))]
        (let [fpath (.getPath f)]
            (when (str/ends-with? fpath ".clj")
                (println "compiling " (.getPath f))
                (go-compile (.getPath f))))))
