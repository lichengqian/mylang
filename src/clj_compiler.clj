(ns clj-compiler
    (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
    (:use [mylang]
          [language.common]
          [language.golang]))

(defn read-forms [path]
    (-> path
        slurp
        (#(str "(" % "\n)"))    ; 最外层包一个括号，可以解析为一个list，回车是为了防止文件最后一行有注释!
        read-string))

(defn output-with-context [content]
    ; (println *compiler-context*)
    (let [_ns (:ns *compiler-context*)
          _imports (into #{} (:import *compiler-context*))
        ;   _ (println _imports)
          _out (str (emit-ns _ns)
                    (emit-import _imports)
                    "\n"
                    content)]
        _out))

(defn- _compile [path]
    (binding [*compiler-context* {:import #{}}]
        (set-ns (default-go-ns path))
        (-> (read-forms path)
            expand-macros-all
            emit-script
            output-with-context)))

(defn- _transform [path]
    (->> (read-forms path)
        expand-macros-all
        pprint))

(defn clj-transform 
    "source to source transform 一个clj文件，生成同名的golang文件"
    ([path]
     (println (_transform path)))
    ([path target]
     (spit target (_transform path))))

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
