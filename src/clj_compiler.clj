(ns clj-compiler
    (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.main :refer [repl]])
    (:use [mylang]
          [language.reader]
          [language.common]
          [language.golang]))



(defn- output-with-context [content ctx]
    ; (println ctx)
    (let [_ns (:ns ctx)
          _imports (into #{} (:import ctx))]
        ; (println _imports)
        (str (emit-ns _ns)
             (emit-import _imports)
             "\n"
             content)))

(defn- _compile [path]
    (binding [*compiler-context* {:import #{}}]
        (set-ns (default-go-ns path))
        (-> (read-forms path)
            transform
            emit-script
            (output-with-context *compiler-context*))))

(defn- _transform [path]
    (->> (read-forms path)
        transform
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
     (cond
        (string? path) (go-compile path (go-target path))
        :else (with-script-language :language.golang/golang
                    (-> path
                        transform
                        (doto pprint)
                        emit-script
                        println))))
            
    ([path target]
     (with-script-language :language.golang/golang
        (spit target (_compile path))
        (sh "gofmt" "-w" target))))

(defn go-make
    "批量编译一个目录下的clj文件"
    [path]
    (doseq [f (.listFiles (io/file path))]
        (let [fpath (.getPath f)]
            (when (str/ends-with? fpath ".clj")
                (println "compiling " (.getPath f))
                (go-compile (.getPath f))))))


;;;;  golang repl for test!
(defn go-eval
  [form]
  (binding [*compiler-context* {:import #{}}]
    (set-ns "dev")
    (-> (list form)         ;; surrand with list
      transform
      emit-script
      (output-with-context *compiler-context*))))


(defn go-repl
  []
  (with-script-language :language.golang/golang
    (repl :eval go-eval
          :print println  
          :prompt (fn [] (printf "go=> ")))))
  

