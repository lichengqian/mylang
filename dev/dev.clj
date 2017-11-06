(ns dev
  "Tools for interactive development with the REPL. This file should
  not be included in a production build of the application.

  Call `(reset)` to reload modified code and (re)start the system.

  The system under development is `system`, referred from
  `com.stuartsierra.component.repl/system`.

  See also https://github.com/stuartsierra/component.repl"
  (:require
   [clojure.java.io :as io]
   [clojure.java.javadoc :refer [javadoc]]
   [clojure.pprint :refer [cl-format pprint pp]]
   [clojure.reflect :refer [reflect]]
   [clojure.repl :refer [apropos dir doc find-doc pst source]]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :as test]
   [clojure.java.shell :refer [sh]]
   [clojure.tools.namespace.repl :refer [refresh refresh-all clear]]
   [com.stuartsierra.component :as component]
   [com.stuartsierra.component.repl :refer [reset set-init start stop system]]
   [clojure.spec.alpha :as s]
   [cats.builtin]
   [cats.core :as m]
   [clojure.tools.analyzer.jvm :as ast]
   [clojure.tools.analyzer.ast :refer [children, nodes]]
   [language.golang.target :as go]
   [clj-compiler])
  (:use  [mylang]
         [language.reader]
         [language.common]
         [language.golang]
         [language.golang.emit]
         [language.analyzer]
         [language.target]
         [language.golang.compiler]
         [clj-compiler]))

;; Do not try to load source code from 'resources' directory
(clojure.tools.namespace.repl/set-refresh-dirs "dev" "src" "test")

(defn dev-system
  "Constructs a system map suitable for interactive development."
  []
  (component/system-map))
   ;; TODO
   

(set-init (fn [_] (dev-system)))

(alter-var-root #'*script-language* (constantly :language.golang/golang))

(def ^:dynamic *src-dir* "net/transport/tcp")

(defn reset* []
  (sh "touch" "src/language/common.clj")
  (reset))
  
(defn r []
  (reset*)
  (go-make *src-dir*))

(defn- copy-to [target-dir & files]
  (doseq [f files]
    (println "copying " f)
    (io/copy (io/file *src-dir* f) (io/file target-dir f))))

(defn copy-go [target-dir]
  (copy-to target-dir
           "types.go"
           "native.go"
           "impl.go"
           "facade.go"
           "node.go"
           "tcp_test.go"
           "native_test.go"
           "throttle_timer.go"))

(defn to-code
  [form]
  (let [ast (analyze form)
        code (-compile ast)]
    (println "--------form")
    (pprint form)
    (println "--------ast")
    (pprint ast)
    (println "--------code")
    (pprint code)
    (println "--------emit")
    (println code)))
