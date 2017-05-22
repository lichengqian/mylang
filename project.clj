(defproject mylang "0.1.0-SNAPSHOT"
  :description "TODO"
  :url "TODO"
  :license {:name "TODO: Choose a license"
            :url "http://choosealicense.com/"}
  :plugins          [[kotlinc-lein "0.1.2"]]
  :source-paths        ["src/clojure"]
  :kotlin-source-paths ["src/kotlin"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.jetbrains.kotlin/kotlin-runtime "1.1.2"]
                 [org.jetbrains.kotlin/kotlin-stdlib "1.1.2"]
                 [com.stuartsierra/component "0.3.2"]
                 [org.clojure/tools.logging "0.3.1"]
                 [com.palletops/pallet-common "0.4.0"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]
                                  [com.stuartsierra/component.repl "0.2.0"]]
                   :source-paths ["dev"]}})
