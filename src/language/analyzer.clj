(ns language.analyzer 
  (:require [clojure.tools.analyzer
              [passes :refer [schedule]]]
            [clojure.tools.analyzer.jvm :as jvm]
            [clojure.tools.analyzer.passes.jvm
              [analyze-host-expr :refer [analyze-host-expr]]
              [box :refer [box]]
              [constant-lifter :refer [constant-lift]]]))


(def default-passes
  #{#'analyze-host-expr
    #'box
    #'constant-lift})


(defn build-analyzer
  [passes]
  (let [run-passes (schedule passes)]
   (fn -analyze
    ([form] (-analyze form (jvm/empty-env)))
    ([form env] (-analyze form env {}))
    ([form env opts]
     (with-bindings {#'jvm/run-passes run-passes}
       (jvm/analyze form env opts))))))
