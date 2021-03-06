(ns language.golang.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [language.reader :refer :all]
            [language.compiler :refer :all]
            [language.golang.native :refer :all]
            [language.transformer :refer :all]
            [language.golang.analyzer :refer :all]
            [language.golang.compiler :refer :all]))



  
(defn imports
  [decls]
  (let [ps 
        (->> decls
          (map #(collect-tags :package %))
          (apply set/union))]
    (if (empty? ps)
      ""
      (code-block "import (\n~{  \"~S\"\n~})\n"
        ps))))


(defn package
  [name decls]
  (code-block "package ~A\n\n~A\n~{~A\n~}"
    name
    (imports decls)
    decls))


(defn translate-form
  [form]
  (-> form
    analyze
    -compile))


(defn translate-clj
  [package-name file]
  (println (package package-name
            (->> file
              read-forms
              (map translate-form)))))

