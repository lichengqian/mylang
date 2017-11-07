(ns language.golang.native
  (:refer-clojure :exclude [int struct type])
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.analyzer.ast :as ast]
            [language.compiler :refer :all]))


(primitive-types
    int int8 int16 int32
    uint uint8 uint16 uint32)


(defmacro nt*
  [name type]
  (code-block "~A ~A" name type))


(defmacro nt
  [name type]
  `(code-block "~A ~A" ~(str name) ~type))


(defn const*
  [name expr]
  (code-block "const ~A = ~A~%" name expr))


(defmacro const
  [name expr]
  `(code-block "const ~A = ~A~%" ~(str name) ~expr))

      
(defn interface
  [& funs]
  (apply code-block "interface {~%~@{  ~A~%~}}" funs))


(defn struct
  [& fields]
  (code-block "struct {~%~{  ~A~%~}}" fields))


(defn type*
  [name target]
  (code-block "type ~A ~A~%" name target))

  
(defmacro type
  [name target]
  `(type* ~(str name) ~target))


(defn func*
  [name params ret-type body]
  (code-block "func ~A(~{~A~^, ~}) ~A {\n~A\n}\n"
    name params ret-type body)) 


(defmacro func
  [name params ret-type body]
  `(func* ~(str name) ~params ~ret-type ~body))


(def code
  (type TransPortValid (interface (nt a-1 int))))
  ; (nt a3 int))

(def code2
  (func tagSignal [] uint8 "return 0"))

(comment "
(reset) (println go/code)
(nodes go/code)
(collect-tags :format-str go/code)
  ")