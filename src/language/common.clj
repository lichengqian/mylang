(ns language.common
  (:require
   [clojure.java.io :refer [file]]
   [clojure.string :as string]
   [clojure.tools.logging :refer [tracef]]
   [clojure.walk :as walk]
   [clojure.zip :as zip]
   [clojure.pprint :refer [pprint]]
   [clojure.spec.alpha :as s]

   [mylang
    :refer :all]))

(load "common/util")
(load "common/transform")

;; 代码生成主流程：
;; (-> file
;;   read-form    读取源文件，生成form格式     :raw-form
;;   transform    form格式转换，生成form格式   :form
;;   compile      编译，生成ast for target    :ast
;;   emit)        生成代码，string格式，处理package/import等,参考JavaPoet设计 :code
;;

(defmulti transform
  "source to source transformation"
  (fn [form] *script-language*))

(defmulti emit-special
  "Emit a form as a string. Dispatched on the first element of the form."
  (fn [ & args] [*script-language* (identity (first args))]))

(defmulti emit-function
  "Emit a function define"
  (fn [name doc? sig body] *script-language*))

(defmulti emit-type
  "Emit a type define"
  (fn [expr] [*script-language* (type expr)]))

(defmulti emit-type-builtin
  "Emit a built in type define"
  (fn [t] *script-language*))

(defmulti emit-type-constructor
  "Emit a type constractor"
  (fn [constructor args] *script-language*))

(defmulti emit-struct
  "Emit a struct define"
  (fn [name doc? & fields] *script-language*))

(defmulti emit-enum
  "Emit a enum define"
  (fn [name doc? & fields] *script-language*))

(defmulti emit-function-call
  "Emit a function call"
  (fn [name & args] *script-language*))

(defmulti emit-macro-call
  "Emit a macro call"
  (fn [name & args] [*script-language*, name]))

(defmulti emit-infix
  (fn [type [operator & args]] *script-language*))

;; 辅助macro，方便定义具体实现
;; 定义三元组: form params exprs
(defmacro defemit-special [lang & body]
  (let [ps (partition 3 body)
        codes (for [[t ps e] ps]
               `(defmethod emit-special [~lang ~t] [type# [h# ~@ps]] ~e))]
    (list* 'do codes)))

;; Common functions/predicates 

(defn compound-form?
  "Predicate to check if expr is a compound form"
  [expr]
  (= 'do  (first expr)))

(defmulti infix-operator?
  "Predicate to check if expr is an infix operator. Each implementation
  should implement it's own multimethod."
  (fn [expr] *script-language*))

;;; Predicates for keyword/operator classes
(defn special-form?
  "Predicate to check if expr is a special form"
  [expr]
  (contains? special-forms expr))

;;; Implementation coverage tests
;;;
(defn- emit-special-implemented? [impl special-function]
  "Predicate for successfully dispatched special functions."
  (try
    (with-script-language impl
      (emit-special special-function)
      true
      (catch IllegalArgumentException e
        (not
         (.contains
          (str e)
          (str
           "No method in multimethod 'emit-special' for dispatch value: ["
           impl " " special-function "]"))))
      (catch Exception e true))))

(defn emit-special-implemented [impl]
  "Returns a vector of successfully dispatched special functions.
   Example usage:
       (emit-special-implemented :pallet.stevedore.bash/bash)"
  (filter #(emit-special-implemented? impl %) special-forms))

(defn emit-special-not-implemented [impl]
  "Returns a vector of special-functions that fail to dispatch.
       (emit-special-not-implemented :pallet.stevedore.bash/bash)"
  (remove #(emit-special-implemented? impl %) special-forms))

;;; Common implementation
(defn- ex-location [m]
  (str "(" (or (and (:file m) (.getName (file (:file m)))) "UNKNOWN")
       ":" (:line m) ")"))

(defn- is-macro [fn]
  (and  (symbol? fn)
        (string/ends-with? (name fn) "!")))

(defmethod emit-special [::common-impl 'invoke]
  [type form]
  (let [[fn-name-or-map & args] form]
    (tracef "INVOKE %s %s %s" (class fn-name-or-map) fn-name-or-map args)
    (tracef "INVOKE %s" (meta form))
    (cond
     (= fn-name-or-map splice-seq)
     (string/join " " (first args))

     (or (fn? fn-name-or-map) (instance? clojure.lang.MultiFn fn-name-or-map))
     (try
       (apply fn-name-or-map (splice-args args))
       (catch clojure.lang.ArityException e
         ;; Add the script location to the error message, and use the
         ;; unmangled script function name.
         (throw
          (ex-info
           (str "Wrong number of args (" (.actual e) ") passed to: "
                (name (or (:fn-name (meta fn-name-or-map)) "unnamed function"))
                " "
                (ex-location (meta form)))
           (merge
            (meta fn-name-or-map)
            {:actual (.actual e)
             :script-fn (:fn-name (meta fn-name-or-map))})
           e)))
       (catch Exception e
         ;; Add the script location and script function name to the error
         ;; message
         (throw
          (ex-info
           (str (.getMessage e) " in call to "
                (:fn-name (meta fn-name-or-map)) " " (ex-location form))
           (merge
            (meta fn-name-or-map)
            {:script-fn (:fn-name (meta fn-name-or-map))})
           e))))

     :else
     (let [_result
            (if (is-macro fn-name-or-map)
                (apply emit-macro-call
                    (symbol (name fn-name-or-map)) args)
                (apply emit-function-call
                    fn-name-or-map args))]
        (cond 
          (string? _result) _result
          :else (emit _result))))))

(defn- emit-s-expr [expr]
  (if (symbol? (first expr))
    (let [head (symbol (name (first expr))) ; remove any ns resolution
          expr1 (conj (splice-args (rest expr)) head)]
      (cond
       (and (= (first (str head)) \.) (> (count (str head)) 1))
       (emit-special 'dot-method expr1)

       (special-form? head) (emit-special head expr)

       (infix-operator? head) (emit-infix head expr1)
       :else (emit-special 'invoke expr)))
    (emit-special 'invoke expr)))

(defn- spread
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (next arglist)) (seq (first arglist))
   :else (apply list (first arglist) (spread (next arglist)))))

(defmethod emit-type [::common-impl clojure.lang.IPersistentList]
  [[c & args]]
  (emit-type-constructor c args))

(defmethod emit-type [::common-impl clojure.lang.IPersistentVector]
  [types]
  (->> types
      (map emit-type)
      (string/join ", ")
      paren))

(defmethod emit-type [::common-impl java.lang.String]
  [t]
  (str t))

(defmethod emit-type :default
  [t] 
  ; (println "emit default type " t)
  (emit-type-builtin t))

(defmethod emit [::common-impl clojure.lang.IPersistentList] [expr]
  (emit-s-expr expr))

(defmethod emit [::common-impl clojure.lang.PersistentList] [expr]
  (emit-s-expr expr))

(defmethod emit [::common-impl clojure.lang.LazySeq] [exprs]
  ; (print "emit LazySeq" exprs)
  (emit-s-expr exprs))

(defmethod emit [::common-impl clojure.lang.Cons]
  [expr]
  (if (= 'list (first expr))
    (emit-s-expr (rest expr))
    (emit-s-expr expr)))

(defmethod emit-special [::common-impl 'apply] [type [apply & exprs]]
  (emit-s-expr (spread exprs)))

(defmethod emit-special [::common-impl 'native] [_ [_ & exprs]]
  (let [lang (first exprs)
        code (second exprs)]
    (if (= lang (keyword (name *script-language*)))
      (string/split-lines code)
      (str "// not for lang " lang))))

(defmethod emit-special [::common-impl 'defn] [type [fn & expr]]
  (let [name (first expr)
        [doc [signature & body]] (check-doc (next expr))]
    (emit-function name doc signature body)))

(defmethod emit-special [::common-impl 'struct] [_ [_ name & expr]]
  (let [[doc? fields] (check-doc expr)]
    (emit-struct name doc? fields)))

(defmethod emit-special [::common-impl 'enum] [_ [_ name & expr]]
  (let [[doc? fields] (check-doc expr)]
    (add-enum name fields)
    (emit-enum name doc? fields)))

(def ^:dynamic *error-code* "*error-code*")
