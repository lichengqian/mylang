(ns language.golang
  (:require [pallet.common.resource :as resource]
            [pallet.common.string :as common-string]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.walk :refer [prewalk]])
  (:use
   [language.common]
   [mylang]
    ; :only [emit emit-do special-forms splice-seq with-source-line-comments]
    
   [pallet.common.string :only [quoted substring underscore]])
  (:import language.Golang))

(derive ::golang :language.common/common-impl)

;;; * Keyword and Operator Classes
(def infix-operators
  ^{:doc "Operators that should be converted to infix in expressions."
    :private true}
  #{'+ '- '/ '* '% '== '= '< '> '<= '>= '!= '<< '>> '<<< '>>> '& '| '&& '||
    'and 'or})

(def arithmetic-operators
  ^{:doc "Operators that should be converted to infix in expressions."
    :private true}
  #{'+ '- '/ '* '%})

(def logical-operators
  ^{:doc "Logical operators for test expressions."
    :private true}
  #{'== '< '> '<= '>= '!= '<< '>> '<<< '>>> '& '|
    'file-exists? 'directory? 'symlink? 'readable? 'writeable? 'empty?})

(def
  ^{:doc "Operators that should quote their arguments."
    :private true}
  quoted-operators
  (disj logical-operators 'file-exists? 'directory? 'symlink 'can-read 'empty?))

(def
  ^{:doc "Conversion from clojure operators to shell infix operators."
    :private true}
  infix-conversions
     {'&& "&&"
      'and "&&"
      '|| "||"
      'or "||"
      '< "-lt"
      '> "-gt"
      '<= "-le"
      '>= "-ge"
      '= "=="})

(def ^{:dynamic true :private true
       :doc "Control output of delimiters on sequences"}
  *delimited-sequence* true)

;; Helper functions for generating shFlags declarations
;; and initializations

(defn- deconstruct-sig [sig]
  "Returns a vector with the first element being a vector
  of arguments and second being a vector of flags"
  (assert (vector? sig))
  (let [[args flags :as dsig] (split-with symbol? sig)]
    (assert (or (empty? flags) (every? vector? flags)))
    dsig))

(defmethod infix-operator? ::golang [expr]
  (contains? infix-operators expr))

(defn- logical-operator?
  "Predicate to check if expr is a logical operator"
  [expr]
  (contains? logical-operators expr))

(defn- quoted-operator?
  "Predicate to check if expr is a quoted operator"
  [expr]
  (contains? quoted-operators expr))

(defn- logical-test?
  "Check whether a condition should be wrapped in []"
  [test]
  ;; this is hairy
  (and (sequential? test)
       (or (infix-operator? (first test))
           (and (= 'not (first test))
                (let [test2 (fnext test)]
                  (logical-test? test2)))
           (and (not= 'not (first test)) (logical-operator? (first test))))))

;;; Emit special forms
(defn- emit-quoted-if-not-subexpr [f expr]
  (let [s (emit expr)]
    (if (or (.startsWith s "\\(")
            (.startsWith s "!")
            (.startsWith s "-")
            (.startsWith s "@"))
      s
      (f s))))

(defmethod emit-infix ::golang [type [operator & args]]
  (when (< (count args) 2)
    (throw (Exception. "Less than 2 infix arguments not supported yet.")))
  
  (let [[open close] ["(" ")"]
        quoting (if (quoted-operator? operator) quoted identity)]
    (str open (emit (first args)) " "
        (get infix-conversions operator operator)
        " " (emit (second args)) close)))

(defemit-special ::golang
  'ns [path] (set-ns (emit path))
  'import [path] (add-import path)
  'not [expr] (str "!(" (emit expr) ")"))

(defn- emit-monad-binding
  ([v expr]
   (str (emit v)
        ", err := "
        (emit expr)
        "\n  if err != nil {\n "
        *error-code*
        " }"))
  ([expr]
   (str "err := "
        (emit expr)
        "\n  if err != nil {\n "
        *error-code*
        " }")))

(defmethod emit-special [::golang '<-] [_ [_ & args]]
  (apply emit-monad-binding args))

(defmethod emit-special [::golang 'native] [_ [_ & lines]]
  (string/join "\n" lines))
  
(defn- check-symbol [var-name]
  (when (re-matches #".*-.*" var-name)
    (throw
     (ex-info
      (format "Invalid bash symbol %s" var-name)
      {:type :invalid-bash-symbol})))
  var-name)

(defn- munge-symbol [var-name]
  (let [var-name (string/replace var-name "-" "__")
        var-name (string/replace var-name "." "_DOT_")
        var-name (string/replace var-name "/" "_SLASH_")]
    var-name))

(defn- set-map-values
  [var-name m]
  (str "{ "
         (string/join ""
          (map
           #(format "hash_set %s %s %s; "
                    (munge-symbol (emit var-name))
                    (munge-symbol (emit (first %)))
                    (emit (second %)))
           m))
         " }"))

(defmethod emit-special [::golang 'val] [type [var var-name expr]]
  (if (instance? clojure.lang.IPersistentMap expr)
    (set-map-values var-name expr)
    (str
     (check-symbol (emit var-name)) ":=" (emit expr))))

(defmethod emit-special [::golang 'alias] [type [alias name expr]]
  (str "alias " (emit name) "='" (emit expr) "'"))

(defmethod emit-special [::golang 'str] [type [str & args]]
  (apply clojure.core/str (map emit args)))

(defmethod emit-special [::golang 'quoted] [type [quoted & args]]
  (common-string/quoted (string/join " " (map emit args))))

(defmethod emit-special [::golang 'println] [type [println & args]]
  (add-import "fmt")
  (str "fmt.Println(" (string/join ", " (map emit args)) ")"))

(defmethod emit-special [::golang 'print] [type [print & args]]
  (add-import "fmt")
  (str "fmt.Print(" (string/join ", " (map emit args)) ")"))

(defemit ::golang expr
  nil "nil"
  java.lang.Boolean (str expr)
  java.lang.Integer (str expr)
  java.lang.Long (str expr)
  java.lang.String (str "\"" expr "\"")

  clojure.lang.Ratio (str (float expr))
  clojure.lang.Keyword (name expr)
  clojure.lang.Symbol 
    (if (= 'Void expr)
      "struct{}{}"
      (str (name expr)))

  clojure.lang.IPersistentVector
    (str (if *delimited-sequence* "(" "")
       (string/join " " (map emit expr))
       (if *delimited-sequence* ")" ""))
  
  clojure.lang.IPersistentMap
    (letfn [(subscript-assign
             [pair]
             (str "[" (emit (key pair)) "]=" (emit (val pair))))]
      (str "(" (string/join " " (map subscript-assign (seq expr))) ")")))
  

;; TODO should this even exist?
;; It causes seemingly unnessessary conflicts with ::common-impl implementations
;; we don't buy much by having it.
;;
;;(defmethod emit [::golang java.lang.Object] [expr]
;;  (str expr))

;;; TODO move to pallet.common.string
(defn comma-list
  "Emit a collection as a parentesised, comma separated list.
       (comma-list [a b c]) => \"(a, b, c)\""
  [coll]
  (str "(" (string/join ", " coll) ")"))


(defn emit-method [obj method args]
  (str (emit obj) "." (emit method) (comma-list (map emit args))))

(defn- emit-body-for-if [form]
  (if (or (compound-form? form)
          (= 'if (first form))
          (.contains (emit form) "\n"))
    (str "{\n" (emit form) "}")
    (str "{\n " (emit form) "}")))

(defmethod emit-special [::golang 'if] [type [if test true-form false-form]]
  (str "if "
       (emit test) " "
       (emit-body-for-if true-form)
       (str " else " (emit-body-for-if false-form))
       "\n"))
       
(defmethod emit-special [::golang 'when] [type [if test & forms]]
  (str "if "
       (emit test) " "
       (braceln (emit-do forms))
       "\n"))
       
(defmethod emit-special [::golang 'dot-method] [type [method obj & args]]
  (let [method (symbol (substring (str method) 1))]
    (emit-method obj method args)))

(defmethod emit-special [::golang 'set!] [type [set! var val]]
  (str (check-symbol (emit var)) "=" (emit val)))

(defmethod emit-special [::golang 'new] [type [new class & args]]
  (str "new " (emit class) (comma-list (map emit args))))

(defmethod emit-special [::golang 'aget] [type [aget var idx]]
  (str "${" (emit var) "[" (emit idx) "]}"))

(defmethod emit-special [::golang 'aset] [type [aget var idx val]]
  (str (emit var) "[" (emit idx) "]=" (emit val)))

(defmethod emit-special [::golang 'merge!] [type [merge! var-name expr]]
  (set-map-values var-name expr))

(defmethod emit-special [::golang 'assoc!] [type [merge! var-name idx val]]
  (format
   "hash_set %s %s %s"
   (munge-symbol (emit var-name))
   (munge-symbol (emit idx))
   (emit val)))

(defmethod emit-special [::golang 'deref]
  [type [deref expr
         & {:keys [default
                   default-value
                   default-assign
                   default-assign-value
                   alternate
                   alternate-value
                   error
                   error-value]}]]
  (if (instance? clojure.lang.IPersistentList expr)
    (str "$(" (with-source-line-comments false (emit expr)) ")")
    (str "${" (with-source-line-comments false (emit expr))
         (cond
          default (str "-" (emit default))
          default-value (str ":-" (emit default-value))
          default-assign (str "=" (emit default-assign))
          default-assign-value (str ":=" (emit default-assign-value))
          alternate (str "+" (emit alternate))
          alternate-value (str ":+" (emit alternate-value))
          error (str "?" (emit error))
          error-value (str ":?" (emit error-value)))"}")))


(defmethod emit-special [::golang 'do] [type [ do & exprs]]
  (emit-do exprs))

(defmethod emit-special [::golang 'doseq]
  [type [ doseq [arg values] & exprs]]
  (str "for " (emit arg)
       " in " (binding [*delimited-sequence* false] (emit values))
       "; do\n"
       (emit-do exprs)
       "done"))

(defmethod emit-special [::golang 'group]
  [type [ group & exprs]]
  (str "{\n" (string/join (emit-do exprs)) "}"))

(defmethod emit-special [::golang 'pipe]
  [type [ pipe & exprs]]
  (chain-with "|" (map emit exprs)))

(defmethod emit-special [::golang 'chain-or]
  [type [chain-or & exprs]]
  (chain-with "||" (map emit exprs)))

(defmethod emit-special [::golang 'chain-and]
  [type [chain-and & exprs]]
  (chain-with "&&" (map emit exprs)))

(defn emit-field [name type]
  (str (emit name) " " (emit type) "\n"))

(defn emit-fields [exprs]
  (->> exprs
       (map #(apply emit-field %))
       string/join))

(defn emit-doc [doc]
  (when doc
    (->> doc
         string/split-lines
         (map #(str "// " % "\n"))
         string/join)))
        
(defmethod emit-struct ::golang
  [name doc? fields]
  (assert (symbol? name))
  (str 
    (emit-doc doc?)
    (str "type " name " struct"
        (->> (partition 2 fields)
            (map (fn [[v t]]
                    (str v " " (emit-type t) "\n")))
            string/join
            braceln))))

(defn- typeof [v]
  (:tag (meta v)))

(defn- emit-arg [arg]
  (str (name arg) " " (emit-type (typeof arg))))

(defn- emit-var [var]
    (if (vector? var)
        (string/join ","
            (for [v var]
              (str v)))
        (str var)))

(defn- emit-let [body]
  (string/join "\n"
    (for [[var expr] (partition 2 body)]
        (str (emit-var var) " := " (emit expr)))))
  
(defmethod emit-special [::golang 'let]
  [_ [_ & body]]
  (emit-let body))

(defmethod emit-special [::golang 'main]
  [_ [_  & body]]
  (set-ns "main")
  (emit-function 'main nil [] body))

(defmethod emit-special [::golang 'deferr]
  [_ [_ & body]]
  (add-import "errors")
  (->> (partition 2 body)
       (map (fn [[k v]] (str "Err" (name k) " = errors.New(" (emit v) ")")))
       (string/join "\n")
       paren
       (str "var ")))

(defmethod emit-special [::golang 'deftest]
  [_ [_ n & body]]
  (add-import "testing")
  (with-bindings {#'*error-code* "t.Error(err)\n return\n"}
    (->> body
        emit-do
        braceln
        (str "func Test"
              (string/capitalize (str n))
              "(t *testing.T)"))))

(defmethod emit-special [::golang 'tlog]
  [_ [_ & args]]
  (str "t.Log("
    (string/join ", "
      (for [arg args]
        (emit arg)))
    ")"))

(defmethod emit-ns ::golang
  [s]
  (str "package " s))

(defn default-go-ns [path]
    (-> path
        io/file
        .getParentFile
        .getName))

(defmethod emit-import ::golang
    [imports]
    (->> imports
        (map emit)
        (string/join "\n")
        paren
        (str "import ")))

;;; loop / recur support
(def ^:dynamic *recur* nil)

(defmethod emit-special [::golang 'loop]
  [_ [_  bindings & exprs]]
  (let [vars (map first (partition 2 bindings))
        fn-recur (fn [args]
                    (->> args
                        (map #(str (str %1) " = " (emit %2)) vars)
                        (string/join "\n")))]
      (str (emit-let bindings) "\n"
          (with-bindings {#'*recur* fn-recur}
            (->> exprs
              emit-do
              braceln
              (str "for"))))))

(defmethod emit-special [::golang 'recur]
  [_ [_  & exprs]]
  (str (*recur* exprs) "\ncontinue\n"))

;;; defrecord support
(defmethod emit-special [::golang 'defrecord]
  [type [_ n fields]]
  (let [emit-field (fn [field]
                      (str (emit field) " "
                           (emit-type (typeof field))
                           "\n"))]
      (->> fields
          (map emit-field)
          string/join
          brace
          (str "type " n " struct"))))

(defmulti go-call
    (fn [f & args] (simple-symbol f)))

(load "golang/enum")
(load "golang/call")
(load "golang/async")
(load "golang/type")
(load "golang/fn")
(load "golang/match")
(load "golang/macro") 