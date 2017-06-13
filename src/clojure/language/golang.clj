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

(def ^:private golang (Golang. emit emit-type add-import))

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
  #{'== '= '< '> '<= '>= '!= '<< '>> '<<< '>>> '& '|
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
  
  (let [[open close] (cond
                      (logical-operator? operator) ["[ " " ]"]
                      (arithmetic-operators operator) ["(" ")"]
                      :else ["" ""])
        quoting (if (quoted-operator? operator) quoted identity)]
    (str open (emit-quoted-if-not-subexpr quoting (first args)) " "
        (get infix-conversions operator operator)
        " " (emit-quoted-if-not-subexpr quoting (second args)) close)))

(defemit-special ::golang
  'ns [path] (set-ns (emit path))
  'import [path] (add-import path)
  'not [expr] (str "!(" (emit expr) ")")
  'chan [t n] (str "make(chan " (emit t) ", " (emit n) ")")
  '<! [c] (str "<-" (emit c))
  '>! [c v] (str (emit c) "<- " (emit v)))

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

    ;; This requires bash 4
    ;; (str
    ;;  "{ "
    ;;  "declare -a " (emit var-name) "; "
    ;;  (check-symbol (emit var-name)) "=" (emit expr)
    ;;  "; }")

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
  clojure.lang.Symbol (str expr)

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

(defmethod emit-special [::golang 'return] [type [return expr]]
  (str "return " (emit expr)))

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

(defmethod emit-special [::golang 'type]
  [_ [_ alias real-type]]
  (str "type " (emit alias) " " (emit-type real-type) "\n\n"))

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
    (.emitStruct golang name fields)))

(defmethod emit-enum ::golang 
  [name doc? enums]
  (assert (symbol? name))
  (str 
    (emit-doc doc?)
    (.emitEnum golang name enums)))

;;; return function
(def ^:dynamic *return* (fn [v] (str "return " (emit v))))

(defmethod emit-special [::golang 'return]
  [_ [_ v]]
  (*return* v))

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

(defmethod emit-special [::golang 'go]
  [_ [_ & body]]
  (if (= 1 (count body))
      (str "go " (emit-do body))
      (str  "go func() {\n"
            (emit-do body)
            "}()\n")))
  
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

;; We would like to be able to add source comments for each argument of a
;; function inline, but this is not possible (only works in a |, || or &&
;; pipeline).
(defmethod emit-function-call ::golang
  [name & args]
  ; (println (str name) args)
  (cond
    (string/ends-with? (str name) ".")    ; enum constructor
    (->> args
        (map emit)
        (string/join ", ")
        brace
        (str (substring (str name) 0 (- (count (str name)) 1))))

    (string/starts-with? (str name) "map->")  ; struct constructor 
    (do
      (println (into [] (first args)))
      (->> (into [] (first args))
          (map (fn [[k v]] (str (emit k) ": " (emit v) ",\n")))
          string/join
          braceln
          (str (substring (str name) 5))))
        
    :else
    (case (str name)
      "sleep" (do
                  (add-import "time")
                  (str "time.Sleep(" (first args) " * time.Millisecond)"))

      "nil?" (str (emit (first args))
                  " == nil")

      "some?" (str (emit (first args))
                  " /= nil")

      "get"   (let [mv (first args)
                    k  (second args)]
                  (str (emit mv) (bracket (emit k))))

      "assoc" (let [mv (first args)
                    kvs (partition 2 (rest args))
                    emit-kv (fn [[k v]]
                                (str  (emit mv) 
                                      (bracket (emit k)) 
                                      " = "
                                      (emit v)
                                      "\n"))]
                  (->> kvs
                    (map emit-kv)
                    string/join))

      "newMVar" (do
                    (add-import "sync")
                    (str
                      (->> 
                          (str "value " (emit-type (typeof name))
                              "\n sync.Mutex")
                          braceln
                          (str "struct"))
                      (->> 
                          (str "value: " 
                              (emit (first args))
                              ",")
                          braceln)))

      "withMVar" (do
                    (str
                      (->>  ; func () RET 
                          (typeof name)
                          emit-type
                          (str "func () "))
                      (->   ; body
                          (first args)
                          emit
                          (#(str % ".Lock()\n"
                                 "defer " % ".Unlock()\n"
                                 (emit-do (rest args))))
                          braceln)
                      "()\n"))

      (if (seq args)
        (->> args
          (map emit)
          (string/join ", ")
          paren
          (str (emit name)))
        ; (str (emit name) "(" (reduce str (interpose "," args)) ")")
        (str (emit name) "()")))))

(defmethod emit-type-builtin ::golang
  [t]
  ; (println t)
  (if (nil? t)
    ""
    (case (str t)
      "Void" ""
      "Bool" "bool"
      "UInt32" "uint32"
      "UInt64" "uint64"
      "String" "string"
      "ByteString" "[]byte"
      "Error" "error"
      "Chan" "chan"
      "Lock"  (do (add-import "sync") "sync.Mutex")

      "Listener" (do (add-import "net") "net.Listener")
      "Conn" (do (add-import "net") "net.Conn")
      (emit t))))

(defmethod emit-type-constructor ::golang
  [c args]
  (case (str c)
    "Either" (str "(" (emit-type (second args)) ", " (emit-type (first args)) ")")
    "Map" (str "map[" (emit-type (first args)) "]*" (emit-type (second args)))
    "Set" (str "map[" (emit-type (first args)) "]struct{}")
    "IO" (str "(" (emit-type (first args)) ", error)")
    "MVar" 
    (do
        (add-import "sync")
        (string/join "\n"
            ["struct {"
             (str "  value " (emit-type (first args)))
             "  sync.Mutex"
             "}"]))

    (->> args
        (map emit-type)
        (string/join " ")
        (str (emit-type c) " "))))
        

(defmethod emit-ns ::golang
  [s]
  (str "package " s))

(defmethod emit-macro-call [::golang 'decode!]
  [_ enum-name]
  (.macro_decode golang
    enum-name 
    (get-enum enum-name)))

(defmethod emit-macro-call [::golang 'encode!]
  [_ enum-name]
  (.macro_encode golang
    enum-name 
    (get-enum enum-name)))

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

;;; defn / fn support 
(defn- emit-function-decl
  [sig body]
  (println (meta sig))
  (with-local-vars [has-err false]
    (letfn [(check-error-return? [body]
                (prewalk #(if (or (= '<- %) (= 'throw %)) 
                              (do
                                (var-set has-err true)
                                %)
                              %)
                        body))
            (emit-function-sig
              [sig]
              (let [ret-type (if @has-err [(typeof sig), 'Error] [(typeof sig)])
                    args (->> sig
                          (map emit-arg)
                          (string/join ", ")
                          paren)]
                  (str args " " 
                      (emit-type (filterv some? ret-type)))))

            (error-code []
              (if (nil? (typeof sig))
                "return err\n"
                "return nil, err\n"))

            (emit-return [v]
              (str "return " (emit v)
                (if (nil? (typeof sig))
                  "\n"
                  ", nil")))

            (emit-body [body]
              (if @has-err
                (with-bindings {#'*error-code* (error-code)
                                #'*return* emit-return}
                  (emit-do body))
                (emit-do body)))]

      (check-error-return? body)
      (str (emit-function-sig sig) " {\n"
          (emit-body body)
          "}\n\n"))))
  
(defmethod emit-function ::golang
  [name doc? sig body]
  (assert (symbol? name))
  (str (emit-doc doc?)
      "func " name
      (emit-function-decl sig body)))

(defmethod emit-special [::golang 'throw] 
  [_ [ _ err]]
  (str "return " (emit err)))

(defmethod emit-special [::golang 'fn] 
  [_ [ _ sig & body]]
  (str "func "
      (emit-function-decl sig body)))

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

;;; enum match support 
(defn- emit-branch
  ([k expr]
   (cond
      (symbol? k)
      (str "case " (emit k) ":\n"
          (emit expr))

      (vector? k)
      (let [vars (rest k)
            idx (range 1 (+ 1 (count vars)))
            assign-var (str (string/join ", " (map emit vars))
                            " := "
                            (string/join ", "
                                (map #(str "_s._" %) idx))
                            "\n")]
        (str "case *" (str (first k) ":\n")
            assign-var
            (emit expr)))))
            
  ([expr]
   (str "default:\n"
     (emit expr))))

(defmethod emit-special [::golang 'match]
  [type [_ test & exprs]]
  (let [code-test (str (emit test) ".(type)")
        branches (partition 2 exprs)]
      (->> branches
          (map #(apply emit-branch %))
          (string/join "\n")
          braceln
          (str "switch _s := " code-test))))

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
