(ns language.common
  (:require
   [clojure.java.io :refer [file]]
   [clojure.string :as string]
   [clojure.tools.logging :refer [tracef]]
   [mylang
    :refer :all]))


;; Main dispatch functions.
;;
;; `emit` is the entry point for parsing.
;; It dispatches on the type of its argument.
;;
;; For example (emit (ls "asdf")) dispatches on `clojure.lang.IPersistentList`.
;;
;; The above example, along with some others, call `emit-special`
;; `emit-function` or `emit-function-call`.
;;
;; For example:
;;
;;  (emit (ls "asdf"))
;; calls
;;  (emit-special 'ls (ls "asdf"))
;;
;;  (emit (defn foo [a]
;;          "Docstring"
;;          (println "asdf")))
;; calls
;;  (emit-function foo "Docstring" [a] (println "asdf"))
;;
;;  (emit (test-fn 1 2 "a"))
;; calls
;;  (emit-function-call test-fn [1 2 "a"])
;;
;; Generally, the implementations of `emit` in pallet.stevedore.common, which
;; dispatch on compound types, should be sufficient for most implementations.
;;
;; The other emit-* functions are convenience functions
;; which avoid the need to reimplement all of `emit` for each Stevedore
;; implementation.

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

(defn macro?
  "Predicate to check if expr is a macro form"
  [expr]
  (contains? builtin-macros expr))

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
     (let [argseq (with-source-line-comments false
                    (->>
                     args
                     splice-args
                     (map emit)
                     (filter (complement string/blank?))
                     doall))]
       (apply emit-function-call fn-name-or-map argseq)))))

(defn- emit-s-expr [expr]
  (if (symbol? (first expr))
    (let [head (symbol (name (first expr))) ; remove any ns resolution
          expr1 (conj (splice-args (rest expr)) head)]
      (cond
       (and (= (first (str head)) \.) (> (count (str head)) 1))
       (emit-special 'dot-method expr1)

       (special-form? head) (emit-special head expr1)
       (infix-operator? head) (emit-infix head expr1)
       (macro? head) (emit (macroexpand-1 expr))
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

(defmethod emit-type :default
  [t] 
  ; (println "emit default type " t)
  (emit-type-builtin t))

(defmethod emit [::common-impl clojure.lang.IPersistentList] [expr]
  (emit-s-expr expr))

(defmethod emit [::common-impl clojure.lang.PersistentList] [expr]
  (emit-s-expr expr))

(defmethod emit [::common-impl clojure.lang.Cons]
  [expr]
  (if (= 'list (first expr))
    (emit-s-expr (rest expr))
    (emit-s-expr expr)))

(defmethod emit-special [::common-impl 'apply] [type [apply & exprs]]
  (emit-s-expr (spread exprs)))

(defn check-doc [expr]
  (if (string? (first expr))
    [(first expr) (next expr)]
    [nil expr]))

(defmethod emit-special [::common-impl 'fn] [type [fn & expr]]
  (let [name (first expr)
        [doc [signature & body]] (check-doc (next expr))]
    (emit-function name doc signature body)))

(defmethod emit-special [::common-impl 'struct] [_ [_ name & expr]]
  (let [[doc? fields] (check-doc expr)]
    (emit-struct name doc? fields)))

(defmethod emit-special [::common-impl 'enum] [_ [_ name & expr]]
  (let [[doc? fields] (check-doc expr)]
    (emit-enum name doc? fields)))

;;; Script combiner implementations

(defmethod do-script ::common-impl
  [& scripts]
  (str
   (->>
    scripts
    (map #(when % (string/trim %)))
    (filter (complement string/blank?))
    (string/join \newline))
   \newline))

(defn chain-with
  [chain-op scripts]
  (let [scripts (filter (complement string/blank?) scripts)
        sep (if *src-line-comments* " \\\n" " ")]
    (if (= 1 (count scripts))
      (first scripts)
      (->>
       scripts
       (map string/trim)
       (string/join (str " " chain-op sep))
       string/split-lines
       (string/join "\n") ; do not indent blocks to avoid heredoc issues
       string/trim))))

(defmethod chain-commands ::common-impl
  [& scripts]
  (chain-with "&&" scripts))

(def ^:dynamic *status-marker* "#> ")
(def ^:dynamic *status-fail* " : FAIL")
(def ^:dynamic *status-success* " : SUCCESS")

(defn checked-start [message]
  (str "echo '" message "...';"))

(defn checked-fail [message]
  (str "echo '" *status-marker* message *status-fail* "'; exit 1;"))

(defn checked-success [message]
  (str "echo '" *status-marker* message *status-success* "'"))

(defmethod checked-commands ::common-impl
  [message & cmds]
  (let [chained-cmds (apply chain-commands cmds)
        message (string/replace message #"'" "'\\\\''")]
    (if (string/blank? chained-cmds)
      ""
      (str
       (checked-start message) \newline
       "{\n" chained-cmds "\n } || { " (checked-fail message)
       "} >&2 " \newline
       (checked-success message) \newline))))