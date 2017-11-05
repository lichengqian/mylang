(in-ns 'language.common)

;;; utils 

(defn paren [s]
    (str "(" s ")"))

(defn bracket [s]
    (str "[" s "]"))

(defn brace [s]
    (str "{" s "}"))

(defn braceln [s]
    (str "{\n" s "\n}"))

(defn check-doc [expr]
  (if (string? (first expr))
    [(first expr) (next expr)]
    [nil expr]))


(defn maptype? 
  [type-exp]
  (and 
    (list? type-exp)
    (symbol? (first type-exp))
    (= 'Map (first type-exp))))

(defn map-constructor? [s]
  (fn [f]
    (and (seq? f)
         (symbol? (first f))
         (or
            (string/starts-with? (name (first f)) 
              (str "map->" (name s)))
            (string/starts-with? (name (first f)) 
              (str "map->&" (name s)))))))

;;; can use spec !
(defn is-valid? [s]
  #(s/valid? s %))
