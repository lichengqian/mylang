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

