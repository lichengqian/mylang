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

(defn simple-symbol [s]
    (symbol (name s)))

;;; see http://ferret-lang.org/#outline-container-sec-3

(defn morph-form [tree pred f]
  (walk/prewalk (fn [x]
                  (if (pred x)
                    (f x)
                    x)) tree))

(defn remove-form [tree pred]
  (if (every? true? (map #(pred %) tree))
    (list)
    (loop [loc (zip/seq-zip tree)]
      (if (zip/end? loc)
        (zip/root loc)
        (recur
         (zip/next
          (if (pred (zip/node loc))
            (zip/remove loc)
            loc)))))))

(defn select-form [tree pred]
  (loop [loc (zip/seq-zip tree)
         nodes []]
    (if (zip/end? loc)
      nodes
      (recur
       (zip/next loc)
       (if (pred (zip/node loc))
         (conj nodes (zip/node loc))
         nodes)))))

(defn is-form? [& s]
  (fn [f]
    (and (seq? f)
         (some true? (map #(= % (first f)) s)))))

(defn is-special-form? [s f]
  (and (seq? f)
       (= (first f) s)))
