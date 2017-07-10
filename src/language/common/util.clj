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

;;; reduce a form by symbol 
;;; when match symbol, call the function
(defn symbol-reducer [& cases]
  (let [branchs (->> (partition 2 cases)
                    (map (fn [[s f]] [(keyword s) f]))
                    (into {}))]
      (fn [tree]
        (if-let [f (and (seq? tree)
                    (symbol? (first tree))
                    (branchs (keyword (first tree))))]
              (f tree)
              tree))))

;;; reduce a form by spec 
;;; when match symbol, call the function
(defn spec-reducer [& cases]
  (let [branchs (->> (partition 2 cases))]
      (fn [tree]
        (loop [bs branchs]
          (if (empty? bs)
            tree
            (let [[s f] (first bs)
                  ret (s/conform s tree)]
                (if (= :clojure.spec.alpha/invalid ret)
                  (recur (rest bs))
                  (f ret))))))))

(defn prewalk*
  "prewalk until no more changes"
  [f form]
  (loop [form2 form]
    (let [expanded (walk/prewalk f form2)]
      (if (= form2 expanded)
        expanded
        (recur expanded)))))

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
         (symbol? (first f))
         (some true? (map #(= % (simple-symbol (first f))) s)))))

(defn is-not-form? [& s]
  (fn [f]
    (not
      (and (seq? f)
         (symbol? (first f))
         (some true? (map #(= % (simple-symbol (first f))) s))))))
  
(defn is-special-form? [s f]
  (and (seq? f)
       (= (first f) s)))

;;; can use spec !
(defn is-valid? [s]
  #(s/valid? s %))
