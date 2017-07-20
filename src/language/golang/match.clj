(in-ns 'language.golang)

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
            assign-var (fn  [s i]
                            (cond 
                                (= '_ s) ""
                                (string/starts-with? (name s) "*")
                                (cl-format nil "~A := &_s._~A\n" (.substring (name s) 1) i)
                                :else (cl-format nil "~A := _s._~A\n" (name s) i)))
            assign-vars (string/join (map assign-var vars idx))]
        (str "case *" (str (emit (first k)) ":\n")
            assign-vars
            (emit expr)))))
            
  ([expr]
   (str "default:\n"
     (emit expr))))

(defn- match-no-var?
    [exprs]
    (->> exprs
        (partition 2)
        (m/fmap first)
        (filter #(and (vector? %) (> (count %) 1)))
        empty?))

(defmethod emit-special [::golang 'match]
  [type [_ test & exprs]]
  (let [code-test (str (emit test) ".(type)")
        branches (partition-all 2 exprs)]
      (->> branches
          (map #(apply emit-branch %))
          (string/join "\n")
          braceln
          (str "switch "
              (if (match-no-var? exprs) "" "_s := ") 
              code-test))))

