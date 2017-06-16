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
            assign-var (str (string/join ", " (map emit vars))
                            " := "
                            (string/join ", "
                                (map #(str "_s._" %) idx))
                            "\n")]
        (str "case *" (str (emit (first k)) ":\n")
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

