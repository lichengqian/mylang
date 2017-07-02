(in-ns 'language.golang)

(defn let->do [form]
  (-> form

      (morph-form (is-form? 'let*)
                  (fn [[_ bindings & body]]
                    (if (empty? bindings)
                      `(~'do ~@body)
                      (let [decls (->> (partition 2 bindings)
                                    (map (fn [[var value]] `(native-declare ~var ~value))))]
                        `(~'do ~@decls ~@body)))))))
                        

(defmethod transform ::golang
    [form]
    (->> form
        expand-macros-all
        let->do))
