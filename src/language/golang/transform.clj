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
                        
;;; 自动初始化指定struct的所有map类型的字段
(defn init-struct-field 
  ([form] form)
  ([form [_ struct-name & struct-fields]]
   (println "init struct " struct-name "fields" struct-fields)
   (let [new-fields   (->> 
                        (partition 2 struct-fields)
                        (mapcat (fn [[v t]]
                                  (if (maptype? t)
                                    [v `(native ~(str "make(" (emit-type t) ")"))]
                                    []))))]
      (println "init struct " struct-name "new-fields" new-fields)
      (if (empty? new-fields)
        form
        (morph-form form
          (map-constructor? struct-name)
          (fn [[c fields]]
            `(~c ~(apply assoc fields new-fields))))))))

(defn init-all-struct-field [form] 
  (->> form
    (filter (is-form? 'struct))
    (reduce init-struct-field form)))

(defn reduce-go-form [form]
  (prewalk*
    (spec-reducer
      ;; (nil? s) => (= s nil)
      (s/cat :nil? #{'nil?} :expr any?)
      (fn [m] `(~'= ~(:expr m) nil))
      ;; (not (= a b)) => (not= a b)
      (s/cat :not #{'not} :nest (s/spec (s/cat :nil? #{'=} :left any? :right any?)))
      (fn [m] `(~'not= ~(get-in m [:nest :left]) ~(get-in m [:nest :right]))))

    form))

(defmethod transform ::golang
    [form]
    (->> form
        expand-macros-all
        init-all-struct-field
        let->do
        reduce-go-form))
