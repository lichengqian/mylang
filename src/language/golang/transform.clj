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
  ;  (println "init struct " struct-name "fields" struct-fields)
   (let [new-fields   (->> 
                        (partition 2 struct-fields)
                        (mapcat (fn [[v t]]
                                  (if ((is-form? 'Map 'Set) t)
                                    [v `(native ~(str "make(" (emit-type t) ")"))]
                                    []))))]
      ; (println "init struct " struct-name "new-fields" new-fields)
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

(s/def ::catch-clause
  (s/cat  :catch #{'catch} 
          :classname symbol?
          :name symbol?
          :expr (s/* any?)))

(s/def ::finally-clause
  (s/cat  :finally #{'finally}
          :expr (s/* any?)))
          
;; try catch finally
(s/def ::try-clause
      (s/cat  :try #{'try} 
              :expr (s/+ (is-not-form? 'catch 'finally))
              :catch-clause (s/* (s/spec ::catch-clause))
              :finally-clause (s/? (s/spec ::finally-clause))))

(defn try-clause->defer
  [try-clause]
  ; (println "try-transfor->" try-clause)
  (let []
    `(do
      (defer ~@(get-in try-clause [:finally-clause :expr]))
      ~@(:expr try-clause))))

(s/def ::channel-val
  (s/tuple
    (s/or :wildchar #{'_}
          :value  symbol?)))

(s/def ::channel-op
  (s/cat :chan symbol? 
         :expr (s/spec
                  (s/cat :result ::channel-val :exprs (s/* any?)))))

(s/def ::alt!
  (s/cat  :alt! #{'alt!}
          :result-expr (s/* ::channel-op)))

(defn channel-op->case
  [m]
  (let [[t v] (get-in m [:expr :result 0])
        val (if (= t :wildchar) 
                `(<! ~(:chan m))
                `(let ~v (<! ~(:chan m))))]
      `(go:case ~val
          ~@(get-in m [:expr :exprs]))))

(defn alt!->select
  [m]
  `(go:select ~@(map channel-op->case (:result-expr m))))

;;; (case ) -> (go:switch )
(s/def ::case
  (s/cat  :case #{'case}
          :e any?
          :branches (s/+ (s/cat :condition any?
                                :action any?))
          :default (s/? any?)))

(s/def ::cond
  (s/cat  :cond #{'cond}
          :branches (s/+ (s/cat :condition #(not (= :else %))
                                :action any?))
          :default (s/? (s/cat :else #{:else} :action any?))))

(defn- branch->go:case 
  [m] 
  `(go:case ~(:condition m) ~(:action m)))

(defn case->go:switch
  [spec]
  (let [->go:default
        (fn [e]
          (if (nil? e)
            '()
            `((go:default ~e))))]
    `(go:switch [~(:e spec)]
        ~@(m/fmap branch->go:case (:branches spec))
        ~@(->go:default (:default spec)))))

(defn cond->go:switch
  [spec]
  (let [->go:default
        (fn [e]
          (if (nil? e)
            '()
            `((go:default ~(:action e)))))]
    `(go:switch []
        ~@(m/fmap branch->go:case (:branches spec))
        ~@(->go:default (:default spec)))))

(defn reduce-go-form [form]
  (prewalk*
    (spec-reducer
      ;; (nil? s) => (= s nil)
      (s/cat :nil? #{'nil?} :expr any?)
      (fn [m] `(~'= ~(:expr m) nil))
      ;; (not (= a b)) => (not= a b)
      (s/cat :not #{'not} :nest (s/spec (s/cat :nil? #{'=} :left any? :right any?)))
      (fn [m] `(~'not= ~(get-in m [:nest :left]) ~(get-in m [:nest :right])))
      ;;try -> defer
      ::try-clause
      try-clause->defer
      ;; (go (do xxx)) => (go xxx)
      (s/cat :go #{'go} :do (is-form? 'do))
      (fn [m] `(go ~@(rest (:do m))))
      ;; (alt! ) => (select )
      ::alt!
      alt!->select

      ::case
      case->go:switch
      ::cond
      cond->go:switch)
    form))

(defmethod transform ::golang
    [form]
    (->> form
        expand-macros-all
        init-all-struct-field
        let->do
        reduce-go-form))
