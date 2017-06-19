(in-ns 'language.golang)

(defn- decl-interface [name]
    (->> (str "tag" name "() uint8\n"
            "String() string\n")
        brace
        (str "type " name " interface")))

(defmethod emit-enum ::golang 
  [name doc? enums]
  (assert (symbol? name))
  (letfn [(impl-interface [idx value]
            (let [[self constructor] 
                  (if (list? value)
                    [(str "*" (first value))
                     (first value)]
                    [value value])]
                (str
                    ;; tag function
                    "func (p " self ") tag" name "() uint8 "
                    (braceln (str "return " idx))
                    "\n"
                    ;; string function
                    "func (p " self ") String() string"
                    (braceln (str "return \"" constructor "\"")))))

          (enumValueSingle [idx value]
            (str "type " value " struct{}\n"
                (impl-interface idx value)))
                
          (enumValueList [idx [constructor & fields :as value]]
            (str
                (->> fields
                    (map #(str "_" %1 " " (emit-type %2)) (range 1 10))
                    (string/join "\n")
                    braceln
                    (str "type " constructor " struct"))
                "\n"
                (impl-interface idx value)))

          (enumValue [idx value]
            (cond
                (instance? clojure.lang.IPersistentList value)
                (enumValueList idx value)

                :else
                (enumValueSingle idx value)))]

    (str
        (emit-doc doc?)
        (->> enums
            (map #(enumValue %1 %2) (range))
            (string/join "\n")
            (str (decl-interface name) "\n")))))


