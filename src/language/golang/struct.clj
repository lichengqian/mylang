(in-ns 'language.golang)

(defmethod emit-struct ::golang
  [name doc? fields]
  (assert (symbol? name))
  (letfn [(emit-setter [[n t]]
            (if (:setter (meta n))
                (str "func (p *" name ") Set" (string/capitalize n)
                    (paren (str "v " (emit-type t)))
                    (braceln (str "p." n " = v"))
                    "\n")
                ""))]
    (str
        (emit-doc doc?)
        (str "type " name " struct"
            (->> (partition 2 fields)
                (map (fn [[v t]]
                        (str v " " (emit-type t) "\n")))
                string/join
                braceln))
        "\n"
        ;; setter for :set meta data
        (->> (partition 2 fields)
            (map emit-setter)
            (string/join)))))
