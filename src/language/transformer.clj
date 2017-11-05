(ns language.transformer
  (:require [language.reader :refer :all]))


;;; Source to Source transfermations
(def core-macro-symbols #{'if-not 'when-not '-> '->>})

(defn eval-macros [form]
  (let [form-macros (->> (filter (is-form? 'defmacro) form)
                         (filter (fn [[_ name]]
                                   (not (core-macro-symbols name)))))
        form-macro-symbols (map second form-macros)
        form2 (remove-form form (is-form? 'defmacro))
        macro-symbols (concat core-macro-symbols form-macro-symbols)]

    ; (create-ns temp-ns)
    ; (binding [*ns* (the-ns temp-ns)]
    ;   (refer 'clojure.core :exclude (concat macro-symbols ['fn 'def]))
    ;   (use '[ferret.core :only [symbol-conversion unique-fn]])

    ;   (doseq [m form-macros]
    ;     (eval m)))

    (doseq [m form-macros]
      (println "eval macro : " (second m))
      (eval m))
    [form2 macro-symbols]))

(defn expand-macros [form2 macro-symbols]
    (let [form3 (-> form2
                   (morph-form (apply is-form? macro-symbols)
                               (fn [f]
                                ;  (binding [*ns* (the-ns temp-ns)]
                                  ;  (println "expand" f)
                                   (macroexpand-1 f))))]
      ; (remove-ns temp-ns) 
      form3))

(defn expand-macros-all-aux [form]
  (let [[form2 macro-symbols] (eval-macros form)]
    (loop [f form2]
      (let [expanded (expand-macros f macro-symbols)]
        (if (= f expanded)
          expanded
          (recur expanded))))))

(def expand-macros-all (memoize expand-macros-all-aux))