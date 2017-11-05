(ns language.reader
  (:require [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            [clojure.zip :as zip]))

(defn simple-symbol [s]
    (symbol (name s)))


(defn read-forms [path]
    (-> path
        slurp
        (#(str "(" % "\n)"))    ; 最外层包一个括号，可以解析为一个list，回车是为了防止文件最后一行有注释!
        read-string))


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
