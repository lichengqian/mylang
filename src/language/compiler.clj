(ns language.compiler
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.analyzer.ast :as ast]))

(defrecord CodeBlock [format-str args])

(defmethod print-method CodeBlock
  [cb w]
  (print-method 
    (apply cl-format nil (:format-str cb) (:args cb))
    w))


(defn code-block
  [format-str & args]
  (-> (CodeBlock. format-str (vec args))
    (assoc :children [:args])))

(defn code-block-with-keys
  [format-str & kvs]
  (let [args (mapv second (partition 2 kvs))
        kvs (apply assoc {} kvs)]
    (map->CodeBlock (merge {:format-str format-str
                            :args args
                            :children (into [] (keys kvs))}
                           kvs))))

(defrecord Type [name package])

(defmethod print-method Type
  [t w]
  (print-method (str (:name t)) w))




(defn collect-tags
  [key ast]
  (try
   (->> (ast/nodes ast)
     (map key)
     (filter some?)
     (into #{}))

   (catch Exception e
     #{e})))


(defmacro native-def
  [name init]
  `(def ~(with-meta name {:native true})  ~init))


;;;; 定义语言特殊的语法结构，和代码生成策略
(defmacro native-defn
  [name params & body]
  `(defn ~name {:native true} ~params ~@body))


;;;; 判断一个ast是否是一个native
(defn native?
  [{:keys [op var]}]
  (and (= op :var)
       (= true (:native (meta var)))))


;;;; 语言内置数据类型定义
(defmacro native-types
  [& names]
  `(do
     ~@(for [name names]
          `(native-def ~name ~(str name)))))


;;;; 查找一个ns下所有的native 声明
(defn find-all-natives
  [ns]
  (->> ns
    ns-publics
    (filter (fn [[k v]] (= true (:native (meta v)))))
    (into {})))


;;;; 构造native-map
(defn build-native-map
  [vars]
  (->> vars vals
    (mapcat (fn [var]
              (if-let [from (:from (meta var))]
                [[from var]]
                [])))
    (into {})))
