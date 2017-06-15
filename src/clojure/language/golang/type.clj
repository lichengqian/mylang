(in-ns 'language.golang)

(defmethod emit-type-builtin ::golang
  [t]
  ; (println t)
  (if (nil? t)
    ""
    (case (str t)
      "Void" "struct{}"
      "Bool" "bool"
      "UInt32" "uint32"
      "UInt64" "uint64"
      "String" "string"
      "ByteString" "[]byte"
      "Error" "error"
      "Chan" "chan"
      "Lock"  (do (add-import "sync") "sync.Mutex")

      "Listener" (do (add-import "net") "net.Listener")
      "Conn" (do (add-import "net") "net.Conn")
      (emit t))))

(defmethod emit-type-constructor ::golang
  [c args]
  (case (str c)
    "Either" (str "(" (emit-type (second args)) ", " (emit-type (first args)) ")")
    "Map" (str "map[" (emit-type (first args)) "]" (emit-type (second args)))
    "Set" (str "map[" (emit-type (first args)) "]struct{}")
    "IO" (str "(" (emit-type (first args)) ", error)")
    "MVar" 
    (do
        (add-import "sync")
        (string/join "\n"
            ["struct {"
             (str "  value " (emit-type (first args)))
             "  sync.Mutex"
             "}"]))

    (->> args
        (map emit-type)
        (string/join " ")
        (str (emit-type c) " "))))
        
(defn- maptype? 
  [type-exp]
  (and 
    (list? type-exp)
    (= 'Map (first type-exp))))

(defn- settype? 
  [type-exp]
  (and 
    (list? type-exp)
    (= 'Set (first type-exp))))

(defn- mapmaptype?
  [type-exp]
  (if (maptype? type-exp)
    (maptype? (last type-exp))
    false))

(defn- mapsettype?
  [type-exp]
  (if (maptype? type-exp)
    (settype? (last type-exp))
    false))

(defn- emit-mapmaptype
  [alias [_ k1 [_ k2 v :as t2] :as t]]
  (println alias k1 k2 v)
  (let [str-t (emit-type t)
        str-decl (str "type " (emit alias) " " str-t)
        str-new
          (str "func new" (emit alias) "() " (emit alias)
            (braceln (str "return make" (paren str-t))))
        str-get
          (str "func "
            (paren (str "mm " (emit alias)))
            "get_in "
            (paren (str "from " (emit-type k1) ", to " (emit-type k2))) (emit-type v)
            "{
                if m, ok := mm[from]; ok {
                  return m[to]
                }
                return nil
              }")
        str-assoc
          (str "func "
            (paren (str "mm " (emit alias)))
            "assoc_in "
            (paren (str "from " (emit-type k1) ", to " (emit-type k2) ", v " (emit-type v)))
            (braceln (str "	m, ok := mm[from]
                if !ok {
                  m = make" (paren (emit-type t2)) "
                  mm[from] = m
                }
                m[to] = v
              ")))]
            
      (str  str-decl "\n\n"
            str-new "\n"
            str-get "\n"
            str-assoc "\n")))
  
(defn- emit-mapsettype
  [alias [_ k1 [_ k2 :as t2] :as t]]
  (println alias k1 k2)
  (let [str-t (emit-type t)
        str-decl (str "type " (emit alias) " " str-t)
        str-new
          (str "func new" (emit alias) "() " (emit alias)
            (braceln (str "return make" (paren str-t))))
        str-contains
          (str "func "
            (paren (str "mm " (emit alias)))
            "contains "
            (paren (str "from " (emit-type k1) ", to " (emit-type k2))) " bool "
            "{
                if m, ok := mm[from]; ok {
                  _, ok = m[to]
                  return ok
                }
                return false
              }")
        str-assoc
          (str "func "
            (paren (str "mm " (emit alias)))
            "assoc_in "
            (paren (str "from " (emit-type k1) ", to " (emit-type k2)))
            (braceln (str "	m, ok := mm[from]
                if !ok {
                  m = make" (paren (emit-type t2)) "
                  mm[from] = m
                }
                m[to] = struct{}{}
              ")))]
            
      (str  str-decl "\n\n"
            str-new "\n"
            str-contains "\n"
            str-assoc "\n")))
  

(defmethod emit-special [::golang 'type]
  [_ [_ alias real-type]]
  (cond 
    (mapmaptype? real-type)
    (emit-mapmaptype alias real-type)

    (mapsettype? real-type)
    (emit-mapsettype alias real-type)

    :else
    (str "type " (emit alias) " " (emit-type real-type) "\n\n")))

;;; mapmap op
(defmethod go-call 'get-in
    [_ m ks]
    (->> ks
        (map emit)
        (string/join ", ")
        paren
        (str (emit m) ".get_in")))

(defmethod go-call 'assoc-in
    [_ m ks v]
    (->> (conj ks v)
        (map emit)
        (string/join ", ")
        paren
        (str (emit m) ".assoc_in")))
