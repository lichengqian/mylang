(in-ns 'language.golang)

(defmethod emit-type-builtin ::golang
  [t]
  ; (println t)
  (if (nil? t)
    ""
    (case (str t)
      "Void" ""
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
    "Map" (str "map[" (emit-type (first args)) "]*" (emit-type (second args)))
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
        
