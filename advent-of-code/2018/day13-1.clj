(def initial-map (slurp "2018-day13-input.txt"))

(def valid-carts #{\^ \> \v \<})
; R2 Vectors [ DeltaY , DeltaX ]
(def dir-vectors {\^ [-1  0]
                  \> [ 0  1]
                  \v [ 1  0]
                  \< [ 0 -1]})

(defn at-coord [y x matrix]
  (nth (nth matrix y) x))

(defn make-cart [y x ch]
  (when (valid-carts ch)
    {:dir ch :y y :x x
     :intersections 0}))

(defn parse-carts [lines]
  (flatten (map-indexed
             (fn [y line] 
               (keep-indexed 
                 #(make-cart y %1 %2)
                 line))
             lines)))

(def carts (->> initial-map
                clojure.string/split-lines
                parse-carts
                println))
