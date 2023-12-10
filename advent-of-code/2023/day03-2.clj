(require '[clojure.string :refer [split-lines split]])

(def lines (-> "day03.txt" slurp split-lines))
(def matrix (mapv #(split % #"") lines))

(defn make-num [row m]
  (let [x1 (dec (. m start)), y1 (dec row)
        x2 (. m end)        , y2 (inc row)]
    {:num (Integer/parseInt (. m group))
     :gear (first
            (for [y (range y1 (inc y2))
                  x (range x1 (inc x2))
                  :let [c (get-in matrix [y x])]
                  :when (= c "*")]
              [y x]))}))

(def mapcat-indexed #(mapcat %1 %2 (range)))

(def numbers
  (mapcat-indexed
   (fn [line row]
     (loop [matches []
            m (re-matcher #"\d+" line)]
       (if (. m find)
         (recur (conj matches (make-num row m)) m)
         matches)))
   lines))

(defn valid-gear? [[_loc nums]] (= (count nums) 2))
(defn gear-ratio [[_loc [a b]]] (* (:num a) (:num b)))

(->> (filter :gear numbers)
     (group-by :gear)
     (transduce
      (comp (filter valid-gear?)
            (map gear-ratio))
      + 0)
     println)
