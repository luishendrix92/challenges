(require '[clojure.string :refer [split-lines split]])

(def mapcat-indexed #(mapcat %1 %2 (range)))

(def lines (-> "day03.txt" slurp split-lines))
(def matrix (mapv #(split % #"") lines))

(defn make-num [row m]
  (let [x1 (dec (. m start)), y1 (dec row)
        x2 (. m end)        , y2 (inc row)]
    {:num (Integer/parseInt (. m group))
     :symbols (for [y (range y1 (inc y2))
                    x (range x1 (inc x2))
                    :let [c (get-in matrix [y x])]
                    :when (and (not (nil? c))
                               (re-matches #"[^0-9.]" c))]
                c)}))

(def numbers
  (mapcat-indexed
   (fn [line row]
     (loop [matches []
            m (re-matcher #"\d+" line)]
       (if (. m find)
         (recur (conj matches (make-num row m)) m)
         matches)))
   lines))

(->> numbers
     (transduce
      (comp (filter (comp not-empty :symbols))
            (map :num)) + 0)
     println)
