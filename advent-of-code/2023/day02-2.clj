(require '[clojure.string :as s])

(defn parse-set [set']
  (let [cubes (map #(let [[n color] (s/split % #" ")]
                      {(keyword color)
                       [(Integer/parseInt n)]})
                   (s/split set' #", "))]
    (apply merge cubes)))

(defn parse-game [line]
  (map parse-set (-> (s/split line #": ")
                     second
                     (s/split #"; "))))

; Merge each set's individual cubes into a map, then merge all the sets
; into a map where each color will have a list of numbers. Extract the
; maximum of those lists and multiply them to get the power.
(defn game-power [sets]
  (reduce #(* %1 (->> %2 second (apply max)))
          1 (apply merge-with into sets)))

(->> (slurp "day02.txt")
     s/split-lines
     (transduce
       (map (comp game-power
                  parse-game))
       + 0)
     println)
