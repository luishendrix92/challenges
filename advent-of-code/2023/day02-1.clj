(require '[clojure.string :as s])

(def id-pat #"^Game (\d+):")
(def bag {"red"   12
          "green" 13
          "blue"  14})

(defn parse-set [set']
  (map #(let [[n color] (s/split % #" ")]
          [(Integer/parseInt n) color])
       (s/split set' #", ")))

(defn parse-game [line]
  (let [id (second (re-find id-pat line))
        cube-sets (-> (s/split line #": ")
                      second
                      (s/split #"; "))]
    {:id (Integer/parseInt id)
     :sets (map parse-set cube-sets)}))

; Check that **every** set of cubes is possible, meaning, **every** number
; of cubes shown for each color can be satisfied with the bag's contents.
(defn valid-game? [{sets :sets}]
  (every?
    #(every? (fn [[n color]] (<= n (bag color))) %)
    sets))

(->> (slurp "day02.txt")
     s/split-lines
     (transduce
       (comp (map parse-game)
             (filter valid-game?)
             (map :id))
       + 0)
     println)
