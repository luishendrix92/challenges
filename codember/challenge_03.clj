(def filename "message_03.txt")
(def pattern #"^(\d+)-(\d+) ([a-z]): ([a-z]+)$")

(defn parse-line [[_ min max key password]]
  {:min (Integer/parseInt min)
   :max (Integer/parseInt max)
   :key key
   :password password})

(defn is-valid [{:keys [min max key password]}]
  (let [key-count (->> password (filter (set key)) count)]
    (<= min key-count max)))

(def invalid-passwords 
  (->> filename
       slurp
       clojure.string/split-lines
       (map (comp parse-line
                  #(re-matches pattern %)))
       (remove is-valid)))

; Prints the solution to the 3rd challenge
(-> invalid-passwords (nth 41) :password println)

; Prints the sudo password for the 'private' folder
(-> invalid-passwords (nth 12) :password println)
