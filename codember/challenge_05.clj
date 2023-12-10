(require '[clojure.string :as str])

(def alphanum #"[A-Za-z0-9]+")
(def mail #"\w+\@[a-z]+\.[a-z]{2,}")
(def numeric #"\d+")

(defn valid-user? [[id username email age _location]]
  (not-any? nil? (map re-matches
                      [alphanum alphanum mail numeric] 
                      [id username email age])))

(defn parse-csv [csv-str]
  (let [lines (str/split-lines csv-str)]
    (map #(str/split % #"," 5) lines)))

(println (->> (slurp "message_05.txt")
              parse-csv
              (remove valid-user?)
              (map (comp first second))
              str/join))
