(defn unique-char? [s]
  (fn [c] (= (.indexOf s (str c))
             (.lastIndexOf s (str c)))))

(defn valid-name? [filename]
  (let [[s chksum] (clojure.string/split filename #"-")
        non-repeated (filter (unique-char? s) s)]
    (= (clojure.string/join non-repeated) chksum)))

(def real-files (->> "message_04.txt"
                     slurp
                     clojure.string/split-lines
                     (filter valid-name?)))

(println (nth real-files 32))
