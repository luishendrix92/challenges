(->> (slurp "day01.txt")
     clojure.string/split-lines
     (map (comp
            #(Integer/parseInt %)
            clojure.string/join
            (juxt first last)
            (partial re-seq #"\d")))
     (reduce + 0)
     println)
