(require '[clojure.string :refer [join split-lines]])

; https://stackoverflow.com/questions/11430863/how-to-find-overlapping-matches-with-a-regexp
; ==========================================================================================
; Regexp consumes matches, which becomes problematic when we want to work with
; overlapping matches such as "twone". To solve this, a "positive lookahead"
; needs to be used so that the match is an empty string (whatever is behind
; (?=PATTERN)) but we capture what we are looking for.
(def digit #"(?=([0-9]|one|two|three|four|five|six|seven|eight|nine))")

(def digit-map {"one"   "1"
                "two"   "2"
                "three" "3"
                "four"  "4"
                "five"  "5"
                "six"   "6"
                "seven" "7"
                "eight" "8"
                "nine"  "9"})

(->> (slurp "day01.txt")
     split-lines
     (map (comp
           #(Integer/parseInt %)
           join
           (partial map #(get digit-map % %))
           (juxt (comp second first)
                 (comp second last))
           (partial re-seq digit)))
     (reduce + 0)
     println)
