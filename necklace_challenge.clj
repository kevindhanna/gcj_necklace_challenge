(use '[clojure.string :only (join split-lines)])

(defn rotate [string]
  (let [[start & end] string]
    (join "" [(join "" end) start])))

(defn same_necklace
  ([a b]
   (if (= a b)
     true
     (if (= (count a) (count b))
       (same_necklace a b (count a))
       false)))
  ([a b i]
   (if (and (not= i 0) )
     (if (= a b)
       true
       (same_necklace (rotate a) b (- i 1)))
     false)))

(defn repeats [a]
  (if (< (count a) 2)
    1
    (loop [r a
           i (count a)
           count 0]
      (if (> i 0)
        (if (= r a)
          (recur (rotate r) (dec i) (inc count))
          (recur (rotate r) (dec i) count))
        count))))

(defn getMatches [word words]
  (loop [word word
         words words
         matches '()]
    (if (= (count words) 0)
      matches
      (if (same_necklace word (first words))
        (recur word (rest words) (conj matches (first words)))
        (recur word (rest words) matches)))))

(defn main [words]
  (loop [word (first words)
         words (rest words)]
    (let [matches (getMatches word words)]
      (if (= (count matches) 4)
        matches
        (recur (first words) (rest words))))))

(print (main (split-lines (slurp "words.txt"))))
