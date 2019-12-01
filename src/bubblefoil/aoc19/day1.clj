(ns bubblefoil.aoc19.day1)

(defn read-ints [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (into [] (map #(Integer/parseInt %) (line-seq rdr)))))

(defn fuel [mass]
  (-> mass
      (/ 3)
      long
      (- 2)))

;Part 1
(defn part1 []
  (reduce + (map fuel (read-ints "src/bubblefoil/aoc19/day1.txt"))))

(comment
  (part1)
  (fuel 14)                                                 ;2
  (fuel 100756))                                            ;33583)

;Part 2
(defn fuel-incl-fuel-mass [mass]
  (reduce +
          (take-while pos?
                      (rest (iterate fuel mass)))))


(defn part2 []
  (reduce + (map fuel-incl-fuel-mass (read-ints "src/bubblefoil/aoc19/day1.txt"))))

(comment
  (part2)
  (fuel-incl-fuel-mass 14)                                  ;2
  (fuel-incl-fuel-mass 1969)                                ;966
  (fuel-incl-fuel-mass 100756))                             ;50346
