(ns bubblefoil.aoc19.day4
  (:require [bubblefoil.aoc19.input :as in]))

(def input-d4 "172851-675869")

(defn digits-to-ints [num]
  (map int (str num)))

(defn non-decreasing? [digits]
  (apply <= digits))

(defn same-adjacent? [digits]
  (some (partial apply =)
        (partition 2 1 digits)))

(defn num-range [input]
  (map in/str->int (clojure.string/split input #"-")))

(defn password-range [input]
  (apply range (num-range input)))

(def meets-criteria1? (every-pred non-decreasing? same-adjacent?))

;Part 1
(count (filter (comp meets-criteria1? digits-to-ints) (password-range input-d4)))

(defn some-double-digit? [digits]
  (some #(= 2 %)
        (map count (partition-by identity digits))))

(def meets-criteria2? (every-pred non-decreasing? some-double-digit?))

;Part 2
(count (filter meets-criteria2? (map digits-to-ints (password-range input-d4))))

(comment
  (meets-criteria1? "675869")
  (meets-criteria1? "115569")
  (= 48 (int \0)))


