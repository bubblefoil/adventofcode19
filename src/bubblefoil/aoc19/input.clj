(ns bubblefoil.aoc19.input)

(defn str->int [s] (Integer/parseInt s))

(defn split-by-comma [s]
  (clojure.string/split s #","))

(defn line->ints [line]
  (map str->int
       (split-by-comma line)))