(ns bubblefoil.aoc19.input
  (:require [clojure.java.io :as io]))

(defn str->int [s] (Integer/parseInt s))

(defn split-by-comma [s]
  (clojure.string/split s #","))

(defn line->ints [line]
  (map str->int
       (split-by-comma line)))

(defn map-chars
  "Returns a seq of results of applying xf
   to each character of every line, followed by its line and column number."
  [xf reader]
  (->> reader
       line-seq
       (map-indexed
         (fn [line-idx line]
           (map-indexed
             (fn [char-idx c]
               [c line-idx char-idx])
             line)))
       (apply concat)
       (into [] xf)))
