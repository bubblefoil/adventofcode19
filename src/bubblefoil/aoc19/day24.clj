(ns bubblefoil.aoc19.day24
  (:require [clojure.java.io :as io]
            [bubblefoil.aoc19.input :as in]))

(defn tile-coords [[_ line col]]
  [col line])

(defn bug? [[ch]]
  (= ch \#))

(def map-reader (comp (filter bug?)
                      (map tile-coords)))

(defn read-map []
  (with-open [r (io/reader (io/resource "bubblefoil/aoc19/day24.txt"))]
    (set (in/map-chars map-reader r))))

(def world (for [x (range 0 5)
                 y (range 0 5)]
             [x y]))

(defn adjacent-tiles [[x y]]
  [[(inc x) y]
   [x (inc y)]
   [(dec x) y]
   [x (dec y)]])

(defn count-neighbors [bugs tile]
  (->> (adjacent-tiles tile)
       (filter (set bugs))
       count))

(defn dies? [bugs infested-tile]
  (if (= 1 (count-neighbors bugs infested-tile))
    infested-tile
    nil))

(defn becomes-infested? [bugs empty-tile]
  (if (#{1 2} (count-neighbors bugs empty-tile))
    empty-tile
    nil))

(defn infested? [bugs tile]
  (if (contains? bugs tile)
    (dies? bugs tile)
    (becomes-infested? bugs tile)))

(defn next-minute [bugs]
  (into #{} (keep #(infested? bugs %) world)))

(defn find-repeated-layout [bugs]
  (loop [layout bugs
         layouts #{}]
    (let [next-layout (next-minute layout)]
      (if (layouts next-layout)
        next-layout
        (recur next-layout (conj layouts next-layout))))))

(defn biodiversity-points [[x y]]
  (bit-shift-left 1 (+ x (* 5 y))))

(defn biodiversity-rating [bugs]
  (transduce (map biodiversity-points) + bugs))

(defn part1 []
  (biodiversity-rating (find-repeated-layout (read-map))))

(part1)

(comment
  (def test-map (set (in/map-chars map-reader (io/reader (.toCharArray "....#\n#..#.\n#..##\n..#..\n#....")))))

  (bug? [\# 4 0])

  (in/map-chars (map tile-coords) (io/reader (.toCharArray "....#\n#..#.\n#..##\n..#..\n#....")))
  (in/map-chars map-reader (io/reader (.toCharArray "....#\n#..#.\n#..##\n..#..\n#....")))

  (infested? test-map [0 1])

  (next-minute (read-map))

  (biodiversity-points [0 3])
  (biodiversity-rating (find-repeated-layout test-map)))

