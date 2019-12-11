(ns bubblefoil.aoc19.day3
  (:require [bubblefoil.aoc19.input :refer [split-by-comma]]
            [bubblefoil.aoc19.input :as in]
            [clojure.java.io :as io]))

(def ex1 "R8,U5,L5,D3\nU7,R6,D4,L4")
(def ex2 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")

(def paths (map split-by-comma))

(defn parse-wire-paths [readable]
  (with-open [r (io/reader readable)]
    (into [] paths (line-seq r))))

(defn direction [^String s] (.charAt s 0))
(defn distance [s] (in/str->int (subs s 1)))
(defn parse-turn [t] [(direction t) (distance t)])

(defn next-line [[x y steps] turn-str]
  (let [[direction distance] (parse-turn turn-str)
        new-steps (+ steps distance)]
    (case direction
      \U [x (+ y distance) new-steps]
      \D [x (- y distance) new-steps]
      \L [(- x distance) y new-steps]
      \R [(+ x distance) y new-steps])))

(defn add-line [lines turn-str]
  (conj lines
        (next-line (last lines) turn-str)))

(defn parse-path [turns]
  (reduce add-line [[0 0 0]] turns))

(defn as-lines [path]
  (partition 2 1 path))

(defn lines-to-wire-paths [lines]
  (map (comp as-lines parse-path) lines))

(defn cross-point
  "Returns [x y s1 s2] where x,y are coordinates of cross point
   and s1,2 are steps at which line1 is crossed by line2
  and line2 is crossed by line1, respectively."
  [[[ax1 ay1 step-a1] [bx1 by1 step-b1]]
   [[ax2 ay2 step-a2] [bx2 by2 step-b2]]]
  (cond (and (= ax2 bx2)
             (= ay1 by1)
             (or (<= ax1 ax2 bx1) (<= bx1 ax2 ax1))
             (or (<= ay2 ay1 by2) (<= by2 ay1 ay2)))
        [ax2 ay1
         (+ step-a1 (Math/abs (int (- ax1 ax2))))
         (+ step-a2 (Math/abs (int (- ay1 ay2))))]

        (and (= ax1 bx1)
             (= ay2 by2)
             (or (<= ax2 ax1 bx2) (<= bx2 ax1 ax2))
             (or (<= ay1 ay2 by1) (<= by1 ay2 ay1)))
        [ax1 ay2
         (+ step-a1 (Math/abs (int (- ay1 ay2))))
         (+ step-a2 (Math/abs (int (- ax1 ax2))))]

        :else nil))

(defn cross-points [[wire1 wire2]]
  (for [w1 wire1
        w2 wire2]
    (cross-point w1 w2)))

(defn m-distance [[x y]]
  (+ (Math/abs (int x)) (Math/abs (int y))))

(defn nearest-cross-point [key-fn wires]
  (apply min-key key-fn
         (remove nil? (rest (cross-points (lines-to-wire-paths wires))))))

(defn step-distance [[_ _ s1 s2]] (+ s1 s2))

;Part 1
(m-distance (nearest-cross-point m-distance (parse-wire-paths "src\\bubblefoil\\aoc19\\day3.txt")))
;Part 2
(step-distance (nearest-cross-point step-distance (parse-wire-paths "src\\bubblefoil\\aoc19\\day3.txt")))

(comment
  (parse-turn "U5")
  (next-line [1 1 0] "U5")
  (add-line [[0 0 0]] "U1")

  (cross-point [[0 0] [75 0]]
               [[50 -20] [50 20]])
  (cross-point [[0 0] [75 0]]
               [[0 0] [75 0]])
  (cross-point [[3 5] [3 2]]
               [[6 3] [2 3]])

  (cross-point [[3 5 0] [3 2 3]]
               [[6 3 0] [2 3 4]])

  (cross-point [[0 3 20] [6 3 26]]
               [[3 7 10] [3 0 17]])                         ;[3 3 23 14]

  (m-distance [1 -2])
  (def test-wire-paths (parse-wire-paths (.toCharArray ex1)))
  (lines-to-wire-paths test-wire-paths)
  (nearest-cross-point test-wire-paths))                    ;[3 3])
