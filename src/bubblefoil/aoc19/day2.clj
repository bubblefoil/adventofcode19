(ns bubblefoil.aoc19.day2
  (:require [bubblefoil.aoc19.input :as in]
            [clojure.pprint :refer [pprint]]))

(defn binary-fn [fn {:keys [cursor program] :as state}]
  (let [a1 (aget program (+ cursor 1))
        a2 (aget program (+ cursor 2))
        output-position (aget program (+ cursor 3))]
    (aset-int program output-position (fn (aget program a1) (aget program a2)))
    (update state :cursor + 4)))

(defn run-program! [{:keys [cursor program] :as state}]
  (let [instruction (aget program cursor)]
    (condp = instruction
      1 (run-program! (binary-fn + state))
      2 (run-program! (binary-fn * state))
      99 (aget program 0))))

(defn read-program []
  (int-array (in/line->ints (slurp "src/bubblefoil/aoc19/day2.txt"))))

(defn restore-program
  ([noun verb prog]
   (aset-int prog 1 noun)
   (aset-int prog 2 verb)
   prog)
  ([prog]
   (restore-program 12 2 prog)))

(defn init-and-run-program!
  [noun verb]
  (trampoline run-program! {:cursor  0
                            :program (restore-program noun verb (read-program))}))

;Part I
(init-and-run-program! 12 2)

(defn find-inputs-for-result [result]
  (first (drop-while #(not= result (apply init-and-run-program! %))
                     (for [noun (range 100)
                           verb (range 100)]
                       [noun verb]))))

(defn part-two-answer [noun verb]
  (+ (* 100 noun) verb))

;Part II
(apply part-two-answer (find-inputs-for-result 19690720))

(comment
  (def in "1,9,10,3,2,3,11,0,99,30,40,50")
  (def prog (in/line->ints in))
  (def a (int-array prog))
  (restore-program a)
  (pprint a)
  (def state {:cursor  0
              :program (int-array prog)})

  (pprint (binary-fn + state))

  (pprint (trampoline run-program! state)))