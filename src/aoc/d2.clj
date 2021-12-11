(ns aoc.d2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; fwd (+ x 2)
;; down (+ d 7)
;; up

(def example ["forward 5"
              "down 5"
              "forward 8"
              "up 3"
              "down 8"
              "forward 2"])

(def input (str/split-lines (slurp (io/resource "aoc/d2.txt"))))

(defn parse-line [s]
  (let [[cmd n] (str/split s #" ")]
    [(keyword cmd) (Integer/parseInt n)]))

(defn step1
  [[hor depth] [cmd n]]
  (case cmd
    :forward [(+ hor n) depth]
    :up [hor (- depth n) ]
    :down [hor (+ depth n)]))

(defn step2
  [[hor depth aim] [cmd n]]
  (case cmd
    :forward [(+ hor n) (+ depth (* aim n)) aim]
    :up [hor depth (- aim n)]
    :down [hor depth (+ aim n)]))

(run! println (reductions step2 [0 0 0] (map parse-line example)))

(reduce * (reduce step1 [0 0] (map parse-line example)))
(reduce * (reduce step1 [0 0] (map parse-line input)))


(println (reduce * (take 2 (reduce step2 [0 0 0] (map parse-line example)))))
(println (reduce * (take 2 (reduce step2 [0 0 0] (map parse-line input)))))
