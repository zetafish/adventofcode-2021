(ns aoc.d1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example [199
              200
              208
              210
              200
              207
              240
              269
              260
              263])

(def input (->> (slurp (io/resource "aoc/data1.txt"))
                (str/split-lines)
                (map #(Integer/parseInt %))))

(defn inc-count [coll]
  (count (filter pos? (map - (drop 1 coll) coll))))

(defn window [coll w]
  (map + coll (drop 1 coll) (drop 2 coll)))


;; part 1
(inc-count example)
(inc-count input)

;; part 2
(inc-count (window example 3))
(inc-count (window input 3))
