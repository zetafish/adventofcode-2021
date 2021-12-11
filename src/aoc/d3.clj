(ns aoc.d3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example ["00100"
              "11110"
              "10110"
              "10111"
              "10101"
              "01111"
              "00111"
              "11100"
              "10000"
              "11001"
              "00010"
              "01010"])

(def input (str/split-lines (slurp (io/resource "aoc/d3.txt"))))

(defn bits [s] (map {\0 0 \1 1} s))

(defn slice [coll n]
  (map #(nth % n) coll))

(defn all-slices [report]
  (map #(slice report %) (range (count (first report)))))

(defn ->decimal [bits]
  (Integer/parseInt (str/join bits) 2))

(defn gamma [report]
  (->decimal
   (map (comp first last (partial sort-by second) frequencies)
        (all-slices report))))

(defn epsilon [report]
  (->decimal
   (map (comp first first (partial sort-by second) frequencies)
        (all-slices report))))

;; part 1
(* (gamma example) (epsilon example))
(* (gamma input) (epsilon input))

(defn discard-numbers [report crit n]
  (let [f (sort-by second (frequencies (slice report n)))
        b (cond
            (apply = (vals f)) (case crit :most-common \1 :least-common \0)
            (= :most-common crit) (first (last f))
            (= :least-common crit) (first (first f)))]
    (filter #(= b (nth % n)) report)))

(defn rating [crit report]
  (->decimal
   (reduce (fn [report n]
             (if (= 1 (count report))
               report
               (discard-numbers report crit n)))
           report
           (range (count (first report))))))

(def oxygen-generator-rating (partial #'rating :most-common))
(def co2-scrubbing-rating (partial #'rating :least-common))

;; part 2
(* (oxygen-generator-rating example)
   (co2-scrubbing-rating example))

(* (oxygen-generator-rating input)
   (co2-scrubbing-rating input))
