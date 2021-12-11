(ns aoc.d3
  "--- Day 3: Binary Diagnostic ---"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.zip :as z]))

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

(defn all-slices [report]
  (loop [slices []
         locs (map (comp z/next z/seq-zip seq) report)]
    (if (z/end? (first locs))
      slices
      (recur (conj slices (map z/node locs))
             (map z/next locs)))))

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

(defn rating [crit report]
  (loop [locs (map (comp z/next z/seq-zip seq) report)]
    (if (= 1 (count locs))
      (->decimal (str/join (z/root (first locs))))
      (recur (let [f (sort-by second (frequencies (map z/node locs)))
                   b (cond
                       (apply = (vals f)) (case crit :most-common \1 :least-common \0)
                       (= :most-common crit) (first (last f))
                       (= :least-common crit) (first (first f)))]
               (map z/next (filter #(= b (z/node %)) locs)))))))

(def oxygen-generator-rating (partial #'rating :most-common))
(def co2-scrubbing-rating (partial #'rating :least-common))

;; part 2
(* (oxygen-generator-rating example)
   (co2-scrubbing-rating example))

(* (oxygen-generator-rating input)
   (co2-scrubbing-rating input))
