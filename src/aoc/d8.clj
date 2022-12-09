(ns aoc.d8
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.string :as str]))

(def example "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(def input (str/split-lines (slurp (io/resource "d8.txt"))))

(def digits {0 "abcefg"
             1 "cf"
             2 "acdeg"
             3 "acdfg"
             4 "bcdf"
             5 "abdfg"
             6 "abdefg"
             7 "acf"
             8 "abcdefg"
             9 "abcdfg"})

(def segments->digit (into {} (map (fn [[d wires]]
                                     [(set wires) d])
                                   digits)))

(defn parse-signals [line]
  (str/split (first (str/split line #" \| ")) #" "))

(defn parse-outputs [line]
  (str/split (second (str/split line #" \| ")) #" "))

(defn wires-with-count [n signals]
  (str/join (map first (filter #(= n (second %)) (frequencies (str/join signals))))))

(defn options [[a b] [x y]]
  (if-not b
    [[(str a x)]]
    [[(str a x) (str b y)]
     [(str a y) (str b x)]]))

(defn gen-wirings
  [signals]
  (->> {(wires-with-count 4 signals) "e"
        (wires-with-count 6 signals) "b"
        (wires-with-count 9 signals) "f"
        (wires-with-count 8 signals) "ac"
        (wires-with-count 7 signals) "dg"}
       (map (fn [[x y]] (options x y)))
       (apply cartesian-product)
       (map (comp
             (partial into {})
             (partial map vec)
             (partial apply concat)))))

(defn beta-reduce
  [m signal]
  (str/join (map #(get m % \.) signal)))

(defn valid? [m signals]
  (let [r (zipmap signals
                  (map (partial beta-reduce m) signals))
        f (fn [n]
            (first (filter #(= n (count %)) (vals r))))
        cmp [[(set (f 2)) (set (digits 1))]
             [(set (f 3)) (set (digits 7))]
             [(set (f 4)) (set (digits 4))]
             [(set (f 7)) (set (digits 8))]]]
    (every? (fn [[a b]] (= a b)) cmp)))

(defn decode [s]
  (let [signals (parse-signals s)
        outputs (parse-outputs s)
        wirings (gen-wirings signals)
        assignment (first (filter #(valid? % signals) wirings))]
    (map (comp
          segments->digit
          set
          (partial beta-reduce assignment))
         outputs)))

(defn ->number [digits]
  (Integer/parseInt (str/join digits)))

(decode example)
(decode "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe")
(decode "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc")

(println (reduce + (vals (select-keys (frequencies (mapcat decode input)) [1 4 7 8]))))

(println (reduce + (map (comp ->number decode) input)))
