(ns aoc.d5
  "--- Day 5: Hydrothermal Venture ---"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example ["0,9 -> 5,9"
              "8,0 -> 0,8"
              "9,4 -> 3,4"
              "2,2 -> 2,1"
              "7,0 -> 7,4"
              "6,4 -> 2,0"
              "0,9 -> 2,9"
              "3,4 -> 1,4"
              "0,0 -> 8,8"
              "5,5 -> 8,2"])

(def input (str/split-lines (slurp (io/resource "d5.txt"))))

(defn ->int [s] (Integer/parseInt s))

(defn parse-xy [s]
  (mapv ->int (str/split s #",")))

(defn parse-line [s]
  (map parse-xy (str/split s #" -> ")))

(defn parse [lines]
  (map parse-line lines))

(defn vertical? [[x1 _] [x2 _]] (= x1 x2))
(defn horizontal? [[_ y1] [_ y2]] (= y1 y2))
(defn diagonal? [[x1 y1] [x2 y2]] (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn uni-vals [a b]
  (cond
    (< a b) (range a (inc b))
    (> a b) (range a (dec b) -1)
    :else [a]))

(defn line-points [[[x1 y1 :as a] [x2 y2 :as b]]]
  (cond
    (vertical? a b)   (map #(vector x1 %) (uni-vals y1 y2))
    (horizontal? a b) (map #(vector % y1) (uni-vals x1 x2))))

(defn line-points2 [[[x1 y1 :as a] [x2 y2 :as b]]]
  (cond
    (vertical? a b)   (map #(vector x1 %) (uni-vals y1 y2))
    (horizontal? a b) (map #(vector % y1) (uni-vals x1 x2))
    (diagonal? a b)   (map vector (uni-vals x1 x2) (uni-vals y1 y2))))

(defn overlap [pf lines]
  (->>
   lines
   (reduce (fn [acc line] (concat acc (pf line))) [])
   frequencies
   (filter #(>= (second %) 2))
   count))

;; part 1
(overlap line-points (parse example))
(overlap line-points (parse input))

;; part 2
(overlap line-points2 (parse example))
(overlap line-points2 (parse input))
