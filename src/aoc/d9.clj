(ns aoc.d9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-data [coll]
  (->> coll
       (mapv seq)
       (mapv (partial mapv #(- (int %) (int \0))))))

(def example (parse-data ["2199943210"
                          "3987894921"
                          "9856789892"
                          "8767896789"
                          "9899965678"]))

(def input (parse-data
            (str/split-lines
             (slurp (io/resource "d9.txt")))))

(defn level [grid [x y]]
  (get-in grid [y x]))

(def all-dir [[0 1] [0 -1] [-1 0] [1 0]])

(defn around [grid p]
  (let [max-x (count (first grid))
        max-y (count grid)]
    (->> all-dir
         (map (partial mapv + p))
         (filter (fn [[x y]]
                   (and (< -1 x max-x)
                        (< -1 y max-y)))))))

(defn low-point? [grid p]
  (< (level grid p)
     (->> (around grid p)
          (map (partial level grid))
          (apply min))))

(defn grid-points [grid]
  (for [x (range (count (first grid)))
        y (range (count grid))]
    [x y]))

(defn low-points [grid]
  (filter (partial low-point? grid)
          (grid-points grid)))

(defn risk-level [grid p]
  (inc (level grid p)))

(defn total-risk [grid]
  (->> (low-points grid)
       (map (partial risk-level grid))
       (reduce +)))

(defn grow [grid n basin]
  (->> basin
       (mapcat (partial around grid))
       (remove basin)
       (filter #(<= (level grid %) n))
       (into basin)))

(defn make-basin [grid seed]
  (reduce (fn [basin n]
            (grow grid n basin))
          #{seed}
          (range 9)))

(defn all-basins [grid]
  (map (partial make-basin grid)
       (low-points grid)))

(defn score [basins]
  (->> basins
       (map count)
       sort
       reverse
       (take 3)
       (reduce *)))

(total-risk example)
(total-risk input)

(score (all-basins example))
(score (all-basins input))

