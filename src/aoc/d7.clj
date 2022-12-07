(ns aoc.d7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example [16 1 2 0 4 2 7 1 2 14])

(def input (map #(Integer/parseInt %) (-> (io/resource "d7.txt")
                                          slurp
                                          str/trim
                                          (str/split #","))))

(defn fixed-cost
  [from to]
  (Math/abs (- from to)))

(defn sliding-cost
  [from to]
  (let [n (Math/abs (- from to))]
    (int (* n (inc n) 0.5))))

(defn align [f coll level]
  (reduce + (map f coll (repeat level))))

(defn optimize [f coll]
  (->> (set (range (apply min coll)
                   (apply max coll)))
       (map (fn [level]
              [level (align f coll level)]))
       (sort-by second)))

(first (optimize fixed-cost example))
(first (optimize fixed-cost input))

(first (optimize sliding-cost example))
(println (first (optimize sliding-cost input)))
