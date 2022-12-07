(ns aoc.d6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example [3 4 3 1 2])

(def input (map #(Integer/parseInt %) (-> (io/resource "d6.txt")
                                          slurp
                                          str/trim
                                          (str/split #","))))

(declare count-fish*)

(def count-fish (memoize count-fish*))

(defn count-fish* [timer after-days]
  (cond
    (zero? after-days) 1N
    (zero? timer) (long (+' (count-fish 6 (dec after-days))
                            (count-fish 8 (dec after-days))))
    :else (count-fish (dec timer) (dec after-days))))

(reduce + (map #(count-fish % 80) example))
(reduce + (map #(count-fish % 256) example))
(println (reduce + (map #(count-fish % 256) input)))
