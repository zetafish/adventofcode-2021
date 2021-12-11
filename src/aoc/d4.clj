(ns aoc.d4
  "--- Day 4: Giant Squid ---"
  (:require [clojure.string :as str]
            [clojure.zip :as z]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def example ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
              ""
              "22 13 17 11  0"
              "8  2 23  4 24"
              "21  9 14 16  7"
              "6 10  3 18  5"
              "1 12 20 15 19"
              ""
              "3 15  0  2 22"
              "9 18 13 17  5"
              "19  8  7 25 23"
              "20 11 10 24  4"
              "14 21 16 12  6"
              ""
              "14 21 17 24  4"
              "10 16 15  9 19"
              "18  8 23 26 20"
              "22 11 13  6  5"
              "2  0 12  3  7"])

(def input (str/split-lines (slurp (io/resource "aoc/d4.txt"))))

(defn ->int [s]
  (Integer/parseInt s))

(defn parse-line [s] (map ->int (#(str/split % #",|\s+") (str/trim s))))

(defn parse-draws [s] (parse-line s))

(defn parse-board [lines]
  (mapcat parse-line lines))

(defn parse-game [input]
  {:draws (z/seq-zip (parse-line (first input)))
   :boards (map (comp parse-board rest)
                (partition 6 (rest input)))
   :winners []})

(defn mark-number [board n]
  (map (fn [x]
         (if (= x n)
           [:x x]
           x))
       board))

(defn all-slices [coll]
  (loop [slices []
         locs (map (comp z/next z/seq-zip) coll)]
    (if (z/end? (first locs))
      slices
      (recur (conj slices (map z/node locs))
             (map z/next locs)))))

(defn find-bingo [board]
  (let [rows (partition 5 board)
        cols (all-slices rows)]
    (->> (concat rows cols)
         (filter #(every? vector? %))
         (map #(map second %))
         seq)))

(defn draw [{:keys [draws boards winners] :as game}]
  (if (z/end? draws)
    (assoc game :finished true)
    (let [boards (map #(mark-number % (z/node draws)) boards)]
      {:draws (z/next draws)
       :boards boards
       :winners (concat winners
                        (filter some? (map-indexed (fn [i b]
                                                     (when (and (not (contains? (set winners) i))
                                                                (find-bingo b))
                                                       i))
                                                   boards)))})))

(defn render-board [board]
  (str (str/join "\n"
                 (map #(str/join "\t" %)(partition 5 board)))
       "\n---\n"))

(defn show [g]
  (println (:draws g))
  (run! println (map render-board (:boards g))))

(defn sum-unmarked [board]
  (reduce + (remove vector? board)))

(defn first-winner [g] (seq (:winners g)))

(defn last-winner [g] (= (count (:winners g))
                         (count (:boards g))))

(defn score [stop g]
  (let [{:keys [winners draws boards]} (->> g
                                            (iterate draw)
                                            (drop-while (complement stop))
                                            first)
        last-number (-> draws z/left z/node)
        value (-> boards (nth (last winners)) sum-unmarked)]
    [last-number value (* value last-number)]))

;; part 1
(score first-winner (parse-game example))
(score first-winner  (parse-game input))

;; part 2
(score last-winner (parse-game example))
(score last-winner (parse-game input))
