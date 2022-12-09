(ns aoc.d10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines
            (slurp
             (io/resource "d10.txt"))))

(def error-score {\) 3
                  \] 57
                  \} 1197
                  \> 25137})

(def autocomplete-score {\) 1
                         \] 2
                         \} 3
                         \> 4})

(defn step [state c]
  (if (:error state)
    state
    (case c
      \( (update state :stack conj \))
      \[ (update state :stack conj \])
      \{ (update state :stack conj \})
      \< (update state :stack conj \>)

      (if (= (first (:stack state)) c)
        (update state :stack rest)
        (assoc state :error  c)))))

(defn validate [s]
  (reduce step nil s))

(validate "{([(<{}[<>[]}>{[]{[(<()>")
(validate "<{([([[(<>()){}]>(<<{{")

(reduce + (map error-score (keep :error (map validate input))))

(defn score-stack [stack]
  (reduce (fn [n c]
            (+ (autocomplete-score c) (* 5 n)))
          0 stack))

(println (let [scores (->> (map validate input)
                           (remove :error)
                           (map (comp score-stack :stack))
                           sort)]
           (nth scores (/ (dec (count scores)) 2))))

