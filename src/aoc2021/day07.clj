(ns aoc2021.day07
  (:require
    [clojure.string :as str]
    [aoc2021.util :as util]))

;;; Day 7: The Treachery of Whales

(defn parse-input
  "Returns input as seq of longs."
  []
  (->> (util/get-input *ns*)
       (re-seq #"\d+")
       (map parse-long)))

(defn calc-costs
  "Calculates the costs of the crabs to move to position p using 
  the given costs-function (fn[p crab] ..)."
  [cost-fn crabs p]
  (reduce (fn[acc crab] (+ acc (cost-fn p crab))) 0 crabs))

;; part 1

(defn linear-costs
  "Simple costs: linear with distance."
  [p crab]
  (abs (- p crab)))

(defn part-1
  []
  (let [crabs (parse-input)]
    (->> (range (inc (apply max crabs)))
         (map #(calc-costs linear-costs crabs %))
         (apply min))))

;; part 2

(defn quadratic-costs
  "Rising costs: quadratic with distance."
  [p crab]
  (let [d (abs (- p crab))]
    ; gauss: sum(1..n) = n * (n + 1) / 2
    (* 1/2 d (inc d))))

(defn part-2
  []
  (let [crabs (parse-input)]
    (->> (range (inc (apply max crabs)))
         (map #(calc-costs quadratic-costs crabs %))
         (apply min))))

(comment
  (set! *warn-on-reflection* true)
  )
