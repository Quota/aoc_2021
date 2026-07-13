(ns aoc2021.day06
  (:require
    [clojure.string :as str]
    [aoc2021.util :as util]))

;;; Day 6: Lanternfish

(defn parse-input
  "Returns input as a map with the counts of lanternfish per
  days-till-reproduction.
  Example: input 23,40,23,17 yields {17 1, 23 2, 40 1}."
  []
  (->> (util/get-input *ns*)
       (re-seq #"\d+")
       (map parse-long)
       (reduce (fn[lfish-counts n] (update lfish-counts n (fnil inc 0))) {})))

(defn next-day
  "Move the population of lanternfish to the next day."
  [lfish-counts]
  (let [days0 (lfish-counts 0 0)]
    (-> lfish-counts
        (update-keys dec)
        (dissoc -1)
        (update 6 (fnil + 0) days0)
        (assoc 8 days0))))

(defn population-after-days
  "Passes the given number of days and returns the total number of resulting
  lanternfish. "
  [lfish-counts days]
  (->> lfish-counts
       (iterate next-day)
       (take (inc days))
       last
       vals
       (reduce +)))

;; part 1

(defn part-1
  []
  (population-after-days (parse-input) 80))

;; part 2

(defn part-2
  []
  (population-after-days (parse-input) 256))

(comment
  (set! *warn-on-reflection* true)
  )
