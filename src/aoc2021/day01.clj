(ns aoc2021.day01
  (:require
    [clojure.string :as str]
    [aoc2021.util :as util]))

;;; day template

(defn parse-input
  "Returns input as seq of numbers."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map parse-long)))

;; part 1

(defn count-increase
  "Counts how often two successive numbers increase."
  [depths]
  (->> depths
       (partition 2 1)
       (filter #(apply < %))
       count))

(defn part-1
  []
  (->> (parse-input)
       (count-increase)))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       (partition 3 1)
       (map #(apply + %))
       (count-increase)))

(comment
  (set! *warn-on-reflection* true)
)
