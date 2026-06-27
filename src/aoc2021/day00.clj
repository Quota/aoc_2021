(ns aoc2021.day00
  (:require
    [clojure.string :as str]
    [aoc2021.util :as util]))

;;; day template

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)))

;; part 1

(defn part-1
  []
  (->> (parse-input)
       count))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))

(comment
  (set! *warn-on-reflection* true)
)
