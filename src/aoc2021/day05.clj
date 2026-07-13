(ns aoc2021.day05
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [aoc2021.util :as util]))

;;; Day 5: Hydrothermal Venture

(defn parse-input
  "Returns input as seq of vecs of longs. In other words, returns
  [[x1, y1, x2, y2], [...], ...]."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map #(->> %
                  (re-seq #"\d+")
                  (mapv parse-long)))))

;; part 1

(defn horiz-or-vert?
  "Returns if the given line is either horizontal or vertical."
  [[x1 y1 x2 y2]]
  (or (= x1 x2)
      (= y1 y2)))

(defn get-points-on-line
  "Returns a set with all points on the given line, assuming it is
  either horizontal, vertical or diagonal."
  [[x1 y1 x2 y2]]
  (set/union
    ; `range` excludes end so explicitely add start and end here
    ; (negative direction -> start becomes end)
    #{[x1 y1] [x2 y2]}
    (set
      (cond
        (= x1 x2)
        (for [y (range y1 y2 (if (> y2 y1) 1 -1))] [x1 y])
        (= y1 y2)
        (for [x (range x1 x2 (if (> x2 x1) 1 -1))] [x y1])
        :diagonal
        (map #(vector %1 %2)
             (range x1 x2 (if (> x2 x1) 1 -1))
             (range y1 y2 (if (> y2 y1) 1 -1)))))))

(defn fill-world
  "Updates the given world with the points of the line, increasing every
  point's counter by one."
  [world line]
  (reduce (fn[w point] (update w point (fnil inc 0)))
          world
          (get-points-on-line line)))

(defn part-1
  []
  (->> (parse-input)
       (filter horiz-or-vert?)
       (reduce fill-world {})
       (vals)
       (filter #(> % 1))
       count))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       (reduce fill-world {})
       (vals)
       (filter #(> % 1))
       count))

(comment
  (set! *warn-on-reflection* true)
)
