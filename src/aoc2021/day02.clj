(ns aoc2021.day02
  (:require
    [clojure.string :as str]
    [aoc2021.util :as util]))

;;; Day 2: Dive!

(defn parse-command
  "Returns a `[dir units]` vector from the given input string.
  Example: \"forward 5\" yields [:forward 5]."
  [cmd]
  (let [[dir units] (str/split cmd #" +")]
    [(keyword dir) (parse-long units)]))

(defn parse-input
  "Returns input as seq of `[dir units]` vectors."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map parse-command)))

;; part 1

(defn move
  "Returns the given location `{:horiz n :depth n}` moved according to the
  given `[dir units]` where `dir` is :forward, :down or :up and `units`
  is be a number."
  [loc [dir units]]
  (case dir
    :forward (update loc :horiz + units)
    :down    (update loc :depth + units)
    :up      (update loc :depth - units)))

(defn part-1
  []
  (->> (parse-input)
       (reduce move {:horiz 0 :depth 0})
       vals
       (apply *)))

;; part 2

(defn move-with-aim
  "Returns the given location `{:horiz n :depth n :aim :n}` updated according
  to the given `[dir units]` where dir is :forward, :down or :up and `unit`
  is a number.
  This function applys the _aim_ variant of the movement which means
  `:down` and `:up` just update the `:aim` and only `:forward` really
  moves the location. "
  [loc [dir units]]
  (case dir
    ; forward increases :horiz (by units) and :depth (by aim * units)
    :forward (-> loc
                 (update :horiz + units)
                 (update :depth + (* (:aim loc) units)))
    ; down and up only change the :aim
    :down  (update loc :aim + units)
    :up    (update loc :aim - units)))

(defn part-2
  []
  (as-> (parse-input) $
    (reduce move-with-aim {:horiz 0 :depth 0 :aim 0} $)
    (select-keys $ [:horiz :depth])
    (vals $)
    (apply * $)))

(comment
  (set! *warn-on-reflection* true)
)
