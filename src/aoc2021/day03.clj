(ns aoc2021.day03
  (:require
    [clojure.string :as str]
    [aoc2021.util :as util]))

;;; Day 3: Binary Diagnostic

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)))

(defn binary->int
  "Convert a seq of 0s and 1s into an integer."
  [digits]
  (Integer/parseInt (apply str digits) 2))

;; part 1

(defn part-1
  []
  (let [input (parse-input)
        half (/ (count input) 2)
        most-freq-bits (->> input
                            ; transpose:
                            (apply map vector)
                            ; count the 1s:
                            (map #(count (filter #{\1} %)))
                            ; decide 0 or 1 according to freq:
                            (mapv #(if (>= % half) 1 0)))
        gamma (binary->int most-freq-bits)
        epsilon (->> most-freq-bits
                     ; invert 0s and 1s
                     (mapv {0 1, 1 0}) 
                     binary->int)]
    {:gamma gamma
     :epsilon epsilon
     :power (* gamma epsilon)}))

;; part 2

(defn more-or-equals-ones?
  "Returns if the given lines have the same amount of ones and zeroes
  or more ones than zeros at the given index."
  [idx lines]
  (let [freq (->> lines
                  (map #(get % idx))
                  frequencies)]
    (>= (freq \1 0) (freq \0 0))))

(defn =-idx-val
  "Returns true if `what` equals the line-value at the given index."
  [what idx line]
  (= what (get line idx)))

(defn oxy-bit-crit
  "Returns the oxygen bit criteria fn."
  [idx line]
  (if (more-or-equals-ones? idx line)
    (partial =-idx-val \1 idx)
    (partial =-idx-val \0 idx)))

(defn co2-bit-crit
  "Returns the co2 bit criteria fn."
  [idx line]
  (if (more-or-equals-ones? idx line)
    (partial =-idx-val \0 idx)
    (partial =-idx-val \1 idx)))

(defn filter-rating
  "Filters the given input according to the given criteria fn."
  [crit-fn input]
  (loop [lines input
         idx 0]
    (if (= 1 (count lines))
      (first lines)
      (recur (filter (crit-fn idx lines) lines)
             (inc idx)))))

(defn part-2
  []
  (let [input (parse-input)
        oxy (->> input (filter-rating oxy-bit-crit) binary->int)
        co2 (->> input (filter-rating co2-bit-crit) binary->int)]
    {:oxygen oxy
     :co2 co2
     :life-support (* oxy co2)}))


(comment
  (set! *warn-on-reflection* true)
)
