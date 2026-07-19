(ns aoc2021.day08
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [aoc2021.util :as util]))

;;; Day 8: Seven Segment Search

(defn parse-line
  "Returns one input line as [[<samples: 10 strings>] [<digits: 4 strings>]]."
  [line]
  (->> line
       (re-seq #"[a-g]+")
       (split-at 10)
       (mapv vec)))

(defn parse-input
  "Returns input as seq of [[<samples: 10 strings>] [<digits: 4 strings>]]."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map parse-line)))

(def numbers
  "Map from segment string to number."
  {"abcefg"   0  ; 6 segments
   "cf"       1  ; 2 segments - uniq
   "acdeg"    2  ; 5 segments
   "acdfg"    3  ; 5 segments
   "bcdf"     4  ; 4 segments - uniq
   "abdfg"    5  ; 5 segments
   "abdefg"   6  ; 6 segments
   "acf"      7  ; 3 segments - uniq
   "abcdefg"  8  ; 7 segments - uniq
   "abcdfg"   9}); 6 segments

(def numbers-as-set
  "Like `numbers` but keys are sets of the segment strings."
  (update-keys numbers set))

(def numbers-by-length
  "Map from number of segments to segment string, only for 
  numbers with uniq segment count."
  (as-> numbers $
    (group-by (comp count first) $)
    (remove #(-> % second count (> 1)) $)
    (into {} $)
    (update-vals $ ffirst)))


;; part 1

(defn part-1
  []
  (->> (parse-input)
       (map second)
       (flatten)
       (filter #(contains? numbers-by-length (count %)))
       count))

;; part 2

(defn analyze-samples
  "Analyzes the samples, returns map with wrong-segment -> correct-segment.
  Example input: [<sample string> ...] ; (from one input line)
  Example output: {\\e \\a, \\d \\b, ...}."
  [samples]
  (let [; create map with: number of segments -> sample
	size->samples (->> samples (map set) (group-by count))
        ; deduction: see also day08.txt
        ; a = segments of 7 - segments of 1
	a (-> (first (size->samples 3)) ; 3-> 7
	      (set/difference (first (size->samples 2))) ; 2-> 1
	      first)
        ; g = intersection of size_6 - segments of 4 - a
	g (-> (apply set/intersection (size->samples 6)) ; 6-> 0, 6, 9
	      (set/difference (first (size->samples 4))) ; 4-> 4
	      (disj a)
	      first)
        ; d = intersection of size_5 - a - d
	d (-> (apply set/intersection (size->samples 5)) ; 5-> 2, 3, 5
	      (disj a g)
	      first)
        ; b = segments of 4 - segments of 1 - d
	b (-> (first (size->samples 4)) ; 4-> 4
	      (set/difference (first (size->samples 2))) ; 2-> 1
	      (disj d)
	      first)
        ; f = segments of 5 with b - a - b - d - g
	f (as-> (size->samples 5) $ ; 5-> 2, 3, 5
	    (filter #(% b) $) ; all length-5 with a 'b'
	    (first $)
	    (disj $ a b d g)
	    (first $))
        ; c = segments of 1 - f
	c (-> (size->samples 2) ; 2-> 1
	      first
	      (disj f)
	      first)
        ; e = segments of 8 - <all other known segments>
	e (-> (size->samples 7) ; 7-> 8
	      first (disj a b c d f g)
	      first)]
    (zipmap [a b c d e f g]
	    [\a \b \c \d \e \f \g])))

(defn apply-mapping
  "Applies the mapping (wrong -> correct segments) to the given digits, 
  returning the resulting 4-digit number."
  [mapping digits]
  (->> digits
       (map (fn[ds] (numbers-as-set (set (map mapping ds)))))
       ; digit sequence -> number:
       (reduce (fn[s d] (+ (* 10 s) d)))))

(defn solve-sample-and-digit
  "Solves one input line using `analyze-samples` and `apply-mapping`."
  [[samples digits]]
  (-> (analyze-samples samples)
      (apply-mapping digits)))

(defn part-2
  []
  (->> (parse-input)
       (map solve-sample-and-digit)
       (reduce +)))

(comment
  (set! *warn-on-reflection* true)
)
