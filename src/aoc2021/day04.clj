(ns aoc2021.day04
  (:require
    [clojure.string :as str]
    [aoc2021.util :as util]))

;;; Day 4: Giant Squid

(defn parse-numbers
  "Parses one line of numbers, returning a vec of longs."
  [line]
  (->> line (re-seq #"\d+") (mapv parse-long)))

(defn parse-boards
  "Parses the boards, returning a seq of boards with one board being
  a vec of vecs of longs."
  [lines]
  (->> lines
       (partition-by empty?)
       (take-nth 2)
       (map #(mapv parse-numbers %))))

(defn parse-input
  "Returns bingo input as map {:numbers [...] :boards #{[[...] ...] ...}."
  []
  (let [[numbers-line _ & boards-lines]
        (->> (util/get-input *ns*) (str/split-lines))]
    {:numbers (parse-numbers numbers-line)
     :boards (parse-boards boards-lines)}))

;; part 1

(defn play-board
  "Plays one number on one board, yielding the board with matching numbers
  increased by 100."
  [board n]
  (mapv
    #(mapv (fn[x] (if (= x n) (+ 100 x) x)) %)
    board))

(defn winner-board?
  "Returns if any row or column has all their elements greater than
  or equals to 100."
  [board]
  (some (fn[row] (every? #(>= % 100) row)) ;check-row
        (concat #_:rows board
                #_:cols (apply mapv vector board))))

(defn play-boards
  "Plays the given number on all boards using `play-board`."
  [boards n]
  (map #(play-board % n) boards))

(defn calc-score
  "Calculates the score."
  [board number]
  (->> board
       flatten
       (filter #(< % 100))
       (apply +)
       (* number)))

(defn play-number-then-check
  "Plays one number on all boards then checks for a winner.
  Returns updated `state`, or `(reduced state)` if all boards Bingo!'ed."
  [state n]
  (let [new-state (-> state
                      (update :boards play-boards n)
                      (update :numbers conj n))
        ; {:solved <boards that won>, :unsolved <other boards>}
        boards' (group-by (comp #(if % :solved :unsolved) winner-board?)
                          (:boards new-state))]
    (if (empty? (:solved boards'))
      ; no new Bingo! boards in this round:
      new-state
      ; return updated state:
      (let [with-scores (mapv (fn[b]
                                {:round (count (:numbers new-state))
                                 :number n
                                 :board b
                                 :score (calc-score b n)})
                              (:solved boards'))
            new-state (-> new-state
                          (assoc :boards (:unsolved boards'))
                          (update :winners concat with-scores))]
        (if (empty? (:unsolved boards'))
          (reduced new-state)
          new-state)))))

(defn play-bingo
  "Plays Bingo! with all given numbers on all given boards.

  Returns a map with:
  {:numbers <remaining> :boards <unsolved> :winners <solved>}
  (Any of the seqs can be empty, e.g. if the numbers are all exhausted.)

  Winners are structured like this:
  {:round <n> :number <n> :board <board> :score <n>}."
  [{:keys [numbers boards]}]
  (reduce play-number-then-check
          {:numbers [] :boards boards :winners []}
          numbers))

(defn part-1
  []
  (-> (parse-input)
      play-bingo
      :winners
      first))

;; part 2

(defn part-2
  []
  (-> (parse-input)
      play-bingo
      :winners
      last))

(comment
  (set! *warn-on-reflection* true)
)
