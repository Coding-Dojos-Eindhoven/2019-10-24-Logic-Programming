(ns cojo-logic.sudoku
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

;;
;; Sudoku Solver
;; 

;; This is the sudoku that we're going to solve.
;; Empty cells are represented as zero.
(def sudoku
  [2 0 7 0 1 0 5 0 8
   0 0 0 6 7 8 0 0 0
   8 0 0 0 0 0 0 0 6
   0 7 0 9 0 6 0 5 0
   4 9 0 0 0 0 0 1 3
   0 3 0 4 0 1 0 2 0
   5 0 0 0 0 0 0 0 1
   0 0 0 2 9 4 0 0 0
   3 0 6 0 8 0 4 0 9])

(defn rows
  "Takes a one-dimensional array of values and converts it
   into a matrix (array of rows)."
  [elems]
  (->> elems
       (partition 9)
       (map vec)
       (into [])))
(comment
  (rows sudoku)
  ;; => [[2 0 7 0 1 0 5 0 8]
  ;;     [0 0 0 6 7 8 0 0 0]
  ;;     [8 0 0 0 0 0 0 0 6]
  ;;     [0 7 0 9 0 6 0 5 0]
  ;;     [4 9 0 0 0 0 0 1 3]
  ;;     [0 3 0 4 0 1 0 2 0]
  ;;     [5 0 0 0 0 0 0 0 1]
  ;;     [0 0 0 2 9 4 0 0 0]
  ;;     [3 0 6 0 8 0 4 0 9]]
  )

(defn transpose
  "Transposes a matrix. Represents the matrix as a list of columns
   instead of rows."
  [matrix]
  (into [] (apply map vector matrix)))
(comment
  (transpose (rows sudoku))
  ;; => [[2 0 8 0 4 0 5 0 3]
  ;;     [0 0 0 7 9 3 0 0 0]
  ;;     [7 0 0 0 0 0 0 0 6]
  ;;     [0 6 0 9 0 4 0 2 0]
  ;;     [1 7 0 0 0 0 0 9 8]
  ;;     [0 8 0 6 0 1 0 4 0]
  ;;     [5 0 0 0 0 0 0 0 4]
  ;;     [0 0 0 5 1 2 0 0 0]
  ;;     [8 0 6 0 3 0 1 0 9]]
  )

(defn get-square
  "Returns a seq of values of a 3x3 square submatrix at given coordinates."
  [matrix x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in matrix [x y])))
(comment
  (get-square (rows (range 81)) 0 0)
  ;; => (0 1 2 9 10 11 18 19 20)
  )

(defn squares
  "Returns a list of all squares of the sudoku."
  [matrix]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (get-square matrix x y)))

(comment
  (squares (rows sudoku))
  ;; => ((2 0 7 0 0 0 8 0 0)
  ;;     (0 1 0 6 7 8 0 0 0)
  ;;     (5 0 8 0 0 0 0 0 6)
  ;;     (0 7 0 4 9 0 0 3 0)
  ;;     (9 0 6 0 0 0 4 0 1)
  ;;     (0 5 0 0 1 3 0 2 0)
  ;;     (5 0 0 0 0 0 3 0 6)
  ;;     (0 0 0 2 9 4 0 8 0)
  ;;     (0 0 1 0 0 0 4 0 9))
  )

;;
;; So far everything was plain functional programming. Now we start
;; using core.logic.
;;

(defn init
  "Takes a list of logic variables and a list of values,
   and pairwise unifies the variables and the values. In other
   words: it initializes the logic variables with the values
   provided in the puzzle."
  [vars puzzle]
  (if (seq vars)
    (let [hint (first puzzle)]
      (all
       (if-not (zero? hint)
         (== (first vars) hint)
         succeed)
       (init (next vars) (next puzzle))))
    succeed))

(defn solve-sudoku
  "Solves a sudoku represented as a one-dimensional array of
   81 values. Blanks are represented as zeros."
  [puzzle]
  (let [vars (repeatedly 81 lvar)
        rows (rows vars)
        cols (transpose rows)
        squares (squares rows)]
    (run 1 [q]
         (== q vars)
         (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
         (init vars puzzle)
         (everyg fd/distinct rows)
         (everyg fd/distinct cols)
         (everyg fd/distinct squares))))

(defn print-board
  "Pretty-prints a board, a one-dimensional array of 81 values."
  [board]
  (->> board
       (partition 9)
       (clojure.pprint/pprint)))

(comment
  (def solution (first (solve-sudoku sudoku)))
  solution
  (print-board solution))

#_(defn transpose [matrix]
    (into [] (apply map vector matrix)))

;;
;; DONE!
;;

;; Below is a variation on Sudoku, called S-Doku. It is a normal sudoku
;; with some extra constraints. Suppose you draw an S-shape on the board,
;; like this:
;;
;; +-------+-------+-------+
;; | . . X | X X X | X . . |
;; | . X . | . . . | . X X |
;; | X . . | . . . | . . . |
;; +-------+-------+-------+
;; | X . . | . . . | . . . |
;; | . X X | X X X | X X . |
;; | . . . | . . . | . . X |
;; +-------+-------+-------+
;; | . . . | . . . | . . X |
;; | X X . | . . . | . X . |
;; | . . X | X X X | X . . |
;; +-------+-------+-------+
;;
;; The six additional constrains are that, for each of the three rows
;; and columns in the above diagram, the cells marked with an X also
;; have to contain all digits 1 - 9.
;;
;; Introducing a bit of terminology:
;;     "ennead": a group of nine
;;

;; This is the shape that we're using:
(def shape
  ["  XXXXX  "
   " X     XX"
   "X        "
   "X        "
   " XXXXXXX "
   "        X"
   "        X"
   "XX     X "
   "  XXXXX  "])

;;
;; Defines which groups of rows and columns of the
;; shape should form enneads. For example, [:rows [0 1 2]]
;; means that the X-s of first three rows should
;; form an ennead.
(def ennead-specs
  [[:rows [0 1 2]]
   [:rows [3 4 5]]
   [:rows [6 7 8]]
   [:cols [0 1 2]]
   [:cols [3 4 5]]
   [:cols [6 7 8]]])

(defn shape-line-to-ints
  "Takes a string and converts all spaces to 0 and everything
   else to 1."
  [shape-line]
  (map {false 1 true 0} (map #(= \space %) shape-line)))

(defn shape-to-ints
  "Converts the shape as defined above to a matrix of zeros
   and ones, indicating which cells are part of the shape
   and which are not."
  [shape]
  (into [] (map shape-line-to-ints shape)))

(comment
  (shape-to-ints shape))
  ;; => [(0 0 1 1 1 1 1 0 0)
  ;;     (0 1 0 0 0 0 0 1 1)
  ;;     (1 0 0 0 0 0 0 0 0)
  ;;     (1 0 0 0 0 0 0 0 0)
  ;;     (0 1 1 1 1 1 1 1 0)
  ;;     (0 0 0 0 0 0 0 0 1)
  ;;     (0 0 0 0 0 0 0 0 1)
  ;;     (1 1 0 0 0 0 0 1 0)
  ;;     (0 0 1 1 1 1 1 0 0)]


(defmulti shape-part
  "Given a shape, a row/col-spec indicating which part of the
   shape is relevant, and a board, returns a list of pairs, each
   containing one of the specified rows or cols from both the
   shape and the board. This is a building block to extract
   the enneads for the shape later on."
  (fn [_ spec & args] (first spec)))

(defmethod shape-part :rows [shape spec rows]
  (let [selected-rows (spec 1)]
    (for [row selected-rows]
      [(shape row) (rows row)])))

(defmethod shape-part :cols [shape spec rows]
  (let [selected-cols (spec 1)
        shape_t       (transpose shape)
        cols          (transpose rows)]
    (for [col selected-cols]
      [(shape_t col) (cols col)])))

(comment
  (def enabled-spec-and-rows
    (let [board       (rows (range 81))
          ennead-spec [:rows [0 1 2]]
          shape       (shape-to-ints shape)]
      (shape-part shape ennead-spec board)))
  enabled-spec-and-rows
  ;; => ([(0 0 1 1 1 1 1 0 0) [0 1 2 3 4 5 6 7 8]]
  ;;     [(0 1 0 0 0 0 0 1 1) [9 10 11 12 13 14 15 16 17]]
  ;;     [(1 0 0 0 0 0 0 0 0) [18 19 20 21 22 23 24 25 26]])

  (def enabled-spec-and-cols
    (let [board       (rows (range 81))
          ennead-spec [:cols [6 7 8]]
          shape       (shape-to-ints shape)]
      (shape-part shape ennead-spec board)))
  enabled-spec-and-cols
  ;; => ([[1 0 0 0 1 0 0 0 1] [6 15 24 33 42 51 60 69 78]]
  ;;     [[0 1 0 0 1 0 0 1 0] [7 16 25 34 43 52 61 70 79]]
  ;;     [[0 1 0 0 0 1 1 0 0] [8 17 26 35 44 53 62 71 80]])
  )

(defn select-enabled [[enabled-seq seq]]
  "Takes a pair, containing a list of zeros and ones (the enabled
   indicators), and a row/col from the board, and returns the elements
   of the row/col for which the enabled indicator is one."
  (->> [enabled-seq seq]
       (apply map vector)
       (filter (comp not zero? first))
       (map second)))

(comment
  (select-enabled [[0 0 1 1 1 1 1 0 0] [10 11 12 13 14 15 16 17 18]])
  ;; => (12 13 14 15 16)
  )

(defn get-ennead
  "Takes a shape, a spec, and a board, and returns the ennead from
   the board that matches the specified part of the shape.

   shape -- a matrix of zeros or ones
   spec -- a pair of :row or :col and a list of row/col indexes, e.g. [:row [3 4 5]]
   board -- the matrix containing the puzzle"
  [shape spec board]
  (let [enabled-spec-and-rows (shape-part shape spec board)]
    (apply concat (map select-enabled enabled-spec-and-rows))))

(comment
  ;; So now we can finally use the spec [:rows [0 1 2]] to get the
  ;; ennead from the board:
  (let [shape (shape-to-ints shape)
        spec  (first ennead-specs)
        board (rows (range 81))]
    (get-ennead shape spec board))
  ;; => (2 3 4 5 6 10 16 17 18) 
  )

(defn all-enneads
  "Returns all enneads of the board matching the specs on the given shape."
  [shape specs board]
  (map #(get-ennead shape % board) specs))

(comment
  (let [shape (shape-to-ints shape)
        board (rows (range 81))]
    (all-enneads shape ennead-specs board))
  ;; => ((2 3 4 5 6 10 16 17 18)
  ;;     (27 37 38 39 40 41 42 43 53)
  ;;     (62 63 64 70 74 75 76 77 78)
  ;;     (18 27 63 10 37 64 2 38 74)
  ;;     (3 39 75 4 40 76 5 41 77)
  ;;     (6 42 78 16 43 70 17 53 62))
  )

;; A simple s-doku example:
(def s-doku-1
  [4 0 0 0 6 0 0 0 3
   0 9 0 1 0 7 0 2 0
   3 0 0 0 0 0 0 0 8
   0 8 0 0 0 0 0 4 0
   5 0 0 0 0 0 0 0 2
   0 1 0 0 0 0 0 7 0
   7 0 0 0 0 0 0 0 1
   0 6 0 0 0 0 0 3 0
   1 0 0 0 2 0 0 0 6])

;; A difficult s-doku example:
(def s-doku-2
  [0 0 0 0 0 3 0 0 0
   0 0 0 0 6 0 0 0 0
   0 0 0 4 0 0 0 0 0
   0 0 8 0 0 0 0 0 0
   4 0 0 2 0 0 0 0 0
   0 0 0 9 0 0 0 0 0
   0 0 0 5 0 0 0 0 0
   0 0 0 0 7 0 8 0 0
   0 0 0 0 0 1 0 0 0])

(defn solve-s-doku
  "Solves a s-doku."
  [puzzle]
  (let [vars (repeatedly 81 lvar)
        rows (rows vars)
        cols (transpose rows)
        squares (squares rows)
        enneads (all-enneads (shape-to-ints shape) ennead-specs rows)]
    (run 2 [q]
         (== q vars)
         (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
         (init vars puzzle)
         (everyg fd/distinct rows)
         (everyg fd/distinct cols)
         (everyg fd/distinct squares)
         (everyg fd/distinct enneads))))

(comment
  (print-board (first (solve-s-doku s-doku-1)))
  (print-board (first (solve-s-doku s-doku-2))))
