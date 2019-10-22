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

(defn bind [var hint]
  (if-not (zero? hint)
    (== var hint)
    succeed))

(defn bind-all [vars hints]
  (and* (map bind vars hints)))

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
         (bind-all vars puzzle)
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

;;
;; DONE!
;;
