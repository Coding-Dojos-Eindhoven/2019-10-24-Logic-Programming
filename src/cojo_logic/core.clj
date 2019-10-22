(ns cojo-logic.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(comment
  ;; First some basics. `run*` finds all solutions; it takes a logical variable
  ;; for which you specify a number of goals.
  (run* [q]
        (== q 1))
  ;; => (1)

  (run* [q]
        (== q 42))
  ;; => (42)

  ;; Unification (which is what the `==` does) can be quite clever:
  (run* [q]
        (== {:a q :b 2} {:a 1 :b 2}))
  ;; => (1)

  ;; You can have multiple goals:
  (run* [q r]
        (== {:a q :b 2} {:a 1 :b 2})
        (== {:a 1 :b r} {:a 1 :b 2}))
  ;; => ([1 2])

  ;; You can check membership of a list
  (run* [q]
        (membero q [1 2 3 4]))
  ;; => (1 2 3 4)
  ;; Interpretation: q can be 1, 2, 3, or 4 to satisfy the goal that it has
  ;; to be a member of the list [1 2 3 4]. 
  (run* [q]
        (membero 42 [1 2 q 4]))
  ;; => (42)
  ;; Interpretation: 42 can only be a member of the list [1 2 q 4] if q is 42.

  ;; You can define contraints:
  (defnc evenc [n]
    (even? n))
  (run* [q]
        (membero q (range 10))
        (evenc q))
  ;; => (0 2 4 6 8)
  ;; Interpretation: given that q is in the range 0-9, and that q must be even,
  ;; q can be 0, 2, 4, 6, or 8.

  ;; You can also use the package `clojure.core.logic.fd` for
  ;; finite domains. This is a lot faster than simply using
  ;; ranges and `membero` like above:  
  (with-out-str (time (doall (run* [q]
                                   (membero q (range 10000))
                                   (evenc q)))))
  ;; => "\"Elapsed time: 11399.727992 msecs\"\n"

  (with-out-str (time (doall (run* [q]
                                   (fd/in q (fd/interval 10000))
                                   (evenc q)))))
  ;; => "\"Elapsed time: 263.215347 msecs\"\n"

  ;; Fun with collections:
  (cons 1 [2 3 4]) ;; normal clojure operator `cons`
  ;; => (1 2 3 4)

  (run* [q]
        (conso q [2 3 4] [1 2 3 4]))
  ;; => (1)

  (run* [q]
        (conso 1 q [1 2 3 4]))
  ;; => ((2 3 4))

  (run* [q]
        (conso 1 [2 3 4] [1 2 q 4]))
  ;; => (3)

  ;; Logical conjunction means: all goals have to be satisfied
  (run* [q] (== q 3) (== q 6))
  ;; => ()
  ;; Logical disjunction means: any one of the goals has to be be satisfied
  (run* [q] (conde [(== q 3)]
                   [(== q 6)]))
  ;; => (3 6)

  (run* [x y]
        (conde [(conso x y [1 2 3 4])]
               [(== x y) (== x 2)]))
  ;; => ([1 (2 3 4)] [2 2])

  ;; If you're interested in a limited amount of solutions, use
  ;; `run` instead of `run*`:
  (run 7 [q]
       (fd/in q (fd/interval 3 1000000))
       (evenc q))
  ;; => (4 6 8 10 12 14 16)

  ;; ASSIGNMENT
  ;; Find all solutions of the equation a * b = 2172.
  ;;
  ;; Note: if you have a solution and if you're patient, you could even
  ;; find all sulutions for  all values of c in 2150 - 2200 in a * b = c.
  ;; The search space for that is quite large, so you may want to be a bit
  ;; clever about the way you solve it... 

  ;; SOLUTION
  (defnc is-prod? [a b c]
    (= (* a b) c))
  (with-out-str (time (doall (run* [a b c]
                                   (fd/in a (fd/interval 2 47))
                                   (fd/in b (fd/interval 47 1100))
                                   (== c 2172) ;; the whole range takes a bit more time...
                                   #_(fd/in c (fd/interval 2150 2200))
                                   (is-prod? a b c))
    ;; => ([2 1086 2172] [3 724 2172] [4 543 2172] [6 362 2172] [12 181 2172])
                             )))
  ;; => "\"Elapsed time: 1222.282627 msecs\"\n"

  ;; Using `lvar`, you can also introduce logical variables.
  (let [v (lvar)]
    (run* [q]
          (== v 3)
          (== v q)))
  ;; => (3)

  ;; This technique allows us to dynamically introduce a number of variables:
  (let [vars (repeatedly 9 lvar)]
    (run* [q]
          (== q vars)))
  ;; => ((_0 _1 _2 _3 _4 _5 _6 _7 _8))

  ;; And constrain them to a domain:
  (defn constrain-to-domain
    "Returns a goal that causes the given variable to be constrained
     to a number between 1 and 9."
    [v]
    (fd/in v (fd/domain 1 2 3 4 5 6 7 8 9)))
  (let [vars (repeatedly 9 lvar)]
    (run 3 [q]
         (== q vars)
         (everyg constrain-to-domain vars)))
  ;; => ((1 1 1 1 1 1 1 1 1) (2 1 1 1 1 1 1 1 1) (1 2 1 1 1 1 1 1 1))

  ;; ASSIGNMENT
  ;; Add a constraint to the previous run that causes all numbers from 1-9 to
  ;; be used in each solution. In other words, all numbers have to be distinct.
  ;; Look at http://clojuredocs.org/clojure.core.logic.fd for tools.

  ;; SOLUTION
  (let [vars (repeatedly 9 lvar)]
    (run 3 [q]
         (== q vars)
         (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
         (fd/distinct vars)))
  ;; => ((1 2 3 4 5 6 7 8 9) (2 1 3 4 5 6 7 8 9) (1 3 2 4 5 6 7 8 9))

  ;; Next step is to provide some pieces of the puzzle that are given to us,
  ;; some hints, just like when you're solving a sudoku.
  ;; We do that by inputting a list of 9 numbers. The spots that the program
  ;; needs to fill in are zero; the hints given to us by the puzzle maker are
  ;; the other numbers. For example, [0 5 0 0 3 0 0 0 0] says that the second
  ;; number should be 5, and the fifth should be 3.

  (defn bind [var hint]
    (if-not (zero? hint)
      (== var hint)
      succeed))

  (defn bind-all [vars hints]
    (and* (map bind vars hints)))

  (let [hints [0 5 0 0 3 0 0 0 0]
        vars (repeatedly (count hints) lvar)]
    (run 3 [q]
         (== q vars)
         (bind-all vars hints)
         (everyg constrain-to-domain vars)
         (fd/distinct vars)))
  ;; => ((1 5 2 4 3 6 7 8 9) (2 5 1 4 3 6 7 8 9) (1 5 4 2 3 6 7 8 9))

  ;; Let's extend this to two rows instead of one.
  (defn rows [elements]
    (mapv vec (partition 9 elements)))
  (rows [0 5 0 0 3 0 0 0 0 7 0 0 0 0 9 0 0 0])
  ;; => [[0 5 0 0 3 0 0 0 0] [7 0 0 0 0 9 0 0 0]]

  (let [hints [0 5 0 0 3 0 0 0 0
               7 0 0 0 0 9 0 0 0]
        vars (repeatedly (count hints) lvar)
        rows (rows vars)]
    (run 1 [q]
         (== q vars)
         (bind-all vars hints)
         (everyg constrain-to-domain vars)
         (everyg fd/distinct rows)))
  ;; => ((1 5 2 4 3 6 7 8 9
  ;;      7 1 2 3 4 9 5 6 8))

  ;; You can probably see how this extends to 9 rows instead of just two.
  ;; Columns are next. A small utility:

  (defn transpose
    "Transposes a matrix. Represents the matrix as a list of columns
     instead of rows."
    [matrix]
    (into [] (apply map vector matrix)))

  (transpose (partition 3 [1 2 3
                           4 5 6
                           7 8 9]))
  ;; => [[1 4 7] [2 5 8] [3 6 9]]

  ;; ASSIGNMENT
  ;; Extend the query above to not only cause all rows to have distinct numbers,
  ;; but also all columns.

  ;; SOLUTION
  (partition 9 (first
                (let [hints [2 0 7 0 1 0 5 0 8
                             0 0 0 6 7 8 0 0 0
                             8 0 0 0 0 0 0 0 6
                             0 7 0 9 0 6 0 5 0
                             4 9 0 0 0 0 0 1 3
                             0 3 0 4 0 1 0 2 0
                             5 0 0 0 0 0 0 0 1
                             0 0 0 2 9 4 0 0 0
                             3 0 6 0 8 0 4 0 9]
                      vars (repeatedly (count hints) lvar)
                      rows (rows vars)
                      cols (transpose rows)]
                  (run 1 [q]
                       (== q vars)
                       (bind-all vars hints)
                       (everyg constrain-to-domain vars)
                       (everyg fd/distinct rows)
                       (everyg fd/distinct cols)))))
  ;; => ((2 4 7 3 1 9 5 6 8)
  ;;     (9 1 5 6 7 8 2 3 4)
  ;;     (8 2 1 5 3 7 9 4 6)
  ;;     (1 7 8 9 4 6 3 5 2)
  ;;     (4 9 2 8 6 5 7 1 3)
  ;;     (6 3 9 4 5 1 8 2 7)
  ;;     (5 8 4 7 2 3 6 9 1)
  ;;     (7 6 3 2 9 4 1 8 5)
  ;;     (3 5 6 1 8 2 4 7 9))

  ;; ASSIGNMENT
  ;; Extend the query to not only cause all rows and columns to have distinct
  ;; numbers, but also all 3x3 squares of the puzzle.

  ;; SOLUTION 
  )
