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

  ;; You can define contraints:
  (defnc evenc [n]
    (even? n))
  (run* [q]
        (membero q (range 10))
        (evenc q))
  ;; => (0 2 4 6 8)

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
  (run* [q]
        (conso q [2 3 4] [1 2 3 4]))
  ;; => (1)

  (run* [q]
        (conso 1 q [1 2 3 4]))
  ;; => ((2 3 4))

  (run* [q]
        (conso 1 [2 3 4] [1 2 q 4]))
  ;; => (3)

  ;; Logical disjunction means: any of the goals should be satisfied
  (run* [q] (== q 3) (== q 6))
  ;; => ()
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
  ;; Find all solutions of the equation a * b = c for  all
  ;; values of c in 2150 - 2200.

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
    (run 3 [q]
         (== q vars)))
  ;; => ((_0 _1 _2 _3 _4 _5 _6 _7 _8))

  ;; And constrain them to a domain:
  (let [vars (repeatedly 9 lvar)]
    (run 3 [q]
         (== q vars)
         (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)))
  ;; => ((1 1 1 1 1 1 1 1 1) (2 1 1 1 1 1 1 1 1) (1 2 1 1 1 1 1 1 1))

  ;; ASSIGNMENT
  ;; Add a constraint that causes all numbers from 1-9 to be used in each
  ;; solution. Look at http://clojuredocs.org/clojure.core.logic.fd for tools.

  ;; SOLUTION
  (let [vars (repeatedly 9 lvar)]
    (run 3 [q]
         (== q vars)
         (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
         (fd/distinct vars)))
  ;; => ((1 2 3 4 5 6 7 8 9) (2 1 3 4 5 6 7 8 9) (1 3 2 4 5 6 7 8 9))

  ;; Next step is to provide some pieces of the puzzle that are given to us.
  ;; We do that by inputting a list of 9 numbers. The spots that the program
  ;; needs to fill in are zero; the hints given to us by the puzzle maker are
  ;; the other numbers. For example, [0 5 0 0 3 0 0 0 0] says that the second
  ;; number should be 5, and the fifth should be 3.

  (defn init [vars hints]
    (if (seq vars)                           ; recursion stopping criterion; while non-empty:
      (let [var (first vars)                 ; take the first var in the list
            hint (first hints)]              ; and the first hint
        (all                                 ; all following goals need to be satisfied:
         (if-not (zero? hint)                ;   if a hint was given
           (== var hint)                     ;     unify the var with that hint
           succeed)                          ;     otherwise no constraints here, go on
         (init (next vars) (next hints))))   ;   recursive call with rest of vars and hints
      succeed))                              ; end of recursion: start with success

  (let [hints [0 5 0 0 3 0 0 0 0]
        vars (repeatedly 9 lvar)]
    (run 3 [q]
         (== q vars)
         (init vars hints)
         (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
         (fd/distinct vars)))
  ;; => ((1 5 2 4 3 6 7 8 9) (2 5 1 4 3 6 7 8 9) (1 5 4 2 3 6 7 8 9))
  )
