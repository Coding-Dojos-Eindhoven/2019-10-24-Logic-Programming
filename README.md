# Coding Dojo about Logic Programming, using Clojure

*October 24, 2019*

![Puzzle](https://raw.githubusercontent.com/Coding-Dojos-Eindhoven/website/master/2019-10-24/puzzle.png)

## Intro (from invitation)

We know object-oriented programming, imperative programming, we have even done
functional programming, but there is also “logic programming”. Intuitively this
means that you specify a set of criteria that the solution has to satisfy, and
then let the computer come up with the solution(s). In this coding dojo we will
dip our toes into logic programming. A well-known language for that is Prolog,
but we will use Clojure for this. Don’t worry if you don’t know anything about
Clojure, we’ll start from the beginning.

## Contents

This repo contains three source code files:

* `core.clj` is the file that we are using during the coding dojo. We will
  interactively build this up step by step towards the entire solver at the end.
* `sudoku.clj` is final solution without all the other stuff around it.
* `s-doku.clj` contains a solution to a variant of the sudoku puzzle.

## Usage

This project was created using [NightCode][NightCode]. It's a normal Leiningen
project, so any other Clojure IDE should also work. There is no `main`, so you
cannot run this thing, just use the REPL.

[NightCode]: https://sekao.net/nightcode/
