#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp.rkt")
(require "r0-compiler.rkt")

(debug-level 1)

;(interp-tests "R0: uniquify pass" #f r0-passes interp-scheme "r1" (range 1 22))
;(interp-tests "R0: flatten pass" #f r0-passes interp-scheme "r1a" (range 1 12))
(interp-tests "R0: select instructions pass" #f r0-passes interp-scheme "r1" (range 1 22))
(display "tests passed!") (newline)
