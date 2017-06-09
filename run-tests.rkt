#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp.rkt")
(require "r0-compiler.rkt")

(debug-level 1)

(interp-tests "integers and arithmetic" #f r0-passes interp-scheme "r1" (range 1 21))
(display "tests passed!") (newline)