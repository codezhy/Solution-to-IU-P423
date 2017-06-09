#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp.rkt")
(require "r0-compiler.rkt")

(debug-level 4)

((compile-file #f r0-passes) "tests/r1_8.rkt")