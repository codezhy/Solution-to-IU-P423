#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp.rkt")
(require "r0-compiler.rkt")

(debug-level 4)

(define comfile
  (command-line
    #:program "compile"
    #:args (filename)
    ((compile-file #f r0-passes) filename)))
