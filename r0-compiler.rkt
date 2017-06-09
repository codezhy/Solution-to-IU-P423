#lang racket

(require "interp.rkt")
(require "alist.rkt")

;; This exports r0-passes, defined below, to users of this file.
(provide r0-passes)

(define (uniquify alist)
  (lambda (ast)
    (match ast
      [(? symbol?) 
        (let [(result (alist-lookup ast alist))]
          (if result
            result
            ast))]
      [(? integer?) ast]
      [`(let ([,x ,e]) ,body) 
          (let* [(new-x (gensym x))
                 (new-alist (alist-add new-x x alist))]
            `(let ([,new-x ,((uniquify new-alist) e)]) ,((uniquify new-alist) body)))]
      [`(program ,e)
        `(program ,((uniquify alist) e))]
      [`(,op ,es ...)
        `(,op ,@(map (uniquify alist) es))]
  )))

(define uniquify2 (uniquify (alist-init)))

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (or whatever file provides your passes)
;; should be named "compiler.rkt"
(define r0-passes
  `( ("uniquify" ,uniquify2 ,interp-scheme)
     ))

