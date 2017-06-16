#lang racket

(require "interp.rkt")
(require "dict.rkt")
(require "utilities.rkt")
(require "r0-compiler.rkt")

;; This exports r0-passes, defined below, to users of this file.
(provide r0-reg-passes)

(define r0-reg-compiler
  (class r0-compiler
         
         (super-new)

         (inherit flatten-list-1)

         ; pass uncover-live : x86* -> x86*
         (define/public (uncover-live)
           (lambda (ast)
             (match ast
                    [`(program ,args ,es ...)
                     (let ([live-afters (_uncover-live es '())])
                       `(program ,(cons args live-afters) ,@es))]
                    )))
         
         (define _uncover-live
           (lambda (instruction-lst live-afters)
             (cond [(null? instruction-lst) live-afters]
                   [(null? live-afters)
                    (_uncover-live (cdr instruction-lst)
                                   (list (set-subtract (set-union (set)
                                                                  (_compute-write-variables (car instruction-lst)))
                                                       (_compute-read-variables (car instruction-lst)))))]
                   [else
                    (_uncover-live (cdr instruction-lst)
                                   (append live-afters
                                           (list (set-subtract (set-union (last live-afters)
                                                                          (_compute-write-variables (car instruction-lst)))
                                                               (_compute-read-variables (car instruction-lst))))))]
                   )))
         
         (define _variables
           (lambda (instruction)
             (match instruction
                    [`(,op (var ,e1) ,e2) (set e1)]
                    [`(,op ,e1 (var ,e2)) (set e2)]
                    [`(,op (var ,e1) (var ,e2)) (set e1 e2)]
                    [else (set)])))
         
         (define _compute-read-variables
           (lambda (instruction)
             (match instruction
                    [`(,op (var ,e1) ,e2) (set e1)]
                    [else (set)])))
         
         (define _compute-write-variables
           (lambda (instruction)
             (match instruction
                    [`(,op ,e1 (var ,e2)) (set e2)]
                    [else (set)])))
         
         ; pass build-interference : x86* -> x86*
         (define/public (build-interference)
           (lambda (ast)
             (match ast
                    [`(program (,args ,live-afters ...) ,es ...)
                     `(program ,(cons args (_helper es live-afters (make-graph '()))) ,@es)]
                    [else ast]
                    )))
         
         ; List of Instruction -> List of Live-after Set -> Graph -> Gragh
         (define _helper
           (lambda (instructions live-afters graph)
             (if (null? instructions)
                 graph
               (match (car instructions)
                      [`(movq ,s ,d)
                       (let* ([live-after-set (car live-afters)]
                              [vertices (set-subtract live-after-set (_variables `(movq ,s ,d)))]
                              [edges (flatten-list-1 (set-map vertices
                                                              (lambda (v) (set-map (_compute-write-variables `(movq ,s ,d))
                                                                                   (lambda (d) (cons d v))))))])
                         (_helper (cdr instructions) (cdr live-afters) (add-edges graph edges)))]
                      [`(addq ,s ,d)
                       (let* ([live-after-set (car live-afters)]
                              [vertices (set-subtract live-after-set (_compute-write-variables `(addq ,s ,d)))]
                              [edges (flatten-list-1 (set-map vertices
                                                              (lambda (v) (set-map (_compute-write-variables `(movq ,s ,d))
                                                                                   (lambda (d) (cons d v))))))])
                         (_helper (cdr instructions) (cdr live-afters) (add-edges graph edges)))]
                      [`(callq ,label)
                       (let* ([live-after-set (car live-afters)]
                              [edges (flatten-list-1 (set-map live-after-set
                                                              (lambda (v) (set-map caller-save
                                                                                   (lambda (r) (cons r v))))))])
                         (_helper (cdr instructions) (cdr live-afters) (add-edges graph edges)))]
                      [else
                       (_helper (cdr instructions) (cdr live-afters) graph)]
                      )
               )))
         
         ))

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (or whatever file provides your passes)
;; should be named "compiler.rkt"
(define r0-reg-passes
  (let ([compiler (new r0-reg-compiler)])
  `( ("uniquify" ,(send compiler uniquify '((dict-init))) ,interp-scheme)
    ("flatten" ,(send compiler flatten2) ,interp-C)
    ("select instructions" ,(send compiler select-instructions) ,interp-x86)
    ("uncover lives" ,(send compiler uncover-live) ,interp-x86)
    ("build interference" ,(send compiler build-interference) ,interp-x86)
    ;("assign homes" ,(assign-homes (dict-init)) ,interp-x86)
    ;("patch instructions" ,patch-instructions ,interp-x86)
    ;("print x86" ,print-x86 #f)
    )))
