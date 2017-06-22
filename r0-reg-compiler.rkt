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
         ; Compute from instructions back to front
         (define/public (uncover-live)
           (lambda (ast)
             (match ast
                    [`(program ,args ,es ...)
                     (let ([live-afters (_uncover-live (reverse es) (list (set)))])
                       `(program ,(cons args live-afters) ,@es))]
                    )))
         
         (define _uncover-live
           (lambda (instruction-lst live-afters)
               (cond [(null? instruction-lst) (cdr (reverse live-afters))] ; drop the unexists first live-after set
                     [else
                      (_uncover-live (cdr instruction-lst)
                                     (append live-afters
                                             (list (set-union (set-subtract (last live-afters)
                                                                            (_write-variables (car instruction-lst)))
                                                              (_read-variables (car instruction-lst))))))]
                     )))
         
         (define _variables
           (lambda (instruction)
             (match instruction
                    [`(,op (var ,e1) ,e2) (set e1)]
                    [`(,op ,e1 (var ,e2)) (set e2)]
                    [`(,op (var ,e1) (var ,e2)) (set e1 e2)]
                    [`(,op (var ,e)) (set e)]
                    [else (set)])))
         
         (define _read-variables
           (lambda (instruction)
             (match instruction
                    [`(var ,x) (set x)]
                    [`(movq ,e1 ,e2) (_read-variables e1)]
                    [`(addq ,e1 ,e2) (set-union (_read-variables e1) (_read-variables e2))]
                    [`(neq (var ,e1)) (set e1)]
                    [else (set)])))
         
         (define _write-variables
           (lambda (instruction)
             (match instruction
                    [`(var ,x) (set x)]
                    [`(movq ,e1 ,e2) (_write-variables e2)]
                    [`(addq ,e1 ,e2) (_write-variables e2)]
                    [`(neq (var ,e1)) (set e1)]
                    [else (set)])))
         
         ; pass build-interference : x86* -> x86*
         (define/public (build-interference)
           (lambda (ast)
             (match ast
                    [`(program (,args ,live-afters ...) ,es ...)
                     `(program ,(list args (_helper es live-afters (make-graph '()))) ,@es)]
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
                                                              (lambda (v) (set-map (_write-variables `(movq ,s ,d))
                                                                                   (lambda (d) (cons d v))))))])
                         (_helper (cdr instructions) (cdr live-afters) (add-edges graph edges)))]
                      [`(addq ,s ,d)
                       (let* ([live-after-set (car live-afters)]
                              [vertices (set-subtract live-after-set (_write-variables `(addq ,s ,d)))]
                              [edges (flatten-list-1 (set-map vertices
                                                              (lambda (v) (set-map (_write-variables `(addq ,s ,d))
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
         
         ; pass allocate-registers : x86* -> x86*
         (define/public (allocate-registers var-reg-list)
           (lambda (ast)
             (define _var-reg-list->var-list
               (lambda (lst)
                 (cond [(null? lst) '()]
                       [else (cons (caar lst) (_var-reg-list->var-list (cdr lst)))])))
             (match ast
                    [`(program (,args ,graph) ,es ...)
                     (let* ([var-color-list ((color-graph) graph args)]
                            [var-register-list (map (lambda (v)
                                                      (cons (car v)
                                                            (color->register (cdr v))))
                                                    var-color-list)]
                            [reg-var-register-list (filter (lambda (v) (cdr v)) var-register-list)]
                            [reg-var-list (_var-reg-list->var-list reg-var-register-list)]
                            [stack-args (remove* reg-var-list args)])
                       `(program ,stack-args ,@(map (allocate-registers reg-var-register-list) es))
                       )]
                    [(? symbol?) ast]
                    [(? integer?) ast]
                    [`(var ,x)
                     (let* ([result (findf (lambda (v) (eq? (car v) x)) var-reg-list)])
                       (if result
                           `(reg ,(cdr result))
                         `(var ,x)
                         ))]
                    [`(,op ,es ...)
                     `(,op ,@(map (allocate-registers var-reg-list) es))]
                    [else ast]
                    )))
         
         ; Algorithm: DSATUR
         ; Input: a graph G
         ; Output: an assignment color[v] for each node v ∈ G
         ; W ← vertices(G)
         ; while W /= ∅ do
         ;   pick a node u from W with the highest saturation,
         ;     breaking ties randomly
         ;   find the lowest color c that is not in {color[v] : v ∈ adjacent(v)}
         ;   color[u] ← c
         ;   W ← W − {u}
         ;
         ; param `graph` an interference graph
         ; param `var-list` a list of all the variables in the program
         ; return a mapping of all variables to their colors
         ;
         (define/public (color-graph)
           (lambda (graph var-list)
             (define inner
               (lambda (graph var-color-info-hash var-list)
                 (if (null? var-list)
                     ; Convert to a mapping of all variables to their colors
                   (hash-map var-color-info-hash (lambda (var info) (cons var (car info))))
                   (begin
                    (let* ([current-var (highest-saturation-var var-color-info-hash var-list)]
                           [current-color (_new-color (saturation var-color-info-hash current-var))])
                      (set-color! var-color-info-hash current-var current-color)
                      (let ([adjacent-set (adjacent graph current-var)])
                        (if adjacent-set
                            (for/set ([adj (adjacent graph current-var)])
                                     (add-saturation-color! var-color-info-hash adj current-color))
                          #f))
                      (inner graph var-color-info-hash (set-remove var-list current-var)))))))
             (inner graph (make-color-info-hash (hash-keys graph)) var-list)
             ))
         
         (define _new-color
           (lambda (color-saturation-set)
             (define inner
               (lambda (color-saturation-set n)
                 (if (set-member? color-saturation-set n)
                     (inner color-saturation-set (+ n 1))
                   n)))
             (inner color-saturation-set 0)))
         
         ; Update this var's color and its adjacent color set
         ; var-color-info-hash: var => (color saturation-color-set)
         ; color: start from 0, -1 means no color set yet
         (define _update-color!
           (lambda (var color var-color-info-hash adjacent-graph)
             (let ([adjacent-set (adjacent adjacent-graph)])
               (set-color! var-color-info-hash var color)
               (for/set ([i adjacent-set])
                        (add-saturation-color! var-color-info-hash i color))
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
      ("allocate registers" ,(send compiler allocate-registers '()) ,interp-x86)
      ("assign homes" ,(send compiler assign-homes (dict-init)) ,interp-x86)
      ("patch instructions" ,(send compiler patch-instructions) ,interp-x86)
      ("print x86" ,(send compiler print-x86) #f)
      )))
