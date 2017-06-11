#lang racket

(require "interp.rkt")
(require "alist.rkt")

;; This exports r0-passes, defined below, to users of this file.
(provide r0-passes r0-select-instructions-pass)

; uniquify: R0 -> R0
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

; flatten: R0 -> C0
(define flatten
  (lambda (ast)
    (match ast
           [(? symbol?) (values ast '() '())]
           [(? integer?) (values ast '() '())]
           [`(let ([,x ,e]) ,body)
            (let-values ([(e-exp e-assigns e-tmp-vars) (flatten e)]
                         [(body-exp body-assigns body-tmp-vars) (flatten body)])
              (values body-exp
                      (append e-assigns (list `(assign ,x ,e-exp)) body-assigns)
                      (append e-tmp-vars (list x) body-tmp-vars)))]
           [`(program ,e)
            (let*-values ([(exp assigns tmp-vars) (flatten e)]
                          [(return) `(return ,exp)])
                         (values (append (list 'program) (list tmp-vars) assigns (list return))
                                 (append assigns (list return))
                                 tmp-vars))]
           [`(+ ,e1 ,e2)
            (let*-values ([(e1-exp e1-assigns e1-tmp-vars) (flatten e1)]
                          [(e2-exp e2-assigns e2-tmp-vars) (flatten e2)]
                          [(new-tmp-var) (gensym 'tmp.)]
                          [(new-assign) `(assign ,new-tmp-var (+ ,e1-exp ,e2-exp))])
                         (values new-tmp-var
                                 (append e1-assigns e2-assigns (list new-assign))
                                 (append e1-tmp-vars e2-tmp-vars (list new-tmp-var))))]
           [`(- ,e)
            (let*-values ([(exp assigns tmp-vars) (flatten e)]
                          [(new-tmp-var) (gensym 'tmp.)]
                          [(new-assign) `(assign ,new-tmp-var (- ,exp))])
                         (values new-tmp-var
                                 (append assigns (list new-assign))
                                 (append tmp-vars (list new-tmp-var))))]
           [`(read) (values `(read) '() '())]
           )))

(define flatten2
  (lambda (ast)
    (define-values (exps assigns tmp-vars) (flatten ast))
    exps))

; select-instructions: C0 -> X86
(define select-instructions
  (lambda (ast)
    (match ast
           [(? symbol?) `((var ,ast))]
           [(? integer?) `((int ,ast))]
           [`(assign ,x (+ (read) (read)))
            `((callq read_int) 
              (movq (reg rax) (var ,x)) 
              (callq read_int) 
              (addq (reg rax) (var ,x)))]
           [`(assign ,x (+ ,e1 (read)))
             `((movq ,@(select-instructions e1) (var ,x))
               (callq read_int)
               (addq (reg rax) (var ,x)))]
           [`(assign ,x (+ (read) ,e2))
             `((callq read_int)
               (movq (reg rax) (var ,x))
               (addq ,@(select-instructions e2) (var ,x)))]
           [`(assign ,x (+ ,e1 ,e2))
            (cond
             [(eq? x e1) `(addq ,(select-instructions e2) (var ,x))]
             [(eq? x e2) `(addq ,(select-instructions e1) (var ,x))]
             [else `((movq ,@(select-instructions e1) (var ,x)) (addq ,@(select-instructions e2) (var ,x)))])]
           [`(assign ,x (- (read)))
             `((callq read_int) (movq (reg rax) (var ,x)) (negq (var ,x)))]
           [`(assign ,x (- ,e))
             `((movq ,@(select-instructions e) (var ,x)) (negq (var ,x)))]
           [`(assign ,x (read))
             `((callq read_int) (movq (reg rax) (var ,x)))]
           [`(assign ,x ,e)
             `((movq ,@(select-instructions e) (var ,x)))]
           [`(return (read))
             `((callq read_int))]
           [`(return ,e)
             `((movq ,@(select-instructions e) (reg rax)))]
           [`(program ,args ,es ...)
             (let ([ins (flatten-list-1 (map select-instructions es))])
                 `(program ,args ,@ins))]
    )))

(define flatten-list-1
  (lambda (lst)
    (cond [(null? lst) '()]
          [(list? (car lst)) (append (car lst) (flatten-list-1 (cdr lst)))]
          [else (cons (car lst) (flatten-list-1 (cdr lst)))])))

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (or whatever file provides your passes)
;; should be named "compiler.rkt"
(define r0-passes
  `( ("uniquify" ,(uniquify (alist-init)) ,interp-scheme)
    ("flatten" ,flatten2 ,interp-C)
    ("select instructions" ,select-instructions ,interp-x86)
    ))

(define r0-select-instructions-pass
  `( ("select instructions" ,select-instructions ,interp-x86)
  ))
