#lang racket

(require "interp.rkt")
(require "dict.rkt")
(require "utilities.rkt")

;; This exports r0-passes, defined below, to users of this file.
(provide r0-compiler r0-passes)

(define r0-compiler
  (class object%
         
         (super-new)

         ; Pass uniquify: R0 -> R0
         (define/public (uniquify env)
           (lambda (ast)
             (match ast
                    [(? symbol?)
                     (let [(result (dict-lookup ast env))]
                       (if result
                           result
                         ast))]
                    [(? integer?) ast]
                    [`(let ([,x ,e]) ,body)
                     (let* [(new-x (gensym x))
                            (new-env (dict-add x new-x env))]
                       `(let ([,new-x ,((uniquify new-env) e)]) ,((uniquify new-env) body)))]
                    [`(program ,e)
                     `(program ,((uniquify env) e))]
                    [`(,op ,es ...)
                     `(,op ,@(map (uniquify env) es))]
                    )))
         
         ; Pass flatten: R0 -> C0
         (define/public (flatten)
           (lambda (ast)
             (match ast
                    [(? symbol?) (values ast '() '())]
                    [(? integer?) (values ast '() '())]
                    [`(let ([,x ,e]) ,body)
                     (let-values ([(e-exp e-assigns e-tmp-vars) ((flatten) e)]
                                  [(body-exp body-assigns body-tmp-vars) ((flatten) body)])
                       (values body-exp
                               (append e-assigns (list `(assign ,x ,e-exp)) body-assigns)
                               (append e-tmp-vars (list x) body-tmp-vars)))]
                    [`(program ,e)
                     (let*-values ([(exp assigns tmp-vars) ((flatten) e)]
                                   [(return) `(return ,exp)])
                                  (values (append (list 'program) (list tmp-vars) assigns (list return))
                                          (append assigns (list return))
                                          tmp-vars))]
                    [`(+ ,e1 ,e2)
                     (let*-values ([(e1-exp e1-assigns e1-tmp-vars) ((flatten) e1)]
                                   [(e2-exp e2-assigns e2-tmp-vars) ((flatten) e2)]
                                   [(new-tmp-var) (gensym 'tmp.)]
                                   [(new-assign) `(assign ,new-tmp-var (+ ,e1-exp ,e2-exp))])
                                  (values new-tmp-var
                                          (append e1-assigns e2-assigns (list new-assign))
                                          (append e1-tmp-vars e2-tmp-vars (list new-tmp-var))))]
                    [`(- ,e)
                     (let*-values ([(exp assigns tmp-vars) ((flatten) e)]
                                   [(new-tmp-var) (gensym 'tmp.)]
                                   [(new-assign) `(assign ,new-tmp-var (- ,exp))])
                                  (values new-tmp-var
                                          (append assigns (list new-assign))
                                          (append tmp-vars (list new-tmp-var))))]
                    [`(read) (values `(read) '() '())]
                    )))
         
         (define/public (flatten2)
           (lambda (ast)
             (define-values (exps assigns tmp-vars) ((flatten) ast))
             exps))
         
         ; Pass select-instructions: C0 -> X86*
         (define/public (select-instructions)
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
                     `((movq ,@((select-instructions) e1) (var ,x))
                       (callq read_int)
                       (addq (reg rax) (var ,x)))]
                    [`(assign ,x (+ (read) ,e2))
                     `((callq read_int)
                       (movq (reg rax) (var ,x))
                       (addq ,@((select-instructions) e2) (var ,x)))]
                    [`(assign ,x (+ ,e1 ,e2))
                     (cond
                      [(eq? x e1) `(addq ,((select-instructions) e2) (var ,x))]
                      [(eq? x e2) `(addq ,((select-instructions) e1) (var ,x))]
                      [else `((movq ,@((select-instructions) e1) (var ,x)) (addq ,@((select-instructions) e2) (var ,x)))])]
                    [`(assign ,x (- (read)))
                     `((callq read_int) (movq (reg rax) (var ,x)) (negq (var ,x)))]
                    [`(assign ,x (- ,e))
                     `((movq ,@((select-instructions) e) (var ,x)) (negq (var ,x)))]
                    [`(assign ,x (read))
                     `((callq read_int) (movq (reg rax) (var ,x)))]
                    [`(assign ,x ,e)
                     `((movq ,@((select-instructions) e) (var ,x)))]
                    [`(return (read))
                     `((callq read_int))]
                    [`(return ,e)
                     `((movq ,@((select-instructions) e) (reg rax)))]
                    [`(program ,args ,es ...)
                     (let ([ins (flatten-list-1 (map (select-instructions) es))])
                       `(program ,args ,@ins))]
                    )))

         (define/public (flatten-list-1 lst)
             (cond [(null? lst) '()]
                   [(list? (car lst)) (append (car lst) (flatten-list-1 (cdr lst)))]
                   [else (cons (car lst) (flatten-list-1 (cdr lst)))]))
         
         ; Pass assign-homes: x86* -> x86*
         (define/public (assign-homes env)
           (lambda (ast)
             (match ast
                    [`(var ,x)
                     (let ([value (dict-lookup x env)])
                       (if value
                           value
                         (error 'assign-homes "can't assign home for ~a\n" x)))]
                    [`(movq ,e1 ,e2)
                     `(movq ,((assign-homes env) e1) ,((assign-homes env) e2))]
                    [`(addq ,e1 ,e2)
                     `(addq ,((assign-homes env) e1) ,((assign-homes env) e2))]
                    [`(negq ,e)
                     `(negq ,((assign-homes env) e))]
                    [`(program ,args ,es ...)
                     `(program ,(length args) ,@(map (assign-homes (get-variable-to-homes-map args)) es))]
                    [else ast]
                    )))
         
         (define get-variable-to-homes-map
           (lambda (variable-lst)
             (define inner
               (lambda (variable-lst offset)
                 (cond [(null? variable-lst) '()]
                       [else (dict-add (car variable-lst) `(deref rbp ,offset)
                                       (inner (cdr variable-lst) (+ offset 8)))])))
             (inner variable-lst (* (length variable-lst) -8))))
         
         ; Pass patch-instructions: x86* -> x86
         (define/public (patch-instructions)
           (lambda (ast)
             (match ast
                    [`(movq (deref rbp ,offset1) (deref rbp ,offset2))
                     `((movq (deref rbp ,offset1) (reg rax)) (movq (reg rax) (deref rbp ,offset2)))]
                    [`(addq (deref rbp ,offset1) (deref rbp ,offset2))
                     `((movq (deref rbp ,offset1) (reg rax)) (addq (reg rax) (deref rbp ,offset2)))]
                    [`(program ,arg-num ,es ...)
                     (let ([ins (flatten-list-1 (map (patch-instructions) es))])
                       `(program ,arg-num ,@ins))]
                    [else `(,ast)]
                    )))
         
         ; Pass print-x86: x86 -> x86†
         (define/public (print-x86)
           (lambda (ast)
             (match ast
                    [`(int ,e) (format "$~a" e)]
                    [`(reg ,e) (format "%~a" e)]
                    [`(deref rbp ,offset) (format "~a(%rbp)" offset)]
                    [`(movq ,e1 ,e2) (_s8 (format "movq ~a, ~a\n" ((print-x86) e1) ((print-x86) e2)))]
                    [`(addq ,e1 ,e2) (_s8 (format "addq ~a, ~a\n" ((print-x86) e1) ((print-x86) e2)))]
                    [`(negq ,e) (_s8 (format "negq ~a\n" ((print-x86) e)))]
                    [`(callq ,e)
                     (_s8 (format "callq ~a\n" (_proc-name e)))]
                    [`(program ,arg-num ,es ...)
                     (let ([size (_stack-size arg-num)])
                       (string-append (_s8 (format ".globl ~a\n" (_proc-name "main")))
                                      (format "~a:\n" (_proc-name "main"))
                                      (_s8 "pushq %rbp\n")
                                      (_s8 "movq %rsp, %rbp\n")
                                      (if (= size 0) "" (_s8 (format "subq $~a, %rsp\n" size)))
                                      "\n"
                                      (_string-list-append (map (print-x86) es))
                                      "\n"
                                      (_s8 "movq %rax, %rdi\n")
                                      (_s8 (format "callq ~a\n" (_proc-name "print_int")))
                                      (if (= size 0) "" (_s8 (format "addq $~a, %rsp\n" size)))
                                      (_s8 "popq %rbp\n")
                                      (_s8 "retq")))]
                    )))
         
         (define _s8
           (lambda (str)
             (format "        ~a" str)))
         
         (define _proc-name
           (lambda (name)
             (cond [(eq? (system-type 'os) 'macosx) (format "_~a" name)]
                   [else (format "~a" name)])))
         
         (define _stack-size
           (lambda (arg-num)
             (cond [(eq? (system-type 'os) 'macosx) (* (ceiling (/ arg-num 2)) 16)]
                   [else (* arg-num 8)])))
         
         (define _string-list-append
           (lambda (lst)
             (if (null? lst)
                 ""
               (string-append (car lst) (_string-list-append (cdr lst))))))
         ))

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (or whatever file provides your passes)
;; should be named "compiler.rkt"
(define r0-passes
  (let ([compiler (new r0-compiler)])
  `( ("uniquify" ,(send compiler uniquify '((dict-init))) ,interp-scheme)
    ("flatten" ,(send compiler flatten2) ,interp-C)
    ("select instructions" ,(send compiler select-instructions) ,interp-x86)
    ("assign homes" ,(send compiler assign-homes (dict-init)) ,interp-x86)
    ("patch instructions" ,(send compiler patch-instructions) ,interp-x86)
    ("print x86" ,(send compiler print-x86) #f)
    )))
