#lang racket

(require "interp.rkt")
(require "dict.rkt")
(require "utilities.rkt")

;; This exports r0-passes, defined below, to users of this file.
(provide r0-passes r0-select-instructions-pass)

; Pass uniquify: R0 -> R0
(define (uniquify env)
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

; Pass select-instructions: C0 -> X86*
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

; Pass assign-homes: x86* -> x86*
(define (assign-homes env)
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
(define patch-instructions
  (lambda (ast)
    (match ast
           [`(movq (deref rbp ,offset1) (deref rbp ,offset2))
            `((movq (deref rbp ,offset1) (reg rax)) (movq (reg rax) (deref rbp ,offset2)))]
           [`(addq (deref rbp ,offset1) (deref rbp ,offset2))
            `((movq (deref rbp ,offset1) (reg rax)) (addq (reg rax) (deref rbp ,offset2)))]
           [`(program ,arg-num ,es ...)
            (let ([ins (flatten-list-1 (map patch-instructions es))])
              `(program ,arg-num ,@ins))]
           [else `(,ast)]
           )))

; Pass print-x86: x86 -> x86†
(define print-x86
  (lambda (ast)
    (match ast
           [`(int ,e) (format "$~a" e)]
           [`(reg ,e) (format "%~a" e)]
           [`(deref rbp ,offset) (format "~a(%rbp)" offset)]
           [`(movq ,e1 ,e2) (_s8 (format "movq ~a, ~a\n" (print-x86 e1) (print-x86 e2)))]
           [`(addq ,e1 ,e2) (_s8 (format "addq ~a, ~a\n" (print-x86 e1) (print-x86 e2)))]
           [`(negq ,e) (_s8 (format "negq ~a\n" (print-x86 e)))]
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
                             (_string-list-append (map print-x86 es))
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

; pass uncover-live : x86* -> x86*
(define uncover-live
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
(define build-interference
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


;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (or whatever file provides your passes)
;; should be named "compiler.rkt"
(define r0-passes
  `( ("uniquify" ,(uniquify (dict-init)) ,interp-scheme)
    ("flatten" ,flatten2 ,interp-C)
    ("select instructions" ,select-instructions ,interp-x86)
    ("uncover lives" ,uncover-live ,interp-x86)
    ("build interference" ,build-interference ,interp-x86)
    ;("assign homes" ,(assign-homes (dict-init)) ,interp-x86)
    ;("patch instructions" ,patch-instructions ,interp-x86)
    ;("print x86" ,print-x86 #f)
    ))

(define r0-select-instructions-pass
  `( ("select instructions" ,select-instructions ,interp-x86)
    ))
