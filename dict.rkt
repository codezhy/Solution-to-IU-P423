#lang racket

(provide dict-init dict-add dict-lookup)

(define dict-init 
  (lambda () '()))

(define dict-add
  (lambda (key value lst)
    (cons (cons key value) lst)))

(define dict-lookup
  (lambda (name lst)
    (if (null? lst)
      #f
      (let* [(kv (car lst))
             (key (car kv))
             (value (cdr kv))]
        (if (eq? name key)
          value
          (dict-lookup name (cdr lst)))))))