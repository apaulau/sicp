#lang racket

(require rackunit)

;; Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the
;; original procedure twice. For example, if inc is a procedure that adds 1 to its argument, then (double inc) should be
;; a procedure that adds 2. What value is returned by (((double (double double)) inc) 5)

(define (double f)
  (lambda (x) (f (f x))))
  
(define (inc x)
  (+ x 1))
  
(check-equal? ((double inc) 3) 5)
(check-equal? (((double (double double)) inc) 5) 21)