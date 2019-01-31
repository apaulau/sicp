#lang racket/base

;;  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(require rackunit)

(define (square x) (* x x))
(check-equal? (square 12) 144)

(define (sum-of-squares a b) (+ (square a) (square b)))
(check-equal? (sum-of-squares 3 4) 25)

(define (sum-of-greater-squares a b c) 
  (if (< b a)
      (if (< b c)
        (sum-of-squares a c)
        (sum-of-squares a b))
      (if (< a c)
        (sum-of-squares b c)
        (sum-of-squares b a))))
        
(check-equal? (sum-of-greater-squares 3 4 1) 25)
(check-equal? (sum-of-greater-squares 3 1 4) 25)
(check-equal? (sum-of-greater-squares 1 4 3) 25)
(check-equal? (sum-of-greater-squares 1 3 4) 25)
(check-equal? (sum-of-greater-squares 4 1 3) 25)
(check-equal? (sum-of-greater-squares 4 3 1) 25)

