#lang racket/base

;; Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

(require rackunit)

(define (pascal n k)
  (cond ((< n k) 0) ; out of triangle boundaries
        ((or (= n 0) (= k 0) (= n k)) 1)
        (else (+ (pascal (- n 1) (- k 1))
                 (pascal (- n 1) k)))))
                 
(check-equal? (pascal 5 2) 10)
