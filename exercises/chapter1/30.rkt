#lang racket

(require rackunit)

;; Simpson's Rule is a more accurate method of numerical integration than the method illustrated above. Using 
;; The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed 
;; iteratively. Show how to do this by filling in the missing expressions in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc k) (+ k 1))  
(define (cube x) (* x x x))

(check-equal? (sum cube 1 inc 10) 3025)
