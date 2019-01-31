#lang racket/base

;; Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process for multiplying 
;; two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.

(require rackunit)

(define (double x) (+ x x))
(define (halve x) (/ x 2))
        
(define (fast-mult-iter a b)
  (define (iter a b c)
  (cond ((= b 0) c)
        ((even? b) (iter (double a) (halve b) c))
        (else (iter a (- b 1) (+ a c)))))
  (iter a b 0))
                                  
(check-equal? (fast-mult-iter 2 6) 12)
(check-equal? (fast-mult-iter 12 12) 144)
(check-equal? (fast-mult-iter 0 12) 0)
(check-equal? (fast-mult-iter 42 0) 0)
(check-equal? (fast-mult-iter 42 1) 42)
