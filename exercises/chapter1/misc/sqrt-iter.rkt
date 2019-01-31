#lang racket

(require rackunit)

(require "./abs.rkt")
(require "./average.rkt")
(require "./square.rkt")

(define (improve guess x)
  (average guess (/ x guess)))
  
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
                 
(define (sqrt x) (sqrt-iter 1.0 x))

(check-true (< (- 3 (sqrt 9)) 0.001))
(check-true (< (- 0.03 (sqrt 0.0009)) 0.001))

(provide sqrt)