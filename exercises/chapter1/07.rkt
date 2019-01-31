#lang racket

(require rackunit)

;; The good-enough? test used in computing square roots will not be very effective for finding the square roots of very
;; small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision.
;; This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test
;; fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess 
;; changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design 
;; a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?

(require "./misc/abs.rkt")
(require "./misc/average.rkt")
(require "./misc/square.rkt")

(define (improve guess x)
  (average guess (/ x guess)))
  
(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
                 
(define (sqrt x) (sqrt-iter 1.0 x))

(check-true (< (- 3 (sqrt 9)) 0.001))
(check-true (< (- 0.03 (sqrt 0.0009)) 0.001))
