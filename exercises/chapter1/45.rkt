#lang racket

(require rackunit)

;; We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y -> x/y does not
;; converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points
;; of the average-damped y -> x/y^2. Unfortunately, the process does not work for fourth roots -- a single average damp
;; is not enough to make a fixed-point search for y -> x/y^3 converge. On the other hand, if we average damp twice 
;; (i.e., use the average damp of the average damp of y -> x/y^3) the fixed-point search does converge. Do some 
;; experiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon
;; repeated average damping of y -> x/y^{n-1}. Use this to implement a simple procedure for computing nth roots using
;; fixed-point, average-damp, and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you 
;; need are available as primitives.

(require "./misc/fixed-point.rkt")
(require "./misc/average-damp.rkt")
(require "./43.rkt")

(define (pow a b)
  (exp (* b (log a))))

(define (nth-root-test n damp-count)
  (lambda (x) 
    (define (f y) (/ x (pow y (- n 1))))
    (fixed-point ((repeated average-damp damp-count) f) 1.0)))
  
((nth-root-test 2 1) 2)
((nth-root-test 3 1) 2)
((nth-root-test 4 2) 2)
((nth-root-test 5 2) 2)
((nth-root-test 6 2) 2)
((nth-root-test 7 2) 2)
((nth-root-test 8 3) 2)
((nth-root-test 16 4) 2) ; seems like we need to increase damp count on each nth power of 2

(define (log2 x) 
  (/ (log x) (log 2)))

(define (nth-root n)
  (lambda (x) 
    (define (f y) (/ x (pow y (- n 1))))
    (define damp-count (floor (log2 n)))
    (fixed-point ((repeated average-damp damp-count) f) 1.0)))
    
((nth-root 16) 2)
((nth-root 8) 256)