#lang racket

(require rackunit)

(define tolerance 0.00001)
(define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Show that the golden ratio phi (section 1.2.2) is a fixed point of the transformation x -> 1 + 1/x, and use this fact
;; to compute phi by means of the fixed-point procedure.

(require "./misc/sqrt-iter.rkt")

(define phi (/ (+ 1 (sqrt 5)) 2))

(define (point x)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 
              x))
  
(check-true (close-enough? phi (point 1.0)))