#lang racket

(require rackunit)

(define tolerance 0.00001)
(define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

;; Modify fixed-point so that it prints the sequence of approximations it generates, using the newline and display 
;; primitives shown in exercise 1.22. Then find a solution to x^x = 1000 by finding a fixed point of x -> log(1000)/log(x).
;; (Use Scheme's primitive log procedure, which computes natural logarithms.) Compare the number of steps this takes 
;; with and without average damping. (Note that you cannot start fixed-point with a guess of 1, as this would cause
;; division by log(1) = 0.)
(define (report x count)
  (display count)
  (display ": ")
  (display x)
  (newline))
  
(define (fixed-point f first-guess)
  (define (try guess iteration)
    (let ((next (f guess)))
      (report next iteration)
      (if (close-enough? guess next)
          next
          (try next (+ iteration 1)))))
  (try first-guess 1))

(define (root x)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               x))
          
(newline)
(display "without average damping applied")
(newline)     
(root 1.1)

(require "./misc/average.rkt")
(define (root-average-damping x)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               x))
               
(newline)
(display "with average damping applied")
(newline)
(root-average-damping 1.1)