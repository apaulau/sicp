#lang racket

(require "./fixed-point.rkt")
(require "./deriv.rkt")

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
  
(provide newtons-method)