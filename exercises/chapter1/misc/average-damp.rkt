#lang racket

(require "./average.rkt")

(define (average-damp f)
  (lambda (x) (average x (f x))))

(provide average-damp)
  