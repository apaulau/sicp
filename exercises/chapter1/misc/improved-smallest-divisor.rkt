#lang racket

(require "./square.rkt")

(define (smallest-divisor n)
  (define (next n) (if (= n 2) (+ n 1) (+ n 2)))
  (define (find-divisor n test-divisor)
    (define (divides? a b) (= (remainder b a) 0))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(provide smallest-divisor)