#lang racket

(require rackunit)

;; Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the Fermat test. That is, write
;; a procedure that takes an integer n and tests whether a^n is congruent to a modulo n for every a<n, and try your
;; procedure on the given Carmichael numbers.

(require "./misc/square.rkt")
(require "./misc/expmod.rkt")

(define (test-one a n) 
  (= (expmod a n n) a))
  
(define (test-every n)
  (define (test from to)
    (if (< from to)
      (if (test-one from to)
          (test (+ from 1) to)
          false)
      true))
  (test 1 n))
  
(check-equal? (test-every 100) #f)
(check-equal? (test-every 109) #t)

;; Carmichael numbers test
(check-equal? (test-every 561) #t)
(check-equal? (test-every 1105) #t)
(check-equal? (test-every 1729) #t)
(check-equal? (test-every 2465) #t)
(check-equal? (test-every 2821) #t)
(check-equal? (test-every 6601) #t)
