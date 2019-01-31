#lang racket

;; The Fermat test is performed by choosing at random a number a between 1 and n-1 inclusive and checking whether the
;; remainder modulo n of the nth power of a is equal to a. The random number a is chosen using the procedure random,
;; which we assume is included as a primitive in Scheme. Random returns a nonnegative integer less than its integer input.
;; Hence, to obtain a random number between 1 and n-1, we call random with an input of n-1 and add 1 to the result

(require "./expmod.rkt")

(define (fermat-test n) 
  (define (try-it a) 
    (= (expmod a n n) a)) 
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
  (cond ((= times 0) true) 
        ((fermat-test n) (fast-prime? n (- times 1))) 
        (else false)))

(provide fast-prime?)