#lang racket

(define (runtime) 
  (current-milliseconds))

(require "./misc/square.rkt")
(require "./misc/smallest-divisor.rkt")

;; Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of 
;; time the system has been running (measured, for example, in microseconds). The following timed-prime-test procedure,
;; when called with an integer n, prints n and checks to see if n is prime. If n is prime, the procedure prints three
;; asterisks followed by the amount of time used in performing the test.

(define (prime? n)
  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))
(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)

;; Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in 
;; a specified range. Use your procedure to find the three smallest primes larger than 1000; larger than 10,000;
;; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has
;; order of growth of (n), you should expect that testing for primes around 10,000 should take about 10 times as long 
;; as testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000
;; support the n prediction? Is your result compatible with the notion that programs on your machine run in time 
;; proportional to the number of steps required for the computation?

(define (search-for-primes from count)
  (define (iter number count)
    (when (> count 0) 
      (if (timed-prime-test number) 
          (iter (+ number 2) (- count 1))
          (iter (+ number 2) count))))
  (iter (if (even? from) (+ from 1) from) count))

(search-for-primes 1000000000000 3)
(search-for-primes 10000000000000 3)
(search-for-primes 100000000000000 3)
(search-for-primes 1000000000000000 3)

