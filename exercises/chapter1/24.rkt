#lang racket

;; Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the Fermat method), and test each
;; of the 12 primes you found in that exercise. Since the Fermat test has (log n) growth, how would you expect 
;; the time to test primes near 1,000,000 to compare with the time needed to test primes near 1000? Do your data
;; bear this out? Can you explain any discrepancy you find?

(define (runtime) 
  (current-milliseconds))

(require "./misc/square.rkt")
(require "./misc/smallest-divisor.rkt")
(require "./misc/fast-prime.rkt")

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 100000)
      (report-prime n (- (runtime) start-time))
      false))
(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)

(define (search-for-primes from count)
  (define (iter number count)
    (when (> count 0) 
      (if (timed-prime-test number) 
          (iter (+ number 2) (- count 1))
          (iter (+ number 2) count))))
  (iter (if (even? from) (+ from 1) from) count))
  
  
(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)