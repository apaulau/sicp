#lang racket

;; The smallest-divisor procedure shown at the start of this section does lots of needless testing: After it checks
;; to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even
;; numbers. This suggests that the values used for test-divisor should not be 2, 3, 4, 5, 6, ..., but
;; rather 2, 3, 5, 7, 9, .... To implement this change, define a procedure next that returns 3 if its input is 
;; equal to 2 and otherwise returns its input plus 2. Modify the smallest-divisor procedure to use (next test-divisor)
;; instead of (+ test-divisor 1). With timed-prime-test incorporating this modified version of smallest-divisor, run
;; the test for each of the 12 primes found in exercise 1.22. Since this modification halves the number of test steps,
;; you should expect it to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio
;; of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?

(define (runtime) 
  (current-milliseconds))

(require "./misc/square.rkt")
(require "./misc/improved-smallest-divisor.rkt")

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

