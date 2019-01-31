#lang racket

(require rackunit)

;; You can obtain an even more general version of accumulate (exercise 1.32) by introducing the notion of a filter on 
;; the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified
;; condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an
;; additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to
;; express the following using filtered-accumulate:
;; a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate 
;; already written)
;; b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers
;; i < n such that GCD(i,n) = 1).

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
                (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (filtered-accumulate-iter filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (if (filter a) (term a) null-value) result))))
  (iter a null-value))

(define (inc k) (+ k 1))  
(define (identity x) x)

(define (even-product-iter a b) ; multiply only even numbers from a to b
  (filtered-accumulate-iter even? * 1 identity a inc b))
  
(check-equal? (even-product-iter 1 10) 3840)

(require "./misc/square.rkt")
(require "./misc/improved-smallest-divisor.rkt")
(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-of-prime-squares a b)
 (filtered-accumulate prime? + 0 square a inc b))
 
(check-equal? (sum-of-prime-squares 3 10) 83)
(check-equal? (sum-of-prime-squares 4 10) 74)
(check-equal? (sum-of-prime-squares 5 10) 74)

(define (product-of-related-primes n)
  (define (gcd a b)
    (if (= b 0)
      a
      (gcd b (remainder a b))))
  (define (filter x) (= (gcd x n) 1))      
  (filtered-accumulate filter * 1 identity 1 inc n))
  
(check-equal? (product-of-related-primes 3) 2)
(check-equal? (product-of-related-primes 4) 3)
(check-equal? (product-of-related-primes 5) 24)
(check-equal? (product-of-related-primes 6) 5)