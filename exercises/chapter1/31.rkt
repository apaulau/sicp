#lang racket

(require rackunit)

;; a.  The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as 
;; higher-order procedures. Write an analogous procedure called product that returns the product of the values of a
;; function at points over a given range. Show how to define factorial in terms of product. Also use product to compute
;; approximations to pi using Wallis' product formula
;; b.  If your product procedure generates a recursive process, write one that generates an iterative process. If it 
;; generates an iterative process, write one that generates a recursive process.

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (inc k) (+ k 1))  
(define (identity x) x)

(define (factorial n)
  (product-iter identity 1 inc n))
  
(check-equal? (factorial 5) 120)

;; pi/2 = (/ (* 2 2 4 4 6 6 8 ...) (* 1 3 3 5 5 7 7))
(define (pi-product n)
  (define (term n)
    (define 2n (* 2 n)) 
    (* (/ n (- 2n 1)) (/ n (+ 2n 1))))
  (product-iter term 1 inc n))
  
(check-equal? (pi-product 2) (/ 4 45))