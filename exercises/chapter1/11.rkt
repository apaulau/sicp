#lang racket/base

;; A function f is defined by the rule that f(n) = n if n < 3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n > 3. 
;; Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of 
;; an iterative process.

(require rackunit)

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (f (- n 2))
                 (f (- n 3))))))
                 
(define (f-iter n)
  (define (iter n1 n2 n3 count)
    (if (= count n)
        (+ n1 n2 n3)
        (iter n2 n3 (+ n1 n2 n3) (+ count 1))))
  (if (< n 3) n (iter 0 1 2 3)))
        
(check-equal? (f 1) 1)
(check-equal? (f 2) 2)
(check-equal? (f 3) 3)
(check-equal? (f 4) 6)

(check-equal? (f-iter 1) 1)
(check-equal? (f-iter 2) 2)
(check-equal? (f-iter 3) 3)
(check-equal? (f-iter 4) 6)
(check-equal? (f-iter 40) 20006521300)
