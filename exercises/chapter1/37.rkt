#lang racket

(require rackunit)

;; a. As an example, one can show that the infinite continued fraction expansion with the Ni and the Di all equal to 1
;; produces 1/phi, where phi is the golden ratio (described in section 1.2.2). One way to approximate an infinite continued
;; fraction is to truncate the expansion after a given number of terms. Such a truncation -- a so-called k-term finite 
;; continued fraction. Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di 
;; of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes
;; the value of the k-term finite continued fraction. Check your procedure by approximating 1/phi using

;;(cont-frac (lambda (i) 1.0)
;;           (lambda (i) 1.0)
;;           k)

;; for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal
;; places?
;; b. If your cont-frac procedure generates a recursive process, write one that generates an iterative process. If it 
;; generates an iterative process, write one that generates a recursive process.

(require "./misc/sqrt-iter.rkt")
(define phi (/ (+ 1 (sqrt 5)) 2))

(define (cont-frac n d k)
  (define (recurse i)
    (if (= i k)
        (/ (n k) 
           (d k))
        (/ (n i) 
           (+ (d i) 
              (recurse (+ i 1))))))
  (recurse 1))
  
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) 
                         (+ (d i) result)))))
  (iter k (/ (n k) (d k))))

(define tolerance 10000)
(define (close-enough? v1 v2)
    (= (truncate (* v1 tolerance)) (truncate (* v2 tolerance))))
             
(check-true (close-enough? (/ 1 phi) 
               (cont-frac (lambda (i) 1.0)
                          (lambda (i) 1.0)
                          11)))

(check-true (close-enough? (/ 1 phi) 
            (cont-frac-iter (lambda (i) 1.0)
                            (lambda (i) 1.0)
                            10)))
                          
(provide cont-frac)
(provide cont-frac-iter)