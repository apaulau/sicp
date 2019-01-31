#lang racket

;; Simpson's Rule is a more accurate method of numerical integration than the method illustrated above. Using 
;; Simpson's Rule Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, 
;; computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000), 
;; and compare the results to those of the integral procedure shown above.

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
     
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (g k)
    (define (c k) 
      (cond ((or (= k 0) (= k n)) 1)
            ((even? k) 2)
            (else 4)))
    (* (c k) (f (+ a (* k h)))))
  (define (inc k) (+ k 1))
  (* (/ h 3) (sum g 0 inc n)))
  
  (define (cube x) (* x x x))
  
  (display "reactangular")
  (newline)
  (integral cube 0 1 0.01)
  (integral cube 0 1 0.001)
  
  (display "simpson")
  (newline)
  (simpson-integral cube 0 1 100)
  (simpson-integral cube 0 1 1000)