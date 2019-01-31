#lang racket

(require rackunit)

;; A continued fraction representation of the tangent function was published in 1770 by the German mathematician J.H. Lambert:
;; Define a procedure (tan-cf x k), where x is in radians, that computes an approximation to the tangent function based 
;; on Lambert's formula. K specifies the number of terms to compute, as in exercise 1.37. 

(require "./37.rkt")
(require "./misc/square.rkt")
      
(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1) x (- (square x))))
                  (lambda (i) (- (* 2 i) 1))
                  k))
                       
(define (test x k)
  (display "(tan-cf ")
  (display x)
  (display " ")
  (display k)
  (display "): ")                       
  (tan-cf x k))

(test 10.0 10)      
(test 10.0 20)      
(test 10.0 50)      
