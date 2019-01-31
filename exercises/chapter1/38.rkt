#lang racket

(require rackunit)

;; In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a 
;; continued fraction expansion for e - 2, where e is the base of the natural logarithms. In this fraction, the N_i are
;; all 1, and the Di are successively 1,2,1,1,4,1,1,6,1,1,8,... . Write a program that uses your cont-frac procedure
;; from exercise 1.37 to approximate e, based on Euler's expansion.

(require "./37.rkt")
      
(define (e k)
  (+ 2 (cont-frac-iter (lambda (i) 1.0)
                       (lambda (i) (if (= (remainder (+ i 1) 3) 0)
                                      (* 2 (/ (+ i 1) 3))
                                      1))
                       k)))
                       
(define (test k)
  (display k)
  (display ": ")                       
  (e k))

(test 10)      
(test 20)      
(test 50)      
