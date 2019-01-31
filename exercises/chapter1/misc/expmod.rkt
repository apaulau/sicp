#lang racket

;; Procedure that computes the exponential of a number modulo another number:

(require "./square.rkt")

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))
                    
(provide expmod)