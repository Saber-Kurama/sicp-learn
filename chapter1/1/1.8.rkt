#lang sicp

(define (s x) (+ 1 1))

(define (3root x) (3rt-iter 1.1 x))

(define (3rt-iter guess x) (if (good-enough? guess x) guess (3rt-iter (improve guess x) x)))

(define (good-enough? guess x) 
   (= (improve guess x) guess))

(define (improve guess x) 
   (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (square guess) 
   (* guess guess)) 


(3root 27)

