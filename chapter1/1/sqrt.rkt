#lang sicp
(define (abs x)
    (if (< x 0) (- x) x)
)
( define (square x) (* x x))

; (define (good-enough guess x) (< (abs (- (square guess) x)) 0.001))

; (define (good-enough guess x) 
;    (= (improve guess x) guess))

 (define (good-enough guess x) 
  (< (abs (- (improve guess x) guess)) 
     (* guess .001)))

;  ;;Alternate version, which adds an "oldguess" variable to the main function. 
;  (define (sqrt-iter guess oldguess x) 
;    (if (good-enough? guess oldguess) 
;        guess 
;        (sqrt-iter (improve guess x) guess 
;                   x))) 
  
  
;  (define (good-enough? guess oldguess) 
;    (< (abs (- guess oldguess)) 
;       (* guess 0.001))) 
  
;  (define (sqrt x) 
;    (sqrt-iter 1.0 2.0 x)) 
     
(define (average x y)(/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))

(define (sqrt-iter guess x) (if (good-enough guess x) guess (sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))

(sqrt 2)
(sqrt 4)
(abs 2)
(sqrt 0.0001)
(sqrt 1000000000000)
(sqrt 10000000000000)