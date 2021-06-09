#lang sicp

; make-rat number denom

; 有理数的加法
(define (add-rat x y)
    (make-rat 
        (+ (* (number x) (denom y)) (* (number y) (denom x))) 
        (* (denom x) (denom y))
    )
)
; 有理数的减法
(define (sub-rat x y)
    (make-rat
       (- (* (number x) (denom y)) (* (number y) (denom x)))
       (* (denom x) (denom y))
    )
)
; 有理数的乘法
(define (mul-rat x y)
    (make-rat 
        (* (number x) (number y))
        (* (denom x) (denom y))
    )
)
; 有理数的除法
(define (equal-rat x y)
    (make-rat 
        (* (number x) (denom y))
        (* (number y) (denom x))
    )
)
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)) 
    )
)
; 生成有理数
; (define (make-rat x y) (cons x y))
(define (make-rat x y)
    (let ( 
            (g (gcd x y))
            (x1 (if (or (and (> x 0) (< y 0)) (and (< x 0) (< y 0))) (- x) x ))
            (y1 (if (< y 0) (- y) y ))
            
         )
     (cons (/ x1 g )  (/ y1 g ))
    )
)
(make-rat 5 -10)
(make-rat 5 10)
; 获取有理数的分子
(define (number x) (car x))
; 获取有理数的分母
(define (denom x) (cdr x))

(define n1 (cons 3 5))
(define n2 (cons 3 6))
(add-rat n1 n2)
(sub-rat n1 n2)
(mul-rat n1 n2)
(equal-rat n1 n2)

