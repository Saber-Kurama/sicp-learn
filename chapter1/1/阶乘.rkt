#lang sicp

; 线形递归的方案

(define (factorial num) 
    (if (= num 1) 
        num
        (* num (factorial (- num 1))) 
    )
)

(factorial 3)
(factorial 10)

; 线性迭代方案
; 节省空间

(define (factorial1 num) (factorial-iter 1 1 num))

(define (factorial-iter product counter max) 
    (if (> counter max) product 
        (factorial-iter (* product counter) (+ counter 1) max)
    )
)
(factorial1 3)
(factorial1 10)
