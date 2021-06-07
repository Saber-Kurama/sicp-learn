#lang sicp
; n < 3 f(n) = n  
; n >= 3  f(n) = f(n-1) + f(n - 2) + f(n - 3)

; 递归计算方案

(define (f n) 
    (if (< n 3) n
     (+ (f (- n 1)) (f (- n 2)) (f (- n 3)) ) 
    ) 
)
(f 0)
(f 1)
(f 2)
(f 3)
(f 5)

; 迭代计算方案

(define (f1 n) 
    (if (< n 3) n
      (f-iter 2 1 0 (- n 2))
    )
)
(define (f-iter n3 n2 n1 count) 
    (if (= count 0) n3
        (f-iter (+ n3 n2 n1) n3 n2 (- count 1))
    )
)

(f1 0)
(f1 1)
(f1 2)
(f1 3)
(f1 5)