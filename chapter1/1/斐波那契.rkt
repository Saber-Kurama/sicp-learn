#lang sicp

; 树形递归

(define (fib n)
    (if (= n 0) 0
        (if (= n 1) 1
           (+ (fib (- n 1)) (fib (- n 2))) 
        )
    )
)

(fib 5)

; 树形迭代

(define (fib1 n) (fib-iter 1 0 n))
(define (fib-iter a b count) 
    (if (= count 0) b
        (fib-iter (+ a b) a (- count 1))
    )
)

(fib1 5)