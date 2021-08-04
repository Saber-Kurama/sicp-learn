#lang sicp

; 求值环境模型

; ; 创建过程
;  (define (square x)
;   (* x x))
; ; 等价于
; (define square
;   (lambda (x) (* x x)))

(define (square x) (* x x))
(define (sum-of-squates x y) (+ (square x) (square y)))
(define (f a) (sum-of-squates (+ a 1) (* a 2)))
(f 5)