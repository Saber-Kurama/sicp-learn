#lang sicp

(define (p) (p))

(define (test x y) 
    (if (= x 0) 0 y))


(test 0 (p)) 

; 应用序 的话
; (test 0 (p)) 
; (p) 是一直无限执行
; 求值不会结束

; 正常序 执行的话
; (test 0 (p)) 
; (if (= 0 0) 0 (p))
; 求值结果为 0