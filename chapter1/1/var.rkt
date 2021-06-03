#lang sicp

; 定义变量
(define size 5)
size

; 复合操作 
; 基本数据和过程
; 嵌套过程
; 定义

( define (square x) (* x x))
(square 10)
(square (square 10) )

; (define (square12 y) (square y))
; (square12 3)

; 条件表达式和谓词
; (define (abs x)
;     (
;         cond ((> x 0) x)
;              ((< x 0) (- x))
;              ((= x 0) 0)
;     )
; )

; (define (abs x) (
;     cond((< x 0) (- x))
;     (else x)
; ))

(define (abs x)
    (if (< x 0) (- x) )
)
(abs -10)
(abs 0)

; (and <e>)
; (or <e>)
; (not <e>)

; 常规过程和特殊形式的区别：
; 特殊形式具有自己的求值规则，而常规过程采用的是应用序求值或正则序求值顺序。
; 如果将特殊形式定义为常规过程，就需要按照常规过程的求值方法进行求值，可能会出错

(define (iff <p> <c> <a>) (if <p> <c> <a>))

(define (tryif a) (if (= a 0) 1 (/ 1 0)))

(define (tryiff a) (iff (= a 0) 1 (/ 1 0)))
; if 是特殊形式 会先计算 (= a 0)
; (tryif 0)
; (tryif 1)

; 出错的原因 是 iff 是 应用序 
(tryiff 0)
(tryiff 1)

; 综上：对各种表达式的求值规则可以描述为一个简单的通用规则和一组针对不多的特殊形式的专门规则。


