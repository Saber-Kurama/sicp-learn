#lang sicp

; 从 a + 到 b
; (define (sum-iter a b) 
;     (if (> a b) 0
;       (+ a (sum-iter (+ a 1) b))
;     )
; )

; 立方和
; (define (sum-cubes a b)
;     (
;         if(> a b) 0
;         (+ (cube a) (sum-cubes (+ a 1) b))
;     )
; )
; 1/(1*3) + 1/(5*7) + 1/(9*11) ...

; (define (sum-pi a b) 
;     (if(> a b) 0
;      (+ (/ 1 (* a (+ a 2))) (sum-pi (+ a 4) b))
;     )
; )

; 可以抽象成一个方法
(define (sum term a next b)
    (if (> a b) 0
     (+ (term a) (sum term (next a) next b) )
    )
)
; a -> b 的和

(define (sum-iter1 a b) 
    (sum (lambda (x) x) a (lambda (x) (+ x 1) ) b)
    ; 10
)

(sum-iter1 1 10)