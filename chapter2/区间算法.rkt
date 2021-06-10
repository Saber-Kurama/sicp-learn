#lang sicp

; 区间算法

; 抽象一个数据 (最小值 最大值)

; 生成数据格式
(define (make-interval x y) (cons x y))
; 取出 最小值
(define (lower-bound x) (car x))
; 取出 最大值
(define (upper-bound x) (cdr x))

; 区间数据的 相加
(define (add-interval x y)
  (make-interval 
  	(+ (lower-bound x) (lower-bound y)) 
  	(+ (upper-bound x) (upper-bound y))
  )
)
; 区间 相乘
(define (mul-interval x y)
 (let ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))

   (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
 )
)
; 区间 除法
(define (div-interval x y)
  (mul-interval x
    (make-interval
	(/ 1.0 (upper-bound y))
	(/ 1.0 (lower-bound y))
    )
  )
)

(define a1 (make-interval 1 3))
(define a2 (make-interval 5 6))
(add-interval a1 a2)
(mul-interval a1 a2)
(div-interval a1 a2)