#lang sicp

; 区间算法

; 抽象一个数据 (最小值 最大值)

; 生成数据格式
(define (make-interval x y) (cons x y))
; 取出 最小值
(define (lower-bound x) (min (car x) (cdr x)))
; 取出 最大值
(define (upper-bound x) (max (car x) (cdr x)))

; 区间数据的 相加
(define (add-interval x y)
  (make-interval 
  	(+ (lower-bound x) (lower-bound y)) 
  	(+ (upper-bound x) (upper-bound y))
  )
)
; 区间 相乘
; (define (mul-interval x y)
;  (let ((p1 (* (lower-bound x) (lower-bound y)))
;        (p2 (* (lower-bound x) (upper-bound y)))
;        (p3 (* (upper-bound x) (lower-bound y)))
;        (p4 (* (upper-bound x) (upper-bound y))))

;    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
;  )
; )
(define (mul-interval x y) 
   (define (positive? x) (>= x 0)) 
   (define (negative? x) (< x 0)) 
   (let ((xl (lower-bound x)) 
         (xu (upper-bound x)) 
         (yl (lower-bound y)) 
         (yu (upper-bound y))) 
     (cond ((and (positive? xl) (positive? yl)) 
            (make-interval (* xl yl) (* xu yu))) 
           ((and (positive? xl) (negative? yl)) 
            (make-interval (* xu yl) (* (if (negative? yu) xl xu) yu))) 
           ((and (negative? xl) (positive? yl)) 
            (make-interval (* xl yu) (* xu (if (negative? xu) yl yu)))) 
           ((and (positive? xu) (positive? yu)) 
            (let ((l (min (* xl yu) (* xu yl))) 
                  (u (max (* xl yl) (* xu yu)))) 
              (make-interval l u))) 
           ((and (positive? xu) (negative? yu)) 
            (make-interval (* xu yl) (* xl yl))) 
           ((and (negative? xu) (positive? yu)) 
            (make-interval (* xl yu) (* xl yl))) 
           (else 
            (make-interval (* xu yu) (* xl yl)))))) 
; 区间 除法
; (define (div-interval x y)
;   (mul-interval x
;     (make-interval
; 	(/ 1.0 (upper-bound y))
; 	(/ 1.0 (lower-bound y))
;     )
;   )
; )
(define (div-interval x y) 
   (if (<= (* (lower-bound y) (upper-bound y)) 0) 
       (error "Division error (interval spans 0)" y)
       (mul-interval x  
                     (make-interval (/ 1. (upper-bound y)) 
                                    (/ 1. (lower-bound y)))))) 
; 2.8 减法
(define (sub-interval x y) 
   (make-interval (- (lower-bound x) (upper-bound y)) 
                  (- (upper-bound x) (lower-bound y)))) 

(define a1 (make-interval 1 3))
(define a2 (make-interval 5 6))
(add-interval a1 a2)
(mul-interval a1 a2)
(div-interval a1 a2)
(sub-interval a1 a2)
(sub-interval (make-interval 4 (- 5)) (make-interval 1 2))

