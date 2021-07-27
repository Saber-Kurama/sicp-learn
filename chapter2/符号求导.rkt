#lang sicp

; 求导的规则 
; 1 dc/dx = 0
; 2 dx / dx = 1
; 3 d(u+v) / dx = du / dx + dv /dx
; 4 d(uv) / dx = u(dv / dx) + v ( du / dx)

; 构造函数
; (define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

; 选择函数
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
; 基本函数
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))  

(define (deriv exp var)
 (cond ((number? exp) 0) ;第一条求导规则
       ((variable? exp) (if (same-variable? exp var) 1 0)) ;第二条求导规则
       ((sum? exp) (make-sum (deriv (addend exp) var) ;第三条求导规则
                              (deriv (augend exp) var)))
       ((product? exp) (make-sum ;第四条求导规则
                              (make-product (multiplier exp) 
                                            (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var) 
                                      (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))                                                    

 )
)

(deriv '(+ x 3) 'x) ; 求出的是(+ 1 0) 和有理数一样没有化简 后一种 1

; 有理数和 符号求导 都是采用了表 结构

; 分层设计
; 
