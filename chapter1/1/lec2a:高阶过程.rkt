#lang sicp

(define (abs x)
    (if (< x 0) (- x) x)
)
( define (square x) (* x x))

; 求和的抽象 
; 例子中 过程作为参数.rkt
; 过程作为参数

(define (sum a term next b )
    (if (> a b) 0
        (+ (term a) (sum (next a) term next b))
    )
)

; 函数不动点
; f(根号x) = 根号x  y =  
(define (average x y)(/ (+ x y) 2))


; 函数不动点的实现
(define tolerance 0.00001)

(define (fixed-point f start)
    (define (close-enough? v1 v2)
            (< (abs (- v1 v2)) tolerance)
    )
    (define (try old new)
        (if (close-enough? old new)
            new
            (try new (f new))
        )
    )
    (try start (f start))
)
; 平均 阻尼 技术 过程作为返回值
(define (average-damp f)
    (lambda (x) (average x (f x)))
)
; (define (sqrt x) 
;     ; (fixed-point (lambda (y) (average y (/ x y))) 1)
;     (fixed-point (average-damp (lambda (y) (/ x y) )) 1)
; )
; (sqrt 4)

; 牛顿法
(define dx 0.00001)
(define (deriv g) 
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
)
(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x))))
)

(define (newtons-mothod g guess)
   (fixed-point (newton-transform g) guess)
)

(define (sqrt x)
    (newtons-mothod (lambda (y) (- (square y) x)) 1.0)
)

(sqrt 4)


; 抽象和第一级过程
(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess)
)

;阻尼方法

(define (sqrt1 x)
    (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1)
)
(sqrt1 4)

(define (sqrt2 x)
    (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform  1.0)
)
(sqrt2 4)

; 1. 可以用变量命名
; 2. 可以提供过程作为参数
; 3. 可以由过程作为结果返回
; 4. 可以包含在数据结构中