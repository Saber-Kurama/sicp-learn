#lang sicp

; 先假设 有 4 个选择函数 
; ; 取实部
; real-part
; ; 取虚部
; imag-part
; ; 取 模
; magnitude
; ; 取 角度
; angle
; 另个构造函数
; make-from-real-imag和make-from-mag-ang

; 实数的加减乘除
;  (define (add-complex z1 z2)
;   (make-from-real-imag 
;    (+ (real-part z1) (real-part z2))
;    (+ (imag-part z1) (imag-part z2))))

; (define (sub-complex z1 z2)
;   (make-from-real-imag 
;    (- (real-part z1) (real-part z2))
;    (- (imag-part z1) (imag-part z2))))

; (define (mul-complex z1 z2)
;   (make-from-mag-ang 
;    (* (magnitude z1) (magnitude z2))
;    (+ (angle z1) (angle z2))))

; (define (div-complex z1 z2)
;   (make-from-mag-ang 
;    (/ (magnitude z1) (magnitude z2))
;    (- (angle z1) (angle z2))))

; 系统中使用直角坐标表示
; 构造函数
(define (make-from-real-imag x y) (cons x y))
;; 由于采用直角坐标表示，所以如果传入模和幅角，要将其转换为实部和虚部
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))
; 选择函数
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
;; 由于采用直角坐标表示，所以可以通过三角关系转换为对应的模和幅角
(define (magnitude z)
  (sqrt (+ (square (real-part z)) 
           (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))


;系统使用极坐标表示

; 构造函数
(define (make-from-mag-ang r a) (cons r a))
;; 由于采用极坐标表示，所以如果传入实部和虚部，要将其转换为模和幅角
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
; 选择函数
(define (magnitude z) (car z))
(define (angle z) (cdr z))
;; 由于采用极坐标表示，所以可以通过三角关系转换为对应的实部和虚部
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

; 需要 不同的表示方法进行转换  

