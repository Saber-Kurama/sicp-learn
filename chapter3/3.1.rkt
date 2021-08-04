#lang sicp
; (define balance 100)
; (define (withdraw amount)
;   (if (>= balance amount)
;       (begin (set! balance (- balance amount)))
;       "没有钱了"
;   )
; )

; (withdraw 50)
; balance

; 
; (define (make-withdraw balance)
;  (lambda (amount)
;   (if (>= balance amount)
;       (begin (set! balance (- balance amount)) balance)
;       "no"
;   ) 
;  )
; )
; (define W1 (make-withdraw 100))
; (define W2 (make-withdraw 100))

; (W1 50)

; ; 3.1 答案

; (define (make-accumulator amount)
;   ; (begin (set! amount ()))
;   (lambda (num)
;     (begin (set! amount (+ amount num)) amount) 
;   )
; )
; (define A (make-accumulator 5))
; (A 10)
; (A)

; 3.1.2 赋值带来的利益

; 随机数
; random-init 随机数的种子
; x2 = (rand-update x1)
; x3 = (rand-update x2)

; (define rand 
;   (let ((x random-init))
;     (lambda () (set! x (rand-update x)) x)
;   )
; )
; 如果直接用 rand-update 需要显示传递x

; 蒙特卡罗

; 执行此时
(define (estimate-pi trials)
 (sqrt / 6 (monte-carlo trials ceraro-test))
)
; GCD是否为1
(define (ceraro-test) (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trial-remaining trials-passed)
   (cond ((= trial-remaining 0) (/ trials-passed trials))
         ((experiment)
          (iter (- trial-remaining 1) (+  trials-passed 1))
         )
         (else 
          (iter (- trial-remaining 1) trials-passed)
         ) 
   )

  )
  (iter trials 0)
)
; 如果不使用 赋值 的话， 就需要传参给 保存参数 并传递 ceraro-test 无法将状态隐藏在局部转台中