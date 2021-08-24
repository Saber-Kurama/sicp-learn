#lang sicp

; 定义 构造函数 和选择函数
(cons-stream x y)
(stream-car s)
(stream-cdr s)
; 约束条件
; (stream-car (cons-stream x y)) = x
; (stream-cdr (cons-stream x y)) = y

; 可识别的对象 the-empty-stream 绝不是 任何 cons-stream 的操作结果 类似 空表
; 使用 stream-null? 进行判断

; list-ref 取n数  
; 流的 stream-ref 类似的操作
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))
  )
)

; map 
; 流的 map

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))
  )
)

; for-each
(define (stream-for-each proc s)
  (if (stream-null?)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)) 
    )
  )
)


(define (display-stream s)
  (stream-for-each display-line s)
)

(define (display-line s)
  (newline)
  (display x)
)

; stream-cdr 取访问的时候才去 求值 ？？？ 不是在 cons-stream 构造的时候 求值 ？？？ 流的求值 是啥意思
; 对于常规表来说，其中的car和cdr都是在构造时求值，而对于流来说，其中cdr是在选取的时候才会求值

; 流实现将基于一种成为 delay 的特殊形式，对于(delay <exp>) 的求值 将不对 表达式 <exp> 求值，而是返回一个 延时对象 的对象
; 还有一个 force 的过程 ，它一个延时对象为参数，执行相应的求值工作

; 流的实现
; (const-stream <a> <b>) ---> (cons <a> (delay <b>))

; 
(define (stream-car stream)
  (car stream)
)
(define (stream-cdr stream)
  (force (cdr stream))
)

; 素数计算
(stream-car
  (stream-cdr
    (stream-filter prime? 
      (stream-enumerate-interval 1000 100000)
    )
  )
)

; enumerate-interval 类似
(define (stream-enumerate-interval low high)
  (if (> low high) the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))
  )
)
; 这样 (stream-enumerate-interval 10000 1000000) --> (cons 10000 (delay (stream-enumerate-interval 10001 10000000)))

; filter 过滤
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))
         )
        )
        (else (stream-filter pred (stream-cdr stream)))
  )
)
; 执行流程
; (cons 10000 (delay (stream-enumerate-interval 10001 10000000)))
; pred(10000) false
; (stream-filter pred (stream-cdr stream)) 这个时候  (stream-cdr stream) => (cons 10001 (delay (stream-enumerate-interval 10002 10000000))) 
; ...
; (cons 10007 (delay (stream-enumerate-interval 10008 10000000)))
; pred(10000) true
; 返回 一个新的流 (cons 10007 (stream-filter pred (stream-cdr stream)))
; 后面 stream-cdr 又会强迫 新的延时对象 求值
; stream-car 

; delay 和 force 的实现

; (deplay <exp>) 
; 实际上是 (lambda() <exp>) 的语法糖
(define (force delayed-object)
  (delayed-object)
)

; 保存求值 记忆功能
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result
          )
          result
      )
    )
  )
)

; (define (delay exp)
;   ()
; )
; (delay <exp>) === (memo-proc (lamba() <exp>))


; 无穷流

(define (integers-starting-from n)
  (cons-stream n (integers-staring-from (_ n 1)))
)
; 这是一个无穷的 生成 整数的流
(define integers (integers-starting-from 1))
; 可以通过 integers 生成其他的 无穷流

; 不能被7整除的整数流
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens (stream-fliter (lambda (x) (not (divisible? x 7))) intergers))
(stream-ref no-sevens 100)

; 隐式的定义流

; 定义为1 的无穷流
(define ones (cons-stream 1 ones))

; 两个流逐个元素之和
(define (add-streams s1 s2)
  (stream-map + s1 s2)
)

; (define integers (const-stream 1 (add-streams ones integers)))

; 可以用上面的风格 创建 斐波那契数
(define fibs (const-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))


; 构造素数
(define (sieve stream)
  (cons-stream (stream-car stream)
    (sieve (stream-filter (lambda (x) (not (divisible? x (stream-car stream))))(stream-cdr stream)))
  )
)

(define primes (sieve (integer-starting-from 2)))

; (define (stream-ref s n)
;   (if (= n 0)
;     (stream-car s)
;     (stream-ref (stream-cdr s) (- n 1))
;   )
; )

(stream-fef primes 10)
