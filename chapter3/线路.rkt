#lang sicp



(define (call-each s)
    'done
)

(define (make-wire)
    (let ((signal-value 0) (action-procedure '()))
        (define (set-my-signal! newvalue)
            (if (not (= signal-value newvalue))
                (begin (set! signal-value newvalue)
                       (call-each action-procedure))
                'done       
            )
        )
        (define (accept=action-procedure! proc)
            (set! action-procedure (cons proc action-procedure))
            (proc)
        )
        (define (dispatch m)
            (cond ((eq? m `get-signal) signal-value)
                  ((eq? m `set-signal!) set-my-signal!)
                  ((eq? m `add-action!) accept=action-procedure!)
                  (else (error "Unknow operaction" m))
            )
        )
        dispatch
    )
)

; 一些基本操作
; 基本功能模块
; 获取线的值
(define (get-signal wire)
    (wire 'get-signal)
)
; 设置线的值 （会执行 action）
(define (set-signal! wire value)
    ((wire 'set-signal!) value)
)
; 线添加一些事件
(define (add-action! wire action-procedure)
    ((wire 'add-action!) action-procedure)
)
; atfer-delay 延迟一段时间 执行 一个过程

(define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time the-agenda))
                    action
                    the-agenda))

; 返回新建的空待处理表
(make-agenda) 
; 判断待处理表是否为空
(empty-agenda? <agenda>) 
; 返回待处理表中第一个项
(first-agenda-item <agenda>) 
; 删除待处理表里的第一项
(remove-first-agenda-item! <agenda>) 
; 向待处理表中加入一 项，其意义是要求在给定时间运行的过程
(add-to-agenda! <time> <action> <agenda>) 
; 返回当前时间
(current-time <agenda>) 

(define (probe name wire)
    (add-action! wire
                 (lambda ()        
                    (newline)
                    (display name)
                    (display " ")
                    (display (current-time the-agenda))
                    (display "  New-value = ")
                    (display (get-signal wire)))))