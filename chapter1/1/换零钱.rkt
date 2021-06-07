#lang sicp
; 将总数a的现金换成n种硬币的方案的数目 等于
; 将现金数a 换成 除了第一种 银币的所有方案 加上
; 将现金数换成 a - b 换成所有硬币种类的所有方案， 其中 b 为 金币的价值

; 

; change(jine, zhonglei) = change(jine, zhonglei1) + change(jine - b, zhonglei)
; 1 1 = 
; 归约规则
; 1. 金额 a 是0     1
; 2. 金额 a 小于0    0
; 3. 种类 小于等于0   0

; 树形递归
(define (count-change amount)(cc amount 5))
; 1 5 10 25 50

(define (cc amount kinds) 
    (cond ((= amount 0 ) 1)
          ((or (< amount 0) (= kinds 0)) 0)
          (else
            (+ (cc amount (- kinds 1)) (cc (- amount (find kinds)) kinds))
          )
    )
)
(define (find kinds) 
    (cond ((= kinds 1) 1)
          ((= kinds 2) 5)
          ((= kinds 3) 10)
          ((= kinds 4) 25)
          ((= kinds 5) 50)
    )
)
(count-change 100)

; 动态规划 方案 就是文章说的表格技术

