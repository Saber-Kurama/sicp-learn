#lang sicp

( define (square x) (* x x));

(define (fib n) (fib-iter 1 0 n))

(define (fib-iter a b count) 
    (if (= count 0) b
        (fib-iter (+ a b) a (- count 1))
    )
)


; 定义一个 枚举树的方法
(define (enum-tree tree) 
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enum-tree (car tree)) (enum-tree (cdr tree))))
  )
)

; (fib 5)

; 序列操作，高级抽象 数据流向  信号流图

; 计算给一棵树，计算所有值为奇数的叶子平方和

; 递归方式
(define ( sum-odd-squares tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) (if (odd? tree) (square tree)  0)) ; 是叶子节点话
          (else (+ (sum-odd-squares (car tree)) (sum-odd-squares (cdr tree)))) ; 递归添加
    )
)

; 这是一种 命令式的编程
; 声明式编程  信号流图

; 1. 枚举一棵树
; 2. 过滤其中奇数
; 3. 对每一个数进行 平方
; 4. 用 + 进行累加 从0开始

(define (sum-odd-squares1 tree) 
    (reduce + 0 (map square (filter odd? (enum-tree tree))))
    ; tree
)


(define x (cons (list 1 2) (list 2 3)))



; 所有偶数的斐波那契数列的一个表

; 命令式的编程

(define (even-fibs x)
  (define (next k)
    (if (> k x) nil 
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))
        ) 
      )
    )
  )
  (next 0)
)
; 声明式的编程的话

; 1. 枚举 整数 数组 0 --n
; 2. 每一个整数 斐波那契数列  数
; 3. 过滤是 不是整数的
; 4. 用 cons 累加 从 空表开始 

(define (even-fibs1 x)
  (reduce cons nil (filter even? (map fib (enum-inter 0 x))))
)

; (even-fibs 6)

; (map square (list 1 2 3 4))

; 定义一个 filter 的方法

(define (filter predicate sequence) 
  (cond ((null? sequence) nil)
        ((predicate (car sequence)) ; 如果过滤函数返回真
          (cons (car sequence) (filter predicate (cdr sequence)))
        )
        (else (filter predicate (cdr sequence)))
  )
)

; (filter odd? (list 1 2 3 4 5))

; 定义一个 累积 函数  reduce

(define (reduce op init sequence)
  (if (null? sequence) init
    (op (car sequence) (reduce op init (cdr sequence)))
  )
)
; (reduce + 0 (list 1 2 3 4))

; 定义一个实现的序列的方法
(define (enum-inter low high)
 (if (> low high) nil
   (cons low (enum-inter (+ low 1) high))
 )
)
; (enum-inter 2 5)




; (even-fibs 6)
; (even-fibs1 6)

(sum-odd-squares x)
(sum-odd-squares1 x)

; ()