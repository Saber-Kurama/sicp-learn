#lang sicp

; 模式匹配
; 基于规则的代换

; [【SICP归纳】4 模式匹配和以规则为基础的代换](https://developer.aliyun.com/article/1119)
; [SICP 计算机程序的构造与解释笔记](https://yinode.tech/post/201711/sicp%E8%AE%A1%E7%AE%97%E6%9C%BA%E7%A8%8B%E5%BA%8F%E7%9A%84%E6%9E%84%E9%80%A0%E4%B8%8E%E8%A7%A3%E9%87%8A%E7%AC%94%E8%AE%B0/#%E6%A8%A1%E5%BC%8F%E5%8C%B9%E9%85%8D)
; [运行基于SICP模式匹配规则的替代码](https://www.thinbug.com/q/6970006)
; [使用 MIT Scheme 完成 SICP 模式匹配和基于规则的替换讲座](http://rajiv.sg/blog/2012/07/01/sicp-pattern-matching-and-rule-based-substitution-lecture-with-mit-scheme/)

; 作者写了关于微积分的演算规则的程序. 这是一个很程序化的程序，我们所做的是在讲这些（数学）规则翻译成计算机语言。
; 这些规则都具有左右两侧， 左侧是想要采取的导数表达式，右侧则是其替代

; 用可以匹配的模式，和能够代换的框架，可以得到新的表达式 
; 意味着模式是对源表达式的匹配，并且规则的应用的结果是去产生一个新的表达式，而这个表达式是通过实例化一个框架来传入的目标参数。这就是所谓的实例化。而整体范围上这就是由规则所描述的过程。

; 定义一个求导规则
; ? 代表求导 ：代表 要代换的对象， “骨架求值”
(define deriv-rules
  '(
    ( (dd (?c c) (? v))              0                                 )
    ( (dd (?v v) (? v))              1                                 )
    ( (dd (?v u) (? v))              0                                 )
    ( (dd (+ (? x1) (? x2)) (? v))   (+ (dd (: x1) (: v))
                                        (dd (: x2) (: v)))             )
    ( (dd (* (? x1) (? x2)) (? v))   (+ (* (: x1) (dd (: x2) (: v)))
                                        (* (dd (: x1) (: v)) (: x2)))  )
    ( (dd (** (? x) (?c n)) (? v))   (* (* (: n) (+ (: x) (: (- n 1))))
                                        (dd (: x) (: v)))              )
    ))

; 匹配模式的问题
; foo  ---》 与自身匹配 foo
; (f a b ) ---->  匹配  表 （f a b)
; (? x )   ----->  匹配任意表达式，并将其成为x
; (?c x)   -----> 匹配常量 记作x
; (?v x)   -----> 匹配变量 记作x

; 可以用 x 作为名字取得匹配对象的值

; 骨架实例化 Skeleton
; foo -----> 实例化 自己
; (f a b) -----> 实例化为 一个三元素表 f实例化 a实例化 b实例化
; (: x) ------> 实例化 为x 的值 也就是匹配对象的值 

; 代数规则
(define algebra-rules
  '(
    ( ((? op) (?c c1) (?c c2))                (: (op c1 c2))                )
    ( ((? op) (?  e ) (?c c ))                ((: op) (: c) (: e))          )
    ( (+ 0 (? e))                             (: e)                         )
    ( (* 1 (? e))                             (: e)                         )
    ( (* 0 (? e))                             0                             )
    ( (* (?c c1) (* (?c c2) (? e )))          (* (: (* c1 c2)) (: e))       )
    ( (* (?  e1) (* (?c c ) (? e2)))          (* (: c ) (* (: e1) (: e2)))  )
    ( (* (* (? e1) (? e2)) (? e3))            (* (: e1) (* (: e2) (: e3)))  )
    ( (+ (?c c1) (+ (?c c2) (? e )))          (+ (: (+ c1 c2)) (: e))       )
    ( (+ (?  e1) (+ (?c c ) (? e2)))          (+ (: c ) (+ (: e1) (: e2)))  )
    ( (+ (+ (? e1) (? e2)) (? e3))            (+ (: e1) (+ (: e2) (: e3)))  )
    ( (+ (* (?c c1) (? e)) (* (?c c2) (? e))) (* (: (+ c1 c2)) (: e))       )
    ( (* (? e1) (+ (? e2) (? e3)))            (+ (* (: e1) (: e2))
                                                 (* (: e1) (: e3)))         )
    ))

; 通用 化简器

; (define dsimp(simplifier deriv-rules))
; (dsimp '(+ x 3) 'x) ; 得出 1

; 一堆规则，  每一个规则----》 左侧是 模式， 右侧是骨架
; 规则的模式将传送到匹配器中  规则对应的骨架要送到实例化器中
; 一个匹配器   （一系列模式变量的值从匹配器中传递到实例化容器中）（我们传递的这个东西叫做词典，传递一本词典）（里面记载了： x匹配下例子表达式，而y匹配另一个表达式）
; 一个实例化器  （从实例化器中构造表达式，并传入到匹配器）

; 简单匹配器
; 模式树 和 表达式 树的 对比   
(define (match pattern expression dictionary)
  (cond ((eq? dictionary 'failed) 'failed)
        ; 模式是原子 表达式是原子 (+ 匹配 +) (* 匹配 *)
        ((atom? pattern)
         (if (atom? expression)
             ; 模式和原子相等
             (if (eq? pattern expression)
                 dictionary
                 'failed)
             'failed))
        ; 模式是任意常数  表达式是常量 扩展字典   
        ((arbitrary-constant? pattern)
         (if (constant? expression)
             (extend-dictionary pattern expression dictionary)
             'failed))
        ; 模式是任意变量  表达式是变量 扩展字典        
        ((arbitrary-variable? pattern)
         (if (variable? expression)
             (extend-dictionary pattern expression dictionary)
             'failed))
        ; 模式是任意表达式  扩展字典      
        ((arbitrary-expression? pattern)
         (extend-dictionary pattern expression dictionary))
        ; 表达式 是原子 错误 
        ((atom? expression) 'failed)
        (else
         ; 通用的情况 
         (match (cdr pattern)
                (cdr expression)
                (match (car pattern)
                       (car expression)
                       dictionary)))))

; 实例化器
; 实例化是用来将给定的表达式、词典和骨架生成新的表达式

(define (instantiate skeleton dictionary)
  (cond 
        ; 原子
        ((atom? skeleton) skeleton)
        ; 骨架求值 
        ((skeleton-evaluation? skeleton)
         ; 求值
         (evaluate (evaluation-expression skeleton)
                   dictionary))
        ; 复合表达式 树的递归遍历           
        (else (cons (instantiate (car skeleton) dictionary)
                    (instantiate (cdr skeleton) dictionary)))))


; 这个是执行 表达式？ 后面再理解
(define (evaluate form dictionary)
  (if (atom? form)
      (lookup form dictionary)
      (apply (eval (lookup (car form) dictionary)
                   user-initial-environment)
             (mapcar (lambda (v) (lookup v dictionary))
                     (cdr form)))))


; 控制结构

; 化简器
; 化简方法是： 基本对象 就是最简了，  复合对象 是不知道 ，

; 接受一系列规则，并生成一个使用该规则进行化简的程序
(define (simplifier the-rules)
  ; 返回 这个过程   simplify-exp 和 simplify-parts 一起递归遍历一个表达式
  ; simplify-exp 这个是任何表达式的通用化简方法
  ; simplify-parts 这个用于化简表达式的子部分 
  (define (simplify-exp exp)
    (try-rules (if (compound? exp) ; 是否是复合表达式 不是复合表达式 就使用规则
                   (simplify-parts exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        ; cons 组合成新的表达式
        (cons (simplify-exp   (car exp))
              (simplify-parts (cdr exp)))))
  ; 每一个过程中 做一些复杂操作， 包括尝试这些规则
  (define (try-rules exp)
    ; 扫描 规则
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dictionary (match (pattern (car rules))
                                   exp
                                   (make-empty-dictionary))))
            (if (eq? dictionary 'failed)
                ; 模式匹配失败
                (scan (cdr rules))
                ; 模式成功
                (simplify-exp (instantiate (skeleton (car rules))
                                           dictionary))))))
    (scan the-rules))
  simplify-exp)

; 另一种写法 用于理解
  (define (simplify-exp1 exp) 
    (try-rules 
      (if (compound? exp)
          (map simplify-exp1 exp) 
          exp
      )
    )
  )

; 关键点： 好的编程或设计方法需要知道什么是不需要考虑  