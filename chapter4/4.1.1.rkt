#lang sicp

; eval
; 基本表达式
; * 对于自求值表达式，例如各种数，eval 直接返回这个表达式本身
; * 变量，eval 必须在环境中查找变量，找出他们值
; 特殊形式
; * 对于加了引号的表达式 eval返回被引的表达式
; * 对于变量的赋值（定义）需要递归调用eval去计算出需要关联于这个变量的新值，而后需要修改换将，以改变（新建）响应的变量的约束
; * 一个if表达式要求对其中各部分的特殊处理方式，在谓词为真时，求值其推论部分，否则就求值其替代部分 
; * 一个lambda表达式必须被转换成一个可以应用的过程，方式就是将这个lambda表达式的参数表和体与相应的求值环境包装起来
; * 一个begin表达式要求 求值其中的一系列表达式，按照他们出现的顺序
; * 分情况分享（cond） 将被变换成一组嵌套的if表达式 求值
; 组合式
; * 对于一个过程应用， eval必须递归的求值组合式运算符部分和运算对象部分，而后将这样的带的过程和参数送给apply ，由他去处理是实际的过程应用
(define (eval exp  env)
  (cond ((self-evaluating? exp) exp) ;
        ; ((variable? exp) (lookup-variable-value exp env))
        ; ((quoted? exp) (text-of-quotation exp))
        ; ((assignment? exp) (eval-assignment exp env))
        ; ((definition? exp) (eval-definition exp env))
        ; ((if? exp) (eval-if exp env))
        ; ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ; ((begin? exp) (eval-sequence (begin-actions exp) env))
        ; ((cond? exp) (eval (cond->if exp) exp))
        ; ((application? exp)
        ;  (apply (eval (operator exp) env) (list-of-values (operands exp) env ))
        ; )
        (else (error "Unknow expression type -- EVAL" exp))
  )
)

; apply
; 两个参数 一个是过程，一个该过程应该去应用的实际参数表
; * 直接调用 apply-primitive-procedure 去应用的基本过程
; * 复合过程，顺序的求值组成该该过程体的那些表达式， 在求值复合过程是需要建立相应的环境，该环境是构造方式就是扩充该过程所携带的基本环境, 
; 并加入一个框架,其中将过程的各个形式参数约束于过程的调用的实际参数
(define (apply procedure arguuments)
  ; (cond ((primitive-procedure? procedure) (apply-promitive-procedure procedure arguments))
  ;       ((compound-procedure? prodedure) 
  ;         (eval-sequence 
  ;           (procedure-body procedure) 
  ;           (extend-environment 
  ;             (procedure-parameters procedure) 
  ;             arguments 
  ;             (procedure-environment procedure)
  ;           )
  ;         )
  ;       )
  ;       (else (error "Unknow procedure type -- APPLY" procedure))
  ; )
  ""
)

; 过程参数
; 生成过程的实际参数表
(define (list-of-values exps env)
  ; (if (no-operands? exps)) ()
  ;     (cons (eval (first-operand exps) env) (list-of-values (rest-operands exps) env))
  ""
)

; 条件
; 根据谓词来求值 真 求 if 推论部分 否则就采用替换部分
(define (eval-if exp env)
  ; (if (true? (eval (if-predicate exp) env))
  ;     (eval (if-consequent exp) env)
  ;     (eval (if-alternative exp) env)
  ; )
  ""
)

; 序列
(define (eval-sequence exps env)
""
)

; 赋值 和 定义
; 变量赋值
(define (eval-assignment exp env) 
""
)

; 变量定义
(define (eval-definition exp env)
""
)

; 表达式的表示
; 定义的语言的语法规范

; 这里的自求值表达式只有数 和 字符串
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)
  )
)

; 变量使用符号表示 ('x )
(define (variable? exp) (symbol? exp))

; 引号的表达式的表达形式 是 (quote <text-of-quotation>)
(define (quoted? exp)
  (tagged-list? exp 'qutote)
)

(define (text-of-quotation exp) (cadr exp))

;用以确定一个表的开始是不是某个给定的符号
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define x 'a)
(define y 'x)

; (quoted? "'q")
; (text-of-quotation "'a")
; (variable? x)

; 求值表达式的数据结构


; 运行程序

(define input-prompt "my_evaluator输入：")
(define output-prompt "my_evaluator输出：")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    ; (let ((output (eval input the-global-environment)))
    (let ((output (self-evaluating? input)))
      (announce-output output-prompt)
      (user-print output)
    )
  )
  (driver-loop)
)

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline)
)

(define (announce-output string)
  (newline) (display string) (newline)
)

(define (user-print object)
    ; (if (compound-procedure? object)
    ;     ; (display (list 'compound-procedure (procedure-parameters object) (procedure-body object) '<procedure-env>))
    ;     (display 'guocheng)
    ;     (display object))
    (display object)
)

(driver-loop)
