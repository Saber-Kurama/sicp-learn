#lang sicp

(define (eval exp  env)
  exp
)

;用以确定一个表的开始是不是某个给定的符号
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;判断是否为过程
(define (compound-procedure? p)
    (tagged-list? p 'procedure))
; 环境
;;;关于框架
;框架是一对表形成的序队（变量表、值元素表）
(define (make-frame variables values)
  (cons variables values))
(define the-empty-environment '())
; 返回一个新的环境,这个环境包含了一个新的框架，其中 所有位于表vars里的符号约束到表vals里对应的元素，而其外围环境是环境 base-env
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "")
          (error "") 
          )
      )
  )

; 定义初始环境
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names) 
                             (primitive-procedure-objects)
                             the-empty-environment
                             ) 
         ))
    ; (define-variable! 'true true initial-env)
    ; (define-variable! 'false false initial-env)    
    initial-env
    )
  )

;识别基础过程primitive(原始)
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))


;全局环境中要加入基本过程的名字和相应的实现过程
(define primitive-procedures
  (list  (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         (list 'null? null?)
         (list '+ +)
         (list '- -)
         (list '* *)
         ))

;获取我们定义的解释器中过程的的变量名
(define (primitive-procedure-names)
  (map 
   car
   primitive-procedures))

;获取我们定义的解释器中过程的相应实现
(define (primitive-procedure-objects)
    (map 
        (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))

(define the-global-environment (setup-environment))

(define input-prompt "my_evaluator输入：")
(define output-prompt "my_evaluator输出：")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
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
    (if (compound-procedure? object)
        ; (display (list 'compound-procedure (procedure-parameters object) (procedure-body object) '<procedure-env>))
        (display 'guocheng)
        (display object)))

(driver-loop)
