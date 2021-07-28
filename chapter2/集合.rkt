#lang sicp

; 抽象语言 解决一类问题
; 按照愿望的原则

; 基本元素
; 组合方法
; 抽象方法

;  元素是否是该集合的成员
; (define (element-of-set? x set) 
;     1
; )

;  添加元素到集合
; (define (adjoin-set? x set) 
;     1
; )

; ; 两个集合的并集
; (define (union-set A1 A2)
;     1
; )

; ; 两个集合的交集
; (define (intersection-set A1 A2)
;     1
; )


;==== 集合作为未排序的表

(define (element-of-set? x set)       ; 判断元素是否在集合中
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)            ; 元素添加进集合
  (if (element-of-set? x set) 
      set 
      (cons x set)))

(define (intersection-set set1 set2)  ; 求交集
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
            (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(intersection-set (list 1 2) (list 2 3))

(list 1 2)
'(1 2)