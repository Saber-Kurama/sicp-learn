#lang sicp

(define one-through-four (list 1 2 3 4))

one-through-four

(car one-through-four)
(cdr one-through-four)
one-through-four
(cons 10 one-through-four)
(cons 5 one-through-four)

; 表操作
; 取 n 数
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))	
  )
)
(list-ref one-through-four 2)

; 表长度 递归
; (define (length items)
;  (if (null? items)
;      0
;      (+ 1 (length (cdr items)))
;  )
; )

; 表长度 迭代
(define (lenght items)
  (define (lenght-iter a count)
      (if (null? a) count
	(lenght-iter (cdr a) (+ count 1))
      )
  )
  (lenght-iter items 0)
)
(length one-through-four)

; 表的 append 两个列表的合并

(define (append list1 list2)
  (if (null? list1) list2
     (cons (car list1) (append (cdr list1) list2))
  )
)

(define list1 (list 1 3 5 7))
(define list2 (list 2 4 6 8))
(append list1 list2)

; 列表的最后一个元素

; 列表的反转


; 对表的映射

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor) (scale-list (cdr items) factor))
  )
)
(scale-list (list 1 2 3 4 5) 10)

; 抽象成 map
(define (map proc items)
  (if (null? items)
     nil
     (cons (proc (car items)) (map proc (cdr items)))
  )
)
(map (lambda (x) (* x x)) (list 1 2 3 4 5))