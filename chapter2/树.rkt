#lang sicp

(define (count-leaves x) 
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x)) (count-leaves (cdr x)))
          )
    )
)

(define x (cons (list 1 2) (list 2 3)))

(count-leaves x)
; 练习 2.24
(list 1 (list 2 (list 3 4)))
(define y (list 1 (list 2 (list 3 4))))
; * * --> * * ---* * ---4
; 1       2      3
;      
;   (list 1 (list 2 (list 3 4)))
;          /     \
;      1         (list 2 (list 3 4))
;                   /   \
;                2      (list 3 4)
;                           / \
;                          3   4 
(count-leaves y)

(define (scale-tree tree factor) 
    (cond ((null? tree) nil)
          ((not (pair? tree)) (* tree factor))
          (else 
          (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor))
          )
    )
)
(define list1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

; map

(define (scale-tree1 tree factor)
    (map 
        (lambda (sub-tree) 
            (if (pair? sub-tree) (scale-tree1 sub-tree factor) (* sub-tree factor))
        ) 
    tree)
)
(scale-tree1 list1 10)