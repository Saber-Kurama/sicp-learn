#lang sicp 

(#%require "2.45.rkt")

; 两个 画 左右
(define (beside x y)
 (cons x y)
)

; 两个画 下 上
(define (below x y) 
  (cons x y)
)

; 一个画 上下颠倒
(define (flip-vert x)
 (- x)
)

; 一个画 左右颠倒
(define (flip-horiz x)
 (- x)
)
; 180 反转
(define (rotate180 x) (- (- x)))

(define wave 1)

(define wave2 (beside wave (flip-vert wave)) )

; (define wave4 (below wave2 wave2))

; 重新 抽象 wave4 的实现
(define (filpped-pairs painter)
 (let ((painter2 (beside painter (flip-vert painter)) ))
   (below painter2 painter2)
 )
)

(define wave4 (filpped-pairs wave))

; 递归 右操作
(define (right-split painter n)
 (if (= n 0) painter
  (let ((smaller (right-split painter (- n 1))))
    (beside painter (below smaller smaller))
  )
 )
)
; 递归上操作
(define (up-split painter n)
  (if (= n 0) painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller))
    )
  )
)

; 抽象出split
; (define (split big-combiner small-combiner)
;     (define (inner painter n)
;         (if (= n 0)
;             painter
;             (let ((smaller (inner painter (- n 1))))
;                 (big-combiner painter   
;                               (small-combiner smaller smaller)))))
;     inner)

(define up-split1 (split below  beside))

; 平衡模式 --- 向上 向右 分支
(define (corner-split painter n )
  (if (= n 0) painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
         (let ((top-left (beside up up))
               (bottom-right (below right right))
               (corner (corner-split painter (- n 1))))
              (beside (below painter top-left) (below bottom-right corner))
          )
    )
  )
)

; 完整 图

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
       (let ((half (beside (flip-vert quarter) quarter)))
         (below (flip-vert half) half)
       )
  )
)

; 高阶操作

(define (square-of-four tl tr bl br)
 (lambda (painter)
  (let ((top (beside (tl painter) (tr painter)))
        (bottom (beside (bl painter) (br painter)))
       )
    (below bottom top)   
  )
 )
)

(define (filpped-pairs1 painter) 
  (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)
  )
)

(define (square-limit1 painter)
  (let ((combine4 (square-of-four flip-vert identity rotate180 flip-horiz)))
    (combine4 painter)
  )
)

; 框架

; 生成 框架 
(define (make-frame x) x)

; 框架 基准向量
(define (origin-frame x) x)

; 框架 角度向量1
(define (edge1-frame x) x)

; 框架 角度向量2
(define (edge2-frame x) x)