#lang sicp 

; 基本原语(元素)

(define (painter frame)
  "画家啊"
)
(define wave (painter "frame")) ; 画家wave
(define rogers (painter "frame")) ; 画家 rogers

; 组合手段 
; 两个画家的左右组合
(define (beside p1 p2) 
  (cons p1 p2) 
)
; 两个画家的上下组合
(define (below p1 p2) 
  (cons p1 p2) 
)
; 画家的反转
(define (flip-vert p1)
  "反转啊"
)
; 一个画 左右颠倒
(define (flip-horiz x)
 (- x)
)
; 180 反转
(define (rotate180 x) (- (- x)))

(define ab (beside wave rogers) ) ; 两个画家的 左右合并 ---> 生成 新的画家 ab

(define waveR (flip-vert wave)) ; waveR 是一个画 倒wave的画家

(define wave2 (beside wave (flip-vert wave))); wave2 是一个 画（A(倒A））的画家

; (define wave4 (below wave2 wave2)); wave4 是一个( (上(A(倒A)) (下(A(倒A)) ）的画家

; 抽象手段

; 抽象的一些典型组合方式 例如对 wave4 进行抽象 (这就是一个抽象)
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
       (below painter2 painter2))
)
; 抽象的威力
(define wave4 (flipped-pairs "A")); wave4 是一个( (上(A(倒A)) (下(A(倒A)) ）的画家
(define wave4B (flipped-pairs "B")); wave4B 是一个( (上(B(倒B)) (下(B(倒B)) ）的画家

; 可以抽象方法中可以采用一些递归操作
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

; 完整 图 ， 4个平衡图 生成
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
       (let ((half (beside (flip-vert quarter) quarter)))
         (below (flip-vert half) half)
       )
  )
)

; 高阶组件 的抽象

; 针对 flipped-pairs 和 square-limit 可以抽象成一个 画家 拷贝一个 一个正方形的中，只是4个图旋转了角度
; identity 的重要性 (x) -> x 自函子

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

(define (square-limit1 painter n)
  (let ((combine4 (square-of-four flip-vert identity rotate180 flip-horiz)))
    (combine4 ((corner-split painter n)))
  )
)

; 定义一个split 高级组件抽象 用来 实现  corner-split up-split right-split


; 框架(画布)

; 返回一个过程，经一个 向量 映射到 frame 画布中的向量值
; (define (frame-coord-map frame)
;   (lambda (v)
;     (add-vect 
;         (origin-frame frame)
;         (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
;                   (scale-vect (ycor-vect v) (edge2-frame frame))
;         )
;     ) 
;   )
; )

; add-vect  向量相加 sub-vect 向量相减 

; 
; origin 画布的原点 corner1 画布的边1向量的终点 corner2 画布的边2向量的终点
(define (transform-painer painter origin corner1 corner2 )
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin))) ; 新的原点
         (painter 
            (make-frame new-origin 
                        (sub-vect (m corner1) new-origin)
                        (sub-vect (m corner2) new-origin)  
            )
         ) 
      )
    ) 
  )
)

; 画家的反转
(define (flip-vert-1 painter)
  (transform-painter
    painter
    (make-vect 0 1)    ;新的原点
    (make-vect 1 1)    ;新的角向量edge1
    (make-vect 0 0)))  ;新的角向量edge2

; baside 的实现
(define (beside-1 p1 p2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((l (transform-painter p1
                                (make-vect 0 0)
                                split-point
                                (make-vect 0 1)))
          (r (transform-painter p2
                                split-point
                                (make-vect 1 0)
                                (make-vect 0.5 1))))
      (lambda (frame)
        (l frame)
        (r frame))))) 

;用过程表示 画家 拥有闭包特性 

; 分层涉及 怨言 语言抽象
;  语言嵌入到lisp中
