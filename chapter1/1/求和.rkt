#lang sicp

; 1
(define (1+ x)
  (+ x 1))

; 2
(define (1- x)
  (- x 1))

(define (+* x y)
  (if (= x 0) y
      (+* (1- x) (1+ y))
  )
)

(+* 3 4)
;(+* 3 4)
;(+* (1- 3) (1+ 4))
;(+* 2 5)

(define (+** x y )
  (if (= x 0) y
      (1+ (+** (1- x) y))
  )
)

(+** 3 4)