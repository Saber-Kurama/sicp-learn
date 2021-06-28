#lang sicp
(#%require (only racket provide))

;; provide 公开 x-func
(provide split)

(define (split big-combiner small-combiner)
    (define (inner painter n)
        (if (= n 0)
            painter
            (let ((smaller (inner painter (- n 1))))
                (big-combiner painter   
                              (small-combiner smaller smaller)))))
    inner)

; https://github.com/aQuaYi/SICP-in-Racket    