#lang sicp

(define (sqrt-improve guess x) (average guess (/ x guess)))
; 求一个数的平方根
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses
)
; 打印出整个流
(display-stream (sqrt-stream 2))

(define (stream-map proc s) (if (stream-null? s)
the-empty-stream
(cons-stream (proc (stream-car s))
(stream-map proc (stream-cdr s)))))
