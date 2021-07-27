#lang sicp

(define a 1)
(define b 2)
(list a b)
(list 'a b)
(car '(+ 3 (+ 2 4) 1))
(cdr (cdr '(+ 3 (+ 2 4) 1)))
(cdr (car (cdr (cdr '(+ 3 (+ 2 4) 1)))))