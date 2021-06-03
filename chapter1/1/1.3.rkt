#lang sicp

(define (threeMax a b c) (if (> a b) 
        (if (> a c ) a c ) 
        (if (> b c ) b c )
)) 

(threeMax 3 8 4)