#lang sicp

; 帕斯卡三角形

;  (define (pascal r c) 
;    (if (or (= c 1) (= c r)) 
;        1 
;        (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))
 (define (pascal r c) 
   (if (or (= c 1) (= c r)) 
       1 
       (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))        
  (pascal 1 1) 

  ; 先不考虑便捷
 ; pascal(r c) = pascal( r - 1  c - 1) + pascal(r - 1 c)
 ; c = 1  1
 ; c = r  1 
 ; r = 0 0
 ; c > r 0
