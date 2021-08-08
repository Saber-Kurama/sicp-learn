#lang sicp

(define (call-each s)
    'done
)

(define (make-wire)
    (let ((signal-value 0) (action-procedure '()))
        (define (set-my-signal! newvalue)
            (if (not (= signal-value newvalue))
                (begin (set! signal-value newvalue)
                       (call-each action-procedure))
                'done       
            )
        )
        (define (accept=action-procedure! proc)
            (set! action-procedure (cons proc action-procedure))
            (proc)
        )
        (define (dispatch m)
            (cond ((eq? m `get-signal) signal-value)
                  ((eq? m `set-my-signal!) set-my-signal!)
                  ((eq? m `add-action!) accept=action-procedure!)
                  (else (error "Unknow operaction" m))
            )
        )
        dispatch
    )
)