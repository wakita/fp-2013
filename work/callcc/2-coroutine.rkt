#lang racket

;;; A pair of coroutines

(define todo #f)

(define (switch task)
  (case (call/cc (lambda (k)
                   (let ((saved-todo todo))
                     (set! todo k)
                     (if saved-todo
                       (saved-todo 'wake-up)
                       'go-to-bed))))
    ((wake-up) (sleep 0.5) (task))
    ((go-to-bed) (display "          Somebody goes to bed") (newline))))

(define (message m)
  (switch (lambda ()
            (display m) (newline)
            (message m))))

(message "ping")
(message "     pong")
