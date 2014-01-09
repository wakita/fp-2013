#lang racket

;;; More complete co-routine library

(require data/queue)

(define threads (make-queue))
(define threads-start #f)

(define (switch task)
  (case (call/cc (lambda (k)
                   (enqueue! threads k)
                   (and threads-start
                        ((dequeue! threads) 'wake-up))))
    ((wake-up) (sleep 0.5) (task))))

(define (start-threads)
  (set! threads-start #t)
  ((dequeue! threads) 'wake-up))

(define (message m)
  (switch (lambda ()
            (display m) (newline)
            (message m))))

(switch (lambda () (message "A")))
(switch (lambda () (message "  B")))
(switch (lambda () (message "    C")))
(switch (lambda () (message "      D")))

(start-threads)
