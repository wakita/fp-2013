#lang racket

(define (from-to n m)
  (if (> n m) '()
    (cons n (from-to (+ n 1) m))))

(define (times2 x y)
  (* x y))

(define (times nums)
  (if (null? nums) 1
    (begin
      (sleep 1) (display ".")
      (times2 (car nums) (times (cdr nums))))))

(define a-list (append '(1 2 0) (from-to 3 20)))

(define (times! nums)
  (call/cc
    (lambda (return)
      (define (aux nums)
        (if (null? nums) 1
          (begin
            (sleep 1) (display ".")
            (let ((n (car nums))
                  (nums (cdr nums)))
              (if (= n 0) (return 0)
                (times2 n (aux nums)))))))
      (aux nums))))

; (times! (from-to 1 20))
; (times! (append '(1 2 0) (from-to 3 20)))

(define (times!! nums)
  (call/cc
    (lambda (return)
      (define (aux nums)
        (if (null? nums) 1
          (begin
            (let ((n (car nums))
                  (nums (cdr nums)))
              (if (number? n)
                (if (= n 0) (return 0)
                  (times2 n (aux nums)))
                (return 'not-a-number))))))
      (aux nums))))
