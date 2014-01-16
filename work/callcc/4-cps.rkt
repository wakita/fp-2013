#lang racket

;;; CPS conversion: Conversion to a continuation passing style form

(define (K message)
  (lambda (v)
    (display message) (display ": ") (display v) (newline) (newline)))

;;; Naming convention:
;;;  - variable names: lowercase letters
;;;  - direct-style function names: lowercase letters (e.g., car, cdr, cons)
;;;  - names of CPS for functions with alphabetical names:
;;;      uppercase letters (e.g., CAR, CDR, CONS)
;;;  - names of CPS ofr functions with symbolic names:
;;;      original names followed by `@'
;;;  - continuations of a CPS function: k-<function name> (e.g., k-max, k-fact)

;;; Conversion of primitive functions
;;; CPS form of a primitive unary (arity 1) function f
;;; (f x) => (F x k-F)

; CPS conversions for "car" and "cdr"
(define (CAR pair k-CAR)
  (k-CAR (car pair)))

(CAR '(1 2 3) (K "(car '(1 2 3))"))

;;; CPS conversion of a primitive binary (arity 2) function f
;;; (f x y) => (f@ x y k)

; CPS conversions for "cons"
(define (CONS x y k-CONS)
  (k-CONS (cons x y)))

(CONS 1 '(2 3) (K "(cons 1 '(2 3))"))

;;; Convenience functions for declaring CPS forms of primitive functions

(define (k0 f) (lambda (k) (k (f))))
(define (k1 f) (lambda (a1 k) (k (f a1))))
(define (k2 f) (lambda (a1 a2 k) (k (f a1 a2))))
(define (k3 f) (lambda (a1 a2 a3 k) (k (f a1 a2 a3))))

(define CDR (k1 cdr))

(CDR '(1 2 3) (K "(cdr '(1 2 3))"))

(define =@ (k2 =))
(define >@ (k2 >))
(define <@ (k2 <))
(define >=@ (k2 >=))
(define <=@ (k2 <=))

(>@ 5 0 (K "(> 5 0)"))
(>@ 0 5 (K "(> 0 5)"))

;;; Convenience functions for variable argument primitive functions
(define (k* f) (lambda args
                 (let* ((sgra (reverse args))
                        (k (car sgra))
                        (args (reverse (cdr sgra))))
                   (k (apply f args)))))

(define +@ (k* +))
(define -@ (k* -))
(define *@ (k* *))
(define /@ (k* /))

(+@ 1 2 3 4 5 6 7 8 9 10 (K "1 + 2 + ... + 10"))

;;; CPS conversion of an expression "e"

;;; CPS[e, k]
;;;   where k denotes the current continuation

;;; Conversion of a constants and variables pass their values to the current continuation

;;; CPS[c, k] = (k c)
;;; CPS[v, k] = (k v)

;;; Conversion of conditional expressions

;;; CPS[(if e1 e2 e3), k] =
;;; (let ((k1 (lambda (v) (if v CPS[e2, k] CPS[e3, k]))))
;;;   CPS[e1, k1])

; Example 1: (if #t 'T 'F)
;
; CPS[(if #t 'T 'F), k] =
;
;   (let ((k1 (lambda (v) (if v CPS['T, k] CPS['F, k]))))
;     CPS[#t, k1])
;
; # k1 must be a fresh variable name
;
; = (let ((k1 (lambda (v) (if v (k 'T) (k 'F)))))
;     (k1 #t))

(let ((k (K "(if #t 'T 'F)")))
  (let ((k1 (lambda (v) (if v (k 'T) (k 'F)))))
    (k1 #t)))

; Example 2
;
; CPS[(if #f 1 (if #t 2 3)), k] =
;
;   (let ((k1 (lambda (v)
;               (if v CPS[1, k] CPS[(if #t 2 3), k]))))
;     (k1 #f)) =
;
;   (let ((k1 (lambda (v)
;               (if v (k 1)
;                 (let ((k2 (lambda (v)
;                             (if v CPS[2, k] CPS[3, k]))))
;                   CPS[#t, k2])))))
;     (k1 #f)) =
;
;   (let ((k1 (lambda (v)
;               (if v (k 1)
;                 (let ((k2 (lambda (v)
;                             (if v (k 2) (k 3)))))
;                   (k2 #t))))))
;     (k1 #f))
; 

(let ((k (K "(if #f 1 (if #t 2 3))")))
  (let ((k1 (lambda (v)
              (if v (k 1)
                (let ((k2 (lambda (v)
                            (if v (k 2) (k 3)))))
                  (k2 #t))))))
    (k1 #f)))

;;; Conversion of let-bindings

;;; CPS[(let ((v e1)) e2), k] =
;;; (let ((k1 (lambda (x)
;;;         (let ((v x))
;;;           CPS[e2, k])))
;;;   CPS[e1, k1])

; Example 1
;
; CPS[(let ((x 1)) x), k] =
;
; (let ((k1 (lambda (x)
;         (let ((v x))
;           CPS[x, k])))
;   CPS[1, k1] =
;
; (let ((k1 (lambda (x)
;         (let ((v x))
;           (k x))))
;   (k1 1))

(let ((k (K "(let ((x 1)) x)")))
  (let ((k1 (lambda (x)
              (let ((v x))
                (k x)))))
    (k1 1)))

; Example 2
;
; CPS[(let ((x (if #t 1 2))) (if #f 3 x)), k] =
;
; (let ((k1 (lambda (x)
;             (let ((v x))
;               CPS[(if #f 3 x), k]))))
;   CPS[(if #t 1 2), k1] =
;
; (let ((k1 (lambda (x)
;             (let ((v x))
;               (let ((k2 (lambda (v)
;                           (if v CPS[3, k] CPS[x, k]))))
;                 CPS[#f, k2])))))
;   (let ((k3 (lambda (v)
;               (if v CPS[1, k1] CPS[2, k1]))))
;     CPS[#t, k3])) =
;
; (let ((k1 (lambda (x)
;             (let ((v x))
;               (let ((k2 (lambda (v)
;                           (if v (k 3) (k x)))))
;                 (k2 #f))))))
;   (let ((k3 (lambda (v)
;               (if v (k1 1) (k1 2)))))
;     (k3 #t)))

(let ((k (K "(let ((x (if #t 1 2))) (if #f 3 x))")))
  (let ((k1 (lambda (x)
              (let ((v x))
                (let ((k2 (lambda (v)
                            (if v (k 3) (k x)))))
                  (k2 #f))))))
    (let ((k3 (lambda (v)
                (if v (k1 1) (k1 2)))))
                (k3 #t))))

;;; Conversion of a function application
;;; CPS[(f e1 e2 ...), k] =
;;; CPS[(let ((x1 e1) (x2 e2) ...) (f x1 x2 ...)), k]
;;;
;;; CPS[(f x1 x2), k] = (F x1 x2 k)

(define (fact n)
  (if (= n 0) 1
    (* n (fact (- n 1)))))

;(define (FACT n k)
;  (let ((k1 (lambda (v) (if v (k 1)
;                          CPS[(* n (fact (- n 1))), k]))))
;    [CPS(= n 0), k1]))
;
;(define (FACT n k)
;  (let ((k1 (lambda (v) (if v (k 1)
;                          CPS[(let ((v1 (fact (- n 1)))) (* n v1)), k]))))
;    (=@ n 0 k1)))
;
;(define (FACT n k)
;  (let ((k1 (lambda (v) (if v (k 1)
;                          (let ((k2 (lambda (x)
;                                      (let ((v1 x))
;                                        CPS[(* n v1), k]))))
;                            CPS[(fact (- n 1)), k2])))))
;    (=@ n 0 k1)))
;
;(define (FACT n k)
;  (let ((k1 (lambda (v) (if v (k 1)
;                          (let ((k2 (lambda (x)
;                                      (let ((v1 x))
;                                        (*@ n v1 k)))))
;                            CPS[(let ((v2 (- n 1))) (fact v2)), k2])))))
;    (=@ n 0 k1)))
;
;(define (FACT n k)
;  (let ((k1 (lambda (v) (if v (k 1)
;                          (let ((k2 (lambda (x)
;                                      (let ((v1 x))
;                                        (*@ n v1 k)))))
;                            (let ((k3 (lambda (x)
;                                        (let ((v2 x))
;                                          CPS[(fact v2), k2]))))
;                              CPS[(- n 1), k3]))))))
;    (=@ n 0 k1)))

(define (FACT n k)
  (let ((k1 (lambda (v) (if v (k 1)
                          (let ((k2 (lambda (x)
                                      (let ((v1 x))
                                        (*@ n v1 k)))))
                            (let ((k3 (lambda (x)
                                        (let ((v2 x))
                                          (FACT v2 k2)))))
                              (-@ n 1 k3)))))))
    (=@ n 0 k1)))

(let ((k (K "(fact 5)")))
  (FACT 5 k))

;;; The definition of long-awaited call/cc

(define (CALL/CC f k)
  (f k))
