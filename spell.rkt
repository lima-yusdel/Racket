#lang racket
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2019                              *
; *  Student Version
; * By Yusdel Lima Lorenzo
; *********************************************
(require racket/trace)
;; contains"ctv", "A", and "reduce" definitions
(require "include.rkt")

;; contains simple dictionary definition
(require "test-dictionary.rkt")
;can change the top line to just dictionary

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***
(define initVector
  (lambda (hashfunctionlist dict)
    (cond ((null? dict) '())
          (else (append (hash hashfunctionlist (car dict)) (initVector hashfunctionlist (cdr dict)))))
))

(define hash
  (lambda (hashfunctionlist w)
    (cond ((null? hashfunctionlist) '())
          (else (cons (inexact->exact ((car hashfunctionlist) w)) (hash (cdr hashfunctionlist) w))))
))

(define bitvector?
  (lambda (hashfunctionlist w bitvector)
    (cond ((null? hashfunctionlist) #t)
          ((and (bitvectorHelper? (inexact->exact((car hashfunctionlist) w)) bitvector) (bitvector? (cdr hashfunctionlist) w bitvector)) #t)
          (else #f))
))

(define bitvectorHelper?
  (lambda (value bitvector)
    (define result (reduce compare bitvector value))
    (cond ((number? result) #f)
          ((boolean? result) #t))
))

(define compare
  (lambda (x y)
    (cond ((eq? x y) #t)
          ((or (eq? x #t) (eq? y #t)) #t)
          (else y))
))

;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (if (null? w) 5413
        (+ (* (key (cdr w)) 29)(ctv (car w))))
    )
 )

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 111037761665
;;   (key '(m a y))           = 132038724
;;   (key '(t r e e f r o g)) = 2707963878412931

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (x)
	(modulo (key x) size))))
;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
   (lambda (x)
      (floor(*(-(*(key x)A)(floor(* (key x) A)))size)))))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

;(define hash-1 (gen-hash-division-method 70111))
;(define hash-2 (gen-hash-division-method 89989))
;(define hash-3 (gen-hash-multiplication-method 700426))
;(define hash-4 (gen-hash-multiplication-method 952))

;(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
;(define hashfl-2 (list hash-1 hash-3))
;(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;; (hash-1 '(h e l l o))        ==> 26303
;; (hash-1 '(m a y))            ==> 19711
;; (hash-1 '(t r e e f r o g))  ==> 3010
;;
;; (hash-2 '(h e l l o))        ==> 64598
;; (hash-2 '(m a y))            ==> 24861
;; (hash-2 '(t r e e f r o g))  ==> 23090
;;
;; (hash-3 '(h e l l o))        ==> 313800.0
;; (hash-3 '(m a y))            ==> 317136.0
;; (hash-3 '(t r e e f r o g))  ==> 525319.0
;;
;; (hash-4 '(h e l l o))        ==> 426.0
;; (hash-4 '(m a y))            ==> 431.0
;; (hash-4 '(t r e e f r o g))  ==> 714.0

;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
    (define bitvector (initVector hashfunctionlist dict))
    (lambda (w)
      (bitvector? hashfunctionlist w bitvector)
)))
;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

;(define checker-1 (gen-checker hashfl-1 dictionary))
;(define checker-2 (gen-checker hashfl-2 dictionary))
;(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #f
