;; File: ps3b.rkt
;; Author: James Kuczynski
;; Email: James_Kuczynski@student.uml.edu


#lang racket

(define (inc x) (+ x 1))

;; UPDATED ps3b version 2

;; Honors, Grad Students, and Extra Credit

;; +++++++++++++++ Required for auto grading ++++++++++++++++++++++++++++

(define a (list (list 1 2) (list 3 (list 4 5)))) 

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; **********************************************************************

;; 1. SICP exercise 2.20 (pp. 104), on the dotted-tail notation.
;; While answering this question you may write helper procedures
(define (same-parity first . lst)
  (if (even? first)
      (filter even? (cons first lst))
      (filter odd? (cons first lst))))

;; **********************************************************************

;; 2. Solve SICP exercise 2.27 on deep-reverse. Note: it is be easy
;; enough to Google for the answer on this.  Obviously this will
;; short-circuit your learning.  So don't do that, and instead please
;; show steps along the way to your solution e.g., partly working
;; code, what inputs you tested it on, what results it produced, what
;; you did next.

; reverse
; this should be reverse the items at the top level of the list

(define nil '())

(define (reverse items) 
  '())

; deep reverse
; recursively reverse sublists
(define (deep-reverse items) 
  '())

;; **********************************************************************
;; 3. SICP exercise 2.6 (pp. 91), on Church numerals.

;; Fill in the below procedures below
;; Roy filled in some of these to get the tests running

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f)
  (lambda (x) (f (f x)))))
 
(define (add a b)
  (lambda (f)
   (lambda (x)
     1)))

;; ************************ END OF FILE *********************************
(provide (all-defined-out))
