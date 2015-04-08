#lang racket

;; Name: Roy Van Liew
;; Section: 91.301.201 - Organization of Programming Languages

;; Exercise 2.58 on pp. 151.
;; Converting the differentiator to infix. Do subproblem (a) only.

;; You will need to change the following procedures:
;; make-sum
;; sum?
;; addend
;; make-product
;; product
;; multiplier
;; make-exponentiation
;; base
;; exponent

(define (variable?-infix x) (symbol? x))

(define (same-variable?-infix v1 v2)
  (and (variable?-infix v1) (variable?-infix v2) (eq? v1 v2)))

(define (=number?-infix exp num)
  (and (number? exp) (= exp num)))

;;; SUM EXPRESSIONS

(define (make-sum-infix a1 a2)
  (cond ((=number?-infix a1 0) a2) ; If a1 is zero, just return a2 (this is 0 + a2)
	((=number?-infix a2 0) a1) ; If a2 is zero, just return a1 (this is a1 + 0)
	((and (number? a1) (number? a2)) (+ a1 a2)) ; Just do addition if both are numbers
	(else (list a1 '+ a2)))) ; Otherwise, return a symbolic addition expression

; Check that it's a pair and if the plus is in the middle
(define (sum?-infix x)
  (and (pair? x) (eq? (car (cdr x)) '+)))

; First add term is at the start since plus is in the middle for infix
(define (addend-infix s) (car s))

(define (augend-infix s) (caddr s))


;;; MULTIPLICATION EXPRESSIONS

(define (make-product-infix m1 m2)
  (cond ((or (=number?-infix m1 0) (=number?-infix m2 0)) 0) ; Anything multiplied by 0 is 0
	((=number?-infix m1 1) m2) ; 1 * m2 = m2
	((=number?-infix m2 1) m1) ; m1 * 1 = m1
	((and (number? m1) (number? m2)) (* m1 m2)) ; If not symbols, just multiply it
	(else (list m1 '* m2)))) ; Otherwise, return symbolic multiplication expression

; Check that it's a pair and if the star is in the middle
(define (product?-infix x) (and (pair? x) (eq? (car (cdr x)) '*)))

; Multiplier is at the start since star is in the middle for infix
(define (multiplier-infix p) (car p))

(define (multiplicand-infix p) (caddr p))


;;; differentiation for exponents

(define (make-exponentiation-infix x y)
  (cond ((= y 0) 1)
	((= y 1) x)
	(else (list x '** y))))

; Check that it's a pair and if the double star is in the middle
(define (exponentiation?-infix x)
  (and (pair? x) (eq? (car (cdr x)) '**)))

; Base is at the start since double star is in the middle for infix
(define (base-infix x) (car x))

(define (exponent-infix x) (caddr x))

;;; deriv including exponentiation

(define (deriv-infix exp var)
  (cond ((number? exp) 0)
	((variable?-infix exp)
	 (if (same-variable?-infix exp var) 1 0))
	((sum?-infix exp)
	 (make-sum-infix (deriv-infix (addend-infix exp) var)
		   (deriv-infix (augend-infix exp) var)))
	((product?-infix exp)
	 (make-sum-infix 
	  (make-product-infix (multiplier-infix exp)
			(deriv-infix (multiplicand-infix exp) var))
	  (make-product-infix (deriv-infix (multiplier-infix exp) var)
			(multiplicand-infix exp))))
	((exponentiation?-infix exp)
	 (make-product-infix (exponent-infix exp)
		       (make-product-infix (make-exponentiation-infix (base-infix exp) 
							  (- (exponent-infix exp) 1))
				     (deriv-infix (base-infix exp) var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(provide (all-defined-out))
