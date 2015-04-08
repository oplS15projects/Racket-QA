#lang racket

;; Name: Roy Van Liew
;; Section: 91.301.201 - Organization of Programming Languages

;; Exercise 2.57 on pp. 151.
;; Extend the differentiator to handle sums and products of length 2+.

;; Don't remove the equation1 definition.
(define equation1 '(* x y (+ x 3)))  ; i.e., ((x^2)y + 3xy) dx

;; You will need to change the following procedures:
;; make-sum
;; augend
;; make-product
;; multiplicand

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; per dotted-tail procedure notation, second and subsequent args
;; will get made into a list and provided in "augend"
;;
;; the constructor should deal with three cases:
;; [augend empty]      (make-sum 'x) is 'x
;; [augend length 1]   (make-sum 'x 3) is '(+ x 3)
;;                     (make-sum 'x 0) is 'x
;;                     (make-sum 1 2) is 3
;; [augend is 2+]      (make-sum 'x 'y 'z) is '(+ x y z)
;;
;; the code for the length 1 case is quite similar to the original
;; implementation; you should bring it in and modify it

;;; SUM PROCEDURES

; make-list-helper is used in make-sum to combine together variable lists length 2
; or greater. If the length of var-list (the passed augend) is 2, simply just
; return a list of those two elements. If it's greater than 2, do a recursive
; approach where we cons the first thing in the passed augend, and then
; call make-list-helper again to traverse down the augend list to cons together the
; rest of the variables (done through cdr, just like in accumulate).
(define (make-list-helper var-list)
  (cond ((= (length var-list) 2)
         (list (car var-list) (car (cdr var-list))))
        (else (cons (car var-list) (make-list-helper (cdr var-list))))) ; end cond
) ;; end define for sum-helper

;; The checks done in make-sum are described. If augend is length 2 or
;; greater, branch off to sum-helper to combine the rest of augend.
(define (make-sum a1 . augend)
  (cond
    
        ;; 1) If augend is empty, return a1.
        ((null? augend) a1)
    
        ;; 2) a1 is 0, in which case, return augend.
        ((and (number? a1) (= a1 0)) (car augend))
        
        ;; 3) If augend is length 1 that means it just has one thing.
        ;;    - If both a1 and augend are numbers, simply just add them.
        ;;    - Otherwise, check if either a1 or augend are variables.
        ;;      - If augend is a number equal to 0, just simply return a1.
        ;;      - Otherwise, return a list starting with '+ and combining a1 with the first thing in augend
        ;;    - Otherwise, return a list starting with '+ and combining a1 with the first thing in augend
        ((= (length augend) 1)
         (cond ((and (number? a1) (number? (car augend))) (+ a1 (car augend))) ; add the two numbers
               ((or (variable? a1) (variable? (car augend)))
                (cond ((and (number? (car augend)) (= (car augend) 0)) a1)
                      (else (list '+ a1 (car augend))))) ; end or check
               (else (list '+ a1 (car augend)))))
        
        ;; 4) augend is length 2 or greater. In this case, cons a plus symbol at the start,
        ;;    then cons a1 with the result from branching off to make-list-helper of augend.
        (else (cons '+ (cons a1 (make-list-helper augend))))) ;; end main cond
  
) ; end define

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

;; you're allowed to have augend also be a constructor
;; you will need to test for the length of the augend, and do
;; something different the length=1 case and length is 2+ case.

;; Here I check for several cases.
;; 1) If sum-list "s" is the empty list, return 0.
;; 2) If s is length 1, just return the first thing in it; it could just be a symbol or number.
;; 3) If s is length 3, that means there's a plus symbol with two other elements to be added.
;; 4) If s is greater than length 3, there's a plus symbol with more than two elements being added.
(define (augend s)
  (cond ((= (length s) 1) (car s)) ; e.g. '(x)
        ((= (length s) 3) (caddr s)) ; e.g. '(+ x 3)
        ((> (length s) 3) (cons '+ (cdr (cdr s)))) ; e.g. '(+ x y z), (cdr (cdr s)) gets '(y z)
        (else 0)) ; end cond
) ; end augend define


;;; MULTIPLICATION EXPRESSIONS

;; like make-sum, this should work with 1, 2, or 3+ args
;; and perform reductions on 1 and 2 arg cases
(define (make-product m1 . multiplicand)
    (cond
    
        ;; 1) If multiplicand is empty, return m1.
        ((null? multiplicand) m1)
    
        ;; 2) m1 is 0, in which case, return zero.
        ((and (number? m1) (= m1 0)) 0)
        
        ;; 3) m1 is 1, in which case, just return the multiplicand.
        ((and (number? m1) (= m1 1)) (car multiplicand))
        
        ;; 4) If multiplicand is length 1 that means it just has one thing.
        ;;    - If both m1 and multiplicand are numbers, simply multiply them.
        ;;    - Otherwise, check if either m1 or multiplicand are variables.
        ;;      - If multiplicand is a number equal to 0, return 0.
        ;;      - If multiplicand is a number equal to 1, return m1.
        ;;      - Otherwise, return a list starting with '* and combining m1 with the first thing in multiplicand
        ;;    - Otherwise, return a list starting with '* and combining m1 with the first thing in multiplicand
        ((= (length multiplicand) 1)
         (cond ((and (number? m1) (number? (car multiplicand))) (* m1 (car multiplicand))) ; multiply the two numbers
               ((or (variable? m1) (variable? (car multiplicand)))
                (cond ((and (number? (car multiplicand)) (= (car multiplicand) 0)) 0) ; Anything times 0 is 0
                      ((and (number? (car multiplicand)) (= (car multiplicand) 1)) m1) ; 1 is multiplicative identity
                      (else (list '* m1 (car multiplicand))))) ; end or check
               (else (list '* m1 (car multiplicand))))) ; end length 1 cond     
        
        ;; 5) multiplicand is length 2 or greater. In this case, cons a star symbol at the start,
        ;;    then cons m1 with the result from branching off to make-list-helper of multiplicand.
        (else (cons '* (cons m1 (make-list-helper multiplicand))))) ;; end main cond
  
) ;; end make-product define

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

;; may also construct a product expression
;; 1) If product-list "p" is the empty list, return 0.
;; 2) If p is length 1, just return the first thing in it; it could just be a symbol or number.
;; 3) If p is length 3, that means there's a star symbol with two other elements to be multiplied.
;; 4) If p is greater than length 3, there's a star symbol with more than two elements being multiplied.
(define (multiplicand p)
  (cond ((= (length p) 1) (car p)) ; e.g. '(x)
        ((= (length p) 3) (caddr p)) ; e.g. '(* x 3)
        ((> (length p) 3) (cons '* (cdr (cdr p)))) ; e.g. '(* x y z), (cdr (cdr s)) gets '(y z)
        (else 0)) ; end cond
)

;;; differentiation for exponents
(define (make-exponentiation x y)
  (cond ((= y 0) 1)
	((= y 1) x)
	(else (list '** x y))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

;;; deriv including exponentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum 
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product (make-exponentiation (base exp) 
							  (- (exponent exp) 1))
				     (deriv (base exp) var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(provide (all-defined-out))
