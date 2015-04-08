#lang racket

;; Name: Roy Van Liew
;; Section: 91.301.201 - Organization of Programming Languages
;; Note: The comments included in this submission might look overwhelming
;;       in number at first, but they go into the details of implementation.

;;+++++++++++++++ Required for auto grading +++++++++++++
(define (square x) (* x x))
(define (identity x) x) 
(define (next x) (+ x 1)) 
(define (inc n) (+ n 1))
;;++++++++++++++++++++++++++++++++++++++++++++++++++

;; 6. Working towards higher-order procedures: Do SICP exercise 1.31 (a) and (b) (pp. 60–61).
;; fill in the below procedures

;a)Recursive product procedure.
(define (product1 term a next b)
(if (> a b)
    1 ;; Return 1, the multiplicative identity, if the starting number is bigger than the end number.
    (* (term a) (product1 term (next a) next b)))) ;; Otherwise, continue with the next term.

;; factorial procedure in terms of product
;; Basically this is just multiplying 1 * 2 * 3 * ... * n from definition of n!
(define (factorial n) 
   (product1 identity 1 inc n))

; Approximations to pi using john wallis' formula
; Term  Numerator  Denominator
; 1     2          3
; 2     4          3
; 3     4          5
; 4     6          5
; 5     6          7
; 6     8          7
; n is an integer that specifies which term to pass in the formula, following the above pattern.
(define (pi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))  ;; If n is even term, numerator = n + 2 and denominator = n + 1
      (/ (+ n 1) (+ n 2))) ;; If n is odd term, numerator = n + 1 and denominator = n + 2
)

;b) Iterative product procedure
(define (product2 term a next b)
  ;; "iter" is an iterative procedure that uses upper bound "b" as a counter.
  (define (iter a prod counter)
    (if (= counter 0) ;; if the lower bound is bigger than upper bound, stop and return calculated product.
        prod
        (iter (next a) (* (term a) prod) (- counter 1) ))) ;; apply procedure to a and multiply a to current result.
  ;; Now run iter to return a product for the specified range.
  (iter a 1 (+ (- b a) 1)) ;; (+ (- b a) 1) Is the range b-a, and the +1 includes the last term.
)

;;***************************************************************************************************

;; 7. SICP exercise 1.32 (a only): Implement accumulate and show how sum and product can be defined with calls to accumulate. Specify whether you have built the iterative or recursive version.
;; fill in the below procedures

;a) I have implemented the Recursive Version of accumulate here.
;; Note that combiner is the function of two arguments that allows the current term to be combined with previous terms.
;; combiner will apply the term function to "a" before combining it with the previous terms.
;; We are still accumulating terms in the range a-b. null-value is what to return when we run out of terms to combine.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value ;; will return 0 if sum, return 1 if product
      (combiner (term a) (accumulate combiner null-value term (next a) next b)))
)

;; Write this in terms of simple calls to accumulate
;; The lambda here which creates the combiner function simply takes two arguments to add to each other.
;; The null-value will be zero for the sum.
;; Notice how short this code is now!
(define (sum1 term a next b)  
  (accumulate (lambda (x y) (+ x y)) 0 term a next b))

;; Write this in terms of simple calls to accumulate.
;; The lambda here which creates the combiner function simply takes two arguments to multiply each other.
;; The null-value will be one for the product (multiplicative identity).
;; Notice how short this code is now! The difference from sum1 is that we changed + to *, and 0 to 1.
(define (product term a next b)  
  (accumulate (lambda (x y) (* x y)) 1 term a next b))

;;***************************************************************************************************

;; 8. SICP exercise 1.41 (pp. 77): Procedures that return procedures.
;; fill in the below procedure

; Credit to Mark Sherman for providing help in understanding this return process.
; Note that "f" is a procedure passed to double.
; lambda is a procedure of ONE argument "x" that applies procedure "f"
; which also takes only one argument. The argument passed to f in this case
; is f itself along with the "x" passed to it.
; Example is ((double inc) 5) -- "inc" would be "f", and "5" would be "x".
(define (double f)
  (lambda (x) (f (f x))))

;;***************************************************************************************************

;; 9. SICP exercise 1.42 (pp. 77): More procedures that return procedures.
;; fill in the below procedure

; lambda is a procedure of ONE argument "x". In this procedure,
; function "g" is applied to "x" first, then function "f" is
; applied to the value returned from function "g".
(define (compose f g)
   (lambda (x) (f (g x))))

;;***************************************************************************************************

;; 10. Here is an implementation of expnt, a procedure that generates a procedure for raising its input to its argument. 
;;     E.g., (expnt 2) generates a procedure for squaring a number. The provided implementation of expnt generates 
;;     a recursive process. Re-implement it as an iterative process (probably using a helper procedure).
;; generates a procedure that raises its argument to the nth power
;; Recursive process
(define (expnt n)
  (if (= n 1)
      (lambda (x) x)
      (lambda (x) (* x ((expnt (- n 1)) x)))))

; Iterative process
; The lambda here generates an iterative procedure which takes one argument:
; "base" is the base to raise to a power. This is the argument passed to the created procedure.
; The "n" that is passed to expnt-iter is used to specify the power to raise "base" to,
; and the lambda here references the iterative helper function that uses n as a counter
; and multiplies the base by itself n times.
(define (expnt-iter n)
  (if (= n 1)
      (lambda (x) x) ;; this case means the exponent is 1, in which case just simply return the number
      (lambda (base) (expnt-iter-helper n 1 base)) ;; Product starts at 1, counter starts at exponent passed
  ) ;; end if
)

;; Iterative Helper Function for expnt-iter, used in the lambda.
(define (expnt-iter-helper counter prod base) 
        (if (= counter 0)
            prod ;; If counter is zero it means we've done enough multiplications
            (expnt-iter-helper (- counter 1) (* base prod) base))
) 

;;***************************************************************************************************

;; Honors, Graduate Students, and Undergrads Looking for More
;; 11. SICP exercise 1.43 (pp. 77): Repeated application of procedures.
;; fill in the below procedure

;; repeat takes as two arguments:
;; - first argument is a procedure that takes one argument
;; - second argument is the value to apply the function to
;; lambda takes an argument, such as an integer, to apply the repeated procedure twice to.
;; e.g. (repeat square 2) will apply square twice. If square was 3, it would be applied three times.
;; ((repeat square 2) 5) = (5^2)(5^2) aka 5 squared twice = 625.
;; repeat creates a procedure that applies a procedure "f" n times to one argument "x".
(define (repeat f n)
  (if (= n 1)
      (lambda (x) (f x)) ;; Just apply function once if n = 1.
      (lambda (x) (repeat-helper f n x)) ;; Remember, x is what we're applying repetitions of f to.
      )
)

;; Argument "x" is an integer to apply a function "f" to.
;; n basically acts as our counter -- how many times to apply said function "f".
(define (repeat-helper f count x)
  (if (= count 0)
      x
      (repeat-helper f (- count 1) (f x)))
)

;;****************************** END OF FILE ******************************************************
(provide (all-defined-out))
