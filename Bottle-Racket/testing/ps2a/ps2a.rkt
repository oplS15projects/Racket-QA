#lang racket

;; Name: Roy Van Liew
;; Section: 91.301.201 - Organization of Programming Languages
;; Note: You're going to see some heavy commenting in here.

;;+++++++++++++++ Required for auto grading +++++++++++++
(define (square x) (* x x))
(define (identity x) x) ;; Used as a function for specifying to just use a passed value
(define (next x) (+ x 1)) ;; Basically inc
(define (inc n) (+ n 1)) ;; n++, this is basically next.
;;++++++++++++++++++++++++++++++++++++++++++++++++++


;; 1. exercise 1.9 (pp. 36). In this problem, you will study two implementations of addition in terms of 
;;    increment and decrement, and analyze whether the implementations are iterative or recursive.
;; Write the required steps for each procedure using comment line and then change #f to #t
;; NOTE: pg 46

;; First addition implementation is a recursive process, because the earlier calls
;; have to wait for the later calls to finish (deferring). Values don't start getting
;; returned until (+ 0 5) is reached, then the values are substituted back.
;; (+ 4 5) --> (inc (8)) = 9
;;   (+ 3 5) --> (inc (+ 3 5)) --> (inc (7)) = 8
;;     (+ 2 5) --> (inc (+ 2 5)) --> (inc (6)) = 7
;;       (+ 1 5) --> (inc (+ 1 5)) --> (inc (5)) = 6
;;         (+ 0 5) --> (inc (+ 0 5)) = 5 (b)

;; Second addition implementation is an iterative process, because the function
;; always knows the values of all its variables being worked with. There isn't
;; any deferring going on here.
;; (+ 4 5)
;;   (+ 3 6)
;;     (+ 2 7)
;;       (+ 1 8)
;;         (+ 0 9) = 9 (b value returned)
(define add #t)

;;***************************************************************************************************

;; 2. exercise 1.11 (pp. 42). In this problem, you implement a recursive mathematical function using 
;;    both recursive and iterative processes.
;; fill in the below procedures

;f(n) as a recursive process.
(define (f-recursive n)
  (if (< n 3)
      n ;; if n is less than 3 just return n
      (+ (f-recursive (- n 1)) ;; f(n-1)
         (* 2 (f-recursive (- n 2))) ;; 2*f(n-2)
         (* 3 (f-recursive (- n 3)))) ;; 3*f(n-3)
  )
)

; f(n) as an iterative process
;; Credit to Mark Sherman for giving guidance on approaching this iterative solution.
;; We're adding three things here: f(n-1) + 2*f(n-2) + 3*f(n-3)
;; Four variables in the function, three state variables and a counter.
;; a is (n-1), b is (n-2), c is (n-3). These must be the state variables, along with a counter.
(define (f-iterative n)
  (if (< n 3)
      n
      (f-iter 4 2 1 (- n 2)))
)

;; Example: f(5) is 25. Start counter at (n-2), in this case (5-2).
;; a   b   c   counter
;; 4   2   1   3        ;; NOTE this is the starting case
;; 11  4   2   2
;; 25  11  4   1        ;; When the counter reaches 1, return the a value.
;; counter == 1, stop iterations.
;; c = previous_b
;; b = previous_a
;; a = a + 2*b + 3*c
(define (f-iter a b c count)
  (if (= count 1)
      a                                   ;; If count is zero, return a (total sum)
      (f-iter (+ (* a 1) (* b 2) (* c 3)) ;; a term
              a b                         ;; b = previous_a, c = previous_b
              (- count 1))                ;; reduce counter by 1 for next iteration
              ;; end f-iter call
  )
)

;;***************************************************************************************************

;; 3. When implementing an iterative process in Scheme, the Scheme interpreter uses tail-recursion. 
;;    Explain what this means.
;; Answer this question using comment line and change #f to #t

;; Using tail recursion means the Scheme interpreter will execute an iterative process
;; in constant space even if the iterative process is described by a recursive procedure.
;; In other words, the memory usage doesn't expand if the number of function calls increase
;; for the iterative process.
(define tail-recursion #t)

;;***************************************************************************************************

;; 4. SICP exercise 1.16 (pp. 46).
;; fill in the below procedures

;; Recursive solution from SICP reading for context.
;(define (fast-expt b n)
;  (cond ((= n 0) 1)
;        ((even? n) (square (fast-expt b (/ n 2))))
;        (else (* b (fast-expt b (- n 1))))
;  )
;)

;; even function taken directly from reading.
(define (even? n)
  (= (remainder n 2) 0)
)

;; Goes by squaring operation if exponent is even rather than
;; the traditional multiply base by itself n times, where n is the exponent.
(define (fast-expt b n)
  (fast-expt-iter b n 1))

;; The counter represents the exponent here, and "b" is the base.
;; If the exponent is even, multiply by the square of the base. Subtract 2 from counter to stay even.
;; If the exponent is odd, multiply by the base, then subtract 1 from counter to make it even.
;; Also notice that if the exponent is odd, it becomes even when the counter is reduced by one.
;; Thus making the rest of the iterations squaring operations.
(define (fast-expt-iter b counter prod)
  (if (= counter 0)
      prod ;; If counter is zero it means we've done enough multiplications
      (if (even? counter)
          (fast-expt-iter b (- counter 2) (* prod (square b))) ;; if even, multiply by base squared
          (fast-expt-iter b (- counter 1) (* prod b)) ;; if odd, multiply by base
          ) ;; inner if
       ) ;; outer if
  )

;;***************************************************************************************************

;; 5. More iteration: SICP exercise 1.30 (pp. 60).
;; Un-comment and fill in the below procedure

;; For context, this is the recursive solution in the reading.
;; sum is a procedure that takes 4 arguments:
;; - a procedure with one argument
;; - a second procedure with one argument.
;; i.e. square 2 inc 6 <-- square is team, 2 is a. inc is next, 6 is b.
;; a is lower bound, b is upper bound.
;(define (sum term a next b)
;(if (> a b)
;    0
;    (+ (term a) (sum term (next a) next b))))

;; "a" here is the lower bound being passed.
;; "b" here is the upper bound being passed.
;; So (sum square 2 inc 4) means we're adding the squared numbers with the bases
;; from 2 to 4. (2^2) + (3^2) + (4^2) = 4 + 9 + 16 = 29.
(define (sum term a next b)
  ;; "iter" is its own procedure.
  (define (iter a result)
    (if (> a b) ;; if the lower bound is bigger than upper bound, stop.
        result
        (iter (next a) (+ (term a) result)))) ;; apply procedure to a and add to current result.
  ;; Now run iter, which does the summation.
  (iter a 0) ;; sum result starts at 0. Note if a > b at the start, 0 is returned here.
)

;;****************************** END OF FILE ******************************************************
(provide (all-defined-out))

