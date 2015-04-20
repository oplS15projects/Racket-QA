#lang racket

(require rackunit)
(require racket/include)
(require rackunit/text-ui)
(require rackunit/gui)
(require "procs.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite trees
  #:before (lambda () (display "Starting Test Suite 'last-and-butlast'\n"))
  #:after (lambda () (display "Finished Test Suite 'last-and-butlast'\n"))
  (test-case "summing even leaves" (check-equal? (sum-even-leaves '(1 (2 3) (4 (5 6)))) 12) )
  (test-case "summing odd leaves" (check-equal? (sum-odd-leaves '(1 (2 3) (4 (5 6)))) 9) )
  (test-case "count-leaves" (check-equal? (count-leaves test-tree) 7) )
  (test-case "triple-leaves" (check-equal? (triple-leaves test-tree) '(3 (6 (9 (12) 15) 18) 21)) ) 
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite last-and-butlast
  #:before (lambda () (display "Starting Test Suite 'last-and-butlast'\n"))
  #:after (lambda () (display "Finished Test Suite 'last-and-butlast'\n"))
  (test-case "(last '(1 2 3))" (check-equal? (last '(1 2 3)) 3) )
  (test-case "(last '(1 2))" (check-equal? (last '(1 2)) 2) )
  (test-case "(last '(1))" (check-equal? (last '(1)) 1) )
  (test-case "(last '(1 (2 (3))))" (check-equal? (last '(1 (2 (3)))) '(2 (3))) )
  (test-case "(butlast '(1 2 3))" (check-equal? (butlast '(1 2 3)) '(1 2)) )
  (test-case "(butlast '(1 2))" (check-equal? (butlast '(1 2)) '(1)) )
  (test-case "(butlast '(1))" (check-equal? (butlast '(1)) '()) )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite palindrome-list
  #:before (lambda () (display "Starting Test Suite 'second-suite'\n"))
  #:after (lambda () (display "Finished Test Suite 'second-suite'\n"))
  (test-case "(palindrome? '())" (check-equal? (palindrome? '()) #t) )
  (test-case "(palindrome? '(1))" (check-equal? (palindrome? '(1)) #t) )
  (test-case "(palindrome? '(1 1))" (check-equal? (palindrome? '(1 1)) #t) )
  (test-case "(palindrome? '(1 2 1))" (check-equal? (palindrome? '(1 2 1)) #t) )
  (test-case "(palindrome? '(1 2 3 2 1))" (check-equal? (palindrome? '(1 2 3 2 1)) #t) )
  (test-case "(palindrome? '(1 2 3))" (check-equal? (palindrome? '(1 2 3)) #f) )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-list (list
  trees
  last-and-butlast
  palindrome-list
))

(provide (all-defined-out))

