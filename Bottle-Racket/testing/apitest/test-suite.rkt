#lang racket

(require rackunit)
(require racket/include)
(require rackunit/text-ui)
(require rackunit/gui)
(require "source.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite first-suite
  #:before (lambda () (display "Starting Test Suite 'first-suite'\n"))
  #:after (lambda () (display "Finished Test Suite 'first-suite'\n"))
  (test-case "(square 3)" (check-equal? (square 3) 9))
  (test-case "(square 1)" (check-equal? (square 1) 1))
  (test-case "(abs-using-if 0)" (check-equal? (abs-using-if 0) 0))
  (test-case "(abs-using-if 1)" (check-equal? (abs-using-if 1) 1))
  (test-case "(abs-using-if -3)" (check-equal? (abs-using-if -3) 3))
  (test-case "(abs-using-cond 0)" (check-equal? (abs-using-cond 0) 0))
  (test-case "(abs-using-cond 1)" (check-equal? (abs-using-cond 1) 1))
  (test-case "(abs-using-cond -3)" (check-equal? (abs-using-cond -3) 3))
  (test-case "(fact 1)" (check-equal? (fact 1) 1))
  (test-case "(fact 5)" (check-equal? (fact 5) 120))
  (test-case "(fact 20)" (check-equal? (fact 20) 2432902008176640000))
  (test-case "(comb 3 2)" (check-equal? (comb 3 2) 3))
  (test-case "(comb 4 2)" (check-equal? (comb 4 2) 6))
  (test-case "(comb 10 2)" (check-equal? (comb 10 2) 45))
  (test-case "(comb 93 37)" (check-equal? (comb 93 37) 118206769052646517220135262))
  (test-case "(triple 0)" (check-equal? (triple 0) 0))
  (test-case "(triple 15)" (check-equal? (triple 5) 15))
  (test-case "(triple -1)" (check-equal? (triple -1) -3))
  (test-case "(diff-of-triples 1 0)" (check-equal? (diff-of-triples 1 0) 3))
  (test-case "(diff-of-triples 3 1)" (check-equal? (diff-of-triples 3 1) 6))
  (test-case "(diff-of-triples 10 2)" (check-equal? (diff-of-triples 10 2) 24))
  (test-case "(smallest-of-three 1 2 3)" (check-equal? (smallest-of-three 1 2 3) 1))
  (test-case "(smallest-of-three 2 1 3)" (check-equal? (smallest-of-three 2 1 3) 1))
  (test-case "(smallest-of-three 3 2 1)" (check-equal? (smallest-of-three 3 2 1) 1))
  (test-case "(smallest-of-three 1 1 2)" (check-equal? (smallest-of-three 1 1 2) 1))
  (test-case "(smallest-of-three -1 -1 0)" (check-equal? (smallest-of-three -1 -1 0) -1))
  (test-case "(smallest-of-three 1 1 1)" (check-equal? (smallest-of-three 1 1 1) 1))
  (test-case "(smallest-tripled 1 2 3)" (check-equal? (smallest-tripled 1 2 3) 3))
  (test-case "(smallest-tripled -1 -1 0)" (check-equal? (smallest-tripled -1 -1 0) -3))
  (test-case "(smallest-tripled 30 20 10)" (check-equal? (smallest-tripled 30 20 10) 30))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite second-suite
  #:before (lambda () (display "Starting Test Suite 'second-suite'\n"))
  #:after (lambda () (display "Finished Test Suite 'second-suite'\n"))
  (test-case "(fact 1)" (check-equal? (fact 1) 1))
  (test-case "(fact 5)" (check-equal? (fact 5) 120))
  (test-case "(fact 20)" (check-equal? (fact 20) 2432902008176640000))
  (test-case "(comb 5 2)" (check-equal? (comb 5 2) 10))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-list (list
  first-suite
  second-suite
))

(provide (all-defined-out))

