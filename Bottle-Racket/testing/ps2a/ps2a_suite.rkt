#lang racket

(require rackunit)
(require racket/include)
(require rackunit/text-ui)
(require rackunit/gui)
(require "ps2a.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite ps2a
  #:before (lambda () (display "Starting Test Suite 'ps2a'\n"))
  #:after (lambda () (display "Finished Test Suite 'ps2a'\n"))
  (test-case "add" (check-equal? add #t))
  (test-case "(f-recursive 2)" (check-equal? (f-recursive 2) 2))
  (test-case "(f-recursive 3)" (check-equal? (f-recursive 3) 4))
  (test-case "(f-recursive 4)" (check-equal? (f-recursive 4) 11))
  (test-case "(f-iterative 2)" (check-equal? (f-iterative 2) 2))
  (test-case "(f-iterative 3)" (check-equal? (f-iterative 3) 4))
  (test-case "(f-iterative 5)" (check-equal? (f-iterative 5) 25))
  (test-case "tail-recursion" (check-equal? tail-recursion #t))
  (test-case "(fast-expt 2 0)" (check-equal? (fast-expt 2 0) 1))
  (test-case "(fast-expt 2 1)" (check-equal? (fast-expt 2 1) 2))
  (test-case "(fast-expt 2 2)" (check-equal? (fast-expt 2 2) 4))
  (test-case "(fast-expt 2 3)" (check-equal? (fast-expt 2 3) 8))
  (test-case "(fast-expt 2 5)" (check-equal? (fast-expt 2 5) 32))
  (test-case "(sum square 2 inc 4)" (check-equal? (sum square 2 inc 4) 29))
  (test-case "(sum square 2 inc 2)" (check-equal? (sum square 2 inc 2) 4))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-list (list
  ps2a
))

(provide (all-defined-out))

