#lang racket

(require rackunit)
(require "ps3a.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite ps3a
  #:before (lambda () (display "Starting Test Suite 'ps3a'\n"))
  #:after (lambda () (display "Finished Test Suite 'ps3a'\n"))
  (test-case "(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))))" (check-equal? (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))) '(1 1)))
  (test-case "(area-rect (make-rect (make-point 1 1) (make-point 3 7)))" (check-equal? (area-rect (make-rect (make-point 1 1) (make-point 3 7))) 12))
  (test-case "(perimeter-rect (make-rect (make-point 1 1) (make-point 3 7)))" (check-equal? (perimeter-rect (make-rect (make-point 1 1) (make-point 3 7))) 16))
  (test-case "my-cdr" (check-equal? (my-cdr (my-cons 14 15)) 15))
  (test-case "p5" (check-equal? p4 #t))
  (test-case "(list-prod-iter '(1 2 3 4))" (check-equal? (list-prod-iter '(1 2 3 4)) 24))
  (test-case "(double-list1 '(1 2 3))" (check-equal? (double-list1 '(1 2 3)) '(2 4 6)))
  (test-case "(double-list2 '(1 2 3))" (check-equal? (double-list2 '(1 2 3)) '(2 4 6)))
  (test-case "(double-list2 '(1 2 3))" (check-equal? (double-list3 '(1 2 3)) '(2 4 6)))
  (test-case "(square-list1 '(1 2 3))" (check-equal? (square-list1 '(1 2 3)) '(1 4 9)))
  (test-case "(square-list2 '(1 2 3))" (check-equal? (square-list2 '(1 2 3)) '(1 4 9)))
  (test-case "(my-map square '(1 2 3))" (check-equal? (my-map square '(1 2 3)) '(1 4 9)))
  (test-case "(my-append '(1 2) '(3 4))" (check-equal? (my-append '(1 2) '(3 4)) '(1 2 3 4)))
  (test-case "(my-length '(1 2 3 4))" (check-equal? (my-length '(1 2 3 4)) 4))
  (test-case "(count-leaves '(1 (2 3) 4 (5 6)))" (check-equal? (count-leaves '(1 (2 3) 4 (5 6))) 6))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-list (list
  ps3a
))

(provide (all-defined-out))

