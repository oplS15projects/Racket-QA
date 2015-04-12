#lang racket

;; Racket Unit Testing Libraries
(require racket/include)
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

;; Suite file for this assignment
(define suite-name "ps3a")
(require "ps3a_suite.rkt")

;; Function for recreating a file when running these tests
(define (remake-file file-path)
  (if (file-exists? file-path)
      (delete-file file-path) 0))

;; map is used here to allow each test suite to be run in the textual interface.
(remake-file "test_results.txt")
(define test-result-file (open-output-file "test_results.txt"))
(current-error-port test-result-file) ; File containing failed cases
(define num-tests 15)
(define num-failed (car (map run-tests test-list))) ; run-tests returns list with number of failed cases
(define num-passed (- num-tests num-failed)) ; How many passed
(define failed (/ num-failed num-tests))
(define successful (/ num-passed num-tests))
(close-output-port test-result-file)

(provide (all-defined-out))

