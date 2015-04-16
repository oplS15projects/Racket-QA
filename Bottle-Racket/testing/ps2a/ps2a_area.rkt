#lang racket

;; Racket Unit Testing Libraries
(require racket/include)
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

;; Suite file for this assignment
(define suite-name "ps2a")
(require "ps2a_suite.rkt")

;; Function for recreating a file when running these tests
(define (remake-file file-path)
  (if (file-exists? file-path)
      (delete-file file-path) 0))

;; map is used here to allow each test suite to be run in the textual interface.
(remake-file "test_results_stderr.txt")
(define test-result-stderr (open-output-file "test_results_stderr.txt"))
(remake-file "test_results_stdout.txt")
(define test-result-stdout (open-output-file "test_results_stdout.txt"))
(current-error-port test-result-stderr) ; File containing test information if any cases failed
(current-output-port test-result-stdout) ; File containing test information if all cases successful
(map run-tests test-list)

;(define num-tests 15)
;(define num-failed (car (map run-tests test-list))) ; run-tests returns list with number of failed cases
;(define num-passed (- num-tests num-failed)) ; How many passed
;(define failed (/ num-failed num-tests))
;(define successful (/ num-passed num-tests))

;; Close the output ports after running the tests
(close-output-port test-result-stderr)
(close-output-port test-result-stdout)

(provide (all-defined-out))

