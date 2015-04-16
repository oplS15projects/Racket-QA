#lang racket

;; Racket Unit Testing Libraries
(require racket/include)
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

;; Suite file to run
(require "ps1_suite.rkt")
(define test-result-raw-output (open-output-file "test-results.txt"))
(current-error-port test-result-raw-output) ; File containing test information
(current-output-port test-result-raw-output)
(map run-tests test-list) ; The tests are run with this line
(close-output-port test-result-raw-output)

(provide (all-defined-out))

