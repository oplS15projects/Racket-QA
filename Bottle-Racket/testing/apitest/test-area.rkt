#lang racket

;; Racket Unit Testing Libraries
(require racket/include)
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

;; Function for recreating a file when running these tests
(define (remake-file file-path)
  (if (file-exists? file-path) (delete-file file-path) 0))

;; Suite file to run
(require "test-suite.rkt")
(define test-result-raw-output (open-output-file "test-results.txt"))
(current-error-port test-result-raw-output) ; File containing test information if any cases failed
(current-output-port test-result-raw-output) ; File containing test information if all cases successful
(map run-tests test-list)
(close-output-port test-result-raw-output)

(provide (all-defined-out))

