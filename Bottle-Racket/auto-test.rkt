#lang racket

(require "test-area-runner.rkt")

(define main-path "C:/OPL/Racket-QA/Racket-QA/Bottle-Racket/testing/apitest")
(define area-path "C:/OPL/Racket-QA/Racket-QA/Bottle-Racket/testing/apitest/test-area.rkt")
(define test-result-path "C:/OPL/Racket-QA/Racket-QA/Bottle-Racket/testing/apitest/test_results.txt")
(define output-result-path "C:/OPL/Racket-QA/Racket-QA/Bottle-Racket/testing/apitest/test-results-email.txt")
(define suite-name "api-test")

;; Function for recreating a file when running these tests
(define (remake-file file-path)
  (if (file-exists? file-path) (delete-file file-path) 0))

;; Runs a specified test area file, absolute full directory.
(define (run-test-area full-test-area-path)
  (system (string-append "racket " full-test-area-path)))

;; Looks through a test result file and parses what failed and passed, and
;; creates the output-email file specified.
#|
(define (parse-test-results test-suite-name full-test-results-path output-email-file)
  (define test-results-lines (file->lines full-test-results-path))
  (define failed-case-lines-to-write (create-failed-cases-lines test-results-lines num-failed num-tests suite-name))
)
|#
