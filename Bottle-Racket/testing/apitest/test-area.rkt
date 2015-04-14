#lang racket

;; Racket Unit Testing Libraries
(require racket/include)
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

;; Suite file for this assignment
(define suite-name "testing-for-api")
(require "test-suite.rkt")

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


;; The location of the suite file starts with "location:". For instance
;; (is-suite-location? "location:   ps1_suite.rkt:23:27")
(define stupid "3 success(es) 0 failure(s) 0 error(s) 3 test(s) run")
(define sample-err-file "C:/OPL/Racket-QA/Racket-QA/Bottle-Racket/testing/apitest/test_results_stderr.txt")
(define sample-out-file "C:/OPL/Racket-QA/Racket-QA/Bottle-Racket/testing/apitest/test_results_stdout.txt")
(define sample-err-lines (file->lines sample-err-file))
(define sample-out-lines (file->lines sample-out-file))

;; **********************************************************************
;; * Selectors for finding certain test components
;; **********************************************************************

(define (is-passed-failed-info? line)
  (if (regexp-match #rx"^[0-9]+ success*" line) #t #f))

;; The test name lines have a > symbol in the middle of them. For instance
;; (is-test-name? "ps1 > (comb 3 2)")
(define (is-test-name? line)
  (if (regexp-match #rx"^.*>.*" line) #t #f))

;; The location of the suite file starts with "location:". For instance
;; (is-suite-location? "location:   ps1_suite.rkt:23:27")
(define (is-suite-location? line)
  (if (regexp-match #rx"^\\s*location:.*" line) #t #f))

;; The actual values start with "actual:". For instance
;; (is-actual? "actual:     1")
(define (is-actual? line)
  (if (regexp-match #rx"^\\s*actual:.*" line) #t #f))

;; The expected values start with "expected:". For instance
;; (is-expected? "expected:   45")
(define (is-expected? line)
  (if (regexp-match #rx"^\\s*expected:.*" line) #t #f))

;; **********************************************************************
;; * Constructors for parsing information from failed test cases
;; **********************************************************************

;; The test name lines have a > symbol in the middle of them. They're after it.
;; (parse-test-name "ps1 > (comb 3 2)")
(define (return-passed-failed-line line)
  (define parse-result (regexp-match #rx"^([0-9]+ success*)" line))
  (if (not (equal? parse-result #f)) (string-trim line) #f))

;; The test name lines have a > symbol in the middle of them. They're after it.
;; (parse-test-name "ps1 > (comb 3 2)")
(define (parse-test-name line)
  (define parse-result (regexp-match #rx"^.*>(.*)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

;; The location of the suite file starts with "location:"
;; (parse-suite-location "location:   ps1_suite.rkt:23:27")
(define (parse-suite-location line)
  (define parse-result (regexp-match #rx"^\\s*location:(.*)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

;; The actual values start with "actual:"
;; (parse-actual-value "actual:     1")
(define (parse-actual-value line)
  (define parse-result (regexp-match #rx"^\\s*actual:(.*)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

;; The expected values start with "expected:"
;; (parse-expected-value "expected:   45")
(define (parse-expected-value line)
  (define parse-result (regexp-match #rx"^\\s*expected:(.*)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))



;; **********************************************************************
;; * Procedures for retrieving all test case information, including
;; * test names, locations in the suite file, actual values, expected
;; * values, and the suite name.
;; **********************************************************************

(define (zip . seq)
  (define (helper seq)
    (if (equal? '() (car seq))
        '()
        (cons (map car seq) (helper (map cdr seq)))))
  (helper seq)
)

;; Gets all the names of the failed test cases.
; (get-all-test-cases file-lines)
; '("(comb 3 2)" "(comb 4 2)" "(comb 10 2)" "(comb 93 37)")
(define (get-all-test-cases all-lines)
  (map parse-test-name (filter is-test-name? all-lines)))

;; Gets all the locations in the suite file where the test case was run.
; (get-all-suite-locations file-lines)
; '("ps1_suite.rkt:21:26" "ps1_suite.rkt:22:26" "ps1_suite.rkt:23:27" "ps1_suite.rkt:24:28")
(define (get-all-suite-locations all-lines)
  (map parse-suite-location (filter is-suite-location? all-lines)))

;; Gets all the actual values in the failed test cases.
; (get-all-actual-values file-lines)
; '("1" "1" "1" "1")
(define (get-all-actual-values all-lines)
  (map parse-actual-value (filter is-actual? all-lines)))

;; Gets all the expected values in the failed test cases.
; (get-all-expected-values file-lines)
; '("3" "6" "45" "118206769052646517220135262")
(define (get-all-expected-values all-lines)
  (map parse-expected-value (filter is-expected? all-lines)))

;; Put in order the test case names, suite location, actual values, and expected values.
;; The results file for the suite is to be used separately from this list.
; '(("(comb 3 2)" "ps1_suite.rkt:21:26" "1" "3")
;  ("(comb 4 2)" "ps1_suite.rkt:22:26" "1" "6")
;  ("(comb 10 2)" "ps1_suite.rkt:23:27" "1" "45")
;  ("(comb 93 37)" "ps1_suite.rkt:24:28" "1" "118206769052646517220135262"))
(define (get-all-test-information all-lines)
  (zip (get-all-test-cases all-lines)
       (get-all-suite-locations all-lines)
       (get-all-actual-values all-lines)
       (get-all-expected-values all-lines)))

; Retrieves all passed-failed test suite info.
(define (get-passed-failed-info all-file-lines)
  (filter is-passed-failed-info? all-file-lines))

(define (parse-successful-tests line)
  (define parse-result (regexp-match #rx"^([0-9]+) success*" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

(define (parse-failed-tests line)
  (define parse-result (regexp-match #rx"([0-9]+) failure*" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

(define (parse-total-tests line)
  (define parse-result (regexp-match #rx"([0-9]+) test\\(s\\)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

(define (get-successful-tests pass-fail-list)
  (map parse-successful-tests pass-fail-list))

(define (get-failed-tests pass-fail-list)
  (map parse-failed-tests pass-fail-list))

(define (get-total-tests pass-fail-list)
  (map parse-total-tests pass-fail-list))

;(define (find-total-tests-run output-file)
;  (define test-info-lines 

(define pass-fail-info (get-passed-failed-info sample-err-lines))
(define successful-list (get-successful-tests pass-fail-info))
(define failed-list (get-failed-tests pass-fail-info))
(define total-list (get-total-tests pass-fail-info))

(provide (all-defined-out))

