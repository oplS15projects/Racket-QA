#lang racket

;; **********************************************************************
;; * - Name: Roy Van Liew
;; * - Section: 91.301.201 - Organization of Programming Languages
;; * - TEST AREA RUNNER
;; *   This script provide a GUI takeing a generated test area file from
;; *   converter-gui.rkt and runs it. It then tries to parse what
;; *   test cases failed, and then nicely combine these failed cases
;; *   into a list of strings to send in an email message.
;; **********************************************************************

(require racket/file)
(define nil '())

;; **********************************************************************
;; * Selectors for finding certain parts of the perl test case
;; **********************************************************************

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

;; The suite name lines have a > in the middle of them. They're before it.  
;; (parse-suite-name "ps1 > (comb 3 2)")
(define (parse-suite-name line)
  (define parse-result (regexp-match #rx"^(.*)>.*" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

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
    (if (equal? nil (car seq))
        nil
        (cons (map car seq) (helper (map cdr seq)))))
  (helper seq)
)

;; Gets the name of the suite (returns a string, not a list)
; (get-results-suite-name file-lines)
; "ps1"
(define (get-results-suite-name all-lines)
  (car (map parse-suite-name (filter is-test-name? all-lines))))

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

;; **********************************************************************
;; * Procedures for creating a list of strings to write out to file:
;; * - A list of strings representing the failed test case information
;; * - A list of strings representing the pass/fail rate
;; * - Combining these two lists
;; **********************************************************************

;; The sublist passed contains four pieces of information in this order:
;; Test case name, suite location, actual value, expected value.
(define (create-failed-case sublist)
  (string-append "> FAILED: '" (car sublist) "' in '" (cadr sublist) "'"
                 "\nactual: " (caddr sublist) "\nexpected: " (cadddr sublist) "\n"))

;; (create-list-of-failed-cases file-lines)
(define (create-list-of-failed-cases all-lines)
  (map create-failed-case (get-all-test-information all-lines)))

; Try (create-failed-cases-data num-failed num-tests suite-name)
(define (create-failed-cases-data num-failed-cases num-cases test-suite-name)
  (define num-passed-cases (- num-cases num-failed-cases))
  (define percent-failed (round (* (/ num-failed-cases num-cases) 100)))
  (define percent-passed (round (* (/ num-passed-cases num-cases) 100)))
  (define suite-result-header (list (string-append ">>> Results for test suite '" test-suite-name "'")
                                    (string-append "\n-> Total: " (number->string num-cases))
                                    (string-append "-> Passed: " (number->string num-passed-cases)
                                                   " (" (number->string percent-passed) "%)")
                                    (string-append "-> Failed: " (number->string num-failed-cases)
                                                   " (" (number->string percent-failed) "%)\n")
                                    )) ; end suite-result-header list and define
  suite-result-header
)

(define (create-failed-cases-lines all-lines failed-num total-num name-of-suite)
  (define failed-case-header (create-failed-cases-data failed-num total-num name-of-suite))
  (define failed-case-list (create-list-of-failed-cases all-lines))
  (append failed-case-header failed-case-list))

;; **********************************************************************
;; * MAIN: RUN THE SCRIPT
;; **********************************************************************

;; This line will run the tests
(require "ps1_area.rkt")

;; Read in the lines from the test results file
(define file-lines (file->lines "test_results.txt"))
(define failed-case-lines-to-write (create-failed-cases-lines file-lines num-failed num-tests suite-name))
(remake-file "C:\\Users\\Dossar\\AppData\\Roaming\\Racket QA\\QA Test Result\\test-result-email.txt")
(display-lines-to-file failed-case-lines-to-write "C:\\Users\\Dossar\\AppData\\Roaming\\Racket QA\\QA Test Result\\test-result-email.txt" #:separator"\n")
