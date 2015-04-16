#lang racket

;; RackUnit and other project libraries
(require "../Common/user-settings-directory.rkt") ; For writing out test results
(require "../QA-Email/email.rkt"
         "../QA-Email/email-db.rkt"
         "../QA-Email/email-db-ui.rkt") ; For mailing test results
(require racket/include)
(require racket/file)
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

;; For determining racket path
(require setup/dirs)

(define RACKET-PATH-UNFIXED
   (string-append (path->string (find-console-bin-dir))
                  (cond ((eq? (system-type) 'windows) "racket.exe")
                        ((eq? (system-type) 'unix) "/racket")
                        ((eq? (system-type) 'macosx) "/racket")
                        (else (error "Platform not supported")))))

(define RACKET-PATH
   (cond ((eq? (system-type) 'windows) (valid-path-windows RACKET-PATH-UNFIXED))
         ((eq? (system-type) 'unix) (valid-path-linux RACKET-PATH-UNFIXED))
         ((eq? (system-type) 'macosx) (valid-path-linux RACKET-PATH-UNFIXED))
         (else (error "Platform not supported"))))

;; Function for recreating a file when running these tests
(define (remake-file file-path)
  (if (file-exists? file-path) (delete-file file-path) 0))

;; Runs a specified test area file, absolute full directory.
;; (run-test-area testing-area-path)
(define (run-test-area full-test-area-path)
  (current-directory (get-dirpath-from-filepath full-test-area-path))
  (define output-email-file-path (string-append (cleanse-path-string
                                  (string-append (get-dirpath-from-filepath full-test-area-path) "/"))
                                                "test-results-email.txt"))
  (define output-results-file-path (string-append (cleanse-path-string
                                     (string-append (get-dirpath-from-filepath full-test-area-path) "/"))
                                                "test-results.txt"))
  (begin (remake-file output-email-file-path)
         (remake-file output-results-file-path)
         ;(system (string-append "racket " full-test-area-path))
         (system (string-append RACKET-PATH " " full-test-area-path))
         (parse-test-results (get-filename-from-filepath full-test-area-path)
                             output-results-file-path
                             output-email-file-path))
)

;; Runs a specified test area file, absolute full directory along with mailist list.
#|
(run-test-area-email testing-area-path
                     (string-append "Regression Results: " testing-suite-name)
                     (open-manage-mailing-list-dialog 'return-db))
|#
(define (run-test-area-email full-test-area-path subject-field mailing-list)
  (define mail-list-id (email-db-id mailing-list))
  (define to-field (email-db-name mailing-list))
  (define recipients (email-db-addresses mailing-list))
  (cond ((not (equal? #f mailing-list))
         (current-directory (get-dirpath-from-filepath full-test-area-path))
         (define output-email-file-path (string-append (cleanse-path-string
                                                        (string-append (get-dirpath-from-filepath full-test-area-path) "/"))
                                                       "test-results-email.txt"))
         (define output-results-file-path (string-append (cleanse-path-string
                                                          (string-append (get-dirpath-from-filepath full-test-area-path) "/"))
                                                         "test-results.txt"))
         (begin (remake-file output-email-file-path)
                (remake-file output-results-file-path)
                ;(system (string-append "racket " full-test-area-path))
                (system (string-append RACKET-PATH " " full-test-area-path))
                (parse-test-results (get-filename-from-filepath full-test-area-path)
                                    output-results-file-path
                                    output-email-file-path))
         (send-text-file to-field subject-field output-email-file-path recipients))
        (else "An email list was not selected."))
)

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

#|
;; This definition of accumulate was taken from the SICP reading.
(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))
|#

;; This definition of map was taken from the SICP reading.
(define (map proc items)
  (if (null? items)
      '() ;; This means we reached the end of the list
      (cons (proc (car items)) ;; Apply the procedure to the first item in the pair
            (map proc (cdr items))))) ;; Go down the sublists with "cdr"

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

;; **********************************************************************
;; * Procedures for retrieving test result statistics from the output
;; * file generated, using the get procedures defined above.
;; **********************************************************************

(define (strings-to-nums list-of-num-strings)
  (map string->number list-of-num-strings))

; Takes a list of strings, each string representing numbers parsed from
; each test suite. e.g. '("26" "3")
(define (add-list-of-string-nums passed-list)
  (accumulate (strings-to-nums passed-list) 0 + (lambda (x) x)))

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
  (define suite-result-header (list (string-append ">>> Results for test area file '" test-suite-name "'")
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
;; * PARSING TEST RESULTS
;; **********************************************************************

;; Looks through a test result file and parses what failed and passed, and
;; creates the output-email file specified.
;; (parse-test-results testing-suite-name testing-test-result-path testing-output-result-path)
(define (parse-test-results test-suite-name full-test-results-path output-email-file)
  ;; Analyze test result file lines
  (define test-results-lines (file->lines full-test-results-path))
  (define pass-fail-info (get-passed-failed-info test-results-lines))
  (define successful-list (get-successful-tests pass-fail-info))
  (define failed-list (get-failed-tests pass-fail-info))
  (define total-list (get-total-tests pass-fail-info))
  ;; Here is where we actually got the physical statistics
  (define num-passed (add-list-of-string-nums successful-list))
  (define num-failed (add-list-of-string-nums failed-list))
  (define num-total (add-list-of-string-nums total-list))
  ;; Get file lines to write out to the output file to send as an email
  (define failed-case-lines-to-write (create-failed-cases-lines
                                      test-results-lines num-failed num-total test-suite-name))
  ;; Now write the processed lines above out to file.
  (begin (remake-file output-email-file)
         (display-lines-to-file failed-case-lines-to-write output-email-file #:separator"\n"))
)

;; **********************************************************************
;; * TEST MAIN
;; **********************************************************************

(define testing-area-path "C:\\OPL\\Racket-QA\\Racket-QA\\Bottle-Racket\\testing\\apitest\\test-area.rkt")
(define testing-test-result-path "C:\\OPL\\Racket-QA\\Racket-QA\\Bottle-Racket\\testing\\apitest\\test-results.txt")
(define testing-output-result-path "C:\\OPL\\Racket-QA\\Racket-QA\\Bottle-Racket\\testing\\apitest\\test-results-email.txt")
(define testing-suite-name "api-test")

#|
;; map is used here to allow each test suite to be run in the textual interface.
(remake-file test-result-path)
(define test-result-raw-output (open-output-file test-result-path))
(current-error-port test-result-raw-output) ; File containing test information if any cases failed
(current-output-port test-result-raw-output) ; File containing test information if all cases successful
(map run-tests test-list)

;; Close the output ports after running the tests
(close-output-port test-result-raw-output)
|#

#|
;; The location of the suite file starts with "location:". For instance
;; (is-suite-location? "location:   ps1_suite.rkt:23:27")
(define raw-test-lines (file->lines test-result-path))

(define pass-fail-info (get-passed-failed-info raw-test-lines))
(define successful-list (get-successful-tests pass-fail-info))
(define failed-list (get-failed-tests pass-fail-info))
(define total-list (get-total-tests pass-fail-info))

;; Now for the raw statistics
(define num-passed (add-list-of-string-nums successful-list))
(define num-failed (add-list-of-string-nums failed-list))
(define num-total (add-list-of-string-nums total-list))
;(define percent-passed (round (* (/ num-passed num-total) 100)))
;(define percent-failed (round (* (/ num-failed num-total) 100)))
|#




(provide (all-defined-out))

