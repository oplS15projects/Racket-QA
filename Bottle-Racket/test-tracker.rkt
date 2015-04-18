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

#||
 | This function allows us to recreate
 | existing files.
 | @param file-path Absolute path of file
 |                  to delete if it exists.
 |          
 | @return Arbitrary value. 0 if the file
 |         didn't exist.
 |#
(define (remake-file file-path)
  (if (file-exists? file-path) (delete-file file-path) 0))

#||
 | This function runs a given test area file.
 | @param full-test-area-path
 |        The absolute path to the test
 |        area file that runs the tests.
 |          
 | @return Arbitrary value. parse-test-results
 |         creates output files for the test
 |         results and the email to send out.
 |         However, note that this function
 |         does not actually send an email.
 |#
(define (run-test-area full-test-area-path)
  (current-directory (get-dirpath-from-filepath full-test-area-path))
  (define output-email-file-path (string-append (cleanse-path-string
                                  (string-append (get-dirpath-from-filepath full-test-area-path) "/"))
                                                "test-results-email.txt"))
  (define output-results-file-path (string-append (cleanse-path-string
                                     (string-append (get-dirpath-from-filepath full-test-area-path) "/"))
                                                "test-results.txt"))
  ;; This needs to be done for the system call to work properly for paths with spaces in them.
  (define fixed-full-test-area-path (cond ((eq? (system-type) 'windows) (valid-path-windows full-test-area-path))
                                          ((eq? (system-type) 'unix) (valid-path-linux full-test-area-path))
                                          ((eq? (system-type) 'macosx) (valid-path-linux full-test-area-path))
                                          (else (error "Platform not supported"))))
  (begin (remake-file output-email-file-path)
         (remake-file output-results-file-path)
         (system (string-append RACKET-PATH " " fixed-full-test-area-path))
         (parse-test-results (get-filename-from-filepath full-test-area-path)
                             output-results-file-path
                             output-email-file-path))
)

#||
 | This function runs a given test area file
 | and sends an email after.
 | @param full-test-area-path
 |        The absolute path to the test
 |        area file that runs the tests.
 | @param subject-field
 |        The Subject in the email to be sent.
 | @param mailing-list
 |        The mailing list returned from the
 |        email db mailing list dialog.
 |          
 | @return Arbitrary value. parse-test-results
 |         creates output files for the test
 |         results and the email to send out.
 |         This function sends an email out to
 |         the passed mailing list after the
 |         test area finishes running.
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
         ;; This needs to be done for the system call to work properly for paths with spaces in them.
         (define fixed-full-test-area-path (cond ((eq? (system-type) 'windows) (valid-path-windows full-test-area-path))
                                                 ((eq? (system-type) 'unix) (valid-path-linux full-test-area-path))
                                                 ((eq? (system-type) 'macosx) (valid-path-linux full-test-area-path))
                                                 (else (error "Platform not supported"))))
         (begin (remake-file output-email-file-path)
                (remake-file output-results-file-path)
                (system (string-append RACKET-PATH " " fixed-full-test-area-path))
                (parse-test-results (get-filename-from-filepath full-test-area-path)
                                    output-results-file-path
                                    output-email-file-path))
         (send-text-file to-field subject-field output-email-file-path recipients))
        (else "An email list was not selected."))
)

;; **********************************************************************
;; * Selectors for finding certain test components
;; **********************************************************************

#||
 | This function determines if a certain line in
 | the output test results file specifies test
 | statistics.
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line contains numbers at the
 |         start since that indicates test numbers,
 |         #f otherwise.
 |#
(define (is-passed-failed-info? line)
  (if (regexp-match #rx"^[0-9]+ success*" line) #t #f))

#||
 | This function determines if a certain line in
 | the output test results file specifies a
 | test name. e.g.
 | (is-test-name? "ps1 > (comb 3 2)")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line contains '>'
 |         in the middle, #f otherwise.
 |#
(define (is-test-name? line)
  (if (regexp-match #rx"^.*>.*" line) #t #f))

#||
 | This function determines if a certain line in
 | the output test results file specifies a
 | test case location in the suite. e.g.
 | (is-suite-location? "location:   ps1_suite.rkt:23:27")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line starts with 'location:',
 |         #f otherwise.
 |#
(define (is-suite-location? line)
  (if (regexp-match #rx"^\\s*location:.*" line) #t #f))

#||
 | This function determines if a certain line in
 | the output test results file specifies the
 | actual value returned in a test case. e.g.
 | (is-actual? "actual:     1")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line starts with 'actual:',
 |         #f otherwise.
 |#
(define (is-actual? line)
  (if (regexp-match #rx"^\\s*actual:.*" line) #t #f))

#||
 | This function determines if a certain line in
 | the output test results file specifies the
 | expected value in a test case. e.g.
 | (is-expected? "expected:   45")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line starts with 'expected:',
 |         #f otherwise.
 |#
(define (is-expected? line)
  (if (regexp-match #rx"^\\s*expected:.*" line) #t #f))

;; **********************************************************************
;; * Constructors for parsing information from failed test cases
;; **********************************************************************

;; The test name lines have a > symbol in the middle of them. They're after it.
#||
 | This function extracts the test case name
 | from the test case results file. e.g.
 | (parse-test-name "ps1 > (comb 3 2)")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return The test case name as a string,
 |         #f otherwise.
 |#
(define (parse-test-name line)
  (define parse-result (regexp-match #rx"^.*>(.*)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

#||
 | This function extracts the location in the
 | test suite file of where the case was run. e.g.
 | (parse-suite-location "location:   ps1_suite.rkt:23:27")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return The location in the test suite file
 |         as a string, #f otherwise.       
 |#
(define (parse-suite-location line)
  (define parse-result (regexp-match #rx"^\\s*location:(.*)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

#||
 | This function extracts the actual returned
 | value in a test case. e.g.
 | (parse-actual-value "actual:     1")
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The actual returned value
 |         as a string, #f otherwise.
 |#
(define (parse-actual-value line)
  (define parse-result (regexp-match #rx"^\\s*actual:(.*)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

#||
 | This function extracts the value that a
 | test case expected. e.g.
 | (parse-expected-value "expected:   45")
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The expected value as a string,
 |         #f otherwise.
 |#
(define (parse-expected-value line)
  (define parse-result (regexp-match #rx"^\\s*expected:(.*)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

;; **********************************************************************
;; * Procedures for retrieving all test case information, including
;; * test names, locations in the suite file, actual values, expected
;; * values, and the suite name.
;; **********************************************************************

#||
 | This function takes multiple lists and
 | creates sublists which are composed of
 | the list elements in parallel.
 | @param seq Lists to zip up in parallel.
 |          
 | @return The zipped list, containing
 |         sublists in parallel of the
 |         lists passed.
 |#
(define (zip . seq)
  (define (helper seq)
    (if (equal? '() (car seq))
        '()
        (cons (map car seq) (helper (map cdr seq)))))
  (helper seq)
)

#||
 | The classic map from SICP.
 | @param proc The procedure to apply to an element
 |             in the list before putting it into
 |             the new list.
 | @param items The list of elements to apply proc
 |              to and create a new list.
 |          
 | @return A new list with proc applied to every
 |         element of items.
 |#
(define (map proc items)
  (if (null? items)
      '() ;; This means we reached the end of the list
      (cons (proc (car items)) ;; Apply the procedure to the first item in the pair
            (map proc (cdr items))))) ;; Go down the sublists with "cdr"

#||
 | This function retrieves all the names
 | of the failed test cases in the test suite run.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings that has the
 |         names of all the failed test cases.
 |#
(define (get-all-test-cases all-lines)
  (map parse-test-name (filter is-test-name? all-lines)))

#||
 | This function retrieves all the locations of
 | the failed test cases in the test suite run.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings that has the
 |         locations of all the failed test
 |         cases in the test suite file.
 |#
(define (get-all-suite-locations all-lines)
  (map parse-suite-location (filter is-suite-location? all-lines)))

#||
 | This function retrieves all the actual returned
 | values of the failed test cases.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings that has the
 |         actual returned value of all the
 |         failed test cases.
 |#
(define (get-all-actual-values all-lines)
  (map parse-actual-value (filter is-actual? all-lines)))

#||
 | This function retrieves all the expected
 | values of the failed test cases.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings of the expected
 |         values for all the failed test cases.
 |#
(define (get-all-expected-values all-lines)
  (map parse-expected-value (filter is-expected? all-lines)))

#||
 | This function aligns the failed test case
 | information together in parallel in this order:
 | test case name, test case location in suite file,
 | actual returned value, and expected value.
 | @param all-lines The lines read in from
 |                  the Bottlenose file.
 |          
 | @return A list containing sublists each
 |         of length 4. This information is
 |         described above, and the test case
 |         information is aligned with its
 |         corresponding test case.
 |#
(define (get-all-test-information all-lines)
  (zip (get-all-test-cases all-lines)
       (get-all-suite-locations all-lines)
       (get-all-actual-values all-lines)
       (get-all-expected-values all-lines)))

#||
 | This function retrieves the lines from the test
 | results file that specify test statistics.
 | @param all-file-lines A list of strings, which
 |                       represent all the lines
 |                       in the test result file.
 |          
 | @return A new list of strings representing the
 |         statistics for the test suites run.
 |#
(define (get-passed-failed-info all-file-lines)
  (filter is-passed-failed-info? all-file-lines))

#||
 | This function extracts the number of
 | successful tests when the area ran.
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The number of successful tests,
 |         represented as a string.
 |#
(define (parse-successful-tests line)
  (define parse-result (regexp-match #rx"^([0-9]+) success*" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

#||
 | This function extracts the number of
 | failed tests when the area ran.
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The number of failed tests,
 |         represented as a string.
 |#
(define (parse-failed-tests line)
  (define parse-result (regexp-match #rx"([0-9]+) failure*" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

#||
 | This function extracts the total
 | number of tests when the area ran.
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The total number of tests ran,
 |         represented as a string.
 |#
(define (parse-total-tests line)
  (define parse-result (regexp-match #rx"([0-9]+) test\\(s\\)" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))

#||
 | This function extracts the number of
 | successful test cases for all suites
 | run in the area file.
 | @param pass-fail-list
 |        The list returned from
 |        get-passed-failed-info.
 |          
 | @return Number of successful tests
 |         across the test results file,
 |         as a list of strings.
 |#
(define (get-successful-tests pass-fail-list)
  (map parse-successful-tests pass-fail-list))

#||
 | This function extracts the number of
 | failed test cases for all suites
 | run in the area file.
 | @param pass-fail-list
 |        The list returned from
 |        get-passed-failed-info.
 |          
 | @return Number of failed tests
 |         across the test results file,
 |         as a list of strings.
 |#
(define (get-failed-tests pass-fail-list)
  (map parse-failed-tests pass-fail-list))

#||
 | This function extracts the total 
 | number of test cases for all suites
 | run in the area file.
 | @param pass-fail-list
 |        The list returned from
 |        get-passed-failed-info.
 |          
 | @return Total number of tests across
 |         the test results file, as a
 |         list of strings.
 |#
(define (get-total-tests pass-fail-list)
  (map parse-total-tests pass-fail-list))

;; **********************************************************************
;; * Procedures for retrieving test result statistics from the output
;; * file generated, using the get procedures defined above.
;; **********************************************************************

#||
 | This function is used to turn the numbers
 | represented as strings retrieved from
 | get-successful-tests, get-failed-tests,
 | and get-total-tests into actual numbers
 | to be accumulated in a new list.
 | @param list-of-num-strings
 |        The list returned from any of the
 |        three above mentioned functions.
 |          
 | @return A list of integers.
 |#
(define (strings-to-nums list-of-num-strings)
  (map string->number list-of-num-strings))

#||
 | This function is used to calculate
 | the successful, failed, and total
 | test cases.
 | @param passed-list
 |        A list of strings representing
 |        numbers.
 |          
 | @return An integer.
 |#
(define (add-list-of-string-nums passed-list)
  (accumulate (strings-to-nums passed-list) 0 + (lambda (x) x)))

;; **********************************************************************
;; * Procedures for creating a list of strings to write out to file:
;; * - A list of strings representing the failed test case information
;; * - A list of strings representing the pass/fail rate
;; * - Combining these two lists
;; **********************************************************************

#||
 | This function is used to create a string
 | that represents a description of a failed
 | test case to write out to an email file.
 | @param sublist
 |        A list of length 4 with the following:
 |        test case name, test case location,
 |        actual returned value, expected value.
 |          
 | @return A string with the description of
 |         a failed test case.
 |#
(define (create-failed-case sublist)
  (string-append "> FAILED: '" (car sublist) "' in '" (cadr sublist) "'"
                 "\nactual: " (caddr sublist) "\nexpected: " (cadddr sublist) "\n"))

#||
 | This function is used to create a list
 | of strings, each string containing
 | descriptions about each failed test case.
 | @param all-lines
 |        Every line read in from the
 |        test results file.
 |          
 | @return A list of strings with
 |         descriptions about each
 |         failed test case.
 |#
(define (create-list-of-failed-cases all-lines)
  (map create-failed-case (get-all-test-information all-lines)))

#||
 | This function creates the top part of the
 | email body, with an overview of the test
 | results as a string.
 | @param num-failed-cases
 |        The number of failed test cases.
 | @param num-cases
 |        The total number of test cases.
 | @param test-suite-name
 |        The name of the test suite run.
 |          
 | @return A string representing the top of
 |         the email body, with an overview
 |         of the test results.
 |#
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

#||
 | This function creates the entirery of
 | the email file to be sent.
 | @param all-lines
 |        Every line read in from the test
 |        results file.
 | @param failed-num
 |        The number of failed test cases.
 | @param total-num
 |        The total number of test cases.
 | @param name-of-suite
 |        The name of the test suite run.
 |          
 | @return A list of strings representing
 |         what to write out to the test
 |         result email file to be sent
 |         as an email body.
 |#
(define (create-failed-cases-lines all-lines failed-num total-num name-of-suite)
  (define failed-case-header (create-failed-cases-data failed-num total-num name-of-suite))
  (define failed-case-list (create-list-of-failed-cases all-lines))
  (append failed-case-header failed-case-list))

;; **********************************************************************
;; * PARSING TEST RESULTS
;; **********************************************************************

#||
 | This function looks through a test result file and
 | parses what failed and passed, and then creates the
 | output email file.
 | @param test-suite-name
 |        The name of the test suite run.
 | @param full-test-results-path
 |        Absolute path to the test result file.
 | @param output-email-file
 |        Absolute path to write the email body
 |        file to.
 |          
 | @return Arbitrary value. The important result
 |         is the output email file containing
 |         the email body to send.
 |#
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

(provide (all-defined-out))
