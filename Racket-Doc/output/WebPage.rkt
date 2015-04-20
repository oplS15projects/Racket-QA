;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  								;;
;; AUTO-GENERATED CODE.  To run, either open in Dr. Racket and  ;;
;; select the "Run" button, or open a terminal, go to the 	;;
;; directory containing this file, and run			;;
;; "racket [file_name].rkt".					;;
;;                                                              ;;
;; Date Generated On: 4/19/2015                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#lang racket

(require web-server/servlet web-server/servlet-env)
(require racket/gui)


;(define generat

;(play-sound "./share/rach.wav" #f)

;(define logo
;  (read-bitmap "./../share/button.jpg"))

(define (start request)
  (main-page request))

;"main" page:
(define (main-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
                    (body (h1 (center "Racket-Doc Home"))
                          (center
                          (a ((href, (embed/url fileNameList-page))) "File List")
                          (html nbsp nbsp nbsp nbsp)
                          (a ((href, (embed/url required-page))) "Required")
                          (html nbsp nbsp nbsp nbsp)
                          (a ((href, (embed/url provided-page))) "Provided")
                          (html nbsp nbsp nbsp nbsp)
                          (a ((href, (embed/url procAndData-page))) "Procedures & Data")
                          (html nbsp nbsp nbsp nbsp)
                          (a ((href, (embed/url help-page))) "Help"))
                          )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying file list
(define (fileNameList-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "*.rkt Files")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (p (b "Files:"))
                     (a ((href, (embed/url TestTwo.rkt-page))) "TestTwo.rkt")
                     (br)(br)
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for a specified file
(define (TestTwo.rkt-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "*.rkt Files")
                     (center
                      (a ((href ,(embed/url main-page))) "Home")
                      (html nbsp nbsp nbsp nbsp)
                      )
                     (br)(br)
                     (p "Specified File page")
                     ;add requires
                     (b "Required")
                     (fieldset (code (list "#lang racket" (br) "(require \"../Common/user-settings-directory.rkt\") ; For writing out test results" (br) "(require \"../QA-Email/email.rkt\"" (br) "(require racket/include)" (br) "(require racket/file)" (br) "(require rackunit)" (br) "(require rackunit/text-ui)" (br) "(require rackunit/gui)" (br) "(require setup/dirs)" (br) )))
                     (br) (br) (br)
                     ;add included
                     (b "Included")
                     (fieldset (code (list )))
                     (br) (br) (br)
                     ;add provided
                     (b "Provided")
                     (fieldset (code (list "(provide (all-defined-out))" (br) )))
                     (br) (br) (br)
                     ;;add procs and data
                     (b "Procedures & Data")
                     (fieldset
                      (code (list (b "(define RACKET-PATH-UNFIXED") (br)
                             (i "#||
 | This function...
 | does something...
 |#") (br))))
                     (a ((href, (embed/url codeblock0-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define RACKET-PATH") (br)
                             (i "#||
 | This function...
 | does something
 |#") (br))))
                     (a ((href, (embed/url codeblock1-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (remake-file file-path)") (br)
                             (i "#||
 | This function allows us to recreate
 | existing files.
 | @param file-path Absolute path of file
 |                  to delete if it exists.
 |          
 | @return Arbitrary value. 0 if the file
 |         didn't exist.
 |#") (br))))
                     (a ((href, (embed/url codeblock2-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (run-test-area full-test-area-path)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock3-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (run-test-area-email full-test-area-path subject-field mailing-list)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock4-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-passed-failed-info? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies test
 | statistics.
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line contains numbers at the
 |         start since that indicates test numbers,
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock5-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-test-name? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies a
 | test name. e.g.
 | (is-test-name? \"ps1 > (comb 3 2)\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line contains '>'
 |         in the middle, #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock6-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-suite-location? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies a
 | test case location in the suite. e.g.
 | (is-suite-location? \"location:   ps1_suite.rkt:23:27\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line starts with 'location:',
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock7-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-actual? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies the
 | actual value returned in a test case. e.g.
 | (is-actual? \"actual:     1\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line starts with 'actual:',
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock8-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-expected? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies the
 | expected value in a test case. e.g.
 | (is-expected? \"expected:   45\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line starts with 'expected:',
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock9-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-test-name line)") (br)
                             (i "#||
 | This function extracts the test case name
 | from the test case results file. e.g.
 | (parse-test-name \"ps1 > (comb 3 2)\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return The test case name as a string,
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock10-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-suite-location line)") (br)
                             (i "#||
 | This function extracts the location in the
 | test suite file of where the case was run. e.g.
 | (parse-suite-location \"location:   ps1_suite.rkt:23:27\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return The location in the test suite file
 |         as a string, #f otherwise.       
 |#") (br))))
                     (a ((href, (embed/url codeblock11-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-actual-value line)") (br)
                             (i "#||
 | This function extracts the actual returned
 | value in a test case. e.g.
 | (parse-actual-value \"actual:     1\")
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The actual returned value
 |         as a string, #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock12-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-expected-value line)") (br)
                             (i "#||
 | This function extracts the value that a
 | test case expected. e.g.
 | (parse-expected-value \"expected:   45\")
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The expected value as a string,
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock13-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (zip . seq)") (br)
                             (i "#||
 | This function takes multiple lists and
 | creates sublists which are composed of
 | the list elements in parallel.
 | @param seq Lists to zip up in parallel.
 |          
 | @return The zipped list, containing
 |         sublists in parallel of the
 |         lists passed.
 |#") (br))))
                     (a ((href, (embed/url codeblock14-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (map proc items)") (br)
                             (i "#||
 | The classic map from SICP.
 | @param proc The procedure to apply to an element
 |             in the list before putting it into
 |             the new list.
 | @param items The list of elements to apply proc
 |              to and create a new list.
 |          
 | @return A new list with proc applied to every
 |         element of items.
 |#") (br))))
                     (a ((href, (embed/url codeblock15-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-test-cases all-lines)") (br)
                             (i "#||
 | This function retrieves all the names
 | of the failed test cases in the test suite run.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings that has the
 |         names of all the failed test cases.
 |#") (br))))
                     (a ((href, (embed/url codeblock16-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-suite-locations all-lines)") (br)
                             (i "#||
 | This function retrieves all the locations of
 | the failed test cases in the test suite run.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings that has the
 |         locations of all the failed test
 |         cases in the test suite file.
 |#") (br))))
                     (a ((href, (embed/url codeblock17-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-actual-values all-lines)") (br)
                             (i "#||
 | This function retrieves all the actual returned
 | values of the failed test cases.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings that has the
 |         actual returned value of all the
 |         failed test cases.
 |#") (br))))
                     (a ((href, (embed/url codeblock18-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-expected-values all-lines)") (br)
                             (i "#||
 | This function retrieves all the expected
 | values of the failed test cases.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings of the expected
 |         values for all the failed test cases.
 |#") (br))))
                     (a ((href, (embed/url codeblock19-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-test-information all-lines)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock20-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-passed-failed-info all-file-lines)") (br)
                             (i "#||
 | This function retrieves the lines from the test
 | results file that specify test statistics.
 | @param all-file-lines A list of strings, which
 |                       represent all the lines
 |                       in the test result file.
 |          
 | @return A new list of strings representing the
 |         statistics for the test suites run.
 |#") (br))))
                     (a ((href, (embed/url codeblock21-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-successful-tests line)") (br)
                             (i "#||
 | This function extracts the number of
 | successful tests when the area ran.
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The number of successful tests,
 |         represented as a string.
 |#") (br))))
                     (a ((href, (embed/url codeblock22-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-failed-tests line)") (br)
                             (i "#||
 | This function extracts the number of
 | failed tests when the area ran.
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The number of failed tests,
 |         represented as a string.
 |#") (br))))
                     (a ((href, (embed/url codeblock23-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-total-tests line)") (br)
                             (i "#||
 | This function extracts the total
 | number of tests when the area ran.
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The total number of tests ran,
 |         represented as a string.
 |#") (br))))
                     (a ((href, (embed/url codeblock24-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-successful-tests pass-fail-list)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock25-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-failed-tests pass-fail-list)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock26-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-total-tests pass-fail-list)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock27-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (strings-to-nums list-of-num-strings)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock28-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (add-list-of-string-nums passed-list)") (br)
                             (i "#||
 | This function is used to calculate
 | the successful, failed, and total
 | test cases.
 | @param passed-list
 |        A list of strings representing
 |        numbers.
 |          
 | @return An integer.
 |#") (br))))
                     (a ((href, (embed/url codeblock29-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (create-failed-case sublist)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock30-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (create-list-of-failed-cases all-lines)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock31-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (create-failed-cases-data num-failed-cases num-cases test-suite-name)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock32-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (create-failed-cases-lines all-lines failed-num total-num name-of-suite)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock33-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-test-results test-suite-name full-test-results-path output-email-file)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock34-page))) "Code")
                             (br) (br) (br)
                         )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying dependencies
(define (required-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Dependencies")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (b "Required")
                     (fieldset (code (list "#lang racket" (br) "(require \"../Common/user-settings-directory.rkt\") ; For writing out test results" (br) "(require \"../QA-Email/email.rkt\"" (br) "(require racket/include)" (br) "(require racket/file)" (br) "(require rackunit)" (br) "(require rackunit/text-ui)" (br) "(require rackunit/gui)" (br) "(require setup/dirs)" (br) )))
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying provideds
(define (provided-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Provided")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (b "Provided")
                     (fieldset (code (list "(provide (all-defined-out))" (br) )))
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying procs and data of a single file
(define (procAndData-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedures & Data")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     ;;add procs and data
                     (br) (br)
                     (fieldset
                      (code (list (b "(define RACKET-PATH-UNFIXED") (br)
                             (i "#||
 | This function...
 | does something...
 |#") (br))))
                     (a ((href, (embed/url codeblock0-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define RACKET-PATH") (br)
                             (i "#||
 | This function...
 | does something
 |#") (br))))
                     (a ((href, (embed/url codeblock1-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (remake-file file-path)") (br)
                             (i "#||
 | This function allows us to recreate
 | existing files.
 | @param file-path Absolute path of file
 |                  to delete if it exists.
 |          
 | @return Arbitrary value. 0 if the file
 |         didn't exist.
 |#") (br))))
                     (a ((href, (embed/url codeblock2-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (run-test-area full-test-area-path)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock3-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (run-test-area-email full-test-area-path subject-field mailing-list)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock4-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-passed-failed-info? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies test
 | statistics.
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line contains numbers at the
 |         start since that indicates test numbers,
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock5-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-test-name? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies a
 | test name. e.g.
 | (is-test-name? \"ps1 > (comb 3 2)\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line contains '>'
 |         in the middle, #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock6-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-suite-location? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies a
 | test case location in the suite. e.g.
 | (is-suite-location? \"location:   ps1_suite.rkt:23:27\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line starts with 'location:',
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock7-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-actual? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies the
 | actual value returned in a test case. e.g.
 | (is-actual? \"actual:     1\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line starts with 'actual:',
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock8-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (is-expected? line)") (br)
                             (i "#||
 | This function determines if a certain line in
 | the output test results file specifies the
 | expected value in a test case. e.g.
 | (is-expected? \"expected:   45\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return #t if the line starts with 'expected:',
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock9-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-test-name line)") (br)
                             (i "#||
 | This function extracts the test case name
 | from the test case results file. e.g.
 | (parse-test-name \"ps1 > (comb 3 2)\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return The test case name as a string,
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock10-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-suite-location line)") (br)
                             (i "#||
 | This function extracts the location in the
 | test suite file of where the case was run. e.g.
 | (parse-suite-location \"location:   ps1_suite.rkt:23:27\")
 | @param line A line from the test results file
 |             as a string.
 |          
 | @return The location in the test suite file
 |         as a string, #f otherwise.       
 |#") (br))))
                     (a ((href, (embed/url codeblock11-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-actual-value line)") (br)
                             (i "#||
 | This function extracts the actual returned
 | value in a test case. e.g.
 | (parse-actual-value \"actual:     1\")
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The actual returned value
 |         as a string, #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock12-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-expected-value line)") (br)
                             (i "#||
 | This function extracts the value that a
 | test case expected. e.g.
 | (parse-expected-value \"expected:   45\")
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The expected value as a string,
 |         #f otherwise.
 |#") (br))))
                     (a ((href, (embed/url codeblock13-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (zip . seq)") (br)
                             (i "#||
 | This function takes multiple lists and
 | creates sublists which are composed of
 | the list elements in parallel.
 | @param seq Lists to zip up in parallel.
 |          
 | @return The zipped list, containing
 |         sublists in parallel of the
 |         lists passed.
 |#") (br))))
                     (a ((href, (embed/url codeblock14-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (map proc items)") (br)
                             (i "#||
 | The classic map from SICP.
 | @param proc The procedure to apply to an element
 |             in the list before putting it into
 |             the new list.
 | @param items The list of elements to apply proc
 |              to and create a new list.
 |          
 | @return A new list with proc applied to every
 |         element of items.
 |#") (br))))
                     (a ((href, (embed/url codeblock15-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-test-cases all-lines)") (br)
                             (i "#||
 | This function retrieves all the names
 | of the failed test cases in the test suite run.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings that has the
 |         names of all the failed test cases.
 |#") (br))))
                     (a ((href, (embed/url codeblock16-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-suite-locations all-lines)") (br)
                             (i "#||
 | This function retrieves all the locations of
 | the failed test cases in the test suite run.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings that has the
 |         locations of all the failed test
 |         cases in the test suite file.
 |#") (br))))
                     (a ((href, (embed/url codeblock17-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-actual-values all-lines)") (br)
                             (i "#||
 | This function retrieves all the actual returned
 | values of the failed test cases.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings that has the
 |         actual returned value of all the
 |         failed test cases.
 |#") (br))))
                     (a ((href, (embed/url codeblock18-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-expected-values all-lines)") (br)
                             (i "#||
 | This function retrieves all the expected
 | values of the failed test cases.
 | @param all-lines Every line read in from the test
 |                  results file, as a string.
 |          
 | @return A new list of strings of the expected
 |         values for all the failed test cases.
 |#") (br))))
                     (a ((href, (embed/url codeblock19-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-all-test-information all-lines)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock20-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-passed-failed-info all-file-lines)") (br)
                             (i "#||
 | This function retrieves the lines from the test
 | results file that specify test statistics.
 | @param all-file-lines A list of strings, which
 |                       represent all the lines
 |                       in the test result file.
 |          
 | @return A new list of strings representing the
 |         statistics for the test suites run.
 |#") (br))))
                     (a ((href, (embed/url codeblock21-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-successful-tests line)") (br)
                             (i "#||
 | This function extracts the number of
 | successful tests when the area ran.
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The number of successful tests,
 |         represented as a string.
 |#") (br))))
                     (a ((href, (embed/url codeblock22-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-failed-tests line)") (br)
                             (i "#||
 | This function extracts the number of
 | failed tests when the area ran.
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The number of failed tests,
 |         represented as a string.
 |#") (br))))
                     (a ((href, (embed/url codeblock23-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-total-tests line)") (br)
                             (i "#||
 | This function extracts the total
 | number of tests when the area ran.
 | @param line A line from the test results
 |             file as a string.
 |          
 | @return The total number of tests ran,
 |         represented as a string.
 |#") (br))))
                     (a ((href, (embed/url codeblock24-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-successful-tests pass-fail-list)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock25-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-failed-tests pass-fail-list)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock26-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (get-total-tests pass-fail-list)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock27-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (strings-to-nums list-of-num-strings)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock28-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (add-list-of-string-nums passed-list)") (br)
                             (i "#||
 | This function is used to calculate
 | the successful, failed, and total
 | test cases.
 | @param passed-list
 |        A list of strings representing
 |        numbers.
 |          
 | @return An integer.
 |#") (br))))
                     (a ((href, (embed/url codeblock29-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (create-failed-case sublist)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock30-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (create-list-of-failed-cases all-lines)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock31-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (create-failed-cases-data num-failed-cases num-cases test-suite-name)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock32-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (create-failed-cases-lines all-lines failed-num total-num name-of-suite)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock33-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (parse-test-results test-suite-name full-test-results-path output-email-file)") (br)
                             (i "#||
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
 |#") (br))))
                     (a ((href, (embed/url codeblock34-page))) "Code")
                             (br) (br) (br)
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock0-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "   (string-append (path->string (find-console-bin-dir))
                  (cond ((eq? (system-type) 'windows) \"racket.exe\")
                        ((eq? (system-type) 'unix) \"/racket\")
                        ((eq? (system-type) 'macosx) \"/racket\")
                        (else (error \"Platform not supported\")))))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock1-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "   (cond ((eq? (system-type) 'windows) (valid-path-windows RACKET-PATH-UNFIXED))
         ((eq? (system-type) 'unix) (valid-path-linux RACKET-PATH-UNFIXED))
         ((eq? (system-type) 'macosx) (valid-path-linux RACKET-PATH-UNFIXED))
         (else (error \"Platform not supported\"))))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock2-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (if (file-exists? file-path) (delete-file file-path) 0))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock3-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (current-directory (get-dirpath-from-filepath full-test-area-path))
  (define output-email-file-path (string-append (cleanse-path-string
                                  (string-append (get-dirpath-from-filepath full-test-area-path) \"/\"))
                                                \"test-results-email.txt\"))
  (define output-results-file-path (string-append (cleanse-path-string
                                     (string-append (get-dirpath-from-filepath full-test-area-path) \"/\"))
                                                \"test-results.txt\"))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock4-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define mail-list-id (email-db-id mailing-list))
  (define to-field (email-db-name mailing-list))
  (define recipients (email-db-addresses mailing-list))
  (cond ((not (equal? #f mailing-list))
         (current-directory (get-dirpath-from-filepath full-test-area-path))
         (define output-email-file-path (string-append (cleanse-path-string
                                                        (string-append (get-dirpath-from-filepath full-test-area-path) \"/\"))
                                                       \"test-results-email.txt\"))
         (define output-results-file-path (string-append (cleanse-path-string
                                                          (string-append (get-dirpath-from-filepath full-test-area-path) \"/\"))
                                                         \"test-results.txt\"))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock5-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (if (regexp-match #rx\"^[0-9]+ success*\" line) #t #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock6-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (if (regexp-match #rx\"^.*>.*\" line) #t #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock7-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (if (regexp-match #rx\"^\\s*location:.*\" line) #t #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock8-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (if (regexp-match #rx\"^\\s*actual:.*\" line) #t #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock9-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (if (regexp-match #rx\"^\\s*expected:.*\" line) #t #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock10-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define parse-result (regexp-match #rx\"^.*>(.*)\" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock11-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define parse-result (regexp-match #rx\"^\\s*location:(.*)\" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock12-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define parse-result (regexp-match #rx\"^\\s*actual:(.*)\" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock13-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define parse-result (regexp-match #rx\"^\\s*expected:(.*)\" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock14-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define (helper seq)
    (if (equal? '() (car seq))
        '()
        (cons (map car seq) (helper (map cdr seq)))))
  (helper seq)
)")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock15-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (if (null? items)
      '() ;; This means we reached the end of the list
      (cons (proc (car items)) ;; Apply the procedure to the first item in the pair
            (map proc (cdr items))))) ;; Go down the sublists with \"cdr\"")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock16-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (map parse-test-name (filter is-test-name? all-lines)))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock17-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (map parse-suite-location (filter is-suite-location? all-lines)))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock18-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (map parse-actual-value (filter is-actual? all-lines)))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock19-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (map parse-expected-value (filter is-expected? all-lines)))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock20-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (zip (get-all-test-cases all-lines)
       (get-all-suite-locations all-lines)
       (get-all-actual-values all-lines)
       (get-all-expected-values all-lines)))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock21-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (filter is-passed-failed-info? all-file-lines))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock22-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define parse-result (regexp-match #rx\"^([0-9]+) success*\" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock23-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define parse-result (regexp-match #rx\"([0-9]+) failure*\" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock24-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define parse-result (regexp-match #rx\"([0-9]+) test\\(s\\)\" line))
  (if (not (equal? parse-result #f)) (string-trim (cadr parse-result)) #f))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock25-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (map parse-successful-tests pass-fail-list))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock26-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (map parse-failed-tests pass-fail-list))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock27-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (map parse-total-tests pass-fail-list))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock28-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (map string->number list-of-num-strings))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock29-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (accumulate (strings-to-nums passed-list) 0 + (lambda (x) x)))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock30-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (string-append \"> FAILED: '\" (car sublist) \"' in '\" (cadr sublist) \"'\"
                 \"\nactual: \" (caddr sublist) \"\nexpected: \" (cadddr sublist) \"\n\"))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock31-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (map create-failed-case (get-all-test-information all-lines)))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock32-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define num-passed-cases (- num-cases num-failed-cases))
  (define percent-failed (round (* (/ num-failed-cases num-cases) 100)))
  (define percent-passed (round (* (/ num-passed-cases num-cases) 100)))
  (define suite-result-header (list (string-append \">>> Results for test area file '\" test-suite-name \"'\")
                                    (string-append \"\n-> Total: \" (number->string num-cases))
                                    (string-append \"-> Passed: \" (number->string num-passed-cases)
                                                   \" (\" (number->string percent-passed) \"%)\")
                                    (string-append \"-> Failed: \" (number->string num-failed-cases)
                                                   \" (\" (number->string percent-failed) \"%)\n\")
                                    )) ; end suite-result-header list and define
  suite-result-header
)")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock33-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define failed-case-header (create-failed-cases-data failed-num total-num name-of-suite))
  (define failed-case-list (create-list-of-failed-cases all-lines))
  (append failed-case-header failed-case-list))")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock34-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "  (define test-results-lines (file->lines full-test-results-path))  ;; Analyze test result file lines
  (define pass-fail-info (get-passed-failed-info test-results-lines))
  (define successful-list (get-successful-tests pass-fail-info))
  (define failed-list (get-failed-tests pass-fail-info))
  (define total-list (get-total-tests pass-fail-info))  ;; Here is where we actually got the physical statistics
  (define num-passed (add-list-of-string-nums successful-list))
  (define num-failed (add-list-of-string-nums failed-list))
  (define num-total (add-list-of-string-nums total-list))  ;; Get file lines to write out to the output file to send as an email
  (define failed-case-lines-to-write (create-failed-cases-lines
                                      test-results-lines num-failed num-total test-suite-name))
  (begin (remake-file output-email-file)  ;; Now write the processed lines above out to file.
         (display-lines-to-file failed-case-lines-to-write output-email-file #:separator\"\n\"))
)")
                     )))))
    (send/suspend/dispatch response-generator)))


;;help page
(define (help-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Help")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (p "help page...")
                     )))))
    (send/suspend/dispatch response-generator)))






(serve/servlet start
               #:listen-ip "127.0.0.1"
               ;#:port 8080
               #:servlet-path "/")
