;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: bn-to-racket.rkt
;; Author: Roy Van Liew
;; Email: roy_vanliew@student.uml.edu
;; File Description: Procedures used in Bottle-Racket
;;
;; Last Modified 04/22/2015 2:43 pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui

(require racket/file)
(define nil '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean checks for file lines in a Bottlenose Perl
;; test file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
 | This function checks if a certain file line
 | is a test case in the Bottlenose file.
 | @param line The file line to check if it
 |             starts with ok and if it does
 |             that means it is a test case.
 |          
 | @return #t or #f depending on if ok is found
 |         in the beginning of the line, in
 |         which case it is a test case.
 |#
(define (is-test? line)
  (if (regexp-match #rx"^\\s*ok.*" line) #t #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "get" functions for retrieving certain data from
;; lines specifying test cases in the Bottlenose file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
 | This function checks if a certain file line
 | is a test case in the Bottlenose file.
 | @param line The test case file line to check
 |             if it has 4 elements in it.
 |             For ps1 to ps3, these are simple
 |             assignment files with three parts,
 |             but ps5b loads in two different
 |             files, which is four parts.
 |          
 | @return The file loaded in if the test case
 |         file line has 4 parts as a string,
 |         otherwise return "none". This is
 |         unparsed, as a warning.
 |#
(define (get-loaded-file line)
  (define all-parts (string-split line ","))
  (if (= (length all-parts) 4)
      (string-trim (car all-parts))
      "none")
)

#||
 | This function gets the first part of the
 | test case line, which is the input to give
 | to the Racket interpreter. Note if the
 | test case has 4 elements, this also
 | compensates for that situation.
 | @param line The test case line from the
 |             Bottlenose file.
 |          
 | @return The not-parsed input to give to the
 |         Racket interpreter, as a string.
 |#
(define (get-test-input line)
  (define all-parts (string-split line ","))
  (if (= (length all-parts) 3)
      (car all-parts)
      (string-append "(" (string-trim (cadr all-parts)))))

#||
 | This function gets the second part of the
 | test case line, which is the expected
 | value for the test case. Note if the
 | test case has 4 elements, this also
 | compensates for that situation.
 | @param line The test case line from the
 |             Bottlenose file.
 |          
 | @return The not-parsed expected value,
 |         as a string.
 |#
(define (get-expected-value line)
  (define all-parts (string-split line ","))
  (if (= (length all-parts) 3)
      (cadr all-parts)
      (caddr all-parts)))

#||
 | This function gets the third part of the
 | test case line, which is the name of the
 | test case. Note if the test case has
 | 4 elements, this also compensates for
 | that situation.
 | @param line The test case line from the
 |             Bottlenose file.
 |          
 | @return The not-parsed test case name,
 |         as a string.
 |#
(define (get-test-name line)
  (define all-parts (string-split line ","))
  (if (= (length all-parts) 3)
      (caddr all-parts)
      (cadddr all-parts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "find" functions that parse a given test case line
;; from the Bottlenose file for certain information.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
 | This function actually parses the loaded file
 | in the Bottlenose test case. The original
 | unparsed string is retrieved from
 | get-loaded-file.
 | @param line The unparsed string containing
 |             the loaded file for a test case.
 |          
 | @return The parsed file name loaded, as a
 |         string.
 |#
(define (find-loaded-file line)
  (define expected (regexp-match #rx"\\\"\\s*(.*)\\s*\\\"" line))
  (if (not (equal? expected #f))
      (string-trim (cadr expected))
      "none")
)

#||
 | This function actually parses the input
 | that is given to the Racket interpreter.
 | The original unparsed string is
 | retrieved from get-test-input.
 | @param line The unparsed string containing
 |             the input to give for the test
 |             case.
 |          
 | @return The parsed test case input, as a
 |         string.
 |#
(define (find-test-input line)
  (define expected (regexp-match #rx"\\(\\s*\\\"\\s*(.*)\\s*\\\"\\s*\\)" line))
  (if (not (equal? expected #f))
      (string-trim (cadr expected))
      #f))

#||
 | This function actually parses the
 | expected test case value retrieved
 | from get-expected-value.
 | @param line The unparsed string containing
 |             the expected value for the 
 |             test case.
 |          
 | @return The parsed expected value, as a
 |         string.
 |#
(define (find-expected-value line)
  (define expected (regexp-match #rx"^\\s*(.*)\\)$" line))
  (if (not (equal? expected #f))
      (string-trim (string-trim (cadr expected)) "\"")
      #f))

#||
 | This function actually parses the name of
 | the test case retrieved from get-test-name.
 | @param line The unparsed string containing
 |             the name of the test case.
 |          
 | @return The parsed test case name, as a
 |         string.
 |#
(define (find-test-name line)
  (define expected (regexp-match #rx"\\s*\\\"\\s*(.*)\\s*\\\"\\s*\\)" line))
  (if (not (equal? expected #f))
      (string-trim (cadr expected))
      #f))

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
    (if (equal? nil (car seq))
        nil
        (cons (map car seq) (helper (map cdr seq)))))
  (helper seq)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "get" functions for collectively retrieving all kinds
;; of information regarding tests in the Bottlenose file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
 | This function retrieves all inputs of
 | the test cases to the racket interpreter
 | from the Bottlenose file lines.
 | @param all-lines The lines read in from
 |                  the Bottlenose file.
 |          
 | @return A list containing the inputs for
 |         the test cases, represented as
 |         strings.
 |#
(define (get-all-test-inputs all-lines)
  (map find-test-input (map get-test-input (filter is-test? all-lines))))

#||
 | This function retrieves all expected
 | values of the test cases from the
 | Bottlenose file lines.
 | @param all-lines The lines read in from
 |                  the Bottlenose file.
 |          
 | @return A list containing the expected
 |         values, represented as strings.
 |#
(define (get-all-expected-values all-lines)
  (map find-expected-value (map get-expected-value (filter is-test? all-lines))))

#||
 | This function retrieves all test case
 | names from the Bottlenose file lines.
 | @param all-lines The lines read in from
 |                  the Bottlenose file.
 |          
 | @return A list containing the test case
 |         names, represented as strings.
 |#
(define (get-all-test-names all-lines)
  (map find-test-name (map get-test-name (filter is-test? all-lines))))

#||
 | This function aligns the test case
 | inputs, expected values, and names
 | in parallel for each test case in the
 | Bottlenose file.
 | @param all-lines The lines read in from
 |                  the Bottlenose file.
 |          
 | @return A list containing sublists each
 |         of length 3. In order, it's
 |         test inputs, expected values,
 |         and test names all represented
 |         as strings.
 |#
(define (get-all-test-information all-lines)
  (zip (get-all-test-inputs all-lines)
       (get-all-expected-values all-lines)
       (get-all-test-names all-lines)))

#||
 | This function checks for any files loaded
 | in for a test case, such as in ps5b.
 | @param all-lines The lines read in from
 |                  the Bottlenose file.
 |          
 | @return A list containing the source
 |         files that are required for the
 |         test cases to run properly,
 |         represented as strings.
 |#
(define (get-all-loaded-files all-lines)
  (remove-duplicates (map find-loaded-file (map get-loaded-file (filter is-test? all-lines)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for writing the Test Suite File.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
 | This function takes the loaded files
 | retrieved from get-all-loaded-files
 | and creates require statement strings
 | for writing out to file.
 | @param loaded-file-list Result from
 |                         get-all-loaded-files.
 |          
 | @return A list containing the lines to
 |         write out to file that resemble
 |         require statements in a Racket
 |         source file, as strings.
 |#
(define (create-requires-for-loads loaded-file-list)
  (if (null? loaded-file-list)
      nil
      (cons (string-append "(require \"" (car loaded-file-list) "\")") (create-requires-for-loads (cdr loaded-file-list))))
)

#||
 | This function takes a sublist from a
 | zipped list generated from
 | get-all-test-information and writes
 | strings representing a test case structure
 | in RackUnit to write out to file.
 | @param sublist A list containing three
 |                elements in order:
 |                test input, expected value,
 |                and test name.
 |          
 | @return A string representing a RackUnit
 |         test case to write out to file.
 |#
(define (create-test-case sublist)
  (define sym-to-number (regexp-match #rx"'(.*)'" (cadr sublist)))
  (if (not (equal? sym-to-number #f))
      (string-append "  (test-case \"" (caddr sublist) "\" (check-equal? " (car sublist) " " (cadr sym-to-number) "))")
      (string-append "  (test-case \"" (caddr sublist) "\" (check-equal? " (car sublist) " " (cadr sublist) "))")))

#||
 | This function takes a name for the test
 | suite and the list of test case information
 | generated from get-all-test-information
 | to create the main body of the suite file
 | to write.
 | @param suite-name Name of the test suite
 | @param test-cases List generated from
 |                   get-all-test-information
 |          
 | @return A list of strings containing the
 |         test suite body to write out to
 |         file.
 |#
(define (create-test-suite suite-name test-cases)
  (define suite-header (list ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
                             (string-append "(define-test-suite " suite-name)
                             (string-append "  #:before (lambda () (display \"Starting Test Suite '" suite-name "'\\n\"))")
                             (string-append "  #:after (lambda () (display \"Finished Test Suite '" suite-name "'\\n\"))")))
  (define header-with-tests (append suite-header (map create-test-case test-cases)))
  (define suite-to-return (append header-with-tests (list ")\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")))
  suite-to-return
)

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
(if (file-exists? file-path)
    (delete-file file-path)
    0))

#||
 | This function creates a list of strings
 | representing the top of the suite file
 | to write.
 | @param assignment-name
 |        Basically the source file name.
 | @param all-lines
 |        All the lines from the Bottlenose
 |        test file.
 |          
 | @return List of strings representing the
 |         top of the suite file to write.
 |#
(define (make-suite-header assignment-name all-lines)
  (define all-source-requires (create-requires-for-loads (get-all-loaded-files all-lines)))
  (define rkt-header (list "#lang racket\n"
                           "(require rackunit)"
                           "(require racket/include)"
                           "(require rackunit/text-ui)"
                           "(require rackunit/gui)"))
  (define single-assignment-require (list (string-append "(require \"" assignment-name ".rkt\")\n")))
  (if (= (length all-source-requires) 1)
      (append rkt-header single-assignment-require)
      (append rkt-header all-source-requires))
)

#||
 | This function formats a test suite name,
 | represented as a string, to be formatted
 | with an indent.
 | @param sublist Name of the test suite.
 |          
 | @return The test suite name as a string
 |         indented with two spaces at the
 |         start.
 |#
(define (create-suite-string sublist)
  (string-append "  " sublist))

#||
 | This function takes a list of suite names,
 | represented as strings, and writes a
 | footer for the bottom of the test suite
 | file represented as a list of strings
 | which indicates the list of test suites
 | to be run.
 | @param suite-names List of test suite
 |                    names as strings.
 |          
 | @return List of strings representing the
 |         bottom of the test suite file.
 |#
(define (create-test-suite-list suite-names)
  (define test-list-define (list "(define test-list (list"))
  (define test-suite-list (map create-suite-string suite-names))
  (define define-with-suites (append test-list-define test-suite-list (list "))\n")))
  (define footer-to-return (append define-with-suites (list "(provide (all-defined-out))\n")))
  footer-to-return
)

#||
 | This function takes a list of strings
 | representing the lines to write out as
 | the suite file along with an absolute
 | path specifying where the test suite
 | file will be written.
 | @param lines A list of strings to be
 |              written out to file.
 | @param absolute-dir Full path to the
 |                     test suite file
 |                     to be written.
 |          
 | @return Arbitrary value. The suite
 |         file is written out to
 |         absolute-dir.
 |#
(define (write-suite-file lines absolute-dir)
  (define path-back (regexp-match #rx"\\\\" absolute-dir))
  (define path-forward (regexp-match #rx"/" absolute-dir))
  (cond ((not (equal? path-back #f))
         (define placeholder (remake-file absolute-dir))
         (display-lines-to-file lines absolute-dir #:separator"\n"))
        ((not (equal? path-forward #f))
         (define placeholder (remake-file absolute-dir))
         (display-lines-to-file lines absolute-dir #:separator"\n"))
        (else "undefined"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A few handy filepath utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
 | This function is used to get the
 | test-suite name from a specified
 | racket file.
 | @param absolute-dir
 |        Full path to a file.
 |          
 | @return String representing the name
 |         of the test suite filename.
 |#
(define (get-assn-from-filepath absolute-dir)
  (define separation-back-slash (string-split absolute-dir "\\"))
  (define assignment-back (if (not (equal? (regexp-match #rx"\\\\" absolute-dir) #f))
                              (regexp-match #rx"^\\s*(.*)\\.rkt$" (last separation-back-slash))
                              #f))
  (define separation-forward-slash (string-split absolute-dir "/"))
  (define assignment-forward (if (not (equal? (regexp-match #rx"/" absolute-dir) #f))
                                 (regexp-match #rx"^\\s*(.*)\\.rkt$" (last separation-forward-slash))
                                 #f))
  (cond ((not (equal? assignment-back #f)) (cadr assignment-back))
        ((not (equal? assignment-forward #f)) (cadr assignment-forward))
        (else "undefined")))

#||
 | This function is used to create a
 | full absolute path combining
 | several parts.
 | @param absolute-dir
 |        Full path to a directory.
 | @param assignment-name
 |        First part of the filename
 | @param filetype
 |        Extension to add to the
 |        filename
 |          
 | @return String representing the
 |         full path of the file.
 |#
(define (get-full-path absolute-dir assignment-name filetype)
  (define path-back (regexp-match #rx"\\\\" absolute-dir))
  (define path-forward (regexp-match #rx"/" absolute-dir))
  (cond ((not (equal? path-back #f))
         (string-append absolute-dir "\\" assignment-name filetype))
        ((not (equal? path-forward #f))
         (string-append "/" absolute-dir "/" assignment-name filetype))
        (else "undefined"))
)

#||
 | This function retrieves everything except
 | for the last element in a list.
 | @param list The list to process.
 |          
 | @return List containing everything except
 }         the last element.
 |#
(define (butlast lst)
  (define (helper current lst counter)
    (if (> counter 1)
        (helper (append current (list (car lst)))
                (cdr lst) (- counter 1))
        current));; end helper
 (helper '() lst (length lst))) ;; end outer define

(provide (all-defined-out))
