#lang racket/gui

;; **********************************************************************
;; * - Name: Roy Van Liew
;; * - Section: 91.301.201 - Organization of Programming Languages
;; * - FP1: BOTTLENOSE PERL TESTS TO RACKET TEST CONVERTER
;; *   This script takes in a perl bottlenose test script file, usually
;; *   labeled test.t, and then changes the perl tests into Racket
;; *   test cases to be run in either the RackUnit GUI or
;; *   textual interface. That script must be run separately.
;; **********************************************************************

(require racket/file)
(define nil '())

;; **********************************************************************
;; * Selectors for finding certain parts of the perl test case
;; **********************************************************************

;; The perl tests start with "ok".
(define (is-test? line)
  (if (regexp-match #rx"^\\s*ok.*" line) #t #f))

(define (get-loaded-file line)
  (define all-parts (string-split line ","))
  (if (= (length all-parts) 4)
      (string-trim (car all-parts))
      "none")
)

;; Gets first test input part of the line to parse in regexp in find-test-input
(define (get-test-input line)
  (define all-parts (string-split line ","))
  (if (= (length all-parts) 3)
      (car all-parts)
      (string-append "(" (string-trim (cadr all-parts)))))

;; Gets second expected value part of the line to parse in regexp in find-expected-value
(define (get-expected-value line)
  (define all-parts (string-split line ","))
  (if (= (length all-parts) 3)
      (cadr all-parts)
      (caddr all-parts)))

;; Gets third test name part of the line to parse in regexp in find-test-name
(define (get-test-name line)
  (define all-parts (string-split line ","))
  (if (= (length all-parts) 3)
      (caddr all-parts)
      (cadddr all-parts)))

;; **********************************************************************
;; * Constructors for removing unnecessary parts of the perl test case
;; **********************************************************************

(define (find-loaded-file line)
  (define expected (regexp-match #rx"\\\"\\s*(.*)\\s*\\\"" line))
  (if (not (equal? expected #f))
      (string-trim (cadr expected))
      "none")
)

;; first-test for scheme test inputs
(define (find-test-input line)
  ;; Opening parentheses, followed by zero or more whitespaces, followed by \ and ",
  ;; followed by zero or more whitespaces, everything in between is the test input,
  ;; followed by zero or more whitespaces, followed by \ and ", followed by zero or more
  ;; whitespaces, followed by a closing parentheses.
  (define expected (regexp-match #rx"\\(\\s*\\\"\\s*(.*)\\s*\\\"\\s*\\)" line))
  (if (not (equal? expected #f))
      (string-trim (cadr expected))
      #f))

;; second-test for expected values
(define (find-expected-value line)
  (define expected (regexp-match #rx"^\\s*(.*)\\)$" line))
  (if (not (equal? expected #f))
      (string-trim (string-trim (cadr expected)) "\"")
      #f))

;; third-test for test names
(define (find-test-name line)
  ;; Zero or more whitespaces, followed by \ and ",
  ;; followed by zero or more whitespaces, everything in between is the test input,
  ;; followed by zero or more whitespaces, followed by \ and ", followed by zero or more
  ;; whitespaces, followed by a closing parentheses.
  (define expected (regexp-match #rx"\\s*\\\"\\s*(.*)\\s*\\\"\\s*\\)" line))
  (if (not (equal? expected #f))
      (string-trim (cadr expected))
      #f))

;; **********************************************************************
;; * Procedures for retrieving all test inputs, expected values, and
;; * test names in a bottlenose perl file.
;; **********************************************************************

(define (zip . seq)
  (define (helper seq)
    (if (equal? nil (car seq))
        nil
        (cons (map car seq) (helper (map cdr seq)))))
  (helper seq)
)

(define (get-all-test-inputs all-lines)
  (map find-test-input (map get-test-input (filter is-test? all-lines))))

(define (get-all-expected-values all-lines)
  (map find-expected-value (map get-expected-value (filter is-test? all-lines))))

(define (get-all-test-names all-lines)
  (map find-test-name (map get-test-name (filter is-test? all-lines))))

(define (get-all-test-information all-lines)
  (zip (get-all-test-inputs all-lines)
       (get-all-expected-values all-lines)
       (get-all-test-names all-lines)))

(define (get-all-loaded-files all-lines)
  (remove-duplicates (map find-loaded-file (map get-loaded-file (filter is-test? all-lines)))))

(define (create-requires-for-loads loaded-file-list)
  (if (null? loaded-file-list)
      nil
      (cons (string-append "(require \"" (car loaded-file-list) "\")") (create-requires-for-loads (cdr loaded-file-list))))
)

;; **********************************************************************
;; * Procedures for creating strings representing what we want to write
;; * out to our suite file for the test cases inside the test suite.
;; **********************************************************************

(define (create-test-case sublist)
  (define sym-to-number (regexp-match #rx"'(.*)'" (cadr sublist)))
  (if (not (equal? sym-to-number #f))
      (string-append "  (test-case \"" (caddr sublist) "\" (check-equal? " (car sublist) " " (cadr sym-to-number) "))")
      (string-append "  (test-case \"" (caddr sublist) "\" (check-equal? " (car sublist) " " (cadr sublist) "))")))

(define (create-test-suite suite-name test-cases)
  (define suite-header (list ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
                             (string-append "(define-test-suite " suite-name)
                             (string-append "  #:before (lambda () (display \"Starting Test Suite '" suite-name "'\\n\"))")
                             (string-append "  #:after (lambda () (display \"Finished Test Suite '" suite-name "'\\n\"))")))
  (define header-with-tests (append suite-header (map create-test-case test-cases)))
  (define suite-to-return (append header-with-tests (list ")\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")))
  suite-to-return
)

;; **********************************************************************
;; * Test Suite File Creation Procedures
;; **********************************************************************

(define (remake-file file-path)
(if (file-exists? file-path)
    (delete-file file-path)
    0))

;; Create header of the suite file
(define (make-suite-header assignment-name all-lines)
  (define all-source-requires (create-requires-for-loads (get-all-loaded-files all-lines)))
  (define rkt-header (list "#lang racket\n"
                           "(require rackunit)"))
  (define single-assignment-require (list (string-append "(require \"" assignment-name ".rkt\")\n")))
  (if (= (length all-source-requires) 1)
      (append rkt-header single-assignment-require)
      (append rkt-header all-source-requires))
)

;; Create test-list for this bottlenose suite
(define (create-suite-string sublist)
  (string-append "  " sublist))

;; suite-names is a list of the suite names we're creating.
(define (create-test-suite-list suite-names)
  (define test-list-define (list "(define test-list (list"))
  (define test-suite-list (map create-suite-string suite-names))
  (define define-with-suites (append test-list-define test-suite-list (list "))\n")))
  (define footer-to-return (append define-with-suites (list "(provide (all-defined-out))\n")))
  footer-to-return
)

;; Now we need a procedure that can write these lines out to a file
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

;; **********************************************************************
;; * Test Area File Creation Procedures
;; **********************************************************************

(define (gui-or-text test-mode)
  (cond ((equal? test-mode "make-gui-runner")
         (list ";; map is used here to allow each test suite to appear in the same GUI window."
               "(map (make-gui-runner) test-list)\n"))
        ((equal? test-mode "run-tests")
         (list "(define test-result-raw-output (open-output-file \"test-results.txt\"))"
               "(current-error-port test-result-raw-output) ; File containing test information"
               "(current-output-port test-result-raw-output)"
               "(map run-tests test-list) ; The tests are run with this line"
               "(close-output-port test-result-raw-output)"))
        (else nil)) ;; end cond
) ;; end define
        

(define (create-test-area-lines assignment-name test-mode)
  (define rkt-header (list "#lang racket\n"
                           ";; Racket Unit Testing Libraries"
                           "(require racket/include)"
                           "(require rackunit)"
                           "(require rackunit/text-ui)"
                           "(require rackunit/gui)\n"
                           ";; Suite file to run"
                           (string-append "(require \"" assignment-name "_suite.rkt\")")
                           ))
  (define gui-or-text-lines (gui-or-text test-mode))
  (define test-area-lines (append rkt-header gui-or-text-lines (list "\n(provide (all-defined-out))\n")))
  test-area-lines
)

;; Now we need a procedure that can write these lines out to a file
(define (write-area-file lines absolute-dir)
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

;; **********************************************************************
;; * Windows/Unix Filepath Utilities
;; **********************************************************************

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

#|
(define (get-dir-from-filepath absolute-dir)
  (define path-back (regexp-match #rx"\\\\" absolute-dir))
  (define path-forward (regexp-match #rx"/" absolute-dir))
  (cond ((not (equal? path-back #f))
         (string-join (butlast (string-split absolute-dir "\\")) "\\"))
        ((not (equal? path-forward #f))
         (string-join (butlast (string-split absolute-dir "/")) "/"))
        (else "undefined"))
)
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


(define (butlast lst)
  (define (helper current lst counter)
    (if (> counter 1)
        (helper (append current (list (car lst)))
                (cdr lst) (- counter 1))
        current));; end helper
 (helper '() lst (length lst))) ;; end outer define

(provide (all-defined-out))

