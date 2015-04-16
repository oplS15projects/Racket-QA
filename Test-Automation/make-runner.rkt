#lang racket

(require racket/file
         2htdp/batch-io
         "../Common/user-settings-directory.rkt")

(provide make-runner)

(define RUNNER-TEMP-DIRECTORY
  (full-path-in-settings-directory "temp"))
(define RUNNER-FILE-PREFIX "at-run")
(define RUNNER-FILE-EXTENSION ".rkt")
(define RUNNER-HEADER "runner-header.rkt")
(define RUNNER-TRAILER "runner-trailer.rkt")

(define test-file-1 
  "C:\\Users\\choyong\\Documents\\GitHub\\Racket-QA\\Bottle-Racket\\testing\\ps3a\\test_script.rkt")
(define test-file-2
  "C:\\Users\\choyong\\Documents\\GitHub\\Racket-QA\\Test-Automation\\sample test\\msconfig.rkt")

(define (generate-runner-file-name)
  (define new-runner-file
    (cleanse-path-string 
           (string-append RUNNER-TEMP-DIRECTORY "/"
                          RUNNER-FILE-PREFIX
                          (number->string (random 99999))
                          RUNNER-FILE-EXTENSION)))
  (cond ((file-exists? new-runner-file)
         (generate-runner-file-name))
        (else new-runner-file)))

(define (append-to-runner out-port)
  (define command-prefix "(system (string-append \"\\\"\" RACKET-PATH \"\\\" \\\"\" \"")
  (define command-suffix "\" \"\\\"\"))")
  (lambda (file)
    (fprintf out-port 
             "~a~a~a~n"
             command-prefix
             (string-append (double-backslash file))
             command-suffix)))

(define (create-temp-directory)
  (when (not (directory-exists? RUNNER-TEMP-DIRECTORY))
    (make-directory* RUNNER-TEMP-DIRECTORY)))

(define (make-runner files)
  (create-temp-directory)  
  (define header (read-file RUNNER-HEADER))
  (define trailer (read-file RUNNER-TRAILER))
  (define runner-file (generate-runner-file-name))
  (call-with-output-file runner-file
    (lambda (out)
      (fprintf out "~a~n" header)
      (for-each (append-to-runner out) files)
      (fprintf out "~n~a~n" trailer))
    #:mode 'binary
    #:exists 'truncate/replace)
  runner-file)