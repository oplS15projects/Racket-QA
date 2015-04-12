#lang racket/gui

;; Load in definitions from test-area-runner for procedures that create strings to write out to a file
(require "bn-to-racket.rkt") ; Windows/Unix filepath utilities
(require "../Common/user-settings-directory.rkt") ; For writing out test results
(require "../QA-Email/email.rkt")
(require "../QA-Email/email-db-ui.rkt") ; For mailing list dialog

; We want to write a new file with the definitions of test-area-runner
; and has a require statement with the ps1_area.rkt file. This way that
; newly generated script will be in the same directory as the test area
; file and won't need to do any complex directory stuff. This is possibly
; Just a simple GUI that asks the user to browse the computer for the
; location of the test area file, then get the directory from there
; to create a new script that can run that area and generate the test
; results (and later on, to send an email).

;; **********************************************************************
;; * Procedures for writing a new file
;; **********************************************************************

; Takes test area file directory to generate a new script in the same directory
; to be run on the tests and generate results. This will be placed after the
; loaded test-area-runner lines, so this will be at the end of the file
; e.g. area-filename is "ps1_area.rkt"
; e.g. test-area-dir is "C:\OPL\FP2\FP2\testing\ps1"
(define (create-run-script-lines test-area-dir area-filename test-result-filepath)
  (define run-script-header (list "\n;; **********************************************************************"
                                  ";; * MAIN: RUN THE SCRIPT"
                                  ";; **********************************************************************"
                                  (string-append "\n;; This line will run the tests\n(require \"" area-filename "\")")
                                  "\n;; Read in the lines from the test results file"
                                  "(define file-lines (file->lines \"test_results.txt\"))"
                                  (string-append "(define failed-case-lines-to-write (create-failed-cases-lines "
                                                 "file-lines num-failed num-tests suite-name))")
                                  ;"(remake-file \"test_email.txt\")"
                                  (string-append "(remake-file \"" test-result-filepath "\")")
                                  (string-append "(display-lines-to-file failed-case-lines-to-write \""
                                                 test-result-filepath "\" #:separator\"\\n\")")))
  run-script-header
)


;; **********************************************************************
;; * GUI
;; **********************************************************************

; Create a dialog window
(define dialog (new frame%
                         (label "Test-Capture")))

; Display simple message prompting user to enter input
(define description (string-append "Select a Test Area File and specify email fields."))

(define user-prompt (new message% [parent dialog]
                         [auto-resize #t]
                          [label description]))

; Test Suite Text Field and Button.
(define suite-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define suite-filepath (new text-field%
                         (parent suite-panel)
                         (label "Test Area File:")
                         (min-width 600)))

(send suite-filepath set-value "Click Browse and locate the test area file.")

(new button%
     (parent suite-panel)
     (label "Browse...")
     (callback (lambda (button event)
                 (define filepath (get-file))
                 (send suite-filepath set-value (path->string filepath)))))

; Button for Results Directory.
(define test-out-dir-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define test-out-dir-description (new text-field%
                         (parent test-out-dir-panel)
                         (label "Test Result Output Directory:")
                         (min-width 600)))

(send test-out-dir-description set-value "QA Test Result")

; The "To" Text Field and Button.
(define to-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define to-description (new text-field%
                         (parent to-panel)
                         (label "To:")
                         (min-width 600)))

(send to-description set-value "QA Team")

; The "Subject" Text Field and Butsubjectn.
(define subject-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define subject-description (new text-field%
                         (parent subject-panel)
                         (label "Subject:")
                         (min-width 600)))

(send subject-description set-value "Regression Statistics")

;; **********************************************************************
;; * FILE CREATION AND RUNNING BUTTON
;; **********************************************************************


; Create the button for generating the test running script, but DOESN'T run it.
; Add click button to the horizontal panel
(new button% [parent dialog] [label "Make Test Running Script"]
      [callback (lambda (button event)
                  
                  ;; Determine where the test results will go.
                  (define test-output-dir (send test-out-dir-description get-value))
                  (when (not (settings-directory-exists?))
                    (create-settings-directory))
                  (when (not (directory-exists-in-settings-directory? test-output-dir))
                    (make-directory-in-settings-directory test-output-dir))
                  (define result-file-path
                    (double-backslash (full-path-in-settings-directory
                     (cleanse-path-string (string-append test-output-dir "/test-result-email.txt")))))

                  ;; Variables specifying test data                 
                  (define output-dir (get-dir-from-filepath (send suite-filepath get-value)))
                  (define area-file (string-append (get-assn-from-filepath (send suite-filepath get-value)) ".rkt"))
                  (define run-script-path (get-full-path output-dir "test" "_script.rkt"))
                  
                  ;; Run script lines to add with test-area-runner
                  (define test-area-runner-lines (file->lines "test-area-runner.rkt"))
                  (define run-script-lines (create-run-script-lines output-dir area-file result-file-path))
                  (define all-run-script-lines (append test-area-runner-lines run-script-lines))
                  
                  ;; Write the lines out to the file
                  (define placeholder (remake-file run-script-path))
                  (display-lines-to-file all-run-script-lines run-script-path #:separator"\n")
                  
                  ;; Indicate to the user that the script was successfully created
                  (send user-prompt set-label (string-append "Created 'test_script.rkt' for "
                                                             "test area file '" area-file "'."))
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button

; Create the button which opens an email managing dialog, and then runs the script when that's closed
; Add click button to the horizontal panel
(new button% [parent dialog] [label "Configure Emails and Run"]
      [callback (lambda (button event)
                  
                  ;; Determine where the test results will go.
                  (define test-output-dir (send test-out-dir-description get-value))
                  (when (not (settings-directory-exists?))
                    (create-settings-directory))
                  (when (not (directory-exists-in-settings-directory? test-output-dir))
                    (make-directory-in-settings-directory test-output-dir))
                  (define result-file-path
                    (double-backslash (full-path-in-settings-directory
                     (cleanse-path-string (string-append test-output-dir "/test-result-email.txt")))))

                  ;; Variables specifying test data                 
                  (define output-dir (get-dir-from-filepath (send suite-filepath get-value)))
                  (define area-file (string-append (get-assn-from-filepath (send suite-filepath get-value)) ".rkt"))
                  (define run-script-path (get-full-path output-dir "test" "_script.rkt"))
                  
                  ;; Variables specifying list of recipients, subject, and to fields
                  (define to-field (send to-description get-value))
                  (define subject-field (send subject-description get-value))
                  (define recipients (open-manage-mailing-list-dialog 'return-addresses))
                  
                  ;; Run the generated test running script. Change working directory to that script's directory.
                  (current-directory output-dir)
                  (system (string-append "racket " run-script-path))
                  (send-text-file to-field subject-field result-file-path recipients)
                  
                  ;; Indicate to the user that the script was successfully created
                  (send user-prompt set-label (string-append "Successfully ran 'test_script.rkt' for "
                                                             "test area file '" area-file "'."))
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button
                  

; Show the dialog
(send dialog show #t)
