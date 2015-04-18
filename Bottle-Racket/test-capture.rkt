#lang racket/gui

(require "test-tracker.rkt")
(require "../Common/user-settings-directory.rkt") ; For writing out test results
(require "../QA-Email/email.rkt"
         "../QA-Email/email-db.rkt"
         "../QA-Email/email-db-ui.rkt") ; For mailing test results

;; **********************************************************************
;; * A couple handy functions from bn-to-racket
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

(define (get-full-path absolute-dir assignment-name filetype)
  (define path-back (regexp-match #rx"\\\\" absolute-dir))
  (define path-forward (regexp-match #rx"/" absolute-dir))
  (cond ((not (equal? path-back #f))
         (string-append absolute-dir "\\" assignment-name filetype))
        ((not (equal? path-forward #f))
         (string-append "/" absolute-dir "/" assignment-name filetype))
        (else "undefined"))
)

;; **********************************************************************
;; * Information storing mailing information for the test script,
;; * initialized to nothing but changed when script is running.
;; **********************************************************************

(define original-script-dir (path->string (current-directory)))
(define mailing-list '()) ;; Global-variable for passing to run-test-area-email

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

; The "To" Text Field and Button.
(define to-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define to-description (new text-field%
                         (parent to-panel)
                         (label "To:")
                         (min-width 600)))

(send to-description set-value "< Specified by Mailing List >")

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

; Create the button which opens an email managing dialog
; Add click button to the horizontal panel
(new button% [parent dialog] [label "Configure Emails"]
      [callback (lambda (button event)
                                   
                  (define local-mail-list (open-manage-mailing-list-dialog 'return-db))
                  (cond ((not (equal? #f local-mail-list))
                         (define local-mail-list-name (email-db-name local-mail-list))
                         (set! mailing-list local-mail-list)
                         ;; Also update the text field on the test-capture GUI
                         (send to-description set-value local-mail-list-name)
                         ;; Indicate to the user that the script was successfully created
                         (send user-prompt set-label (string-append "Results configured to send to '"
                                                                    local-mail-list-name "'.")))
                        (else "An email list was not selected."))
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button

; Create the button which runs the test area, but doesn't send an email
(new button% [parent dialog] [label "Run Script"]
      [callback (lambda (button event)

                  ;; Variables specifying test data                 
                  (define output-dir (get-dirpath-from-filepath (send suite-filepath get-value)))
                  (define area-file (get-filename-from-filepath (send suite-filepath get-value)))
                  (define full-test-area-path (get-full-path output-dir "" area-file))
                  (run-test-area full-test-area-path)
                  
                  ;; Indicate to the user that the script was successfully created
                  (send user-prompt set-label (string-append "Successfully ran '" area-file "'."))
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button

; Create the button which runs the test area and also sends an email
(new button% [parent dialog] [label "Run Script and Send Email"]
      [callback (lambda (button event)

                  ;; Variables specifying test data                 
                  (define output-dir (get-dirpath-from-filepath (send suite-filepath get-value)))
                  (define area-file (get-filename-from-filepath (send suite-filepath get-value)))
                  (define full-test-area-path (get-full-path output-dir "" area-file))
                  
                  ;; Run the generated test running script. Change working directory to that script's directory.
                  ;; Remember mailing-list is the global variable to pass to the run-test-area-email procedure.
                  (define subject-field (send subject-description get-value))
                  (run-test-area-email full-test-area-path subject-field mailing-list)
                  
                  ;; Indicate to the user that the script was successfully created
                  (send user-prompt set-label (string-append "Successfully ran '" area-file "' and sent email."))
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button
                  

; Show the dialog
(send dialog show #t)
