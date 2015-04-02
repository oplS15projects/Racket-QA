#lang racket/gui

;; **********************************************************************
;; * - Name: Roy Van Liew
;; * - Section: 91.301.201 - Organization of Programming Languages
;; * - FP2: USING THE GUI RACKET LIBRARY TO MAKE A USER-FRIENDLY WINDOW
;; *   This converter is now user-friendly with this GUI implementation.
;; *   Now the user can browse for the files on their filesystem.
;; **********************************************************************

(require rackunit)
(require "bn-to-racket.rkt")

;; **********************************************************************
;; * REMEMBER TO PUT THE PROVIDE STATEMENT AT THE END OF THE SOURCE FILE.
;; * (provide (all-defined-out))
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

(define (get-dir-from-filepath absolute-dir)
  (define path-back (regexp-match #rx"\\\\" absolute-dir))
  (define path-forward (regexp-match #rx"/" absolute-dir))
  (cond ((not (equal? path-back #f))
         (string-join (butlast (string-split absolute-dir "\\")) "\\"))
        ((not (equal? path-forward #f))
         (string-join (butlast (string-split absolute-dir "/")) "/"))
        (else "undefined"))
)

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

;; **********************************************************************
;; * WINDOW DISPLAY - TOP TEXT FIELDS
;; **********************************************************************

; Display simple message prompting user to enter input
(define description (string-append "Awaiting an assignment to create a test suite and area."))

; Create a dialog window
(define dialog (new frame%
                         (label "Bottle-Racket")))

; Load in the bottlenose to racket image
(define background
  (read-bitmap "images/bottleracket.png"))

(define image-loaded (new message% [parent dialog]
                          [label background]))

(define user-prompt (new message% [parent dialog]
                         [auto-resize #t]
                          [label description]))

; Assignment Source File Text Field and Button.
(define assn-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define assn-filepath (new text-field%
                         (parent assn-panel)
                         (label "Assignment Source File:")
                         (min-width 800)))

(send assn-filepath set-value "Click Browse and locate the source assignment file (e.g. ps1.rkt).")

(new button%
     (parent assn-panel)
     (label "Browse...")
     (callback (lambda (button event)
                 (define filepath (get-file))
                 (send assn-filepath set-value (path->string filepath)))))

; Bottlenose Perl Test File Text Field and Button.
(define bn-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define bn-filepath (new text-field%
                         (parent bn-panel)
                         (label "Bottlenose Perl Test File:")
                         (min-width 800)))

(send bn-filepath set-value "Click Browse and locate the bottlenose test file for this assignment (e.g. test.t).")

(new button%
     (parent bn-panel)
     (label "Browse...")
     (callback (lambda (button event)
                 (define filepath (get-file))
                 (send bn-filepath set-value (path->string filepath)))))

; Specify either make-gui-runner or run-tests with a radio button
(define mode-radio (new radio-box%
     (parent dialog)
     (label "Test Mode:")
     (choices '("make-gui-runner" "run-tests"))
     (style (list 'horizontal))
                     ))

;; **********************************************************************
;; * FILE CREATION CONVERT BUTTON
;; **********************************************************************

; Create the convert button
; Add click button to the horizontal panel
(new button% [parent dialog] [label "Convert"]
      [callback (lambda (button event)

                  ;; Variables specifying test data
                  (define file-lines (file->lines (send bn-filepath get-value)))
                  (define assn-name (get-assn-from-filepath (send assn-filepath get-value)))
                  (define testing-mode (send mode-radio get-item-label (send mode-radio get-selection)))
                  (define output-dir (get-dir-from-filepath (send assn-filepath get-value)))
                  
                  ;; Get a list of all the "ok" test lines in the perl file
                  (define all-tests (get-all-test-information file-lines))
                  
                  ;; Get the list of strings to write out to the suites file for this assignment test file.
                  (define suite-file-header (make-suite-header assn-name))
                  (define bottlenose-suite (create-test-suite assn-name all-tests))
                  (define suite-file-footer (create-test-suite-list (list assn-name)))
                  
                  ;; Create the full lists that are needed to write out to both the test suite and test area files.
                  ;; The mode to pass to create-test-area-lines should be one of "make-gui-runner" or "run-tests"
                  (define scheme-suite-file-lines (append suite-file-header bottlenose-suite suite-file-footer))
                  (define scheme-area-file-lines (create-test-area-lines assn-name testing-mode))
                  
                  ;; Define the output directories for the test suite and test area files
                  (define suite-output-dir (get-full-path output-dir assn-name "_suite.rkt"))
                  (define area-output-dir (get-full-path output-dir assn-name "_area.rkt"))
                  
                  ;; Write the suite file
                  ;(write-suite-file scheme-suite-file-lines assn-name output-dir)
                  (write-suite-file scheme-suite-file-lines suite-output-dir)
                  (send suite-out-field set-value suite-output-dir)
                  (display (string-append "Created '" suite-output-dir "'\n"))
                  
                  ;; Write the area file
                  ;(write-area-file scheme-area-file-lines assn-name output-dir)
                  (write-area-file scheme-area-file-lines area-output-dir)
                  (send area-out-field set-value area-output-dir)
                  (display (string-append "Created '" area-output-dir "'\n"))
                  
                  ;; Bring specified test mode back to the dialog box as well
                  (send testbox-out-field set-value testing-mode)
                  (display (string-append "Testing Mode specified was '" testing-mode "'\n"))
                  
                  ;; Debug lines in the DrRacket window
                  (send user-prompt set-label (string-append "'" assn-name "' tests successfully converted."))
                  (display (string-append "Run '" assn-name "_area.rkt' in the above path to see what test cases pass or fail.\n\n"))
                  
                  ) ; end lambda
      ] ; end callback
) ;; end button

;; **********************************************************************
;; * OUTPUT FILE TEXT FIELDS
;; **********************************************************************

; Output textbox that will show the resulting test suite file path.
(define suite-out-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define suite-out-field (new text-field%
                         (parent suite-out-panel)
                         (label "Output Test Suite File:")
                         (min-width 800)))

(send suite-out-field set-value "The Test Suite path will be shown here after Convert is clicked.")

; Output textbox that will show the resulting test area file path.
(define area-out-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define area-out-field (new text-field%
                         (parent area-out-panel)
                         (label "Output Test Area File:")
                         (min-width 800)))

(send area-out-field set-value "The Test Area path will be shown here after Convert is clicked.")

; Output textbox that will show the resulting test area file path.
(define testbox-out-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define testbox-out-field (new text-field%
                         (parent testbox-out-panel)
                         (label "Testing Mode Chosen:")
                         (min-width 800)))

(send testbox-out-field set-value "The Testing Mode chosen will be displayed here when Convert is clicked.")

;; **********************************************************************
;; * THREE EXTRA BUTTONS - RUN NOW, SCHEDULER, ADDRESS BOOK
;; **********************************************************************

; Buttons for accesing the Address Book for email functionality
(define address-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(center bottom))))

; Note that this is part of the functionality we'd want in the Add button for
; the Address Book. This is a bit misleading because the Address Book window
; has several options, but this functionality can be extended to make the add
; button work.
(new button%
     (parent address-panel)
     (label "Address Book")
     [callback (lambda (button event)
                 
                 ; Create new dialog window for Address Book
                 (define address-dialog (new frame% (label "Address Book")))
                 
                 ; Output textbox that will show the resulting test area file path.
                 (define email-panel (new horizontal-panel%
                                             (parent address-dialog)
                                             (alignment '(left top))))
                 
                 (define email-field (new text-field%
                                             (parent email-panel)
                                             (label "Email:")
                                             (min-width 400)))
                 
                 (send email-field set-value "Enter Email Address")
                 (send address-dialog show #t)
                 
                 ;; **********************************************************************
                 ;; * Buttons for determining to add or close email dialog window
                 ;; **********************************************************************
                 
                 (define control-panel (new horizontal-panel%
                                             (parent address-dialog)
                                             (alignment '(center bottom))))
                 
                 ; Ok button, for adding in an email
                 (new button%
                      (parent control-panel)
                      (label "Ok")
                 ) ; end Cancel button
                 
                 ; Close the dialog
                 (new button%
                      (parent control-panel)
                      (label "Cancel")
                      [callback (lambda (button event) (send address-dialog show #f))]
                 ) ; end Cancel button
                 
                 ) ; end lambda
               ] ; end callback
)


; Buttons for running the test suites
(define run-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(center bottom))))

(new button%
     (parent run-panel)
     (label "Immediate Run"))

(new button%
     (parent run-panel)
     (label "Scheduled Run"))



; Show the dialog
(send dialog show #t)
