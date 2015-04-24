;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: bottle-racket.rkt
;; Author: Roy Van Liew
;; Email: roy_vanliew@student.uml.edu
;; File Description: GUI for Bottle-Racket
;;
;; Last Modified 04/22/2015 2:36 pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui

(require rackunit)
(require "../Common/user-settings-directory.rkt") ; For writing out test results
(require "bn-to-racket.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Description at the top
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define description (string-append "Awaiting an assignment to create a test suite and area."))

(define dialog (new frame%
                         (label "Bottle-Racket")))

(define background
  (read-bitmap (cond ((eq? (system-type) 'windows) "images\\bottleracket.png")
                     ((eq? (system-type) 'unix) "images/bottleracket.png")
                     ((eq? (system-type) 'macosx) "images/bottleracket.png")
                     (else (error "Platform not supported")))))

(define image-loaded (new message% [parent dialog]
                          [label background]))

(define user-prompt (new message% [parent dialog]
                         [auto-resize #t]
                          [label description]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Assignment Text Field and Button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define assn-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define assn-filepath (new text-field%
                         (parent assn-panel)
                         (label "Assignment Source File:")
                         (min-width 700)))

(send assn-filepath set-value "Click Browse and locate the source assignment file (e.g. ps1.rkt).")

(new button%
     (parent assn-panel)
     (label "Browse...")
     (callback (lambda (button event)
                 (define filepath (get-file))
                 (send assn-filepath set-value (path->string filepath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Bottlenose File Text Field and Button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bn-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define bn-filepath (new text-field%
                         (parent bn-panel)
                         (label "Bottlenose Perl Test File:")
                         (min-width 700)))

(send bn-filepath set-value "Click Browse and locate the bottlenose test file for this assignment (e.g. test.t).")

(new button%
     (parent bn-panel)
     (label "Browse...")
     (callback (lambda (button event)
                 (define filepath (get-file))
                 (send bn-filepath set-value (path->string filepath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Convert Button for Test Suite File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new button% [parent dialog] [label "Convert"]
      [callback (lambda (button event)

                  ;; Variables specifying test data
                  (define file-lines (file->lines (send bn-filepath get-value)))
                  (define assn-name (get-assn-from-filepath (send assn-filepath get-value)))
                  (define output-dir (get-dirpath-from-filepath (send assn-filepath get-value)))
                  (define suite-output-dir (get-full-path output-dir assn-name "_suite.rkt"))
                  
                  ;; Get a list of all the "ok" test lines in the perl file
                  (define all-tests (get-all-test-information file-lines))
                  
                  ;; Get the list of strings to write out to the suites file for this assignment test file.
                  (define suite-file-header (make-suite-header assn-name file-lines))
                  (define bottlenose-suite (create-test-suite assn-name all-tests))
                  (define suite-file-footer (create-test-suite-list (list assn-name)))
                  (define scheme-suite-file-lines (append suite-file-header bottlenose-suite suite-file-footer))                 
                  
                  ;; Write the suite file
                  (write-suite-file scheme-suite-file-lines suite-output-dir)
                  (send suite-out-field set-value suite-output-dir)
                  (display (string-append "Created '" suite-output-dir "'\n"))
                  
                  ;; Debug lines in the DrRacket window
                  (send user-prompt set-label (string-append "'" assn-name "' tests successfully converted."))
                  (display (string-append "Run '" assn-name "_suite.rkt' in the above path to see what test cases pass or fail.\n\n"))
                  
                  ) ; end lambda
      ] ; end callback
) ;; end button

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Text field for Test Suite File Path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define suite-out-panel (new horizontal-panel%
                     (parent dialog)
                     (alignment '(left top))))

(define suite-out-field (new text-field%
                         (parent suite-out-panel)
                         (label "Output Test Suite File:")
                         (min-width 700)))

(send suite-out-field set-value "The Test Suite path will be shown here after Convert is clicked.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Displaying the Window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send dialog show #t)
