;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: master-gui.rkt
;; Author: Roy Van Liew
;; Email: roy_vanliew@student.uml.edu
;; File Description: GUI for all four components
;;
;; Last Modified 04/22/2015 2:55 pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui

(require "Common/user-settings-directory.rkt") ; Filepath utilities
;; (require "Test-Automation/scheduler_ui.rkt") ; For Scheduler GUI. But this breaks on Windows.
;; (require "Racket-Doc/src/MainGui.rkt") ; Racket-Doc GUI. Breaks on Windows.
(require setup/dirs)

#|
Doing Bottle-Racket and Test-Capture for the time being. The other two parts break on Windows.

For Racket-Doc:
open-output-file: cannot open output file
  path: C:\OPL\Racket-QA\Racket-QA\./../output/WebPage.rkt
  system error: The system cannot find the path specified.; errno=3

For Scheduler:
open-input-file: cannot open input file
  path: C:\OPL\Racket-QA\Racket-QA\images/add-file-1.png
  system error: The system cannot find the path specified.; errno=3
|#

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Description at the top
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define description (string-append "Welcome to Racket-QA."))

(define main-window (new frame% (label "Racket-QA") (width 500)))

(define user-prompt (new message% [parent main-window]
                         [auto-resize #t]
                          [label description]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for launching Bottle-Racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new button% [parent main-window] [label "Bottle-Racket"]
      [callback (lambda (button event)
                  
                  ;; Configure necessary paths to call the Bottle-Racket script
                  (define master-gui-directory (current-directory))
                  (define bottle-racket-relative-path "Bottle-Racket/bottle-racket.rkt")
                  (define bottle-racket-full-path (string-append (cleanse-path-string
                                  (string-append (get-dirpath-from-filepath (current-directory))
                                                 "/" bottle-racket-relative-path))))
                  (define bottle-racket-fixed-path (cond ((eq? (system-type) 'windows) (valid-path-windows bottle-racket-full-path))
                                          ((eq? (system-type) 'unix) (valid-path-linux bottle-racket-full-path))
                                          ((eq? (system-type) 'macosx) (valid-path-linux bottle-racket-full-path))
                                          (else (error "Platform not supported"))))
                  
                  ;; Debugging
                  (display "Clicked Bottle-Racket.\n")
                  (display bottle-racket-full-path)
                  (display "\n")
                  (display bottle-racket-fixed-path)
                  
                  ;; Make the system call to Bottle-Racket.
                  (current-directory (get-dirpath-from-filepath bottle-racket-full-path)) ;; Change to Bottle-Racket directory.
                  (system (string-append RACKET-PATH " " bottle-racket-fixed-path))
                  (current-directory master-gui-directory) ;; Go back to the main page directory when finished.
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for launching Test-Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new button% [parent main-window] [label "Test-Capture"]
      [callback (lambda (button event)

                  ;; Configure necessary paths to call the test-capture script
                  (define master-gui-directory (current-directory))
                  (define test-capture-relative-path "Bottle-Racket/test-capture.rkt")
                  (define test-capture-full-path (string-append (cleanse-path-string
                                  (string-append (get-dirpath-from-filepath (current-directory))
                                                 "/" test-capture-relative-path))))
                  (define test-capture-fixed-path (cond ((eq? (system-type) 'windows) (valid-path-windows test-capture-full-path))
                                          ((eq? (system-type) 'unix) (valid-path-linux test-capture-full-path))
                                          ((eq? (system-type) 'macosx) (valid-path-linux test-capture-full-path))
                                          (else (error "Platform not supported"))))
                  
                  ;; Debugging
                  (display "Clicked Test-Capture.\n")
                  (display test-capture-full-path)
                  (display "\n")
                  (display test-capture-fixed-path)
                  
                  ;; Make the system call to test-capture.
                  (current-directory (get-dirpath-from-filepath test-capture-full-path)) ;; Change to Bottle-Racket directory.
                  (system (string-append RACKET-PATH " " test-capture-fixed-path))
                  (current-directory master-gui-directory) ;; Go back to the main page directory when finished.
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for launching Scheduler
;;                  Paths currently mixed on Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new button% [parent main-window] [label "Scheduler"]
      [callback (lambda (button event)

                  (display "Clicked Scheduler.\n")
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for launching Racket-Doc
;;                  Paths currently mixed on Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new button% [parent main-window] [label "Racket-Doc"]
      [callback (lambda (button event)

                  (display "Clicked Racket-Doc.\n")
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Displaying the Window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send main-window show #t)
