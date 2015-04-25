;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: master-gui.rkt
;; Author: Roy Van Liew
;; Email: roy_vanliew@student.uml.edu
;; File Description: GUI for all four components
;;
;; Last Modified 04/25/2015 2:15 am by James Kuczynski
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui

(require "Common/user-settings-directory.rkt") ; Filepath utilities
(require "./Racket-Doc/src/MainGui.rkt")
(require "Test-Automation/scheduler_ui.rkt")
(require "QA-Email/email-db-ui.rkt")
(require "Racket-Doc/src/MainGui.rkt")
(require "about-me.rkt")
(require setup/dirs)
(require web-server/servlet web-server/servlet-env)
(require xrepl)
(require racket/enter)


(define bottle-racket-icon (read-bitmap "demo/bottle-racket.png"))
(define test-capture-icon (read-bitmap "demo/test-capture.png"))
(define scheduler-icon (read-bitmap "demo/clock-icon-4.png"))
(define racket-doc-icon (read-bitmap "demo/racket-doc.png"))
(define mailing-list-icon (read-bitmap "demo/mailing-list.png"))
(define about-me-icon (read-bitmap "demo/question.png"))
;; Icon for question mark is Simple Question Mark Icon #069497

(define caption-width 150)


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

(define main-window (new frame% (label "Racket-QA") (width 600)))

(define background (read-bitmap "racketqa-logo.png"))

(define image-loaded (new message% [parent main-window] [label background]))

(define buttons-v-pane (new vertical-pane% [parent main-window] [alignment '(center center)]
                            [vert-margin 5] [spacing 5] [border 10]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for launching Bottle-Racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bottle-racket-h-pane
  (new horizontal-pane%
       (parent buttons-v-pane)
       (spacing 10)
       (alignment '(left center))))

(new button% [parent bottle-racket-h-pane] [label bottle-racket-icon]
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

(new message%
     (parent bottle-racket-h-pane)
     (min-width caption-width)
     (stretchable-width #f)
     (label "Bottle-Racket  ")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for launching Test-Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new button% [parent bottle-racket-h-pane] [label test-capture-icon]
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

(new message%
     (parent bottle-racket-h-pane)
     (min-width caption-width)
     (stretchable-width #f)
     (label "Test-Capture  "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for launching Scheduler
;;                  Paths currently mixed on Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scheduler-h-pane
  (new horizontal-pane%
       (parent buttons-v-pane)
       (spacing 10)
       (alignment '(left center))))

(new button% [parent scheduler-h-pane] [label scheduler-icon]
      [callback (lambda (button event) (launch-scheduler))])

(new message%
     (parent scheduler-h-pane)
     (min-width caption-width)
     (stretchable-width #f)
     (label "Test Scheduler  "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for launching Racket-Doc
;;                  Paths currently mixed on Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new button% [parent scheduler-h-pane] [label racket-doc-icon]
      [callback (lambda (button event)
                  (display "Clicked Racket-Doc.\n")
                  (send frame show #t)
                  
                                    ) ; end lambda
      ] ; end callback
) ;; end button

(new message%
     (parent scheduler-h-pane)
     (min-width caption-width)
     (stretchable-width #f)
     (label "Racket-Doc  "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for Mailing List Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mailing-list-h-pane
  (new horizontal-pane%
       (parent buttons-v-pane)
       (spacing 10)
       (alignment '(left center))))

(new button% [parent mailing-list-h-pane] [label mailing-list-icon]
      [callback (lambda (button event) (open-manage-mailing-list-dialog))])

(new message%
     (parent mailing-list-h-pane)
     (min-width caption-width)
     (stretchable-width #f)
     (label "Manage Mailing List  "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Button for "About Me" Section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (aboutMeCallback button event)
  (display "in callback")
(serve/servlet start-about-me-web-page
               #:quit? #t
               #:listen-ip "127.0.0.1"
               ;#:port 8080
               #:servlet-path "/")
  (display (current-thread))
  ;(kill-thread (current-thread))
)

(new button% [parent mailing-list-h-pane] [label about-me-icon]
     [callback aboutMeCallback] ; end callback
) ;; end button


(new message%
     (parent mailing-list-h-pane)
     (min-width caption-width)
     (stretchable-width #f)
     (label "About-Me  "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Display - Displaying the Window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send main-window show #t)
