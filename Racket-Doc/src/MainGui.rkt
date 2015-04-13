;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: RacketDocGui.rkt
;; Author: James Kuczynski
;; Email: jkuczyns@cs.uml.edu
;; File Description: frontend for Racket-Doc
;;
;; Created 04/04/2015 1:40pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/gui)

(require "Decapitron.rkt")
(require "FileIO.rkt")
(require "AdvancedGui.rkt")

(define background
  (read-bitmap "./../share/racket1.jpg"))

(define frame (new frame%
                   (label "Racket-Doc")
                   (width 400)
                   (height 500)))

(new message% [parent frame]
              [label background])

;;create button callbacks
(define (selectInCallback button event)
  (display "\n\"Select (in)\" button pressed\n"))

(define (selectOutCallback button event)
  (display "\n\"Select (out)\" button pressed\n"))

(define (okBtnCallback button event)
  (display "\n\"Run\" button pressed\n")
  (toFile (each-line "./../tests/Test.rkt" '() '() '() '() '()) reqs? provs? procs?))

(define (advancedBtnCallback button event)
  (display "\n\"Advanced\" button pressed\n")
  (send advFrame show #t)
  )

(define (cancelBtnCallback button event)
  (display "\n\"Cancel\" button pressed\n")
  (send frame show #f))

;create panels
(define inputPanel (new horizontal-panel% [parent frame]
                                          [alignment '(center center)]))
(define outputPanel (new horizontal-panel% [parent frame]
                                           [alignment '(center center)]))
                        
(define btnPanel (new horizontal-panel% [parent frame]
                                        [alignment '(right bottom)]))

;create input text fields
(define inputTextField (new text-field% [parent inputPanel]
                                        [label "Input File:"]
                                        [min-width 150]))
;(send inputTextField set-value "/path/to/input/dir/or/.rkt/file")


(define outputTextField (new text-field% [parent outputPanel]
                                         [label "Output File:"]
                                         [min-width 150]))
;(send outputTextField set-value "/path/to/output/dir")


;create buttons
(define selectInBtn (new button% [parent inputPanel]
                                 [label "Select"]
                                 [callback (λ (button event)
                                             (define filepath (get-file))
                                             (send inputTextField set-value (path->string filepath)))]))

(define selectOutBtn (new button% [parent outputPanel]
                                  [label "Select"]
                                  [callback (λ (button event)
                                              (define filepath (get-file))
                                              (send inputTextField set-value (path->string filepath)))]))

(define okBtn (new button% [parent btnPanel]
                           [label "Run"]
                           [callback okBtnCallback]))

(define advancedBtn (new button% [parent btnPanel]
                                 [label "Advanced"]
                                 [callback advancedBtnCallback]))

(define cancelBtn (new button% [parent btnPanel]
                               [label "Cancel"]
                               [callback cancelBtnCallback]))





(send frame show #t)