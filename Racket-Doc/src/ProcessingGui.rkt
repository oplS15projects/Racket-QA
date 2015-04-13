;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: AdvancedGui.rkt
;; Author: James Kuczynski
;; Email: jkuczyns@cs.uml.edu
;; File Description: "Processing" GUI for Racket-Doc
;;
;; Created 04/04/2015 3:50pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/gui)

(define frame (new frame%
                   (label "Racket-Doc")
                   (width 400)
                   (height 500)))

(new message% [parent frame]
              [label "Processing Data"])

;create panels
(define processingPanel (new vertical-panel% [parent frame]
                                             [alignment '(left center)]))

(define btnPanel (new horizontal-panel% [parent frame]
                                        [alignment '(right bottom)]))


;callbacks
(define (displayBtnCallback button event)
  (display "Display\n"))

(define (deleteBtnCallback button event)
  (display "Delete\n"))

(define (exitBtnCallback button event)
  (display "Exit\n")
  (send frame show #f))


;create textfieldthingy
(define procMessage (new message% [parent processingPanel]
                            [label "Processing..."]))

(define procTf (new text-field% [parent processingPanel]
                                [label ""]
                                [min-width 50]
                                [min-height 200]))

;;for demo
(send procTf set-value "recursing directory Ps2...
 found file webEx.rkt
 found file toFile.rkt
 found file testDoc.rkt
 extracting comments...
 generating documentation for web deployment...
 execution completed successfully!")

;create buttons
(define displayBtn (new button% [parent btnPanel]
                           [label "Display"]
                           [callback displayBtnCallback]))

(define deleteBtn (new button% [parent btnPanel]
                               [label "Delete"]
                               [callback deleteBtnCallback]))

(define exitBtn (new button% [parent btnPanel]
                               [label "Exit"]
                               [callback exitBtnCallback]))









(send frame show #t)