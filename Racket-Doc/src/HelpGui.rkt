;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: HelpGui.rkt
;; Author: James Kuczynski
;; Email: jkuczyns@cs.uml.edu
;; File Description: This file provides a "help" GUI.
;;
;; Created 04/04/2015 10:00pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/gui)

(define frame (new frame%
                   (label "Racket-Doc")
                   (width 300)
                   (height 350)))

(new message% [parent frame]
              [label "Help"])






(send frame show #t)