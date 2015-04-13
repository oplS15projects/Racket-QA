;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: AdvancedGui.rkt
;; Author: James Kuczynski
;; Email: jkuczyns@cs.uml.edu
;; File Description: "Advanced options" GUI for Racket-Doc
;;
;; Created 04/04/2015 1:40pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/gui)

(provide advFrame)
(provide reqs?)
(provide incls?)
(provide provs?)
(provide procs?)

(define reqs? #t)
(define incls? #t)
(define provs? #t)
(define procs? #t)

(define advFrame (new frame%
                   (label "Racket-Doc")
                   (width 400)
                   (height 300)))

(new message% [parent advFrame]
              [label "Advanced Options"])

;create panels
(define optionPanel (new vertical-panel% [parent advFrame]
                                          [alignment '(left center)]))
(define btnPanel (new horizontal-panel% [parent advFrame]
                                        [alignment '(right bottom)]))


;callbacks
(define (reqCbCallback cb event)
  (display "\n\"Requre\" cb clicked\n")
  (set! reqs? (flip-bool reqs?))
  (display reqs?))

(define (inclCbCallback cb event)
  (display "\n\"Include\" cb clicked\n")
  (set! incls? (flip-bool reqs?))
  (display incls?))

(define (provCbCallback cb event)
  (display "\"Provide\"cb clicked\n")
  (set! provs? (flip-bool provs?))
  (display provs?))

(define (procCbCallback cb event)
  (display "\"Proc\"cb clicked\n")
  (set! procs? (flip-bool procs?))
  (display procs?))


(define (okBtnCallback button event)
  (display "\"ok\" button pressed\n")
  (send advFrame show #f))

(define (cancelBtnCallback button event)
  (display "cancel\n")
  (set! reqs? #t)
  (set! incls? #t)
  (set! provs? #t)
  (set! procs? #t)
  (send advFrame show #f))

;;helper to flip a bool true->false or false->true
(define (flip-bool var)
  (if (equal? var #t)
      #f
      #t))

;create checkboxes
(define reqCb (new check-box% [label "\"Required\" Libraries"]
                              [parent optionPanel]
                              [value #t]
                              [callback reqCbCallback]))

(define inclCb (new check-box% [label "\"Included\" Files"]
                               [parent optionPanel]
                               [value #t]
                               [callback inclCbCallback]))

(define provCb (new check-box% [label "\"Provided\" Procedures"]
                               [parent optionPanel]
                               [value #t]
                               [callback provCbCallback]))

(define procCb (new check-box% [label "Procedures"]
                               [parent optionPanel]
                               [value #t]
                               [callback procCbCallback]))


;create buttons
(define okBtn (new button% [parent btnPanel]
                           [label "OK"]
                           [callback okBtnCallback]))

(define cancelBtn (new button% [parent btnPanel]
                               [label "Cancel"]
                               [callback cancelBtnCallback]))
                              




(send advFrame show #f)
