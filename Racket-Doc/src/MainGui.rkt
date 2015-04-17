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

(require "Parser.rkt")
(require "FileIO.rkt")
(require "AdvancedGui.rkt")
(require "ProcessingGui.rkt")

(define background
  (read-bitmap "./../share/racket1.jpg"))

(define frame (new frame%
                   (label "Racket-Doc")
                   (width 400)
                   (height 500)))

(new message% [parent frame]
              [label background])

;;create radio button callback
(define (selectFileOrDirCallback radio event)
  (display (send fileOrDirRBtn get-selection))
  (display "\n"))

;;create button callbacks
(define (selectInCallback button event)
  (display "\n\"Select (in)\" button pressed\n")
    (cond ( (= (send fileOrDirRBtn get-selection) 0)
            (define filepath (get-file))
            (send inputTextField set-value (path->string filepath))
          )
          (else
           (define dirpath (get-directory))
           (send inputTextField set-value (path->string dirpath))
          )))

(define (selectOutCallback button event)
  (display "\n\"Select (out)\" button pressed\n")
    (cond ( (= (send fileOrDirRBtn get-selection) 0)
            (define filepath (get-file))
            (if (equal? (path->string filepath) #f)
                (display "Invalid path selected")
                (send outputTextField set-value (path->string filepath))
            )
          )
          (else
           (define dirpath (get-directory))
           (send outputTextField set-value (path->string dirpath))
          )))

(define (okBtnCallback button event)
  (display "\n\"Run\" button pressed\n")
  ;(send frame show #f)
  ;(send procFrame show #t)
  (toFile (each-line "./../tests/Test.rkt" '() '() '() '() '()) reqs? incls? provs? procs?))

(define (advancedBtnCallback button event)
  (display "\n\"Advanced\" button pressed\n")
  (send advFrame show #t)
  )

(define (cancelBtnCallback button event)
  (display "\n\"Cancel\" button pressed\n")
  (send frame show #f))

;create panels
(define radioPanel (new horizontal-panel% [parent frame]
                                          [alignment '(center center)]))


(define inputPanel (new horizontal-panel% [parent frame]
                                          [alignment '(center center)]))
(define outputPanel (new horizontal-panel% [parent frame]
                                           [alignment '(center center)]))
                        
(define btnPanel (new horizontal-panel% [parent frame]
                                        [alignment '(right bottom)]))

;create input text fields
(define inputTextField (new text-field% [parent inputPanel]
                                        [label "Input Location:"]
                                        [min-width 150]))
;(send inputTextField set-value "/path/to/input/dir/or/.rkt/file")


(define outputTextField (new text-field% [parent outputPanel]
                                         [label "Output Location:"]
                                         [min-width 150]))
;(send outputTextField set-value "/path/to/output/dir")


;create radio buttons
(define fileOrDirRBtn (new radio-box% [parent radioPanel]
                                      [label "Input Options"]
                                      [choices '("File" "Directory")]
                                      [callback selectFileOrDirCallback]))

;create buttons
(define selectInBtn (new button% [parent inputPanel]
                                 [label "Select"]
                                 [callback selectInCallback]))

(define selectOutBtn (new button% [parent outputPanel]
                                  [label "Select"]
                                  [callback selectOutCallback]))


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