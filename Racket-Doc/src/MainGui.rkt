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
(require "PageGenerator.rkt")
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
  ;;each-line will return a list of lists: a list containing the requires, includes, etc.
  (let ([aFileStruct (each-line (open-input-file (send inputTextField get-value)) (file-name-from-path (send inputTextField get-value)) '() '() '() '() '())])
    (display "\nHERE!\n")
    (display (string-length (car aFileStruct)))
    (display "\n\n")
    (generationMaster (list aFileStruct) ;;list of file structures (i.e. "objects")
                        (list (car aFileStruct)) ;;list of file names
                        (car (cdr (cdr aFileStruct))) ;;list of "requires"
                        (car (cdr (cdr (cdr aFileStruct)))) ;;list of "includes"
                        (car (cdr (cdr (cdr (cdr aFileStruct))))) ;;list of "provides"
                        (car (cdr (cdr (cdr (cdr (cdr aFileStruct)))))) ;;list of proc "headers"
                        '("NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA") ;;list of code blocks
                        (car (cdr (cdr (cdr (cdr (cdr (cdr aFileStruct))))))) ;;list of proc doc blocks 
                        )
  )
)

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