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

(require "../../Common/user-settings-directory.rkt")
(require "DirSearcher.rkt")
(require "ElementOrganizer.rkt")
(require "Parser.rkt")
(require "PageGenerator.rkt")
(require "AdvancedGui.rkt")
(require "ProcessingGui.rkt")
(require "NotificationGui.rkt")

(provide frame)

(define background
  (read-bitmap "./Racket-Doc/share/racket1.jpg"))

(define frame (new frame%
                   (label "Racket-Doc")
                   (width 400)
                   (height 500)))

(new message% [parent frame]
              [label background])

;;create radio button callback
(define (selectFileOrDirCallback radio event)
  ;(display (send fileOrDirRBtn get-selection))
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

;;select output directory
(define outputDir "")

(define (selectOutCallback button event)
  (display "\n\"Select (out)\" button pressed\n")
  (display "selecting directory")
  (define dirpath (get-directory))
  (set! outputDir (cleanse-path-string (string-append (path->string dirpath) "/racketDocOutput.rkt")))
  (display "- - -\n")
  (display outputDir)
  (display "- - -\n")
  (send outputTextField set-value (path->string dirpath)))


(define (okBtnCallback button event)
  (display "\n\"Run\" button pressed\n")
  ;(send frame show #f)
  ;(send procFrame show #t)
  ;;each-line will return a list of lists: a list containing the requires, includes, etc.
  (cond ( (= (send fileOrDirRBtn get-selection) 0)
          (let ([aFileStruct (each-line (open-input-file (send inputTextField get-value)) (file-name-from-path (send inputTextField get-value)) '() '() '() '() '() '())])
            (generationMaster outputDir
                              (list aFileStruct) ;;list of file structures (i.e. "objects")
                              (cons "PLACE_HOLDER" (list (car aFileStruct))) ;;list of file names
                              (if (equal? reqs? #t) (car (cdr (cdr aFileStruct))) '() ) ;;list of "requires"
                              (if (equal? incls? #t) (car (cdr (cdr (cdr aFileStruct)))) '()) ;;list of "includes"
                              (if (equal? provs? #t) (car (cdr (cdr (cdr (cdr aFileStruct))))) '())  ;;list of "provides"
                              (car (cdr (cdr (cdr (cdr (cdr aFileStruct)))))) ;;list of proc "headers"
                              (car (cdr (cdr (cdr (cdr (cdr (cdr aFileStruct))))))) ;;list of code blocks
                              (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr aFileStruct))))))))) ;; list of proc doc blocks 
           )
         )
        (else
            (display "deal with dir of files here")
            ;;pass each file to each-line
            (define (loopEachLine fileList finalList)
              (cond ( (null? fileList)
                      (display "successfully finished extracting data from files.")
                      (display "\n---------------------------\n")
                      (display (getRequiresLooper finalList '()) )
                      (display "\n---------------------------\n")
                      (generationMaster outputDir
                                        finalList
                                        (cons "PLACE_HOLDER" (getFileNameLooper finalList '()))
                                        (if (equal? reqs? #t) (catWithoutDuplLst (getRequiresLooper finalList '()) ) '())
                                        (if (equal? incls? #t) (catWithoutDuplLst (getIncludesLooper finalList '()) ) '())
                                        (if (equal? provs? #t) (getProvidesLooper finalList '()) '())
                                        (getProcHeadersLooper finalList '())
                                        (getProcBodiesLooper finalList '())
                                        (getProcDocBlocksLooper finalList '())
                      )
                    )
                    (else
                     (loopEachLine (cdr fileList) (cons (each-line (open-input-file (car fileList)) (file-name-from-path (car fileList)) '() '() '() '() '() '()) finalList))
                    )
              )
            )
            (loopEachLine (search (send inputTextField get-value)) '())
            ;;bundle
            ;;pass to generationMaster
        )
   ) ;; end of cond
  (send dialog show #t)
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


;;---------------------helper procedures-----------------------


(define (getFileNameLooper fileList finalList)
  (cond ( (null? fileList)
          (reverse (flatten finalList))
        )
        (else
         (getFileNameLooper (cdr fileList) (cons (car (car fileList)) finalList))
        )
  )
)


(define (getRequiresLooper fileList finalList)
  (cond ( (null? fileList)
          (flatten finalList)
        )
        (else
         (getRequiresLooper (cdr fileList) (cons (car (cdr (cdr (car fileList)))) finalList))
        )
  )
)


(define (getIncludesLooper fileList finalList)
  (cond ( (null? fileList)
          (flatten finalList)
        )
        (else
         (getIncludesLooper (cdr fileList) (cons (car (cdr (cdr (cdr (car fileList))))) finalList))
        )
  )
)


(define (getProvidesLooper fileList finalList)
  (cond ( (null? fileList)
          (flatten finalList)
        )
        (else
         (getProvidesLooper (cdr fileList) (cons (car (cdr (cdr (cdr (cdr (car fileList)))))) finalList))
        )
  )
)


(define (getProcHeadersLooper fileList finalList)
  (cond ( (null? fileList)
          (flatten finalList)
        )
        (else
         (getProcHeadersLooper (cdr fileList) (cons (car (cdr (cdr (cdr (cdr (cdr (car fileList))))))) finalList))
        )
  )
)


(define (getProcBodiesLooper fileList finalList)
  (cond ( (null? fileList)
          (flatten finalList)
        )
        (else
         (getProcBodiesLooper (cdr fileList) (cons (car (cdr (cdr (cdr (cdr (cdr (cdr (car fileList)))))))) finalList))
        )
  )
)


(define (getProcDocBlocksLooper fileList finalList)
  (cond ( (null? fileList)
          (flatten finalList)
        )
        (else
         (getProcDocBlocksLooper (cdr fileList) (cons (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (car fileList))))))))) finalList))
        )
  )
)




(send frame show #f)
