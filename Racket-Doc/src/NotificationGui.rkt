#lang racket

(require racket/gui)

(provide dialog)

(define dialog (new dialog%
                   (label "Racket-Doc")
                   (width 200)
                   (height 100)))

(new message% [parent dialog]
              [label "Execution Complete"])

;create panels
(define processingPanel (new vertical-panel% [parent dialog]
                                             [alignment '(left center)]))

(define btnPanel (new horizontal-panel% [parent dialog]
                                        [alignment '(right bottom)]))


;callback
(define (exitBtnCallback button event)
  (display "Exit\n")
  (send dialog show #f))


;create textfieldthingy
(define procMessage (new message% [parent processingPanel]
                            [label "(program is in testing mode)\nFinished process.  Exit out of the app.  Then\ngo to Racket-Doc/output/OutputDoc.rkt to view results"]))

;create button

(define exitBtn (new button% [parent btnPanel]
                               [label "Exit"]
                               [callback exitBtnCallback]))









(send dialog show #f)
