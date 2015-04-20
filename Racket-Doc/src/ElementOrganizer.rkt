;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; File: PageGenerator.rkt                                      ;;
;; Author: James Kuczynski                                      ;;
;; Email: jkuczyns@cs.uml.edu                                   ;;
;; File Description: This file compares the tokenized elements  ;;
;;                   of the source files of a given package     ;;
;;                   to ensure not specifing duplacates.  This  ;;
;;                   is only nessisary if there are at least 2  ;;
;;                   source files in the given package.         ;;
;;Created 04/14/2015                                            ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide catWithoutDuplLst)

;;compile a list of "required" packages, OR "included" packages, OR etc.  The firstList can 
;;double as the final list.
(define (catWithoutDuplLst firstList . listOfOtherLists)
   (looper firstList (flatten listOfOtherLists))
)


;;this takes each element and passes it to exists? to compare against
;;the master list.
(define (looper finalList myList)
  (cond ( (null? myList)
          finalList
        )
        (else
         (cond ( (exists? finalList (car myList))
                 (looper finalList (cdr myList))
               )
               (else
                (looper (cons (car myList) finalList) (cdr myList))
               )

         )
        )
  )
)


;;This procedure compares a given element against a master list.
(define (exists? masterList element)
  (cond ( (null? masterList)
          #f
        )
        ( (equal? (car masterList) element)
           #t
        )
        (else
         (exists? (cdr masterList) element)
        )
  )
)
  
  
;;exe--------------------
#|
(catWithoutDuplLst '("#lang racket" "racket/gui"   "scheme/gui")
                   '("#lang racket" "racket/gui"   "racket/filesystem" "racket/regex")
                   '("#lang racket" "racket/gui"   "racket/regex"      "racket/io")
                   '("scheme/gui"   "racket/regex"))
|#


