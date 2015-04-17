;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: RacketDocGui.rkt
;; Author: James Kuczynski
;; Email: jkuczyns@cs.uml.edu
;; File Description: This file saves data to file.
;;
;; Created 04/04/2015 1:40pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/file)
(require "NotificationGui.rkt")

(provide toFile)


(define input (open-input-file "./../tests/Test.rkt"))
(define output (open-output-file "./../output/OutputDoc.rkt"
                                 #:mode 'text
                                 #:exists 'update))

(define (toFile listOfLists reqsBool inclsBool provsBool procsBool)
  (display "writing to file...")
  (cond ( (equal? reqsBool #t)
          (write-string "requires-------------------------------" output)
          ;(write-string (car (car listOfLists)) output)
          (loopout (car listOfLists))
        ))
  (cond ( (equal? inclsBool #t)
          (write-string "includes-------------------------------" output)
          ;(write-string (car (cdr listOfLists)) output)
          (loopout (car (cdr listOfLists)))))
   (cond ( (equal? provsBool #t)
          (write-string "provides-------------------------------" output)
          ;(write-string (car (cdr (cdr listOfLists))) output)
          (loopout (car (cdr (cdr listOfLists))))
        ))
   (cond ( (equal? procsBool #t)
          (write-string "procedures & data-----------------------" output)
          ;(write-string (car (cdr (cdr listOfLists))) output)
          (loopout (car (cdr (cdr (cdr listOfLists)))))
        ))
  (write-string "documentation blocks---------------------" output)
  (loopout (car (cdr (cdr (cdr (cdr listOfLists))))))
  (close-output-port output)
  (send dialog show #t)
)
      


(define (loopout mylist)
  (cond ( (null? mylist)
          (write-string "\n" output)
        )
        (else
         (write-string "\n" output)
         (write-string (car mylist) output)
         (loopout (cdr mylist))
        )
  )
)
  
#|(define (toFile requireLst includeLst provideLst procHeaderLst procDocLst)
  (write-string "requires-----------------------\n")
  (write-string requireLst)
  (write-string "includes-----------------------\n")
  (write-string includeLst)
  (write-string "provides-----------------------\n")
  (write-string provideLst)
  (write-string "procedure headers & variables-----------------------\n")
  (write-string procHeaderLst)
  (write-string "documentation blocks-----------------------\n")
  (write-string procDocLst))|#
  
;;test read/write
#|(define thisLine "")

(define (each-line file)
  (set! thisLine (read-line input))
  (cond ( (eof-object? thisLine)
          (display "\n")
        )
        (else
         (display "\n")
         (display thisLine)
         (write-string thisLine output)
         (write-string "\n" output)
         (each-line file)
        )
  )
)

(each-line input)|#
