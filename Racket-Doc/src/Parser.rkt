;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: RacketDocGui.rkt
;; Author: James Kuczynski
;; Email: jkuczyns@cs.uml.edu
;; File Description: This file using string comparison procs
;;                   and regex to select and organize text
;;                   into catagories.
;;
;; Created 04/04/2015 1:40pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/file)

(provide each-line)



(define input (open-input-file "./../tests/Test.rkt"))

(define requireLst '())
(define includeLst '())
(define provideLst '())
(define procHeaderLst '())
(define procDocLst '())
(define prevLine 'null)


(define (each-line file requireLst includeLst provideLst procHeaderLst procDocLst)
  (let ([thisLine (read-line input)])
    ;(display thisLine)
    ;(display "\n")
    (cond ( (eof-object? thisLine)
          #|
          (display requireLst)
          (display "\n- - - - - - - - - - - -\n")
          (display includeLst)
          (display "\n- - - - - - - - - - - -\n")
          (display provideLst)
          (display "\n- - - - - - - - - - - -\n")
          (display procHeaderLst)
          (display "\n- - - - - - - - - - - -\n")
          (display procDocLst)
          |#
          (list requireLst includeLst provideLst procHeaderLst procDocLst)
        )
        (else
         (cond
              ( (equal? (req? thisLine) #t) ;;requires
                (set! prevLine 'req)
                (each-line file (cons thisLine requireLst) includeLst provideLst procHeaderLst procDocLst)
              )
              ( (equal? (incl? thisLine) #t) ;;includes
                (set! prevLine 'incl)
                (each-line file requireLst (cons thisLine includeLst) provideLst procHeaderLst procDocLst)
              )
              ( (equal? (prov? thisLine) #t) ;;provides
                (set! prevLine 'prov)
                (each-line file requireLst includeLst (cons thisLine provideLst) procHeaderLst procDocLst)
              )
              ( (equal? (proc? thisLine) #t) ;; procedure and data first line (i.e. "header")
                (set! prevLine 'proc)
                (each-line file requireLst includeLst provideLst (cons thisLine procHeaderLst) procDocLst)
              )
              ( (equal? (commentStatus thisLine) 'begin);;--------------------
                (set! prevLine 'docu)
                (each-line file requireLst includeLst provideLst procHeaderLst (cons thisLine procDocLst))
              )
              ( (equal? (commentStatus thisLine) 'middle)
                (set! prevLine 'docu)
                (each-line file requireLst includeLst provideLst procHeaderLst (cons (string-append (car procDocLst) "\n" thisLine) (cdr procDocLst)))
              )
              (else
               (set! prevLine 'proc)
               (each-line file requireLst includeLst provideLst procHeaderLst procDocLst)
              )
         )
        )
    )
  )
)
      

(define (req? line)
  (cond ((equal? (regexp-match #rx"(?<=require).*" line) #f)
         #f
         )
        (else
         #t
        )))
        

(define (incl? line)
  (cond ((equal? (regexp-match #rx"(?<=include).*" line) #f)
         #f
        )
        (else
         #t
        )))


(define (prov? line)
  (cond ((equal? (regexp-match #rx"(?<=provide).*" line) #f)
         #f
        )
        (else
         #t
        )))


(define (proc? line)
  (cond ( (equal? (regexp-match #rx"(?<=define).*" line) #f)
         #f
        )
        (else
         #t
        )))


(define (commentStatus line)
  (cond ( (and (> (string-length line) 2) 
               (equal? (substring line 0 3) "#||")
          )
          'begin
        )
        ( (and (> (string-length line) 2)
               (equal? (substring (remove-lead-whitespace line) 0 1) "|")
          )
          ;(display "!!! ")
          'middle
        )
        ( (and (> (string-length line) 1)
               (equal? (substring line 0 2) "|#")
          )
          'end
        )
        (else
         'error
        )
  )
)
                 
(define (remove-lead-whitespace line)
  (car (regexp-match #rx"[^ \b].*" line)))


;;exe------------------------------
;(each-line input requireLst includeLst provideLst procHeaderLst procDocLst)

