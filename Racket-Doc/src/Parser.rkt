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




;(define input (open-input-file "./../tests/Test.rkt"))

#|
(define fileName "File")
(define requireLst '())
(define includeLst '())
(define provideLst '())
(define procHeaderLst '())
(define procDocLst '())|#
(define prevLine 'null)

(define (tmp file fileNm requireLst includeLst provideLst procHeaderLst procDocLst)
  0)

(define (each-line file fileNm requireLst includeLst provideLst procHeaderLst procBodyLst procDocLst)
  (let ([thisLine (read-line file)]
       [fileName fileNm])
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
            (display "reached the end of file.")
            (list (some-system-path->string fileName)
	          #t
                  (reverse requireLst)
                  (reverse includeLst)
                  (reverse provideLst)
                  (reverse procHeaderLst)
                  (reverse procBodyLst)
                  (reverse procDocLst) )
        )
        (else
         (cond
              ( (equal? (req? thisLine) #t) ;;requires
                (set! prevLine 'req)
                (each-line file fileNm (cons (string-replace thisLine "\"" "\\\"") requireLst) includeLst provideLst procHeaderLst procBodyLst procDocLst)
              )
              ( (equal? (incl? thisLine) #t) ;;includes
                (set! prevLine 'incl)
                (each-line file fileNm requireLst (cons (string-replace thisLine "\"" "\\\"") includeLst) provideLst procHeaderLst procBodyLst procDocLst)
              )
              ( (equal? (prov? thisLine) #t) ;;provides
                (set! prevLine 'prov)
                (each-line file fileNm requireLst includeLst (cons (string-replace thisLine "\"" "\\\"") provideLst) procHeaderLst procBodyLst procDocLst)
              )
              ( (equal? (procHead? thisLine) #t) ;; procedure and data first line (i.e. "header")
                (set! prevLine 'procHead)
                (each-line file fileNm requireLst includeLst provideLst (cons (string-replace thisLine "\"" "\\\"") procHeaderLst) procBodyLst procDocLst)
              )
              ( (equal? (commentStatus thisLine) 'begin) ;; beginning of documentation block
                (set! prevLine 'docu)
                (each-line file fileNm requireLst includeLst provideLst procHeaderLst procBodyLst (cons (string-replace (string-replace thisLine "\"" "\\\"") "|" "") procDocLst))
              )
              ( (equal? (commentStatus (string-replace thisLine "\"" "\\\"")) 'middle) ;; middle of documentation block
                (set! prevLine 'docu)
                (each-line file fileNm requireLst includeLst provideLst procHeaderLst procBodyLst (cons (string-append (car procDocLst) "\n" (string-replace (string-replace thisLine "\"" "\\\"") "|" "")) (cdr procDocLst)))
              )
              ( (and (equal? (procBody? thisLine) #t)
                     (equal? prevLine 'procHead)
                )
                (set! prevLine 'procBody)
                (each-line file fileNm requireLst includeLst provideLst procHeaderLst (cons (string-replace thisLine "\"" "\\\"") procBodyLst) procDocLst)
              )
              ( (and (equal? (procBody? thisLine) #t)
                     (equal? prevLine 'procBody)
                )
                (each-line file fileNm requireLst includeLst provideLst procHeaderLst (cons (string-append (car procBodyLst) "\n" (string-replace thisLine "\"" "\\\"")) (cdr procBodyLst)) procDocLst)
              )
              (else
               (set! prevLine 'null) ;; inside procedure
               ;(display prevLine)
               (each-line file fileNm requireLst includeLst provideLst procHeaderLst procBodyLst procDocLst)
               ;(each-line file fileNm requireLst includeLst provideLst procHeaderLst (cons thisLine procBodyLst) procDocLst)
              )
         )
        )
    )
  )
)
      

(define (req? line)
  (cond ( (and (equal? (regexp-match #rx"(?<=require).*" line) #f)
              (equal? (regexp-match #rx"(?<=#lang).*" line) #f)
          )
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

;;determines if the line is a procedure "header".
(define (procHead? line)
  (cond ( (or (equal? (regexp-match #rx"(?<=define).*" line) #f)
              (equal? (substring line 0 1) " ")
          )
         #f
        )
        (else
         #t
        )))

    
(define (procBody? line)
  (cond ( (equal? line "")
          #f
        )
        ( (equal? (substring (remove-lead-whitespace line) 0 1) ";")
          #f
        )
        (else
         #t
        )
  )
)


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

#|
(each-line (open-input-file "/home/james/OplFiles/Racket-QA/Racket-Doc/tests/Test.rkt")
           (file-name-from-path "file_name.rkt")
           '("req1" "req2")
           '("incl1" "incl2")
           '("prov1" "prov2")
           '("procH1" "procH2")
           '("procB1" "procB2")
           '("procDoc1" "procDoc2"))
|#
