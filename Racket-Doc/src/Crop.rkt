;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: RacketDocGui.rkt
;; Author: James Kuczynski
;; Email: jkuczyns@cs.uml.edu
;; File Description: This file crops out unwanted text (i.e. "define").
;;
;; Created 04/12/2015 8:40pm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

#|(define (crop-define line)
  (let ([tmpLine (remove-lead-whitespace line)]
        [size (string-length line)]
        
       )
    (display size)
    (display tmpLine)
  )
)
|#
         
(define (crop-define line)
  (substring (remove-lead-whitespace line) 7 (string-length (remove-lead-whitespace line))))
                 
         
(define (remove-lead-whitespace line)
  (car (regexp-match #rx"[^ \b].*" line)))

;;exe--------------
(crop-define "(define (testcode num1 num2))")