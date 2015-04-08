#lang racket

;; Name: Roy Van Liew
;; Section: 91.301.201 - Organization of Programming Languages

;;+++++++++++++++ Required for auto grading +++++++++++++++++++++++++
(define nil '())

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; **********************************************************************

;;1. Exercise 2.36 on pp. 120, accumulating with lists of lists.
;; fill in code where it's got <??> in two places. Page 163.
;;
;; e.g.,
;; (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9)))
;; 
;; should be
;; (12 15 18)
;;
;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;;       nil
;;       (cons (accumulate op init <??>  )
;;             (accumulate-n op init <??> ))))
;; seqs is a list of lists, so every element is itself a sublist. This means we can take
;; the car of the sublists using map to extract the correct list of values to add together with accumulate.
;; So for example, the first accumulate call would return the following:
;; '(1 4 7) <-- this is the result from (map car seqs) and is summed up
;; Then the last accumulate call in this procedure would return the following:
;; '((2 3) (5 6) (8 9)) <-- Notice how we're in the second elements of each sublist now
;; Call car again in the next recursive call, you get '(2 5 8) to add up and so forth.
(define (accumulate-n op init list-with-sublists)
  (if (null? (car list-with-sublists))
       nil
       (cons (accumulate op init (map car list-with-sublists))
             (accumulate-n op init (map cdr list-with-sublists)))))


;; **********************************************************************

;;2. Exercise 2.53 on pp. 144, testing quote, pair?, and memq. Page 195.
;; Write the answers and explanation in comment line and change #f to #t

;(list 'a 'b 'c)
; '(a b c)
; Using a single quote just means to treat the following characters as
; a literal interpretation, not a symbol -- so there isn't a lookup.
; So we're making a list of literals.
(define p2_1 #t)

;(list (list 'george))
; '((george))
; The inner list call will result in a sublist, and the single quote before
; george means to interpret it literally. So this list containing the single
; literal 'george will be inside the outer list.
(define p2_2 #t)

;(cdr '((x1 x2) (y1 y2)))
; '((y1 y2))
; Each element in this list is a sublist of two elements. The car of the outer
; list is the first sublist, and the cdr is pointing to the second sublist.
(define p2_3 #t)

;(cadr '((x1 x2) (y1 y2)))
; '(y1 y2)
; cadr means the car of the cdr, so here cdr is going to the rest of the
; list which in this case is only '((y1 y2)). This sublist contains the
; sublist (y1 y2) and the empty list, and the car will get you the first
; element which is the sublist (y1 y2).
(define p2_4 #t)

;(pair? (car '(a short list)))
; #f
; '(a short list) creates a list with the literals 'a, 'short, and 'list.
; So this means the list has three elements, and taking the car only gets
; the first literal 'a which is not a pair so therefore false.
(define p2_5 #t)

;(memq 'red '((red shoes) (blue socks)))
; #f
; memq here checks if the literal symbol 'red is contained in the list.
; 'red is not eq? to any item in the list. Keep in mind that in (red shoes)
; this is embedded in a sublist and eq? will be comparing 'red with (red shoes)
; which is not the same object.
(define p2_6 #t)

;(memq 'red '(red shoes blue socks))
; '(red shoes blue socks)
; memq returns the sublist of the list beginning with the first occurrence
; of the symbol 'red. Here since we have a flat list with 'red, memq finds
; the literal 'red and is able to compare 'red with 'red which is the same object.
; Previously above, memq was doing 'red with (red shoes) and although (car '(red shoes))
; does give you 'red, the 'red was embedded in a sublist previously and to eq? that is a
; different object.
(define p2_7 #t)

;; **********************************************************************

;;3. Exercise 2.54 on pp. 145, building your own version of equal?. Page 196.
; Doing several checks here:
; 1.) If handed two empty lists, return true.
; 2.) If list1 and list2 are pairs, if the car of both lists satisfy equal?, if
;     the cdr of both lists satisfy equal? then return true. Otherwise, false.
; 3.) If list1 and list2 are eq? (the same object), return true.
; 4.) Otherwise, return false.

(define (my-equal? list1 list2) 
   (cond ((and (null? list1) (null? list2)) #t)
         ((and (pair? list1) (pair? list2)) ; do we have pairs here?
          (and (equal? (car list1) (car list2))) ; are car of both lists equal?
          (if (equal? (cdr list1) (cdr list2)) #t #f)) ; are cdr of both lists equal?
         ((eq? list1 list2) #t) ; if not a pair, is it same symbol?
         (else #f)) ; end cond
) ;; end define

;;4. Exercise 2.55 on pp. 145, pushing the limits of quote. Page 196.
;
; Eva Lu Ator types to the interpreter the expression
;
; (car ''abracadabra)
; 
; To her surprise, the interpreter prints back quote. Explain.
; 
; Write the answer and explanation for the below procedure call and replace
; #f with #t.
;
; > (car ''abracadabra)
; 'quote
; > (car (cdr ''abracadabra))
; 'abracadabra
; > (cdr (cdr ''abracadabra))
; '()
; ''abracadabra here is interpreted as '(quote abracadabra). This means we
; have actually generated a list of two elements; the second ' is interpreted
; literally as the symbol 'quote.
(define p4 #t)

;; **********************************************************************
(provide (all-defined-out))

