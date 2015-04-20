#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; general
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; last and butlast
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define (butlast lst)
  (define (helper current lst counter)
    (if (> counter 1)
        (helper (append current (list (car lst)))
                (cdr lst) (- counter 1))
        current));; end helper
 (helper '() lst (length lst))) ;; end outer define
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; palindrome?
(define (palindrome? lst)
  (cond ((null? lst) #t)
        ((< (length lst) 2) #t)
        ((not (equal? (car lst) (last lst))) #f)
        (else (palindrome? (cdr (butlast lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; trees
(define test-tree '(1 (2 (3 (4) 5) 6) 7))

(define (tree-manip tree init leaf accum) 
  (cond ((null? tree) init) 
        ((not (pair? tree)) (leaf tree)) 
        (else (accum  
               (tree-manip (car tree) init leaf accum) 
               (tree-manip (cdr tree) init leaf accum)))))

(define (sum-even-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (even? tree)
             tree
             0)) ; end not
        (else (+ (sum-even-leaves (car tree))
                 (sum-even-leaves (cdr tree))))))

(define (sum-odd-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (even? tree)
             tree
             0)) ; end not
        (else (+ (sum-even-leaves (car tree))
                 (sum-even-leaves (cdr tree))))))

(define (count-leaves tree) 
  (tree-manip test-tree 0 (lambda (x) x) +))

(define (triple-leaves tree) 
  (tree-manip test-tree '() (lambda (x) (* x 3)) cons))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end
	 
(provide (all-defined-out))
