#lang racket

(require racket/date)

(provide (all-defined-out))

(define cumulative-days '((0 . 0) (1 . 31)  (2 . 59)   (3 . 90)   (4 . 120)
                                  (5 . 151) (6 . 181)  (7 . 212)  (8 . 243)
                                  (9 . 273) (10 . 304) (11 . 334)))

(define (find-year-day day month year)
  (let ((days-pr (assoc (- month 1) cumulative-days)))
    (if (and (leap-year? year) (> month 2))
        (- (+ day (cdr days-pr) 1) 1)
        (- (+ day (cdr days-pr)) 1))))

(define (find-week-day day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- year a))
         (m (+ month (* 12 a) -2)))
    (modulo (+ day y (quotient y 4) (- (quotient y 100))
               (quotient y 400) (quotient (* 31 m) 12))
            7)))

(define (leap-year? year)
  (or (and (equal? 0 (modulo year 4))
           (not (equal? 0 (modulo year 100))))
      (equal? 0 (modulo year 400))))

(define (current-day) (date-day (seconds->date (current-seconds))))
(define (current-month) (date-month (seconds->date (current-seconds))))
(define (current-year) (date-year (seconds->date (current-seconds))))

(define (days-in-month month year)
  (cond ((or (equal? month 1)
             (equal? month 3)
             (equal? month 5)
             (equal? month 7)
             (equal? month 8)
             (equal? month 10)
             (equal? month 12))
         31)
        ((or (equal? month 4)
             (equal? month 6)
             (equal? month 9)
             (equal? month 11))
         30)
        ((equal? month 2)
         (if (leap-year? year)
             29
             28))
        (else #f)))
