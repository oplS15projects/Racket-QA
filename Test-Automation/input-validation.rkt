#lang racket

(require "calendar.rkt")

(provide (all-defined-out))


(define year-list
  (list (number->string (current-year)) (number->string (+ 1 (current-year)))))

(define month-list (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"))



;; validate autotest name
(define (valid-autotest-name-string? a-string)
  (< 0 (string-length (string-trim a-string))))

;; validate minute
(define (valid-minute-string? a-string)
  (let ((minute (string->number a-string)))
    (and (exact-nonnegative-integer? minute)
         (<= minute 60)
         (>= minute 0))))

;; validate hour
(define (valid-hour-string? a-string)
  (let ((hour (string->number a-string)))
    (and (exact-nonnegative-integer? hour)
         (< hour 13)
         (> hour 0))))

;; validate am/pm
(define (valid-ampm-string? a-string)
  (or (string=? "AM" a-string)
      (string=? "PM" a-string)))

;; validate year
(define (valid-year-string? a-string)
  (not (equal? #f (member a-string year-list))))
  
;; validate month
(define (valid-month-string? a-string)
  (not (equal? #f (member a-string month-list))))

;; validate date
(define (valid-date-string? a-string month year)
  (let ((max-days (days-in-month month year)))
    (not (or (not (string? a-string))
             (> (string-length a-string) 2)
             (equal? #f (string->number a-string))
             (< (string->number a-string) 1)
             (> (string->number a-string) max-days)))))
