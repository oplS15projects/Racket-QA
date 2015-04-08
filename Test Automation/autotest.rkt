#lang racket
(require racket/date)

(define SECONDS-IN-A-DAY 86400)
(define test-queue '())

;; (define date-now (current-date))
;; (define datetime (parameterize ((date-display-format 'rfc2822))
;;                    (date->string (seconds->date (current-seconds)) #t)))

;; (current-date) -> (date* 6 35 19 6 4 2015 1 95 #t -14400 503027915 "Eastern Daylight Time")


(define an-hour-later (+ 3600 (current-seconds)))
(define ten-minutes-later (+ 600 (current-seconds)))
(current-seconds)
ten-minutes-later
an-hour-later

(define (make-queue-entry next-due data)
  (define (dispatch m)
    (cond ((eq? m 'when-due) next-due)
          ((eq? m 'data) data)
          (else "Unknown command")))
  dispatch)

;; Queues a test suite.
(define (add-to-queue queue-entry)
  (set! test-queue (append test-queue (list queue-entry)))
  (set! test-queue (sort test-queue a-smaller?)))

;; Finds out which test suite comes due first.
;; Used to sort the test-queue.
(define (a-smaller? a b)
  (< a b))



;; Create a separate loop thread which continuously compares current seconds
;; with the seconds it was given.
;; it should be able to update the given seconds when given a new one.








          
;; Automatic-test-runner module operation

;; Has no GUI

;; Starts in the background when the machine starts (if possible in all platforms).

;; Or starts in the background when the user runs the main program UI
;; if it is not already running (GUI can check this)


;; Finds out the current time

;; Scans Auto-Test directory for an active scheduled test
;; (each scheduled test suite will have an active flag)

;; Priority-Queues a list of due-date/testsuite pair

;; (--Loop--)
;; Continuously checks the current time(seconds)/day for matching the top-queue item
;; once the top-queue is due, 
;; put it back in the priority-queue as next-due-time(seconds)/testsuite
;; check if the test is already running (test may run longer than the interval)
;; , then execute the test in a separate thread
;; (--End Loop--)