#lang racket

;; needs a way for the manager thread to find a thread by
;; some test information (test id? file path?)

(provide (all-defined-out))

(define active-test-list '())
(define inactive-test-list '())

(define (make-test tid name path)
  (define (change-name new-name)
    (set! name new-name))
  (define (change-path new-path)
    (set! path new-path))
  (define (dispatch m a)
    (cond ((eq? m 'name) name)
          ((eq? m 'tid) tid)
          ((eq? m 'path) path)
          ((eq? m 'set-name) (change-name a))
          ((eq? m 'set-path) (change-path a))))
  dispatch)

(define (test-name test)
  (test 'name ""))

(define (make-timer-thread notify-at data parent)
  (thread
   (lambda ()
     (let loop ((notify-at notify-at)
                (data data)
                (parent parent))
       (define command #f)
       (if (> (current-seconds) notify-at)
           (begin
             (thread-send parent (list 'Due! notify-at data))
             (set! command (thread-receive)))
           (set! command (thread-try-receive)))
       (when (not (eq? command #f))
         (cond ((and (pair? command) (eq? (car command) 'update))
                (printf "Updating due time to ~a~n" (cadr command))
                (set! notify-at (cadr command)))
               ((eq? command 'print)
                (printf "Due: ~a, Data: ~a~n" notify-at data))
               (else
                (void))))
       (loop notify-at data parent)))))


(define (current-date) (date-day (seconds->date (current-seconds))))
(define (current-month) (date-month (seconds->date (current-seconds))))
(define (current-year) (date-year (seconds->date (current-seconds))))

(define (leap-year?)
  (define year (current-year))
  (or (and (equal? 0 (modulo year 4))
           (not (equal? 0 (modulo year 100))))
      (equal? 0 (modulo year 400))))

(define (days-in-month month)
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
         (if (leap-year?)
             29
             28))
        (else #f)))
