#lang racket

(require "../scheduler.rkt")

(define test-thread-1 (make-timer-thread 30000000000000000 "data 1" (current-thread)))
(thread-send test-thread-1 'print)
(sleep 0.2)
(thread-send test-thread-1 '(update 5000))
(define message #f)
(set! message (thread-receive))
message
(thread-send test-thread-1 '(update 9000))
(set! message (thread-receive))
message
(thread-send test-thread-1 (list 'update (+ 10 (current-seconds))))
(set! message (thread-receive))
message

(kill-thread test-thread-1)