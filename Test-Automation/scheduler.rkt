#lang racket

(require racket/date
         "autotest.rkt"
         "calendar.rkt")

(define NOTIFY-COMMAND 'Due!)

;; needs a way for the manager thread to find a thread by
;; some test information (test id? file path?)

(provide (all-defined-out))

(define (make-timer-thread runner)
  (thread
   (lambda ()
     (let loop ((at-list autotest-list))
       (define active-at (filter autotest-active? at-list))
       (define command #f)
       (define sleep? #f)
       (for-each 
        (lambda (at)
          (when (equal? (current-seconds) (autotest-next-due at))
            (thread-send runner (list NOTIFY-COMMAND (autotest-next-due at) at))
            (set! sleep? #t)))
        active-at)
       (when sleep?
         (sleep 1.1)
         (set! sleep? #f))
       (set! command (thread-try-receive))

       (when (not (eq? command #f))
         (cond ((eq? command 'print)
                (printf "Current-seconds = ~a~nActive autotests:~n" (current-seconds)) 
                (for-each
                 (lambda (at)
                   (printf "~a due at ~a (~a)~n" 
                           (autotest-name at) 
                           (autotest-next-due at) 
                           (- (autotest-next-due at) (current-seconds))))
                 active-at))
               (else
                (void))))
       (loop autotest-list)))))
















































