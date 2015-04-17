#||
 | scheduler.rkt
 | author: Yong Cho (Yong_Cho@student.uml.edu)
 | Created on: 4/8/2015
 |
 | This file implements timer and runner threads which are used to
 | keep track of current time and execute automated tests that come due.
 |#

#lang racket

(require racket/date
         "autotest.rkt"
         "../Bottle-Racket/test-tracker.rkt")

(provide make-runner
         make-timer
         INIT-TIMER-COMMAND)

(define RUN-COMMAND 'Run!)
(define INIT-TIMER-COMMAND 'Set-timer)

(define (make-timer runner)
  (define DEBUG #f)
  (thread
   (lambda ()
     (let loop ((at-list autotest-list))
       (define active-at (filter autotest-active? at-list))
       (define cs (current-seconds))
       (for-each 
        (lambda (at)
          (when DEBUG
            (printf "cs = ~a, due = ~a~n" cs (autotest-next-due at)))
          (when (equal? cs (autotest-next-due at))
            (when DEBUG
              (printf "Timer: Sending a run command for ~a~n" (autotest-name at)))
            (thread-send runner (list RUN-COMMAND (autotest-next-due at) at))))
        active-at)
       (define command (thread-try-receive))
       (when (not (eq? #f command))
         (cond ((eq? command 'print)
                (printf "Current-seconds = ~a~nActive autotests:~n" cs) 
                (for-each
                 (lambda (at)
                   (printf "~a due at ~a seconds (~a) on ~a~nFiles:~n" 
                           (autotest-name at) 
                           (autotest-next-due at) 
                           (- (autotest-next-due at) cs)
                           (date->string (seconds->date (autotest-next-due at)) #t))
                   (for-each
                    (lambda (file) (printf "~a~n" file))
                    (autotest-files at))
                   (printf "~n"))
                 (sort active-at (lambda (a b) (< (autotest-next-due a) (autotest-next-due b))))))
               (else
                (void))))
       (let hold ()
         (when (equal? cs (current-seconds))
           (sleep 0.3)
           (hold)))
       (loop autotest-list)))))

(define (make-runner)
  (define DEBUG #t)
  (thread
   (lambda ()
     (let loop ((timer #f))
       (define command (thread-try-receive))
       (when (not (eq? #f command))
         (cond ((and (pair? command)
                     (eq? INIT-TIMER-COMMAND (car command)))
                (set! timer (cadr command)))
               ((and (pair? command)
                     (eq? RUN-COMMAND (car command)))
                (when DEBUG
                  (printf "Test Runner: Received a run command~n"))
                (let ((files (autotest-files (caddr command))))
                  (for-each 
                   (lambda (file)
                     (when DEBUG
                       (printf "Test Runner: Executing file ~a..." file))
                     (with-handlers ((exn:fail? (lambda (e) 
                                                  (printf "~n** Error in (run-test-area ~v)! **~n~a~n"
                                                          file
                                                          (exn-message e)))))
                       (run-test-area file)
                       (when DEBUG
                         (printf "Complete~n"))))
                   files)))
         ((eq? command 'print)
          (thread-send timer 'print))))
     (sleep 1.0)
     (loop timer)))))
