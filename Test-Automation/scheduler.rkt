#||
 | scheduler.rkt
 | author: Yong Cho (Yong_Cho@student.uml.edu)
 | Created on: 4/8/2015
 |
 | This file implements timer and runner threads which are used to
 | keep track of current time and execute automated tests as they come due.
 |#

#lang racket

(require racket/date
         rackunit
         setup/dirs
         "autotest.rkt"
         "../Common/user-settings-directory.rkt"
         "../QA-Email/email.rkt"
         "../QA-Email/email-db.rkt"
         "../Bottle-Racket/test-tracker.rkt")

(provide make-runner
         make-timer
         INIT-TIMER-MESSAGE)

(define RUN-COMMAND 'Run!)
(define INIT-TIMER-MESSAGE 'Set-timer)
(define RACKET-COMMAND
  (string-append
   (whitespace-fix 
    (string-append (path->string (find-console-bin-dir))
                   (cond ((eq? (system-type) 'windows) "racket.exe")
                         ((eq? (system-type) 'unix) "/racket")
                         ((eq? (system-type) 'macosx) "/racket")
                         (else (error "Platform not supported")))))
   " "))

(define (make-shell-command arg-file-path)
  (string-append RACKET-COMMAND (whitespace-fix arg-file-path)))

;; Executes racket executable with a .rkt file as argument. Returns the output as a string.
(define (run-racket-file arg-file-path)
  (call-with-output-string
   (lambda (p) (parameterize ((current-error-port p)
                              (current-output-port p))
                 (system (make-shell-command arg-file-path))))))

(define (attach-output-header at-name executed-time-in-sec output-string arg-file-path)
  (string-append "################################################################################\n"
                 "# Auto-test Name: " at-name "\n"
                 "# RKT File Executed: " arg-file-path "\n"
                 "# Executed on: " (date->string (seconds->date executed-time-in-sec) #t) "\n"
                 "################################################################################\n\n"
                 output-string))

(define (save-result output-string arg-file-path file-exists-option)
  (let ((output-directory
         (cleanse-path-string
          (string-append 
           (get-dirpath-from-filepath arg-file-path) "/Racket-QA Auto-Test Result/"))))
    (when (not (directory-exists? output-directory))
      (make-directory* output-directory))
    (call-with-output-file 
        (cleanse-path-string 
         (string-append output-directory 
                        "result-"
                        (get-filename-from-filepath arg-file-path)
                        ".txt"))
      (lambda (out)
        (fprintf out "~a~n~n~n~n" output-string))
      #:mode 'text
      #:exists file-exists-option)))


;; Timer thread - tracks current time and notifies runner thread when 
;; a scheduled autotest comes due.
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
                 (sort active-at 
                       (lambda (a b) (< (autotest-next-due a) (autotest-next-due b))))))
               (else
                (void))))
       (let hold ()
         (when (equal? cs (current-seconds))
           (sleep 0.3)
           (hold)))
       (loop autotest-list)))))

;; Runner thread - executes an autotest item when directed by timer thread.
(define (make-runner)
  (define DEBUG #f)
  (define VERBOSE #t)
  (thread
   (lambda ()
     (let loop ((timer #f))
       (define command (thread-try-receive))
       (when (not (eq? #f command))
         (cond ((and (pair? command)
                     (eq? INIT-TIMER-MESSAGE (car command)))
                (set! timer (cadr command)))
               ((and (pair? command)
                     (eq? RUN-COMMAND (car command)))
                (when DEBUG
                  (printf "Test Runner: Received a run command~n"))
                (let ((at (caddr command))
                      (notify? (autotest-notify? (caddr command)))
                      (test-name (autotest-name (caddr command)))
                      (email-db (autotest-email-db (caddr command)))
                      (files (autotest-files (caddr command)))
                      (sec (current-seconds)))
                  (autotest-set-last-run at sec)
                  (write-autotest at)
                  (for-each
                   (lambda (file)
                     (when (or DEBUG VERBOSE)
                       (printf "Test Runner: Executing file ~a..." file))
                     (define email-subject (string-append "Autotest Result - " (get-filename-from-filepath file)))
                     (if #t
                         (cond ((and notify? (not (equal? #f email-db)))  ; Bottle-Racket area file
                                (with-handlers 
                                    ((exn:fail? 
                                      (lambda (e) 
                                        (printf "~n** Error in (run-test-area-email ~v ~v email-db)! **~n~a~n"
                                                file
                                                email-subject
                                                (exn-message e)))))
                                  (run-test-area-email file email-subject email-db)))
                               (else
                                (with-handlers 
                                    ((exn:fail? (lambda (e) 
                                                  (printf "~n** Error in (run-test-area ~v)! **~n~a~n"
                                                          file
                                                          (exn-message e)))))
                                  (run-test-area file))))
                         (with-handlers  ; other .rkt file
                             ((exn:fail?
                               (lambda (e)
                                 (printf "~n** Error in (run-racket-file ~v)! **~n~a~n" file (exn-message e)))))
                           (let ((output-string (string-append (run-racket-file file))))
                             (save-result (attach-output-header test-name sec output-string file) file 'append)
                             (when notify? 
                               (send-text (email-db-name email-db) 
                                          email-subject 
                                          (attach-output-header test-name sec output-string file)
                                          (email-db-addresses email-db))))))
                     
                     (when (or DEBUG VERBOSE)
                       (printf "Complete~n")))
                   files)))
               ((eq? command 'print)
                (thread-send timer 'print))))
       (sleep 1.0)
       (loop timer)))))
