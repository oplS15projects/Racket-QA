#||
 | autotest.rkt
 | author: Yong Cho (Yong_Cho@student.uml.edu)
 | Created on: 4/8/2015
 |
 | This file implements autotest object that stores all information
 | needed to execute automatically a set of racket source files specified 
 | by the user, as well as procedures to store and retrieve it.
 |#

#lang racket

(require racket/date
         racket/flonum
         "../QA-Email/email-db.rkt"
         "../Common/user-settings-directory.rkt"
         "calendar.rkt")

(provide (all-defined-out))

(define AUTOTEST-DIRNAME (cond ((eq? (system-type) 'windows) "Autotest DB")
                               ((eq? (system-type) 'unix) "Autotest_DB")
                               ((eq? (system-type) 'macosx) "Autotest DB")
                               (else "autotest_db")))
(define AUTOTEST-DIRPATH (full-path-in-settings-directory AUTOTEST-DIRNAME))
(define AUTOTEST-LIST-FILE-NAME "atlist")
(define AUTOTEST-LIST-FILE (cleanse-path-string 
                            (string-append AUTOTEST-DIRPATH "/" AUTOTEST-LIST-FILE-NAME)))
(define AUTOTEST-ID-FILE-NAME "at-minid")
(define AUTOTEST-ID-FILE (cleanse-path-string
                          (string-append AUTOTEST-DIRPATH "/" AUTOTEST-ID-FILE-NAME)))
(define AUTOTEST-FILE-PREFIX "at")

(define autotest-list '())
(define existing-autotest-ids '())
(define autotest-min-id 0)


;; Creates necessary files and initializes global variables
;; to get the autotest database functioning.
(define (init-autotest)
  (when (not (directory-exists? AUTOTEST-DIRPATH))    
    (create-autotest-directory))
  (when (not (file-exists? AUTOTEST-LIST-FILE))
    (create-autotest-list-file)
    (write-autotest-list))
  (when (not (file-exists? AUTOTEST-ID-FILE))
    (create-autotest-id-file)
    (write-autotest-id-file))
  (read-autotest-list)
  (read-autotest-id-file)
  (set! existing-autotest-ids (ids-in-autotest-list))
  (when (and (not (null? existing-autotest-ids))
             (<= autotest-min-id (argmax identity existing-autotest-ids)))
    (set! autotest-min-id (+ 1  (argmax identity existing-autotest-ids)))
    (write-autotest-id-file)))

;; Creates the autotest directory in the application settings directory.
;; This directory will store all the autotest information.
;; This procedure needs to be called only once on the first run per user/machine.
(define (create-autotest-directory)
  (when (not (directory-exists? AUTOTEST-DIRPATH))
    (printf "Creating autotest database directory~n")
    (make-directory* AUTOTEST-DIRPATH)))

;; Creates the list file that will contain the list of
;; all autotests and their ids and paths to their files.
(define (create-autotest-list-file)
  (when (not (file-exists? AUTOTEST-LIST-FILE))
    (call-with-output-file AUTOTEST-LIST-FILE 
      (lambda (out) (void))
      #:mode 'binary 
      #:exists 'truncate/replace)))

;; Writes an autotest object to a file.
(define (write-autotest at)
  (define path (autotest-at-file-path at))
  (define dirpath (get-dirpath-from-filepath path))
  (when (not (directory-exists? dirpath))
    (make-directory* dirpath))
  (call-with-output-file path
    (lambda (out) (write (autotest-to-list at) out))
    #:mode 'binary 
    #:exists 'truncate/replace))

;; Reads an autotest object from a file and returns it.
(define (read-autotest at-id)
  (define at-file-path (autotest-id-to-file-path at-id))
  (call-with-input-file at-file-path
    (lambda (in)
      (define (autotest? something)
        #t)
      (define contents (read in))
      (if (autotest? contents)
          (list-to-autotest contents)
          '()))
    #:mode 'binary))

;; Creates a file tracking the minimum id
;; to assign to a new autotest.
(define (create-autotest-id-file)
  (when (not (file-exists? AUTOTEST-ID-FILE))
    (call-with-output-file AUTOTEST-ID-FILE
      (lambda (out) (void))
      #:mode 'binary
      #:exists 'truncate/replace)))

(define (write-autotest-id-file)
  (call-with-output-file AUTOTEST-ID-FILE
    (lambda (out)
      (write autotest-min-id out))
    #:mode 'binary
    #:exists 'truncate/replace))

(define (read-autotest-id-file)
  (call-with-input-file AUTOTEST-ID-FILE
    (lambda (in)
      (set! autotest-min-id (read in))
      autotest-min-id)
    #:mode 'binary))

;; Checks if an id exists already.
(define (autotest-id-exists? id)
  (accumulate existing-autotest-ids
              #f 
              (lambda (a b) (or a b))
              (lambda (entry)
                (equal? id entry))))

;; Generates a new autotest id.
;; TODO: change this to resemble the mailing list id generator.
(define generate-autotest-id
  (let ((try-this-id 0))
    (lambda ()
      (cond ((not (autotest-id-exists? try-this-id)) 
             (set! existing-autotest-ids (append existing-autotest-ids (list try-this-id)))
             try-this-id)
            (else (set! try-this-id (+ 1 try-this-id))
                  (generate-autotest-id))))))

;; Converts an id to autotest file name.
;; ex) 12 -> "at00012"
(define (autotest-id-to-file-name id-number)
  (string-append AUTOTEST-FILE-PREFIX
                 (~a id-number #:min-width 5 #:align 'right #:pad-string "0")))

(define (autotest-id-to-file-path id-number)
  (cleanse-path-string (string-append AUTOTEST-DIRPATH "/" (autotest-id-to-file-name id-number))))

;; Converts an autotest file name to its id.
;; ex) "at00012" -> 12
(define (file-name-to-autotest-id autotest-file-name)
  (string->number (cadr (regexp-match (string-append AUTOTEST-FILE-PREFIX "([0-9]*)") autotest-file-name))))



;; *******************************************************
;; * autotest-list
;; *******************************************************

;; Returns the list of all the existing autotest ids.
(define (ids-in-autotest-list)
  (map autotest-id autotest-list))

;; Adds an entry to autotest-list.
(define (add-to-autotest-list at)
  (printf "at = ~a~n" at) 
  (cond ((not (equal? #f (member (autotest-id at) (ids-in-autotest-list))))
         (printf "add-to-autotest-list: duplicate id~n" (autotest-id at))
         #f)
        (else         
         (set! autotest-list (append autotest-list (list at)))
         (write-autotest-list)
         (write-autotest at))))

;; Removes an entry from the autotest-list.
;; This is usually called when an autotest schedule is deleted.
(define (remove-from-autotest-list at-id)
  (set! autotest-list
        (remove* (list at-id)
                 autotest-list
                 (lambda (id entry) (equal? at-id (autotest-id entry)))))
  (write-autotest-list)
  (when (file-exists? (autotest-id-to-file-path at-id))
    (delete-file (autotest-id-to-file-path at-id))))

;; Returns an existing autotest with the specified id.
(define (find-autotest-by-id id)
  (define result (filter (lambda (at)
                           (equal? id (autotest-id at)))
                         autotest-list))
  (if (null? result)
      #f
      (car result)))

;; Writes the current autotest-list to the file.
(define (write-autotest-list)
  (define (at-to-storage-form at)
    (list (autotest-id at)
          (autotest-name at)
          (autotest-at-file-path at)))
  (define autotest-list-storage-form (map at-to-storage-form autotest-list))
  (call-with-output-file AUTOTEST-LIST-FILE    
    (lambda (out) (write autotest-list-storage-form out))
    #:mode 'binary 
    #:exists 'truncate/replace))

;; Reads the autotest-list from the file.
(define (read-autotest-list)
  (define (storage-form-to-at stored-at)
    (define path (caddr stored-at))
    (call-with-input-file path
      (lambda (at-in)
        (list-to-autotest (read at-in)))))
  (call-with-input-file AUTOTEST-LIST-FILE
    (lambda (in)
      (set! autotest-list (map storage-form-to-at (read in))))
    #:mode 'binary))

;; autotest object - contains all information needed to schedule an automatic test
(define (make-autotest id name active? files 
                       type  ; 'one-time or 'periodic                   
                       year month date 
                       daily? mon? tue? wed? thu? fri? sat? sun?
                       hour minute ampm
                       notify? email-db)
  (let ((at-file-path "")
        (hour-in-24 0)
        (next-due 0)
        (due-week-days '())
        (DEBUG #f))
    (define (week-days-to-list)
      (set! due-week-days '())
      (when sun? (set! due-week-days (append due-week-days (list 0))))
      (when mon? (set! due-week-days (append due-week-days (list 1))))
      (when tue? (set! due-week-days (append due-week-days (list 2))))
      (when wed? (set! due-week-days (append due-week-days (list 3))))
      (when thu? (set! due-week-days (append due-week-days (list 4))))
      (when fri? (set! due-week-days (append due-week-days (list 5))))
      (when sat? (set! due-week-days (append due-week-days (list 6)))))    
    (define (find-next-due-date)
      (when DEBUG
        (printf "Starting (find-next-due-date)~n"))
      (week-days-to-list)
      (define current-week-day (date-week-day (current-date)))
      (when DEBUG
        (printf "current-week-day = ~a~n" current-week-day)
        (printf "due-week-days = ~a~n" due-week-days))
      (cond ((null? due-week-days) #f)
            ((and (member current-week-day due-week-days)
                  (< (+ (* (date-hour (current-date)) 60) (date-minute (current-date)))  ; is due today
                     (+ (* hour-in-24 60) minute)))             
             (date-day (current-date)))
            ((and (member current-week-day due-week-days)  ; was due today, now due a week later
                  (equal? (length due-week-days) 1))
             (+ 7 (date-day (current-date))))
            ((eq? #f (findf (lambda (day) (> day current-week-day)) due-week-days))
             (+ (date-day (current-date))
                (- 7 (- current-week-day (argmin identity due-week-days)))))  ; wrapped around to next week
            (else (+ (date-day (current-date))
                     (- (findf (lambda (day) (> day current-week-day)) due-week-days)
                        current-week-day)))))
    (define (update-hour-in-24)
      (set! hour-in-24 (cond ((equal? ampm "AM") (if (equal? hour 12)
                                                     0
                                                     hour))
                             ((equal? ampm "PM") 
                              (if (equal? hour 12)
                                  12
                                  (+ 12 hour)))
                             (else #f))))
    
    (define (update-next-due)
      (let ((cd (current-date))
            (current-minute (date-minute (current-date)))
            (current-hour (date-hour (current-date)))
            (current-day (date-day (current-date)))
            (current-month (date-month (current-date)))
            (current-year (date-year (current-date))))

        (cond ((eq? type 'one-time)
               (define due-at
                 (date->seconds (make-date* 0 
                                            minute
                                            hour-in-24
                                            date
                                            month
                                            year
                                            (find-week-day date month year)
                                            (find-year-day date month year)
                                            (date-dst? cd)
                                            (date-time-zone-offset cd)
                                            (date*-nanosecond cd)
                                            (date*-time-zone-name cd))))
               (if (> due-at (current-seconds))
                   (set! next-due due-at)
                   (set! next-due 0)))
              (else  ; recurring test
               (week-days-to-list)
               (cond ((null? due-week-days) (set! next-due 0) (printf "~a~n" due-week-days))
                     (else
                      (define due-date (find-next-due-date))
                      (define due-month current-month)
                      (define due-year current-year)                    
                      (cond ((> due-date (days-in-month current-month current-year))
                             (if (equal? due-month 12)
                                 (begin (set! due-month 1)
                                        (set! due-year (+ 1 due-year))
                                        (set! due-date (- due-date 31)))
                                 (begin (set! due-month (+ 1 due-month))
                                        (set! due-date (- due-date (days-in-month current-month))))))
                            (else (void)))
                      
                      (define due-at
                        (date->seconds (make-date* 0
                                                   minute
                                                   hour-in-24
                                                   due-date
                                                   due-month
                                                   due-year
                                                   (find-week-day due-date due-month due-year)
                                                   (find-year-day due-date due-month due-year)
                                                   (date-dst? cd)
                                                   (date-time-zone-offset cd)
                                                   0
                                                   (date*-time-zone-name cd))))
                      (set! next-due due-at)))))
        (when DEBUG
          (printf "~n-- <DEBUG INFO> ----------~nnext due at ~a seconds~n" next-due)
          (printf "next-due at ~a~n" (parameterize ((date-display-format 'rfc2822))
                                       (date->string (seconds->date next-due) #t)))
          
          (define due-until (- next-due (current-seconds)))
          (define due-until-minutes (exact->inexact (/ due-until 60)))
          (define due-until-hours (exact->inexact (/ due-until-minutes 60)))
          (printf "~a seconds = ~a minutes = ~a hours until due!~n-------------------------~n"                 
                  due-until
                  due-until-minutes
                  due-until-hours))))
    
    (define (dispatch m a)
      (cond ((eq? m 'id) id)
            ((eq? m 'name) name)
            ((eq? m 'at-file-path) at-file-path)
            ((eq? m 'active?) active?)
            ((eq? m 'files) files)
            ((eq? m 'type) type)
            ((eq? m 'year) year)
            ((eq? m 'month) month)
            ((eq? m 'date) date)
            ((eq? m 'daily?) daily?)
            ((eq? m 'mon?) mon?)
            ((eq? m 'tue?) tue?)
            ((eq? m 'wed?) wed?)
            ((eq? m 'thu?) thu?)
            ((eq? m 'fri?) fri?)
            ((eq? m 'sat?) sat?)
            ((eq? m 'sun?) sun?)
            ((eq? m 'hour) hour)
            ((eq? m 'minute) minute)
            ((eq? m 'ampm) ampm)
            ((eq? m 'hour-in-24) hour-in-24)
            ((eq? m 'notify?) notify?)
            ((eq? m 'email-db) email-db)
            ((eq? m 'next-due) (update-next-due) next-due)
            
            ((eq? m 'set-name) (set! name a))
            ((eq? m 'set-at-file-path) (set! at-file-path a))
            ((eq? m 'set-active?) (set! active? a))
            ((eq? m 'set-files) (set! files a))
            ((eq? m 'add-file) (set! files (append files (list a))))
            ((eq? m 'remove-file) 
             (set! files (filter (lambda (file) (not (equal? file a))) files)))
            ((eq? m 'set-type) (set! type a))
            ((eq? m 'set-year) (set! year a))
            ((eq? m 'set-month) (set! month a))
            ((eq? m 'set-date) (set! date a))
            ((eq? m 'set-daily?) (set! daily? a))
            ((eq? m 'set-mon?) (set! mon? a))
            ((eq? m 'set-tue?) (set! tue? a))
            ((eq? m 'set-wed?) (set! wed? a))
            ((eq? m 'set-thu?) (set! thu? a))
            ((eq? m 'set-fri?) (set! fri? a))
            ((eq? m 'set-sat?) (set! sat? a))
            ((eq? m 'set-sun?) (set! sun? a))
            ((eq? m 'set-hour) (update-hour-in-24) (set! hour a))
            ((eq? m 'set-minute) (set! minute a))
            ((eq? m 'set-ampm) (update-hour-in-24) (set! ampm a))
            ((eq? m 'set-notify?) (set! notify? a))
            ((eq? m 'set-email-db) (set! email-db a))
            (else (void))))
    (update-hour-in-24)
    (set! at-file-path (autotest-id-to-file-path id))
    dispatch))

;; selectors for autotest obj
(define (autotest-id at) (at 'id #f))
(define (autotest-name at) (at 'name #f))
(define (autotest-at-file-path at) (at 'at-file-path #f))
(define (autotest-active? at) (at 'active? #f))
(define (autotest-files at) (at 'files #f))
(define (autotest-type at) (at 'type #f))
(define (autotest-year at) (at 'year #f))
(define (autotest-month at) (at 'month #f))
(define (autotest-date at) (at 'date #f))
(define (autotest-daily? at) (at 'daily? #f))
(define (autotest-mon? at) (at 'mon? #f))
(define (autotest-tue? at) (at 'tue? #f))
(define (autotest-wed? at) (at 'wed? #f))
(define (autotest-thu? at) (at 'thu? #f))
(define (autotest-fri? at) (at 'fri? #f))
(define (autotest-sat? at) (at 'sat? #f))
(define (autotest-sun? at) (at 'sun? #f))
(define (autotest-hour at) (at 'hour #f))
(define (autotest-minute at) (at 'minute #f))
(define (autotest-ampm at) (at 'ampm #f))
(define (autotest-hour-in-24 at) (at 'hour-in-24 #f))
(define (autotest-notify? at) (at 'notify? #f))
(define (autotest-email-db at) (at 'email-db #f))
(define (autotest-next-due at) (at 'next-due #f))
(define (autotest-next-due-string at)
  (date->string (seconds->date (autotest-next-due at)) #t))

;; mutators for autotest object
(define (autotest-set-name at new-name) (at 'set-name new-name))
(define (autotest-set-active? at active?) (at 'set-active? active?))
(define (autotest-set-files at files) (at 'set-files files))
(define (autotest-add-file at new-file) (at 'add-file new-file))
(define (autotest-remove-file at file) (at 'remove-file file))
(define (autotest-set-type at type) (at 'set-type type))  ; 'one-time or 'periodic
(define (autotest-set-year at year) (at 'set-year year))
(define (autotest-set-month at month) (at 'set-month month))
(define (autotest-set-date at date) (at 'set-date date))
(define (autotest-set-daily? at daily?) (at 'set-daily? daily?))
(define (autotest-set-mon? at mon?) (at 'set-mon? mon?))
(define (autotest-set-tue? at tue?) (at 'set-tue? tue?))
(define (autotest-set-wed? at wed?) (at 'set-wed? wed?))
(define (autotest-set-thu? at thu?) (at 'set-thu? thu?))
(define (autotest-set-fri? at fri?) (at 'set-fri? fri?))
(define (autotest-set-sat? at sat?) (at 'set-sat? sat?))
(define (autotest-set-sun? at sun?) (at 'set-sun? sun?))
(define (autotest-set-hour at hour) (at 'set-hour hour))
(define (autotest-set-minute at minute) (at 'set-minute minute))
(define (autotest-set-ampm at ampm) (at 'set-ampm ampm))
(define (autotest-set-notify? at notify?) (at 'set-notify? notify?))
(define (autotest-set-email-db at email-db) (at 'set-email-db email-db))


(define (copy-autotest! from-at to-at)
  (autotest-set-name to-at (autotest-name from-at))
  (autotest-set-active? to-at (autotest-active? from-at))
  (autotest-set-files to-at (autotest-files from-at))
  (autotest-set-type to-at (autotest-type from-at))
  (autotest-set-year to-at (autotest-year from-at))
  (autotest-set-month to-at (autotest-month from-at))
  (autotest-set-date to-at (autotest-date from-at))
  (autotest-set-daily? to-at (autotest-daily? from-at))
  (autotest-set-mon? to-at (autotest-mon? from-at))
  (autotest-set-tue? to-at (autotest-tue? from-at))
  (autotest-set-wed? to-at (autotest-wed? from-at))
  (autotest-set-thu? to-at (autotest-thu? from-at))
  (autotest-set-fri? to-at (autotest-fri? from-at))
  (autotest-set-sat? to-at (autotest-sat? from-at))
  (autotest-set-sun? to-at (autotest-sun? from-at))
  (autotest-set-hour to-at (autotest-hour from-at))
  (autotest-set-minute to-at (autotest-minute from-at))
  (autotest-set-ampm to-at (autotest-ampm from-at))
  (autotest-set-notify? to-at (autotest-notify? from-at))
  (autotest-set-email-db to-at (autotest-email-db from-at))
  (write-autotest to-at))


;; Converts an autotest object to a list.
(define (autotest-to-list at)
  (list (autotest-id at) (autotest-name at) (autotest-active? at)
        (autotest-files at) (autotest-type at) (autotest-year at) (autotest-month at)
        (autotest-date at) (autotest-daily? at) (autotest-mon? at) (autotest-tue? at) (autotest-wed? at) 
        (autotest-thu? at) (autotest-fri? at) (autotest-sat? at) (autotest-sun? at) 
        (autotest-hour at) (autotest-minute at) (autotest-ampm at) (autotest-notify? at)
        (if (not (equal? (autotest-email-db at) #f))
            (email-db-to-list (autotest-email-db at))
            #f)))

;; Converts a list to an autotest object.
(define (list-to-autotest at-list)
  (cond ((< (length at-list) 21) 
         (printf "list-to-autotest: list too short") 
         #f)
        (else
         (make-autotest (list-ref at-list 0)
                        (list-ref at-list 1)
                        (list-ref at-list 2)
                        (list-ref at-list 3)
                        (list-ref at-list 4)
                        (list-ref at-list 5)
                        (list-ref at-list 6)
                        (list-ref at-list 7)
                        (list-ref at-list 8)
                        (list-ref at-list 9)
                        (list-ref at-list 10)
                        (list-ref at-list 11)
                        (list-ref at-list 12)
                        (list-ref at-list 13)
                        (list-ref at-list 14)
                        (list-ref at-list 15)
                        (list-ref at-list 16)
                        (list-ref at-list 17)
                        (list-ref at-list 18)
                        (list-ref at-list 19)
                        (if (equal? #f (list-ref at-list 20))
                            #f
                            (list-to-email-db (list-ref at-list 20)))))))

(init-autotest)
