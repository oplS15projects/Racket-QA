#lang racket
(require racket/date
         "../Common/user-settings-directory.rkt")

(provide (except-out (all-defined-out)
                     SYSTEM-TYPE))

(define SYSTEM-TYPE (system-type))
(define AUTOTEST-DIRNAME (cond ((eq? SYSTEM-TYPE 'windows) "Autotest DB")
                                  ((eq? SYSTEM-TYPE 'unix) "Autotest_DB")
                                  ((eq? SYSTEM-TYPE 'macosx) "Autotest DB")
                                  (else #f)))
(define AUTOTEST-DIRPATH (full-path-in-settings-directory AUTOTEST-DIRNAME))
(define AUTOTEST-LIST-FILE-NAME "atlist")
(define AUTOTEST-LIST-FILE (cleanse-path-string 
                            (string-append AUTOTEST-DIRPATH "/" AUTOTEST-LIST-FILE-NAME)))
(define AUTOTEST-FILE-PREFIX "at")


(define autotest-list '())
(define existing-autotest-ids '())



;; Creates necessary files and initializes global variables
;; to get the autotest database functioning.
;; Populates autotest-list global variables.
(define (init-autotest)
  (when (not (directory-exists? AUTOTEST-DIRPATH))    
    (create-autotest-directory))
  (cond ((not (file-exists? AUTOTEST-LIST-FILE))
         (create-autotest-list-files)
         (write-autotest-lists))
        (else 
         (read-autotest-lists)
         (set! existing-autotest-ids
               (ids-in-autotest-lists)))))

;; Creates the autotest directory in the application settings directory.
;; This directory will store all the autotest information.
;; This procedure needs to be called only once on the first run.
(define (create-autotest-directory)
  (when (not (directory-exists? AUTOTEST-DIRPATH))
    (write "Creating autotest database directory")
    (make-directory* AUTOTEST-DIRPATH)))

;; Creates the list file that will contain the list of
;; all autotests and their id's and paths to their files.
(define (create-autotest-list-files)
  (when (not (file-exists? AUTOTEST-LIST-FILE-NAME))
    (call-with-output-file AUTOTEST-LIST-FILE-NAME 
      (lambda (out) (void))
      #:mode 'binary 
      #:exists 'truncate/replace)))

;; Writes an email-db structure to a file.
(define (write-autotest at)
  (define path (autotest-at-file-path at))
  (call-with-output-file path
    (lambda (out) (write (email-db-to-list db) out))
    #:mode 'binary 
    #:exists 'truncate/replace))

;; Reads an email-db structure from a file and returns it.
(define (read-email-db db-id)
  (define db-file-path (db-id-to-file-path db-id))
  (call-with-input-file db-file-path
    (lambda (in)
      (define (email-db? something)
        #t)
      (define contents (read in))
      (if (email-db? contents)
          (list-to-email-db contents)
          '()))
    #:mode 'binary))


;; Checks if there is an existing email-db with the given id.
(define (email-db-id-exists? id)
  (accumulate existing-email-db-ids
              #f 
              (lambda (a b) (or a b))
              (lambda (entry)
                (equal? id entry))))
   
;; Generates a number value unique from all existing email-db ids.
(define generate-email-db-id
  (let ((try-this-id 0))
    (lambda ()
      (cond ((not (email-db-id-exists? try-this-id)) 
             (set! existing-email-db-ids (append existing-email-db-ids (list try-this-id)))
             try-this-id)
            (else (set! try-this-id (+ 1 try-this-id))
                  (generate-email-db-id))))))

;; Converts to a number to email-db file name.
;; ex) 12 -> "edb00012"
(define (db-id-to-file-name id-number)
  (string-append EMAIL-DB-FILE-PREFIX 
                 (~a id-number #:min-width 5 #:align 'right #:pad-string "0")))

(define (db-id-to-file-path id-number)
  (cleanse-path-string (string-append EMAIL-DB-DIRPATH "/" (db-id-to-file-name id-number))))

;; Converts an email-db file name to its id.
;; ex) "edb00012" -> 12
(define (file-name-to-db-id db-file-name)
  (string->number (cadr (regexp-match "edb([0-9]*)" db-file-name))))



;; *******************************************************
;; * autotest-lists
;; *******************************************************

;; Returns the list of all the existing autotest ids.
(define (ids-in-autotest-list)
  (map autotest-id autotest-list))

;; Adds an entry to autotest-list.
(define (add-to-autotest-list at)
  (cond ((member (autotest-id at) (ids-in-autotest-list))
         (printf "add-to-autotest-list: failed to add autotest id ~a - duplicate id~n" (autotest-id at))
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



;; Finds out which test suite comes due first.
;; Used to sort the test-queue.
(define (a-smaller? a b)
  (< a b))



;; autotest object - contains all information needed to schedule an automatic test
(define (make-autotest id
                       name
                       at-file-path
                       active? 
                       files 
                       type  ; 'one-time or 'periodic                   
                       year 
                       month 
                       date 
                       daily? 
                       mon? 
                       tue? 
                       wed? 
                       thu? 
                       fri? 
                       sat? 
                       sun?
                       notify?
                       email-db)
  
  ;; next-due-in-seconds
  
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
          ((eq? m 'notify?) notify?)
          ((eq? m 'email-db) email-db)
          
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
          ((eq? m 'set-notify?) (set! notify? a))
          ((eq? m 'set-email-db) (set! email-db a))
          (else (void))))
  dispatch)

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
(define (autotest-notify? at) (at 'notify? #f))
(define (autotest-email-db at) (at 'email-db #f))

;; mutators for autotest obj
(define (autotest-set-name at new-name) (at 'set-name new-name))
(define (autotest-set-at-file-path at file-path) (at 'set-at-file-path file-path))
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
(define (autotest-set-notify? at notify?) (at 'set-notify? notify?))
(define (autotest-set-email-db at email-db) (at 'set-email-db email-db))










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