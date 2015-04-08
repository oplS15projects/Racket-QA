#lang racket

(require racket/file
         racket/gui/base
         "user-settings-directory.rkt")


;; Exporting all definitions only so they can be tested by a test code
;; during development.
(provide (all-defined-out))


(define SYSTEM-TYPE (system-type))
(define EMAIL-DB-DIRNAME (cond ((eq? SYSTEM-TYPE 'windows) "Email DB")
                               ((eq? SYSTEM-TYPE 'unix) "Email_DB")
                               ((eq? SYSTEM-TYPE 'macosx) "Email DB")
                               (else #f)))
(define EMAIL-DB-DIRPATH (full-path-in-settings-directory EMAIL-DB-DIRNAME))
(define EMAIL-DB-LIST-FILE-NAME "edblist")
(define EMAIL-DB-LIST-FILE (cleanse-path-string 
                                 (string-append EMAIL-DB-DIRPATH "/" EMAIL-DB-LIST-FILE-NAME)))
(define EMAIL-DB-FILE-PREFIX "edb")
(define email-db-list '())
(define existing-email-db-ids '())


;; Creates necessary files and initializes global variables
;; to get the email database functioning.
;; Populates email-db-list global variable.
(define (init-email-db)
  (when (not (directory-exists? EMAIL-DB-DIRPATH))    
    (create-email-db-directory))
  (cond ((not (file-exists? EMAIL-DB-LIST-FILE))
         (create-email-db-list-file)
         (write-email-db-list))
        (else 
         (validate-and-correct-email-db-list-file)
         (read-email-db-list)
         (set! existing-email-db-ids
               (ids-in-email-db-list)))))

;; Creates the email db directory in the application settings directory.
;; This directory will store all the email dbs.
;; This procedure needs to be called only on the first run.
(define (create-email-db-directory)
  (when (not (directory-exists? EMAIL-DB-DIRPATH))
    (write "Creating email database directory")
    (make-directory* EMAIL-DB-DIRPATH)))

;; Creates the list file that will contain the list of
;; all email-db's and their id's and paths to their files.
(define (create-email-db-list-file)
  (when (not (file-exists? EMAIL-DB-LIST-FILE))
    (call-with-output-file EMAIL-DB-LIST-FILE 
      (lambda (out) (void))
      #:mode 'binary 
      #:exists 'truncate/replace)))




;; *******************************************************
;; * email-db-entry object
;; *******************************************************
(define (make-email-db-entry name address)
  (define (change-name new-name)
    (set! name new-name))
  (define (change-addr new-addr)
    (set! address new-addr))
  (define (dispatch m a)
    (cond ((eq? m 'get-name) name)         
          ((eq? m 'get-addr) address)
          ((eq? m 'set-name) (change-name a))
          ((eq? m 'set-addr) (change-addr a))))
  dispatch)

;; selectors and mutators for email-db-entry object
(define (email-db-entry-name db-entry)
  (db-entry 'get-name ""))
(define (email-db-entry-email-address db-entry)
  (db-entry 'get-addr ""))
(define (set-email-db-entry-name! db-entry new-name)
  (db-entry 'set-name new-name))
(define (set-email-db-entry-email-address! db-entry new-addr)
  (db-entry 'set-addr new-addr))

;; Checks if two email-db-entries are the same.
(define (same-email-db-entry? entry-a entry-b)
  (cond ((eq? entry-a entry-b) #t)
        ((equal? (email-db-entry-email-address entry-a)
                 (email-db-entry-email-address entry-b)) #t)
        (else #f)))

;; Converts email-db-entry to list
(define (email-db-entry-to-list db-entry)
  (list (email-db-entry-name db-entry)
        (email-db-entry-email-address db-entry)))

;; Creates an email-db-entry from a list of strings
(define (list-to-email-db-entry a-list)
  (make-email-db-entry (car a-list)  ; name
                       (cadr a-list)))  ; email



;; *******************************************************
;; * email-db object
;; *******************************************************
(define (make-email-db db-id db-name email-db-entries)
  (let ((db-file-path (db-id-to-file-path db-id)))
    (define (change-db-name new-db-name)
      (set! db-name new-db-name))
    ;; TODO: check for duplicate address
    (define (add-entry new-entry)
      (set! email-db-entries (append email-db-entries (list new-entry))))
    (define (remove-entry an-entry)
      (define (not-this-entry an-entry)
        (lambda (entry)
          (not (same-email-db-entry? entry an-entry))))
      (set! email-db-entries (filter (not-this-entry an-entry) email-db-entries)))
    (define (remove-by-email-addr an-addr)
      (define (not-this-addr an-addr)
        (lambda (entry)
          (not (equal? an-addr (email-db-entry-email-address entry)))))
      (set! email-db-entries (filter (not-this-addr an-addr) email-db-entries)))
    (define (search-by-addr addr)
      (define result (filter (lambda (entry) (equal? (email-db-entry-email-address entry) addr))
                             email-db-entries))
      (cond ((not (null? result))
             (car result))
            (else #f)))
    (define (dispatch m a)
      (cond ((eq? m 'id) db-id)
            ((eq? m 'name) db-name)
            ((eq? m 'entries) email-db-entries)
            ((eq? m 'change-name) (change-db-name a))
            ((eq? m 'add-entry) (add-entry a))
            ((eq? m 'remove-entry) (remove-entry a))
            ((eq? m 'remove-by-email) (remove-by-email-addr a))
            ((eq? m 'empty?) (null? email-db-entries))
            ((eq? m 'size) (length email-db-entries))
            ((eq? m 'path) db-file-path)
            ((eq? m 'search-by-addr) (search-by-addr a))))
    dispatch))
    
;; selectors and mutators for email-db object
(define (email-db-id db) 
  (db 'id ""))
(define (email-db-name db) 
  (db 'name ""))
(define (email-db-entries db) 
  (db 'entries ""))
(define (email-db-file-path db)
  (db 'path ""))
(define (set-email-db-name! db new-name) 
  (db 'change-name new-name)
  (write-email-db db))
(define (add-email-db-entry db entry) 
  (db 'add-entry entry)
  (write-email-db db))
(define (remove-email-db-entry db entry) 
  (db 'remove-entry entry)
  (write-email-db db))
(define (remove-email-db-entry-by-address db addr) 
  (db 'remove-by-email addr)
  (write-email-db db))
(define (length-email-db db)
  (db 'size ""))
(define (get-db-entry-by-addr db addr)
  (db 'search-by-addr addr))
(define (replace-db-entry db old-entry new-entry)
  (set-email-db-entry-name! old-entry (email-db-entry-name new-entry))
  (set-email-db-entry-email-address! old-entry (email-db-entry-email-address new-entry))
  (write-email-db db))


;; Checks whether an email-db is empty.
(define (email-db-empty? db)
  (db 'empty? ""))

;; Converts an email-db object to a list.
(define (email-db-to-list db)
  (list (email-db-id db)
        (email-db-name db)
        (map email-db-entry-to-list (email-db-entries db))))

;; Constructs an email-db object from a list of strings.
(define (list-to-email-db a-list)
  (make-email-db (car a-list)  ; db-id
                 (cadr a-list)  ; db-name
                 (map list-to-email-db-entry (caddr a-list))))  ; db-entries


;; Replaces an entry in email-db
;; TODO: Finish this.
(define (replace-email-db-entry db-id old-entry new-entry)
  (void))
  

;; Writes an email-db structure to a file.
(define (write-email-db db)
  (define path (email-db-file-path db))
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
;; * email-db list
;; *******************************************************

;; Returns the list of all the email-db-ids in the current email-db-list.
(define (ids-in-email-db-list)
  (map email-db-id email-db-list))

;; Adds an entry to the email-db-list.
;; This is usually called when a new email-db is created.
(define (add-to-email-db-list new-db)
  (cond ((member (email-db-id new-db) (ids-in-email-db-list))
         (printf "add-to-email-db-list: failed to add db id ~a - duplicate id~n" (email-db-id new-db))
         #f)
        (else
         (set! email-db-list (append email-db-list (list new-db)))
         (write-email-db-list)
         (write-email-db new-db))))

;; Removes an entry from the email-db-list.
;; This is usually called when an email-db is deleted.
(define (remove-from-email-db-list db-id)
  (set! email-db-list
    (remove* (list db-id)
             email-db-list
             (lambda (id entry) (equal? db-id (email-db-id entry)))))
  (write-email-db-list)
  (when (file-exists? (db-id-to-file-path db-id))
    (delete-file (db-id-to-file-path db-id))))

;; Returns an existing email-db with the given id.
(define (find-db-by-id id)
  (define result (filter (lambda (db)
            (equal? id (email-db-id db)))
          email-db-list))
  (if (null? result)
      #f
      (car result)))

;; Writes the current email-db-list to the file.
(define (write-email-db-list)
  (define (db-to-storage-form db)
    (list (email-db-id db)
          (email-db-name db)
          (email-db-file-path db)))
  (define email-db-list-storage-form (map db-to-storage-form email-db-list))
  (call-with-output-file EMAIL-DB-LIST-FILE    
    (lambda (out) (write email-db-list-storage-form out))
    #:mode 'binary 
    #:exists 'truncate/replace))

;; Reads the email-db-list from the file.
(define (read-email-db-list)
  (define (storage-form-to-db stored-db)
    (define path (caddr stored-db))
    (call-with-input-file path
      (lambda (db-in)
        (list-to-email-db (read db-in)))))
  (call-with-input-file EMAIL-DB-LIST-FILE
    (lambda (in)
      (set! email-db-list (map storage-form-to-db (read in))))
    #:mode 'binary))

;; Checks the integrity of the contents in the EMAIL-DB-LIST-FILE and removes invalid 
;; entries. Leaves a valid email-db-list which can be used throughout the program run.
(define (validate-and-correct-email-db-list-file)
  (void))



;; good-o accumulate helper
(define (accumulate sequence init op term)
  (cond ((null? sequence) init)
        (else (op (term (car sequence))
                  (accumulate (cdr sequence) init op term)))))
