#lang racket

(require racket/file
         racket/gui/base
         "user-settings-directory.rkt")

(define SYSTEM-TYPE (system-type))

(define master-db-filename "address_master.dat")
(define db-dirname
  (cond ((eq? SYSTEM-TYPE 'windows) "Email DB")
        ((eq? SYSTEM-TYPE 'unix) "Email_DB")
        (else #f)))

(define db-dirpath 
  (full-path-in-settings-directory db-dirname))

(define master-db-filepath 
  (cleanse-path-string (string-append db-dirpath "/" master-db-filename)))

(define (create-db-directory)
  (when (not (directory-exists? db-dirpath))
    (make-directory* db-dirpath)))                                 


;; 'email-db-entry' data-structure is a list of first name, last name, middle name, 
;; and email address. It represents an entry for one person.
(define (make-email-db-entry firstname middlename lastname email-address)
  (list firstname middlename lastname email-address))
(define (email-db-entry? lst)
  (equal? (length lst) 4))
(define email-db-entry-firstname car)
(define email-db-entry-middleinitial cadr)
(define email-db-entry-lastname caddr)
(define email-db-entry-email-address cadddr)
(define (email-db-entry-who entry)
  (string-append
   (email-db-entry-firstname entry) " "
   (email-db-entry-middleinitial entry) ". "
   (email-db-entry-lastname entry)))


;; 'email-db' data structure is a list composed of a db name and a list of
;; 'email-db-entry's.
(define (make-email-db db-name email-db-entries)
  (list db-name email-db-entries))
(define email-db-name car)
(define email-db-entries cadr)
(define (email-db-empty? db)
  (null? (email-db-entries db)))
(define (add-email-db-entry email-db entry)
  (make-email-db (email-db-name email-db) (append (email-db-entries email-db) entry)))



