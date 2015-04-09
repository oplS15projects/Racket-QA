#lang racket

(require rackunit
         "../email-db.rkt")

(init-email-db)

(define mydb (make-email-db (generate-email-db-id) "my-db" '()))
(define mydb2 (make-email-db (generate-email-db-id) "my-db2" '()))

(define db-entry-1 (make-email-db-entry "yong" "j" "cho" "yongjec@gmail.com"))
(define db-entry-2 (make-email-db-entry "tom" "s" "yushi" "tom@gmail.com"))

(add-email-db-entry mydb db-entry-1)
(add-email-db-entry mydb db-entry-2)
(add-email-db-entry mydb2 db-entry-1)
(add-email-db-entry mydb2 db-entry-2)

(add-to-email-db-list mydb)
(add-to-email-db-list mydb2)
