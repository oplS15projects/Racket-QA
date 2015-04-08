;; This code tests the procedures in "email-db.rkt".

#lang racket

(require rackunit
         "../email-db.rkt")

(init-email-db)



(define TEST-DB-NAME-1 "Dev Team A")
(define TEST-DB-NAME-2 "Dev Team B")
(define TEST-DB-NAME-3 "Team Racket Science")

(define test-email-db-1
  (make-email-db (generate-email-db-id)
                 TEST-DB-NAME-1
                 (list
                  (make-email-db-entry "Yong J. Cho" "yongjec@gmail.com")
                  (make-email-db-entry "Timothy M. McClusky" "mtimothy@yahoo.com")
                  (make-email-db-entry "Carrie Mason" "cmason@gmail.com")
                  (make-email-db-entry "Ashley Lyon" "ashlyon@nba.com")
                  (make-email-db-entry "Bill Gordon" "bill.gordon@naver.com")
                  (make-email-db-entry "Laura Gonzalez" "l.gonzalez@aspen.com")
                  (make-email-db-entry "Tom S. Uksa" "tsuksa@gmail.com")
                  (make-email-db-entry "Abia Brown" "abrown@teradyne.com")
                  (make-email-db-entry "Lynne Thompson" "lthompson@ibm.com")
                  (make-email-db-entry "Ezria Lopez" "LoPeZ@google.com"))))

(define test-email-db-2
  (make-email-db (generate-email-db-id)
                 TEST-DB-NAME-2
                 (list
                  (make-email-db-entry "Hikari Taylor" "httr.asn@ipage.com")
                  (make-email-db-entry "Janina Wilson" "ieusl@HostGator.com")
                  (make-email-db-entry "Elsie Etherie" "cyj1983@Arvixe.com")
                  (make-email-db-entry "GreeN Lopez" "godaddy@GoDaddy.com")
                  (make-email-db-entry "Cree R. Young" "cree9382@yahoo.com")
                  (make-email-db-entry "Orana Christian" "Orana.Christian@bae.com")
                  (make-email-db-entry "Moriko Enrio" "Moriko_Enrio@student.uml.edu")
                  (make-email-db-entry "Ena J. Garcia" "Ena_Garcia@faculty.uml.edu")
                  (make-email-db-entry "Daisy A. Miller" "daisy_miller@osu.edu"))))

(define test-email-db-3
  (make-email-db (generate-email-db-id)
                 TEST-DB-NAME-3
                 (list
                  (make-email-db-entry "Roy S. VanLieu" "racket.riot@gmail.com")
                  (make-email-db-entry "James T. Kuczynski" "racket.riot@gmail.com")
                  (make-email-db-entry "Yong J. Cho" "yong_cho@student.uml.edu"))))

(add-to-email-db-list test-email-db-1)
(add-to-email-db-list test-email-db-2)
(add-to-email-db-list test-email-db-3)