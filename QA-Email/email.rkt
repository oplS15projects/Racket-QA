#||
 | email.rkt
 | author: Yong Cho (Yong_Cho@student.uml.edu)
 | Created on: 4/3/2015
 |
 | This file implements procedures to send an email and a UI to
 | configure smtp settings needed for sending an email.
 |#

#lang racket

(require net/smtp
         openssl/mzssl
         racket/date
         2htdp/batch-io
         "smtp-info.rkt")

(provide send-text
         send-html-file
         send-text-file
         valid-address?)



(define (construct-header from to subject mime-type)
  (define CRLF "\r\n")
  (define datetime (parameterize ((date-display-format 'rfc2822))
                     (date->string (seconds->date (current-seconds)) #t)))
  (string-append "From: " from CRLF
                 "To: " to CRLF
                 "Subject: " subject CRLF
                 "Date: " datetime CRLF
                 "Mime-Version: 1.0" CRLF
                 (cond ((eq? mime-type 'html)
                        "Content-Type: text/html; charset=UTF-8")
                       ((eq? mime-type 'text)
                        "Content-Type: text; charset=UTF-8")
                       (else
                        (raise-argument-error 'construct-header "'html or 'text" mime-type)))
                 CRLF
                 CRLF))


#||
 | Returns #t if the argument is a valid email-address string
 | or an error message if it is not.
 |#
(define (valid-address? something)
  (and (string? something)
       (regexp-match #rx"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+.[a-zA-Z0-9.-]+$" something)))


;; Sends an email with given parameters.
;;
;; Parameters:
;;
;; to -> string?
;;  ex) "Aristotle Project Dev Team A"
;;
;; subject -> string?
;;  ex) "Autotest Result 4/9/2015"
;;
;; body -> string?
;;  ex) "Test 1 - Pass </ br>Test 2 - Fail</ br>"
;;
;; list-of-recipients -> listof string?
;;  ex) (list "roy.racketscience@gmail.com" "racket.riot@gmail.com")
;;
(define (connect-and-send to subject body list-of-recipients text-type)
  (define (valid-recipients? a-list)
    (and (not (null? a-list))
         (foldr (lambda (a b) (and a b)) #t (map valid-address? list-of-recipients))))
  (cond ((not (smtp-info-available?))
         (displayln "Unable to send email: no SMTP information is available"))
        ((not (string? to)) 
         (raise-argument-error 'connect-and-send "string?" to))
        ((not (string? subject)) 
         (raise-argument-error 'connect-and-send "string?" subject))
        ((not (string? body)) 
         (raise-argument-error 'connect-and-send "string?" body))
        ((not (pair? list-of-recipients)) 
         (raise-argument-error 'connect-and-send "list" list-of-recipients))
        ((not (valid-recipients? list-of-recipients)) 
         (raise-argument-error 'connect-and-send "email-address-string?" "invalid-email-address"))
        (else
         (define from "Racket QA")
         (define-values (server user pass port) (read-smtp-info))
         (smtp-send-message server
                            from
                            list-of-recipients
                            (construct-header from to subject text-type)
                            (list body)
                            #:port-no port
                            #:auth-user user
                            #:auth-passwd pass
                            #:tcp-connect ssl-connect))))


#||
 | Sends a string in email.
 | 
 |@param to -> string?
 |          ex) "Team Racket Science"
 |       subject -> string?
 |          ex) "Lower your mortgage rate!"
 |       a-string-to-send -> string?
 |          ex) "Become an insider! Here are 5 quick tips for lowering your mortgage rate ..."
 |       list-of-recipients -> listof string?
 |          ex) (list "roy.racketscience@gmail.com" "racket.riot@gmail.com")
 |#
(define (send-text to subject a-string-to-send list-of-recipients)
  (connect-and-send to subject a-string-to-send list-of-recipients 'text))


#||
 | Sends an html-formatted text file as the email-body.
 |
 |@param to -> string?
 |          ex) "Team Racket Science"
 |       subject -> string?
 |          ex) "Autotest Result 4/9/2015"
 |       html-file-path -> string?
 |          ex) "D:\\Data\\online\\Development\\Git Clone\\FPX\\a-html-file.html"
 |       list-of-recipients -> listof string?
 |          ex) (list "roy.racketscience@gmail.com" "racket.riot@gmail.com")
 |#
(define (send-html-file to subject html-file-path list-of-recipients)
  (connect-and-send to subject (read-file html-file-path) list-of-recipients 'html))


#||
 | Sends a text file as the email-body.
 |
 |@param to -> string?
 |          ex) "Team Racket Science"
 |       subject -> string?
 |          ex) "Autotest Result 4/9/2015"
 |       text-file-path -> string?
 |          ex) "D:\\Data\\online\\Development\\Git Clone\\FPX\\a-text-file.txt"
 |       list-of-recipients -> listof string?
 |          ex) (list "roy.racketscience@gmail.com" "racket.riot@gmail.com")
 |#
(define (send-text-file to subject text-file-path list-of-recipients)
  (connect-and-send to subject (read-file text-file-path) list-of-recipients 'text))
