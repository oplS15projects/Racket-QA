#lang racket

(require net/smtp
         openssl/mzssl
         racket/date
         racket/gui/base             
         2htdp/batch-io
         "../Common/user-settings-directory.rkt")

(provide send-text
         send-html-file
         send-text-file
         valid-address?
         open-smtp-config)


;; SMTP log-in credentials will only be hard-coded for demo purpose.
;; For production copy(?), they will be obtained from the user at first 
;; program run.
(define SMTP-SERVER-ADDR "smtp.googlemail.com")
(define SMTP-PORT 465)
(define SMTP-USER "test.racketscience")
(define SMTP-PASSWORD "12#$zxCV")

;; Needs error checking
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


;; Returns #t if the argument is a valid email-address string
;; or an error message if it is not.
(define (valid-address? something)
  (cond ((not (string? something)) #f)
        ((equal? (regexp-match #rx"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+.[a-zA-Z0-9.-]+$" something) #f) #f)
        (else #t)))        

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
  (define (valid-recipients? some-list)
    (foldr (lambda (a b) (and a b)) #t (map valid-address? list-of-recipients)))
  (cond ((not (string? to)) 
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
         (smtp-send-message SMTP-SERVER-ADDR
                            from
                            list-of-recipients
                            (construct-header from to subject text-type)
                            (list body)
                            #:port-no SMTP-PORT
                            #:auth-user SMTP-USER
                            #:auth-passwd SMTP-PASSWORD
                            #:tcp-connect ssl-connect))))


;; Sends a string in email.
;;
;; to -> string?
;;  ex) "Aristotle Project Dev Team A"
;;
;; subject -> string?
;;  ex) "Lower your mortgage rate"
;;
;; a-string-to-send -> string?
;;  ex) "Become an insider! Here are 5 quick tips for lowering your mortgage rate ..."
;;
;; list-of-recipients -> listof string?
;;  ex) (list "roy.racketscience@gmail.com" "racket.riot@gmail.com")
;;
(define (send-text to subject a-string-to-send list-of-recipients)
  (connect-and-send to subject a-string-to-send list-of-recipients 'text))


;; Convenience proc for sending an html-formatted text file as the email-body.
;;
;; to -> string?
;;  ex) "Aristotle Project Dev Team A"
;;
;; subject -> string?
;;  ex) "Autotest Result 4/9/2015"
;;
;; html-file-path -> string?
;;  ex) "D:\\Data\\online\\Development\\Git Clone\\FPX\\a-html-file.html"
;;
;; list-of-recipients -> listof string?
;;  ex) (list "roy.racketscience@gmail.com" "racket.riot@gmail.com")
;;
(define (send-html-file to subject html-file-path list-of-recipients)
  (define body (read-file html-file-path))
  (connect-and-send to subject body list-of-recipients 'html))


;; Convenience proc for sending the entire contents of a text file.
;;
;; to -> string?
;;  ex) "Aristotle Project Dev Team A"
;;
;; subject -> string?
;;  ex) "Autotest Result 4/9/2015"
;;
;; text-file-path -> string?
;;  ex) "D:\\Data\\online\\Development\\Git Clone\\FPX\\a-text-file.txt"
;;
;; list-of-recipients -> listof string?
;;  ex) (list "roy.racketscience@gmail.com" "racket.riot@gmail.com")
;;
(define (send-text-file to subject text-file-path list-of-recipients)
  (define body (read-file text-file-path))
  (connect-and-send to subject body list-of-recipients 'text))




;; Converting plain text to html-tagged text
;(define file-lines (file->lines results-filepath))
;(define body-with-br (map (lambda (x) (string-append x "<br />")) file-lines))
;(define body-into-one-string (accumulate-for-email string-append "" body-with-br))
;(define body-with-p (string-append "<p>" body-into-one-string "</p>"))
;(define body (list body-with-p))

  
;; **********************************************************************
;; * This section involves managing the SMTP log-in information.
;; * The SMPT log-in information need to be supplied by the user once 
;; * per machine/user to be able to send emails. If this information
;; * has not yet been provided, (email-available?) will return #f.
;; * Once the user enters the information, it will be stored in the
;; * machine's hard drive and used in the subsequent executions. 
;; **********************************************************************
(define smtp-profile-filename "smtp_profile.dat")

(define smtp-profile-filepath (full-path-in-settings-directory smtp-profile-filename))

(define (email-function-available?) 
  #t)


;; **********************************************************************
;; * This section contains UI that interacts with the user to
;; * acquire and store the SMTP log-in information.
;; **********************************************************************
(define main-dialog
  (new dialog%
       (label "SMTP Configuration")
       (width 300)
       (height 250)
       (style '(close-button))))

(define v-pane
  (new vertical-pane%
       (spacing 5)
       (border 10)
       (parent main-dialog)))

(define server-text-field
  (new text-field%
       (parent v-pane)
       (label "Server (ex. smtp.googlemail.com)")
       (init-value "")
       (style '(single vertical-label))))

(define username-text-field
  (new text-field%
       (parent v-pane)
       (label "Username")
       (init-value "")
       (style '(single vertical-label))))

(define password-text-field
  (new text-field%
       (parent v-pane)
       (label "Password")
       (init-value "")
       (style '(single password vertical-label))))

(define button-pane
  (new horizontal-pane%
       (parent v-pane)
       (vert-margin 5)
       (alignment '(center center))))

;; Cancel button
(new button% 
     (parent button-pane) 
     (label "Cancel")
     (callback (λ (button event)
                 (send main-dialog show #f))))

;; Ok button saves profile to profile-filepath.
(new button% 
     (parent button-pane) 
     (label "Ok")
     (callback (λ (button event)
                 (save-profile
                  (send server-text-field get-value)
                  (send username-text-field get-value)
                  (send password-text-field get-value)
                  smtp-profile-filepath)
                 (send main-dialog show #f))))

(when (system-position-ok-before-cancel?)
  (send button-pane change-children reverse))     

;; TODO: filepath shouldn't be an argument
(define (save-profile server username password filepath)
  ;; TODO: check if directory exists
  (define out (open-output-file filepath #:mode 'binary #:exists 'truncate/replace))
  (fprintf out "~a\t~a\t~a~n" server username password)
  (close-output-port out))

(define (open-smtp-config)
  (cond ((file-exists? smtp-profile-filepath)
         (define in (open-input-file smtp-profile-filepath #:mode 'binary))
         (define raw-filedata (port->string in))
         (close-input-port in)
         (define server (cadr (regexp-match #rx"^(.*)\t(.*)\t(.*)$" raw-filedata)))
         (define username (caddr (regexp-match #rx"^(.*)\t(.*)\t(.*)$" raw-filedata)))
         (define password (cadddr (regexp-match #rx"^(.*)\t(.*)\t(.*)$" raw-filedata)))
         (send server-text-field set-value server)
         (send username-text-field set-value username)
         (send main-dialog show #t))
        (else
         (send main-dialog show #t))))


