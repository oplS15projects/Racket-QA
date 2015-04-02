#lang racket

(require net/smtp
         openssl/mzssl
         racket/date
         racket/file  
         racket/gui/base             
         racket/system
         "address-db.rkt"
         "user-settings-directory.rkt")

;; SMTP log-in credentials will only be hard-coded for demo purpose.
;; For production copy(?), they will be obtained from the user at first 
;; program run.
(define SMTP-SERVER-ADDR "smtp.googlemail.com")
(define SMTP-PORT 465)
(define SMTP-USER "test.racketscience")
(define SMTP-PASSWORD "12#$zxCV")
(define SYSTEM-TYPE (system-type))

(define (construct-header from to subject)
  (define CRLF "\r\n")
  (define datetime (parameterize ((date-display-format 'rfc2822))
                 (date->string (seconds->date (current-seconds)) #t)))
  (string-append "From: " from CRLF
                 "To: " to CRLF
                 "Subject: " subject CRLF
                 "Date: " datetime CRLF
                 "Mime-Version: 1.0" CRLF
                 "Content-Type: text/html; charset=UTF-8" CRLF
                 CRLF))

(define (connect-and-send)
  ;; Some of these should come in as parameters.
  (define from "Unit Test")
  (define to "QA Team")
  (define subject "Regression Statistics")
  (define to-addresses (list "yong.cho3@gmail.com"))
  (define body '("<p>Test 1 - ok<br />Test 2 - ok<br />Test 3 - ok<br />Test 4 - fail</p>"))
  
  (define header
    (construct-header from to subject))
  
  (smtp-send-message SMTP-SERVER-ADDR
                     from
                     to-addresses
                     header
                     body
                     #:port-no SMTP-PORT
                     #:auth-user SMTP-USER
                     #:auth-passwd SMTP-PASSWORD
                     #:tcp-connect ssl-connect))


;; **********************************************************************
;; * This section involves managing the SMTP log-in information.
;; * The SMPT log-in information need to be supplied by the user once 
;; * per machine/user to be able to send emails. If this information
;; * has not yet been provided, (email-available?) will return #f.
;; * Once the user enters the information, it will be stored in the
;; * machine's hard drive and used in the subsequent executions. 
;; * On Windows, the information is saved in %APPDATA%\"Racket Unit Test"
;; * folder. On Linux, it is saved in $HOME/.Racket_Unit_Test directory.
;; * The specific data saved are the SMTP server address, log-in username, 
;; * and the password.
;; **********************************************************************
(define smtp-profile-filename "smtp_profile.dat")

(define smtp-profile-filepath (full-path-in-settings-directory smtp-profile-filename))

(define (email-function-available?) 
  #f)

(define (send-email recipients body)
  #f)


;; **********************************************************************
;; * This section contains GUI that interacts with the user to
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
         ; (send password-text-field set-value password)
         (send main-dialog show #t))
        (else
         (send main-dialog show #t))))


