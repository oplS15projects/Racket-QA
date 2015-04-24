;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; File: smtp-info.rkt
;; Author: Yong Cho 
;; Email: Yong_Cho@student.uml.edu
;; File Description: UI for saving SMTP log-in info
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/gui/base
         "../Common/user-settings-directory.rkt")

(provide smtp-info-available?
         read-smtp-info
         open-smtp-config)


(define DEMO? #t)
;; SMTP log-in credentials will be hard-coded only for demo purpose.
;; For release, they will be obtained from the user through the
;; mailing list UI.
(define SMTP-SERVER-ADDR "smtp.googlemail.com")
(define SMTP-PORT 465)
(define SMTP-USER "test.racketscience")
(define SMTP-PASSWORD "12#$zxCV")
(define SMTP-PROFILE-FILENAME "smtp_profile")
(define SMTP-PROFILE-DIRNAME "smtp")
(define SMTP-PROFILE-FILE
  (cleanse-path-string 
   (string-append 
    (full-path-in-settings-directory SMTP-PROFILE-DIRNAME) "/" SMTP-PROFILE-FILENAME)))



;;@return #t if user has provided smtp log-in info
;;        #f otherwise
;;
(define (smtp-info-available?)
  (cond (DEMO? #t)
        (else
         (let-values (((server user pass port) (read-smtp-info)))
           (not (equal? #f (and server user pass port)))))))

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

(define port-no-text-field
  (new text-field%
       (parent v-pane)
       (label "Port number")
       (init-value "465")
       (style '(single vertical-label))))

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
(define ok-button
  (new button% 
       (parent button-pane) 
       (label "Ok")
       (callback (λ (button event)
                   (when (and (not (equal? "" (string-trim (send server-text-field get-value))))
                              (not (equal? "" (string-trim (send username-text-field get-value))))
                              (not (equal? "" (string-trim (send password-text-field get-value))))
                              (not (equal? "" (string-trim (send port-no-text-field get-value)))))
                     (save-profile
                      (send server-text-field get-value)
                      (send username-text-field get-value)
                      (send password-text-field get-value)
                      (send port-no-text-field get-value))
                     (send main-dialog show #f))))))

(when (system-position-ok-before-cancel?)
  (send button-pane change-children reverse))     

(define (save-profile server username password port-no-string)
  (when (not (directory-exists? (full-path-in-settings-directory SMTP-PROFILE-DIRNAME)))
    (make-directory-in-settings-directory SMTP-PROFILE-DIRNAME))
  (call-with-output-file SMTP-PROFILE-FILE
    (lambda (out)
      (fprintf out "~a\t~a\t~a\t~a~n" server username password port-no-string))
    #:mode 'binary #:exists 'truncate/replace))



;; Reads smtp log-in information from the user's storage.
;;
;;@return (values servername username password portnumber) if the information exists.
;;        (values #f #f #f #f) if it does not.
;;
(define (read-smtp-info)
  (cond (DEMO? (values SMTP-SERVER-ADDR SMTP-USER SMTP-PASSWORD SMTP-PORT))
        ((file-exists? SMTP-PROFILE-FILE)
         (call-with-input-file SMTP-PROFILE-FILE
           (lambda (in)
             (let ((raw-filedata (port->string in)))
               (define-values (server username password port-no)
                 (cond ((regexp-match #rx"^.*\t.*\t.*\t.*$" raw-filedata)
                        (values (list-ref (regexp-match #rx"^(.*)\t(.*)\t(.*)\t(.*)$" raw-filedata) 1)
                                (list-ref (regexp-match #rx"^(.*)\t(.*)\t(.*)\t(.*)$" raw-filedata) 2)
                                (list-ref (regexp-match #rx"^(.*)\t(.*)\t(.*)\t(.*)$" raw-filedata) 3)
                                (list-ref (regexp-match #rx"^(.*)\t(.*)\t(.*)\t(.*)\n$" raw-filedata) 4)))
                       (else (values #f #f #f #f))))
               (values server username password (string->number port-no))))))
        (else (values #f #f #f #f))))



;; Opens a dialog box to configure smtp information.
;; The user must supply server name, user name, and password.
(define (open-smtp-config)
  (cond ((or DEMO? (file-exists? SMTP-PROFILE-FILE))
         (define-values (server username password port) (read-smtp-info))
         (when server
           (send server-text-field set-value server))
         (when username
           (send username-text-field set-value username))
         (when password
           (send password-text-field set-value password))
         (when port
           (send port-no-text-field set-value (number->string port)))
         (send main-dialog show #t))
        (else
         (send main-dialog show #t))))
