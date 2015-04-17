#||
 | email-db-entry-edit.rkt
 | author: Yong Cho (Yong_Cho@student.uml.edu)
 | Created on: 4/3/2015
 |
 | This file implements user interface to create or edit a
 | mailing list entry.
 |#

#lang racket/gui

(require "email.rkt"
         "email-db.rkt")

(provide open-email-db-entry-edit)

(define title "")
(define new-entry '())

(define email-db-entry-edit-dialog
  (new dialog%
       (label title)
       (width 300)
       (height 250)
       (border 4)
       (style '(close-button))))

(define v-pane
  (new vertical-pane%
       (spacing 2)
       (border 5)
       (parent email-db-entry-edit-dialog)))

(define name-text-field
  (new text-field%
       (parent v-pane)
       (label "Name")
       (init-value "")
       (style '(single vertical-label))))

(define address-text-field
  (new text-field%
       (parent v-pane)
       (label "Email")
       (init-value "")
       (style '(single vertical-label))))

(define button-pane
  (new horizontal-pane%
       (parent v-pane)
       (spacing 10)
       (alignment '(center center))))

;; Cancel button
(new button% 
     (parent button-pane) 
     (label "Cancel")
     (callback (λ (button event)
                 (set! new-entry '())
                 (send email-db-entry-edit-dialog show #f))))

;; Ok button saves entry
(define ok-button
  (new button% 
       (parent button-pane) 
       (label "Ok")
       (callback (λ (button event)
                   (define name (send name-text-field get-value))
                   (define address (send address-text-field get-value))
                   (define name-good? (validate-name name))
                   (cond ((not (equal? name-good? #t))
                          (send warning-message set-label name-good?))
                         ((not (valid-address? address))
                          (send warning-message set-label "Email address is invalid"))
                         (else
                          (set! name (string-trim name))
                          (set! new-entry (make-email-db-entry name address))
                          (send email-db-entry-edit-dialog show #f)))))))

(when (system-position-ok-before-cancel?)
  (send button-pane change-children reverse))

(define warning-message
  (new message%
       (parent email-db-entry-edit-dialog)
       (label "")
       (auto-resize #t)))

;; Returns #t if name is valid or an error message string if it is not.
(define (validate-name name)
  (cond ((< (string-length (string-replace name " " "")) 2)
         "Name is too short")
        ((not (equal? (regexp-match #rx"[^a-zA-Z. ]" name) #f))
         "Name contains an invalid character.")
        (else #t)))

(define (open-email-db-entry-edit button event (old-entry #f))
  (set! title (if (equal? old-entry #f)
                  "Add New Email DB Entry"
                  "Edit Email DB Entry"))
  (send email-db-entry-edit-dialog set-label title)
  (cond ((not (equal? old-entry #f))
         (send name-text-field set-value (email-db-entry-name old-entry))
         (send address-text-field set-value (email-db-entry-email-address old-entry)))
        (else 
         (send name-text-field set-value "")
         (send address-text-field set-value "")))
  (send ok-button focus)
  (send email-db-entry-edit-dialog show #t)
  ;; blocking here.
  ;; new-entry is set by the time the dialog closes.
  new-entry)