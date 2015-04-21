#||
 | email-db-ui.rkt
 | author: Yong Cho (Yong_Cho@student.uml.edu)
 | Created on: 4/3/2015
 |
 | This file implements user interface to manage multiple mailing lists.
 |#

#lang racket/gui

(require "email.rkt"
         "email-db.rkt"
         "email-db-entry-edit-ui.rkt"
         "smtp-info.rkt")

(provide open-manage-mailing-list-dialog)



(init-email-db)

(define return-list '())
(define return-db #f)

(define manage-mailing-list-dialog
  (new dialog%
       (label "Manage Mailing List")
       (spacing 2)
       (style '(close-button))))

(define main-h-panel
  (new horizontal-panel%
       (parent manage-mailing-list-dialog)
       (spacing 10)
       (border 8)
       (alignment '(left top))))

(define left-v-panel
  (new vertical-panel%
       (parent main-h-panel)
       (spacing 0)
       (alignment '(center top))))

(define right-v-panel
  (new vertical-panel%
       (parent main-h-panel)
       (alignment '(left top))))

;; List box for the list of email-dbs (left pane).
(define email-db-list-box
  (new list-box%
       (parent left-v-panel)
       (label #f)
       (choices '())
       (horiz-margin 0)
       (vert-margin 0)
       (min-width 150)
       (min-height 400)
       (stretchable-height #f)
       (callback
        (lambda (list click-type)
          (populate-email-address-list-box)))))

;; Fills the email db list box (left pane)
(define (populate-email-db-list-box)
  (send email-db-list-box clear)
  (define (add-to-list-box db)
    (define db-name (email-db-name db))
    (send email-db-list-box append db-name db))
  (cond ((equal? (length email-db-list) 0)
         (send email-db-list-box append "There is no mailing list"))
        (else (map add-to-list-box email-db-list))))


;; Drop-down menu below the email db list box.
(define email-db-actions-choice
  (new choice%
       (parent left-v-panel)
       (label #f)
       (min-width (send email-db-list-box min-width))
       (horiz-margin 0)
       (vert-margin 0)       
       (choices (list "Mailing List Actions" 
                      "Add Mailing List..."
                      "Delete selected mailing list"))
       (callback
        (lambda (ch e)
          (define selection-index (send email-db-actions-choice get-selection))
          (cond ((equal? selection-index 0)
                 (void))
                ((equal? selection-index 1)  ; add a new db
                 (define name (get-text-from-user "Create new mailing list"
                                                  "Please enter a name:"
                                                  manage-mailing-list-dialog))
                 (when (not (equal? name #f))
                   (define new-db (make-email-db (generate-email-db-id) name '()))
                   (add-to-email-db-list new-db)
                   (populate-email-db-list-box))
                 (send email-db-actions-choice set-selection 0))
                ((equal? selection-index 2)  ; delete selected db
                 (define db-list-indexes (send email-db-list-box get-selections))
                 (when (not (null? db-list-indexes))
                   (define selection (car db-list-indexes))
                   (define db (send email-db-list-box get-data selection))
                   (remove-from-email-db-list (email-db-id db))
                   (populate-email-db-list-box))
                 (send email-db-actions-choice set-selection 0))
                (else
                 (printf "Mailing List Action: Unknown Action~n")
                 (send email-db-actions-choice set-selection 0)))))))

;; box enclosing the selected email-db name
(define selected-email-db-title-box
  (new horizontal-pane%
       (parent right-v-panel)
       (min-width 450)
       (min-height 30)
       (stretchable-height #f)
       (alignment '(left center))))

(define selected-email-db-title-label
  (new message%
       (parent selected-email-db-title-box)
       (label "")
       (min-width 200)
       (auto-resize #t)))

(define test-email-button-pane
  (new horizontal-pane%
       (parent selected-email-db-title-box)
       (alignment '(right center))))

;; Button - send test email to selected email-db
(define test-email-button
  (new button%
       (parent test-email-button-pane)
       (label "Send Test Email")
       (callback
        (lambda (b e)
          (define selected-db-index (send email-db-list-box get-selection))
          (when (not (eq? selected-db-index #f))
            (define db (send email-db-list-box get-data selected-db-index))
            (send-text "Team Racket Science" "Test Email" "This is a test email sent from Manage-Email-DB UI" (email-db-addresses db)))))))

;; Button - configures smtp log-in information for sending emails.
(define smtp-config-button
  (new button%
       (parent test-email-button-pane)
       (label "SMTP Config...")
       (callback
        (lambda (b e)
          (open-smtp-config)))))

;; pane below the selected email-db title box
(define right-email-db-pane
  (new pane%
       (parent right-v-panel)
       (alignment '(left top))))

(define NAME-COLUMN 0)
(define EMAIL-COLUMN 1)

(define email-db-horizontal-pane
  (new horizontal-pane%
       (parent right-email-db-pane)
       (alignment '(left top))))

;; list box containing all persons in the currently selected email-db.
(define email-address-list-box
  (new list-box%
       (parent email-db-horizontal-pane)
       (label #f)
       (choices '())
       (horiz-margin 0)
       (vert-margin 0)
       (min-width 300)
       (min-height 350)
       (columns '("Name" "E-mail"))
       (style '(single vertical-label column-headers)))) 
(send email-address-list-box set-column-width NAME-COLUMN 160 30 300)   ; width, min-width, max-width
(send email-address-list-box set-column-width EMAIL-COLUMN 200 30 300)

;; fills the list box on the right pane with names and email addresses
(define (populate-email-address-list-box)
  (send email-address-list-box clear)
  (when (not (null? (send email-db-list-box get-selections)))
    (define selection (car (send email-db-list-box get-selections)))
    (define db (send email-db-list-box get-data selection))
    (when (not (equal? db #f))
      (send selected-email-db-title-label set-label (email-db-name db))
      (define db-entries (email-db-entries db))
      (define (add-to-list db-entry)
        (define name (email-db-entry-name db-entry))
        (define address (email-db-entry-email-address db-entry))
        (send email-address-list-box append name db-entry)
        (define last (- (send email-address-list-box get-number) 1))
        (send email-address-list-box set-string last address EMAIL-COLUMN))
      (map add-to-list db-entries))))

;; contains Edit, Add, Delete buttons.
(define email-db-buttons-vertical-pane
  (new vertical-pane%
       (parent email-db-horizontal-pane)
       (stretchable-width #f)
       (alignment '(left top))))

;; Edit button - modifying the selected email-db-entry.
(define email-db-edit-button
  (new button%
       (parent email-db-buttons-vertical-pane)
       (label "Edit...")
       (callback 
        (lambda (button event)
          (define selection-index (send email-address-list-box get-selection))
          (when (not (equal? selection-index #f))
            (define old-entry (send email-address-list-box get-data selection-index))
            (when (not (equal? old-entry #f))
              (define new-entry (open-email-db-entry-edit button event old-entry))
              (when (not (null? new-entry))
                (define db-selection (car (send email-db-list-box get-selections)))
                (define db (send email-db-list-box get-data db-selection))
                (replace-db-entry db old-entry new-entry)
                (populate-email-address-list-box))))))))

;; Add button - adding an entry to the selected email-db.
(define email-db-add-button
  (new button%
       (parent email-db-buttons-vertical-pane)
       (label "Add...")
       (callback
        (lambda (button event)
          (define selection (send email-db-list-box get-selections))
          (when (not (null? selection))
            (define db-selection (car selection))
            (define db (send email-db-list-box get-data db-selection))
            (define new-entry (open-email-db-entry-edit button event))
            (when (not (null? new-entry))
              (add-email-db-entry db new-entry)
              (populate-email-address-list-box)))))))

;; Delete button - deleting an entry from the selected email-db.
(define email-db-delete-button
  (new button%
       (parent email-db-buttons-vertical-pane)
       (label "Delete")
       (callback
        ;; what happens when Delete button is clicked
        (lambda (button event) 
          (define selection (send email-db-list-box get-selections))
          (when (not (null? selection))
            (define db-selection (car selection))
            (define db (send email-db-list-box get-data db-selection))
            (define entry-selections (send email-address-list-box get-selections))
            (define (remove-entry-by-index index)
              (define entry (send email-address-list-box get-data index))
              (remove-email-db-entry db entry))
            (map remove-entry-by-index entry-selections)
            (populate-email-address-list-box))))))

;; Contains Ok and Cancel buttons.
(define bottom-buttons-pane
  (new horizontal-pane%
       (parent right-v-panel)
       (spacing 10)
       (alignment '(center top))))

;; Cancel button
(define cancel-button
  (new button% 
       (parent bottom-buttons-pane) 
       (label "Cancel")
       (min-width 75)
       (callback (λ (button event)
                   (send manage-mailing-list-dialog show #f)))))

;; Ok button - returns a list of the email addresses for the selected email-db.
(define ok-button
  (new button% 
       (parent bottom-buttons-pane) 
       (label "Ok")
       (min-width 75)
       (callback (λ (button event)
                   (define selection (send email-db-list-box get-selections))
                   (when (not (null? selection))
                     (define db-selection (car selection))
                     (define db (send email-db-list-box get-data db-selection))
                     (set! return-db db)
                     (set! return-list (map email-db-entry-email-address (email-db-entries db))))
                   (send manage-mailing-list-dialog show #f)))))

(when (system-position-ok-before-cancel?)
  (send bottom-buttons-pane change-children reverse))


#||
 | Opens the main dialog. Upon exit from the dialog, this procedure can return
 | a list of email addresses for the selected mailing list, or the raw
 | email-db object based on the optional 'command' argument.
 | 
 | Example:
 | > (open-manage-mailing-list-dialog 'return-addresses)
 | > (open-manage-mailing-list-dialog 'return-db)
 | > (open-manage-mailing-list-dialog)
 |
 |@param command 'return-addresses
 |       Returns the list of email addresses for the chosen mailing list.
 |       command 'return-db
 |       Returns the raw email-db object.
 |#
(define (open-manage-mailing-list-dialog (command #f))
  (populate-email-db-list-box)
  (populate-email-address-list-box)
  (if (or (eq? command 'return-addresses)
          (eq? command 'return-db))
      (begin
        (send manage-mailing-list-dialog set-label "Select a mailing list")
        (send ok-button show #t)
        (send cancel-button show #t))
      (begin
        (send manage-mailing-list-dialog set-label "Manage Mailing List")
        (send ok-button show #f)
        (send cancel-button show #f)))
  (set! return-list '())
  (set! return-db #f)
  (send selected-email-db-title-label set-label
        (cond ((= 0 (send email-db-list-box get-number)) "There is no mailing list configured")
              ((null? (send email-db-list-box get-selections)) "No mailing list selected ")
              (else "")))
  (send manage-mailing-list-dialog center)
  (send manage-mailing-list-dialog show #t)
  ;; Blocking until Cancel or Ok button is clicked.
  (cond ((eq? command 'return-addresses)
         return-list)
        ((eq? command 'return-db)
         return-db)
        (else (void))))
