#||
 | scheduler_ui.rkt
 | author: Yong Cho (Yong_Cho@student.uml.edu)
 | Created on: 4/8/2015
 |
 | This file implements user interface to schedule and manage automated
 | tests.
 |#

#lang racket/gui

(require racket/date
         racket/flonum
         "autotest.rkt"
         "calendar.rkt"
         "input-validation.rkt"
         "scheduler.rkt"         
         "../QA-Email/email-db.rkt"
         "../QA-Email/email-db-ui.rkt")

(provide open-manage-schedule)


(define NO-ACTIVE-AUTOTEST-MESSAGE "No active autotest")
(define NO-INACTIVE-AUTOTEST-MESSAGE "No inactive autotest")
(define NO-AUTOTEST-SELECTED-MESSAGE "** No autotest selected **  ")
(define NO-MAILING-LIST-MESSAGE "No mailing list selected  ")
(define VALID-BG-COLOR "White")
(define INVALID-BG-COLOR "Red")

(define selected-email-db #f)
(define APPROXIMATE-SIZE-X 700)
(define APPROXIMATE-SIZE-Y 550)

(define-values (size-x size-y) (get-display-size))
(define x-pos (fl->exact-integer (* (- size-x APPROXIMATE-SIZE-X) 0.5)))
(define y-pos (fl->exact-integer (* (- size-y APPROXIMATE-SIZE-Y) 0.5)))

(define manage-scheduler-frame  
  (new frame%
       (label "Manage Automated Tests")
       (x x-pos)
       (y y-pos)
       (spacing 2)))

(define main-h-panel
  (new horizontal-panel%
       (parent manage-scheduler-frame)
       (spacing 10)
       (border 8)
       (alignment '(left top))))

(define left-v-panel
  (new vertical-panel%
       (parent main-h-panel)
       (spacing 6)
       (horiz-margin 3)
       (alignment '(center top))))

(define right-v-panel
  (new vertical-panel%
       (parent main-h-panel)  
       (spacing 4)
       (min-width 400)
       (alignment '(left top))))

;; List box for active tests (left pane).
(define active-tests-list-box
  (new list-box%
       (parent left-v-panel)
       (label "Active Tests")
       (choices '())
       (horiz-margin 0)
       (vert-margin 0)
       (min-width 170)
       (min-height 200)
       (style '(single vertical-label))
       (callback
        (lambda (list click-type)
          (define selection-in-inactive-tests-list-box (send inactive-tests-list-box get-selection))
          (when (not (equal? #f selection-in-inactive-tests-list-box))
            (send inactive-tests-list-box select selection-in-inactive-tests-list-box #f))
          (setup-ui)))))

;; Fills the active autotest list box (left pane).
(define (populate-active-tests-list-box)
  (define active-test-list (filter autotest-active? autotest-list))  
  (define (add-to-list-box test)
    (define name (autotest-name test))
    (send active-tests-list-box append name test))  
  (send active-tests-list-box clear)
  (cond ((equal? (length active-test-list) 0)
         (send active-tests-list-box append NO-ACTIVE-AUTOTEST-MESSAGE))
        (else (map add-to-list-box active-test-list))))

;; List box for inactive tests (left pane).
(define inactive-tests-list-box
  (new list-box%
       (parent left-v-panel)
       (label "Inactive Tests")
       (choices '())
       (horiz-margin 0)
       (vert-margin 0)
       (min-width 170)
       (min-height 220)
       (style '(single vertical-label))
       (callback
        (lambda (list click-type)
          (define selection-in-active-tests-list-box (send active-tests-list-box get-selection))
          (when (not (equal? #f selection-in-active-tests-list-box))
            (send active-tests-list-box select selection-in-active-tests-list-box #f))
          (setup-ui)))))

;; Fills the inactive autotest list box (left pane)
(define (populate-inactive-tests-list-box)
  (define inactive-test-list (filter (negate autotest-active?) autotest-list))
  (define (add-to-list-box test)
    (define name (autotest-name test))
    (send inactive-tests-list-box append name test))
  (send inactive-tests-list-box clear)
  (cond ((equal? (length inactive-test-list) 0)
         (send inactive-tests-list-box append NO-INACTIVE-AUTOTEST-MESSAGE))
        (else (map add-to-list-box inactive-test-list))))

(define autotest-actions-choice
  (new choice%
       (parent left-v-panel)
       (label #f)
       (min-width (send active-tests-list-box min-width))
       (horiz-margin 0)
       (vert-margin 3)
       (choices (list "Autotest Actions"
                      "Activate this test"
                      "Deactivate this test"
                      "Delete this test"))
       (callback
        (lambda (ch e)
          (let ((selected-index (send autotest-actions-choice get-selection)))
            (cond ((equal? selected-index 0)
                   (void))
                  ((equal? selected-index 1)  ; activate
                   (let ((inactive-selection (send inactive-tests-list-box get-selection)))
                     (when (and (not (equal? #f inactive-selection))
                                (not (equal? NO-INACTIVE-AUTOTEST-MESSAGE 
                                             (send inactive-tests-list-box get-string-selection))))
                       (let ((at (send inactive-tests-list-box get-data inactive-selection)))
                         (autotest-set-active? at #t)
                         (write-autotest-list)
                         (write-autotest at)))))
                  ((equal? selected-index 2)  ; deactivate
                   (let ((active-selection (send active-tests-list-box get-selection)))
                     (when (and (not (equal? #f active-selection))
                                (not (equal? NO-ACTIVE-AUTOTEST-MESSAGE 
                                             (send active-tests-list-box get-string-selection))))
                       (let ((at (send active-tests-list-box get-data active-selection)))
                         (autotest-set-active? at #f)
                         (write-autotest-list)
                         (write-autotest at)))))
                  ((equal? selected-index 3)  ; delete
                   (let ((active-selection (send active-tests-list-box get-selection))
                         (inactive-selection (send inactive-tests-list-box get-selection)))
                     (cond ((and (not (equal? #f active-selection))
                                 (not (equal? NO-ACTIVE-AUTOTEST-MESSAGE 
                                              (send active-tests-list-box get-string-selection))))
                            (let ((at (send active-tests-list-box get-data active-selection)))
                              (remove-from-autotest-list (autotest-id at))))
                           ((and (not (equal? #f inactive-selection))
                                 (not (equal? NO-INACTIVE-AUTOTEST-MESSAGE
                                              (send inactive-tests-list-box get-string-selection))))
                            (let ((at (send inactive-tests-list-box get-data inactive-selection)))
                              (remove-from-autotest-list (autotest-id at))))
                           (else (void)))))
                  (else
                   (printf "Autotest Action: Unknown Action~n")))
            (populate-active-tests-list-box)
            (populate-inactive-tests-list-box)
            (send autotest-actions-choice set-selection 0)
            (setup-ui))))))

(define top-right-h-pane
  (new horizontal-pane%
       (parent right-v-panel)))

;; box enclosing the selected autotest name (top right)
(define selected-test-name-box
  (new pane%
       (parent top-right-h-pane)
       (min-height 30)
       (min-width 300)
       (stretchable-width #f)
       (stretchable-height #f)
       (alignment '(left center))))

(define selected-autotest-message
  (new message%
       (parent selected-test-name-box)
       (horiz-margin 12)
       (label NO-AUTOTEST-SELECTED-MESSAGE)
       (auto-resize #t)))

(define autotest-name-text-field
  (new text-field%
       (parent selected-test-name-box)
       (label "Autotest Name ")
       (min-width 280)
       (horiz-margin 3)
       (callback
        (lambda (tf e)
          (set-autotest-name-text-field-background)))))

(define schedule-new-button-pane
  (new pane%
       (parent top-right-h-pane)
       (alignment '(right center))))

(define schedule-new-button
  (new button%
       (parent schedule-new-button-pane)
       (label "Create A New Autotest")
       (callback
        (lambda (b e)
          (config-ui-create-mode)))))

;; files list box
(define test-resource-panel
  (new group-box-panel%
       (parent right-v-panel)
       (label "Files to execute")
       (border 5)
       (spacing 5)
       (alignment '(left top))))

(define files-list-and-buttons-pane 
  (new horizontal-pane%
       (parent test-resource-panel)
       (alignment '(left top))))

(define files-list-box
  (new list-box%
       (parent files-list-and-buttons-pane)
       (label #f)
       (choices '())
       (min-width 400)
       (min-height 150)
       (style '(multiple))))

(define right-buttons-pane
  (new vertical-pane%
       (parent files-list-and-buttons-pane)
       (alignment '(left top))))

;; "Add file" button
(new button%
     (parent right-buttons-pane)
     (label "Add...")
     (callback 
      (lambda (b e)
        (let ((filepaths (get-file-list "Select test files" manage-scheduler-frame)))
          (when (not (eq? filepaths #f))                   
            (define (add-to-file-list-box path)
              (send files-list-box append (path->string path)))
            (map add-to-file-list-box filepaths))))))

;; Button to remove files
(new button%
     (parent right-buttons-pane)
     (label "Remove")
     (callback
      (lambda (b e)
        (let ((selected-indexes (sort (send files-list-box get-selections) >))
              (delete-n (lambda (n) (send files-list-box delete n))))
          (when (not (null? selected-indexes))
            (for-each delete-n selected-indexes))))))

;; area below files list box - configures time schedule
(define schedule-panel
  (new group-box-panel%
       (parent right-v-panel)
       (label "When to execute")
       (border 5)
       (spacing 5)
       (alignment '(left top))))

(define time-of-day-pane
  (new horizontal-panel%
       (parent schedule-panel)
       (spacing 12)
       (vert-margin 5)
       (alignment '(left top))))

(define hour-combo-field
  (new combo-field%
       (parent time-of-day-pane)
       (label "Hour")
       (min-width 85)
       (stretchable-width #f)
       (choices (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"))
       (init-value "1")
       (callback
        (lambda (c e)
          (set-hour-combo-field-background)))))

(define minute-text-field
  (new text-field%
       (parent time-of-day-pane)
       (label "Minute")
       (init-value "0")
       (min-width 85)
       (stretchable-width #f)
       (callback
        (lambda (t e)
          (set-minute-text-field-background)))))

(define ampm-combo-field
  (new combo-field%
       (parent time-of-day-pane)
       (label #f)
       (min-width 45)
       (stretchable-width #f)
       (choices (list "AM" "PM"))
       (init-value "AM")
       (callback
        (lambda (c e)
          (set-ampm-combo-field-background)))))

;; Some UIs need to be disabled depending on whether one-time or periodic
;; radio button is selected.
(define (toggle-onetime-periodic)
  (define selected-index (send one-time-periodic-radio-box get-selection)) 
  (if (eq? selected-index 0)  ; one-time
      (begin (disable-all-children days-of-week-pane)
             (enable-all-children date-picker-pane))
      (begin (send date-text-field set-field-background (make-object color% VALID-BG-COLOR))
             (disable-all-children date-picker-pane)
             (enable-all-children days-of-week-pane))))

(define one-time-periodic-radio-box
  (new radio-box%
       (parent schedule-panel)
       (label #f)
       (style '(horizontal))
       (choices '("One-Time" "Periodic"))
       (selection 0)
       (callback
        (lambda (r e)
          (toggle-onetime-periodic)))))

(define one-time-group-box
  (new group-box-panel%
       (parent schedule-panel)
       (border 5)
       (label "One-Time")))

(define date-picker-pane
  (new horizontal-pane%
       (parent one-time-group-box)
       (spacing 5)
       (alignment '(left top))))

(define year-combo-field
  (new combo-field%
       (parent date-picker-pane)
       (label "Year ")
       (min-width 88)
       (stretchable-width #f)
       (horiz-margin 2)
       (choices year-list)
       (init-value (number->string (current-year)))
       (callback
        (lambda (c e)
          (set-date-text-field-background)
          (set-month-combo-field-background)
          (set-year-combo-field-background)))))

(define month-combo-field
  (new combo-field%
       (parent date-picker-pane)
       (label "Month ")
       (min-width 85)
       (stretchable-width #f)
       (choices month-list)
       (init-value (number->string (current-month)))
       (callback
        (lambda (c e)
          (set-date-text-field-background)
          (set-month-combo-field-background)
          (set-year-combo-field-background)))))

(define date-text-field
  (new text-field%
       (parent date-picker-pane)
       (label "Date ")
       (min-width 65)
       (stretchable-width #f)
       (init-value (number->string (current-day)))
       (callback
        (lambda (t e)
          (set-date-text-field-background)
          (set-month-combo-field-background)
          (set-year-combo-field-background)))))

(define periodic-group-box
  (new group-box-panel%
       (parent schedule-panel)
       (label "Periodic")
       (border 5)))

(define days-of-week-pane
  (new horizontal-pane%
       (parent periodic-group-box)
       (spacing 10)
       (alignment '(left top))))

(define (toggle-check-daily check-box)
  (if (and (send check-monday get-value)
           (send check-tuesday get-value)
           (send check-wednesday get-value)
           (send check-thursday get-value)
           (send check-friday get-value)
           (send check-saturday get-value)
           (send check-sunday get-value))
      (send check-daily set-value #t)
      (send check-daily set-value #f)))

(define (check-all-days)
  (send check-monday set-value #t)
  (send check-tuesday set-value #t)
  (send check-wednesday set-value #t)
  (send check-thursday set-value #t)
  (send check-friday set-value #t)
  (send check-saturday set-value #t)
  (send check-sunday set-value #t))
(define (uncheck-all-days)
  (send check-monday set-value #f)
  (send check-tuesday set-value #f)
  (send check-wednesday set-value #f)
  (send check-thursday set-value #f)
  (send check-friday set-value #f)
  (send check-saturday set-value #f)
  (send check-sunday set-value #f))

(define check-daily
  (new check-box%
       (parent days-of-week-pane)
       (label "Daily")
       (value #f)
       (callback
        (lambda (c e)
          (if (send check-daily get-value)
              (check-all-days)
              (uncheck-all-days))))))
(new message%
     (parent days-of-week-pane)
     (label "")
     (min-width 10)) 
(define check-monday
  (new check-box%
       (parent days-of-week-pane)
       (label "Mon")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)))))
(define check-tuesday
  (new check-box%
       (parent days-of-week-pane)
       (label "Tue")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)))))
(define check-wednesday
  (new check-box%
       (parent days-of-week-pane)
       (label "Wed")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)))))
(define check-thursday
  (new check-box%
       (parent days-of-week-pane)
       (label "Thu")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)))))
(define check-friday
  (new check-box%
       (parent days-of-week-pane)
       (label "Fri")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)))))
(define check-saturday
  (new check-box%
       (parent days-of-week-pane)
       (label "Sat")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)))))
(define check-sunday
  (new check-box%
       (parent days-of-week-pane)
       (label "Sun")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)))))

;; UI for configuring email notification
(define email-panel
  (new group-box-panel%
       (parent right-v-panel)
       (label "Notify results via email")
       (border 2)
       (min-height 60)  ; see if this can go later
       (alignment '(left top))))

(define email-v-pane
  (new vertical-pane%
       (parent email-panel)
       (spacing 2)
       (alignment '(left center))))

;; TODO: Check if mailing list is selected when Notify? is checked.
(define check-notify
  (new check-box%
       (parent email-v-pane)
       (label "Notify?")
       (horiz-margin 10)
       (value #f)
       (callback 
        (lambda (c e)
          (define checked? (send check-notify get-value))
          (void)))))

(define email-h-pane
  (new horizontal-pane%
       (parent email-v-pane)
       (spacing 2)
       (alignment '(left center))))

(define email-db-name-h-pane
  (new horizontal-pane%
       (parent email-h-pane)
       (alignment '(left center))))

(new message%
     (parent email-db-name-h-pane)
     (label "To:  ")
     (horiz-margin 10))

(define mailing-list-name-message
  (new message%
       (parent email-db-name-h-pane)
       (label "")
       (auto-resize #t)))

(define email-db-button-h-pane
  (new horizontal-pane%
       (parent email-h-pane)
       (alignment '(right center))))

;; Button - Select mailing list
(define select-email-db-button
  (new button%
       (parent email-db-button-h-pane)
       (label "Select mailing list...")
       (callback 
        (lambda (b e)
          (let ((email-db (open-manage-mailing-list-dialog 'return-db)))
            (when (not (equal? #f email-db))
              (set! selected-email-db email-db)
              (send mailing-list-name-message set-label (email-db-name selected-email-db))))))))

;; Bottom buttons row
(define activate-buttons-pane
  (new horizontal-pane%
       (parent right-v-panel)
       (vert-margin 3)
       (alignment '(center top))))

(define save-changes-button
  (new button%
       (parent activate-buttons-pane)
       (label "Save Changes")
       (callback
        (lambda (b e)
          (let ((active-selection (send active-tests-list-box get-selection))
                (inactive-selection (send inactive-tests-list-box get-selection)))
            (cond ((and (not (equal? #f active-selection))
                        (not (equal? NO-ACTIVE-AUTOTEST-MESSAGE 
                                     (send active-tests-list-box get-string-selection))))
                   (let ((at (send active-tests-list-box get-data active-selection)))
                     (when (entries-are-valid?)
                       (copy-autotest! (make-autotest-from-ui (autotest-active? at)) at))))
                  ((and (not (equal? #f inactive-selection))
                        (not (equal? NO-INACTIVE-AUTOTEST-MESSAGE
                                     (send inactive-tests-list-box get-string-selection))))
                   (let ((at (send inactive-tests-list-box get-data inactive-selection)))
                     (when (entries-are-valid?)
                       (copy-autotest! (make-autotest-from-ui (autotest-active? at)) at))))
                  (else (void))))))))

;; 'Create and Activate' Button 
;; - Creates a new test and activates it at the same time
(define create-active-button
  (new button%
       (parent activate-buttons-pane)
       (label "Create and Activate")
       (callback
        (lambda (b e)
          (when (entries-are-valid?)
            (add-to-autotest-list (make-autotest-from-ui #t))
            (populate-active-tests-list-box)
            (setup-ui))))))

;; 'Create as Inactive' Button
;; - Creates a new autotest schedule but does not activate it
(define create-inactive-button
  (new button%
       (parent activate-buttons-pane)
       (label "Create as Inactive")
       (callback
        (lambda (b e)
          (when (entries-are-valid?)
            (add-to-autotest-list (make-autotest-from-ui #f))
            (populate-inactive-tests-list-box)
            (setup-ui))))))

;; Cancel Button - Cancels creating a new autotest schedule
(define cancel-button
  (new button%
       (parent activate-buttons-pane)
       (label "Cancel")
       (callback
        (lambda (b e) (setup-ui)))))

;; Creates a new autotest.
(define (make-autotest-from-ui (activate? #t))
  (when (entries-are-valid?)
    (let ((id (generate-autotest-id))
          (num-files (send files-list-box get-number))
          (files '()))
      (for ((i num-files))
        (set! files (append files (list (send files-list-box get-string i)))))
      (make-autotest id
                     (send autotest-name-text-field get-value)
                     activate?
                     files
                     (if (equal? (send one-time-periodic-radio-box get-selection) 0)
                         'one-time
                         'periodic)
                     (string->number (send year-combo-field get-value))
                     (string->number (send month-combo-field get-value))
                     (string->number (send date-text-field get-value))
                     (send check-daily get-value)
                     (send check-monday get-value)
                     (send check-tuesday get-value)
                     (send check-wednesday get-value)
                     (send check-thursday get-value)
                     (send check-friday get-value)
                     (send check-saturday get-value)
                     (send check-sunday get-value)
                     (string->number (send hour-combo-field get-value))
                     (string->number (send minute-text-field get-value))
                     (send ampm-combo-field get-value)
                     (if (not selected-email-db)
                         #f
                         (send check-notify get-value))
                     selected-email-db))))


;; Fills the configuration pane (right side) for the selected autotest.
(define (populate-autotest-info lb)
  (cond ((eq? lb 'default)
         (send files-list-box clear)
         (send one-time-periodic-radio-box set-selection 0)
         (send year-combo-field set-value (number->string (date-year (current-date))))
         (send month-combo-field set-value (number->string (date-month (current-date))))
         (send date-text-field set-value (number->string (date-day (current-date))))
         (send check-daily set-value #f)
         (send check-monday set-value #f)
         (send check-tuesday set-value #f)
         (send check-wednesday set-value #f)
         (send check-thursday set-value #f)
         (send check-friday set-value #f)
         (send check-saturday set-value #f)
         (send check-sunday set-value #f)
         (toggle-onetime-periodic)
         (send hour-combo-field set-value "1")
         (send minute-text-field set-value "0")
         (send ampm-combo-field set-value "AM")
         (send check-notify set-value #f)
         (send mailing-list-name-message set-label NO-MAILING-LIST-MESSAGE))        
        (else
         (define selection-index (send lb get-selection))
         (cond ((equal? #f selection-index)
                (send selected-autotest-message set-label NO-AUTOTEST-SELECTED-MESSAGE))
               (else
                (let ((selected-at (send lb get-data selection-index)))
                  (send autotest-name-text-field set-value (autotest-name selected-at))
                  (set-autotest-name-text-field-background)
                  (send files-list-box clear)
                  (for-each (lambda (file) (send files-list-box append file)) (autotest-files selected-at))
                  (if (eq? 'one-time (autotest-type selected-at))
                      (send one-time-periodic-radio-box set-selection 0)
                      (send one-time-periodic-radio-box set-selection 1))
                  (send year-combo-field set-value (number->string (autotest-year selected-at)))
                  (send month-combo-field set-value (number->string (autotest-month selected-at)))
                  (send date-text-field set-value (number->string (autotest-date selected-at)))
                  (send check-daily set-value (autotest-daily? selected-at))
                  (send check-monday set-value (autotest-mon? selected-at))
                  (send check-tuesday set-value (autotest-tue? selected-at))
                  (send check-wednesday set-value (autotest-wed? selected-at))
                  (send check-thursday set-value (autotest-thu? selected-at))
                  (send check-friday set-value (autotest-fri? selected-at))
                  (send check-saturday set-value (autotest-sat? selected-at))
                  (send check-sunday set-value (autotest-sun? selected-at))
                  (toggle-onetime-periodic)
                  (send hour-combo-field set-value (number->string (autotest-hour selected-at)))
                  (send minute-text-field set-value (number->string (autotest-minute selected-at)))
                  (send ampm-combo-field set-value (autotest-ampm selected-at))
                  (send check-notify set-value (autotest-notify? selected-at))
                  (if (not (equal? #f selected-email-db))
                      (send mailing-list-name-message set-label (email-db-name selected-email-db))
                      (send mailing-list-name-message set-label NO-MAILING-LIST-MESSAGE))))))))

;; UI setting for normal browsing (when an entry is selected in autotest lists)
(define (config-ui-normal-mode)
  (define at
    (cond ((equal? #f (send active-tests-list-box get-selection))
           (send inactive-tests-list-box get-data (send inactive-tests-list-box get-selection)))
          (else
           (send active-tests-list-box get-data (send active-tests-list-box get-selection)))))
  (send autotest-name-text-field set-value (autotest-name at))
  (send schedule-new-button enable #t)
  (enable-all-children left-v-panel)
  (send selected-test-name-box change-children name-field-normal-mode)
  (send files-list-box enable #t)
  (enable-all-children right-buttons-pane)
  (send one-time-periodic-radio-box enable #t)
  (toggle-onetime-periodic)
  (enable-all-children time-of-day-pane)
  (enable-email-db-controls)
  (send activate-buttons-pane change-children buttons-normal-mode)
  (enable-all-children activate-buttons-pane)
  (set! selected-email-db (autotest-email-db at))
  (define selection-in-active (send active-tests-list-box get-selection))
  (cond ((not (equal? #f selection-in-active))
         (populate-autotest-info active-tests-list-box))
        (else
         (populate-autotest-info inactive-tests-list-box))))

;; UI setting when no autotest is selected
(define (config-ui-no-selection-mode)
  (enable-all-children left-v-panel)
  (populate-autotest-info 'default)
  (send selected-test-name-box change-children name-field-no-selection-mode)
  (send selected-autotest-message set-label NO-AUTOTEST-SELECTED-MESSAGE)
  (send schedule-new-button enable #t)
  (send files-list-box enable #t)
  (disable-all-children right-buttons-pane)
  (send one-time-periodic-radio-box enable #f)
  (send date-text-field set-field-background (make-object color% VALID-BG-COLOR))
  (disable-all-children date-picker-pane)  
  (disable-all-children days-of-week-pane)
  (send minute-text-field set-field-background (make-object color% VALID-BG-COLOR))
  (disable-all-children time-of-day-pane)  
  (disable-email-db-controls)
  (send activate-buttons-pane change-children buttons-empty-mode)
  (disable-all-children activate-buttons-pane)
  (set! selected-email-db #f))

;; UI setting when no autotest schedule exists
(define (config-ui-empty-mode)
  (disable-all-children left-v-panel)
  (send selected-test-name-box change-children name-field-empty-mode)
  (send selected-autotest-message set-label "No autotest scheduled")
  (send schedule-new-button enable #t)
  (send files-list-box enable #f)
  (disable-all-children right-buttons-pane)
  (send one-time-periodic-radio-box enable #f)
  (send date-text-field set-field-background (make-object color% VALID-BG-COLOR))
  (disable-all-children date-picker-pane)  
  (disable-all-children days-of-week-pane)
  (send minute-text-field set-field-background (make-object color% VALID-BG-COLOR))
  (disable-all-children time-of-day-pane)  
  (disable-email-db-controls)
  (send activate-buttons-pane change-children buttons-empty-mode)
  (disable-all-children activate-buttons-pane)
  (set! selected-email-db #f))

;; UI setting when creating a new autotest schedule
(define (config-ui-create-mode)
  (define selection-in-active-tests-list-box (send active-tests-list-box get-selection))
  (when (not (equal? #f selection-in-active-tests-list-box))
    (send active-tests-list-box select selection-in-active-tests-list-box #f))
  (define selection-in-inactive-tests-list-box (send inactive-tests-list-box get-selection))
  (when (not (equal? #f selection-in-inactive-tests-list-box))
    (send inactive-tests-list-box select selection-in-inactive-tests-list-box #f))
  (disable-all-children left-v-panel)
  (populate-autotest-info 'default)
  (send selected-test-name-box change-children name-field-create-mode)
  (send autotest-name-text-field set-value "")
  (set-autotest-name-text-field-background)
  (send schedule-new-button enable #f)
  (send files-list-box enable #t)
  (enable-all-children right-buttons-pane)
  (send one-time-periodic-radio-box enable #t)
  (set-date-text-field-background)
  (set-minute-text-field-background)
  (toggle-onetime-periodic)
  (enable-all-children time-of-day-pane)
  (enable-email-db-controls)
  (send activate-buttons-pane change-children buttons-create-mode)
  (set! selected-email-db #f))

;; Renders an appropriate UI configuration based on user selection.
(define (setup-ui)
  (cond ((or (and (equal? 0 (send active-tests-list-box get-number))
                  (equal? 0 (send inactive-tests-list-box get-number)))
             (and (equal? NO-ACTIVE-AUTOTEST-MESSAGE (send active-tests-list-box get-string 0))
                  (equal? 1 (send active-tests-list-box get-number))
                  (equal? NO-INACTIVE-AUTOTEST-MESSAGE (send inactive-tests-list-box get-string 0))
                  (equal? 1 (send inactive-tests-list-box get-number))))
         (config-ui-empty-mode))
        ((or (and (equal? #f (send active-tests-list-box get-selection))
                  (equal? #f (send inactive-tests-list-box get-selection)))
             (and (equal? NO-ACTIVE-AUTOTEST-MESSAGE (send active-tests-list-box get-string-selection))
                  (equal? #f (send inactive-tests-list-box get-selection)))
             (and (equal? NO-INACTIVE-AUTOTEST-MESSAGE (send inactive-tests-list-box get-string-selection))
                  (equal? #f (send active-tests-list-box get-selection))))
         (config-ui-no-selection-mode))
        (else (config-ui-normal-mode))))


;; **********************************************************************
;; * Enable/disable UI controls
;; **********************************************************************

;; helpers for change-children method for selected-test-name-box
(define (name-field-create-mode arg-not-used)
  (list autotest-name-text-field))
(define (name-field-normal-mode arg-not-used)
  (list autotest-name-text-field))
(define (name-field-empty-mode arg-not-used)
  (list selected-autotest-message))
(define (name-field-no-selection-mode arg-not-used)
  (list selected-autotest-message))

;; helpers for change-children method for activate-buttons-pane
(define (buttons-create-mode arg-not-used)
  (list create-active-button create-inactive-button cancel-button))
(define (buttons-normal-mode arg-not-used)
  (list save-changes-button))
(define buttons-empty-mode buttons-normal-mode)

(define (disable-all-children area-container)
  (for-each send-disable (send area-container get-children)))

(define (enable-all-children area-container)
  (for-each send-enable (send area-container get-children)))

(define (delete-all-children area-container)
  (define (send-delete-child child)
    (send area-container delete-child child))
  (for-each send-delete-child (send area-container get-children)))

(define (enable-email-db-controls)
  (send check-notify enable #t)
  (send select-email-db-button enable #t))

(define (disable-email-db-controls)
  (send check-notify enable #f)
  (send select-email-db-button enable #f))

;; helpers for enable-all-children and disable-all-children
(define (send-enable control) (send control enable #t))
(define (send-disable control) (send control enable #f))

;; show different background color for invalid entries
(define (set-autotest-name-text-field-background)
  (if (valid-autotest-name-string? (send autotest-name-text-field get-value))
      (send autotest-name-text-field set-field-background (make-object color% VALID-BG-COLOR))
      (send autotest-name-text-field set-field-background (make-object color% INVALID-BG-COLOR))))

(define (set-minute-text-field-background)
  (if (valid-minute-string? (send minute-text-field get-value))
      (send minute-text-field set-field-background (make-object color% VALID-BG-COLOR))
      (send minute-text-field set-field-background (make-object color% INVALID-BG-COLOR))))

(define (set-hour-combo-field-background)
  (if (valid-hour-string? (send hour-combo-field get-value))
      (send hour-combo-field set-field-background (make-object color% VALID-BG-COLOR))
      (send hour-combo-field set-field-background (make-object color% INVALID-BG-COLOR))))

(define (set-ampm-combo-field-background)
  (if (valid-ampm-string? (send ampm-combo-field get-value))
      (send ampm-combo-field set-field-background (make-object color% VALID-BG-COLOR))
      (send ampm-combo-field set-field-background (make-object color% INVALID-BG-COLOR))))

(define (set-year-combo-field-background)
  (if (valid-year-string? (send year-combo-field get-value))
      (send year-combo-field set-field-background (make-object color% VALID-BG-COLOR))
      (send year-combo-field set-field-background (make-object color% INVALID-BG-COLOR))))

(define (set-month-combo-field-background)
  (if (valid-month-string? (send month-combo-field get-value))
      (send month-combo-field set-field-background (make-object color% VALID-BG-COLOR))
      (send month-combo-field set-field-background (make-object color% INVALID-BG-COLOR))))

(define (set-date-text-field-background)
  (define month (string->number (send month-combo-field get-value)))
  (define year (string->number (send year-combo-field get-value)))
  (if (valid-date-string? (send date-text-field get-value) month year)
      (send date-text-field set-field-background (make-object color% (make-object color% VALID-BG-COLOR)))
      (send date-text-field set-field-background (make-object color% (make-object color% INVALID-BG-COLOR)))))

;; At least one day of week must be checked for periodic test.
(define (valid-one-time-periodic-entries?)
  (not (and (equal? 1 (send one-time-periodic-radio-box get-selection))
            (equal? #f (send check-monday get-value))
            (equal? #f (send check-tuesday get-value))
            (equal? #f (send check-wednesday get-value))
            (equal? #f (send check-thursday get-value))
            (equal? #f (send check-friday get-value))
            (equal? #f (send check-saturday get-value))
            (equal? #f (send check-sunday get-value)))))

;; All user entries must be validated before creating an autotest.
(define (entries-are-valid?)
  (and (valid-autotest-name-string? (send autotest-name-text-field get-value))
       (valid-hour-string? (send hour-combo-field get-value))
       (valid-minute-string? (send minute-text-field get-value))
       (valid-ampm-string? (send ampm-combo-field get-value))
       (valid-year-string? (send year-combo-field get-value))
       (valid-month-string? (send month-combo-field get-value))
       (valid-date-string? (send date-text-field get-value)
                           (string->number (send month-combo-field get-value))
                           (string->number (send year-combo-field get-value)))
       (valid-one-time-periodic-entries?)
       (not (equal? 0 (send files-list-box get-number)))))


;; entry point
(define (open-manage-schedule)
  (populate-active-tests-list-box)
  (populate-inactive-tests-list-box)
  (setup-ui)
  (send manage-scheduler-frame show #t))

(open-manage-schedule)