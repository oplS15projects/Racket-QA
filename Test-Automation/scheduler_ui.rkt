#||
 | scheduler_ui.rkt
 | author: Yong Cho (Yong_Cho@student.uml.edu)
 | Created on: 4/8/2015
 |
 | This file implements user interface to schedule and manage automated
 | tests.
 |#

#lang racket

(require racket/date
         racket/gui/base
         "autotest.rkt"
         "bg-process.rkt"
         "calendar.rkt"
         "input-validation.rkt"
         "../QA-Email/email-db.rkt"
         "../QA-Email/email-db-ui.rkt")

(provide launch-scheduler)


(define NO-ACTIVE-AUTOTEST-MESSAGE "No active autotest")
(define NO-INACTIVE-AUTOTEST-MESSAGE "No inactive autotest")
(define NO-AUTOTEST-SELECTED-MESSAGE "** No autotest selected **  ")
(define NO-MAILING-LIST-MESSAGE "No mailing list selected  ")
(define NO-AUTOTEST-SCHEDULED-MESSAGE "No autotest scheduled  ")
(define VALID-FIELD-COLOR (make-object color% "White"))
(define INVALID-FIELD-COLOR (make-object color% "Red"))
(define ADD-FILE-ICON (read-bitmap "images/add-file-1.png"))
(define REMOVE-FILE-ICON (read-bitmap "images/remove-file-1.png"))
(define OK-ICON (read-bitmap "images/ok-1.png"))
(define WARNING-ICON (read-bitmap "images/warning-1.png"))

(define selected-email-db #f)

(define main-frame
  (new frame%
       (label "Test Scheduler")
       (spacing 2)))

(define main-h-panel
  (new horizontal-panel%
       (parent main-frame)
       (spacing 10)
       (border 8)
       (alignment '(left top))))

(define left-v-panel
  (new vertical-panel%
       (parent main-h-panel)
       (spacing 0)
       (horiz-margin 3)
       (min-width 260)
       (stretchable-width #f)
       (alignment '(left top))))

(define right-v-panel
  (new vertical-panel%
       (parent main-h-panel)
       (spacing 4)
       (alignment '(left top))))

;; **********************************************************************
;; * Left Side
;; **********************************************************************

;; List box containing active tests (top left).
(define active-tests-list-box
  (new list-box%
       (parent left-v-panel)
       (label "Active Tests")
       (choices '())
       (horiz-margin 0)
       (vert-margin 2)
       (min-height 180)
       (style '(single vertical-label))
       (callback
        (lambda (list click-type)
          (let ((selection-in-inactive-tests-list-box (send inactive-tests-list-box get-selection)))
            (when (not (equal? #f selection-in-inactive-tests-list-box))
              (send inactive-tests-list-box select selection-in-inactive-tests-list-box #f))
            (setup-ui))))))

;; Fills the active autotest list box.
(define (populate-active-tests-list-box)
  (let ((active-test-list (filter autotest-active? autotest-list))
        (add-to-list-box
         (lambda (test) (send active-tests-list-box append (autotest-name test) test))))
    (send active-tests-list-box clear)
    (cond ((null? active-test-list)
           (send active-tests-list-box append NO-ACTIVE-AUTOTEST-MESSAGE))
          (else (for-each add-to-list-box active-test-list)))))

;; List box for inactive tests (below the active tests list box).
(define inactive-tests-list-box
  (new list-box%
       (parent left-v-panel)
       (label "Inactive Tests")
       (choices '())
       (horiz-margin 0)
       (vert-margin 0)
       (min-height 200)
       (style '(single vertical-label))
       (callback
        (lambda (list click-type)
          (let ((selection-in-active-tests-list-box (send active-tests-list-box get-selection)))
            (when (not (equal? #f selection-in-active-tests-list-box))
              (send active-tests-list-box select selection-in-active-tests-list-box #f))
            (setup-ui))))))

;; Fills the inactive autotest list box.
(define (populate-inactive-tests-list-box)
  (let ((inactive-test-list (filter (negate autotest-active?) autotest-list))
        (add-to-list-box
         (lambda (test) (send inactive-tests-list-box append (autotest-name test) test))))
    (send inactive-tests-list-box clear)
    (cond ((null? inactive-test-list)
           (send inactive-tests-list-box append NO-INACTIVE-AUTOTEST-MESSAGE))
          (else (for-each add-to-list-box inactive-test-list))))) 

;; "Last Run:"
(define last-run-label
  (new message%
       (parent left-v-panel)
       (label "Last Run:  ")
       (vert-margin 0)
       (auto-resize #f)))

;; "Saturday, April 10th, 2015 4:01:00pm"
(define last-run-text
  (new message%
       (parent left-v-panel)
       (label "")
       (vert-margin 0)
       (min-width 220)
       (stretchable-width #f)
       (auto-resize #t)))

;; "Next Due:"
(define next-due-label
  (new message%
       (parent left-v-panel)
       (label "Next Due:  ")
       (stretchable-width #f)
       (vert-margin 0)
       (auto-resize #f)))

;; "Sunday, April 11th, 2015 4:01:00pm"
(define next-due-text
  (new message%
       (parent left-v-panel)
       (label "")
       (stretchable-width #f)
       (vert-margin 0)
       (min-width 220)
       (auto-resize #t)))

;; Updates last-run and next-due labels for the selected autotest.
(define (refresh-run-info)
  (let ((active-selection (send active-tests-list-box get-selection))
        (inactive-selection (send inactive-tests-list-box get-selection)))
    (define at (cond (active-selection (send active-tests-list-box get-data active-selection))
                     (inactive-selection (send inactive-tests-list-box get-data inactive-selection))
                     (else #f)))
    (when at
      (send last-run-text set-label (autotest-last-run-string at))
      (send next-due-text set-label (autotest-next-due-string at)))))

;; drop-down menu on bottom left
(define autotest-actions-choice
  (new choice%
       (parent left-v-panel)
       (label #f)
       (min-width 200)
       (horiz-margin 0)
       (vert-margin 3)
       (choices (list "Autotest Actions"
                      "Activate this test"
                      "Deactivate this test"
                      "Duplicate this test"
                      "Delete this test"))
       (callback
        (lambda (ch e)
          (let ((selected-index (send autotest-actions-choice get-selection))
                (active-selection (send active-tests-list-box get-selection))
                (active-string (send active-tests-list-box get-string-selection))
                (inactive-selection (send inactive-tests-list-box get-selection))
                (inactive-string (send inactive-tests-list-box get-string-selection)))
            (cond ((equal? selected-index 1)  ; activate
                   (when (and (not (equal? #f inactive-selection))
                              (not (equal? NO-INACTIVE-AUTOTEST-MESSAGE inactive-string)))
                     (let ((at (send inactive-tests-list-box get-data inactive-selection)))
                       (autotest-set-active? at #t)
                       (write-autotest-list)
                       (write-autotest at))))
                  ((equal? selected-index 2)  ; deactivate
                   (when (and (not (equal? #f active-selection))
                              (not (equal? NO-ACTIVE-AUTOTEST-MESSAGE active-string)))
                     (let ((at (send active-tests-list-box get-data active-selection)))
                       (autotest-set-active? at #f)
                       (write-autotest-list)
                       (write-autotest at))))
                  ((equal? selected-index 3)  ; duplicate
                   (cond ((and (not (equal? #f active-selection))
                               (not (equal? NO-ACTIVE-AUTOTEST-MESSAGE active-string)))
                          (create-duplicate-autotest 
                           (send active-tests-list-box get-data active-selection)))
                         ((and (not (equal? #f inactive-selection))
                               (not (equal? NO-INACTIVE-AUTOTEST-MESSAGE inactive-string)))
                          (create-duplicate-autotest 
                           (send inactive-tests-list-box get-data inactive-selection)))
                         (else (void))))
                  ((equal? selected-index 4)  ; delete
                   (cond ((and (not (equal? #f active-selection))
                               (not (equal? NO-ACTIVE-AUTOTEST-MESSAGE active-string)))
                          (remove-from-autotest-list 
                           (autotest-id (send active-tests-list-box get-data active-selection))))
                         ((and (not (equal? #f inactive-selection))
                               (not (equal? NO-INACTIVE-AUTOTEST-MESSAGE inactive-string)))
                          (remove-from-autotest-list 
                           (autotest-id (send inactive-tests-list-box get-data inactive-selection))))
                         (else (void))))
                  (else
                   (void)))
            (populate-active-tests-list-box)
            (populate-inactive-tests-list-box)
            (send autotest-actions-choice set-selection 0)
            (populate-autotest-info 'default)
            (setup-ui))))))


;; **********************************************************************
;; * Right Side
;; **********************************************************************

(define top-right-h-pane
  (new horizontal-pane%
       (parent right-v-panel)))

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

;; Shows the name of currently selected autotest item.
;; User can edit it to change the name.
(define autotest-name-text-field
  (new text-field%
       (parent selected-test-name-box)
       (label "Autotest Name ")
       (min-width 340)
       (horiz-margin 3)
       (callback
        (lambda (tf e)
          (set-autotest-name-text-field-background)))))

(define schedule-new-button-pane
  (new pane%
       (parent top-right-h-pane)
       (alignment '(right center))))

;; Button - enables the input fields so the user can fill them out
;; to create a new autotest.
(define schedule-new-button
  (new button%
       (parent schedule-new-button-pane)
       (label "Create A New Autotest")
       (callback
        (lambda (b e)
          (config-ui-create-mode)))))

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

;; list box containing the files to execute for the selected autotest.
(define files-list-box
  (new list-box%
       (parent files-list-and-buttons-pane)
       (label #f)
       (choices '())
       (min-width 400)
       (min-height 150)
       (style '(multiple))))

(define MISSING-FILE-PREFIX "<Missing> ")
(define MISSING-FILE-PREFIX-LENGTH 9)
(define (refresh-files-list-box)
  (let ((num-files (send files-list-box get-number)))
    (for ((i num-files))
      (let ((str (send files-list-box get-string i))
            (prefixed? (regexp-match MISSING-FILE-PREFIX (send files-list-box get-string i))))
        (define file (if prefixed?
                         (substring str (+ MISSING-FILE-PREFIX-LENGTH 1))
                         str))
        (cond ((file-exists? file)
               (when prefixed?
                 (send files-list-box set-string i file)))
              (else
               (when (not prefixed?)
                 (send files-list-box set-string i (string-append MISSING-FILE-PREFIX file)))))))))

(define right-buttons-pane
  (new vertical-pane%
       (parent files-list-and-buttons-pane)
       (stretchable-width #f)
       (alignment '(left top))))

;; "Add file" button - opens the file-choose dialog.
(define add-file-button
  (new button%
       (parent right-buttons-pane)
       (label ADD-FILE-ICON)
       (callback
        (lambda (b e)
          (let ((filepaths (get-file-list "Select test files" main-frame)))
            (when (not (eq? filepaths #f))
              (for-each (lambda (path) (send files-list-box append (path->string path))) filepaths))
            (toggle-files-warning-icon))))))

;; "Remove file" button - deletes the selected files.
(define remove-file-button
  (new button%
       (parent right-buttons-pane)
       (label REMOVE-FILE-ICON)
       (callback
        (lambda (b e)
          (let ((selected-indexes (sort (send files-list-box get-selections) >)))
            (when (not (null? selected-indexes))
              (for-each (lambda (n) (send files-list-box delete n)) selected-indexes))
            (toggle-files-warning-icon))))))

(define files-warning-pane
  (new pane%
       (parent right-buttons-pane)
       (alignment '(left bottom))))

;; warning sign to show when no file is selected for an autotest item
(define files-list-box-warning-label
  (new message%
       (parent files-warning-pane)
       (label WARNING-ICON)))

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
       (min-width 55)
       (stretchable-width #f)
       (choices (list "AM" "PM"))
       (init-value "AM")
       (callback
        (lambda (c e)
          (set-ampm-combo-field-background)))))

;; Some UIs need to be disabled depending on whether one-time or periodic
;; radio button is selected.
(define (toggle-onetime-periodic-pane)
  (let ((selected-index (send one-time-periodic-radio-box get-selection)))
    (if (eq? selected-index 0)  ; one-time
        (begin (set-one-time-fields-background)
               (disable-all-children days-of-week-pane)
               (enable-all-children date-picker-pane))
        (begin (set-one-time-fields-background #f)  ; periodic
               (when (send check-daily get-value)
                 (check-all-days))
               (disable-all-children date-picker-pane)
               (enable-all-children days-of-week-pane)
               (toggle-periodic-warning-icon)))))

(define one-time-periodic-radio-box
  (new radio-box%
       (parent schedule-panel)
       (label #f)
       (style '(horizontal))
       (choices '("One-Time" "Repeat"))
       (selection 0)
       (callback
        (lambda (r e)
          (toggle-onetime-periodic-pane)))))

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
       (min-width 100)
       (stretchable-width #f)
       (horiz-margin 2)
       (choices year-list)
       (init-value (number->string (current-year)))
       (callback
        (lambda (c e)
          (set-one-time-fields-background)))))

(define month-combo-field
  (new combo-field%
       (parent date-picker-pane)
       (label "Month ")
       (min-width 95)
       (stretchable-width #f)
       (choices month-list)
       (init-value (number->string (current-month)))
       (callback
        (lambda (c e)
          (set-one-time-fields-background)))))

(define date-text-field
  (new text-field%
       (parent date-picker-pane)
       (label "Date ")
       (min-width 65)
       (stretchable-width #f)
       (init-value (number->string (current-day)))
       (callback
        (lambda (t e)
          (set-one-time-fields-background)))))

(define periodic-group-box
  (new group-box-panel%
       (parent schedule-panel)
       (label "Repeat")
       (border 5)))

(define days-of-week-pane
  (new horizontal-pane%
       (parent periodic-group-box)
       (spacing 10)
       (alignment '(left center))))

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
              (uncheck-all-days))
          (toggle-periodic-warning-icon)))))

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
          (toggle-check-daily c)
          (toggle-periodic-warning-icon)))))
(define check-tuesday
  (new check-box%
       (parent days-of-week-pane)
       (label "Tue")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)
          (toggle-periodic-warning-icon)))))
(define check-wednesday
  (new check-box%
       (parent days-of-week-pane)
       (label "Wed")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)
          (toggle-periodic-warning-icon)))))
(define check-thursday
  (new check-box%
       (parent days-of-week-pane)
       (label "Thu")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)
          (toggle-periodic-warning-icon)))))
(define check-friday
  (new check-box%
       (parent days-of-week-pane)
       (label "Fri")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)
          (toggle-periodic-warning-icon)))))
(define check-saturday
  (new check-box%
       (parent days-of-week-pane)
       (label "Sat")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)
          (toggle-periodic-warning-icon)))))
(define check-sunday
  (new check-box%
       (parent days-of-week-pane)
       (label "Sun")
       (value #f)
       (callback
        (lambda (c e)
          (toggle-check-daily c)
          (toggle-periodic-warning-icon)))))

;; warning to show when "repeat" is selected but no days are checked.
(define periodic-warning-label
  (new message%
       (parent days-of-week-pane)
       (label WARNING-ICON)))

;; UI for configuring email notification
(define email-panel
  (new group-box-panel%
       (parent right-v-panel)
       (label "Notify results via email")
       (border 2)
       (min-height 60)
       (alignment '(left top))))

(define email-v-pane
  (new vertical-pane%
       (parent email-panel)
       (spacing 2)
       (alignment '(left center))))

(define check-notify
  (new check-box%
       (parent email-v-pane)
       (label "Notify?")
       (horiz-margin 10)
       (value #f)))

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

;; Opens a dialog to selects a mailing list for sending the test result.
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

;; Shows an icon signaling changes are successfully saved.
(define saved-changes-label
  (new message%
       (parent activate-buttons-pane)
       (label OK-ICON)))
(define (show-saved-message)
  (thread
   (lambda ()
     (send saved-changes-label show #t)
     (sleep 1.0)
     (send saved-changes-label show #f))))

;; Updates the selected autotest item with information in the 
;; current UI input form.
(define (save-changes at)
  (let ((temp-at (make-autotest-from-ui (autotest-active? at))))
    (autotest-set-name at (autotest-name temp-at))
    (autotest-set-active? at (autotest-active? temp-at))
    (autotest-set-files at (autotest-files temp-at))
    (autotest-set-type at (autotest-type temp-at))
    (autotest-set-year at (autotest-year temp-at))
    (autotest-set-month at (autotest-month temp-at))
    (autotest-set-date at (autotest-date temp-at))
    (autotest-set-daily? at (autotest-daily? temp-at))
    (autotest-set-mon? at (autotest-mon? temp-at))
    (autotest-set-tue? at (autotest-tue? temp-at))
    (autotest-set-wed? at (autotest-wed? temp-at))
    (autotest-set-thu? at (autotest-thu? temp-at))
    (autotest-set-fri? at (autotest-fri? temp-at))
    (autotest-set-sat? at (autotest-sat? temp-at))
    (autotest-set-sun? at (autotest-sun? temp-at))
    (autotest-set-hour at (autotest-hour temp-at))
    (autotest-set-minute at (autotest-minute temp-at))
    (autotest-set-ampm at (autotest-ampm temp-at))
    (autotest-set-notify? at (autotest-notify? temp-at))
    (autotest-set-email-db at (autotest-email-db temp-at))
    (write-autotest at)))

;; Calls save-changes procedure.
(define save-changes-button
  (new button%
       (parent activate-buttons-pane)
       (label "Save Changes")
       (min-height 30)
       (callback
        (lambda (b e)
          (let ((active-selection (send active-tests-list-box get-selection))
                (inactive-selection (send inactive-tests-list-box get-selection)))
            (cond ((not (entries-are-valid?))
                   (toggle-warning-icons))
                  ((and (not (equal? #f active-selection))
                        (not (equal? NO-ACTIVE-AUTOTEST-MESSAGE
                                     (send active-tests-list-box get-string-selection))))
                   (let ((at (send active-tests-list-box get-data active-selection)))
                     (save-changes at)
                     (send active-tests-list-box set-string active-selection
                           (send autotest-name-text-field get-value))
                     (send active-tests-list-box set-selection active-selection)
                     (show-saved-message)
                     (toggle-warning-icons #f)))
                  ((and (not (equal? #f inactive-selection))
                        (not (equal? NO-INACTIVE-AUTOTEST-MESSAGE
                                     (send inactive-tests-list-box get-string-selection))))
                   (let ((at (send inactive-tests-list-box get-data inactive-selection)))
                     (save-changes at)
                     (send inactive-tests-list-box set-string inactive-selection
                           (send autotest-name-text-field get-value))
                     (send inactive-tests-list-box set-selection inactive-selection)
                     (populate-inactive-tests-list-box)
                     (show-saved-message)
                     (toggle-warning-icons #f)))
                  (else (void))))))))

;; 'Create and Activate' Button
;; - Creates a new test and activates it at the same time.
(define create-active-button
  (new button%
       (parent activate-buttons-pane)
       (label "Create and Activate")
       (min-height 30)
       (callback
        (lambda (b e)
          (if (entries-are-valid?)
              (begin (add-to-autotest-list (make-autotest-from-ui #t))
                     (populate-active-tests-list-box)
                     (setup-ui)
                     (toggle-warning-icons #f))
              (toggle-warning-icons))))))

;; 'Create as Inactive' Button
;; - Creates and saves a new autotest schedule but does not activate it (it won't run).
(define create-inactive-button
  (new button%
       (parent activate-buttons-pane)
       (label "Create as Inactive")
       (min-height 30)
       (callback
        (lambda (b e)
          (if (entries-are-valid?)
              (begin (add-to-autotest-list (make-autotest-from-ui #f))
                     (populate-inactive-tests-list-box)
                     (setup-ui)
                     (toggle-warning-icons #f))
              (toggle-warning-icons))))))

;; Cancel Button - Cancels creating a new autotest schedule.
(define cancel-button
  (new button%
       (parent activate-buttons-pane)
       (label "Cancel")
       (min-height 30)
       (callback
        (lambda (b e)
          (toggle-warning-icons #f)
          (setup-ui)))))

;; Creates a new autotest object from the Ui input form.
(define (make-autotest-from-ui (activate? #t))
  (when (entries-are-valid?)
    (let ((num-files (send files-list-box get-number))
          (files '()))
      (for ((i num-files))
        (let ((file (send files-list-box get-string i)))
          (when (regexp-match MISSING-FILE-PREFIX file)
            (set! file (substring file (+ MISSING-FILE-PREFIX-LENGTH 1))))
          (set! files (append files (list file)))))
      (make-autotest (generate-autotest-id)
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

;; Fills the hour, minute, and AM/PM input fields with current time values.
(define (refresh-time-fields)
  (define hour (date-hour (current-date)))
  (define ampm (if (< hour 12) "AM" "PM"))
  (when (> hour 12) (set! hour (- hour 12)))
  (send hour-combo-field set-value (number->string hour))
  (send minute-text-field set-value (number->string (date-minute (current-date))))
  (send ampm-combo-field set-value ampm))

;; Fills the year, month, date input fields with current date values.
(define (refresh-date-fields)
  (send year-combo-field set-value (number->string (date-year (current-date))))
  (send month-combo-field set-value (number->string (date-month (current-date))))
  (send date-text-field set-value (number->string (date-day (current-date)))))

;; Fills the configuration pane (right side) with the selected autotest.
;; Uses default values when 'default is given as the argument.
(define (populate-autotest-info at)
  (cond ((eq? at 'default)
         (send autotest-name-text-field set-value "")
         (send files-list-box clear)
         (send one-time-periodic-radio-box set-selection 0)
         (toggle-onetime-periodic-pane)
         (uncheck-all-days)         
         (refresh-date-fields)
         (refresh-time-fields)
         (send check-notify set-value #f)
         (send mailing-list-name-message set-label NO-MAILING-LIST-MESSAGE))
        (else
         (send autotest-name-text-field set-value (autotest-name at))
         (set-autotest-name-text-field-background)
         (send files-list-box clear)
         (for-each (lambda (file) (send files-list-box append file)) (autotest-files at))
         (refresh-files-list-box)
         (if (eq? 'one-time (autotest-type at))
             (send one-time-periodic-radio-box set-selection 0)
             (send one-time-periodic-radio-box set-selection 1))
         (send year-combo-field set-value (number->string (autotest-year at)))
         (send month-combo-field set-value (number->string (autotest-month at)))
         (send date-text-field set-value (number->string (autotest-date at)))
         (send check-daily set-value (autotest-daily? at))
         (send check-monday set-value (autotest-mon? at))
         (send check-tuesday set-value (autotest-tue? at))
         (send check-wednesday set-value (autotest-wed? at))
         (send check-thursday set-value (autotest-thu? at))
         (send check-friday set-value (autotest-fri? at))
         (send check-saturday set-value (autotest-sat? at))
         (send check-sunday set-value (autotest-sun? at))
         (toggle-onetime-periodic-pane)
         (send hour-combo-field set-value (number->string (autotest-hour at)))
         (send minute-text-field set-value (number->string (autotest-minute at)))
         (send ampm-combo-field set-value (autotest-ampm at))
         (send check-notify set-value (autotest-notify? at))
         (if (not (equal? #f selected-email-db))
             (send mailing-list-name-message set-label (email-db-name selected-email-db))
             (send mailing-list-name-message set-label NO-MAILING-LIST-MESSAGE)))))

;; UI setting for normal browsing (when user clicks an autotest item from the list).
(define (config-ui-normal-mode)
  (define at
    (cond ((equal? #f (send active-tests-list-box get-selection))
           (send inactive-tests-list-box get-data (send inactive-tests-list-box get-selection)))
          (else
           (send active-tests-list-box get-data (send active-tests-list-box get-selection)))))
  (send last-run-text set-label (autotest-last-run-string at))
  (send next-due-text set-label (autotest-next-due-string at))
  (send last-run-text show #t)
  (send next-due-text show #t)
  (send schedule-new-button enable #t)
  (enable-all-children left-v-panel)
  (send selected-test-name-box change-children name-field-normal-mode)
  (send files-list-box enable #t)
  (enable-file-buttons)
  (send one-time-periodic-radio-box enable #t)
  (toggle-onetime-periodic-pane)
  (enable-all-children time-of-day-pane)
  (enable-email-db-controls)
  (send activate-buttons-pane change-children buttons-normal-mode)
  (enable-all-children activate-buttons-pane)
  (set! selected-email-db (autotest-email-db at))
  (toggle-warning-icons #f)
  (populate-autotest-info at))

;; UI setting when no autotest is selected by user.
(define (config-ui-no-selection-mode)
  (send last-run-text show #f)
  (send next-due-text show #f)
  (enable-all-children left-v-panel)
  (populate-autotest-info 'default)
  (send selected-test-name-box change-children name-field-no-selection-mode)
  (send selected-autotest-message set-label NO-AUTOTEST-SELECTED-MESSAGE)
  (send schedule-new-button enable #t)
  (send files-list-box enable #f)
  (disable-file-buttons)
  (send one-time-periodic-radio-box enable #f)
  (set-one-time-fields-background #f)
  (set-time-fields-background #f)
  (disable-all-children date-picker-pane)
  (disable-all-children days-of-week-pane)
  (disable-all-children time-of-day-pane)
  (disable-email-db-controls)
  (send activate-buttons-pane change-children buttons-empty-mode)
  (disable-all-children activate-buttons-pane)
  (set! selected-email-db #f)
  (toggle-warning-icons #f))

;; UI setting when no autotest schedule exists to show.
(define (config-ui-empty-mode)
  (send last-run-text show #f)
  (send next-due-text show #f)
  (enable-all-children left-v-panel)
  (send selected-test-name-box change-children name-field-empty-mode)
  (send selected-autotest-message set-label NO-AUTOTEST-SCHEDULED-MESSAGE)
  (send schedule-new-button enable #t)
  (send files-list-box enable #f)
  (disable-file-buttons)
  (send one-time-periodic-radio-box enable #f)
  (set-one-time-fields-background #f)
  (set-time-fields-background #f)
  (disable-all-children date-picker-pane)
  (disable-all-children days-of-week-pane)
  (disable-all-children time-of-day-pane)
  (disable-email-db-controls)
  (send activate-buttons-pane change-children buttons-empty-mode)
  (disable-all-children activate-buttons-pane)
  (set! selected-email-db #f)
  (toggle-warning-icons #f))

;; UI setting when creating a new autotest schedule.
(define (config-ui-create-mode)
  (define selection-in-active-tests-list-box (send active-tests-list-box get-selection))
  (when (not (equal? #f selection-in-active-tests-list-box))
    (send active-tests-list-box select selection-in-active-tests-list-box #f))
  (define selection-in-inactive-tests-list-box (send inactive-tests-list-box get-selection))
  (when (not (equal? #f selection-in-inactive-tests-list-box))
    (send inactive-tests-list-box select selection-in-inactive-tests-list-box #f))
  (send last-run-text show #f)
  (send next-due-text show #f)
  (disable-all-children left-v-panel)
  (populate-autotest-info 'default)
  (set-autotest-name-text-field-background)
  (send selected-test-name-box change-children name-field-create-mode)
  (send schedule-new-button enable #f)
  (send files-list-box enable #t)
  (enable-file-buttons)
  (send one-time-periodic-radio-box enable #t)
  (set-time-fields-background)
  (toggle-onetime-periodic-pane)
  (enable-all-children time-of-day-pane)
  (enable-email-db-controls)
  (send activate-buttons-pane change-children buttons-create-mode)
  (set! selected-email-db #f))

;; Renders an appropriate UI configuration based on user action.
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
        (else (config-ui-normal-mode)))
  (send saved-changes-label show #f))


;; **********************************************************************
;; * Helpers to enable/disable UI controls based on user move
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
  (list save-changes-button saved-changes-label))
(define buttons-empty-mode buttons-normal-mode)

(define (disable-all-children area-container)
  (for-each send-disable (send area-container get-children)))

(define (enable-all-children area-container)
  (for-each send-enable (send area-container get-children)))

(define (delete-all-children area-container)
  (define (send-delete-child child)
    (send area-container delete-child child))
  (for-each send-delete-child (send area-container get-children)))

(define (enable-file-buttons)
  (send add-file-button enable #t)
  (send remove-file-button enable #t))

(define (disable-file-buttons)
  (send add-file-button enable #f)
  (send remove-file-button enable #f))

(define (enable-email-db-controls)
  (send check-notify enable #t)
  (send select-email-db-button enable #t))

(define (disable-email-db-controls)
  (send check-notify enable #f)
  (send select-email-db-button enable #f))

;; helpers for enable-all-children and disable-all-children
(define (send-enable control) (send control enable #t))
(define (send-disable control) (send control enable #f))


;; **********************************************************************
;; * Helpers to show a different field background color for invalid input.
;; **********************************************************************

(define (set-autotest-name-text-field-background)
  (if (valid-autotest-name-string? (send autotest-name-text-field get-value))
      (send autotest-name-text-field set-field-background VALID-FIELD-COLOR)
      (send autotest-name-text-field set-field-background INVALID-FIELD-COLOR)))

(define (set-minute-text-field-background)
  (if (valid-minute-string? (send minute-text-field get-value))
      (send minute-text-field set-field-background VALID-FIELD-COLOR)
      (send minute-text-field set-field-background INVALID-FIELD-COLOR)))

(define (set-hour-combo-field-background)
  (if (valid-hour-string? (send hour-combo-field get-value))
      (send hour-combo-field set-field-background VALID-FIELD-COLOR)
      (send hour-combo-field set-field-background INVALID-FIELD-COLOR)))

(define (set-ampm-combo-field-background)
  (if (valid-ampm-string? (send ampm-combo-field get-value))
      (send ampm-combo-field set-field-background VALID-FIELD-COLOR)
      (send ampm-combo-field set-field-background INVALID-FIELD-COLOR)))

(define (set-year-combo-field-background)
  (if (valid-year-string? (send year-combo-field get-value))
      (send year-combo-field set-field-background VALID-FIELD-COLOR)
      (send year-combo-field set-field-background INVALID-FIELD-COLOR)))

(define (set-month-combo-field-background)
  (if (valid-month-string? (send month-combo-field get-value))
      (send month-combo-field set-field-background VALID-FIELD-COLOR)
      (send month-combo-field set-field-background INVALID-FIELD-COLOR)))

(define (set-date-text-field-background)
  (define month (string->number (send month-combo-field get-value)))
  (define year (string->number (send year-combo-field get-value)))
  (if (valid-date-string? (send date-text-field get-value) month year)
      (send date-text-field set-field-background VALID-FIELD-COLOR)
      (send date-text-field set-field-background INVALID-FIELD-COLOR)))

(define (set-time-fields-background (enable? #t))
  (if (not enable?)
      (begin (send hour-combo-field set-field-background VALID-FIELD-COLOR)
             (send minute-text-field set-field-background VALID-FIELD-COLOR)
             (send ampm-combo-field set-field-background VALID-FIELD-COLOR))
      (begin (set-hour-combo-field-background)
             (set-minute-text-field-background)
             (set-ampm-combo-field-background))))
             
(define (set-one-time-fields-background (enable? #t))
  (if (not enable?)
      (begin (send year-combo-field set-field-background VALID-FIELD-COLOR)
             (send month-combo-field set-field-background VALID-FIELD-COLOR)
             (send date-text-field set-field-background VALID-FIELD-COLOR))
      (begin
        (set-date-text-field-background)
        (set-month-combo-field-background)
        (set-year-combo-field-background))))

;; Shows a warning icon for entries that have no field background to signal invalid input.
(define (toggle-files-warning-icon (enable? #t))
  (cond (enable?
         (if (< 0 (send files-list-box get-number))
             (send files-list-box-warning-label show #f)
             (send files-list-box-warning-label show #t)))
        (else
         (send files-list-box-warning-label show #f))))

(define (toggle-periodic-warning-icon (enable? #t))
  (cond (enable?
         (if (valid-one-time-periodic-entries?)
             (send periodic-warning-label show #f)
             (send periodic-warning-label show #t)))
        (else
         (send periodic-warning-label show #f))))

(define (toggle-warning-icons (enable? #t))
  (cond (enable?
         (toggle-files-warning-icon)
         (toggle-periodic-warning-icon))
        (else
         (toggle-files-warning-icon #f)
         (toggle-periodic-warning-icon #f))))

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

;; clock
(thread (lambda ()
          (let loop ()
            (refresh-run-info)
            (refresh-files-list-box)
            (sleep 1.0)
            (loop))))

;; entry point - opens the UI.
(define (launch-scheduler)
  (populate-active-tests-list-box)
  (populate-inactive-tests-list-box)
  (setup-ui)
  (send main-frame center)
  (send main-frame show #t))
