#lang racket/gui

(require racket/flonum
         "autotest.rkt"
         "scheduler.rkt"         
         "../QA-Email/email-db.rkt"
         "../QA-Email/email-db-ui.rkt")

(define test-file-list '())
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
       (spacing 10)
       (horiz-margin 3)
       (alignment '(center top))))

(define right-v-panel
  (new vertical-panel%
       (parent main-h-panel)  
       (spacing 4)
       (min-width 400)
       (alignment '(left top))))

;; List box for the list of active tests (left pane).
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
          (populate-active-tests-list-box)))))

;; Fills the email db list box (left pane)
(define (populate-active-tests-list-box)
  (send active-tests-list-box clear)
  (define (add-to-list-box test)
    (define name (test-name test))
    (send active-tests-list-box append name test))
  (cond ((equal? (length active-test-list) 0)
         (send active-tests-list-box append "No active test"))
        (else (map add-to-list-box active-test-list))))


;; List box for the list of INactive tests (left pane).
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
          (populate-active-tests-list-box)))))

;; Fills the email db list box (left pane)
(define (populate-inactive-tests-list-box)
  (send inactive-tests-list-box clear)
  (define (add-to-list-box test)
    (define name (test-name test))
    (send inactive-tests-list-box append name test))
  (cond ((equal? (length inactive-test-list) 0)
         (send inactive-tests-list-box append "No inactive test"))
        (else (map add-to-list-box inactive-test-list))))

(define top-right-h-pane
  (new horizontal-pane%
       (parent right-v-panel)))

;; box enclosing the selected email-db name (right pane)
(define selected-test-name-box
  (new pane%
       (parent top-right-h-pane)
       (min-height 20)
       (stretchable-width #f)
       (stretchable-height #f)
       (alignment '(left center))))

(define selected-test-name-label
  (new message%
       (parent selected-test-name-box)
       (horiz-margin 12)
       (label "No auto-test is selected")
       (auto-resize #t)))

(define create-button-pane
  (new pane%
       (parent top-right-h-pane)
       (alignment '(right center))))

(define create-autotest-button
  (new button%
       (parent create-button-pane)
       (label "Schedule A New Autotest")
       (callback
        (lambda (b e)
          (config-ui-create-mode)))))

;; pane below the selected email-db title box
(define test-resource-panel
  (new group-box-panel%
       (parent right-v-panel)
       (label "Files to execute")
       (alignment '(left top))))

(define files-list-and-buttons-pane 
  (new horizontal-pane%
       (parent test-resource-panel)
       (alignment '(left top))))

(define file-list-box
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
        (define filepaths (get-file-list))
        (when (not (eq? filepaths #f))                   
          (define (add-to-file-list-box path)
            (send file-list-box append (path->string path)))
          (map add-to-file-list-box filepaths)))))

;; Button to remove files
(new button%
     (parent right-buttons-pane)
     (label "Remove"))

;; UI for configuring schedule
(define schedule-panel
  (new group-box-panel%
       (parent right-v-panel)
       (label "When to execute")
       (border 5)
       (spacing 5)
       (alignment '(left top))))

;; Some UIs need to be disabled depending on whether one-time or periodic
;; radio button is selected.
(define (toggle-onetime-periodic)
  (define selection-index (send one-time-periodic-radio-box get-selection)) 
  (if (eq? selection-index 0)  ; one-time
      (begin (disable-all-children days-of-week-pane)
             (enable-all-children date-picker-pane))
      (begin (send date-text-field set-field-background (make-object color% "White"))
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

(define date-picker-pane
  (new horizontal-pane%
       (parent schedule-panel)
       (spacing 10)
       (alignment '(left top))))

(define year-picker
  (new combo-field%
       (parent date-picker-pane)
       (label "Year ")
       (stretchable-width #f)
       (choices '())
       (init-value "")))  ; change this to current year

(define month-picker
  (new combo-field%
       (parent date-picker-pane)
       (label "Month ")
       (stretchable-width #f)
       (choices (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"))
       (init-value "")))                     

(define date-text-field
  (new text-field%
       (parent date-picker-pane)
       (label "Date ")
       (stretchable-width #f)
       (callback
        (lambda (t e)
          (set-date-text-field-background)))))

(define days-of-week-pane
  (new horizontal-pane%
       (parent schedule-panel)
       (spacing 10)
       (alignment '(left top))))

(define check-all
  (new check-box%
       (parent days-of-week-pane)
       (label "Daily")
       (value #f)
       (callback
        (lambda (c e)
          (if (send check-all get-value)
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
       (value #f)))
(define check-tuesday
  (new check-box%
       (parent days-of-week-pane)
       (label "Tue")
       (value #f)))
(define check-wednesday
  (new check-box%
       (parent days-of-week-pane)
       (label "Wed")
       (value #f)))
(define check-thursday
  (new check-box%
       (parent days-of-week-pane)
       (label "Thu")
       (value #f)))
(define check-friday
  (new check-box%
       (parent days-of-week-pane)
       (label "Fri")
       (value #f)))
(define check-saturday
  (new check-box%
       (parent days-of-week-pane)
       (label "Sat")
       (value #f)))
(define check-sunday
  (new check-box%
       (parent days-of-week-pane)
       (label "Sun")
       (value #f)))
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

(define time-of-day-pane
  (new horizontal-pane%
       (parent schedule-panel)
       (spacing 10)
       (alignment '(left top))))
(define hour-combo-field
  (new combo-field%
       (parent time-of-day-pane)
       (label "Hour")
       (min-width 85)
       (stretchable-width #f)
       (choices (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"))
       (init-value "1")))
(define minute-text-field
  (new text-field%
       (parent time-of-day-pane)
       (label "Minute")
       (init-value "00")
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
       (init-value "AM")))


;; UI for configuring email notification
(define email-panel
  (new group-box-panel%
       (parent right-v-panel)
       (label "Notify results via email")
       (border 2)
       (min-height 60)  ; see if this can go later
       (alignment '(left top))))

(define email-h-pane
  (new horizontal-pane%
       (parent email-panel)
       (spacing 2)
       (alignment '(left center))))

(define check-notify
  (new check-box%
       (parent email-h-pane)
       (label "Notify?")
       (horiz-margin 10)
       (value #f)
       (callback 
        (lambda (c e)
          (define checked? (send check-notify get-value))
          (void)
          ;; if checking, see if mailing list is configured
          ;; ask for it if not
          ))))

;; Button - Select mailing list
(new button%
     (parent email-h-pane)
     (label "Select mailing list...")
     (callback 
      (lambda (b e)
        (define email-db (open-manage-mailing-list-dialog 'return-db))
        (define db-name (email-db-name email-db))
        (send mailing-list-name-message set-label db-name))))

(define mailing-list-name-message
  (new message%
       (parent email-h-pane)
       (label "Mailing list name goes here...")))

(define activate-buttons-pane
  (new horizontal-pane%
       (parent right-v-panel)
       (alignment '(center top))))

;; Toggle activate/deactivate
;; Only one of these buttons should show at any time.
(define activate-button
  (new button%
       (parent activate-buttons-pane)
       (label "Activate")
       (min-width 80)
       (callback
        (lambda (b e) (void)))))

(define deactivate-button
  (new button%
       (parent activate-buttons-pane)
       (label "Deactivate")
       (min-width 80)
       (callback
        (lambda (b e) (void)))))

;; Button - Deletes the currently selected test
(define delete-test-button
  (new button%
       (parent activate-buttons-pane)
       (label "Delete this test")
       (callback 
        (lambda (b e) (void)))))

;; 'Create and Activate' Button 
;; - Creates a new test and activates it at the same time
(define create-active-button
  (new button%
       (parent activate-buttons-pane)
       (label "Create and Activate")
       (callback
        (lambda (b e) (void)))))

;; 'Create as Inactive' Button
;; - Creates a new autotest schedule but does not activate it
(define create-inactive-button
  (new button%
       (parent activate-buttons-pane)
       (label "Create as Inactive")
       (callback
        (lambda (b e) (void)))))

;; Cancel Button - Cancels creating a new autotest schedule
(define cancel-button
  (new button%
       (parent activate-buttons-pane)
       (label "Cancel")
       (callback
        (lambda (b e) (config-ui-normal-mode)))))

(define (create-new-autotest)
  (void))

(define (config-ui-normal-mode)
  (cond ((and (equal? 0 (send active-tests-list-box get-number))
              (equal? 0 (send inactive-tests-list-box get-number)))
         (config-ui-empty-mode))
        ((and (equal? #f (send active-tests-list-box get-selection))
              (equal? #f (send inactive-tests-list-box get-selection)))
         (config-ui-empty-mode)
         (send selected-test-name-label set-label "No autotest selected"))
        (else
         (enable-all-children left-v-panel)
         (send file-list-box enable #t)
         (enable-all-children right-buttons-pane)
         (send one-time-periodic-radio-box enable #t)
         (toggle-onetime-periodic)
         (send activate-buttons-pane change-children buttons-normal-mode)
         (enable-all-children activate-buttons-pane))))

;; UI setting for when no autotest schedule exists
(define (config-ui-empty-mode)
  (disable-all-children left-v-panel)
  (send file-list-box enable #f)
  (disable-all-children right-buttons-pane)
  (send one-time-periodic-radio-box enable #f)
  (send date-text-field set-field-background (make-object color% "White"))
  (disable-all-children date-picker-pane)  
  (disable-all-children days-of-week-pane)
  (send minute-text-field set-field-background (make-object color% "White"))
  (disable-all-children time-of-day-pane)  
  (disable-all-children email-h-pane)
  (send activate-buttons-pane change-children buttons-empty-mode)
  (disable-all-children activate-buttons-pane)  
  (send selected-test-name-label set-label "No autotest scheduled"))

;; UI setting for creating a new autotest schedule
(define (config-ui-create-mode)
  (disable-all-children left-v-panel)
  (send file-list-box enable #t)
  (enable-all-children right-buttons-pane)
  (send one-time-periodic-radio-box enable #t)
  (set-date-text-field-background)
  (set-minute-text-field-background)
  (toggle-onetime-periodic)
  (enable-all-children time-of-day-pane)
  (enable-all-children email-h-pane)
  (send activate-buttons-pane change-children buttons-create-mode))

;; helpers for change-children method for activate-buttons-pane
(define (buttons-create-mode arg-not-used)
  (list create-active-button create-inactive-button cancel-button))
(define (buttons-normal-mode arg-not-used)
  (list activate-button deactivate-button delete-test-button))
(define buttons-empty-mode buttons-normal-mode)

(define (disable-all-children area-container)
  (map send-disable (send area-container get-children)))

(define (enable-all-children area-container)
  (map send-enable (send area-container get-children)))

(define (delete-all-children area-container)
  (define (send-delete-child child)
    (send area-container delete-child child))
  (map send-delete-child (send area-container get-children)))

;; helpers for enable-all-children and disable-all-children
(define (send-enable control) (send control enable #t))
(define (send-disable control) (send control enable #f))

;; Shows red background when invalid date is entered.
(define (set-date-text-field-background)
  (define month (string->number (send month-picker get-value)))
  (define year (string->number (send year-picker get-value)))
  (if (valid-date-string? (send date-text-field get-value) month year)
      (send date-text-field set-field-background (make-object color% "White"))
      (send date-text-field set-field-background (make-object color% "Red"))))

;; Shows red background when invalid minute (!0-60) is typed.
(define (set-minute-text-field-background)
  (if (valid-minute-string? (send minute-text-field get-value))
      (send minute-text-field set-field-background (make-object color% "White"))
      (send minute-text-field set-field-background (make-object color% "Red"))))

(define (valid-date-string? a-string month year)
  (define max-days (days-in-month month year))
  (not (or (not (string? a-string))
           (> (string-length a-string) 2)
           (equal? #f (string->number a-string))
           (< (string->number a-string) 1)
           (> (string->number a-string) max-days))))

(define (valid-minute-string? a-string)
  (not (or (not (string? a-string))
           (> (string-length a-string) 2)
           (equal? #f (string->number a-string))
           (< (string->number a-string) 0)
           (> (string->number a-string) 60))))

;; entry point
;; Opens the autotest scheduler UI
(define (open-manage-schedule)
  (send year-picker append (number->string (current-year)))
  (send year-picker append (number->string (+ 1 (current-year))))
  (send year-picker set-value (number->string (current-year)))
  (send month-picker set-value (number->string (current-month)))
  (send date-text-field set-value (number->string (current-date)))
  (send one-time-periodic-radio-box set-selection 0)
  (disable-all-children days-of-week-pane)
  (enable-all-children date-picker-pane)
  (cond ((and (equal? 0 (send active-tests-list-box get-number))
              (equal? 0 (send inactive-tests-list-box get-number)))
         (config-ui-empty-mode))
        (else (config-ui-normal-mode)))
  (send manage-scheduler-frame show #t))

(open-manage-schedule)