#lang racket/gui

(require "scheduler.rkt"
         "user-settings-directory.rkt"
         "email-db.rkt"
         "email-db-ui.rkt")

(define test-file-list '())

(define manage-scheduler-dialog
  (new dialog%
       (label "Manage Automated Tests")
       (spacing 2)
       (style '(close-button))))

(define main-h-panel
  (new horizontal-panel%
       (parent manage-scheduler-dialog)
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
       (stretchable-height #f)
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
       (stretchable-height #f)
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



;; box enclosing the selected email-db name (right pane)
(define selected-test-name-box
  (new pane%
       (parent right-v-panel)
       (min-height 20)
       (stretchable-width #f)
       (stretchable-height #f)
       (alignment '(left center))))

(define selected-test-name-label
  (new message%
       (parent selected-test-name-box)
       (horiz-margin 12)
       (label "Selected test name goes here...")
       (auto-resize #t)))

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
       (stretchable-height #f)
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
       (spacing 0)
       (alignment '(left top))))

(define days-of-week-pane
  (new horizontal-pane%
       (spacing 10)
       (parent schedule-panel)
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
       (min-width 70)
       (stretchable-width #f)
       (choices (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"))
       (init-value "1")))
(define minute-text-field
  (new text-field%
       (parent time-of-day-pane)
       (label "Minute")
       (init-value "00")
       (min-width 70)
       (stretchable-width #f)))
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
(new button%
     (parent activate-buttons-pane)
     (label "Activate")
     (min-width 80)
     (callback
      (lambda (b e) (void))))
(new button%
     (parent activate-buttons-pane)
     (label "Deactivate")
     (min-width 80)
     (callback
      (lambda (b e) (void))))

;; Button - Deletes the currently selected test
(new button%
     (parent activate-buttons-pane)
     (label "Delete this test")
     (callback 
      (lambda (b e) (void))))


;; file info - one file
(define (make-file-info id name path)
  (define (change-id new-id)
    (set! id new-id))
  (define (change-path new-file-path)
    (set! path new-file-path)
    (set! name (get-filename-from-filepath new-file-path)))
  (define (dispatch m a)
    (cond ((eq? m 'id) id)
          ((eq? m 'name) name)
          ((eq? m 'path) path)
          ((eq? m 'set-id) (change-id a))
          ((eq? m 'set-path) (change-path a))))
  dispatch)
  

(define (make-files-list file-infos)
  1)


















(define (open-manage-schedule)
  (send manage-scheduler-dialog show #t))
(open-manage-schedule)