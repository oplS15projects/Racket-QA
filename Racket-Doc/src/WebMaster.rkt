;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; File: WebMaster.rkt                                          ;;
;; Author: James Kuczynski                                      ;;
;; Email: jkuczyns@cs.uml.edu                                   ;;
;; File Description: This file runs a web server and initiates  ;;
;;                   pages on it.                               ;;
;;                                                              ;;
;;Created 04/03/2015                                            ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket
(require web-server/servlet web-server/servlet-env)
(require racket/gui)

;;data--------------------
;list of requires
(define requireLst (list "racket/gui" "racket/filesystem" "racket/base"))

(define (generateReqBlock block givenList)
  (if (= (car givenList) empty)
          block
         (generateReqBlock (string-append block (car givenList)))
  )
)

;list of provides
(define provideLst (list "square" "addTwo"))

;list of proc headers
(define procHeadersLst (list "(square num)" "(addTwo val1 val2)" "(doToVal proc num)"))

;list of proc documentation
(define procDoc (list "this is @param and other doc stuff" "doc stuf" "doc stuff(3)"))

;list of proc code
(define procCode (list "code..." "code(2)..." "code(3)..."))
;;--------------------

;(define generat

;(play-sound "./share/rach.wav" #f)

;(define logo
;  (read-bitmap "./../share/button.jpg"))

(define (start request)
  (main-page request))

;"main" page:
(define (main-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
                    (body (h1 (center "Racket-Doc Home"))
                          (center
                          (a ((href, (embed/url fileList-page))) "File List")
                          (html nbsp nbsp nbsp nbsp)                           
                          (a ((href, (embed/url required-page))) "Required")
                          (html nbsp nbsp nbsp nbsp)
                          (a ((href, (embed/url provided-page))) "Provided")
                          (html nbsp nbsp nbsp nbsp)
                          (a ((href, (embed/url procAndData-page))) "Procedures & Data")
                          (html nbsp nbsp nbsp nbsp)
                          (a ((href, (embed/url help-page))) "Help"))
                          )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying file list
(define (fileList-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Test.rkt")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (p (b "Files:"))
                     (a ((href, (embed/url specifiedFile-page))) "file_name1.rkt")
                     (br)
                     (br)
                     (a ((href, (embed/url specifiedFile-page))) "file_name2.rkt")
                     (br)
                     (br)
                     (a ((href, (embed/url specifiedFile-page))) "file_name3.rkt")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying file list
(define (specifiedFile-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Test.rkt")
                     (center 
                      (a ((href ,(embed/url main-page))) "<--")
                      (html nbsp nbsp nbsp nbsp)
                      (a ((href ,(embed/url main-page))) "Home")
                      (html nbsp nbsp nbsp nbsp)
                      (a ((href ,(embed/url main-page))) "-->")
                      )
                     (br)(br)
                     (p "\"Specified File\" page")
                     ;add requires
                     (b "Required")
                     (fieldset (code (list "racket/gui" (br) "racket/filesystem" (br))))
                     (br) (br) (br)
                     ;add provides
                     (b "Provided")
                     (fieldset (code (list "square" (br) "addTwo")))
                     (br) (br) (br)
                     ;;add procs and data
                     (fieldset
                      (code (list (b "(square num)") (br) 
                                  (html "This procedure squares a given number") (br)
                                  (i "@param num: ") (html "Given value to be squared.") (br)
                                  (i "@return: the squared number.")
                                  )))
                     (a ((href, (embed/url fileList-page))) "Code")
                     (br) (br) (br)
                     (fieldset
                      (code (list (b "(addTwo val1 val2)") (br)
                                  (html "This procedure adds together two numbers.") (br)
                                  (i "@param val1: ") (html "The first addend.") (br)
                                  (i "@param val2: ") (html "The second addend.")
                                  )))
                     (a ((href, (embed/url fileList-page))) "Code")
                         )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying dependencies
(define (required-page request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Dependencies")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (b "Required")
                     #|(spin-out procCode)
                     (define (spin-out myList)
                       (cond ( (null? myList)
                               (display "end")
                             )
                             (else
                              (fieldset (code (car myList) (br)))
                              (spin-out (cdr myList))
                             )
                       )
                     )|#
                     (fieldset (code (list "racket/gui" (br) "racket/filesystem" (br))))
                     ))))]
    (send/suspend/dispatch response-generator)))


;;page for displaying provideds
(define (provided-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Provided")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (b "Provided")
                     (fieldset (code (list "square" (br) "addTwo")))
                     )))))
    (send/suspend/dispatch response-generator)))



;;page for displaying procs and data of a single file
(define (procAndData-page request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedures & Data")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     ;;add procs and data
                     (br) (br)
                     (fieldset (code (list (b "(square num)") (br) (i "doc..." (br) "here"))))
                     (a ((href, (embed/url fileList-page))) "Code")
                     (br) (br) (br)
                     (fieldset (code (list (b "(addTwo val1 val2)") (br) (i "doc..." (br) "here"))))
                     (a ((href, (embed/url fileList-page))) "Code")
                     ))))]
    (send/suspend/dispatch response-generator)))


;;page for displaying procs and data of a single file
(define (help-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Help")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (p "help page...")
                     )))))
    (send/suspend/dispatch response-generator)))






(serve/servlet start
               #:listen-ip "127.0.0.1"
               ;#:port 8080
               #:servlet-path "/")