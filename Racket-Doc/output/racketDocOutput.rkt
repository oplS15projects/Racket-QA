;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  								;;
;; AUTO-GENERATED CODE.  To run, either open in Dr. Racket and  ;;
;; select the "Run" button, or open a terminal, go to the 	;;
;; directory containing this file, and run			;;
;; "racket [file_name].rkt".					;;
;;                                                              ;;
;; Date Generated On: 4/27/2015                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#lang web-server/insta

(require web-server/servlet web-server/servlet-env)
(require racket/gui)


;(define generat

;(play-sound "./Racket-Doc/share/rach.wav" #f)

;(define logo
;  (read-bitmap "./Racket-Doc/share/button.jpg"))

(define (start request)
  (main-page request))

;"main" page:
(define (main-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))                    (body (h1 (center "Racket-Doc Home"))
                          (center
                          (a ((href, (embed/url fileNameList-page))) "File List")
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
(define (fileNameList-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))               (body (h1 "*.rkt Files")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (p (b "Files:"))
                     (a ((href, (embed/url Test.rkt-page))) "Test.rkt")
                     (br)(br)
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for a specified file
(define (Test.rkt-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))               (body (h1 "*.rkt Files")
                     (center
                      (a ((href ,(embed/url main-page))) "Home")
                      (html nbsp nbsp nbsp nbsp)
                      )
                     (br)(br)
                     (p "Specified File page")
                     ;add requires
                     (b "Required")
                     (fieldset (code (list "#lang racket" (br) "(require \"testing.rkt\")" (br) "(require \"andThisTest.rkt\")" (br) )))
                     (br) (br) (br)
                     ;add included
                     (b "Included")
                     (fieldset (code (list "(include \"firstIncl.rkt\")" (br) "(include \"secIncl.rkt\")" (br) )))
                     (br) (br) (br)
                     ;add provided
                     (b "Provided")
                     (fieldset (code (list "(provide square)" (br) )))
                     (br) (br) (br)
                     ;;add procs and data
                     (b "Procedures & Data")
                     (fieldset
                      (code (list (b "(define (square num)") (br)
                             (i "#
  This function squares a given number.
  @param num Is the number to be squared.
           
  @return the square of the numbers
 #") (br))))                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (addFour num1 num2 num3 num4)") (br)
                             (i "#
  This function adds four numbers together.
  @param num1 The first number.
  @param num2 The
 	       second
 	       number.
  @param num3 The third number.
  @param num4 The fourth number.
  @return The sum of the numbers.
 #") (br))))                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define five") (br)
                             (i "#
  Created a variable named five.
  and gave it a value.
 #") (br))))                             (br) (br) (br)
                         )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying dependencies
(define (required-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))               (body (h1 "Dependencies")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (b "Required")
                     (fieldset (code (list "#lang racket" (br) "(require \"testing.rkt\")" (br) "(require \"andThisTest.rkt\")" (br) )))
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying provideds
(define (provided-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))               (body (h1 "Provided")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (b "Provided")
                     (fieldset (code (list "(provide square)" (br) )))
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying procs and data of a single file
(define (procAndData-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))               (body (h1 "Procedures & Data")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     ;;add procs and data
                     (br) (br)
                     (fieldset
                      (code (list (b "(define (square num)") (br)
                             (i "#
  This function squares a given number.
  @param num Is the number to be squared.
           
  @return the square of the numbers
 #") (br))))
                     (a ((href, (embed/url codeblock0-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define (addFour num1 num2 num3 num4)") (br)
                             (i "#
  This function adds four numbers together.
  @param num1 The first number.
  @param num2 The
 	       second
 	       number.
  @param num3 The third number.
  @param num4 The fourth number.
  @return The sum of the numbers.
 #") (br))))
                     (a ((href, (embed/url codeblock1-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "(define five") (br)
                             (i "#
  Created a variable named five.
  and gave it a value.
 #") (br))))
                     (a ((href, (embed/url codeblock2-page))) "Code")
                             (br) (br) (br)
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock0-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))                (body (h3 "Procedure:  (define (square num)")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                      (fieldset (code (list "  (* num num)"
(br)
"  (display \"finished doing math\")"
(br)
"  (display \"reached end of procedure squre\")"
(br)
"  (dispayy \"testing\")"
(br)
"  (display \"longish\")"
(br)
"  (display \"procedure\")"
(br)
"  (display \"body\"))"
(br)
)))
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock1-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))                (body (h3 "Procedure:  (define (addFour num1 num2 num3 num4)")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                      (fieldset (code (list "  (display \"this is a string to be printed\")"
(br)
"  (+ num1 num2 num3 num4))"
(br)
)))
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock2-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))                (body (h3 "Procedure:  (define five")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                      (fieldset (code (list "     5)"
(br)
)))
                     )))))
    (send/suspend/dispatch response-generator)))


;;help page
(define (help-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc")                          (link ((rel "stylesheet")                                 (href "/test-static.css")                                 (type "text/css"))))               (body (h1 "Help")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (p "For more information, run the project and select the About Me option")
                     )))))
    (send/suspend/dispatch response-generator)))





(static-files-path "../htdocs")

;(serve/servlet start
;               #:listen-ip "127.0.0.1"
;               ;#:port 8080
;               #:servlet-path "/")
