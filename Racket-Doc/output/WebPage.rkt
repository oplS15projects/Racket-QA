;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  								;;
;; AUTO-GENERATED CODE.  To run, either open in Dr. Racket and  ;;
;; select the "Run" button, or open a terminal, go to the 	;;
;; directory containing this file, and run			;;
;; "racket [file_name].rkt".					;;
;;                                                              ;;
;; Date Generated On: 4/17/2015                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#lang racket

(require web-server/servlet web-server/servlet-env)
(require racket/gui)


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
             `(html (head (title "Racket-Doc"))
               (body (h1 "*.rkt Files")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (p (b "Files:"))
                     (a ((href, (embed/url file_name_1-page))) "file_name_1")
                     (br)(br)
                     (a ((href, (embed/url file_name_1-page))) "file_name_2")
                     (br)(br)
                     (a ((href, (embed/url file_name_1-page))) "file_name_3")
                     (br)(br)
                     (a ((href, (embed/url file_name_1-page))) "file_name_4")
                     (br)(br)
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for a specified file
(define (file_name_1-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "*.rkt Files")
                     (center
                      (a ((href ,(embed/url main-page))) "Home")
                      (html nbsp nbsp nbsp nbsp)
                      (a ((href ,(embed/url file_name_2-page))) "-->")
                      )
                     (br)(br)
                     (p "Specified File page")
                     ;add requires
                     (b "Required")
                     (fieldset (code (list "req_1" (br) "req_3" (br) "req_5" (br) "req_7" (br) )))
                     (br) (br) (br)
                     ;add included
                     (b "Included")
                     (fieldset (code (list "incl_1" (br) "incl_3" (br) "incl_5" (br) "incl_7" (br) )))
                     (br) (br) (br)
                     ;add provided
                     (b "Provided")
                     (fieldset (code (list "prov_1" (br) "prov_3" (br) "prov_5" (br) "prov_7" (br) )))
                     (br) (br) (br)
                     ;;add procs and data
                     (b "Procedures & Data")
                     (fieldset
                      (code (list (b "proc_1") (br)
                             (i "blockComment_1") (br))))
                     (a ((href, (embed/url codeblock0-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_3") (br)
                             (i "blockComment_3") (br))))
                     (a ((href, (embed/url codeblock1-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_5") (br)
                             (i "blockComment_5") (br))))
                     (a ((href, (embed/url codeblock2-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_7") (br)
                             (i "procBody_7") (br))))
                     (a ((href, (embed/url codeblock3-page))) "Code")
                             (br) (br) (br)
                         )))))
    (send/suspend/dispatch response-generator)))


;;page for a specified file
(define (file_name_2-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "*.rkt Files")
                     (center
                      (a ((href ,(embed/url file_name_1-page))) "<--")
                      (html nbsp nbsp nbsp nbsp)
                      (a ((href ,(embed/url main-page))) "Home")
                      (html nbsp nbsp nbsp nbsp)
                      (a ((href ,(embed/url file_name_3-page))) "-->")
                      )
                     (br)(br)
                     (p "Specified File page")
                     ;add requires
                     (b "Required")
                     (fieldset (code (list "req_2" (br) "req_4" (br) "req_6" (br) "req_8" (br) )))
                     (br) (br) (br)
                     ;add included
                     (b "Included")
                     (fieldset (code (list "incl_2" (br) "incl_4" (br) "incl_6" (br) "incl_8" (br) )))
                     (br) (br) (br)
                     ;add provided
                     (b "Provided")
                     (fieldset (code (list "prov_2" (br) "prov_4" (br) "prov_6" (br) "prov_8" (br) )))
                     (br) (br) (br)
                     ;;add procs and data
                     (b "Procedures & Data")
                     (fieldset
                      (code (list (b "proc_2") (br)
                             (i "blockComment_2") (br))))
                     (a ((href, (embed/url codeblock0-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_4") (br)
                             (i "blockComment_4") (br))))
                     (a ((href, (embed/url codeblock1-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_6") (br)
                             (i "blockComment_6") (br))))
                     (a ((href, (embed/url codeblock2-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_8") (br)
                             (i "procBody_8") (br))))
                     (a ((href, (embed/url codeblock3-page))) "Code")
                             (br) (br) (br)
                         )))))
    (send/suspend/dispatch response-generator)))


;;page for a specified file
(define (file_name_3-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "*.rkt Files")
                     (center
                      (a ((href ,(embed/url file_name_2-page))) "<--")
                      (html nbsp nbsp nbsp nbsp)
                      (a ((href ,(embed/url main-page))) "Home")
                      (html nbsp nbsp nbsp nbsp)
                      (a ((href ,(embed/url file_name_4-page))) "-->")
                      )
                     (br)(br)
                     (p "Specified File page")
                     ;add requires
                     (b "Required")
                     (fieldset (code (list "req_11" (br) "req_33" (br) "req_55" (br) "req_77" (br) )))
                     (br) (br) (br)
                     ;add included
                     (b "Included")
                     (fieldset (code (list "incl_11" (br) "incl_33" (br) "incl_55" (br) "incl_77" (br) )))
                     (br) (br) (br)
                     ;add provided
                     (b "Provided")
                     (fieldset (code (list "prov_11" (br) "prov_33" (br) "prov_55" (br) "prov_77" (br) )))
                     (br) (br) (br)
                     ;;add procs and data
                     (b "Procedures & Data")
                     (fieldset
                      (code (list (b "proc_11") (br)
                             (i "blockComment_11") (br))))
                     (a ((href, (embed/url codeblock0-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_33") (br)
                             (i "blockComment_33") (br))))
                     (a ((href, (embed/url codeblock1-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_55") (br)
                             (i "blockComment_55") (br))))
                     (a ((href, (embed/url codeblock2-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_77") (br)
                             (i "procBody_77") (br))))
                     (a ((href, (embed/url codeblock3-page))) "Code")
                             (br) (br) (br)
                         )))))
    (send/suspend/dispatch response-generator)))


;;page for a specified file
(define (file_name_4-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "*.rkt Files")
                     (center
                      (a ((href ,(embed/url file_name_3-page))) "<--")
                      (html nbsp nbsp nbsp nbsp)
                      (a ((href ,(embed/url main-page))) "Home")
                      (html nbsp nbsp nbsp nbsp)
                      )
                     (br)(br)
                     (p "Specified File page")
                     ;add requires
                     (b "Required")
                     (fieldset (code (list "req_22" (br) "req_44" (br) "req_66" (br) "req_88" (br) )))
                     (br) (br) (br)
                     ;add included
                     (b "Included")
                     (fieldset (code (list "incl_22" (br) "incl_44" (br) "incl_66" (br) "incl_88" (br) )))
                     (br) (br) (br)
                     ;add provided
                     (b "Provided")
                     (fieldset (code (list "prov_22" (br) "prov_44" (br) "prov_66" (br) "prov_88" (br) )))
                     (br) (br) (br)
                     ;;add procs and data
                     (b "Procedures & Data")
                     (fieldset
                      (code (list (b "proc_22") (br)
                             (i "blockComment_22") (br))))
                     (a ((href, (embed/url codeblock0-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_44") (br)
                             (i "blockComment_44") (br))))
                     (a ((href, (embed/url codeblock1-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_66") (br)
                             (i "blockComment_66") (br))))
                     (a ((href, (embed/url codeblock2-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_88") (br)
                             (i "procBody_88") (br))))
                     (a ((href, (embed/url codeblock3-page))) "Code")
                             (br) (br) (br)
                         )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying dependencies
(define (required-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Dependencies")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (br)(br)
                     (b "Required")
                     (fieldset (code (list "req_1" (br) "req_2" (br) "req_3" (br) "req_4" (br) "req_5" (br) "req_6" (br) "req_7" (br) "req_8" (br) )))
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying provideds
(define (provided-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Provided")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     (b "Provided")
                     (fieldset (code (list "prov_1" (br) "prov_2" (br) "prov_3" (br) "prov_4" (br) "prov_5" (br) "prov_6" (br) "prov_7" (br) "prov_8" (br) )))
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying procs and data of a single file
(define (procAndData-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedures & Data")
                     (center (a ((href ,(embed/url main-page))) "Home"))
                     ;;add procs and data
                     (br) (br)
                     (fieldset
                      (code (list (b "proc_1") (br)
                             (i "blockComment_1") (br))))
                     (a ((href, (embed/url codeblock0-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_2") (br)
                             (i "blockComment_2") (br))))
                     (a ((href, (embed/url codeblock1-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_3") (br)
                             (i "blockComment_3") (br))))
                     (a ((href, (embed/url codeblock2-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_4") (br)
                             (i "blockComment_4") (br))))
                     (a ((href, (embed/url codeblock3-page))) "Code")
                             (br) (br) (br)
                     (fieldset
                      (code (list (b "proc_5") (br)
                             (i "blockComment_5") (br))))
                     (a ((href, (embed/url codeblock4-page))) "Code")
                             (br) (br) (br)
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock0-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "procBody_1")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock1-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "procBody_2")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock2-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "procBody_3")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock3-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "procBody_4")
                     )))))
    (send/suspend/dispatch response-generator)))


;;page for displaying a procedure body.;;help page
(define (codeblock4-page request)
  (local ((define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Procedure")
                     (center (a ((href ,(embed/url fileNameList-page))) "<--Back"))
                     (br)(br)
                     (p "procBody_5")
                     )))))
    (send/suspend/dispatch response-generator)))


;;help page
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
