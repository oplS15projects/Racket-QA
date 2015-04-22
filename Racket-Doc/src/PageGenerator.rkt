;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; File: PageGenerator.rkt                                      ;;
;; Author: James Kuczynski                                      ;;
;; Email: jkuczyns@cs.uml.edu                                   ;;
;; File Description: This file generates executable .rkt files  ;;
;;                   which launch the web server.               ;;
;;Created 04/14/2015                                            ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/file)
(require racket/date)

(provide generationMaster)
;;begin test---------------


#|
(define file1 (list "file_name_1"
                    #t ;;has corrisponding documentation block?
                    '("req_1" "req_3" "req_5" "req_7")
                    '("incl_1" "incl_3" "incl_5" "incl_7")
                    '("prov_1" "prov_3" "prov_5" "prov_7")
                    '("proc_1" "proc_3" "proc_5" "proc_7")
                    '("procBody_1" "procBody_3" "procBody_5" "procBody_7")
                    '("blockComment_1" "blockComment_3" "blockComment_5" "procBody_7"))
)

(define file2 (list "file_name_2"
                    #t ;;has corrisponding documentation block?
                    '("req_2" "req_4" "req_6" "req_8")
                    '("incl_2" "incl_4" "incl_6" "incl_8")
                    '("prov_2" "prov_4" "prov_6" "prov_8")
                    '("proc_2" "proc_4" "proc_6" "proc_8")
                    '("procBody_2" "procBody_4" "procBody_6" "procBody_8")
                    '("blockComment_2" "blockComment_4" "blockComment_6" "procBody_8")))
  
  
(define file3 (list "file_name_3"
                    #t ;;has corrisponding documentation block?
                    '("req_11" "req_33" "req_55" "req_77")
                    '("incl_11" "incl_33" "incl_55" "incl_77")
                    '("prov_11" "prov_33" "prov_55" "prov_77")
                    '("proc_11" "proc_33" "proc_55" "proc_77")
                    '("procBody_11" "procBody_33" "procBody_55" "procBody_77")
                    '("blockComment_11" "blockComment_33" "blockComment_55" "procBody_77")))
  

(define file4 (list "file_name_4" 
                    #t ;;has corrisponding documentation block?
                    '("req_22" "req_44" "req_66" "req_88")
                    '("incl_22" "incl_44" "incl_66" "incl_88")
                    '("prov_22" "prov_44" "prov_66" "prov_88")
                    '("proc_22" "proc_44" "proc_66" "proc_88")
                    '("procBody_22" "procBody_44" "procBody_66" "procBody_88")
                    '("blockComment_22" "blockComment_44" "blockComment_66" "procBody_88")))
|#

;;end test-----------------
(define currentDateStr "")

(let ((date (seconds->date (current-seconds))))
  (set! currentDateStr (string-append 
                        (string-append
                         (string-append
                          (string-append
                           (string-append currentDateStr (number->string (date-month date)))
                           "/")
                          (number->string (date-day date)))
                         "/")
                        (number->string (date-year date)))
  )
)

(define output (open-output-file "./../output/WebPage.rkt"
                                 #:mode 'text
                                 #:exists 'replace))

(define (generationMaster fileList fileNameList reqList inclList provList procList procBodyList docList)
  (display "fine (1)\n")
  (generateFileHeader)
  (display "fine (2)\n")
  (generateMainPage)
  (display "fine (3)\n")
  (generatefileNameListPage fileNameList)
  (display "fine (4)\n")
  (generateSpecifiedFile fileList (cons "PLACE_HOLDER" fileNameList))
  (display "fine (5)\n")
  (generateRequiresPage reqList)
  (display "fine (6)\n")
  (generateProvidesPage provList)
  (display "fine (7)\n")
  ;(display (length procList))
  (display "\n")
  ;(display (length docList))
  (generateProcHeaderPage procList docList)
  (display "fine (8)\n")
  ;(display procBodyList)
  (generateProcBodyPages procBodyList)
  (display "fine (9)\n")
  (generateHelpPage)
)

;generate documentation header
(define (generateFileHeader)
  (write-string ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n" output)
  (write-string ";;  								;;\n" output)
  (write-string ";; AUTO-GENERATED CODE.  To run, either open in Dr. Racket and  ;;\n" output)
  (write-string ";; select the \"Run\" button, or open a terminal, go to the 	;;\n" output)
  (write-string ";; directory containing this file, and run			;;\n" output)
  (write-string ";; \"racket [file_name].rkt\".					;;\n" output)
  (write-string ";;                                                              ;;\n" output)
  (write-string ";; Date Generated On: " output)
  (write-string currentDateStr output)
  (write-string "                                 ;;\n" output)
  (write-string ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n" output)
  (write-string "\n\n\n" output)
)


;generate static main page
(define (generateMainPage)
  (write-string "#lang racket\n\n" output)
  (write-string "(require web-server/servlet web-server/servlet-env)\n" output)
  (write-string "(require racket/gui)\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
  (write-string ";(define generat\n" output)
  (write-string "\n" output)
  (write-string ";(play-sound \"./share/rach.wav\" #f)\n" output)
  (write-string "\n" output)
  (write-string ";(define logo\n" output)
  (write-string ";  (read-bitmap \"./../share/button.jpg\"))\n" output)
  (write-string "\n" output)
  (write-string "(define (start request)\n" output)
  (write-string "  (main-page request))\n" output)
  (write-string "\n" output)
  (write-string ";\"main\" page:\n" output)
  (write-string "(define (main-page request)\n" output)
  (write-string "  (local ((define (response-generator embed/url)\n" output)
  (write-string "            (response/xexpr\n" output)
  (write-string "             `(html (head (title \"Racket-Doc\"))\n" output)
  (write-string "                    (body (h1 (center \"Racket-Doc Home\"))\n" output)
  (write-string "                          (center\n" output)
  (write-string "                          (a ((href, (embed/url fileNameList-page))) \"File List\")\n" output)
  (write-string "                          (html nbsp nbsp nbsp nbsp)\n" output)      
  (write-string "                          (a ((href, (embed/url required-page))) \"Required\")\n" output)
  (write-string "                          (html nbsp nbsp nbsp nbsp)\n" output)
  (write-string "                          (a ((href, (embed/url provided-page))) \"Provided\")\n" output)
  (write-string "                          (html nbsp nbsp nbsp nbsp)\n" output)
  (write-string "                          (a ((href, (embed/url procAndData-page))) \"Procedures & Data\")\n" output)
  (write-string "                          (html nbsp nbsp nbsp nbsp)\n" output)
  (write-string "                          (a ((href, (embed/url help-page))) \"Help\"))\n" output)
  (write-string "                          )))))\n" output)
  (write-string "    (send/suspend/dispatch response-generator)))\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
)
 

;; generate code for the source file index
(define (generatefileNameListPage fileNameList)
  (write-string ";;page for displaying file list\n" output)
  (write-string "(define (fileNameList-page request)\n" output)
  (write-string "  (local ((define (response-generator embed/url)\n" output)
  (write-string "            (response/xexpr\n" output)
  (write-string "             `(html (head (title \"Racket-Doc\"))\n" output)
  (write-string "               (body (h1 \"*.rkt Files\")\n" output)
  (write-string "                     (center (a ((href ,(embed/url main-page))) \"Home\"))\n" output)
  (write-string "                     (br)(br)\n" output)
  (write-string "                     (p (b \"Files:\"))\n" output)
  (define (fileNameLooper lst)
    (cond ( (null? lst)
            (display "")
          )
          (else
           (write-string"                     (a ((href, (embed/url " output)
           (write-string (car lst) output)
           (write-string "-page))) \"" output)
           (write-string (car lst) output)
           (write-string "\")\n" output)
           (write-string "                     (br)(br)\n" output)
           (fileNameLooper (cdr lst))
          )
    )
  )
  (fileNameLooper fileNameList)  
  (write-string "                     )))))\n" output)
  (write-string "    (send/suspend/dispatch response-generator)))\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
)


;;generate code to create a page designated for each source file.
(define isFirstItFlag #t)

(define (generateSpecifiedFile fileList fileNameList)
  (define (fileLooper fileList fileNameList)
    (cond ( (null? fileList)
            (display "")
          )
          (else
           (write-string ";;page for a specified file\n" output);;begin---------------
           (write-string "(define (" output)
           (write-string (car (car fileList)) output) ;; 
           (write-string "-page" output)
           (write-string " request)\n" output)
           (write-string "  (local ((define (response-generator embed/url)\n" output)
           (write-string "            (response/xexpr\n" output)
           (write-string "             `(html (head (title \"Racket-Doc\"))\n" output)
           (write-string "               (body (h1 \"*.rkt Files\")\n" output)
           (write-string "                     (center\n" output)
           (cond ( (equal? isFirstItFlag #t)
                   ;(display isFirstItFlag)
                   (set! isFirstItFlag #f)
                 )
                 (else
                  ;(display isFirstItFlag)
                  (write-string "                      (a ((href ,(embed/url " output)
                  (write-string (car fileNameList) output)
                  (write-string "-page))) \"<--\")\n" output)
                  (write-string "                      (html nbsp nbsp nbsp nbsp)\n" output)
                 )
           )
           (write-string "                      (a ((href ,(embed/url main-page))) \"Home\")\n" output)
           (write-string "                      (html nbsp nbsp nbsp nbsp)\n" output)
           (cond ( (= (length fileList) 1)
                   (display "")
                 )
                 (else
                  (write-string "                      (a ((href ,(embed/url " output)
                  (write-string (car (cdr (cdr fileNameList))) output)
                  (write-string "-page))) \"-->\")\n" output)
                 )
           )
           (write-string "                      )\n" output)
           (write-string "                     (br)(br)\n" output)
           (write-string "                     (p \"Specified File page\")\n" output)
           (write-string "                     ;add requires\n" output)
           (write-string "                     (b \"Required\")\n" output)
           (write-string "                     (fieldset (code (list " output)
           (display "")
           (reqLooper (car (cdr (cdr (car fileList)))))
           (write-string ")))\n" output)
           (write-string "                     (br) (br) (br)\n" output)
           (write-string "                     ;add included\n" output)
           (write-string "                     (b \"Included\")\n" output)
           (write-string "                     (fieldset (code (list " output)
           (define (inclLooper lst)
             (cond ( (null? lst)
                     (display "")
                     )
                   (else
                    (write-string "\"" output)
                    (write-string (car lst) output)
                    (write-string "\"" output)
                    (write-string " (br) " output)
                    (inclLooper (cdr lst))
                   )
              )
            )  
           (inclLooper (car (cdr (cdr (cdr (car fileList))))))
           (write-string ")))\n" output)
           (write-string "                     (br) (br) (br)\n" output)
           (write-string "                     ;add provided\n" output);;sub-begin---------------------
           (write-string "                     (b \"Provided\")\n" output)
           (write-string "                     (fieldset (code (list " output)
           (provLooper (car (cdr (cdr (cdr (cdr (car fileList)))))))
           (write-string ")))\n" output)
           (write-string "                     (br) (br) (br)\n" output);;sub-end------------
           (write-string "                     ;;add procs and data\n" output);;sub-begin-----------------
           (write-string "                     (b \"Procedures & Data\")\n" output)
           ;(display "\n\n\n%%%%%%%%%%%%%%%%\n\n")
           ;(display (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (car fileList))))))))))
           ;(display "\n%%%%%%%%%%%%%%%%\n\n")
           (procLooper
            (car (cdr (cdr (cdr (cdr (cdr (car fileList)))))))
            (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (car fileList)))))))))
            0)
           (write-string "                         )))))\n" output)  
           (write-string "    (send/suspend/dispatch response-generator)))" output)
           (write-string "\n\n\n" output)
           (fileLooper (cdr fileList) (cdr fileNameList))
          )
        )
    )
  (fileLooper fileList fileNameList)
)
  
  


;;  
(define (generateRequiresPage reqList)
  (write-string ";;page for displaying dependencies\n" output);;begin----------
  (write-string "(define (required-page request)\n" output)
  (write-string "  (local ((define (response-generator embed/url)\n" output)
  (write-string "            (response/xexpr\n" output)
  (write-string "             `(html (head (title \"Racket-Doc\"))\n" output)
  (write-string "               (body (h1 \"Dependencies\")\n" output)
  (write-string "                     (center (a ((href ,(embed/url main-page))) \"Home\"))\n" output)
  (write-string "                     (br)(br)\n" output)
  (write-string "                     (b \"Required\")\n" output)                  
  (write-string "                     (fieldset (code (list " output)
  (reqLooper reqList)
  (write-string ")))\n" output)
  (write-string "                     )))))\n" output)
  (write-string "    (send/suspend/dispatch response-generator)))\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
  (write-string "\n\n\n");;end-----------
)
  

;;Generate the joint "provides" list page
(define (generateProvidesPage provList)
  (write-string ";;page for displaying provideds\n" output);;begin---------
  (write-string "(define (provided-page request)\n" output)
  (write-string "  (local ((define (response-generator embed/url)\n" output)
  (write-string "            (response/xexpr\n" output)
  (write-string "             `(html (head (title \"Racket-Doc\"))\n" output)
  (write-string "               (body (h1 \"Provided\")\n" output)
  (write-string "                     (center (a ((href ,(embed/url main-page))) \"Home\"))\n" output)
  (write-string "                     (b \"Provided\")\n" output)
  (write-string "                     (fieldset (code (list " output)
  (provLooper provList)
  (write-string ")))\n" output)
  (write-string "                     )))))\n" output)
  (write-string "    (send/suspend/dispatch response-generator)))\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
  (write-string "\n\n\n");;end-----------
)
  

;;generate procs & data page
(define (generateProcHeaderPage procList docList)
  (write-string ";;page for displaying procs and data of a single file\n" output);;begin---------
  (write-string "(define (procAndData-page request)\n" output)
  (write-string "  (local ((define (response-generator embed/url)\n" output)
  (write-string "            (response/xexpr\n" output)
  (write-string "             `(html (head (title \"Racket-Doc\"))\n" output)
  (write-string "               (body (h1 \"Procedures & Data\")\n" output)
  (write-string "                     (center (a ((href ,(embed/url main-page))) \"Home\"))\n" output)
  (write-string "                     ;;add procs and data\n" output)
  (write-string "                     (br) (br)\n" output)
  (procLooper procList docList 0)
  (write-string "                     )))))\n" output)
  (write-string "    (send/suspend/dispatch response-generator)))\n" output)
  (write-string "\n" output)
  (write-string "\n" output);;end
)
  
(define globalCounter 0)

;;generate procedure body pages (each will be named "codeblock[number]-page")
(define (generateProcBodyPages procBodyList)
  ;(display "******This is the list of proc bodies*******\n")
  ;(display procBodyList)
  ;(display "\n\n\n")
  (define (bodyLooper lst count)
    (cond ( (null? lst)
            (display "")
          )
          (else
           (write-string ";;page for displaying a procedure body." output)
           (write-string ";;help page\n" output)
           (write-string "(define (codeblock" output)
           (write-string (number->string count) output)
           (write-string "-page" output)
           (write-string " request)\n" output)
           (write-string "  (local ((define (response-generator embed/url)\n" output)
           (write-string "            (response/xexpr\n" output)
           (write-string "             `(html (head (title \"Racket-Doc\"))\n" output)
           (write-string "               (body (h1 \"Procedure" output)
           ;(write-string (car lst) output)
           (write-string "\")\n" output)
           (write-string "                     (center (a ((href ,(embed/url fileNameList-page))) \"<--Back\"))\n" output)
           (write-string "                     (br)(br)\n" output)
           (write-string "                     (p \"" output)
           (write-string (car lst) output)
           (write-string "\")\n" output)
           (write-string "                     )))))\n" output)
           (write-string "    (send/suspend/dispatch response-generator)))\n" output)
           (write-string "\n" output)
           (write-string "\n" output)
           (set! globalCounter (+ globalCounter 1))
           (bodyLooper (cdr lst) globalCounter)
          )
     )
  )
  (bodyLooper procBodyList globalCounter)
)
  
;;generate help page
(define (generateHelpPage)
  (write-string ";;help page\n" output)
  (write-string "(define (help-page request)\n" output)
  (write-string "  (local ((define (response-generator embed/url)\n" output)
  (write-string "            (response/xexpr\n" output)
  (write-string "             `(html (head (title \"Racket-Doc\"))\n" output)
  (write-string "               (body (h1 \"Help\")\n" output)
  (write-string "                     (center (a ((href ,(embed/url main-page))) \"Home\"))\n" output)
  (write-string "                     (br)(br)\n" output)
  (write-string "                     (p \"help page...\")\n" output)
  (write-string "                     )))))\n" output)
  (write-string "    (send/suspend/dispatch response-generator)))\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
  (write-string "\n" output)
  (write-string "(serve/servlet start\n" output)
  (write-string "               #:listen-ip \"127.0.0.1\"\n" output)
  (write-string "               ;#:port 8080\n" output)
  (write-string "               #:servlet-path \"/\")\n" output)
  (close-output-port output)
)

;;-------------------------------------------------------------------------------------------

(define (reqLooper lst)
  (cond ( (null? lst)
          (display "")
        )
        (else
         (write-string "\"" output)
         (write-string (car lst) output)
         (write-string "\"" output)
         (write-string " (br) " output)
         (reqLooper (cdr lst))
        )
  )
) 


(define (provLooper lst)
  (cond ( (null? lst)
          (display "")
        )
        (else
         (write-string "\"" output)
         (write-string (car lst) output)
         (write-string "\"" output)
         (write-string " (br) " output)
         (provLooper (cdr lst))
        )
  )
) 

(define gollum 0)

(define (procLooper pLst dLst count)
  (display "!!!!!!!!!!!!!!!!!!:")
  (display gollum)
  (cond ( (or (null? pLst)
              (null? dLst)
          )
          (set! gollum 0)
          (display "")
          )
        (else
         (write-string "                     (fieldset\n" output)
         (write-string "                      (code (list (b \"" output)
         (write-string (car pLst) output)
         (write-string "\") (br)" output)
         (write-string "\n" output)
         (write-string "                             (i " output)
         (write-string "\"" output)
         (write-string (car dLst) output)
         (write-string "\") (br)" output)
         (write-string ")))" output)
         (write-string "\n                     (a ((href, (embed/url codeblock" output)
         (write-string (number->string count) output)
         (write-string "-page))) \"Code\")\n" output)
         (write-string "                             (br) (br) (br)\n" output)
         (set! gollum (+ gollum 1))
         (procLooper (cdr pLst) (cdr dLst) gollum)
        )
   )
)
;;exe-------------------
#|(generationMaster (list file1 file2 file3 file4)
                  '("file_name_1" "file_name_2" "file_name_3" "file_name_4")
                  '("req_1"  "req_2"  "req_3"  "req_4"  "req_5"  "req_6"  "req_7"  "req_8")
                  '("incl_1" "incl_2" "incl_3" "incl_4" "incl_5" "incl_6" "incl_7" "incl_8")
                  '("prov_1" "prov_2" "prov_3" "prov_4" "prov_5" "prov_6" "prov_7" "prov_8")
                  '("proc_1" "proc_2" "proc_3" "proc_4" "proc_5")
                  '("procBody_1" "procBody_2" "procBody_3" "procBody_4" "procBody_5")
                  '("blockComment_1" "blockComment_2" "blockComment_3" "blockComment_4" "blockComment_5"))|#
                  



