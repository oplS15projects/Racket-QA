;;http://docs.racket-lang.org/scribble-pp/html-html.html
;http://docs.racket-lang.org/scribble/index.html


#lang web-server/insta

(define testlist (list "1" "2" "3"))

(define (start req)
  (match (request->basic-credentials req)
    [(cons user pass)
     (response/xexpr
      `(html (head (title "Basic Auth Test"))
             (body (h1 "User: " ,(bytes->string/utf-8 user))
                   (h1 "Pass: " ,(bytes->string/utf-8 pass))
                   (code "(define (main num1 num2)")
                   (br)
                   (code "(+ num1 num2))")
                   (br)
                   (var "int")
                   (br)
                   (button "OK")
                   (col "red")
                   (br)
                   (textarea "test case")
                   (br)
                   (fieldset (b "test case 2" (list (br) "1" (br) "2" (br) "3") (br) "test 2.2"))
                   (br)
                   (fieldset (b "another test" (car testlist) ))
                   (br)
                   (site-navbar "navbar"))))]
    [else
     (response
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list
       (make-basic-auth-header
        (format "Basic Auth Test: ~a" (gensym))))
      void)]))

#|
#lang racket/gui

(define frame (new frame% [label "Test"] [width 300] 
                                         [height 300]))
(define text (new text%))
(define canvas (new editor-canvas% [parent frame] 
                                   [editor text]))

(define style-delta (make-object style-delta% 
                                 'change-normal-color))

;; do some red
(send style-delta set-delta-foreground "red")
(send text change-style style-delta)
(send text insert "Hello world in red\n")

;; do some blue
(send style-delta set-delta-foreground "blue")
(send text change-style style-delta)
(send text insert "Now available in blue")

(send frame show #t)|#