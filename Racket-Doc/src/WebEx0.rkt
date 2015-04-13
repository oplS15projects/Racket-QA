#lang web-server/insta
(require racket/gui)

(define (start request)
  (page-one request))

; "page" 1
(define (page-one request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "file_name.rkt")
                     (a ((href ,(embed/url phase-2)))
                        "Next->")))))]
    (send/suspend/dispatch response-generator)))
 
; "page" 2
(define (phase-2 request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "file_name2.rkt")
                     (a ((href ,(embed/url phase-3)))
                        "Next->")))))]
    (send/suspend/dispatch response-generator)))

; phase-3: request -> response
(define (phase-3 request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Racket-Doc"))
               (body (h1 "Test.rkt")
                     (a ((href ,(embed/url page-one)))
                        "<-Beginning")
                     (br)(br)
                     (p "--------------------------------------------------------------------")
                      (b "(addTwo num1 num2)")
                      (p (i "@param num1") ": first parameter"
                         (br)
                         (i "@param num2") ": is another parameter that is added"
                         (br)
                         (i "@return ") ": returns the sum of the two numbers")
                      (p "--------------------------------------------------------------------")
                      (br)
                      (br)
                      (br)
                      (br)
                      (p "--------------------------------------------------------------------")
                      (b "(apply op nums)")
                      (p (i "@param op") ": Expects a operation to be applied to the parameters"
                         (br)
                         (i "@param nums") ": A list of number values."
                         (br)
                         (i "@return ") ": returns the solution.")  
                      (p "--------------------------------------------------------------------")

                     ))))]
    (send/suspend/dispatch response-generator)))


