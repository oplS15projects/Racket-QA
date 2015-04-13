#lang web-server/insta
 
(define (start initial-request)
  (define counter1 (make-counter))
  (define counter2 (make-counter))
  (define include1 (include-counter counter1))
  (define include2 (include-counter counter2))
  (send/suspend/dispatch
   (lambda (embed/url)
     (response/xexpr
      `(html
        (body (h2 "Double Counters")
              (div (h3 "First")
                   ,(include1 embed/url))
              (div (h3 "Second")
                   ,(include2 embed/url))))))))
 
(define (make-counter)
 (make-web-cell 0))
 
(define (include-counter a-counter)
 (call-with-current-continuation
  (λ (k)
    (let loop ()
      (k
       (lambda (embed/url)
         `(div (h3 ,(number->string (web-cell-ref a-counter)))
               (a ([href
                    ,(embed/url
                      (lambda _
 
                        (define last (web-cell-ref a-counter))
 
                        (web-cell-shadow a-counter (add1 last))
 
                        (loop)))])
                  "+"))))))
  servlet-prompt))