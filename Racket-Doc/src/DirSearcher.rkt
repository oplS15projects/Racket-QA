#lang racket

(provide search)

(define (search locationStr)
  (rkt-source? (find-files path?
              locationStr
           ) '()
  )
)

(define (rkt-source? mypathList resultList)
  (cond ( (null? mypathList)
          ;(display "\n\n")
          resultList
        )
        ( (equal? (filename-extension (car mypathList)) #"rkt" ) ;comparing byte-string
          ;(display "\nAdding file")
          ;(display (car mypathList))
          (rkt-source? (cdr mypathList) (cons (car mypathList) resultList))
        )
        (else
         ;(display "\nSkipping ")
         ;(display (car mypathList))
         (rkt-source? (cdr mypathList) resultList)
        )
  )
)
  



;;exe---------
;(search)
