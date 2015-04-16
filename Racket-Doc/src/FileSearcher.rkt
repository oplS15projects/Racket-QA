#lang racket

(define (search)
  (rkt-source? (find-files path?
              "/home/james/OplFiles/Racket-QA/Racket-Doc/tests/testDir"
           ) '()
  )
)

(define (rkt-source? mypathList resultList)
  (cond ( (null? mypathList)
          resultList
        )
        ( (equal? (filename-extension (car mypathList)) #"rkt" ) ;comparing byte-string
          (display "adding file\n")
          (rkt-source? (cdr mypathList) (cons (car mypathList) resultList))
        )
        (else
         (display "not adding file: ")
         (display (filename-extension (car mypathList)))
         (display "\n")
         (rkt-source? (cdr mypathList) resultList)
        )
  )
)
  



;;exe---------
(search)