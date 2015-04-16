#lang racket

(define (search)
  (rkt-source? (find-files path?
              "/home/james/OplFiles/Racket-QA/Racket-Doc/tests/testDir"
           ) '()
  )
)

(define (rkt-source? mypathList resultList)
  (cond ( (null? mypathList)
          (display "\n\n")
          resultList
        )
        ( (equal? (filename-extension (car mypathList)) #"rkt" ) ;comparing byte-string
          (display "\nAdding file")
          (display (car mypathList))
          (rkt-source? (cdr mypathList) (cons (car mypathList) resultList))
        )
        (else
         (display "\nSkipping ")
         (display (car mypathList))
         (rkt-source? (cdr mypathList) resultList)
        )
  )
)
  



;;exe---------
(search)