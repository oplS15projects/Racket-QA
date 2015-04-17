

#lang racket

(require racket/file)

(define input (open-input-file "Test.rkt"))

(define thisLine "")

(define requireLst '())
(define provideLst '())
(define procHeaderLst '())
(define docTextLst '())

(define (each-line file)
  (set! thisLine (read-line input))
  (cond ( (eof-object? thisLine)
          (display "\n")
        )
        (else
         ;(display thisLine)
         (req? thisLine)
         ;(display "")
         ;(display "\n")
         ;(display thisLine)
         (each-line file)
        )
  )
)


(define (req? line)
  (cond ((equal? (regexp-match #rx"(?<=require).*" line) #f)
         ;(display "#f\n")
         )
        (else
         (display "#t\n")
         (cons line requireLst)
        )))
        


(define (header? line)
  (cond ((equal? (regexp-match #rx"(?<=define).*" line) #f)
         #f
        )
        (else
         (if (equal? (substring (car (regexp-match #rx"(?<=define).*" line)) 1 2) "(")
             (display (car (regexp-match #rx"(?<=define).*" line)))
             (display (car (regexp-match #rx"(?<=define).*[^)]" line)))
         )
         (display "\n")
        )
  )
)

(define (remove-lead-whitespace line)
  (regexp-match #rx"[^ \b].*" line));car it
  
(define (beginning-doc? line)
  (if (and (equal? (substring line 0 1) "#")
           (equal? (substring line 1 2) "|")
           (equal? (substring line 2 3) "|")
      )
      #t
      #f))

(define (end-doc? line)
  (if (and (equal? (substring line 0 1) "|")
               (equal? (substring line 1 2) "#")
          )
      #t
      #f
  )
) 

(define (remove-docChar line)
  (substring line 1 (- (string-length line) 1)))

(define (token-type tokenType line)
  (if (equal? (regexp-match tokenType line))
      #t
      #f
  )
)



(each-line input)
;(car (remove-lead-whitespace " | This function adds four numbers together."))
;(define tl "| This function adds four numbers together.")
;(substring tl 1 (string-length tl))
;(beginning-doc? (car (remove-lead-whitespace "  #This function adds four numbers together.")))

