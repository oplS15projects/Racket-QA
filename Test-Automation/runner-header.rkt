#lang racket

(require setup/dirs)

(define RACKET-PATH 
  (string-append (path->string (find-console-bin-dir))
                 (cond ((eq? (system-type) 'windows) "racket.exe")
                       ((eq? (system-type) 'unix) "/racket")
                       ((eq? (system-type) 'macosx) "/racket")
                       (else (error "Platform not supported")))))


