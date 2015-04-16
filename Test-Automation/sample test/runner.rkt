#lang racket

(require setup/dirs)


(define RACKET-PATH 
  (string-append (path->string (find-console-bin-dir))
                 (cond ((eq? (system-type) 'windows) "racket.exe")
                       ((eq? (system-type) 'unix) "")
                       ((eq? (system-type) 'macosx) "")
                       (else (error "Platform not supported")))))
         



(system (string-append "\"" RACKET-PATH "\" \"" file "\""))

(system (string-append "\""
                       RACKET-PATH
                       "\" \""
                       SAMPLE-TEST-DIR
                       "notepad.rkt\"")) 

(system (string-append "\""
                       RACKET-PATH
                       "\" \""
                       SAMPLE-TEST-DIR
                       "dir.rkt\""))
