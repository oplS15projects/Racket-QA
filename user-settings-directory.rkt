;; **********************************************************************
;; * The set of APIs defined in this file defines a name and path to the user-
;; * specific settings directory for the Racket QA application and provides
;; * uniform ways to access the directory and the files in the directory
;; * by other modules of the application.
;; *
;; * The APIs defined in this module use following directories as the
;; * user-specific settings directory by the platform convention.
;; *
;; * %APPDATA%/Racket QA  (Windows)
;; * $HOME/.Racket_QA     (Linux)
;; *
;; * Users of this API need not concern about distinguishing
;; * the specific platform they are calling the procedures in. All
;; * procedures in this API set will automatically find out the OS type at
;; * runtime and use the correct directory.
;; *
;; * Moreover, users can use both / or \\ directory separators in the
;; * arguments to the procedures in this API regardless of the platform
;; * they are being called. The procedures will automatically use the
;; * correct separator for the platform type.
;; *
;; * ex)
;; * > (file-exists-in-settings-directory? "Addresses/master-address.dat")
;; * #t
;; * > (file-exists-in-settings-directory? "Addresses\\master-address.dat")
;; * #t
;; *
;; * These APIs do not provide a means to directly write or modify data.
;; * Instead, they provide path strings that can be used in conjunction
;; * with the standard Racket file I/O APIs.
;; *
;; * ex)
;; * > (define smtp-settings-file (full-file-path-in-settings-directory "smtp-settings.conf"))
;; * > smtp-settings-file
;; * "C:\\Users\\yongjec\\AppData\\Roaming\\Racket QA\\smtp-settings.conf"
;; * > (define out (open-output-file smtp-settings-file #:mode 'binary))
;; * > (fprintf out "~a\t~a\t~a~n" server username password)
;; *
;; * Include (require "user-settings-directory.rkt") to your source file
;; * to use the procedures in this API set. (might need reload)
;; **********************************************************************

#lang racket

(require racket/file
         racket/system)

(provide (except-out (all-defined-out)
                     SETTINGS-DIRECTORY-NAME-WINDOWS
                     SETTINGS-DIRECTORY-NAME-UNIX))


(define SETTINGS-DIRECTORY-NAME-WINDOWS "Racket QA")
(define SETTINGS-DIRECTORY-NAME-UNIX ".Racket_QA")


;; Returns the name of the user-specific settings directory
;; suitable for the platform convention.
;;
;; (settings-directory-name)
;; "Racket QA"                    ;(Windows)
;;
;; (settings-directory-name)
;; ".Racket_QA"                   ;(Linux)
;;
(define (settings-directory-name)
  (cond ((eq? (system-type) 'windows) SETTINGS-DIRECTORY-NAME-WINDOWS)
        ((eq? (system-type) 'unix) SETTINGS-DIRECTORY-NAME-UNIX)
        (else #f)))


;; Returns the full path of the user-specific settings directory
;; suitable for the platform convention.
;;
;; > (settings-directory-path)
;; "C:\\Users\\yongjec\\AppData\\Roaming\\Racket QA"  ;(Windows)
;;
;; > (settings-directory-path)
;; "/home/yongjec/.Racket_QA"                         ;(Liunx)
;;
(define (settings-directory-path)
  (cond ((eq? (system-type) 'windows)
         (string-append (getenv "APPDATA") "\\" SETTINGS-DIRECTORY-NAME-WINDOWS))
        ((eq? (system-type) 'unix)
         (string-append (getenv "HOME") "/" SETTINGS-DIRECTORY-NAME-UNIX))
        (else #f)))


;; Returns the full path of a file as if it is located in the
;; user-specific settings directory. You can provide this value to
;; the standard Racket file procedures to create or open a file
;; for read/write.
;;
;; > (full-file-path-in-settings-directory "a file you want the full path string.txt")
;; "C:\\Users\\yongjec\\AppData\\Roaming\\Racket QA\\a file you want the full path string.txt"  ;(Windows)
;; "/home/yongjec/.Racket_QA/a file you want the full path.txt"                                 ;(Linux)
;;
;; > (full-file-path-in-settings-directory "smtp-info.conf")
;; "C:\\Users\\yongjec\\AppData\\Roaming\\Racket QA\\smtp-info.conf"                            ;(Windows)
;; "/home/yongjec/.Racket_QA/smtp-info.conf"                                                    ;(Linux)
;;
;; > (full-file-path-in-settings-directory "dir/filename.txt")                             ;(Windows)
;; "C:\\Users\\yongjec\\AppData\\Roaming\\Racket QA\\dir\\filename.txt"
;; > (full-file-path-in-settings-directory "dir\\filename.txt")                            ;(Windows)
;; "C:\\Users\\yongjec\\AppData\\Roaming\\Racket QA\\dir\\filename.txt"
;;
;; > (full-file-path-in-settings-directory "dir/filename.txt")                             ;(Linux)
;; "/home/yongjec/.Racket_QA/dir/filename.txt"
;; > (full-file-path-in-settings-directory "dir\\filename.txt")                            ;(Linux)
;; "/home/yongjec/.Racket_QA/dir/filename.txt"
;;
;; (define smtp-settings-file (full-file-path-in-settings-directory "smtp-settings.conf"))
;; (define out (open-output-file full-file-path-in-settings-directory #:mode 'binary #:exists 'truncate/replace))
;; (fprintf out "~a\t~a\t~a~n" server username password)
;;
(define (full-file-path-in-settings-directory filename)
  (define cleansed-filename (cleanse-path-string filename))
  (cond ((eq? (system-type) 'windows)
         (string-append (settings-directory-path) "\\" cleansed-filename))
        ((eq? (system-type) 'unix)
         (string-append (settings-directory-path) "/" cleansed-filename))
        (else #f)))


;; Checks if the user-specific settings directory exists for the current user.
;; This will usually return #f when the application is run for the first
;; time for the machine/user.
;;
(define (settings-directory-exists?)
  (directory-exists? (settings-directory-path)))


;; Creates the user-specific settings directory whose path depends on
;; the platform convention as returned by (settings-directory-path).
;; This shouldn't need to be called more than once per machine/user.
;;
;; > (settings-directory-exists?)
;; #f
;;
;; > (create-settings-directory)
;; > (settings-directory-exists?)
;; #t
;;
;; > (settings-directory-path)
;; "C:\\Users\\yongjec\\AppData\\Roaming\\Racket QA"  ;(Windows)
;; "/home/yongjec/.Racket_QA"                         ;(Linux)
;;
(define (create-settings-directory)
  (when (not (directory-exists? (settings-directory-path)))
    (make-directory* (settings-directory-path))))


;; Checks if a file exists in the user-specific settings directory.
;;
;; > (file-exists-in-settings-directory? "some_file_that_is_not_there.txt")
;; #f
;;
;; > (file-exists-in-settings-directory? "smtp-login-info.conf")
;; #t
;;
;; > (file-exists-in-settings-directory? "Addresses/master-address.dat")   ;(Windows/Linux)
;; #t
;;
;; > (file-exists-in-settings-directory? "Addresses\\master-address.dat")  ;(Windows/Linux)
;; #t
;;
(define (file-exists-in-settings-directory? filename)
  (define cleansed-filename (cleanse-path-string filename))
  (file-exists? (full-file-path-in-settings-directory cleansed-filename)))


;; Checks if the specified sub-directory exists in the settings directory.
;;
;; > (directory-exists-in-settings-directory? "Addresses")
;; #f
;;
;; > (directory-exists-in-settings-directory? "Some Non-Existing Folder")
;; #f
;;
;; > (directory-exists-in-settings-directory? "Addresses/Boston Office")   ;(Windows/Linux)
;; #t
;;
;; > (directory-exists-in-settings-directory? "Addresses\\Boston Office")  ;(Windows/Linux)
;; #t
;;
(define (directory-exists-in-settings-directory? dirname)
  (define cleansed-dirname (cleanse-path-string dirname))
  (cond ((eq? (system-type) 'windows)
         (directory-exists? (string-append (settings-directory-path) "\\" cleansed-dirname)))
        ((eq? (system-type) 'unix)
         (directory-exists? (string-append (settings-directory-path) "/" cleansed-dirname)))
        (else #f)))


;; Create a sub-directory in the user-specific settings directory.
;;
;; > (directory-exists-in-settings-directory? "Addresses")
;; #f
;; > (make-directory-in-settings-directory "Addresses")
;; > (directory-exists-in-settings-directory? "Addresses")
;; #t
;;
;; > (make-directory-in-settings-directory "Addresses/Lowell Office")   ;(Windows/Linux)
;; > (make-directory-in-settings-directory "Addresses\\Lowell Office")  ;(Windows/Linux)
;;
(define (make-directory-in-settings-directory dirname)
  (define cleansed-dirname (cleanse-path-string dirname))
  (if (not (directory-exists-in-settings-directory? cleansed-dirname))
      (cond ((eq? (system-type) 'windows)
             (make-directory* (string-append (settings-directory-path) "\\" cleansed-dirname)))
            ((eq? (system-type) 'unix)
             (make-directory* (string-append (settings-directory-path) "/" cleansed-dirname)))
            (else #f))
      #f))


;; Deletes a file from the user-specific settings directory.
;;
;; > (file-exists-in-settings-directory? "unneeded_file.dat")
;; #t
;; > (delete-file-from-settings-directory "unneeded_file.dat")
;; > (file-exists-in-settings-directory? "unneeded_file.dat")
;; #f
;;
;; > (delete-file-from-settings-directory "Addresses/unneeded-addresses.dat")   ;(Windows/Linux)
;; > (delete-file-from-settings-directory "Addresses\\unneeded-addresses.dat")  ;(Windows/Linux)
;;
(define (delete-file-from-settings-directory filename)
  (define cleansed-filename (cleanse-path-string filename))
  (if (file-exists? (full-file-path-in-settings-directory cleansed-filename))
      (delete-file (full-file-path-in-settings-directory cleansed-filename))
      #f))


;; Renames a file or a sub-directory in the user-specific settings directory.
;; If 'old' is a file and 'new' already exists, it will be overwritten.
;; If 'old' is a directory and 'new' already exists, an exception will be thrown.
;;
;; > (rename-in-settings-directory "old-config.conf" "new-conf.conf")
;; > (rename-in-settings-directory "Old DB" "New DB")
;; > (rename-in-settings-directory "New DB\\old-db-name.dat" "New DB\\new-db-name.dat")  ;(Windows/Linux)
;; > (rename-in-settings-directory "New DB/old-db-name.dat" "New DB/new-db-name.dat")    ;(Windows/Linux)
;;
(define (rename-something-in-settings-directory old new)
  (define cleansed-old (cleanse-path-string old))
  (define cleansed-new (cleanse-path-string new))
  (cond ((or (file-exists-in-settings-directory? cleansed-old)
             (directory-exists-in-settings-directory? cleansed-old))
         (rename-file-or-directory
          (full-file-path-in-settings-directory cleansed-old)
          (full-file-path-in-settings-directory cleansed-new)
          #t))
        (else #f)))


;; For Windows, applies a 'hidden' attribute to a file or directory.
;; For Linux, attaches a dot before the name of the file or directory.
;;
;; > (hide-file-or-directory "D:\\Data\\AVs")
;; > (hide-file-or-directory (full-file-path-in-settings-directory "smtp-credentials.dat"))
;; > (hide-file-or-directory "a-file-in-current-folder.txt")
;;
(define (hide-file-or-directory filepath)
  (define dirpath (get-dirpath-from-filepath filepath))
  (define filename (get-filename-from-filepath filepath))
  (cond ((eq? (system-type) 'windows)
         (process (string-append "attrib +h \"" filepath "\"")))
        ((and (eq? (system-type) 'unix)
              (not (first-char-is-dot? filename)))
         (define new-filepath
           (string-append dirpath "/." filename))
         (process (string-append "mv " filepath " " new-filepath)))
        (else #f)))


;; Extracts the directory portion from a file path.
;;
;; > (get-dirpath-from-filepath "/home/yongjec/.Racket_QA/smtp-credentials.conf")
;; "/home/yongjec/.Racket_QA"
;;
(define (get-dirpath-from-filepath filepath)
  (cond ((eq? (system-type) 'windows)
         (cadr (regexp-match #rx"(.*)\\\\[^\\]*$" filepath)))
        ((eq? (system-type) 'unix)
         (cadr (regexp-match #rx"(.*)/[^/]*$" filepath)))
        (else #f)))


;; Extracts the file name portion from a file path.
;;
;; > (get-filename-from-filepath "/home/yongjec/.Racket_QA/smtp-credentials.conf")
;; "smtp-credentials.conf"
;;
(define (get-filename-from-filepath filepath)
  (cond ((eq? (system-type) 'windows)
         (if (pair? (regexp-match #rx"\\\\([^\\]*)$" filepath))
             (cadr (regexp-match #rx"\\\\([^\\]*)$" filepath))
             filepath))
        ((eq? (system-type) 'unix)
         (if (pair? (regexp-match #rx"/([^/]*)$" filepath))
             (cadr (regexp-match #rx"/([^/]*)$" filepath))
             filepath))
        (else #f)))


;; Checks if a string starts with a dot character.
;;
;; > (first-char-is-dot? "smtp-credentials.conf")
;; #f
;; > (first-char-is-dot? ".smtp-credentials.conf")
;; #t
;;
(define (first-char-is-dot? a-string)
  (pair? (regexp-match #rx"^\\." a-string)))


;; For Windows, replaces all / to \\.
;; For Linux, does the opposite.
;;
;; > (cleanse-path-string "Addresses/master.db")
;; "Addresses\\master.db"                           ;(Windows)
;; > (cleanse-path-string "Addresses\\master.db")
;; "Addresses\\master.db"                           ;(Windows)
;;
;; > (cleanse-path-string "Addresses/master.db")
;; "Addresses/master.db"                            ;(Linux)
;; > (cleanse-path-string "Addresses\\master.db")
;; "Addresses/master.db"                            ;(Linux)
;;
;; > (cleanse-path-string "smtp-config.conf")
;; "smtp-config.conf"
(define (cleanse-path-string str)
  (cond ((eq? (system-type) 'windows)
           (string-replace str "/" "\\"))
          ((eq? (system-type) 'unix)
           (string-replace str "\\" "/"))
          (else str)))
