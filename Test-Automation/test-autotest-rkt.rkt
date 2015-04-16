#lang racket
(require "autotest.rkt")

(define test-file-1 "")
(define test-file-2 "")

(define at-one-time
  (make-autotest 1000
                 "Auto Test 1"  ;name
                 #t  ;active?
                 (list test-file-1 test-file-2)
                 'one-time
                  2015
                  4
                  15
                  #t  ;mon
                  #t
                  #t
                  #t
                  #t
                  #t
                  #t  ;sun
                  0  ;hour
                  0  ;minute
                  #f
                  #f))

(define at-recur
  (make-autotest 1001
                 "Auto Test 1"  ;name
                 #t  ;active?
                 (list test-file-1 test-file-2)
                 'periodic
                  2015
                  4
                  14
                  #f  ;mon
                  #f  ;tue
                  #f  ;wed
                  #f  ;thu
                  #f  ;fri
                  #f  ;sat
                  #t  ;sun
                  12  ;hour
                  30  ;minute
                  #f
                  #f))

(autotest-next-due at-one-time)
(autotest-next-due at-recur)

(write-autotest at-one-time)