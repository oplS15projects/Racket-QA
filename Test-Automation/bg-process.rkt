#||
 | bg-process.rkt
 | author: Yong Cho (Yong_Cho@student.uml.edu)
 | Created on: 4/17/2015
 |#

#lang racket

(require "scheduler.rkt")

(define runner (make-runner))
(define timer (make-timer runner))
(thread-send runner (list INIT-TIMER-MESSAGE timer))
