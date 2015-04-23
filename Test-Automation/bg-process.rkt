;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: bg-process.rkt
;; Author: Yong Cho 
;; Email: Yong_Cho@student.uml.edu
;; File Description: Executes timer and runner threads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "scheduler.rkt")

(define runner (make-runner))
(define timer (make-timer runner))
(thread-send runner (list INIT-TIMER-MESSAGE timer))
