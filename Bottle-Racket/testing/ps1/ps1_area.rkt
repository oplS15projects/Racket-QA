#lang racket

;; Racket Unit Testing Libraries
(require racket/include)
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

;; Suite file for this assignment
(require "ps1_suite.rkt")

;; map is used here to allow each test suite to appear in the same GUI window.
(map (make-gui-runner) test-list)


(provide (all-defined-out))

