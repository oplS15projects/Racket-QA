#lang racket

(include "firstIncl.rkt")
(include "secIncl.rkt")

(require "testing.rkt")
(require "andThisTest.rkt")

(provide square)

#||
 | This function squares a given number.
 | @param num Is the number to be squared.
 |          
 | @return the square of the numbers
 |#
(define (square num)
  (* num num)
  (display "finished doing math")
  (display "reached end of procedure squre"))


#||
 | This function adds four numbers together.
 | @param num1 The first number.
 | @param num2 The
 |	       second
 |	       number.
 | @param num3 The third number.
 | @param num4 The fourth number.
 | @return The sum of the numbers.
 |#
(define (addFour num1 num2 num3 num4)
  (display "this is a string to be printed")
  (+ num1 num2 num3 num4))


#||
 | Created a variable named five.
 | and gave it a value.
 |#
(define five 5)
