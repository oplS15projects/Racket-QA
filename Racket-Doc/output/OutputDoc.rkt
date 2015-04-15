requires-------------------------------
(require "andThisTest.rkt")
(require "testing.rkt")
includes-------------------------------
(include "secIncl.rkt")
(include "firstIncl.rkt")
provides-------------------------------
(provide square)
procedures & data-----------------------
(define five 5)
(define (addFour num1 num2 num3 num4)
(define (square num)
documentation blocks---------------------
#||
 | Created a variable named five.
 | and gave it a value.
 |#
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
#||
 | This function squares a given number.
 | @param num Is the number to be squared.
 |          
 | @return the square of the numbers
 |#
