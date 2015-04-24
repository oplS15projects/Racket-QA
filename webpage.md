# Racket-QA

##Authors

* Roy Van Liew
* James Kuczynski
* Yong Cho

##Overview
Racket-QA is a set of utilities that expand on RackUnit to provide a Regression Harness that QA Engineers could potentially use if coding in Racket. Along with automated testing, Racket-QA also provides a means of standardizing documentation in code so it can also be displayed properly in web pages.

##Screenshot
(insert a screenshot here. You may opt to get rid of the title for it. You need at least one screenshot. Make it actually appear here, don't just add a link.)

Here's a demonstration of how to display an image that's uploaded to this repo:
![screenshot showing env diagram](withdraw.png)

##Concepts Demonstrated
Identify the OPL concepts demonstrated in your project. Be brief. A simple list and example is sufficient. 
* Scheduler took advantage of threads. More input can be given on this by Yong.
* Using Higher Order Procedures (HOP) such as zip and map to organize information together into a nicely formatted list structure.
* Symbolic language processing in determining operating systems to allow for cross-platform compatibility in paths

##External Technology and Libraries
Briefly describe the existing technology you utilized, and how you used it. Provide a link to that technology(ies).
* [**RackUnit**][RackUnit] is an API that provides a way to create test cases, include them in test suites, and run a procedure on these test suites. The textual interface was used, and it provides detailed information on which test cases failed and the test results when the output is redirected to an output file to parse for analysis.
* List more libraries here such as GUI, setup/dirs, whatever

##Favorite Lines of Code
####Mark (a team member)
Each team member should identify a favorite line of code, expression, or procedure written by them, and explain what it does. Why is it your favorite? What OPL philosophy does it embody?
Remember code looks something like this:
```scheme
(map (lambda (x) (foldr compose functions)) data)
```
####Roy
This procedure is an example of not only using HOP, but also abstracting the procedures called within such that the person doesn't have to know how any of the other procedures are implemented to see what this procedure is doing. You call `get-all-test-information` and it retrieves all the important test case information needed to generate a test suite file.
```scheme
(define (get-all-test-information all-lines)
  (zip (get-all-test-inputs all-lines)
       (get-all-expected-values all-lines)
       (get-all-test-names all-lines)))
```
####Yong
Description of procedure or other piece of code
```scheme
(define stuff 1)
```
####James
Description of procedure or other piece of code
```scheme
(define stuff 1)
```

##Additional Remarks
Anything else you want to say in your report. Can rename or remove this section.

#How to Download and Run
You may want to link to your latest release for easy downloading by people (such as Mark).

Include what file to run, what to do with that file, how to interact with the app when its running, etc. 

<!-- Links -->
[RackUnit]: http://docs.racket-lang.org/rackunit/index.html
