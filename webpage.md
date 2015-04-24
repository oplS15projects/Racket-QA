# Racket-QA

##Authors

* Roy Van Liew
* James Kuczynski
* Yong Cho

##Overview
Racket-QA is a set of utilities that expand on RackUnit to provide a Regression Harness that QA Engineers could potentially use if coding in Racket. Along with automated testing, Racket-QA also provides a means of standardizing documentation in code so it can also be processed and displayed in web pages.

##Screenshots
![master-gui-page.png](https://raw.githubusercontent.com/oplS15projects/Racket-QA/master/demo/master-gui-page.png)

![bottle-racket-gui.png](https://raw.githubusercontent.com/oplS15projects/Racket-QA/master/demo/bottle-racket-gui.png)

![test-capture-gui.png](https://raw.githubusercontent.com/oplS15projects/Racket-QA/master/demo/test-capture-gui.png)

![scheduler-gui.png](https://raw.githubusercontent.com/oplS15projects/Racket-QA/master/demo/scheduler-gui.png)

![racket-doc-gui.png](https://raw.githubusercontent.com/oplS15projects/Racket-QA/master/demo/racket-doc-gui.png)

![mail-list.png](https://raw.githubusercontent.com/oplS15projects/Racket-QA/master/demo/mail-list.png)

##Concepts Demonstrated
Identify the OPL concepts demonstrated in your project. Be brief. A simple list and example is sufficient. 
* Mailing list and automated test database used an object with local variables and procedures, reinforcing use of classes to keep track of state.
* Using Higher Order Procedures (HOP) such as zip and map to organize information together into a nicely formatted list structure.
* Symbolic language processing for determining operating systems to allow for cross-platform compatibility in paths.

##External Technology and Libraries
Briefly describe the existing technology you utilized, and how you used it. Provide a link to that technology(ies).
* [**RackUnit**][RackUnit] is an API that provides a way to create test cases, include them in test suites, and run a procedure on these test suites. The textual interface was used, and it provides detailed information on which test cases failed and the test results when the output is redirected to an output file to parse for analysis.
* [**Email**][QA-Email] functionality was implemented using [**NET/SMTP**][net/smtp] library. It is used by Bottle-Racket and test scheduler components to email unit test results.
* [**GUI library**][GUI] was used throughout the project to implement user interfaces. Example usages include [**Bottle-Racket**][Bottle-Racket] and [**Test Scheduler**][Scheduler] UIs.

##Favorite Lines of Code
####Roy
File: `bn-to-racket.rkt` line 266

This procedure is an example of not only using HOP, but also abstracting the procedures called within such that the person doesn't have to know how any of the other procedures are implemented to see what this procedure is doing. You call `get-all-test-information` and it retrieves all the important test case information needed to generate a test suite file, and the implementations can be changed if the user is trying to convert a different kind of test file (in our case, the Bottlenose files).
```scheme
(define (get-all-test-information all-lines)
  (zip (get-all-test-inputs all-lines)
       (get-all-expected-values all-lines)
       (get-all-test-names all-lines)))
```
####Yong
File: `scheduler_ui.rkt` line 1038, 1067

These lines conveniently disable all children elements in a GUI area container. I used it in the test scheduler UI where a group of the UI elements becomes irrelevant when user makes certain selections.
```scheme
(define (disable-all-children area-container)
  (for-each send-disable (send area-container get-children)))
(define (send-disable control) (send control enable #f))
```
####James
File: `PageGenerator.rkt` line 475

This code segment generates Racket code for a web server and static web pages, inserting into them data parsed from the user's *.rkt files.  I like it because although I--like every other cs student--have written text to files, this is the first time I'm writing code which in turn writes code of its own! *(add what OPL concepts it demonstrates)*
```scheme
(write-string "\n                     (a ((href, (embed/url codeblock" output)
         (write-string (number->string count) output)
         (write-string "-page))) \"Code\")\n" output)
         (write-string "                             (br) (br) (br)\n" output)
         (set! count (+ count 1))
         (procLooper (cdr pLst) (cdr dLst) count)
```

##Additional Remarks
Anything else you want to say in your report. Can rename or remove this section.

#How to Download and Run
Note: After the main gui page has all four working buttons, we could provide a demo video. But we definitely want the user to run the main gui page, not individual components separately in four scripts open.

You may want to link to your latest release for easy downloading by people (such as Mark).

Include what file to run, what to do with that file, how to interact with the app when its running, etc. 

<!-- Links -->
[QA-Email]: https://github.com/oplS15projects/Racket-QA/blob/master/QA-Email/readme.md
[Scheduler]: https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/readme.md
[Bottle-Racket]: https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md
[Scheduler]: https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/readme.md
[RackUnit]: http://docs.racket-lang.org/rackunit/index.html
[net/SMTP]: http://docs.racket-lang.org/net/smtp.html
[GUI]: http://docs.racket-lang.org/gui/
