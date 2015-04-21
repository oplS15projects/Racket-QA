#### README layout for Second Milestone description

Running and Testing this Version of Racket-QA
* [**Bottle-Racket and Test-Capture**][Bottle-Racket Document]
* [**QA-Email**][QA-Email Document]
* [**Scheduler**][Scheduler Document]
* [**Racket-Doc**][Racket-Doc Document]

For Second Milestones:

1. The file `test-tracker.rkt` has been formatted with the specifications of `Racket-Doc` and is able to be converted successfully and displayed into a web page. In the future, `bn-to-racket.rkt` is something that we'd like to convert properly.
1. `Racket-Doc` is able to successfully port extracted data to the web pages, but with several stringent limitations. See the [**Racket-Doc README**][Racket-Doc Document] for more details. Before the final code turn-in, `Racket-Doc` is planned to be more flexible in its conversions.
1. `Test-Capture` can run a test suite file upon the user's choice, regardless if it's a test suite file generated from `Bottle-Racket` as long as it maintains [**consistent structure of a test suite file**][Bottle-Racket Document].
  2. The test suite `midterm` has been provided for this purpose.
1. `Scheduler` meets the expectations for the second milestone.
  2. `Scheduler` has been tested fully functional with ability to execute both Bottlenose test suite files and other RackUnit test suite files on Windows, Linux, and Mac at any specified time. 
  3. `Scheduler` has successfully generated result files for test run-ups as well as sent them to a mailing list configured through its UI.
  4. `Scheduler` is only functional when loaded in Dr.Racket framework. Once Dr.Racket closes, time-tracking threads are killed, and scheduled tests will not run anymore.


Future Tasks:
1. We can standardize our file headers. Currently, Roy uses ;; *** style headers, James uses ;;, and I use #|| style headers. We have also been putting varying information on our headers. Although this is not a professional software project and there is no requirement for it, we can surely look more professional.
2. We need to ensure our codes are compatible with the major platforms - Windows, Linux, and Mac. We have four different components that have been developed in all different platforms; Windows for Bottle-Racket, Linux for RacketDoc, Mac and Windows for Test Scheduler and QA-Email. Although Racket language is supposed to be cross-platform, it is not guaranteed that a code written in one platform would work correctly in another. It wouldn't make sense a part of a program works on one platform, and another part have to be on a different platform when they are all in one program. We need to test compatibility as a whole program to ensure parts of it doesn't break on users.
3. If we have time, we can create a user interface that brings together all components of the Racket-QA application. Currently, we have three major components (except QA-Email which is kind of a helper to other components) that have been developed separately and operates independently from each other. From the user's perspective, it would make more sense for an application to have one executable, instead of three. Maybe we can have a small start-up UI with 3 icon buttons launching each component.



<!-- Links -->
[QA-Email Document]: https://github.com/oplS15projects/Racket-QA/blob/master/QA-Email/readme.md
[Racket-Doc Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Racket-Doc/README.md
[Bottle-Racket Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md
[Scheduler Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/readme.md
