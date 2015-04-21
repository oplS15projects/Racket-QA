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

<!-- Links -->
[QA-Email Document]: https://github.com/oplS15projects/Racket-QA/blob/master/QA-Email/readme.md
[Racket-Doc Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Racket-Doc/README.md
[Bottle-Racket Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md
[Scheduler Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/readme.md
