### Note: This is still in progress

Running and Testing this Version of Racket-QA
* [**Bottle-Racket and Test-Capture**][Bottle-Racket Document]
* [**QA-Email**][QA-Email Document]
* [**Scheduler**][Scheduler Document]
* [**Racket-Doc**][Racket-Doc Document]

For Second Milestones:

1. The file `test-tracker.rkt` has been formatted with the specifications of `Racket-Doc` and is able to be converted successfully and displayed into a web page `bn-to-racket.rkt` is still something to get displaying.
1. `Racket-Doc` is able to successfully port extracted data to the web pages, but with limitations. See the Racket-Doc README for more details.
1. `Test-Capture` and `Scheduler` can run a test suite file upon the user's choice, regardless if it's a test suite file generated from `Bottle-Racket` as long as it maintains [**consistent structure of a test suite file**][Bottle-Racket Document].
  2. The test suite `midterm` has been provided for this purpose.
1. `Scheduler` is able to send emails automatically after a scheduled test run-up.

<!-- Links -->
[QA-Email Document]: https://github.com/oplS15projects/Racket-QA/blob/master/QA-Email/readme.md
[Racket-Doc Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Racket-Doc/README.md
[Bottle-Racket Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md
[Scheduler Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/readme.md
