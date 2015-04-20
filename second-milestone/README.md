### Note: This is still in progress

Running and Testing this Version of Racket-QA
* [**Bottle-Racket and Test-Capture**][Bottle-Racket Document]
* [**QA-Email**][QA-Email Document]
* [**Scheduler**][Scheduler Document]
* [**Racket-Doc**][Racket-Doc Document]

For Second Milestones:

1. The files `bn-to-racket.rkt` and `test-tracker.rkt` sourced in `Bottle-Racket` and `Test-Capture` (respectively) are formatted with Racket-Doc's specifications.
  2. Test this on `Racket-Doc`
1. `Racket-Doc` will be able successfully port the extracted data to the web pages.
1. `Test-Capture` and `Scheduler` can run a test suite file upon the user's choice, regardless if it's a test suite file generated from `Bottle-Racket` as long as it maintains [**consistent structure of a test suite file**][Bottle-Racket Document].
  2. A test suite file will be provided that's not generated from `Bottle-Racket` to show this.
1. `Scheduler` is able to send emails automatically after a scheduler test run-up.

<!-- Links -->
[QA-Email Document]: https://github.com/oplS15projects/Racket-QA/blob/master/QA-Email/readme.md
[Racket-Doc Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Racket-Doc/README.md
[Bottle-Racket Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md
[Scheduler Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/readme.md
