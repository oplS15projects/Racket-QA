### Racket-QA Automated Test Scheduler

Racket QA Test Scheduler provides functionality to schedule a one-time or recurring test run-up without user intervention. The scheduler relies on Bottle-Racket functionality to execute a test and generate the results.

There are 6 files that comprise the test scheduler.
* `autotest.rkt` implements an autotest object that contains all the information necessary to execute a set of racket source files without user intervention, such as the execution time, list of files to execute, whether to notify the results through email, and so on.
* `scheduler.rkt` implements threads to keep track of current time and calls Bottle-Racket APIs to execute autotests as they come due.
* `bg-process.rkt` initializes threads defined in `scheduler.rkt`.
* `scheduler-ui.rkt` implements the user interface to schedule an automated test. Information gathered through the UI is saved as an autotest object implemented in the `autotest.rkt`.
* `input-validation.rkt` provides procedures to validate user entries in the scheduler UI.
* `calendar.rkt` provides utility procedures for the autotest object to calculate the correct due time when requested by the scheduler threads.


### How to use Test Scheduler

In order to start the scheduler, load (require) `scheduler_ui.rkt` and run `(launch-scheduler)` procedure. On the left side of the scheduler window are two list boxes containing active and inactive auto-test items. When there is no auto-test configured, these lists will be empty. The purpose of inactive auto-test is to allow the user to temporarily disable a periodic auto-test instead of having to delete it altogether.

![UI_empty](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_empty.png)

User can click the "Create A New Autotest" button on the top right corner to schedule a new auto-test. Clicking this button will enable the input form on the right side of the window so they can be filled out. When the user enters an invalid entry or omits a required field, the background color of that field will change to red to signal it needs correction.

![UI_create](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_create.png)

The input form collects all the information necessary to manage automatic test items and execute test files without user intervention. Here are the information required from the user.
* Name of the auto-test item
* Files to execute - one or more files can be associated with an auto-test item
* Time of day to execute the test
* Whether to execute it once or periodically
* The date of execution for one-time test
* Days of week for periodic test
* Whether to notify the result via email
* Mailing list used for e-mail notification (configured through QA-Email UI)

The information filled out by the user can be reviewed after the test has been created by clicking the test name shown on either active or inactive test list box on the left. User can also modify the configuration by editing the input fields and clicking "Save Changes" button on the bottom.

![UI_browse](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_browse.png)

"Autotest Actions" choice field below the test list boxes allows user to activate, deactivate, or delete an auto-test item.

![UI_actions](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_actions.png)

When an auto-test is due and is active, the files associated with it will be executed by Bottle-Racket's APIs. The scheduler will also output to the Dr.Racket REPL console which files are currently being executed.

![UI_run_test](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_run_test.png)
