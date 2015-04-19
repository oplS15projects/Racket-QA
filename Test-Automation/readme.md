# Racket-QA Automated-Test Scheduler

Racket QA Test Scheduler provides functionality to schedule a one-time or recurring test to be run without the user's presence and store the result or send it to a specified mailing list. The scheduler uses <a href="https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md" target="_blank">**Bottle-Racket**</a> functionality to execute a test and generate the results.

There are 6 files that comprise the test scheduler.
* `autotest.rkt` implements an autotest object that contains all the information necessary to execute a set of racket source files without user intervention, such as the execution time, list of files to execute, whether to notify the results through email, and so on.
* `scheduler.rkt` implements threads to keep track of current time and calls Bottle-Racket APIs to execute autotests as they come due.
* `bg-process.rkt` initializes threads defined in `scheduler.rkt`.
* `scheduler-ui.rkt` implements the user interface to schedule an automated test. Information gathered through the UI is saved as an autotest object implemented in the `autotest.rkt`.
* `input-validation.rkt` provides procedures to validate user entries in the scheduler UI.
* `calendar.rkt` provides utility procedures for the autotest object to calculate the correct due time when requested by the scheduler threads.


### How to use Scheduler

In order to start the scheduler, load (require) `scheduler_ui.rkt` and run `(launch-scheduler)` procedure. On the left side of the scheduler window are two list boxes containing active and inactive autotest items. When there is no autotest configured, these lists will be empty. The purpose of 'deactivating' an autotest is to allow the user to temporarily disable a periodic autotest from running instead of having to delete it altogether.

![UI_empty](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_empty.png)

User can click the "Create A New Autotest" button on the top right corner to schedule a new autotest. Clicking this button will enable the input fields on the right side of the window so they can be filled out. Some of the input fields will be disabled based on whether the user has selected 'one-time' or 'repeat' radio button. For one-time test, a date of execution must be provided. For repeating test, at least one day of week must be checked. When the user enters an invalid entry or omits a required field, the background color of that field will change to red to signal it needs correction.

![UI_create](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_create.png)

The input form collects all the information necessary to manage an autotest item and execute test files associated with it without user's presence. Here are the information required from the user to schedule an autotest.
* Name of the autotest item
* Files to execute - one or more files can be associated with an autotest item
* Time of day to execute the test
* Whether to execute it once or periodically
* The date of execution for one-time test
* Days of week to run a periodic test
* Whether to notify the result via email
* Mailing list to be used for e-mail notification

The information filled out by the user can be reviewed after the test has been created by clicking the test name shown on either active or inactive test list box on the left. User can also modify the configuration of an autotest item after creating it by editing the input fields and clicking "Save Changes" button on the bottom.

![UI_browse](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_browse.png)

Clicking "Select Mailing List..." button launches QA-Email UI which allows configuring and selecting a mailing list to associate it with the autotest item.

![UI_email](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_email.png)

"Autotest Actions" choice field below the test list boxes allows user to activate, deactivate, duplicate, or delete an autotest item.

![UI_actions](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_actions.png)

When an active autotest comes due, the files associated with it will be executed by <a href="https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md" target="_blank">**Bottle-Racket**</a> APIs. The scheduler will also output to the Dr.Racket REPL console which files are currently being executed.

![UI_run_test](https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/images/documentation/ui_run_test.png)

Once the test is run, the result files will be generated and placed in locations specified when the test was created through <a href="https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md" target="_blank">**Bottle-Racket**</a>. It will also be mailed to the mailing list if Notify? checkbox was checked and a mailing list was provided. If the test is a repeating test, it will automatically execute again at the next due time. A one-time test will only run once. It will stay in the scheduler UI, however, so the user can run it again by changing its due time.


### Limitation
* The periodic test functionality currently supports automatic execution of a same test as often as daily. It does not support running the same test automatically more than once a day. However, users can easily work around this by creating more than one autotest items with same files and making each test run at different hours. The UI provides 'duplicate test' option for this.
* The scheduled tests will run automatically only when scheduler_ui.rkt file stays loaded in the Dr.Racket framework. Once Dr.Racket closes, scheduler threads are terminated and the scheduled tests will not run automatically anymore. The scheduler window can be closed as long as Dr.Racket stays open. Enabling the scheduler threads to stay active without the presence of Dr.Racket requires the scheduler component to be a standalone executable, and may be implemented some time in the future.


<!-- Links -->
[Bottle-Racket Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md