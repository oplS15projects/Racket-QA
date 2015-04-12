### Racket QA Email Feature

Racket QA Email component provides functionality to send a text of html file, as well as plain strings to specified email addresses.

There are 4 files that comprise the email component.
* `email.rkt` contains procedures to construct and send an email message using SMTP and SSL libraries.
* `email-db.rkt` implements procedures to store and retrieve the user's mailing lists from the user's disk storage.
* `email-db-ui.rkt` implements GUI that allows the users to add or remove a mailing list, as well as manage email addresses for each mailing address.
* `email-db-entry-ui.rkt` implements GUI that is used to add a new entry to a mailing list.

Among the above files, `email.rkt` and `email-db-ui.rkt` are intended to be 'require'd by other components of the Racket QA application. The other files are to be used only by the email component and their contents are subject to change ay any time to improve efficiency and reliability.


### How to use email sender

There are 3 interface procedures that are provided in `email.rkt` that can be used to send an email. In order to use them, an application component needs to include `email.rkt` file in their source code. All procedures in `email.rkt` file other than these 3 procedures are subject to change ay any time, so it is recommended that applications use only these 3 procedures. Here are the signatures and usage examples of the procedures.

* send-text
: sends a plain text string as the body of an email.
```
(send-text to subject a-string-to-send list-of-recipients)

(send-text "Project Dev Team A"
           "Regression Statistics 4/11"
           "Everything passed guys!"
           (list "test.racketscience@gmail.com" "test2.racketscience@gmail.com"))
```

* send-text-file
: sends the entire contents of a text file as the body of an email.
```
(send-text-file to subject a-text-file-to-send list-of-recipients)

(send-text-file "Project Dev Team B"
			    "Autotest Result 4/12"
			    "D:\\Data\\Racket QA\\Autotest\\test-result.txt"
			    (list "test.racketscience@gmail.com" "test2.racketscience@gmail.com"))
```

* send-html-file
: sends the entire contents of an html file as the body of an email.
```
(send-html-file to subject a-html-file-to-send list-of-recipients)

(send-html-file "Project Dev Team C"
				"Autotest Result 5/2"
				"D:\\Data\\Racket QA\\Autotest\\test-result.html"
				(list "test.racketscience@gmail.com" "test2.racketscience@gmail.com"))
```

### Mailing List UI

User interface for managing mailing list is implemented in `email-db-ui.rkt` file. In order to launch the UI, an application needs to 'require' `email-db-ui.rkt` file and call `open-manage-mailing-list-dialog` procedure. Calling this procedure will launch a dialog box as shown below.

![Mailing List UI](https://raw.githubusercontent.com/YongCho/FPX/master/QA-Email/Documentation/images/db-ui.png)

The list box on the left side of the dialog contains the names of the existing mailing lists. When the user first launches this dialog, it will be empty. The user can create a new mailing list by using the drop down control below the list box and selecting 'Add mailing list...'. The list box on the right side shows all the entries that are in the currently selected mailing list. A mailing list entry consists of a name and an email address. The user can use the 3 buttons (Edit, Add, Delete) on the right side to configure each entry.

`open-manage-mailing-list-dialog-box` has optional argument `command` which allows returning the list of email addresses in the selected mailing list when the dialog is closed. This may be used to ask the user to select a mailing list for the purpose of sending a mass email. Here is the signature and usage example of `open-manage-mailing-list-dialog-box` procedure.

```
Signature:

(open-manage-mailing-list-dialog (command #f))


Example:

(require "../Common/email.rkt"
         "../Common/email-db-ui.rkt")

(define recipient-addresses (open-manage-mailing-list-dialog 'return-addresses))
(send-text-file "Project Dev Team A"
				"Test Result 4/9"
				"D:\\Data\\Racket QA\\Autotest\\test-result.txt"
				recipient-addresses)
```
