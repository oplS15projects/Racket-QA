### Racket QA Email Feature

Racket QA Email component provides functionality to send a text of html file, as well as plain strings to specified email addresses.

There are 4 files that comprise the email component.
* `email.rkt` contains procedures to construct and send an email message using SMTP and SSL libraries.
* `email-db.rkt` implements procedures to store and retrieve the user's mailing lists from the user's disk storage.
* `email-db-ui.rkt` implements GUI that allows the users to add or remove a mailing list, as well as manage email addresses for each mailing address.
* `email-db-entry-ui.rkt` implements GUI that is used to add a new entry to a mailing list.

Among the above files, `email.rkt` and `email-db-ui.rkt` are intended to be 'require'd by other components of the Racket QA application. The other files are to be used only by the email component and their contents are subject to change at any time to improve efficiency and reliability of the email component.


### How to use email sender

There are 3 interface procedures that are provided in `email.rkt` that can be used to send an email. In order to use them, an application needs to include `email.rkt` file in their source code. All procedures in `email.rkt` file other than these 3 procedures are subject to change at any time, so it is recommended that applications use only these 3 procedures. Here are the signatures and usage examples of the procedures.

* send-text
: sends a plain text string as the body of an email.
```
(send-text to subject a-string-to-send list-of-recipients)

(send-text "Project Dev Team A"
           "Test email"
           "Hi, team. Ignore this email. It's for test."
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

User interface for managing mailing list is implemented in `email-db-ui.rkt` file. In order to launch the UI, an application needs to 'require' `email-db-ui.rkt` file and call `open-manage-mailing-list-dialog` procedure. Calling this procedure will launch a dialog box shown below.

![Mailing List UI](https://github.com/oplS15projects/Racket-QA/blob/master/QA-Email/images/db-ui.png)
![Mailing List UI](/images/db-ui.png)

The list box on the left side of the dialog contains the names of the existing mailing lists. It will be empty when the user first launches this dialog. The user can create a new mailing list by using the drop down control below the list box and selecting 'Add mailing list...'. The list box on the right side shows all the entries that are in the currently selected mailing list. A mailing list entry consists of a name and an email address. The user can use the 3 buttons (Edit, Add, Delete) on the right side to configure each entry.

This UI is designed to completely separate the user experience in managing mailing lists from the internal implementations of how mailing lists are actually stored in the user's storage. The user will never have to "load a mailing list file" or "save the current addresses to a file" in order to manage mailing lists. All are done automatically by the UI when the user creates or deletes a mailing list, or adds or removes an entry in a mailing list.

`open-manage-mailing-list-dialog-box` has optional argument `command` which allows returning the list of email addresses in the selected mailing list when the dialog is closed. This may be used to ask the user to select a mailing list for the purpose of sending a mass email. Here is the signature and usage example of `open-manage-mailing-list-dialog-box` procedure.

```
Signature:

(open-manage-mailing-list-dialog (command #f))
```

```
Example 1:

(require "../QA-Email/email.rkt"
         "../QA-Email/email-db-ui.rkt")

(define recipient-addresses (open-manage-mailing-list-dialog 'return-addresses))
(send-text-file "Project Dev Team A"
                "Test Result 4/9"
                "D:\\Data\\Racket QA\\Autotest\\test-result.txt"
                recipient-addresses)
```

Below example script shows how to retrieve certain attributes of a mailing list selected from the UI - name, id, and database file path. The application can associate these information with the user's unit test suite and store them together.

When it needs to retrieve the email addresses for a specific mailing list in the future for the purpose of mailing a test result, it can call the `db-id-to-addresses` procedure which will return the email addresses associated with a specific mailing list id.

Note that procedures to retrieve email addresses by other attributes of a mailing list will not be implemented because ID is the only attribute that is guaranteed to be unique per each mailing list. For example, the user can create multiple mailing list with the same name, so it is not a good practice
to identify mailing lists by their names. File path is not the best choice either because file names or paths may change as internal implementation of the email database changes. The ID is the only thing guaranteed not to change for a specific mailing list, and therefore, should be used to identify each mailing list.
```
Example 2:

#lang racket

(require "../QA-Email/email.rkt"
         "../QA-Email/email-db.rkt"
         "../QA-Email/email-db-ui.rkt")

(define mailing-list (open-manage-mailing-list-dialog 'return-db))
(define mailing-list-id (email-db-id mailing-list))
(define mailing-list-name (email-db-name mailing-list))
(define mailing-list-addresses (email-db-addresses mailing-list))

> mailing-list-id
1
> mailing-list-name
"Team Racket Science"
> mailing-list-addresses
'("yong_cho@student.uml.edu" "yongjec@gmail.com" "racket.riot@gmail.com")

(define recipients (db-id-to-addresses mailing-list-id))

> recipients
'("yong_cho@student.uml.edu" "yongjec@gmail.com" "racket.riot@gmail.com")

(send-text-file "Project Dev Team A"
                "Test Result 4/9"
                "D:\\Data\\Racket QA\\Autotest\\test-result.txt"
                recipients)

```
