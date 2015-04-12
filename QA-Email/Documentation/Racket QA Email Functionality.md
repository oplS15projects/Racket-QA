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
`(send-text to subject a-string-to-send list-of-recipients)`
: sends a plain text string as the body of an email.
```
(send-text "Project Dev Team A"
           "Regression Statistics 4/11"
           "Everything passed guys!"
           (list "test.racketscience@gmail.com" "test2.racketscience@gmail.com"))
```

* send-text-file
`(send-text-file to subject a-text-file-to-send list-of-recipients)`
: sends the entire contents of a text file as the body of an email.
```
(send-text-file "Project Dev Team B"
			    "Autotest Result 4/12"
			    "D:\\Data\\Racket QA\\Autotest\\test-result.txt"
			    (list "test.racketscience@gmail.com" "test2.racketscience@gmail.com"))
```

* send-html-file
`(send-html-file to subject a-html-file-to-send list-of-recipients)`
: sends the entire contents of an html file as the body of an email.
```
(send-html-file "Project Dev Team C"
				"Autotest Result 5/2"
				"D:\\Data\\Racket QA\\Autotest\\test-result.html"
				(list "test.racketscience@gmail.com" "test2.racketscience@gmail.com"))

