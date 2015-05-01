# Racket QA

### [**Racket-QA Web Page**][WebPage]


### Versions
* [**Racket-QA v0.3.0**][WebPageRelease]
* [**Racket-QA v0.2.0**][Milestone2]
* [**Racket-QA v0.1.0**][Milestone1]


### Project Information
* <a href="https://github.com/Dossar/FP4-proposal" target="_blank">**Racket-QA Project Proposal**</a>
* <a href="https://docs.google.com/spreadsheets/d/1FT8ZNomihkExBPH3syUVGtOUYCphharifb_k9nDoNv0/edit#gid=0" target="_blank">**Project Calendar**</a>
* <a href="https://docs.google.com/presentation/d/1Ff5LjW92cEDqhPJGla6IjBosKEh1DuKNqqaBsNtIqRg/edit#slide=id.p" target="_blank">**Presentation Slides**</a>


### Demo Videos
* <a href="https://www.youtube.com/watch?v=Ws2mMMBFns4" target="_blank">**Bottle-Racket Demo Video**</a>
* <a href="https://www.youtube.com/watch?v=JqngnONV9ks" target="_blank">**Test Scheduler Demo Video**</a>
* <a href="https://www.youtube.com/watch?v=jTNaCMzuZeQ" target="_blank">**Racket-QA Email Feature Demo Video**</a>


### Documentation
* [**Racket-Doc**][Racket-Doc Document]
* [**Bottle-Racket and Test-Capture**][Bottle-Racket Document]
* [**Test Scheduler**][Test Scheduler Document]
* [**QA-Email**][QA-Email Document]

### Note Concerning "**About Me**" Page
Due to the way Racket handles multi-threading and web servers, if you click the **About Me** button, you MUST click "**Exit**" when you wish to return to the main GUI and main thread.

**DO NOT** click the web browsers X untill **AFTER** you do this.  Otherwise, the web server will still be the thread running, and the main GUI will be unable to process events.


<!-- Links -->
[WebPage]: http://opls15projects.github.io/Racket-QA/
[WebPageRelease]: https://github.com/oplS15projects/Racket-QA/releases/tag/v0.3.0
[Milestone1]: https://github.com/oplS15projects/Racket-QA/releases/tag/v0.1.0
[Milestone2]: https://github.com/oplS15projects/Racket-QA/releases/tag/v0.2.0
[QA-Email Document]: https://github.com/oplS15projects/Racket-QA/blob/master/QA-Email/readme.md
[Racket-Doc Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Racket-Doc/README.md
[Bottle-Racket Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Bottle-Racket/README.md
[Test Scheduler Document]: https://github.com/oplS15projects/Racket-QA/blob/master/Test-Automation/readme.md
