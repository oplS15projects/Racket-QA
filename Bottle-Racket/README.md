# Final Project Assignment 2: Explore One More! (FP2) 
DUE March 30, 2015 Monday (2015-03-30)

### My Library: Racket Graphical Interface Toolkit

For the second exploration solo project I used the GUI Toolkit that comes with Racket. This is an expansion to my FP1 which looked through a bottlenose perl test file and converted the tests to Racket test cases in a new suite file, along with creating an area file for running that suite. The motivation behind using this library was to create a simple user-friendly window for a person to use if they wanted to run this converter and generate test suite and area files for their assignment implementations.

The `converter-gui.rkt` script generates a window which allows a user to browse their computer for the source assignment file and the bottlenose test file, along with specifying the testing mode (GUI or textual interface). Then there is a `Convert` button that does everything FP1 did in converting the files and writing to output files. However, an advantage to having this GUI is that it shows the source assignment file path and bottlenose test file path along with test mode so the user knows he/she is working with the desired files. After clicking `Convert` the user will be able to see the paths of the two generated files along with the test mode they specified, so they can go straight to the path of the test area file and run it.

In terms of actually using the GUI library, I just did a `#lang racket/gui` and according to the Racket Documentation it combines the following:
* All bindings in `racket` language
* All bindings in `racket/gui/base`
* All bindings in `racket/draw`

### Script

You can find the code for implementing this GUI in [**converter-gui.rkt**][gui-code-rkt]. The original `bn-to-racket.rkt` code was changed a bit from FP1 to try handling linux paths with forward slashes. Originally the script had a dependency on windows in that it only worked with backslash paths. `converter-gui.rkt` is what this FP2 is really about however since it implemented the GUI for the converter.

This high level overview of `bn-to-racket.rkt` is taken from FP1.
* Read in all lines from the bottlenose perl test script.
* Obtain all the test cases in the perl test script and convert them to Racket test cases.
* Create a list of strings that represent what to write out to the Racket file containing the test suite.
* Create a list of strings that represent what to write out to the Racket file containing the test area (the script that runs the test cases).
* `display-lines-to-file` is used to write these strings out to the test suite and test area files.

### Output

The output here will consist of screenshots that demonstrate running the script.

#### Running the script

1. This is the window you will see when running `converter-gui.rkt`, and the text fields have some helpful descriptions as to what will go in them.
![ss1.png](https://raw.githubusercontent.com/Dossar/FP2/master/demo/ss1.png)

2. The Browse buttons are provided to search the computer for the assignment source file and bottlenose test file. This is a window that opens when you click Browse for instance.
![ss2.png](https://raw.githubusercontent.com/Dossar/FP2/master/demo/ss2.png)

3. This is the screen after you've browsed the computer for the files; note the text fields have the absolute path of the source files.
![ss3.png](https://raw.githubusercontent.com/Dossar/FP2/master/demo/ss3.png)

4. The `Convert` button is clicked and you see that the bottom three text fields are now filled in with the absolute paths of the test suite and test area files. Along with that, the dialog under the picture changed to saying that the conversion for the assignment was successful.
![ss4.png](https://raw.githubusercontent.com/Dossar/FP2/master/demo/ss4.png)

5. Go to the path specified in the Output Test Area File field. You'll find the suite and area files generated here.
![ss5.png](https://raw.githubusercontent.com/Dossar/FP2/master/demo/ss5.png)

6. Open the test area script and run it. In this case we specified make-gui-runner so we get the RackUnit GUI when running the test area file.
![ss6.png](https://raw.githubusercontent.com/Dossar/FP2/master/demo/ss6.png)

I would also like to note that the paths shown here have backslashes, indicating I am using Windows. FP1 had the dependency of Windows for this. In FP2, I've made some changes to try dealing with paths that have forward slashes like in Linux. When in doubt, go by the Browse button paths rather than entering in the paths manually.

<!-- Links -->
[gui-code-rkt]: https://github.com/Dossar/FP2/blob/master/converter-gui.rkt
