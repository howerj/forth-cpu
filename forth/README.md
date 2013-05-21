A Portable FORTH implementation: Howe Forth
===========================================

![Howe Forth Logo](https://raw.github.com/howerj/c-forth/master/logo.png "By the power of HOWE FORTH!")


Author:             

* Richard James Howe.

Copyright:          

* Copyright 2013 Richard James Howe.

License:            

* LGPL

Email(s):              

* howe.r.j.89@googlemail.com

INTRO
=====

This is a portable FORTH implementation called "Howe Forth", it is designed to
be customizable to your needs, modular and fairly fast. Eventually it is
intended that this will be ported to an embedded system (Most likely ARM based),
for now it will stay on the desktop computer.

REQUIREMENTS
============

* GCC

Used to compile the C program.

* GNU Make

Used for the build system.

There are some optional dependencies that are more to do with debugging the
system then anything else and are probably not relevant. 

OPTIONAL REQUIREMENTS
---------------------

* markdown

Typing **make html** will create html files of all \*.md in the directory. 

* gcov

**make gcov** will compile the forth interpreter and run it with the initial
configuration file. "gcov" is then run showing what lines are executed most
frequently.

* valgrind

**make valgrind**

This compiles the program and runs it in valgrind, outputting everything to a
file called *valgrind.log*. The program halts after the initial configuration
file is given as with **make gcov**.

* GNU indent

NOTES
=====

To compile type **make** and then to run type **./forth**. The makefile has more
options in it which can be displayed with the command **make help**.

The documentation is provided in three files: *MANUAL.md*, *README.md* (this
file) and *TODO.md*. You probably want to start off with the file *MANUAL.md*
which naturally contains the manual. *FORTH.md* contains a tutorial on my
dialect of Forth in more details than *MANUAL.md*.

Please contact me with any errors you encounter.

