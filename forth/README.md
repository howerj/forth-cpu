## A Portable FORTH implementation: Howe Forth

![Howe Forth Logo](https://raw.github.com/howerj/c-forth/master/doc/logo.png "By the power of HOWE FORTH!")

Author:             

* Richard James Howe.

Copyright:          

* Copyright 2013 Richard James Howe.

License:            

* LGPL

Email(s):              

* howe.r.j.89@googlemail.com

## Introduction

This is a portable FORTH implementation called "Howe Forth", it is designed to
be customizable to your needs, modular and fairly fast. Eventually it is
intended that this will be ported to an embedded system (Most likely ARM based),
for now it will stay on the desktop computer.

### Directory structure

Projects directory structure:

#### bin/

This contains the binary that *should* have built without any problems what so
ever, it also contains a system link into the "fth/" directory.

To this forth once it has been built change directories to "bin/" and type:

  ./forth

It will look for a file called "forth.4th" in the same directory which is the
first thing it will execute.

#### doc/

The manual(s) written in markdown are in this directory. They can be compiled to
HTML by typing "make html" which will create the documents in the same
directory, "doc/".

#### fth/

The Forth source is contained in here! This is where most of the functionality
is actually defined.

#### lib/

"lib/" contains the Howe Forth library which "main.c" is simply a wrapper
around, it defines the Forth virtual machine and implements the core primitives.

### Requirements

* GCC

Used to compile the C program.

* GNU Make

Used for the build system.

There are some optional dependencies that are more to do with debugging the
system then anything else and are probably not relevant. 

### Optional requirements

These are extra tools that can be used in conjunction with the project. In the
make file there are commands that you can run that will act as wrappers around
these tools.

#### markdown

Typing **make html** will create html files of all \*.md in the directory. 

#### gcov

**make gcov** will compile the forth interpreter and run it with the initial
configuration file. "gcov" is then run showing what lines are executed most
frequently.

#### valgrind

**make valgrind**

This compiles the program and runs it in valgrind, outputting everything to a
file called *valgrind.log*. The program halts after the initial configuration
file is given as with **make gcov**.

#### GNU indent

This will format the files in standard way, if you do not like the way my code
looks (formatting wise!) you can change the command passed to indent in the make
file and rerun **make pretty**. This command will also clean up the directory
structure like **make clean** does as well as run "wc" over the sources files.

### Notes

To compile type **make** and then to run type **./forth**. The makefile has more
options in it which can be displayed with the command **make help**.

The documentation is provided in three files: *MANUAL.md*, *README.md* (this
file) and *TODO.md*. You probably want to start off with the file *MANUAL.md*
which naturally contains the manual. *FORTH.md* contains a tutorial on my
dialect of Forth in more details than *MANUAL.md*. They might not be up to date
however! 

*TODO.md*, *MANUAL.md* and *FORTH.md* are all in the "doc/" directory.

Please contact me with any errors you encounter.
