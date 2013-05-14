
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

* howerj@aston.ac.uk

INTRO
=====

This is a portable FORTH implementation called "Howe Forth", it is designed to
be customizable to your needs, modular and fairly fast. Eventually it is
intended that this will be ported to an embedded system (Most likely ARM based),
for now it will stay on the desktop computer.

NOTES
=====

To run first compile the program by running **./compile.sh**, then type
**./forth** which will run the interpreter. The manual for the program
is in **MANUAL.md**.  The other scripts **./gcov.sh** and **./git.sh**
are for my benefit, although feel free to reuse them trivial as they
may be. **./pretty.sh** can be run after either **./compile.sh** or
**./gcov.sh** has been run or the code has been edited. It indents the
**\*.c** and **\*.h** files in a standard way and removes any temporary
files that might be hanging around.

Please contact me with any errors you encounter.

