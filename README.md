Simple VHDL Computing System
============================

Author:             

* Richard James Howe.

Copyright:          

* Copyright 2013 Richard James Howe.

License:            

* LGPL

Email(s):              

* howe.r.j.89@googlemail.com

* howerj@aston.ac.uk

All works are copyright of Richard James Howe unless stated elsewhere, all
works not written by me have also been released under the LGPL license.

Introduction
============

This project contains various modules linked together to create a small
computer. It is meant to be extensible and is an on going project,
although it\`s ultimate purpose as my final year project while studying
for an electronic engineering degree.

The project is written in VHDL, and is built around a CPU core called
the H2, which is a rewrite and extension of a Forth based CPU core called
the J1.

Software needed
===============

* Make

* GCC

* Bash

* GHDL

* Gtkwave

* Xilinx Webpack 14.X

Hardware needed
===============

* Digilent\`s Nexys 3 FPGA development board.

* 2 x Micro USB cables.

* Home computer.

* VGA Cable.

* VGA compatible monitor.

Directory structure.
====================

forth/  
------

Contains a Forth interpreter, this has been used in the build process
and as an assembler.

Edit the file **h2.fs** with the program you want to run on the device.

docs/
-----

Contains the documentation for the project.

vhdl/
-----  

Contains the actual code, written in what should be portable VHDL,
it includes a test benches.

To run the test bench, run the script **make simulation**, to show
the output run **make viewer**.

To implement the design type:

~~~

    make synthesis
    make implementation
    make bitfile
    make upload

~~~

*make upload* requires the device (the Nexys 3) be plugged in.

vm/
---   

Contains virtual machine, simulation of the H2 CPU. [TODO]


EOF
