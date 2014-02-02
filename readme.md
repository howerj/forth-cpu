## Simple VHDL Computing System  {#mainpage}

Author:             

* Richard James Howe.

Copyright:          

* Copyright 2013 Richard James Howe.

License:            

* LGPL

Email:              

* howe.r.j.89@googlemail.com


All works are copyright of Richard James Howe unless stated elsewhere, all
works not written by me have also been released under the LGPL license.

### Introduction

This project contains various modules linked together to create a small
computer. It is meant to be extensible and is an on going project.

The project is written in VHDL, and is built around a CPU core called
the H2, which is a rewrite and extension of a Forth based CPU core called
the J1.

### Software needed

* Make

* GCC

* Perl

* Bash

* GHDL

* Gtkwave

* Xilinx Webpack 14.X

* Digilent drivers for the Nexys 3 development board.

I am running all my build on Debian version 7.0.X at the moment, but it should
run on other systems perfectly fine as well.

### Hardware needed

* Digilent\`s Nexys 3 FPGA development board.

* 2 x Micro USB cables.

* Home computer.

* VGA Cable.

* VGA compatible monitor.

### Directory structure.

* asm/
* doc/
* tst/
* vhd/

#### asm/  

Contains the assembler and instructions on how to use it. The assembler itself
is written in perl, *asm.pl*. This is a work in progress, but it does work! More
directives will be added as needed. There is documentation in the folder
(*asm.md*).

The is also a disassembler in here, *dis.pl*. It is fairly primitive but useful
for verifying that the code generation has worked the way you think it should.

#### doc/

Documentation pertaining to the entire project should go here, it is often out
of date however as the project is currently not in a stable state at the moment
and constantly rewriting it when it is in a state of flux would only hinder and
confuse things.

#### vhd/

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

#### tst/

Testing directory for miscellaneous side and sub projects. It also contains old
sub-projects that I would rather have on the tip of the repository instead of
floating around in the history only.

### To-do

My priorities at the moment are getting the tool chain used to program the
processor up and running with a basic boot loader, getting the PS/2 keyboard
working and testing and bug fixing the system.

I would very much like to make the CPU more generic, that is you can instantiate
copies with variable bit lengths but there are a few things stopping me from
doing that at the moment.

EOF
