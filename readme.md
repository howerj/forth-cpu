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

### Hardware needed

* Digilent\`s Nexys 3 FPGA development board.

* 2 x Micro USB cables.

* Home computer.

* VGA Cable.

* VGA compatible monitor.

### Directory structure.

#### asm/  

Contains the assembler and instructions on how to use it. The assembler itself
is written in perl, *asm.pl*.

#### vhdl/

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

#### test/

Testing directory for miscellaneous side and sub projects.


EOF
