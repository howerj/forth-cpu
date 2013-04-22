Simple VHDL Computing System
============================

Author:         
    * Richard James Howe.
Copyright:      
    * Copyright 2013 Richard James Howe.
License:        
    * LGPL
Email:          
    * howe.rj.89@googlemail.com

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

    * Digilent\`s Nexys3 FPGA development board.
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


latex/  
------

Contains the report and documentation.

vhdl/
-----  

Contains the actual code, written in what should be portable VHDL,
it includes a test bench.

vm/
---   

Contains virtual machine, simulation of the H2 CPU.


EOF
