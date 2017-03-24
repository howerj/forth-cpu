# Forth computing system

Author:             

* Richard James Howe.

Copyright:          

* Copyright 2013-16 Richard James Howe.

License:            

* LGPL

Email:              

* howe.r.j.89@googlemail.com

## Introduction

This project implements a small stack computer tailored to executing Forth
based on the [J1][] CPU.

## To Do

* Fix bugs in:
	* Load / Store
	* UART
* Make a bootloader/program loader
* Get keyboard working
* Implement Forth interpreter on device
* Memory interface to Nexys 3 board on board memory
* CHIP-8 emulator
* Redo Thesis and other documentation
* Add more assertions into the core
* Fix timing issues in load/store to block ram
* Change license to MIT to files that can be changed.
* Make an emulator for the H2 Core in C.
  - Start of with a simple emulator with UART simulated
  with fgetc/fputc (this would require putting the terminal
  in raw mode first).
  - Work on an OpenGL version that emulators the screen.
* Work on porting my [PL/0][] compiler so it can emit code
for the H2 core. This should speed up development a lot.
* A graphics mode for the VGA core could be made, this
would allow address the screen as an array of pixels.

## Ideas

* A multicore version could be made, one core could be used
for graphics processing, for example.

## Forth

* The on board memory could be linked up to the Forth block
word set.
* Most of the Forth code could be taken from my [libforth][]
project.

[J1]: http://www.excamera.com/sphinx/fpga-j1.html
[PL/0]: https://github.com/howerj/pl0
[libforth]: https://github.com/howerj/libforth/
