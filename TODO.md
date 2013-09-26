forth/
======

* FORTH word, change current vocabulary. 

* Improve assembler.

Loops and if statements


* It is possible that I could implement the
  Virtual machine here as well.  

vhdl/
=====

* Pseudo terminal for the system.
  - <https://groups.google.com/forum/?_escaped_fragment_=msg/comp.lang.vhdl/vu9fVTdvSP0/0mmztHvZfDsJ#!msg/comp.lang.vhdl/vu9fVTdvSP0/0mmztHvZfDsJ>

* Better and more tested peripherals:

  - Generic Port peripherals
  - Handling of 7-segment displays
  - Test the DAC

* Work on input/output more.

* Implement simple bootloader as firmware.

* top\_level.vhd

* VGA and UART top levels and testing.

Move out the I/O section into its own module.

What might be interesting is using some spare
dual port RAM as a UART buffer, and reading from
the buffers instead of handling the UART directly.


vm/
===

* Implement peripherals
* Move to Forth

ALL:
====

* Documentation.

* Edit all modules so they have a uniform naming
convention and style.

* Add my public keys to the repo.
forth/
  * FORTH word, change current vocabulary. 
  * Improve assembler.
    - Loops and if statements
  * It is possible that I could implement the
    Virtual machine here as well.  

Old TODO:

vhdl/

  * Work on input/output more.
  * Implement simple bootloader as firmware.
  * top\_level.vhd
    > Move out the I/O section into it's own module.
    > What might be interesting is using some spare
    dual port RAM as a UART buffer, and reading from
    the buffers instead of handling the UART directly.
vm/

  * Implement virtual machine?

latex/

  * Poster.
  * Thesis.
ALL:

  * Documentation.
  * Edit all modules so they have a uniform naming
  convention and style.
  * Add my public keys to the repo.
