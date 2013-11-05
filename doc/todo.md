### General

* Create different test benches:

  - Interactive test bench; type and input is sent to the model.
  - Automatic verification against working model (H2 Virtual machine)
  - Feature testing test bench; for simple tests in experimental
  branches.

* Instead of:

    -- synthesis translate_on
      ... debugging code
    -- synthesis translate_off

  Could I find something more portable, or go all the way and use
  a preprocessor (which would also solve the "locally static" restriction
  problem in making my code *very* generic.

* Drastically improve the VM so it can be used to implement a test
interpreter.

* Coding standards

Bring everything in line with the same coding standards.

* Automatic test bench generation:

  - Create a fast virtual machine in C.
  - Run a constrained random set of instructions on that processor.
  - Save the random input and the virtual machines state
  - Feed the state and the random input into a VHDL test bench.
  - Compare the results with assertions, they should be equal.

* PTTY testbench:

  - A Pseudo Terminal driven test bench that the user can type
  into, and the H2 Core will process the results fed in over a
  simulated UART

### forth/

* FORTH word, change current vocabulary. 

* Improve assembler.

Loops and if statements


* It is possible that I could implement the
  Virtual machine here as well.  

### vhdl/

* Add instruction to set depth

* PS/2 Keyboard

* 16 bit floating point unit? Half float.

* The memory modules should be made more generic:

  They should accept a file name and a word length.

  There is a lot of structure that is repeated, I should try to minimize it.

* VHDL

  The VGA memory (font and text buffer) as well as any DAC memories
  should be memory mapped onto the CPUs memory space.


* I should probably have some kind of Data Pointer / Program Pointer to
overcome address limitations.

* Pseudo terminal for the system.
  - <https://groups.google.com/forum/?_escaped_fragment_=msg/comp.lang.vhdl/vu9fVTdvSP0/0mmztHvZfDsJ#!msg/comp.lang.vhdl/vu9fVTdvSP0/0mmztHvZfDsJ>

* Better and more tested peripherals:

  - Generic Port peripherals
  - Handling of 7-segment displays
  - Test the DAC

* Work on input/output more.

* Implement simple bootloader as firmware.

* top\_level.vhd

* TEA/XTEA/XXTEA cryptographic primitves:
  - Use as pseudo random number generator (non cryptographic)

* VGA and UART top levels and testing.

Move out the I/O section into its own module.

What might be interesting is using some spare
dual port RAM as a UART buffer, and reading from
the buffers instead of handling the UART directly.


### ALL:

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

* Superoptimizer targeting my CPU?
 
<https://en.wikipedia.org/wiki/Superoptimization>

* Compiler targeting my assembler?

<https://en.wikipedia.org/wiki/PL/0>
<https://en.wikipedia.org/wiki/Recursive_descent_parser>
Old TODO:

* Implement a simple Unix on the board.

* Improve memory addressing.

vhdl/

  * Work on input/output more.
  * Implement simple bootloader as firmware.
  * top\_level.vhd
    > Move out the I/O section into it's own module.
    > What might be interesting is using some spare
    dual port RAM as a UART buffer, and reading from
    the buffers instead of handling the UART directly.

ALL:

  * Documentation.
  * Edit all modules so they have a uniform naming
  convention and style.
  * Add my public keys to the repo.
