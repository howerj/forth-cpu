## asm.md

Assembler for the H2 CPU architecture.

###  Options

    Usage: ./asm.pl (-(h|i|o|t|x|b|v)+( +filename)*)*

  Options:

  --, ignored
  -h, print this message and quit.
  -i, next argument the input file to be assembled.
  -o, next argument is the output file we generate.
  -t, next argument is a temporary file we use.
  -x, output file in base 16 (hexadecimal).
  -b, output file in base 2  (binary).
  -v, increase verbosity level.

* Author:
 - Richard James Howe
* Email (bug reports to):
 - <howe.r.j.89@gmail.com>

### Assembler overview

The assembler is targets the H2 CPU, which is stack based, all instructions take
their arguments off the stack and have no operands which is reflected in the
assembler; only jump instructions take arguments (which are assembled to a
single instruction) and higher level assembly directives.

Two passes are needed to construct the executable, the first:

* Reads in all the macro definitions
* Expands all macro definitions
* Evaluates all compile time expressions
* Evaluates and expands out all "if" statements
* Expands out includes
* Counts the number of instructions
* Calculates the position of each label in the final executable

The result is sent into a temporary file on the disk which is then read back in
on the second pass.

The second pass should contain only labels and instructions which get turned
into the actual output.

### Assembler directives
  
  Macros and if statements can be nested indefinitely. 

  "$var" or "var" represents a place holder for a variable name. "$macro"
  represents a macro name.

  * %if $var

  If $var evaluates to true (not zero), then anything in between this statement
  and the next %elsif, %else or %endif is put into the output file, any other
  case from then on are discarded. 

  * %ifdef $var or $macro

  If either $var or $macro are defined select anything between this statement
  and either the next %else or %end if. Any other statements are discarded.

  * %ifndef $var or $macro

  Like %ifdef, but if *not* defined.

  * %elsif $var

  If $var evaluates to true (not zero), then anything in between this statement
  and the next %elsif, %else or %endif is put into the output file, any other
  case from then on are discarded.

  * %else

  If hit, anything between this and the next %else statement unconditionally for
  processing.

  * %endif

    Ends an %if/%else/... sequence.

  * %macro var

  * %endmacro

    Ends a macro.

  * %include
  
  Includes another file for processing.

  * $var "expression"

    Expression can contain other variables.

  * var
    
    Either a macro or a value. 

### H2 CPU overview

The H2 CPU is a stack based CPU, it has two stacks; a variable stack and a
return stack. The instruction set is dense with nearly every possible state
corresponding to an instruction, although most of these are to do with loading
literals.

The CPU architecture is described in VHDL and runs on an FPGA (made by Xilinx)
and is tightly integrated with the on board Dual Port RAM. An instruction can be
fetched and a read or write to that memory can be performed in a single cycle.
The memory is 8 Kibi Words in size and a word is 16 bits.

The CPU exclusively works with 16 bit values and the entire system is tailored
for this.

### H2 Instructions

      dup
      over
      invert
      +
      -
      1-
      equal
      and
      or
      xor
      swap
      nip
      drop
      exit
      rshift
      lshift
      >r
      r>
      r@
      @
      !
      *
      depth
      interrupts
      swapbytes
      dptr

