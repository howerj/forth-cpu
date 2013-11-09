## testbench.md

1) User types into and receives output via the interpreter.
2) Interpreter runs within the H2 Virtual Machine and system simulation.
3) H2 Virtual Machine spits out a file for verification:
      x      ... y      ...
      000100 ... 000101 ...
4) VHDL model reads in this file line by line, compares current state of
the VHDL model to that of the virtual machine for correctness.

This is a *much* better way of doing things than via a ptty written
directly in VHDL as it is *faster*.
