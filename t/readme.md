# t/

This folder is for tests. At the moment the tests are for the ANSI terminal
emulator, but code that exercises the eForth interpreter on the running system
could be added.

The file [ansi.fth][] contains a framework for generated text files that
contain ANSI Escape Sequences, which can be used to test that the VT100 module
is working correctly.

[eforth.fth][] contains code that should run on the target.

All of this should be transmitted to the target over a UART.

[ansi.fth]: ansi.fth
[eforth.fth]: eforth.fth
