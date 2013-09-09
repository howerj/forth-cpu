## The Plan

* Test Benches!
  - I should make much better test benches,
  read data from a file, output it to a terminal
  or a file.
  - Generic test benches!
* Wishbone compliance
  - All HDL components should be wishbone compliant
* Replace Forth?
  - Should I replace my Forth interpreter with a
  Perl interpreter for assembly?
* Coding standards
  - Come up with naming convention and stick with it.
  - Create better test benched.
* Documentation
  - LaTeX book and markdown help files.
  - Produce a full data sheet.
    - Automatic generation of certain sections of documentation
    from code?
* Implement *portable* VHDL modules for the following:
  - Ethernet/UDP <http://www.fpga4fun.com/10BASE-T2.html>.
  - Logic Analyzer.
  - Digital to Analogue (DAC) front end.
  - Analogue to Digital (ADC) front end.
  - Fast Fourier Transform (FFT) core (8,16,64 or greater point), 8-bit.
  - Pulse Width Modulation (PWM) channels.
  - CORDIC Module
  - PS/2 Keyboard, or homemade keyboard.
  - Better VGA driver.
  - VHDL building block library:
    - Generic flip flops, timers, registers, muxes, ...
* Firmware:
  - Simple boot loader
  - Forth based interpreter
  - Expand interpreter into operating system
* Tool chain and simulation
  - An accurate Virtual Machine, including peripheral simulation, in C
  *and* VHDL
  - A C or Pascal compiler.
* Forth
  - Implement Forth on the device
  - Use "Howe Forth" (for desktops) for more of the build process?
* GTKwave
  - Advance debugging scripts using the Tool Command Language (TCL)
