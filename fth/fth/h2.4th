\ ==============================================================================
\ H2 assmebly. Written in Forth.
\ @author         Richard James Howe.
\ @copyright      Copyright 2013 Richard James Howe.
\ @license        LGPL      
\ @email          howe.r.j.89@gmail.com
\ ==============================================================================

\ ==============================================================================
\ Defining constants
\ ==============================================================================

\ Hello World!
\ 72 101 108 108 111 32 87 111 114 108 100 33 

\ Default Register assignment

122 constant o_vgaCtrlDefault \ 01111010, VGA control register set to this.

\ Outputs
\ 0x6000 - 0x7FFF
0  3 13 lshift or constant o_7seg
1  3 13 lshift or constant o_ledS
2  3 13 lshift or constant o_vgaCursor
3  3 13 lshift or constant o_vgaCtrl
4  3 13 lshift or constant o_vgaTxtAddr
5  3 13 lshift or constant o_vgaTxtDin
6  3 13 lshift or constant o_vgaWrite
7  3 13 lshift or constant o_uartWrite
8  3 13 lshift or constant o_uartStbWrite
9  3 13 lshift or constant o_uartAckDout

\ Inputs
\ 0x6000 - 0x7FFF
0  3 13 lshift or constant i_buttons
1  3 13 lshift or constant i_switches
2  3 13 lshift or constant i_vgaTxtDout
3  3 13 lshift or constant i_uartRead
4  3 13 lshift or constant i_uartAckWrite
5  3 13 lshift or constant i_uartStbDout

\ ==============================================================================
\ Word definitions.
\ ==============================================================================

\ Interrupt Service Routing jump table
 label isr_0 4 jmp
 label isr_1 isr_0 jmp
 label isr_2 isr_0 jmp
 label isr_3 isr_0 jmp
\ End table!

: [SETUP]
    o_vgaCtrlDefault lit
    o_vgaCtrl lit 
    _!
;

: [LED]
    o_ledS lit _!
;

: [SWITCH]
    i_switches lit _@
;

\ ==============================================================================
\ Begin program loop.
\ ==============================================================================

start
  [SETUP]

\ Small test prog
  _togglei _depth \ Set interrupt enable to '1'.
  _swapbytes


  32767 lit
  _dup
  _dup
  _dup

  _+
  _+
  _+

  1 lit
  1 lit
  _=


  256 lit
  label main
      2 lit
      _-
\    [SWITCH]
\    [LED]
  main jmp
stop 

