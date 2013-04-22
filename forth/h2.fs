\ ==============================================================================
\ H2 assmebly. Written in Forth.
\ @author         Richard James Howe.
\ @copyright      Copyright 2013 Richard James Howe.
\ @license        LGPL      
\ @email          howe.rj.89@googlemail.com
\ ==============================================================================

\ ==============================================================================
\ Defining constants
\ ==============================================================================

\ Hello World!
\ 72 101 108 108 111 32 87 111 114 108 100 33 

\ Default Register assignment

242 constant o_vgaCtrlDefault \ 11110010, VGA control register set to this.

\ Outputs
0 constant o_7seg
1 constant o_ledS
2 constant o_vgaCursor
3 constant o_vgaCtrl
4 constant o_vgaTxtAddr
5 constant o_vgaTxtDin

\ Inputs
0 constant i_buttons
1 constant i_switches
2 constant i_vgaTxtDout

\ ==============================================================================
\ Word definitions.
\ ==============================================================================

: [SETUP]
    o_vgaCtrlDefault lit
    o_vgaCtrl lit 
    _output
;

: [LED]
    o_ledS lit _output
;

: [SWITCH]
    i_switches lit _input
;

\ ==============================================================================
\ Begin program loop.
\ ==============================================================================
start
[SETUP]
label begin
    [SWITCH]
    [LED]
begin jmp
stop
