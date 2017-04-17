( This program is written in a pseudo Forth like language,
it is not Forth and does not behave like it, it just looks 
like it. This should be thought of as assembly code and 
not Forth.

For a grammar of the language look into the file "h2.c".

Execution begins at a label called "start".

TODO:
* Function for displaying numbers on the display
* VGA driver
* Hex number printer
* Bootloader 
* Minimal Forth interpreter )




( ======================== System Constants ================= )
( Outputs: 0x6000 - 0x7FFF
NB. 0x6000 itself is not used. )

constant oNotUsed      0x6000  
constant oLeds         0x6001  
constant oVgaCursor    0x6002  
constant oVgaCtrl      0x6003  
constant oVgaTxtAddr   0x6004  
constant oVgaTxtDin    0x6005  
constant oVgaWrite     0x6006  
constant oUartWrite    0x6007  
constant oUartStbWrite 0x6008  
constant oUartAckDout  0x6009  
constant oTimerCtrl    0x600a
constant o8SegLED_0    0x600b 
constant o8SegLED_1    0x600c
constant o8SegLED_2    0x600d 
constant o8SegLED_3    0x600e


( Inputs: 0x6000 - 0x7FFF )
constant iButtons      0x6000 
constant iSwitches     0x6001
constant iVgaTxtDout   0x6002 
constant iUartRead     0x6003 
constant iUartAckWrite 0x6004 
constant iUartStbDout  0x6005 
constant iPs2New       0x6006 
constant iPs2Char      0x6007 
constant iTimerCtrl    0x6008

( Interrupt service Routine: Memory locations )
constant isrReset      0
constant isrClock      1
constant isrUartAck    2
constant isrUartStb    3
constant isrUnused03   4
constant isrUnused04   5
constant isrUnused05   6
constant isrUnused06   7

: noop ;
: reset >r drop branch 8 ;
( @todo fix setting isr 0: perhaps by moving
interrupts to the end of main memory: The problem is caused by the fact
that the reset is not propagated to the h2 core in the VHDL simulation )
\ isr reset isrReset 
isr noop isrClock
isr noop isrUartAck
isr noop isrUartStb
isr noop isrUnused03
isr noop isrUnused04
isr noop isrUnused05
isr noop isrUnused06

( ======================== System Constants ================= )

( ======================== User Code ======================== )

( Initial value of VGA
  BIT     MEANING
  7   -  Select high fonts
  6   -  Enable VGA
  5   -  Cursor enable
  4   -  Cursor blinks
  3   -  Cursor mode
  2   -  Blue
  1   -  Green 
  0   -  Blue )
constant vgaInit       122 \ 0x007A 

constant vgaX          80
constant vgaY          40
constant vgaTextSize   3200

( Initial value of timer
  BIT     MEANING
  15   -  Enable Timer
  14   -  Reset Timer Value immediately
  13   -  Interrupt Enable
  12-0 -  Value to compare against )
constant timerInit     0x8032

variable cursorX 0  ( x component of cursor )
variable cursorY 0  ( y component of cursor )
variable cursorT 0  ( index into VGA text memory )

: 1+ 1 + ;
: 2+ 2 + ;
: 2/ 1 rshift ;
: 2* 1 lshift ;
: >= < invert ;
: >  swap < ;
: u> swap u< ;
: u>= u< invert ;
: <> = invert ;
\ : 0<> 0= invert ;
: 2dup over over ;
: 2drop drop drop ;
: negate invert 1+ ;
: - negate + ;
: tuck swap over ;
: +! tuck @ + swap ! ;
: 1+! 1 swap +! ;
\ @todo AT-XY
: y1+ cursorY 1+! cursorY @ vgaY u>= if 0 cursorY ! then ;
: x1+ cursorX 1+! cursorX @ vgaX u>= if 0 cursorX ! y1+ then ;
: cursorT1+ cursorT 1+! cursorT @ vgaTextSize u>= if 0 cursorT ! then ;

: uart-write ( char -- bool : write out a character ) 
	begin iUartAckWrite @ 0 = until oUartWrite ! 1 oUartStbWrite ! iUartAckWrite @ ; 

: key?
	iUartStbDout @ ;

: uart-read  ( -- char : blocks until character read in )
	begin key? 0 = until iUartRead  @ 1 oUartAckDout  ! ;

: key ( -- char : read in a key, echoing to output )
	uart-read dup uart-write drop ;

: char
	uart-read ;

: emit
	uart-write drop ;


: led ( n -- : display a number on the LED 8 display )
	dup 12 rshift o8SegLED_0 !
	dup  8 rshift o8SegLED_1 !
	dup  4 rshift o8SegLED_2 !
	              o8SegLED_3 ! ;

constant bootstart 1024
constant programsz 5120 ( bootstart + 4096 )
variable readin    0

: boot
	bootstart readin !
	begin 
		key 8 lshift ( big endian )
		key
		or readin !
		readin 1+!
		readin @ programsz u>=
	until 
	r> drop
	branch bootstart ;

: init
	vgaInit   oVgaCtrl   ! \ Turn on VGA monitor
	timerInit oTimerCtrl ! \ Enable timer
	0xFEDC led ;

( The start up code begins here, on initialization the assembler
jumps to a special symbol "start". )
start:
	 init

nextChar:
	\ boot

	begin 
		iSwitches @ oLeds !  \ Set LEDs to switches
		\ iPs2New @          \ Wait for PS/2 a character
		key?                 \ Wait for UART character
	until 
	cursorT   @ oVgaTxtAddr !    \ Set index into VGA memory
	key         oVgaTxtDin  !    \ Character to write
	\ iPs2Char @ oVgaTxtDin  !   \ Character to write
	          1 oVgaWrite   !    \ Perform write

	x1+
	cursorT1+
	cursorT @ led

	cursorX @ cursorY @ 8 lshift or oVgaCursor !

branch nextChar

( ======================== User Code ======================== )


