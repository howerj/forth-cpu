\ This program is written in a pseudo Forth like language, it is *not* Forth
\ and does not behave like it, it just looks like it. This should be thought
\ of as assembly code and not Forth.
\ 
\ For a grammar of the language look into the file "h2.c". 
\ 
\ Execution begins at a label called "start". 

\ Outputs: 0x6000 - 0x7FFF
\ NB. 0x6000 itself is not used.
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


\ Inputs: 0x6000 - 0x7FFF
constant iButtons      0x6000 
constant iSwitches     0x6001
constant iVgaTxtDout   0x6002 
constant iUartRead     0x6003 
constant iUartAckWrite 0x6004 
constant iUartStbDout  0x6005 
constant iPs2New       0x6006 
constant iPs2Char      0x6007 

\ Interrupt service Routine: Memory locations
constant isrReset      0
constant isrClock      1
constant isrUartAck    2
constant isrUartStb    3
constant isrUnused03   4
constant isrUnused04   5
constant isrUnused05   6
constant isrUnused06   7

\ Initial value of VGA
\ BIT     MEANING
\  7   -  Select high fonts
\  6   -  Enable VGA
\  5   -  Cursor enable
\  4   -  Cursor blinks
\  3   -  Cursor mode
\  2   -  Blue
\  1   -  Green 
\  0   -  Blue 
constant vgaInit       122 \ 0x007A 
constant vgaX          80
constant vgaY          40

\ Initial value of timer
\ BIT     MEANING
\ 15   -  Enable Timer
\ 14   -  Reset Timer Value immediately
\ 13   -  Interrupt Enable
\ 12-0 -  Value to compare against
constant timerInit     0x8032

\ Place a variable here
constant cursor        1024

\ TODO: 
\ * Function for displaying numbers on the display
\ * key? key emit and a VGA driver
\ * Bootloader 
\ * Minimal Forth interpreter

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

start:
	vgaInit   oVgaCtrl   ! \ Turn on VGA monitor
	timerInit oTimerCtrl ! \ Enable timer

	\ This is buggy at the moment
	1         o8SegLED_0 !
	2         o8SegLED_1 !
	1         o8SegLED_2 !
	1         o8SegLED_3 !

nextChar:

	begin 
		iSwitches @ oLeds !  \ Set LEDs to switches
		iPs2New   @          \ Wait for a character
	until 

	cursor   @ oVgaTxtAddr !     \ Set index into VGA memory
	iPs2Char @ oVgaTxtDin  !     \ Character to write
	         0 oVgaWrite   !     \ Perform write

	cursor   @ 1 + cursor  !     \ Increment cursor value
	\ 1 cursor +!

	cursor   @ oVgaCursor  !     \ Update cursor position

branch nextChar



\ : 2+ 2 + ;
\ : 3+ 3 + ;
