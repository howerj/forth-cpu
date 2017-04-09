\ This program is written in a pseudo Forth like language, it is *not* Forth
\ and does not behave like it, it just looks like it. This should be thought
\ of as assembly code and not Forth.

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
constant o8SegLED_0_1  0x600b 
constant o8SegLED_2_3  0x600c

\ Inputs: 0x6000 - 0x7FFF
constant iButtons      0x6000 
constant iSwitches     0x6001
constant iVgaTxtDout   0x6002 
constant iUartRead     0x6003 
constant iUartAckWrite 0x6004 
constant iUartStbDout  0x6005 
constant iAscii_new    0x6006 
constant iAscii_char   0x6007 

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
constant vgaInit       122

constant cursor        1024


init:
	vgaInit oVgaCtrl ! \ Turn on VGA monitor
start:

	begin 
		iSwitches  @ oLeds ! \ Set LEDs to switches
		iAscii_new @         \ Wait for a character
	until 

	cursor      @ oVgaTxtAddr !  \ Set index into VGA memory
	iAscii_char @ oVgaTxtDin  !  \ Character to write
	            0 oVgaWrite   !  \ Perform write

	cursor @ 1 + cursor !        \ Increment cursor value

	cursor @ oVgaCursor !        \ Update cursor position

branch start


: x 2 + ;

\ : 2+ 2 + ;
\ : 3+ 3 + ;
