( This program is written in a pseudo Forth like language,
it is not Forth and does not behave like it, it just looks
like it. This should be thought of as assembly code and
not Forth.

A lot of the code has been taken verbatim from "The Zen
of eForth by C. H. Ting".

For a grammar of the language look into the file "h2.c".

Execution begins at a label called "start".

TODO:
* Function for displaying numbers on the display
* VGA driver
* Hex number printer
* Bootloader
* Minimal Forth interpreter 
* Add built in words to the dictionary
* Turn this into a literal file
* Add assembler directives for setting starting position of
program counter and the like )

( ======================== System Constants ================= )

.mode 1 ( Turn word header compilation on )


( Outputs: 0x6000 - 0x7FFF )
constant oUart         0x6000
constant oLeds         0x6001
constant oTimerCtrl    0x6002
constant oVgaCursor    0x6003
constant oVgaCtrl      0x6004
constant o8SegLED      0x6005
constant oIrcMask      0x6006

( Inputs: 0x6000 - 0x7FFF )
constant iUart         0x6000
constant iSwitches     0x6001
constant iTimerCtrl    0x6002
constant iTimerDin     0x6003
constant iVgaTxtDout   0x6004
constant iPs2          0x6005

( Interrupt service Routine: Memory locations )
constant isrNone           0
constant isrRxFifoNotEmpty 1
constant isrRxFifoFull     2
constant isrTxFifoNotEmpty 3
constant isrTxFifoFull     4
constant isrKbdNew         5
constant isrTimer          6
constant isrBrnLeft        7


( ======================== System Constants ================= )

( ======================== System Variables ================= )

variable pwd  0  ( Present Word Variable: Set at end of file )
variable cp   0  ( Dictionary Pointer: Set at end of file )
variable hld  0  ( Pointer into hold area for numeric output )
variable base 10 ( Current output radix )

( ======================== System Variables ================= )

( ======================== Forth Kernel ===================== )


: ! store drop ;
: 256* 8 lshift ;
: 256/ 8 rshift ;
: sp@ depth ;
: 1+ 1 + ;
: 2+ 2 + ;
: 2/ 1 rshift ;
: 2* 1 lshift ;
: >= < invert ;
: >  swap < ;
: u> swap u< ;
: u>= u< invert ;
: <> = invert ;
: 0<> 0= invert ;
: 0>  0 > ;
: 0<  0 < ;
: 2dup over over ;
: 2drop drop drop ;
: tuck swap over ;
: negate invert 1+ ;
: - negate + ;
: 2- 2 - ;
: +! tuck @ + swap ! ;
: 1+! 1 swap +! ;
: ?dup dup if dup then ;
: execute >r ;
: c@ dup @ swap 1 and if 256/ else 0xff and then ;
: c! 
	swap 0xff and dup 256* or swap
	tuck dup @ swap 1 and 0 = 0xff xor
	>r over xor r> and xor swap ! ;

: !io ; ( Initialize I/O devices )
: ?rx ( -- c 1 | 0 : read in a character of input from UART )
	iUart @ 0x0100 and if 0xff and 1 else drop 0 then ;
: tx! ( c -- : write a character to UART )
	0x2000 or oUart ! ; ( @todo loop until TX FIFO is not full )

: um+ ( w w -- w carry )
	2dup + >r
	r@ 0 >= >r
	2dup and
	0< r> or >r
	or 0< r> and negate
	r> swap ;

( @todo Implement "sp!" and "next" )

( With the built in words defined in the assembler, and the words
defined so far, all of the primitive words needed by eForth should
be available. "doList" and "doLit" do not need to be implemented. )

( ======================== Forth Kernel ===================== )

( ======================== Word Set ========================= )


: cell- 2- ;
: cell+ 2+ ;
: cells 2* ;
: 2! ( d a -- ) tuck ! cell+ ! ;
: 2@ ( a -- d ) dup cell+ @ swap @ ;
: here cp @ ;
: pad here 80 + ;
: @execute @ ?dup if execute then ;
: bl 32 ;
: within over - >r - r> u< ; ( u lo hi -- t )
: not -1 xor ;
: dnegate not >r not 1 um+ r> + ; ( d -- d )
: abs dup 0< if negate then ;
: count ( cs -- c-addr u )
	dup 1+ swap c@ ;
	\ dup c@ swap 1+ swap ;
: rot >r swap r> swap ;
: -rot swap >r swap r> ;
: min  2dup < if drop else nip then ;
: max  2dup > if drop else nip then ;
: >char ( c -- c : convert character to '_' if not ASCII or is control char )
  0x7f and dup 127 bl within if drop [char] _ then ;

: um/mod ( ud u -- ur uq )
	2dup u<
	if negate  15
		for >r dup um+ >r >r dup um+ r> + dup
			r> r@ swap >r um+ r> or
			if >r drop 1 + r> else drop then r>
		next 
		drop swap exit
	then drop 2drop  -1 dup ;

: m/mod ( d n -- r q ) \ floored division
	dup 0< dup >r
	if 
		negate >r dnegate r>
	then 
	>r dup 0< if r@ + then r> um/mod r>
	if swap negate swap then ;

: /mod ( n n -- r q ) over 0< swap m/mod ;
: mod ( n n -- r ) /mod drop ;
: / ( n n -- q ) /mod swap drop ;

: um* ( u u -- ud )
	0 swap ( u1 0 u2 ) 15
	for dup um+ >r >r dup um+ r> + r>
		if >r over um+ r> + then
	next rot drop ;

: * ( n n -- n ) um* drop ;

: m* ( n n -- d )
	2dup xor 0< >r abs swap abs um*  r> if dnegate then ;

: */mod ( n n n -- r q ) >r m* r> m/mod ;
: */ ( n n n -- q ) */mod swap drop ;

: aligned ( b -- a )
	dup 0 2 um/mod drop dup
	if 2 swap - then + ;

: at-xy ( x y -- : set terminal cursor to x-y position )
	256* or oVgaCursor ! ;

: /string ( c-addr u1 u2 -- c-addr u : advance a string u2 characters )
	over min rot over + -rot - ;

: latest pwd @ ;

: uart-write ( char -- bool : write out a character )
	0x2000 or oUart ! 1 ; \ @todo Check that the write succeeded by looking at the TX FIFO

: emit ( char -- : write out a char )
	uart-write drop ;

: cr 10 emit ;
: space bl emit ;

: type ( c-addr u -- : print a string )
 	dup 0= if 2drop exit then
 	begin 
 		swap dup c@ emit 1+ swap 1-
 		dup 0=
 	until 2drop ;

: digit ( u -- c ) 9 over < 7 and + 48 + ;
: extract ( n base -- n c ) 0 swap um/mod swap digit ;

: <# ( -- ) pad hld ! ;

: hold ( c -- ) hld @ 1 - dup hld ! c! ;

: # ( u -- u ) base @ extract hold ;

: #s ( u -- 0 ) begin # dup while repeat ;

: sign ( n -- ) 0< if 45 hold then ;

: #> ( w -- b u ) drop hld @ pad over - ;

: hex ( -- ) 16 base ! ;
: decimal ( -- ) 10 base ! ;

: str   ( n -- b u : convert a signed integer to a numeric string )
	dup >r                ( save a copy for sign )
	abs                   ( use absolute of n )
	<# #s                 ( convert all digits )        
	r> sign               ( add sign from n )
	#> ;                  ( return number string addr and length )

: nchars ( +n c -- ) \ "chars" in eForth, this is an ANS FORTH conflict
  swap 0 max for aft dup emit then next drop ;

: spaces ( +n -- ) bl nchars ;

: .r    ( n +n -- :display an integer in a field of n columns, right justified )
	>r str                ( convert n to a number string )
	r> over - spaces      ( print leading spaces )
	type ;                ( print number in +n column format )

: u.r   ( u +n -- : display an unsigned integer in n column, right justified )
	>r                    ( save column number )
	<# #s #> r>           ( convert unsigned number )
	over - spaces         ( print leading spaces )
	type ;                ( print number in +n columns )

: u.    ( u -- : display an unsigned integer in free format )
	<# #s #>              ( convert unsigned number )
	space                 ( print one leading space )
	type ;                ( print number )

: .     ( w -- : display an integer in free format, preceeded by a space )
	base @ 10 xor         ( if not in decimal mode)
	if u. exit then       ( print unsigned number)
	str space type ;      ( print signed number if decimal)

: ?     ( a -- : display the contents in a memory cell )
	@ . ;    ( very simple but useful command)

: words
	latest
	begin
		dup
	while
		dup cell+ count type space @
	repeat drop cr ;


( ======================== Word Set ========================= )

( ======================== Miscellaneous ==================== )

( Initial value of VGA
  BIT     MEANING
  7   -  Display Next Screen
  6   -  Enable VGA
  5   -  Cursor enable
  4   -  Cursor blinks
  3   -  Cursor mode
  2   -  Red
  1   -  Green
  0   -  Blue )
constant vgaInit       0x7A \ 0x007A

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

( If the VGA display was 64 characters by 16 lines of text 
this cursor logic would be a lot simpler )
: y1+ cursorY 1+! cursorY @ vgaY u>= if 0 cursorY ! then ;
: x1+ cursorX 1+! cursorX @ vgaX u>= if 0 cursorX ! y1+ then ;
: cursorT1+ cursorT 1+! cursorT @ vgaTextSize u>= if 0 cursorT ! then ;

: led ( n -- : display a number on the LED 8 display )
	o8SegLED ! ;

: key?
	iUart @ 0x0100 and ;

variable uart-read-count 0

: uart-read  ( -- char : blocks until character read in )
	begin key? 0= until 0x0400 oUart ! iUart @ 0xff and
	uart-read-count 1+!
	uart-read-count @ led ;

: key ( -- char : read in a key, echoing to output )
	uart-read dup emit ;

: char
	uart-read ;

( ======================== Miscellaneous ==================== )

( ======================== Starting Code ==================== )

\ This boot sequence code is untested, it should also add a checksum
\ function such as a 16-bit Fletcher checksum (see
\ https://en.wikipedia.org/wiki/Fletcher's_checksum ) for more
\ information.
\ 
\ constant bootstart 4096
\ constant programsz 8191 ( bootstart + 4095 )
\ variable readin    0
\ variable bootmsg1  "BOOT: Send 4095 chars"
\ variable bootmsg2  "DONE"
\ 
\ : boot
\ 	bootstart 2* readin !
\	bootmsg1 count type cr  
\ 	begin
\ 		key 256* ( big endian )
\ 		key
\ 		or readin 2* !
\ 		readin 1+!
\ 		readin @ programsz u>=
\ 	until
\ 	r> drop
\	bootmsg2 count type cr
\ 	branch bootstart ;

\ : 40* dup 5 lshift swap 3 lshift + ;
\ : 80* dup 6 lshift swap 4 lshift + ;


: init
	vgaInit   oVgaCtrl   ! \ Turn on VGA monitor
	timerInit oTimerCtrl ! \ Enable timer
	0xCAFE led ;
	\ 0x00FF oIrcMask !
	\ 1   seti ;



( The start up code begins here, on initialization the assembler
jumps to a special symbol "start".

@todo This special case symbol should be removed by adding
adequate assembler directives )

variable welcome "H2 Forth:"
start:
	 init

	\ boot

	welcome count type cr  

	words

	\ @bug This should print 8...0
	9 for r@ . cr next

nextChar:

	begin
		iSwitches @ 0xff and oLeds !  \ Set LEDs to switches
		key? 0=                \ Wait for UART character
	until
	key cursorT @ 0xE000 or !

	x1+
	cursorT1+

	cursorX @ cursorY @ at-xy

branch nextChar

( ======================== User Code ======================== )

\ 0 for dup dup aft key key then over over next


.set pwd $pwd
.set cp  $pc



