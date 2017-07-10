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

.mode 1 ( Turn word header compilation and optimization on)


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
variable span 0  ( Hold character count received by expect   )
variable #tib 0  ( Current count of terminal input buffer    )
variable tib-buf 0 ( ... and address )
.set tib-buf $pc
.allocate 80

variable >in     0 ( Hold character pointer when parsing input )
variable state   0 ( compiler state variable )

variable '?key   0 ( execution vector of ?key,   default to ?rx.)
variable 'emit   0 ( execution vector of emit,   default to tx!)
variable 'expect 0 ( execution vector of expect, default to 'accept'.)
variable 'tap    0 ( execution vector of tap,    default to ktap.)
variable 'echo   0 ( execution vector of echo,   default to tx!.)
variable 'prompt 0 ( execution vector of prompt, default to '.ok'.)
variable OK      "ok"
( ======================== System Variables ================= )

( ======================== Forth Kernel ===================== )

( Add the assembler words to the dictionary: certain built in words
will need an in-line bit, as well as an immediate bit )
.built-in

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
: ?rx ( -- c -1 | 0 : read in a character of input from UART )
	iUart @ 0x0100 and 0= 
	if
		0x0400 oUart ! iUart @ 0xff and -1
	else
		0
	then ;

: 40ns ( n -- : wait for 'n'*40 nanoseconds + 30us )
	begin dup while 1- repeat drop ;

( @todo determine the correct value for this magic number by instruction
counting )
: ms ( n -- : wait for 'n' milliseconds )
	for 24940 40ns next ; 

( @todo loop until TX FIFO is not full, a bug in the VHDL prevents this from
working )
: tx! ( c -- : write a character to UART )
	0x2000 or oUart ! 1 ms ; 

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
: 3drop 2drop drop ;
: bl 32 ;
: within over - >r - r> u< ; ( u lo hi -- t )
: not -1 xor ;
: dnegate not >r not 1 um+ r> + ; ( d -- d )
: d= ( d d -- f )
	>r swap r> = >r = r> = ;
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

: last pwd @ ;

: uart-write ( char -- bool : write out a character )
	0x2000 or oUart ! 1 ; \ @todo Check that the write succeeded by looking at the TX FIFO

: emit ( char -- : write out a char )
	'emit @execute ;

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

( @todo Test cmove and the following words )

: cmove ( b b u -- )
	for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ;

: fill ( b u c -- )
	swap for swap aft 2dup c! 1+ then next 2drop ;

: -trailing ( b u -- b u )
	for aft bl over r@ + c@ <
		if r> 1+ exit then then
	next 0 ;

: pack$ ( b u a -- a ) \ null fill
	aligned  dup >r over
	dup 0 2 um/mod drop
	- over +  0 swap !  2dup c!  1 + swap cmove  r> ;

: tib ( -- a : terminal input buffer )
	#tib cell+ @ ;

: ^h ( b b b -- b b b ) \ backspace
  >r over r> swap over xor
  if  8 'echo @execute
     32 'echo @execute \ destructive
      8 'echo @execute \ backspace
  then ;

: tap ( bot eot cur c -- bot eot cur )
  dup 'echo @execute over c! 1 + ;

: ktap ( bot eot cur c -- bot eot cur )
  dup 13 xor
  if 8 xor if bl tap else ^h then exit
  then drop swap drop dup ;

: key? '?key @execute ;
: key begin key? until ;

: accept ( b u -- b u )
   over + over
   begin 2dup xor
   while  key  dup bl -  95 u<
     if tap else 'tap @execute then
   repeat drop  over - ;
 
: expect ( b u -- ) 'expect @execute span ! drop ;

: query ( -- )
  tib 80 'expect @execute #tib !  drop 0 >in ! ;

: allot cp +! ;

: , here dup cell+ cp ! ;  

: =string ( a1 u2 a1 u2 -- f : string equality )
	>r swap r> ( a1 a2 u1 u2 ) 
	over <> if 3drop  0 exit then
	dup  0= if 3drop -1 exit then
	1-
	for ( a1 a2 )
		2dup c@ swap c@ <> if 2drop rdrop 0 exit then
		1+ swap 1+ 
	next 2drop -1 ;

: nfa ( pwd -- nfa : move to name field address)
	cell+ ;

: cfa ( pwd -- cfa : move to code field address )
	nfa dup
	count nip + ;

: 2rdrop
	r> rdrop rdrop >r ;

: find ( a u -- pwd -1 | 0 : find a word in the dictionary )
	>r >r
	last
	begin
		dup
	while
		dup nfa count r> r> 2dup >r >r =string
		if rdrop rdrop -1 exit then
		@
	repeat 
	rdrop rdrop drop 0 exit ;

: words ( -- : list all the words in the dictionary )
	last
	begin
		dup
	while
		dup nfa count type space @
	repeat drop cr ;

: .base ( -- ) base @ decimal dup . base  ! ;

: nuf? ( -- f ) key? dup if 2drop key 13 = then ;

: ?exit if rdrop then ;

: decimal? ( c -- f : is character a number? )
	48 58 within ; ( '0' = 48, 58 = ':', or 9+1 )

: lowercase? ( c -- f : is character lower case? )
	[char] a [char] { within ; ( 'z' + 1 = '{' )

: uppercase? ( C -- f : is character upper case? )
	[char] A [char] [ within ; ( 'Z' + 1 = '[' )

: >upper ( c -- C : convert char to uppercase iff lower case )
	dup lowercase? if bl xor then ;

: >lower ( C -- c : convert char to lowercase iff upper case )
	dup uppercase? if bl xor then ;

: numeric? ( char -- n|-1 : convert character in 0-9 a-z range to number )
	dup lowercase? if 97 - 10 + exit then ( 97 = 'a' )
	dup decimal?   if 48 -      exit then ( 48 = '0' )
	drop -1 ;

: number? ( char -- bool : is a character a number in the current base )
	>lower numeric? base @ u< ;

: >number ( n c-addr u -- n c-addr u : convert string )
	begin
		( get next character )
		2dup >r >r drop c@ dup number? ( n char bool, R: c-addr u )
		if   ( n char )
			swap base @ * swap numeric? + ( accumulate number )
		else ( n char )
			drop
			r> r> ( restore string )
			exit
		then
		r> r> ( restore string )
		1 /string dup 0= ( advance string and test for end )
	until ;

( @todo suppress ok prompt when in compiling mode ) 
: .ok OK count type space ;

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

variable cursor 0  ( index into VGA text memory )

: vga! ( n a -- : write to VGA memory and adjust cursor position )
	 0xE000 or ! ;

: page
	0 cursor !
	0x1FFF for bl r@ vga! next ;
	
\ : 40* dup 5 lshift swap 3 lshift + ;
\ : 80* dup 6 lshift swap 4 lshift + ;

\ @todo Optimize and extend (handle tabs, back spaces, etcetera )
: terminal ( n a -- a : act like a terminal )
	swap
	dup dup 10 = swap 13 = or if drop vgaX / 1+ dup 0 swap at-xy vgaX * exit then
	swap vgaTextSize mod tuck dup 1+ vgaX /mod at-xy vga! 1+ ;

: led! ( n -- : display a number on the LED 8 display )
	o8SegLED ! ;

variable uart-read-count 0

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

: init
	vgaInit   oVgaCtrl   ! \ Turn on VGA monitor
	timerInit oTimerCtrl ! \ Enable timer
	cpu-id led! ;
	\ 0x00FF oIrcMask !
	\ 1   seti ;

( The start up code begins here, on initialization the assembler
jumps to a special symbol "start".

@todo This special case symbol should be removed by adding
adequate assembler directives )

\ : 2. swap . . ;

variable welcome "H2 Forth:"


start:
	 init

	\ boot

	welcome count type cr words

	.break

nextChar:
	\ query tib #tib @ 2dup 2. cr type cr branch nextChar
	\ query 0 tib #tib @ >number . . . cr branch nextChar
	\ query tib #tib @ find . cr branch nextChar
	\ 0 counter: 1000 ms 1+ dup led! branch counter

	begin
		iSwitches @ 0xff and oLeds !  \ Set LEDs to switches
		key?                 \ Wait for UART character
	until
	dup emit cursor @ terminal 
	dup cursor @ u< if drop page else cursor ! then

	uart-read-count 1+!
	uart-read-count @ led!

branch nextChar

( ======================== User Code ======================== )

.set pwd $pwd
.set cp  $pc

.set '?key   ?rx    ( execution vector of ?key,   default to ?rx.)
.set 'emit   tx!    ( execution vector of emit,   default to tx!)
.set 'expect accept ( execution vector of expect, default to 'accept'.)
.set 'tap    ktap   ( execution vector of tap,    default the ktap.)
.set 'echo   tx!    ( execution vector of echo,   default to tx!.)
.set 'prompt .ok    ( execution vector of prompt, default to '.ok'.)



