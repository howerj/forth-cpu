( This program is written in a pseudo Forth like language, it is not
Forth and does not behave like it, it just looks like it. This should
be thought of as assembly code and not Forth.

A lot of the code has been taken verbatim from "The Zen of eForth by
C. H. Ting".

For a grammar of the language look into the file "h2.c".

Execution begins at a label called "start".

TODO:
* Bootloader: A super simple one, preferably on that takes up less than 100
words
* Turn this into a literate file
* This will need to be optimized for size soon, a mechanism for conditional
compilation would help, or simply commenting out code that is not needed
* The maximum value for the return stack depth is 24, out of a possible 32 this
is quite high, this is not during general running though, but is still higher
than desired.
* Make a special version of exit, which does exactly the same as exit but with
a different op-code value. This special version could be compiled in by ';', so
the decompiler knows when a word ends.
* A way to mark words in this file as run time only should be made, this could
cause confusion otherwise
* Turn magic number into constants
* A way of making words with no header would be useful for size purposes
* Does it make more sense for the top three bits of an instruction set
to zero to encode a call or a branch?
* Fix interrupt code
* An automatic test suite for this program would help in development a lot,
it would have to interact with the debugger, a Perl script that deals
with would be the optimal solution.
* eForth passes around pointers to counted strings all over the place,
using a pointer to a counted string takes up less space on the stack and is
generally easier to manipulate - they should be used more.
* Refactoring "find" to accept a counted string would be useful, however
it would mean changing token to copy the string it parses into a temporary
buffer.

* For tiny math routines, look at:
http://files.righto.com/calculator/sinclair_scientific_simulator.html
https://en.wikipedia.org/wiki/Sinclair_Scientific

Forth To Do:
* Strings, Throw/Catch, abort, vocabularies, see, create/does, constant,
variable, make/doer, ...
* Word for printing out the state of the interpreter would help, if a
word specifically for printing out variables is produced this would help
* "-1" is a commonly used constant, however it takes up two instructions,
it is worth turning this into a word which pushes -1 onto the stack, however
the maximum number of items on the stack is already too high )

( ======================== System Constants ================= )

constant cell 2

( The first 8 cells [16 bytes] of memory contain the entry point and interrupt
service routine call locations, we can set the instruction to be run [such as
a jump or a call] by setting the label to it with the ".set" directive. Later
in the program the entry point, the first location in memory, is set to the
start label )
entry:             .allocate cell
isrRxFifoNotEmpty: .allocate cell ( UART RX FIFO not empty )
isrRxFifoFull:     .allocate cell ( UART RX FIFO full )
isrTxFifoNotEmpty: .allocate cell ( UART TX FIFO not empty )
isrTxFifoFull:     .allocate cell ( UART TX FIFO full )
isrKbdNew:         .allocate cell ( New PS/2 Keyboard character )
isrTimer:          .allocate cell ( Timer interrupt )
isrBrnLeft:        .allocate cell ( Left button pressed )

.mode 1   ( Turn word header compilation and optimization off )
.built-in ( Add the built in words to the dictionary )

constant =exit         0x601c ( op code for exit )
constant =invert       0x6600 ( op code for invert )
constant =>r           0x6147 ( op code for >r )
constant =bl           32     ( blank, or space )
constant =cr           13     ( carriage return )
constant =lf           10     ( line feed )
constant =bs           8      ( back space )

constant tib-length    80     ( size of terminal input buffer )
constant pad-length    80     ( pad area begins HERE + pad-length )
constant word-length   31     ( maximum length of a word )

( Outputs: 0x6000 - 0x7FFF )
constant oUart         0x4000 ( UART TX/RX Control register )
constant oLeds         0x4001 ( LEDs )
constant oTimerCtrl    0x4002 ( Timer Control )
constant oVgaCursor    0x4003 ( VGA X/Y Cursor position )
constant oVgaCtrl      0x4004 ( VGA Control )
constant o8SegLED      0x4005 ( 4x7 Segment display )
constant oIrcMask      0x4006 ( Interrupt Mask )
constant oLfsr         0x4007 ( Seed value of LFSR )

( Inputs: 0x6000 - 0x7FFF )
constant iUart         0x4000 ( Matching registers for iUart )
constant iSwitches     0x4001 ( Switch control [on/off] )
constant iTimerCtrl    0x4002 ( Timer control, not really needed )
constant iTimerDin     0x4003 ( Current timer value )
constant iVgaTxtDout   0x4004 ( VGA text output, currently broken )
constant iPs2          0x4005 ( PS/2 keyboard input )
constant iLfsr         0x4006 ( Input from Linear Feedback Shift Register )

( ======================== System Constants ================= )

( ======================== System Variables ================= )

( Execution vectors for changing the behaviour of the program,
they are set at the end of this file )
variable _key?    0  ( "key?" vector )
variable _emit    0  ( "emit" vector )
variable _expect  0  ( "accept" vector )
variable _tap     0  ( "tap" vector, for terminal handling )
variable _echo    0  ( "echo" vector )
variable _prompt  0  ( "prompt" vector )
variable _boot    0  ( "boot" vector )

location OK     "ok" ( used by "prompt" )
variable csp      0  ( current data stack pointer - for error checking )
variable >in      0  ( Hold character pointer when parsing input )
variable state    0  ( compiler state variable )
variable pwd      0  ( Present Word Variable: Set at end of file )
variable cp       0  ( Dictionary Pointer: Set at end of file )
variable hld      0  ( Pointer into hold area for numeric output )
variable base     10 ( Current output radix )
variable span     0  ( Hold character count received by expect   )
variable #tib     0  ( Current count of terminal input buffer    )
location tib-buf  0  ( ... and address )
.set tib-buf $pc     ( set tib-buf to current dictionary location )
.allocate tib-length ( allocate enough for the terminal input buffer )
.allocate cell       ( plus one extra cell for safety )
location word-buf 0  ( transient word buffer starts here )
.allocate 34         ( allocate room for it )

( ======================== System Variables ================= )

( ======================== Forth Kernel ===================== )

: ! store drop ;
: 256* 8 lshift ;
: 256/ 8 rshift ;
: 1+ 1 + ;
: negate invert 1 + ;
: - invert 1 + + ;
\ : 2+ 2 + ;
\ : 2- 2 - ;
: 2/ 1 rshift ;
\ : 2* 1 lshift ;
\ : cell- cell - ;
: cell+ cell + ;
\ : cells 2* ;
: ?dup dup if dup then ;
\ : >= < invert ;
: >  swap < ;
\ : u> swap u< ;
\ : u>= u< invert ;
\ : <> = invert ;
: 0<> 0= invert ;
: 0> 0 > ;
: 0< 0 < ;
: 2dup over over ;
: 2drop drop drop ;
: tuck swap over ;
: +! tuck @ + swap ! ;
: 1+! 1 swap +! ;
: 1-! -1 swap +! ;
: execute >r ;
: c@ dup ( -2 and ) @ swap 1 and if 8 rshift else 0xff and then ;
: c!
	swap 0xff and dup 8 lshift or swap
	swap over dup ( -2 and ) @ swap 1 and 0 = 0xff xor
	>r over xor r> and xor swap ( -2 and ) store drop ;

\ : !io ; ( Initialize I/O devices )
: rx? ( -- c -1 | 0 : read in a character of input from UART )
	iUart @ 0x0100 and 0=
	if
		0x0400 oUart ! iUart @ 0xff and -1
	else
		0
	then ;

40ns: begin dup while 1- repeat drop exit ( n -- : wait for 'n'*40ns + 30us )
: ms for 25000 call 40ns next ; ( n -- : wait for 'n' milliseconds )

: tx! ( c -- : write a character to UART )
	begin iUart @ 0x0800 and until ( Wait until TX FIFO is not full )
	0x2000 or oUart ! ;            ( Write character out )

: um+ ( w w -- w carry )
	over over + >r
	r@ 0 < invert >r
	over over and
	0 < r> or >r
	or 0 < r> and invert 1 +
	r> swap ;

: rp! ( n -- , R: ??? -- ??? : set the return stack pointer )
	begin dup rp@ = 0= while rdrop repeat drop ;

( @todo Implement "sp!" and "next" )

( With the built in words defined in the assembler, and the words
defined so far, all of the primitive words needed by eForth should
be available. "doList" and "doLit" do not need to be implemented. )

( ======================== Forth Kernel ===================== )

( ======================== Word Set ========================= )

\ : 0<= 0> 0= ;
\ : 0>= 0< 0= ;
\ : 2! ( d a -- ) tuck ! cell+ ! ;
\ : 2@ ( a -- d ) dup cell+ @ swap @ ;
: here cp @ ;
: pad here pad-length + ;
: @execute @ ?dup if execute then ;
: 3drop 2drop drop ;
: bl =bl ;
: within over - >r - r> u< ; ( u lo hi -- t )
\ : not -1 xor ;
: dnegate invert >r invert 1 um+ r> + ; ( d -- d )
\ : d=  >r swap r> = >r = r> and ; ( d d -- f )
\ : d<> d= 0= ; ( d d -- f )
: abs dup 0< if negate then ;
: count  dup 1+ swap c@ ; ( cs -- b u )
: rot >r swap r> swap ;
: -rot swap >r swap r> ;
: min over over < if drop else nip then ;
: max over over > if drop else nip then ;
: >char 0x7f and dup 127 =bl within if drop [char] _ then ; ( c -- c )
: tib #tib cell+ @ ;        ( -- a )
: echo _echo @execute ;     ( c -- )
: key? _key? @execute ;     ( -- c -1 | 0 )
: key begin key? until ;    ( -- c )
: allot cp +! ;             ( u -- )
: , here dup cell+ cp ! ! ; ( u -- )
: /string over min rot over + -rot - ; ( b u1 u2 -- b u : advance a string u2 characters )
: last pwd @ ;
: emit _emit @execute ; ( char -- : write out a char )
: toggle over @ xor swap ! ; ( a u -- : xor value at addr with u )
: cr =cr emit =lf emit ;
: space =bl emit ;
: pick ?dup if swap >r 1- pick r> swap exit then dup ; ( @bug does not work for high stack depths - mashes the return stack )
\ : roll  dup 0> if swap >r 1- roll r> swap else drop then ;
: ndrop for aft drop then next ;
: type begin dup while swap count emit swap 1- repeat 2drop ; ( b u -- : print a string )
: _type begin dup while swap count >char emit swap 1- repeat 2drop ; ( b u -- : print a string )
: print count type ;
\ : .base ( -- ) base @ decimal dup . base  ! ;
\ : nuf? ( -- f ) key? dup if 2drop key =cr = then ;
\ : ?exit if rdrop then ;
\ : 2rdrop r> rdrop rdrop >r ;
: decimal? 48 58 within ; ( c -- f : decimal char? )
: lowercase? [char] a [char] { within ; ( c -- f : is character lower case? )
: uppercase? [char] A [char] [ within ; ( c -- f : is character upper case? )
\ : >upper dup lowercase? if =bl xor then ; ( c -- c : convert to upper case )
: >lower  dup uppercase? if =bl xor then ; ( c -- c : convert to lower case )
: nchars swap 0 max for aft dup emit then next drop ; ( +n c -- : emit c n times  ) 
: spaces ( +n -- ) =bl nchars ;
: cmove for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ; ( b b u -- )
: fill swap for swap aft 2dup c! 1+ then next 2drop ; ( b u c -- )
: switch 2dup @ >r @ swap ! r> swap ! ; ( a a -- swap contents )

: um/mod ( ud u -- ur uq )
	2dup u<
	if negate 15
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
: m* 2dup xor 0< >r abs swap abs um* r> if dnegate then ; ( n n -- d )
: */mod ( n n n -- r q ) >r m* r> m/mod ;
: */ ( n n n -- q ) */mod swap drop ;

: aligned ( b -- a )
	dup 0 cell um/mod drop dup
	if 2 swap - then + ;

: digit ( u -- c ) 9 over < 7 and + 48 + ;
: extract ( n base -- n c ) 0 swap um/mod swap digit ;
: <# ( -- ) pad hld ! ;
: hold ( c -- ) hld @ 1 - dup hld ! c! ;
\ : holds begin dup while 1- 2dup + c@ hold repeat 2drop ; ( a u -- )
: # ( u -- u ) base @ extract hold ;
: #s ( u -- 0 ) begin # dup while repeat ;
: sign ( n -- ) 0< if [char] - hold then ;
: #> ( w -- b u ) drop hld @ pad over - ;
: hex ( -- ) 16 base ! ;
: decimal ( -- ) 10 base ! ;

: str   ( n -- b u : convert a signed integer to a numeric string )
	dup >r                ( save a copy for sign )
	abs                   ( use absolute of n )
	<# #s                 ( convert all digits )
	r> sign               ( add sign from n )
	#> ;                  ( return number string addr and length )

\ : .r  >r str r> over - spaces type ; ( n n : print n, right justified by +n )
: u.r >r <# #s #> r> over - spaces type ; ( u +n -- : print u right justified by +n)
: u. <# #s #> space type ; ( u -- : print unsigned number )
: . base @ 10 xor if u. exit then str space type ; ( n -- print space, signed number )

: ? @ . ; ( a -- : display the contents in a memory cell )
\ : 2. swap . . ;

: -trailing ( b u -- b u : remove trailing spaces )
	for 
		aft =bl over r@ + c@ <
			if r> 1+ exit then 
		then
	next 0 ;

: pack$ ( b u a -- a ) \ null fill
	aligned  dup >r over
	dup 0 cell um/mod drop
	- over +  0 swap !  2dup c!  1 + swap cmove  r> ;

: ^h ( bot eot cur c -- bot eot cur )
	>r over r@ < dup
	if
		=bs dup echo =bl echo echo
	then r> + ;

: tap dup echo over c! 1 + ; ( bot eot cur c -- bot eot cur )

: ktap ( bot eot cur c -- bot eot cur )
	dup =cr xor
	if =bs xor 
		if =bl tap else ^h then 
		exit
	then drop swap drop dup ;

: accept ( b u -- b u )
	over + over
	begin
		2dup xor
	while
		key  dup =bl -  95 u<
		if tap else _tap @execute then
	repeat drop over - ;

: expect ( b u -- ) _expect @execute span ! drop ;
: query tib tib-length _expect @execute #tib !  drop 0 >in ! ; ( -- )


: =string ( a1 u2 a1 u2 -- f : string equality )
	>r swap r> ( a1 a2 u1 u2 )
	over xor if 3drop  0 exit then
	for ( a1 a2 )
		aft
			count >r swap count r> xor
			if 2drop rdrop 0 exit then
		then
	next 2drop -1 ;

: address 0x1fff and ; ( a -- a : mask off address bits )
: nfa address cell+ ; ( pwd -- nfa : move to name field address)
: cfa nfa dup count nip + cell + ; ( pwd -- cfa : move to code field address )
: .id nfa print space ; ( pwd -- : print out a word )

logical: 0= 0= exit ( n -- f )
: immediate? @ 0x2000 and call logical ; ( pwd -- f : is immediate? )
: hidden?    @ 0x4000 and call logical ; ( pwd -- f : is hidden? )
: inline?    @ 0x8000 and call logical ; ( pwd -- f : is inline? )

: find ( a -- pwd 1 | pwd -1 | a 0 : find a word in the dictionary )
	>r
	last address
	begin
		dup
	while
		dup nfa count r@ count =string
		if ( found! )
			dup hidden? 
			0= if dup immediate? if 1 else -1 then rdrop exit then 
		then
		@ address
	repeat
	drop r> 0 ;

: words ( -- : list all the words in the dictionary )
	space
	last address
	begin
		dup
	while
		dup hidden? 0= if dup .id then @ address
	repeat drop cr ;

: numeric? ( char -- n|-1 : convert character in 0-9 a-z range to number )
	>lower
	dup lowercase? if 87 - exit then ( 97 = 'a', +10 as 'a' == 10 )
	dup decimal?   if 48 - exit then ( 48 = '0' )
	drop -1 ;

: digit? >lower numeric? base @ u< ; ( c -- f : is char a digit given base )

do-number: ( n b u -- n b u : convert string )
	begin
		( get next character )
		2dup >r >r drop c@ dup digit? ( n char bool, R: b u )
		if   ( n char )
			swap base @ * swap numeric? + ( accumulate number )
		else ( n char )
			drop
			r> r> ( restore string )
			exit
		then
		r> r> ( restore string )
		1 /string dup 0= ( advance string and test for end )
	until exit

: >number ( n b u -- n b u : convert string )
	base @ >r
	over c@ 0x2D = if 1 /string -1 >r else 0 >r then ( -negative )
	over c@ 0x24 = if 1 /string hex then ( $hex )
	call do-number
	r> if rot negate -rot then
	r> base ! ;

: number? 0 -rot >number nip 0= ; ( b u -- n f : is number? )

: dm+ ( a u -- a )
	over 5 u.r space
	for
		aft count 3 u.r then
	next ;

constant dump-length 16

: dump ( a u -- )
	base @ >r hex dump-length /
	for
		cr dump-length 2dup dm+ -rot
		2 spaces _type
	next drop r> base ! ;

location _test 0

lookfor: ( b u c -- b u : skip until _test succeeds )
	>r 
	begin
		dup 
	while
		over c@ r@ - r@ =bl = _test @execute if rdrop exit then
		1 /string 
	repeat rdrop exit

: skipTest if 0> else 0<> then ; ( n f -- f )
: scanTest skipTest invert ;     ( n f -- f )
: skip ' skipTest _test ! call lookfor ;
: scan ' scanTest _test ! call lookfor ;

( @todo store tmp on the return stack with stack magic )
location tmp 0

: parse 
	>r tib >in @ + #tib @ >in @ - r> 

	( ============================================== )
	( This should be a separate word called "parser",
	with the following stack comment:
		b u c -- b u delta
	But to save on space it has been folder into this
	word )
	tmp ! over >r
	tmp @ skip 2dup
	tmp @ scan swap r> - >r - r> 
	( ============================================== )
	>in +! ; ( c -- b u ; <string> )

: .( 41 parse type ; immediate
: "(" 41 parse 2drop ; immediate
: ) ; immediate
: "\" #tib @ >in ! ; immediate
: word parse here pack$ ; ( c -- a ; <string> )
: token =bl parse word-length min word-buf pack$ ; ( -- a ; <string> )
: char token count drop c@ ; ( -- c; <string> )

( @todo .s and pick should be changed so as not to use the
return stack, which can cause a lot of problems, but instead
use the space pad )
: .s ( -- ) cr sp@ for aft r@ pick . then next [char] * emit ;
: unused 0x4000 here - ;
\ : .free unused u. ;

: preset sp@ ndrop tib #tib cell+ ! ;
: [  0 state ! ; immediate 
: ] -1 state ! ;

: error ( @todo retreat to nearest marker set by ':' or ':noname' )
	[char] ? emit cr ( print error message )
	preset
	[ 
	1 rp! ;     ( reset machine )

doLit: 0x8000 or , exit
: literal ( n -- : write a literal into the dictionary )
	dup 0x8000 and ( n > $7fff ? )
	if
		invert call doLit =invert , ( store inversion of n the invert it )
	else
		call doLit ( turn into literal, write into dictionary )
	then ; immediate

: compile, ( cfa -- : compile a code field address )
	2/ 0x4000 or , ; ( 2/ ? )

: interpret ( ??? a -- ??? : The command/compiler loop )
	find ?dup if
		state @ 
		if
			0> if \ immediate 
				cfa execute 
			else 
				dup inline? 
				if 
					cfa @ , ( @todo copy until exit? )
				else 
					cfa compile, 
				then 
			then
		else
			drop cfa execute
		then
	else \ not a word 
		dup count number? if
			nip
			state @ if literal then
		else
			drop space print error
		then
	then ;

: "immediate" last address 0x2000 toggle ;
: "hide" bl parse find if address 0x4000 toggle else error then ;
: .ok state @ 0= if space OK print space then cr ;
: quit 
	quitLoop:
	preset [ 
	begin 
		query 
		( begin...while...repeat is 'eval' ) 
		begin token dup count nip while interpret repeat drop _prompt @execute
	again 
	branch 0 ;

( @todo save marker for dictionary and previous word pointer, restore this
on any kind of failure that occurs between ":" and ";" )
!csp: sp@ csp ! exit
?csp: sp@ csp @ xor if error then exit
+csp: csp 1+! exit
-csp: csp 1-! exit
: ?compile state @ 0= if error then ; ( fail if not compiling )
location redefined " redefined"
: ?unique dup find if drop redefined print cr else drop then ;
: smudge last address 0x4000 toggle ;
: ":" call !csp here last address , pwd ! smudge =bl word ?unique  count + aligned cp ! ] ; 
: "'" token find if cfa else error then ; immediate
: ";" =exit , [ call ?csp smudge ; immediate
jumpz,: 2/ 0x2000 or , exit
jump,: 2/ ( 0x0000 or ) , exit
: "begin" ?compile here call -csp ; immediate
: "until" ?compile call jumpz, call +csp ; immediate
: "again" ?compile call jump, call +csp ; immediate
: "if" ?compile here 0 call jumpz, call -csp ; immediate
doThen:  here 2/ over @ or swap ! exit
: "then" ?compile call doThen call +csp ; immediate
: "else" ?compile here 0 call jump, swap call doThen ; immediate
: "while" ?compile call "if" ; immediate
: "repeat" ?compile swap call "again" call "then" ; immediate
: recurse ?compile last cfa compile, ; immediate
: tail ?compile last cfa call jump, ; immediate
: create call ":" ' doVar compile, [ ; ( @todo does> )
: >body cell+ ;
: "variable" create 0 , ;
: ":noname" smudge ( <-- bug ) here ] call !csp ;
: "for" ?compile =>r , here call -csp ; immediate
: doNext r> r> ?dup if 1- >r @ >r exit then cell+ >r ; 
: "next" ?compile ' doNext compile, , call +csp ; immediate

\ : [compile] ( -- ; <string> ) "'" compile, ; immediate
\ : compile ( -- ) r> dup @ , cell+ >r ;

( strings: almost work - there is a problem with parsing, the terminating '"'
is not consumed )
: do$ ( -- a ) r> r@ r> count + aligned >r swap >r ;
: $"| ( -- a ) do$ ;
: ."| ( -- ) do$ print ; ( compile-only )
: $,' ( -- ) 34 word count + aligned cp ! ;
: $" ( -- ; <string> ) ' $"| compile, $,' ; immediate
: ." ( -- ; <string> ) ' ."| compile, $,' ; immediate

\ ccitt: ( crc c -- crc : calculate polynomial 0x1021 AKA "x16 + x12 + x5 + 1" )
\ 	over 256/ xor           ( crc x )
\ 	dup  4  rshift xor      ( crc x )
\ 	dup  5  lshift xor      ( crc x )
\ 	dup  12 lshift xor      ( crc x )
\ 	swap 8  lshift xor exit ( crc )
\ 
\ : crc 0xffff >r ( b u -- u : calculate ccitt-ffff CRC )
\ begin dup while 
\ 	r> over c@ call ccitt >r
\ 	1 /string
\ repeat 2drop r> ;
 
( ======================== Word Set ========================= )

( ======================== Miscellaneous ==================== )


( Initial value of VGA

  BIT     MEANING
  7   -  Display Next Screen
  6   -  Enable VGA
  5   -  Cursor enable
  4   -  Cursor blinks
  3   -  Cursor mode
  2   -  Blue
  1   -  Green
  0   -  Red )
constant vgaInit       0x7A \ 0x007A

constant vgaX          80
constant vgaY          40
constant vgaTextSize   3200

variable cursor 0  ( index into VGA text memory )

\ variable vga.on        1
\ variable vga.cursor.en 0
\ variable vga.color     1
\ variable vga.screen    0

\ : vga.ctrl! ( -- )
\ 	0
\ 	vga.screen    @ if 0x80 or then
\ 	vga.on        @ if 0x40 or then
\ 	vga.screen    @ if 0x20 or then
\ 	vga.cursor.en @ if 0x10 or then
\ 	vga.color     @ 0x7 and or
\ 	0x4004 ! ;

: vga! ( vga.screen @ if 4096 + then ) 0xC000 or ! ; ( n a -- : write to VGA memory and adjust cursor position )

( This should also emit the ANSI Terminal codes for paging as well,
perhaps it could be a vectored word )
: page ( -- : clear VGA screen )
	0 cursor !
	vgaTextSize
	begin
		dup
	while
		dup =bl swap vga! 1-
	repeat drop ;	

: seed    oLfsr ! ; ( u -- : seed PRNG, requires non-zero value )
: random  iLfsr @ ; ( -- u : get a pseudo random number @todo replace with XORShift )

: at-xy 256* or oVgaCursor ! ; ( x y -- : set terminal cursor to x-y position )

( @todo Optimize and extend handle tabs, delete, and
even perhaps ANSI Terminal codes. This word takes up far too much
stack space and uses arithmetic too freely

Dividing by vgaX [80] can be done with:
	0xCCCD um* swap drop 6 rshift
vgaY [40] with:
	0xCCCD um* swap drop 5 rshift

This is clearly magic. )
: terminal ( n a -- a : act like a terminal )
	swap
	dup =lf = if drop vgaX / 1+ dup 0 swap at-xy vgaX * exit then
	dup =cr = if drop exit then
	dup =bs = if drop 1- exit then
	swap vgaTextSize mod tuck dup 1+ vgaX /mod at-xy vga! 1+ ;

: segments! o8SegLED ! ; ( n -- : display a number on the LED 7/8 segment display )
: led!      oLeds ! ; ( n -- : write to LED lights )
: switches  iSwitches  @ ;
: timer!    oTimerCtrl ! ;
: timer@    iTimerDin  @ ;

: ps2? ( -- c -1 | 0 : like "rx?" but for the PS/2 keyboard )
	iPs2 @ dup 0xff and swap 0x0100 and if -1 else drop 0 then ;

\ variable wrote-count     0
\ variable read-count      0

: input ( -- c -1 | 0 : look for input from UART or PS/2 keyboard )
	rx? if -1 else ps2? then ( if read-count 1+! -1 else 0 then ) ;

: output ( c -- : write to UART and VGA display )
	dup tx!
	( drop exit \ @note The rest of this word is responsible for the large return stack usage )
	cursor @ terminal
	dup 1+ cursor @ u< if drop page else cursor ! then
	( wrote-count 1+! ) ;


( Initial value of timer
  BIT     MEANING
  15   -  Enable Timer
  14   -  Reset Timer Value immediately
  13   -  Interrupt Enable
  12-0 -  Value to compare against )

( \ Testing for the interrupt mechanism 
irq:
	.break
	0 seti
	switches led!
	1 seti
	exit
 .set 12 irq
: irqTest
	0xffff oIrcMask !
	0xffff oTimerCtrl !
	1 seti ; )

( ======================== Miscellaneous ==================== )

( ======================== ANSI SYSTEM   ==================== )

( Terminal colorization module, via ANSI Escape Codes

see: https://en.wikipedia.org/wiki/ANSI_escape_code
These codes will provide a relatively portable means of
manipulating a terminal )
\ 
\ constant escape 27
\ variable colorize 0 ( set to 'drop' to disable, 'tx!' turn on )
\ .set colorize tx!
\ 
\ CSI: escape emit [char] [ emit exit
\ 10u.: base @ >r decimal <# #s #> type r> base ! exit ( n -- : print a number in decimal )
\ 
\ ( Colors can be used for the lower VGA control bits, as well as for ANSI
\ Terminal escape sequences )
\ : black    0 ;
\ : red      1 ;
\ : green    2 ;
\ : yellow   3 ;
\ : blue     4 ;
\ : magenta  5 ;
\ : cyan     6 ;
\ : white    7 ;
\ : dark     0 ;
\ : bright   1 ;
\ : ansi.foreground ;
\ : ansi.background 10 + ;
\ 
\ ( usage:
\ 
\ 	dark red foreground color
\ 	bright blue background color 
\ )
\ : ansi.color ( brightness color-code -- : set the color on an ANSI terminal )
\ 	colorize @ 0= if 2drop exit then
\ 	_emit colorize switch
\ 	30 + 
\ 	call CSI call 10u. 
\ 	if  
\ 		59 emit  49 emit ( ." ;1" ) 
\ 	then 
\ 	[char] m emit
\ 	_emit colorize switch ;
\ 
\ : ansi.at-xy call CSI call 10u. 59 emit call 10u. [char] H emit ; ( x y -- : set ANSI terminal cursor position to x y )
\ : ansi.page call CSI 2 call 10u. [char] J emit 1 1 ansi.at-xy ; ( -- : clear ANSI terminal screen and move cursor to beginning )
\ 
\ location HIDE_CURSOR "?25l" : ansi.hide-cursor call CSI HIDE_CURSOR print ; ( -- : hide cursor )
\ location SHOW_CURSOR "?25h" : ansi.show-cursor call CSI SHOW_CURSOR print ; ( -- : show the cursor )
\ : ansi.save-cursor call CSI [char] s emit ; ( -- : save cursor position )
\ : ansi.restore-cursor call CSI [char] u emit ; ( -- : restore saved cursor position )
\ : ansi.reset-color colorize @ 0= if exit then call CSI 0 call 10u. [char] m emit ; ( -- : reset terminal color to its default value)
\ 
( ======================== ANSI SYSTEM   ==================== )

( ======================== Starting Code ==================== )

( @bug The ".set" directive is a bit of a hack at the moment, it divides the
address by two for function address but not for labels, this works in the
current setup, but is not ideal )
start:
.set entry start

	0 seti
	0 oIrcMask           !
	vgaInit   oVgaCtrl   ! \ Turn on VGA monitor
	cpu-id    segments!    \ Display CPU ID on 7-Segment Displays
	page                   \ Clear display
	1 seed                 \ Set up PRNG seed

	here . 0x2000 here - u. ( unused u. ) .ok 

	_boot @execute  ( _boot contains zero by default, does nothing )
	branch quitLoop ( jump to main interpreter loop if _boot returned )

( ======================== User Code ======================== )

.set pwd $pwd
.set cp  $pc

.set _key?    input      ( execution vector of ?key,   default to input. )
.set _emit    output     ( execution vector of emit,   default to output )
.set _expect  accept     ( execution vector of expect, default to 'accept'. )
.set _tap     ktap       ( execution vector of tap,    default the ktap. )
.set _echo    output     ( execution vector of echo,   default to output. )
.set _prompt  .ok        ( execution vector of prompt, default to '.ok'. )
.set _boot    0          ( @execute does nothing if zero )
