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
isrRxFifoNotEmpty: .allocate cell
isrRxFifoFull:     .allocate cell
isrTxFifoNotEmpty: .allocate cell
isrTxFifoFull:     .allocate cell
isrKbdNew:         .allocate cell
isrTimer:          .allocate cell
isrBrnLeft:        .allocate cell

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

( Outputs: 0x6000 - 0x7FFF )
constant oUart         0x4000 ( UART TX/RX Control register )
constant oLeds         0x4001 ( LEDs )
constant oTimerCtrl    0x4002 ( Timer Control )
constant oVgaCursor    0x4003 ( VGA X/Y Cursor position )
constant oVgaCtrl      0x4004 ( VGA Control )
constant o8SegLED      0x4005 ( 4x7 Segment display )
constant oIrcMask      0x4006 ( Interrupt Mask )
constant oLfsr         0x4007

( Inputs: 0x6000 - 0x7FFF )
constant iUart         0x4000
constant iSwitches     0x4001
constant iTimerCtrl    0x4002
constant iTimerDin     0x4003
constant iVgaTxtDout   0x4004
constant iPs2          0x4005
constant iLfsr         0x4006

( ======================== System Constants ================= )

( ======================== System Variables ================= )

( These variables are for vectored word execution, there are
set at the end of the file )
variable _?key    0
variable _emit    0
variable _expect  0
variable _tap     0
variable _echo    0
variable _prompt  0
variable _boot    0
variable OK      "ok"
variable csp      0  ( current data stack pointer - for error checking )
variable >in      0  ( Hold character pointer when parsing input )
variable state    0  ( compiler state variable )
variable pwd      0  ( Present Word Variable: Set at end of file )
variable cp       0  ( Dictionary Pointer: Set at end of file )
variable hld      0  ( Pointer into hold area for numeric output )
variable base     10 ( Current output radix )
variable span     0  ( Hold character count received by expect   )
variable #tib     0  ( Current count of terminal input buffer    )
variable tib-buf  0  ( ... and address )
.set tib-buf $pc
.allocate tib-length
.allocate cell

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
: min 2dup < if drop else nip then ;
: max 2dup > if drop else nip then ;
: >char 0x7f and dup 127 =bl within if drop [char] _ then ; ( c -- c )
: tib #tib cell+ @ ;        ( -- a )
: echo _echo @execute ;     ( c -- )
: key? _?key @execute ;     ( -- c -1 | 0 )
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

( @todo simplify this code )
: =string ( a1 u2 a1 u2 -- f : string equality )
	>r swap r> ( a1 a2 u1 u2 )
	over xor if 3drop  0 exit then
	dup  0=  if 3drop -1 exit then
	1-
	for ( a1 a2 )
		2dup c@ swap c@ xor if 2drop rdrop 0 exit then
		1+ swap 1+
	next 2drop -1 ;

: address 0x1fff and ; ( a -- a : mask off address bits )
: nfa address cell+ ; ( pwd -- nfa : move to name field address)
: cfa nfa dup count nip + cell + ; ( pwd -- cfa : move to code field address )
: .id nfa print space ; ( pwd -- : print out a word )

logical: 0= 0= exit ( n -- f )
: immediate? @ 0x2000 and call logical ; ( pwd -- f : is immediate? )
: hidden?    @ 0x4000 and call logical ; ( pwd -- f : is hidden? )
: inline?    @ 0x8000 and call logical ; ( pwd -- f : is inline? )

( Find should have the following stack comment: 

	[ c-addr -- c-addr 0 | xt 1 | xt -1 ]

A counted string is passed in, and 0 and the original string
is returned if the string has not been found, the execution
and either 1 [word is immediate] or -1 [word is compiling] is
returned.

http://lars.nocrew.org/forth2012/core/FIND.html )

: find ( a u -- pwd 1 | pwd -1 | 0 : find a word in the dictionary )
	>r >r
	last address
	begin
		dup
	while
		dup nfa count r> r> 2dup >r >r =string
		if ( found! )
			rdrop rdrop    ( get rid of string )
			dup hidden? 
			( "0=" is "drop 0" in this case as argument is alway non zero ) 
			if 0= else dup immediate? if 1 else -1 then then 
			exit
		then
		@ address
	repeat
	rdrop rdrop drop 0 exit ;


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

variable _test 0

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
variable tmp 0

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
: "\" #tib @ >in ! ; immediate
: word parse here pack$ ; ( c -- a ; <string> )
: token =bl parse ; ( -- a u; <string> )
: char token drop c@ ; ( -- c; <string> )

( @todo .s and pick should be changed so as not to use the
return stack, which can cause a lot of problems, but instead
use the space pad )
: .s ( -- ) cr sp@ for aft r@ pick . then next [char] * emit ;
: unused 0x2000 here - ;
\ : .free unused u. ;

: preset sp@ ndrop tib #tib cell+ ! ;
: [  0 state ! ; immediate 
: ] -1 state ! ;

: error ( @todo retreat to nearest marker set by ':' or ':noname' )
	[char] ? emit cr ( print error message )
	preset
	[ ;
	\ branch 0 ;     ( reset machine )

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

( @todo This should be changes so it accept a counted string )
: interpret ( ??? b u -- ??? : The command/compiler loop )
	2dup find ?dup if
		>r >r 2drop r> r>
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
		2dup number? if
			nip nip
			state @ if literal then
		else
			drop space type error
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
		begin token dup while interpret repeat 2drop _prompt @execute
	again 
	branch 0 ;

!csp: sp@ csp ! exit
?csp: sp@ csp @ xor if error then exit
+csp: csp 1+! exit
-csp: csp 1-! exit
\ : compile-only state @ 0= if error then ;
\ variable redefined "redefined"
\ : ?unique dup count find if drop redefined print then ;
: ":" call !csp here last address , pwd ! =bl word count + aligned cp ! ] ; 
: "'" token find if cfa else error then ; immediate
: ";" =exit , [ call ?csp ; immediate
jumpz,: 2/ 0x2000 or , exit
jump,: 2/ ( 0x0000 or ) , exit
: "begin" here call -csp ; immediate
: "until" call jumpz, call +csp ; immediate
: "again" call jump, call +csp ; immediate
: "if" here 0 call jumpz, call -csp ; immediate
: "then" here 2/ over @ or swap ! call +csp ; immediate
: "while" call "if" ; immediate
: "repeat" swap call "again" call "then" ; immediate
: recurse last cfa compile, ; immediate
: tail last cfa call jump, ; immediate
: create call ":" ' doVar compile, [ ; ( @todo does> )
: >body cell+ ;
: "variable" create 0 , ;
: ":noname" here ] call !csp ;
: "for" =>r , here call -csp ; immediate
: doNext r> r> ?dup if 1- >r @ >r exit then cell+ >r ; 
: "next" ' doNext compile, , call +csp ; immediate

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


( ======================== Word Set ========================= )

( ======================== Miscellaneous ==================== )

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

variable cursor 0  ( index into VGA text memory )

: vga! 0xC000 or ! ; ( n a -- : write to VGA memory and adjust cursor position )

( This should also emit the ANSI Terminal codes for paging as well,
perhaps it could be a vectored word )
: page ( -- : clear VGA screen )
	0 cursor !
	0x1fff
	begin
		dup
	while
		dup =bl swap vga! 1+
	repeat drop ;	
	\ 0x1FFF for =bl r@ vga! next ;

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

	here . unused u. .ok

	_boot @execute  ( _boot contains zero by default, does nothing )
	branch quitLoop ( jump to main interpreter loop if _boot returned )

( ======================== User Code ======================== )

.set pwd $pwd
.set cp  $pc

.set _?key    input      ( execution vector of ?key,   default to input. )
.set _emit    output     ( execution vector of emit,   default to output )
.set _expect  accept     ( execution vector of expect, default to 'accept'. )
.set _tap     ktap       ( execution vector of tap,    default the ktap. )
.set _echo    output     ( execution vector of echo,   default to output. )
.set _prompt  .ok        ( execution vector of prompt, default to '.ok'. )
.set _boot    0          ( @execute does nothing if zero )
