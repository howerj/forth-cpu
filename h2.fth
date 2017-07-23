( This program is written in a pseudo Forth like language,
it is not Forth and does not behave like it, it just looks
like it. This should be thought of as assembly code and
not Forth.

A lot of the code has been taken verbatim from "The Zen
of eForth by C. H. Ting".

For a grammar of the language look into the file "h2.c".

Execution begins at a label called "start".

TODO:
* Hex number printer
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

Forth To Do:
* Strings, Throw/Catch, abort, vocabularies, see, create/does, constant,
variable, make/doer, ...
* Word for printing out the state of the interpreter would help, if a
word specifically for printing out variables is produced this would help
* "-1" is a commonly used constant, however it takes up two instructions,
it is worth turning this into a word which pushes -1 onto the stack, however
the maximum number of items on the stack is already too high )

( ======================== System Constants ================= )

.mode 1 ( Turn word header compilation and optimization off)

.allocate 16
( Add the assembler words to the dictionary: certain built in words
will need an in-line bit, as well as an immediate bit )
.built-in

constant =exit         0x601c ( op code for exit )
constant =invert       0x6600 ( op code for invert )
constant =bl           32     ( blank, or space )
constant =cr           13     ( carriage return )
constant =lf           10     ( line feed )
constant =bs           8      ( back space )

( Outputs: 0x6000 - 0x7FFF )
constant oUart         0x6000
constant oLeds         0x6001
constant oTimerCtrl    0x6002
constant oVgaCursor    0x6003
constant oVgaCtrl      0x6004
constant o8SegLED      0x6005
constant oIrcMask      0x6006
constant oLfsr         0x6007

( Inputs: 0x6000 - 0x7FFF )
constant iUart         0x6000
constant iSwitches     0x6001
constant iTimerCtrl    0x6002
constant iTimerDin     0x6003
constant iVgaTxtDout   0x6004
constant iPs2          0x6005
constant iLfsr         0x6006

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

( These variables are for vectored word execution, there are
set at the end of the file )
variable _?key    0
variable _emit    0
variable _expect  0
variable _tap     0
variable _echo    0
variable _prompt  0
variable _eval    0
variable _number? 0
variable OK      "ok"
( ======================== System Variables ================= )

( ======================== Forth Kernel ===================== )

: ! store drop ;
: 256* 8 lshift ;
: 256/ 8 rshift ;
: 1+ 1 + ;
: negate invert 1+ ;
: - negate + ;
\ : 2+ 2 + ;
\ : 2- 2 - ;
: 2/ 1 rshift ;
\ : 2* 1 lshift ;
: cell- 2 - ;
: cell+ 2 + ;
: cells 1 lshift ;
: ?dup dup if dup then ;
\ : do-next r> r> ?dup if 1- >r @ >r exit then cell+ >r ;
: >= < invert ;
: >  swap < ;
: u> swap u< ;
\ : u>= u< invert ;
: <> = invert ;
: 0<> 0= invert ;
: 0>  0 > ;
: 0<  0 < ;
: 2dup over over ;
: 2drop drop drop ;
: tuck swap over ;
: +! tuck @ + swap ! ;
: 1+! 1 swap +! ;
: execute >r ;
: c@ dup ( -2 and ) @ swap 1 and if 256/ else 0xff and then ;
: c!
	swap 0xff and dup 256* or swap
	tuck dup ( - 2 and ) @ swap 1 and 0 = 0xff xor
	>r over xor r> and xor swap ( - 2 and ) ! ;

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

: tx! ( c -- : write a character to UART )
	begin iUart @ 0x0800 and until ( Wait until TX FIFO is not full )
	0x2000 or oUart ! ;            ( Write character out )

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

: 0<= 0> 0= ;
: 0>= 0< 0= ;
: 2! ( d a -- ) tuck ! cell+ ! ;
: 2@ ( a -- d ) dup cell+ @ swap @ ;
: here cp @ ;
: pad here 80 + ;
: @execute @ ?dup if execute then ;
: 3drop 2drop drop ;
: bl =bl ;
: within over - >r - r> u< ; ( u lo hi -- t )
: not -1 xor ;
: dnegate not >r not 1 um+ r> + ; ( d -- d )
: d= ( d d -- f )
	>r swap r> = >r = r> and ;
\ : d<> d= 0= ; ( d d -- f )
: abs dup 0< if negate then ;
: count ( cs -- b u )
	dup 1+ swap c@ ;
	\ dup c@ swap 1+ swap ;
: rot >r swap r> swap ;
: -rot swap >r swap r> ;
: min  2dup < if drop else nip then ;
: max  2dup > if drop else nip then ;
: >char ( c -- c : convert character to '_' if not ASCII or is control char )
  0x7f and dup 127 =bl within if drop [char] _ then ;

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

: m* ( n n -- d )
	2dup xor 0< >r abs swap abs um*  r> if dnegate then ;

: */mod ( n n n -- r q ) >r m* r> m/mod ;
: */ ( n n n -- q ) */mod swap drop ;

: aligned ( b -- a )
	dup 0 2 um/mod drop dup
	if 2 swap - then + ;

: at-xy ( x y -- : set terminal cursor to x-y position )
	256* or oVgaCursor ! ;

: /string ( b u1 u2 -- b u : advance a string u2 characters )
	over min rot over + -rot - ;

: last pwd @ ;

: emit ( char -- : write out a char )
	_emit @execute ;

: toggle ( a u -- : xor value at addr with u )
	over @ xor swap ! ;

: cr =cr emit =lf emit ;
: space =bl emit ;

: type ( b u -- : print a string )
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

: spaces ( +n -- ) =bl nchars ;

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

: cmove ( b b u -- )
	for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ;

: fill ( b u c -- )
	swap for swap aft 2dup c! 1+ then next 2drop ;

: -trailing ( b u -- b u )
	for aft =bl over r@ + c@ <
		if r> 1+ exit then then
	next 0 ;

: pack$ ( b u a -- a ) \ null fill
	aligned  dup >r over
	dup 0 2 um/mod drop
	- over +  0 swap !  2dup c!  1 + swap cmove  r> ;

: tib ( -- a : terminal input buffer )
	#tib cell+ @ ;

: echo _echo @execute ;

( @todo ^h and ktap should optionally emit a delete key, and the terminal word
should handle it as well )

: ^h ( bot eot cur c -- bot eot cur )
	>r over r@ < dup
	if
		=bs dup echo =bl echo echo
	then r> + ;

: tap ( bot eot cur c -- bot eot cur )
	dup echo over c! 1 + ;

: ktap ( bot eot cur c -- bot eot cur )
	dup =cr xor
	if =bs xor if =bl tap else ^h then exit
	then drop swap drop dup ;

: key? _?key @execute ;
: key begin key? until ;

: accept ( b u -- b u )
	over + over
	begin
		2dup xor
	while
		key  dup =bl -  95 u<
		if tap else _tap @execute then
	repeat drop over - ;

: expect ( b u -- ) _expect @execute span ! drop ;

: query ( -- )
  tib 80 _expect @execute #tib !  drop 0 >in ! ;

: allot cp +! ;

: , here dup cell+ cp ! ! ;

: =string ( a1 u2 a1 u2 -- f : string equality )
	>r swap r> ( a1 a2 u1 u2 )
	over <> if 3drop  0 exit then
	dup  0= if 3drop -1 exit then
	1-
	for ( a1 a2 )
		2dup c@ swap c@ <> if 2drop rdrop 0 exit then
		1+ swap 1+
	next 2drop -1 ;

: address ( a -- a : mask off any bits not used for the address )
	0x1FFF and ;

: nfa ( pwd -- nfa : move to name field address)
	address cell+ ;

: cfa ( pwd -- cfa : move to code field address )
	nfa dup
	count nip + cell+ ;

: 2. swap . . ;

\ : 2rdrop r> rdrop rdrop >r ;

: logical ( n -- f )
	0= 0= ;

: immediate? ( pwd -- f : is immediate? )
	0x2000 and logical ;

: hidden? ( pwd -- f : is hidden? )
	0x4000 and logical ;

: inline? ( pwd -- f : is inline? )
	0x8000 and logical ;

: finder ( a u -- pwd -1 | 0 : find a word in the dictionary )
	>r >r
	last address
	begin
		dup
	while
		dup nfa count r> r> 2dup >r >r =string
		if rdrop rdrop -1 exit then
		@ address
	repeat
	rdrop rdrop drop 0 exit ;

: find
	finder
	dup if
		over @ hidden? if 2drop 0 then
	then ;

: .id ( pwd -- : print out a word )
	nfa count type space ;

: words ( -- : list all the words in the dictionary )
	space
	last address
	begin
		dup
	while
		dup @ hidden? 0= if dup .id then @ address
	repeat drop cr ;

\ : .base ( -- ) base @ decimal dup . base  ! ;

: nuf? ( -- f ) key? dup if 2drop key =cr = then ;

\ : ?exit if rdrop then ;

: decimal? ( c -- f : is character a decimal number? )
	48 58 within ; ( '0' = 48, 58 = ':', or 9+1 )

: lowercase? ( c -- f : is character lower case? )
	[char] a [char] { within ; ( 'z' + 1 = '{' )

: uppercase? ( C -- f : is character upper case? )
	[char] A [char] [ within ; ( 'Z' + 1 = '[' )

: >upper ( c -- C : convert char to uppercase iff lower case )
	dup lowercase? if =bl xor then ;

: >lower ( C -- c : convert char to lowercase iff upper case )
	dup uppercase? if =bl xor then ;

: numeric? ( char -- n|-1 : convert character in 0-9 a-z range to number )
	dup lowercase? if 97 - 10 + exit then ( 97 = 'a' )
	dup decimal?   if 48 -      exit then ( 48 = '0' )
	drop -1 ;

: digit? ( char -- f : is a character a number in the current base )
	>lower numeric? base @ u< ;

: do-number ( n b u -- n b u : convert string )
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
	until ; hidden

: >number ( n b u -- n b u : convert string )
	base @ >r
	over c@ 0x2D = if 1 /string -1 >r else 0 >r then ( -negative )
	over c@ 0x24 = if 1 /string hex then ( $hex )
	do-number
	r> if rot negate -rot then
	r> base ! ;

: number? ( b u -- n f )
	0 -rot
	>number nip 0= ;

( @todo replace with XORShift )
: seed ( u -- : seed PRNG, requires non-zero value )
	oLfsr ! ;

: random ( -- u : get a pseudo random number )
	iLfsr @ ;

: pick
	?dup if swap >r 1- pick r> swap exit then dup ;

: _type ( b u -- )
	for
		aft count >char emit then
	next drop ;

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

: lookfor ( b u c -- b u : skip until _test succeeds )
	>r dup 0= if rdrop exit then
	begin
		over c@ r@ - r@ =bl = _test @execute if rdrop exit then
		1 /string dup 0=
	until rdrop ; hidden

skipTest: if 0> else 0<> then exit
scanTest: if 0<= else 0= then exit
: skip ' skipTest _test ! lookfor ;
: scan ' scanTest _test ! lookfor ;

( @todo store tmp on the return stack with stack magic )
variable tmp 0

: parser ( b u c -- b u delta : )
	tmp ! over >r
	tmp @ skip 2dup
	tmp @ scan swap r> - >r - r> ;

: parse ( c -- b u ; <string> )
	>r tib >in @ + #tib @ >in @ - r> parser >in +! ;

: .( 41 parse type ; immediate
: "(" 41 parse 2drop ; immediate
: "\" #tib @ >in ! ; immediate

: word parse here pack$ ;
: char =bl parse drop c@ ;

: .s ( -- ) cr sp@ for aft r@ pick . then next ( ."  <sp" ) ;
: free 0x2000 here - ;
: .free free u. ;

\ : mswap ( a a -- : swap two memory locations )
\	2dup >r @ swap ! r> @ swap ! ;

( @todo Better factor $interpret and $compile, as well as making
them vectored words )

: $interpret ( a u -- )
	2dup find
	if
		nip nip cfa 2/ execute
	else
		number? 0= if drop ( else abort ) then
	then ;

: literal ( n -- )
	dup 0x8000 and
	if
		invert 0x8000 or , =invert ,
	else
		0x8000 or ,
	then ; immediate

: $compile ( a u -- )
	2dup find
	if
		nip nip dup @ immediate? if
			cfa 2/ execute
		else
			dup @ inline? if ( could improve this by copy all data until exit is found )
				cfa @ ,
			else
				cfa 2/ 0x4000 or , ( turn into a call )
			then
		then
	else
		number? if literal ( else abort ) then
	then ;

: "immediate" last address 0x2000 toggle ;
\ : "hide"      last address 0x4000 toggle ; ( make a parsing version )

: [ ' $interpret _eval ! ; immediate
: ] ' $compile   _eval ! ;

: token =bl parse ;

: .ok ' $interpret _eval @ = if space OK count type space then cr ;

: eval
	begin token dup while _eval @execute repeat 2drop _prompt @execute ;

: quit begin query eval again ;

: ":" here last address , pwd ! =bl word count + aligned cp ! ] ;
: "'" =bl parse find if cfa else ( -1 throw ) 0 then ; immediate
: ";" =exit , [ ; immediate

: "?branch" 2/ 0x2000 or , ;
: "branch" 2/ ( 0x0000 or ) , ;
: "begin" here ; immediate
: "until" call "?branch" ; immediate
: "again" call "branch" ; immediate
: "if" here 0 call "?branch" ; immediate
: "then" here 2/ over @ or swap ! ; immediate
: "while" call "if" ; immediate
: "repeat" swap call "again" call "then" ; immediate
: recurse last cfa 2/ 0x4000 or , ; immediate
: tail last cfa call "branch" ; immediate

( create should use doVar, but that does not seem to work )
\ : doVariable r> 1 lshift ;
\ : create call ":" ' doVariable , [ ;
: create call ":"  here 2 cells + literal =exit , [ ;
: "variable" create 0 , ;

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
constant timerInit     0x9FFF

variable cursor 0  ( index into VGA text memory )

: vga! ( n a -- : write to VGA memory and adjust cursor position )
	 0xE000 or ! ;

( This should also emit the ANSI Terminal codes for paging as well,
perhaps it could be a vectored word )
: page ( -- : clear VGA screen )
	0 cursor !
	0x1FFF for =bl r@ vga! next ;

\ @todo Optimize and extend handle tabs, back spaces, delete, and
\ even perhaps ANSI Terminal codes
: terminal ( n a -- a : act like a terminal )
	swap
	dup =lf = if drop vgaX / 1+ dup 0 swap at-xy vgaX * exit then
	dup =cr = if drop exit then
	\ dup =bs = if 1- exit then
	swap vgaTextSize mod tuck dup 1+ vgaX /mod at-xy vga! 1+ ;

: segment! o8SegLED ! ; ( n -- : display a number on the LED 7/8 segment display )
: led!     oLeds ! ; ( n -- : write to LED lights )
: switches iSwitches  @ ;
: timer!   oTimerCtrl ! ;
: timer@   iTimerDin  @ ;

: ?ps2 ( -- c -1 | 0 : like ?rx but for the PS/2 keyboard )
	iPs2 @ dup 0xff and swap 0x0100 and if -1 else drop 0 then ;

variable wrote-count     0
variable read-count      0

: input ( -- c -1 | 0 : )
	?rx if -1 else ?ps2 then if read-count 1+! -1 else 0 then ;

: output ( c -- )
	dup tx!
	( drop exit \ @note The rest of this word is responsible for the large return stack usage )
	cursor @ terminal
	dup cursor @ u< if drop page else cursor ! then
	wrote-count 1+! ;

( This routine must be written in assembly; begin...while...repeat compiles to
simple branches

@bug This doesn't work - yet! )
\ : rp!
\	begin dup rp@ = 0= while rdrop .break repeat drop .break ;


( ======================== Miscellaneous ==================== )

( ======================== Starting Code ==================== )

: init
	vgaInit   oVgaCtrl   ! \ Turn on VGA monitor
	timerInit timer!       \ Enable timer
	cpu-id segment!        \ Display CPU ID on 7-Segment Displays
	page
	1 seed ;               \ Set up PRNG seed
	\ 0x00FF oIrcMask !
	\ 1   seti ;

( The start up code begins here, on initialization the assembler
jumps to a special symbol "start".

@todo This special case symbol should be removed by adding
adequate assembler directives )

start:
	init

	here . .free .ok

	\ basic command loop
	quit
	branch start

( ======================== User Code ======================== )

.set pwd $pwd
.set cp  $pc

.set _?key    input      ( execution vector of ?key,   default to ?rx. )
.set _emit    output     ( execution vector of emit,   default to tx! )
.set _expect  accept     ( execution vector of expect, default to 'accept'. )
.set _tap     ktap       ( execution vector of tap,    default the ktap. )
.set _echo    output     ( execution vector of echo,   default to tx!. )
.set _prompt  .ok        ( execution vector of prompt, default to '.ok'. )
.set _eval    $interpret ( execution vector of eval,   default to $interpret )
.set _number? number?    ( execution vector of number? default to number? )


