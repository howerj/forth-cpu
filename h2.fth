( This program is written in a pseudo Forth like language, it is not
Forth and does not behave like it, it just looks like it. This should
be thought of as assembly code and not Forth.

A lot of the code has been taken verbatim from "The Zen of eForth by
C. H. Ting".

For a grammar of the language look into the file "h2.c".

Execution begins at a label called "start".

TODO:
* Bootloader: A super simple one, preferably on that takes up less than 100
words. For that matter a very small and minimal Forth could be create, aiming
to take up less than 1/2k of space.
* Turn this into a literate file
* The maximum value for the return stack depth is 21, out of a possible 32 this
is quite high, this is not during general running though, but is still higher
than desired. This is mostly caused by the "terminal" word.
* Make a special version of exit, which does exactly the same as exit but with
a different op-code value. This special version could be compiled in by ';', so
the decompiler knows when a word ends.
* Turn magic number into constants
* Fix interrupt code
* An automatic test suite for this program would help in development a lot,
it would have to interact with the debugger, a Perl script that deals
with would be the optimal solution.
* There is a bit of confusion over what words accept as execution tokens,
some take pointers to the CFA of a word, other the PWD field
* An optimized, smaller version, of this interpreter should be created.
* Load initial VGA screen from NVRAM?

* For tiny math routines, look at:
http://files.righto.com/calculator/sinclair_scientific_simulator.html
https://en.wikipedia.org/wiki/Sinclair_Scientific

Forth To Do:
* Throw/Catch, abort, vocabularies, see, does>, make/doer, ...
* Fix PARSE
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
entry:             .allocate cell ( Entry point - not an interrupt )
isrRxFifoNotEmpty: .allocate cell ( UART RX FIFO not empty )
isrRxFifoFull:     .allocate cell ( UART RX FIFO full )
isrTxFifoNotEmpty: .allocate cell ( UART TX FIFO not empty )
isrTxFifoFull:     .allocate cell ( UART TX FIFO full )
isrKbdNew:         .allocate cell ( New PS/2 Keyboard character )
isrTimer:          .allocate cell ( Timer interrupt )
isrBrnLeft:        .allocate cell ( Left button pressed )

.mode 1   ( Turn word header compilation and optimization off )
.built-in ( Add the built in words to the dictionary )

constant =exit         $601c ( op code for exit )
constant =invert       $6600 ( op code for invert )
constant =>r           $6147 ( op code for >r )
constant =bl           32     ( blank, or space )
constant =cr           13     ( carriage return )
constant =lf           10     ( line feed )
constant =bs           8      ( back space )

constant c/l           64     ( characters per line in a block )

constant tib-length    80     ( size of terminal input buffer )
constant pad-length    80     ( pad area begins HERE + pad-length )
constant word-length   31     ( maximum length of a word )

( Outputs: $6000 - $7FFF )
constant oUart         $4000 ( UART TX/RX Control register )
constant oLeds         $4001 ( LEDs )
constant oTimerCtrl    $4002 ( Timer Control )
constant oVgaCursor    $4003 ( VGA X/Y Cursor position )
constant oVgaCtrl      $4004 ( VGA Control )
constant o8SegLED      $4005 ( 4x7 Segment display )
constant oIrcMask      $4006 ( Interrupt Mask )
constant oLfsr         $4007 ( Seed value of LFSR )
constant oMemControl   $4008 ( Memory control and high address bits )
constant oMemAddrLow   $4009 ( Lower memory address bits )
constant oMemDout      $400A ( Memory output for writes )

( Inputs: $6000 - $7FFF )
constant iUart         $4000 ( Matching registers for iUart )
constant iSwitches     $4001 ( Switch control [on/off] )
constant iTimerCtrl    $4002 ( Timer control, not really needed )
constant iTimerDin     $4003 ( Current timer value )
constant iVgaTxtDout   $4004 ( VGA text output, currently broken )
constant iPs2          $4005 ( PS/2 keyboard input )
constant iLfsr         $4006 ( Input from Linear Feedback Shift Register )
constant iMemDin       $4007 ( Memory input for reads )

( ======================== System Constants ================= )

( ======================== System Variables ================= )

( Execution vectors for changing the behaviour of the program,
they are set at the end of this file )
variable _key?     0  ( -- c -1 | 0 : new character available? )
variable _emit     0  ( c -- : emit character )
variable _expect   0  ( "accept" vector )
variable _tap      0  ( "tap" vector, for terminal handling )
variable _echo     0  ( c -- : emit character )
variable _prompt   0  ( -- : display prompt )
variable _boot     0  ( -- : execute program at startup )
variable _bload    0  ( a u k -- f : load block )
variable _bsave    0  ( a u k -- f : save block )
variable _binvalid 0  ( k -- k : throws error if k invalid )

location OK     "ok"  ( used by "prompt" )
location pwd       0  ( Present Word Variable: Set at end of file )
location cp        0  ( Dictionary Pointer: Set at end of file )
location csp       0  ( current data stack pointer - for error checking )
location _id       0  ( used for source id )
variable >in       0  ( Hold character pointer when parsing input )
variable state     0  ( compiler state variable )
variable hld       0  ( Pointer into hold area for numeric output )
variable base      10 ( Current output radix )
variable span      0  ( Hold character count received by expect   )
variable #tib      0  ( Current count of terminal input buffer    )
location tib-buf   0  ( ... and address )
.set tib-buf $pc      ( set tib-buf to current dictionary location )
.allocate tib-length  ( allocate enough for the terminal input buffer )
.allocate cell        ( plus one extra cell for safety )
location word-buf  0  ( transient word buffer starts here )
.allocate 34          ( allocate room for it )

constant block-invalid   -1 ( block invalid number )
constant block-size    1024 ( size of a block )
variable blk             -1 ( current blk loaded )
location block-dirty      0 ( -1 if loaded block buffer is modified )
location block-buffer     0 ( block buffer starts here )
.allocate block-size

( ======================== System Variables ================= )

( ======================== Forth Kernel ===================== )

: ! store drop ;           ( n a -- )
: 256* 8 lshift ;          ( u -- u )
: 256/ 8 rshift ;          ( u -- u )
: 1+ 1 + ;                 ( n -- n )
: negate invert 1 + ;      ( n -- n )
: - invert 1 + + ;         ( n n -- n )
: 2+ 2 + ;                 ( n -- n )
: 2- 2 - ;                 ( n -- n )
: 2/ 1 rshift ;            ( n -- n )
: 2* 1 lshift ;            ( n -- n )
: cell- cell - ;           ( a -- a )
: cell+ cell + ;           ( a -- a )
: cells 2* ;               ( n -- n )
: ?dup dup if dup then ;   ( n -- | n n  )
: >= < invert ;            ( n n -- f )
: >  swap < ;              ( n n -- f )
: u> swap u< ;             ( u u -- f )
: u>= u< invert ;          ( u u -- f )
: <> = invert ;            ( n n -- f )
: 0<> 0= invert ;          ( n n -- f )
: 0> 0 > ;                 ( n -- f )
: 0< 0 < ;                 ( n -- f )
: 2dup over over ;         ( n1 n2 -- n1 n2 n1 n2 )
: 2drop drop drop ;        ( n n -- )
: tuck swap over ;         ( n1 n2 -- n2 n1 n2 )
: +! tuck @ + swap ! ;     ( n a -- )
: 1+! 1 swap +! ;          ( a -- )
: 1-! -1 swap +! ;         ( a -- )
: execute >r ;             ( cfa -- )
: c@ dup ( -2 and ) @ swap 1 and if 8 rshift else $ff and then ; ( b -- c )
: c!                       ( c b -- )
	swap $ff and dup 8 lshift or swap
	swap over dup ( -2 and ) @ swap 1 and 0 = $ff xor
	>r over xor r> and xor swap ( -2 and ) store drop ;

: !io ;                    ( -- : Initialize I/O devices )
: rx? ( -- c -1 | 0 : read in a character of input from UART )
	iUart @ $0100 and 0=
	if
		$0400 oUart ! iUart @ $ff and -1
	else
		0
	then ;

: 40ns begin dup while 1- repeat drop ; hidden ( n -- : wait for 'n'*40ns + 30us )
: ms for 25000 40ns next ; ( n -- : wait for 'n' milliseconds )

: tx! ( c -- : write a character to UART )
	begin iUart @ $0800 and until ( Wait until TX FIFO is not full )
	$2000 or oUart ! ;            ( Write character out )

: um+ ( w w -- w carry )
	over over + >r
	r@ 0 < invert >r
	over over and
	0 < r> or >r
	or 0 < r> and invert 1 +
	r> swap ;

: rp! ( n -- , R: ??? -- ??? : set the return stack pointer )
	begin dup rp@ = 0= while rdrop repeat drop ;

( @todo Implement "sp!" )

( With the built in words defined in the assembler, and the words
defined so far, all of the primitive words needed by eForth should
be available. "doList" and "doLit" do not need to be implemented. )

( ======================== Forth Kernel ===================== )

( ======================== Word Set ========================= )

: 0<= 0> 0= ;                             ( n n -- f )
: 0>= 0< 0= ;                             ( n n -- f )
: 2! ( d a -- ) tuck ! cell+ ! ;          ( n n a -- )
: 2@ ( a -- d ) dup cell+ @ swap @ ;      ( a -- n n )
: here cp @ ;                             ( -- a )
: source #tib 2@ ;                        ( -- a u )
: source-id _id @ ;                       ( -- 0 | -1 )
: pad here pad-length + ;                 ( -- a )
: @execute @ ?dup if execute then ;       ( cfa -- )
: 3drop 2drop drop ;                      ( n n n -- )
: bl =bl ;                                ( -- c )
: within over - >r - r> u< ;              ( u lo hi -- t )
: not -1 xor ;                            ( n -- n )
: dnegate invert >r invert 1 um+ r> + ;   ( d -- d )
: d+  >r swap >r um+ r> r> + + ;          ( d d -- d )
: d=  >r swap r> = >r = r> and ;          ( d d -- f )
: d<> d= 0= ;                             ( d d -- f )
: abs dup 0< if negate then ;             ( n -- u )
: count  dup 1+ swap c@ ;                 ( cs -- b u )
: rot >r swap r> swap ;                   ( n1 n2 n3 -- n2 n3 n1 )
: -rot swap >r swap r> ;                  ( n1 n2 n3 -- n3 n1 n2 )
: min over over < if drop else nip then ; ( n n -- n )
: max over over > if drop else nip then ; ( n n -- n )
: >char $7f and dup 127 =bl within if drop [char] _ then ; ( c -- c )
: tib #tib cell+ @ ;                      ( -- a )
: echo _echo @execute ;                   ( c -- )
: key? _key? @execute ;                   ( -- c -1 | 0 )
: key begin key? until ;                  ( -- c )
: allot cp +! ;                           ( u -- )
: , here dup cell+ cp ! ! ;               ( u -- )
: /string over min rot over + -rot - ;    ( b u1 u2 -- b u : advance a string u2 characters )
: last pwd @ ;                            ( -- pwd )
: emit _emit @execute ;                   ( c -- : write out a char )
: toggle over @ xor swap ! ;              ( a u -- : xor value at addr with u )
: cr =cr emit =lf emit ;                  ( -- )
: space =bl emit ;                        ( -- )
: pick ?dup if swap >r 1- pick r> swap exit then dup ; ( @bug does not work for high stack depths - mashes the return stack )
: roll  dup 0> if swap >r 1- roll r> swap else drop then ;
: ndrop for aft drop then next ;          ( n1 ... nu u -- )
: type begin dup while swap count emit swap 1- repeat 2drop ; ( b u -- : print a string )
: $type begin dup while swap count >char emit swap 1- repeat 2drop ; ( b u -- : print a string )
: print count type ;                      ( b -- )
: nuf? ( -- f ) key? dup if 2drop key =cr = then ;
: ?exit if rdrop then ;                   ( n --, R: n -- n | )
: 2rdrop r> rdrop rdrop >r ;              ( R n n -- )
: decimal? 48 58 within ;                 ( c -- f : decimal char? )
: lowercase? [char] a [char] { within ;   ( c -- f : is character lower case? )
: uppercase? [char] A [char] [ within ;   ( c -- f : is character upper case? )
: >upper dup lowercase? if =bl xor then ; ( c -- c : convert to upper case )
: >lower dup uppercase? if =bl xor then ; ( c -- c : convert to lower case )
: nchars swap 0 max for aft dup emit then next drop ; ( +n c -- : emit c n times  ) 
: spaces =bl nchars ;                     ( +n -- )
: cmove for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ; ( b b u -- )
: fill swap for swap aft 2dup c! 1+ then next 2drop ; ( b u c -- )
: switch 2dup @ >r @ swap ! r> swap ! ; ( a a -- : swap contents )

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
: holds begin dup while 1- 2dup + c@ hold repeat 2drop ; ( a u -- )
: # ( u -- u ) base @ extract hold ;
: #s ( u -- 0 ) begin # dup while repeat ;
: sign ( n -- ) 0< if [char] - hold then ;
: #> ( w -- b u ) drop hld @ pad over - ;
: binary ( -- ) 2 base ! ;
: octal ( -- ) 8 base ! ;
: decimal ( -- ) 10 base ! ;
: hex ( -- ) 16 base ! ;

: str   ( n -- b u : convert a signed integer to a numeric string )
	dup >r                ( save a copy for sign )
	abs                   ( use absolute of n )
	<# #s                 ( convert all digits )
	r> sign               ( add sign from n )
	#> ;                  ( return number string addr and length )

: .r  >r str r> over - spaces type ; ( n n : print n, right justified by +n )
: u.r >r <# #s #> r> over - spaces type ; ( u +n -- : print u right justified by +n)
: u. <# #s #> space type ; ( u -- : print unsigned number )
: . base @ 10 xor if u. exit then str space type ; ( n -- print space, signed number )

: ? @ . ; ( a -- : display the contents in a memory cell )
: 2. swap . . ;
: .base ( -- ) base @ decimal dup . base  ! ;

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

: address $3fff and ; ( a -- a : mask off address bits )
: nfa address cell+ ; ( pwd -- nfa : move to name field address)
: cfa nfa dup count nip + cell + ; ( pwd -- cfa : move to code field address )
: .id nfa print space ; ( pwd -- : print out a word )

: logical 0= 0= ; hidden ( n -- f )
: immediate? @ $4000 and logical ; ( pwd -- f : is immediate? )
: inline?    @ $8000 and logical ; ( pwd -- f : is inline? )

: find ( a -- pwd 1 | pwd -1 | a 0 : find a word in the dictionary )
	>r
	last address
	begin
		dup
	while
		dup nfa count r@ count =string
		if ( found! )
			dup immediate? if 1 else -1 then 
			rdrop exit 
		then
		@ address
	repeat
	drop r> 0 ;

: words ( -- : list all the words in the dictionary )
	space last address begin dup while dup .id @ address repeat drop cr ;

: numeric? ( char -- n|-1 : convert character in 0-9 a-z range to number )
	>lower
	dup lowercase? if 87 - exit then ( 97 = 'a', +10 as 'a' == 10 )
	dup decimal?   if 48 - exit then ( 48 = '0' )
	drop -1 ;

: digit? >lower numeric? base @ u< ; ( c -- f : is char a digit given base )

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
	over c@ $2D = if 1 /string -1 >r else 0 >r then ( -negative )
	over c@ $24 = if 1 /string hex then ( $hex )
	do-number
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
		2 spaces $type
	next drop r> base ! ;

location _test 0

: lookfor ( b u c -- b u : skip until _test succeeds )
	>r 
	begin
		dup 
	while
		over c@ r@ - r@ =bl = _test @execute if rdrop exit then
		1 /string 
	repeat rdrop ; hidden

: skipTest if 0> else 0<> then ; hidden ( n f -- f )
: scanTest skipTest invert ; hidden    ( n f -- f )
: skip ' skipTest _test ! lookfor ;
: scan ' scanTest _test ! call lookfor ;

( @todo store tmp on the return stack with stack magic )
location tmp 0

( @bug ." xxx" creates a string of size 4, the space is
not consumed in the previous parse )
: parse  ( c -- b u ; <string> )
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
	tmp @ =bl <> if 1+ then ( <-- this is a hack, relate to the string length bug )
	( ============================================== )
	>in +! ; 

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
: unused $4000 here - ;
: .free unused u. ;

: preset sp@ ndrop tib #tib cell+ ! ;
: [  0 state ! ; immediate 
: ] -1 state ! ;

: error ( n --, R: ??? -- ??? )
	?dup if
		[char] ? emit ( print error message )
		. [char] # cr ( print error number )
		preset        ( reset machine )
		[             ( back into interpret mode )
		1 rp!         ( reset machine )
	then ;     

: doLit $8000 or , ; hidden
: literal ( n -- : write a literal into the dictionary )
	dup $8000 and ( n > $7fff ? )
	if
		invert doLit =invert , ( store inversion of n the invert it )
	else
		doLit ( turn into literal, write into dictionary )
	then ; immediate

: compile, ( cfa -- : compile a code field address )
	2/ $4000 or , ; ( 2/ ? )

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
			drop space print -13 error
		then
	then ;

: "immediate" last address $4000 toggle ;
( @todo reimplement hide by removing the word to hide from the linked list
of words )
\ : "hide" =bl token find if address $4000 toggle else -13 error then ;
: .ok state @ 0= if space OK print space then cr ;
: eval begin token dup count nip while interpret repeat drop _prompt @execute ;
: quit quitLoop: preset [ begin query eval again branch 0 ;

( @todo save marker for dictionary and previous word pointer, restore this
on any kind of failure that occurs between ":" and ";" )
: !csp sp@ csp ! ;
: ?csp sp@ csp @ xor if -22 error then ;
: +csp csp 1+! ;
: -csp csp 1-! ;
: ?compile state @ 0= if -14 error then ; ( fail if not compiling )
location redefined " redefined"
\ : ?unique dup find if drop redefined print cr else drop then ;
\ : smudge last address $4000 toggle ;
: ":" !csp here last address , pwd ! ( smudge ) =bl word ( ?unique ) count + aligned cp ! ] ; 
: "'" token find if cfa else -13 error then ; immediate
: ";" =exit , [ ?csp ( smudge ) ; immediate
: jumpz, 2/ $2000 or , ; hidden
: jump, 2/ ( $0000 or ) , ; hidden
: "begin" ?compile here -csp ; immediate
: "until" ?compile jumpz, +csp ; immediate
: "again" ?compile jump, +csp ; immediate
: "if" ?compile here 0 jumpz, -csp ; immediate
: doThen  here 2/ over @ or swap ! ; hidden
: "then" ?compile doThen +csp ; immediate
: "else" ?compile here 0 jump, swap doThen ; immediate
: "while" ?compile call "if" ; immediate
: "repeat" ?compile swap call "again" call "then" ; immediate
: recurse ?compile last cfa compile, ; immediate
: tail ?compile last cfa jump, ; immediate
: create call ":" ' doVar compile, [ ; ( @todo does> )
: >body cell+ ;
: "variable" create 0 , ;
: ":noname" ( smudge ) ( <-- bug ) here ] !csp ;
: "for" ?compile =>r , here -csp ; immediate
: doNext r> r> ?dup if 1- >r @ >r exit then cell+ >r ; 
: "next" ?compile ' doNext compile, , +csp ; immediate

: [compile] ( -- ; <string> ) call "'" compile, ; immediate
: compile ( -- ) r> dup @ , cell+ >r ;

: do$ ( -- a ) r> r@ r> count + aligned >r swap >r ;
: $"| ( -- a ) do$ ;
: ."| ( -- ) do$ print ; ( compile-only )
: $,' ( -- ) 34 word count + aligned cp ! ;
: $" ( -- ; <string> ) ' $"| compile, $,' ; immediate
: ." ( -- ; <string> ) ' ."| compile, $,' ; immediate

: updated? block-dirty @ ;
: update -1 block-dirty ! ;
: +block blk @ + ; 
: b/buf block-size ;
: empty-buffers block-invalid blk ! ;
: save-buffers
	blk @ block-invalid  = if exit then
	updated? 0= if exit then
	block-buffer b/buf blk @ _bsave @execute error 
	0 block-dirty ! ;

: flush save-buffers empty-buffers ;

: block ( k -- a )
	_binvalid @execute                         ( check validity of block number )
	dup blk @ = if drop block-buffer exit then ( block already loaded ) 
	flush
	dup >r block-buffer b/buf r> _bload @execute error 
	blk !
	block-buffer ;

: buffer block ; ( k -- a )

 : evaluate ( a u -- )
	0 >in !
	#tib 2@ >r >r 
	swap #tib 2!
	eval
	r> r> #tib 2! ;


\ : load block b/buf evaluate throw ;
\ : --> 1 +block load ;

: scr blk ;
: pipe 124 emit ; hidden
location border-string "+---|---"
: border 3 spaces 7 for border-string print next cr ; hidden
: list block cr border 15 for 15 r@ - 2 u.r pipe dup c/l $type pipe cr c/l + next border drop ;
: thru over - for dup . dup list 1+ key drop next drop ;
: blank =bl fill ;

: ccitt ( crc c -- crc : calculate polynomial $1021 AKA "x16 + x12 + x5 + 1" )
 	over 256/ xor           ( crc x )
 	dup  4  rshift xor      ( crc x )
 	dup  5  lshift xor      ( crc x )
 	dup  12 lshift xor      ( crc x )
 	swap 8  lshift xor ;    ( crc )
 
: crc $ffff >r ( b u -- u : calculate ccitt-ffff CRC )
	begin dup while 
		r> over c@ ccitt >r
		1 /string
	repeat 2drop r> ;
 
\ : square dup * ;
\ : limit rot min max ;
\ : odd 1 and logical ;
\ : evan odd invert ;
\ : nor or invert ;
\ : nand and invert ;
\ : bell 7 emit ;
\ : under >r dup r> ;
\ : 2nip >r >r 2drop r> r> ; ( n1 n2 n3 n4 -- n3 n4 )
\ : 2over >r >r 2dup r> swap >r swap r> r> -rot ; ( n1 n2 n3 n4 – n1 n2 n3 n4 n1 n2 )
\ : 2swap >r -rot r> -rot ; ( n1 n2 n3 n4 – n3 n4 n1 n2 )
\ : 2tuck 2swap 2over ; ( n1 n2 n3 n4 – n3 n4 n1 n2 n3 n4 )
\ : 4drop 2drop 2drop ; ( n1 n2 n3 n4 -- )
\ : ?if ' "dup" compile, call "if" ; immediate
\ : ?dup-if ' "?dup" compile, call "if" ; immediate
\ : trip dup dup ;
\ : <=> 2dup > if 2drop -1 exit then < ; ( x y -- z : spaceship operator! )
\ : bounds over + swap ;
\ : average um+ 2 um/mod nip ;
\ \ : gcd dup if tuck mod tail then drop ; ( u1 u2 -- u : greatest common divisor )
\ \ : lcm 2dup gcd / * ; ( u1 u2 -- u : lowest common multiple of u1 and u2 )
\ ( @todo add log, log2, factorial, etcetera )
\ : ?\ 0= if call "\" then ;
\ : ?( 0= if call "(" then ;

( ======================== Word Set ========================= )

( ======================== Memory Interface ================= )

( @note Flash is word addressable, the RAM is bit addressable? )

variable mwindow  0
variable mram     0
constant mwin-max $3ff

: mcontrol! ( u -- : write to memory control register )
	mram @ if $400 or then  ( select correct memory device )
	mwin-max invert    and   ( mask off control bits )
	mwindow @ mwindow and or ( or in higher address bits )
	oMemControl ! ;          ( and finally write in control )

: m! ( n a -- : write to non-volatile memory )
	oMemAddrLow !
	oMemDout !
	20 40ns 
	$8800 mcontrol! 
	20 40ns 
	$0000 mcontrol! ;

: m@ ( a -- n : read from non-volatile memory )
	oMemAddrLow !
	$4800 mcontrol! ( read enable mode )
	20 40ns 
	iMemDin @        ( get input )
	$0000 mcontrol! ;

: mrst ( -- : reset non-volatile memory )
	$2000 mcontrol!
	20 40ns 
	$0000 mcontrol! ;

: mdump ( a u -- : dump non-volatile memory )
	begin
		dup
	while
		over . 58 emit over m@ . cr
		1 /string
	repeat 2drop cr ;

: r1+ r> r> 1+ >r >r ; hidden ( R: n -- n : increment first return stack value )

: minvalid ( k -- k : is 'k' a valid block number, throw on error )
	dup block-invalid = if -35 error then ; hidden

( @note msave and mload could be factored into a single word with MAKE/DOER,
they are exactly the same apart from the direction of the read/write )
: msave ( a u k -- f )
	minvalid
	512 um* mwindow ! >r
	begin
		dup
	while
		over @ r@ m! r1+
		cell /string
	repeat
	rdrop 2drop 0 ; hidden

: mload ( a u k -- f )
	minvalid
	512 um* mwindow ! >r
	begin
		dup
	while
		over r@ m@ r1+ swap !
		cell /string
	repeat
	rdrop 2drop 0 ; hidden

( ======================== Memory Interface ================= )

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
constant vgaInit       $7A \ $007A

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
\ 	vga.screen    @ if $80 or then
\ 	vga.on        @ if $40 or then
\ 	vga.screen    @ if $20 or then
\ 	vga.cursor.en @ if $10 or then
\ 	vga.color     @ $7 and or
\ 	$4004 ! ;

: vga! ( vga.screen @ if 4096 + then ) $C000 or ! ; ( n a -- : write to VGA memory and adjust cursor position )

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
return stack space and uses arithmetic too freely

Dividing by vgaX [80] can be done with:
	$CCCD um* swap drop 6 rshift
vgaY [40] with:
	$CCCD um* swap drop 5 rshift

This is clearly magic. )

: vgaX*  dup  6 lshift swap 4 lshift + ; hidden ( n -- n : 80* )
: vgaTextSizeMod dup vgaTextSize u> if vgaTextSize - then ; hidden

: terminal ( n a -- a : act like a terminal )
	swap
	dup =lf = if drop vgaX / 1+ dup 0 swap at-xy vgaX* exit then
	dup =cr = if drop exit then
	dup =bs = if drop 1- exit then
	swap vgaTextSizeMod tuck dup 1+ vgaX /mod at-xy vga! 1+ ;

: segments! o8SegLED ! ; ( n -- : display a number on the LED 7/8 segment display )
: led!      oLeds ! ; ( n -- : write to LED lights )
: switches  iSwitches  @ ;
: timer!    oTimerCtrl ! ;
: timer@    iTimerDin  @ ;

: ps2? ( -- c -1 | 0 : like "rx?" but for the PS/2 keyboard )
	iPs2 @ dup $ff and swap $0100 and if -1 else drop 0 then ;

: input ( -- c -1 | 0 : look for input from UART or PS/2 keyboard )
	rx? if -1 else ps2? then ;

: output ( c -- : write to UART and VGA display )
	dup tx!
	( drop exit \ @note The rest of this word is responsible for the large return stack usage )
	cursor @ terminal
	dup 1+ cursor @ u< if drop page else cursor ! then ;

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
	$ffff oIrcMask !
	$ffff oTimerCtrl !
	1 seti ; )

( ======================== Miscellaneous ==================== )


( ======================== Block Editor  ==================== )

( Block Editor Help 

      n    move to next block
      p    move to previous block
    # d    delete line in current block
      x    erase current block
      e    evaluate current block
    # i    insert line
 # #2 ia   insert at line #2 at column #
      q    quit editor loop
    # b    set block number
      s    save block and write it out
      u    update block )

( 
: [block] blk @ block ; hidden
: [check] dup b/buf c/l / u>= if -24 error then ; hidden
: [line] [check] c/l * [block] + ; hidden 
: [clean] ; hidden \ @todo call >char on modified line 
: n  1 +block block ;
: p -1 +block block ;
: d [line] c/l bl fill ;
: x [block] b/buf bl fill ;
: s update save-buffers ;
: q rdrop rdrop ;
\ : e blk @ load char drop ;
: ia c/l * + dup b/buf swap - >r [block] + r> accept drop [clean] ;
: i 0 swap ia ;
: u update ;
: b block ;
: l blk @ list ;

: editor ; \ This should start up the editor loop
)

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
\ : CSI escape emit [char] [ emit ; hidden
\ : 10u. base @ >r decimal <# #s #> type r> base ! ; hidden ( n -- : print a number in decimal )
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
\ 	CSI 10u. 
\ 	if  
\ 		59 emit  49 emit ( ." ;1" ) 
\ 	then 
\ 	[char] m emit
\ 	_emit colorize switch ;
\ 
\ : ansi.at-xy CSI 10u. 59 emit 10u. [char] H emit ; ( x y -- : set ANSI terminal cursor position to x y )
\ : ansi.page CSI 2 10u. [char] J emit 1 1 ansi.at-xy ; ( -- : clear ANSI terminal screen and move cursor to beginning )
\ 
\ location HIDE_CURSOR "?25l" : ansi.hide-cursor CSI HIDE_CURSOR print ; ( -- : hide cursor )
\ location SHOW_CURSOR "?25h" : ansi.show-cursor CSI SHOW_CURSOR print ; ( -- : show the cursor )
\ : ansi.save-cursor CSI [char] s emit ; ( -- : save cursor position )
\ : ansi.restore-cursor CSI [char] u emit ; ( -- : restore saved cursor position )
\ : ansi.reset-color colorize @ 0= if exit then CSI 0 10u. [char] m emit ; ( -- : reset terminal color to its default value)
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
	!io                    \ Initialize I/O

	here . .free cr  .ok 

	_boot @execute  ( _boot contains zero by default, does nothing )
	branch quitLoop ( jump to main interpreter loop if _boot returned )

( ======================== User Code ======================== )


.set pwd $pwd
.set cp  $pc

.set _key?     input      ( execution vector of ?key,   default to input. )
.set _emit     output     ( execution vector of emit,   default to output )
.set _expect   accept     ( execution vector of expect, default to 'accept'. )
.set _tap      ktap       ( execution vector of tap,    default the ktap. )
.set _echo     output     ( execution vector of echo,   default to output. )
.set _prompt   .ok        ( execution vector of prompt, default to '.ok'. )
.set _boot     0          ( @execute does nothing if zero )
.set _bload    mload
.set _bsave    msave
.set _binvalid minvalid

