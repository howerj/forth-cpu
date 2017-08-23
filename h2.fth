( This program is written in a pseudo Forth like language, it is not
Forth and does not behave like it, it just looks like it. This should
be thought of as assembly code and not Forth.

A lot of the code has been taken verbatim from "The Zen of eForth by
C. H. Ting". Some routines have been adapted from the j1eforth implementation
available at https://github.com/samawati/j1eforth.

For a grammar of the language look into the file "h2.c", or "readme.md".

Execution begins at a label called "start".

* Reformat this document to be 64 characters wide maximum, this
is so it could be stored in block memory.

* For tiny math routines, look at:
http://files.righto.com/calculator/sinclair_scientific_simulator.html
https://en.wikipedia.org/wiki/Sinclair_Scientific
Also:
https://groups.google.com/forum/#!topic/comp.lang.forth/_bx4dJFb9R0
http://www.figuk.plus.com/build/arith.htm

Forth To Do:
* Fix PARSE
* Add do...loop, case statements
* Implement some words from the C library, like all of "ctype.h" and
put them in block storage. )

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

.mode 3   ( Turn word header compilation and optimization on )
.built-in ( Add the built in words to the dictionary )

constant =exit         $601c ( op code for exit )
constant =invert       $6600 ( op code for invert )
constant =>r           $6147 ( op code for >r )
constant =bl           32    ( blank, or space )
constant =cr           13    ( carriage return )
constant =lf           10    ( line feed )
constant =bs           8     ( back space )
constant =escape       27    ( escape character )

constant c/l           64    ( characters per line in a block )
constant l/b           16    ( lines in a block )

constant dump-width    16    ( number of columns for 'dump' )
constant tib-length    80    ( size of terminal input buffer )
constant pad-length    80    ( pad area begins HERE + pad-length )
constant word-length   31    ( maximum length of a word )

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
constant oVgaAddr      $400B ( VGA memory address output )

( Inputs: $6000 - $7FFF )
constant iUart         $4000 ( Matching registers for iUart )
constant iSwitches     $4001 ( Switch control [on/off] )
constant iTimerCtrl    $4002 ( Timer control, not really needed )
constant iTimerDin     $4003 ( Current timer value )
constant iVgaTxtDout   $4004 ( VGA text output, currently broken )
constant iPs2          $4005 ( PS/2 keyboard input )
constant iLfsr         $4006 ( Input from Linear Feedback Shift Register )
constant iMemDin       $4007 ( Memory input for reads )

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
constant vgaInit       $7A   ( VGA On, Cursor On, Cursor Blinks, Green Text )

constant vgaX          80    ( Number of columns in the text mode VGA display )
constant vgaY          40    ( Number of rows in the text mode VGA display )
constant vgaTextSize   3200  ( vgaX * vgaY )

( ======================== System Constants ================= )

( ======================== System Variables ================= )

( Execution vectors for changing the behaviour of the program,
they are set at the end of this file )
location _key?      0  ( -- c -1 | 0 : new character available? )
location _emit      0  ( c -- : emit character )
location _expect    0  ( "accept" vector )
location _tap       0  ( "tap" vector, for terminal handling )
location _echo      0  ( c -- : emit character )
location _prompt    0  ( -- : display prompt )
location _boot      0  ( -- : execute program at startup )
location _bload     0  ( a u k -- f : load block )
location _bsave     0  ( a u k -- f : save block )
location _binvalid  0  ( k -- k : throws error if k invalid )
location _page      0  ( -- : clear screen )
location _message   0  ( n -- : display an error message )

location flash-voc  0     ( flash and memory word set )
location cp         0  ( Dictionary Pointer: Set at end of file )
location csp        0  ( current data stack pointer - for error checking )
location _id        0  ( used for source id )
location rendezvous 0  ( saved cp and pwd )
.allocate cell
location seed       1  ( seed used for the PRNG )
location cursor     0  ( index into VGA text memory )
location handler    0  ( current handler for throw/catch )
variable >in        0  ( Hold character pointer when parsing input )
variable state      0  ( compiler state variable )
variable hld        0  ( Pointer into hold area for numeric output )
variable base       10 ( Current output radix )
variable span       0  ( Hold character count received by expect   )

constant #vocs            8 ( number of vocabularies in allowed )
variable forth-wordlist   0 ( set at the end near the end of the file )
location context          0 ( holds current context for vocabulary search order )
.allocate 14                ( ... space for context )
location #tib             0 ( Current count of terminal input buffer    )
location tib-buf          0 ( ... and address )
.set tib-buf $pc            ( set tib-buf to current dictionary location )
.allocate tib-length        ( allocate enough for the terminal input buffer )
.allocate cell              ( plus one extra cell for safety )
constant block-invalid   -1 ( block invalid number )
constant block-size    1024 ( size of a block )
variable blk             -1 ( current blk loaded )
location block-dirty      0 ( -1 if loaded block buffer is modified )
location block-buffer     0 ( block buffer starts here )
.allocate block-size

location _blockop         0             ( used in 'mblock' )
location bcount           0             ( instruction counter used in 'see' )
location _test            0             ( used in skip/test )
location tmp              0             ( used in the parser )
location .s-string        " <sp"        ( used by .s )
location see.unknown      "(no-name)"   ( used by 'see' for calls to anonymous words )
location see.lit          "LIT"         ( decompilation -> literal )
location see.alu          "ALU"         ( decompilation -> ALU operation )
location see.call         "CAL"         ( decompilation -> Call )
location see.branch       "BRN"         ( decompilation -> Branch )
location see.0branch      "BRZ"         ( decompilation -> 0 Branch )
location see.immediate    " immediate " ( used by "see", for immediate words )
location see.inline       " inline "    ( used by "see", for inline words )
location OK               "ok"          ( used by "prompt" )
location redefined        " redefined"  ( used by ":" when a word has been redefined )
location hi-string        "eFORTH V"    ( used by "hi" )
location loading-string   "loading..."

( ======================== System Variables ================= )

( ======================== Forth Kernel ===================== )

: [-1] -1 ; hidden
: ! store drop ;           ( n a -- )
: 256* 8 lshift ; hidden   ( u -- u )
: 256/ 8 rshift ; hidden   ( u -- u )
: 1+ 1 + ;                 ( n -- n )
: negate invert 1 + ;      ( n -- n )
: - invert 1 + + ;         ( n n -- n )
: 2/ 1 rshift ;            ( n -- n : NB. This isn't actually correct, just useful, "1 arshift" would be acceptable )
: 2* 1 lshift ;            ( n -- n )
: cell- cell - ;           ( a -- a )
: cell+ cell + ;           ( a -- a )
: cells 2* ;               ( n -- n )
: ?dup dup if dup then ;   ( n -- | n n  )
: >  swap < ;              ( n n -- f )
: u> swap u< ;             ( u u -- f )
: <> = invert ;            ( n n -- f )
: 0<> 0= invert ;          ( n n -- f )
: 0> 0 > ;                 ( n -- f )
: 0< 0 < ;                 ( n -- f )
: 2dup over over ;         ( n1 n2 -- n1 n2 n1 n2 )
: 2drop drop drop ;        ( n n -- )
: tuck swap over ;         ( n1 n2 -- n2 n1 n2 )
: +! tuck @ + swap ! ;     ( n a -- )
: 1+!  1 swap +! ;         ( a -- )
: 1-! [-1] swap +! ; hidden  ( a -- )
: execute >r ;             ( cfa -- )
: c@ dup ( -2 and ) @ swap 1 and if 8 rshift else $ff and then ; ( b -- c )
: c!                       ( c b -- )
	swap $ff and dup 8 lshift or swap
	swap over dup ( -2 and ) @ swap 1 and 0 = $ff xor
	>r over xor r> and xor swap ( -2 and ) store drop ;
\ : c, cp @ c! cp 1+! ;    ( c -- )

: rx? ( -- c -1 | 0 : read in a character of input from UART )
	iUart @ $0100 and 0=
	if
		$0400 oUart ! iUart @ $ff and [-1]
	else
		0
	then ; hidden

: 40ns begin dup while 1- repeat drop ; hidden ( n -- : wait for 'n'*40ns + 30us )
: ms for 25000 40ns next ; ( n -- : wait for 'n' milliseconds )

: tx! ( c -- : write a character to UART )
	begin iUart @ $1000 and 0= until ( Wait until TX FIFO is not full )
	$2000 or oUart ! ; hidden     ( Write character out )

: um+ ( w w -- w carry )
	over over + >r
	r@ 0 < invert >r
	over over and
	0 < r> or >r
	or 0 < r> and invert 1 +
	r> swap ;

: rp! ( n -- , R: ??? -- ??? : set the return stack pointer )
	r> swap begin dup rp@ = 0= while rdrop repeat drop >r ; hidden

( With the built in words defined in the assembler, and the words
defined so far, all of the primitive words needed by eForth should
be available. "doList" and "doLit" do not need to be implemented. )

( ======================== Forth Kernel ===================== )

( ======================== Word Set ========================= )

: 2! ( d a -- ) tuck ! cell+ ! ;          ( n n a -- )
: 2@ ( a -- d ) dup cell+ @ swap @ ;      ( a -- n n )
: here cp @ ;                             ( -- a )
: source #tib 2@ ;                        ( -- a u )
: source-id _id @ ;                       ( -- 0 | -1 )
: pad here pad-length + ;                 ( -- a )
: @execute @ ?dup if >r then ;            ( cfa -- )
: 3drop 2drop drop ; hidden               ( n n n -- )
: bl =bl ;                                ( -- c )
: within over - >r - r> u< ;              ( u lo hi -- t )
: dnegate invert >r invert 1 um+ r> + ;   ( d -- d )
: abs dup 0< if negate then ;             ( n -- u )
: count  dup 1+ swap c@ ;                 ( cs -- b u )
: rot >r swap r> swap ;                   ( n1 n2 n3 -- n2 n3 n1 )
: -rot swap >r swap r> ;                  ( n1 n2 n3 -- n3 n1 n2 )
: min over over < if drop else nip then ; ( n n -- n )
: max over over > if drop else nip then ; ( n n -- n )
: >char $7f and dup 127 =bl within if drop [char] _ then ; hidden ( c -- c )
: tib #tib cell+ @ ; hidden               ( -- a )
: echo _echo @execute ; hidden            ( c -- )
: key? _key? @execute ;                   ( -- c -1 | 0 )
: key begin key? until ;                  ( -- c )
: allot cp +! ;                           ( u -- )
: /string over min rot over + -rot - ;    ( b u1 u2 -- b u : advance a string u2 characters )
: last context @ @ ;                      ( -- pwd )
: emit _emit @execute ;                   ( c -- : write out a char )
: toggle over @ xor swap ! ; hidden       ( a u -- : xor value at addr with u )
: cr =cr emit =lf emit ;                  ( -- )
: space =bl emit ;                        ( -- )
: pick ?dup if swap >r 1- pick r> swap exit then dup ; ( @bug does not work for high stack depths - mashes the return stack )
: ndrop for aft drop then next ; hidden   ( n1 ... nu u -- )
: type begin dup while swap count emit swap 1- repeat 2drop ; ( b u -- : print a string )
: $type begin dup while swap count >char emit swap 1- repeat 2drop ; hidden ( b u -- : print a string )
: print count type ; hidden               ( b -- )
: nuf? ( -- f ) key =cr = ;  ( -- f : true if 'cr' pressed, blocking )
: decimal? 48 58 within ; hidden            ( c -- f : decimal char? )
: lowercase? [char] a [char] { within ; hidden  ( c -- f : is character lower case? )
: uppercase? [char] A [char] [ within ; hidden  ( c -- f : is character upper case? )
\ : >upper dup lowercase? if =bl xor then ; ( c -- c : convert to upper case )
: >lower dup uppercase? if =bl xor then ; hidden ( c -- c : convert to lower case )
: nchars swap 0 max for aft dup emit then next drop ; hidden ( +n c -- : emit c n times  )
: spaces =bl nchars ;                     ( +n -- )
: cmove for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ; ( b b u -- )
: fill swap for swap aft 2dup c! 1+ then next 2drop ; ( b u c -- )
: substitute dup @ >r ! r> ; hidden ( u a -- u : substitute value at address )
: switch 2dup @ >r @ swap ! r> swap ! ; hidden ( a a -- : swap contents )
: aligned dup 1 and if 1+ then ;          ( b -- a )
: align cp @ aligned cp ! ; hidden        ( -- )

: catch  ( xt -- exception# | 0 : return addr on stack )
	sp@ >r        ( xt : save data stack depth )
	handler @ >r  ( xt : and previous handler )
	rp@ handler ! ( xt : set current handler )
	execute       (      execute returns if no throw )
	r> handler !  (      restore previous handler )
	r> drop       (      discard saved stack ptr )
	0 ;           ( 0  : normal completion )

: throw  ( ??? exception# -- ??? exception# )
	?dup if ( exc# \ 0 throw is no-op )
		handler @ rp! ( exc# : restore prev return stack )
		r> handler !  ( exc# : restore prev handler )
		r> swap >r    ( saved-sp : exc# on return stack )
		sp@ swap - ndrop r>   ( exc# : restore stack )
		( return to the caller of catch because return )
		( stack is restored to the state that existed )
		( when catch began execution )
	then ;

: -throw negate throw ; hidden ( space saving measure )

: um/mod ( ud u -- ur uq )
	?dup 0= if 10 -throw then
	2dup u<
	if negate 15
		for >r dup um+ >r >r dup um+ r> + dup
			r> r@ swap >r um+ r> or
			if >r drop 1 + r> else drop then r>
		next
		drop swap exit
	then drop 2drop  [-1] dup ;

: m/mod ( d n -- r q ) \ floored division
	dup 0< dup >r
	if
		negate >r dnegate r>
	then
	>r dup 0< if r@ + then r> um/mod r>
	if swap negate swap then ;

: um* ( u u -- ud )
	0 swap ( u1 0 u2 ) 15
	for dup um+ >r >r dup um+ r> + r>
		if >r over um+ r> + then
	next rot drop ;

: /mod  over 0< swap m/mod ; ( n n -- r q )
: mod  /mod drop ;           ( n n -- r )
: /    /mod nip ;            ( n n -- q )
: *    um* drop ;            ( n n -- n )
: radix base @ dup 2 - 34 u> if 10 base ! 40 -throw then ; hidden
: digit  9 over < 7 and + 48 + ; hidden      ( u -- c )
: extract  0 swap um/mod swap ; hidden       ( n base -- n c )
: ?hold hld @ cp @ u< if 17 -throw then ; hidden ( -- )
: hold  hld @ 1- dup hld ! ?hold c! ;        ( c -- )
: sign  0< if [char] - hold then ;           ( n -- )
: #>  drop hld @ pad over - ;                ( w -- b u )
: #  radix extract digit hold ;              ( u -- u )
: #s begin # dup while repeat ;              ( u -- 0 )
: <#  pad hld ! ;                            ( -- )
: decimal  10 base ! ;                       ( -- )
: hex  16 base ! ;                           ( -- )
: str dup >r abs <# #s r> sign #> ; hidden   ( n -- b u : convert a signed integer to a numeric string )
\ :  .r >r str r> over - spaces type ;       ( n n : print n, right justified by +n )
: u.r >r <# #s #> r> over - spaces type ;    ( u +n -- : print u right justified by +n)
: u.  <# #s #> space type ;                  ( u -- : print unsigned number )
:  .  radix 10 xor if u. exit then str space type ; ( n -- print space, signed number )
: ? @ . ;                                    ( a -- : display the contents in a memory cell )

: pack$ ( b u a -- a ) \ null fill
	aligned dup >r over
	dup 0 cell um/mod drop
	- over +  0 swap !  2dup c!  1+ swap cmove  r> ;

: ^h ( bot eot cur c -- bot eot cur )
	>r over r@ < dup
	if
		=bs dup echo =bl echo echo
	then r> + ; hidden

: tap dup echo over c! 1+ ; hidden ( bot eot cur c -- bot eot cur )

: ktap ( bot eot cur c -- bot eot cur )
	dup =cr xor
	if =bs xor
		if =bl tap else ^h then
		exit
	then drop nip dup ; hidden

: accept ( b u -- b u )
	over + over
	begin
		2dup xor
	while
		key  dup =bl - 95 u<
		if tap else _tap @execute then
	repeat drop over - ;

: expect ( b u -- ) _expect @execute span ! drop ;
: query tib tib-length _expect @execute #tib !  drop 0 >in ! ; ( -- )

( @todo The 2dupxor instruction would be useful here )
: =string ( a1 u2 a1 u2 -- f : string equality )
	>r swap r> ( a1 a2 u1 u2 )
	over xor if 3drop  0 exit then
	for ( a1 a2 )
		aft
			count >r swap count r> xor
			if 2drop rdrop 0 exit then
		then
	next 2drop [-1] ; hidden

\ : same? ( a a -- a a f )
\ 	for
\ 		aft
\ 			over r@ cells + @
\ 			over r@ cells + @ -  ?dup
\ 			if r> drop exit then
\ 		then
\ 	next 0 ;

: address $3fff and ; hidden ( a -- a : mask off address bits )
: nfa address cell+ ; hidden ( pwd -- nfa : move to name field address)
: cfa nfa dup count nip + cell + $fffe and ; hidden ( pwd -- cfa : move to code field address )
: .id nfa print ; hidden ( pwd -- : print out a word )

: logical 0= 0= ; hidden ( n -- f )
: immediate? @ $4000 and logical ; hidden ( pwd -- f : is immediate? )
: inline?    @ $8000 and logical ; hidden ( pwd -- f : is inline? )

: search ( a a -- pwd 1 | pwd -1 | a 0 : find a word in the dictionary )
	swap >r
	begin
		dup
	while
		dup nfa count r@ count =string
		if ( found! )
			dup immediate? if 1 else [-1] then
			rdrop exit
		then
		@ address
	repeat
	drop r> 0 ; hidden

: find ( a -- pwd 1 | pwd -1 | a 0 : find a word in the dictionary )
	>r
	context
	begin
		dup @
	while
		dup @ @ r@ swap search ?dup if >r >r drop r> r> rdrop exit else drop then
		cell+
	repeat drop r> 0 ;

: numeric? ( char -- n|-1 : convert character in 0-9 a-z range to number )
	>lower
	dup lowercase? if 87 - exit then ( 97 = 'a', +10 as 'a' == 10 )
	dup decimal?   if 48 - exit then ( 48 = '0' )
	drop [-1] ; hidden

: digit? >lower numeric? base @ u< ; hidden ( c -- f : is char a digit given base )

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
	radix >r
	over c@ $2D = if 1 /string [-1] >r else 0 >r then ( -negative )
	over c@ $24 = if 1 /string hex then ( $hex )
	do-number
	r> if rot negate -rot then
	r> base ! ; hidden

: number? 0 -rot >number nip 0= ; ( b u -- n f : is number? )

: -trailing ( b u -- b u : remove trailing spaces )
	for
		aft =bl over r@ + c@ <
			if r> 1+ exit then
		then
	next 0 ; hidden

: lookfor ( b u c -- b u : skip until _test succeeds )
	>r
	begin
		dup
	while
		over c@ r@ - r@ =bl = _test @execute if rdrop exit then
		1 /string
	repeat rdrop ; hidden

: skipTest if 0> else 0<> then ; hidden    ( n f -- f )
: scanTest skipTest invert ; hidden        ( n f -- f )
: skip ' skipTest _test ! lookfor ; hidden ( b u c -- u c )
: scan ' scanTest _test ! lookfor ; hidden ( b u c -- u c )

( @todo store tmp on the return stack with stack magic )
( @bug ." xxx" creates a string of size 4, the space is
not consumed in the previous parse )

: parser ( b u c -- b u delta )
	tmp ! over >r
	tmp @ skip 2dup
	tmp @ scan swap r> - >r - r>
	tmp @ =bl <> if 1+ then ( <-- this is a hack, relate to the string length bug )
	; hidden

: parse >r tib >in @ + #tib @ >in @ - r> parser >in +! ; ( c -- b u ; <string> )
: ) ; immediate
: "(" 41 parse 2drop ; immediate
: .( 41 parse type ; immediate
: "\" #tib @ >in ! ; immediate
: ?length dup word-length u> if 19 -throw then ; hidden
: word parse ?length here pack$ ;          ( c -- a ; <string> )
: token =bl word ;
: char token count drop c@ ;               ( -- c; <string> )
: .s ( -- ) cr sp@ for aft r@ pick . then next .s-string print ;
: unused $4000 here - ; hidden
: .free unused u. ; hidden
: preset sp@ ndrop tib #tib cell+ ! 0 >in ! 0 _id ! ; hidden
: ] [-1] state ! ;
: [  0 state ! ; immediate

\ : save last cp @ rendezvous 2! ; hidden
\ : restore rendezvous 2@ cp ! context ! ; hidden ( @todo Fix restore to be vocabulary friendly )

: on-error ( n -- : perform actions on error )
	?dup if
		[char] ? emit ( print error message )
		\ . [char] # cr ( print error number )
		abs dup 60 < if 32 + _message @execute else negate . cr then
		\ restore     ( restore dictionary to point before error )
		preset        ( reset machine )
		[             ( back into interpret mode )
	then ; hidden

: ?dictionary dup $3f00 u> if 8 -throw then ; hidden
: , here dup cell+ ?dictionary cp ! ! ; ( u -- )
: doLit $8000 or , ; hidden
: literal ( n -- : write a literal into the dictionary )
	dup $8000 and ( n > $7fff ? )
	if
		invert doLit =invert , ( store inversion of n the invert it )
	else
		doLit ( turn into literal, write into dictionary )
	then ; immediate

: make-callable 2/ $4000 or ; hidden ( cfa -- instruction )
: compile, make-callable , ;         ( cfa -- : compile a code field address )
: $compile dup inline? if cfa @ , else cfa compile, then ; hidden ( pwd -- )

: interpret ( ??? a -- ??? : The command/compiler loop )
	find ?dup if
		state @
		if
			0> if \ immediate
				cfa execute
			else
				$compile exit
			then
		else
			drop cfa execute
		then
	else \ not a word
		dup count number? if
			nip
			state @ if literal then
		else
			drop space print 13 -throw
		then
	then ;

: "immediate" last address $4000 toggle ;
: .ok state @ 0= if space OK print space then cr ;
: eval begin token dup count nip while interpret repeat drop _prompt @execute ; hidden
: quit quitLoop: preset [ begin query ' eval catch on-error again ;

: evaluate ( a u -- )
	_prompt @ >r  0 _prompt !
	_id     @ >r [-1] _id !
	>in     @ >r  0 >in !
	source >r >r
	#tib 2!
	' eval catch
	r> r> #tib 2!
	r> >in !
	r> _id !
	r> _prompt !
	throw ;

: ccitt ( crc c -- crc : calculate polynomial $1021 AKA "x16 + x12 + x5 + 1" )
	over 256/ xor               ( crc x )
	dup  4  rshift xor          ( crc x )
	dup  5  lshift xor          ( crc x )
	dup  12 lshift xor          ( crc x )
	swap 8  lshift xor ; hidden ( crc )

: crc ( b u -- u : calculate ccitt-ffff CRC )
	$ffff >r
	begin
		dup
	while
		over c@ r> swap ccitt >r 1 /string
	repeat 2drop r> ;

: random seed @ dup 15 lshift ccitt dup seed ! iLfsr @ invert xor 1- ; ( -- u )

: 5u.r 5 u.r ; hidden
: dm+ 2/ for aft dup @ space 5u.r cell+ then next ; ( a u -- a )

: dump ( a u -- )
	dump-width /
	for
		cr dump-width 2dup
		over 5u.r 58 emit space
		dm+ -rot
		2 spaces $type
	next drop ;

( ==================== Extra Words =================================== )

\ : ?if ' "dup" compile, call "if" ; immediate
\ : ?dup-if ' "?dup" compile, call "if" ; immediate
\ : gcd gcdStart: dup if tuck mod branch gcdStart then drop ; ( u1 u2 -- u : greatest common divisor )
\ : lcm 2dup gcd / * ; ( u1 u2 -- u : lowest common multiple of u1 and u2 )
( : factorial 0
   dup 2 < if drop 1 exit then
   1 swap for aft r@ 1+ * then next ; )
( ==================== Extra Words =================================== )

( ==================== Advanced I/O Control ========================== )

: vga! ( vga.screen @ if 4096 + then ) $C000 or ! ; ( n a -- : write to VGA memory and adjust cursor position )
: vga@ oVgaAddr ! iVgaTxtDout @ ; ( a -- u : read VGA value at address )
: segments! o8SegLED ! ;   ( u -- : display a number on the LED 7/8 segment display )
: led!      oLeds ! ;      ( u -- : write to LED lights )
: switches  iSwitches  @ ; ( -- u : get the state of the switches)
: timer!    oTimerCtrl ! ; ( u -- )
: timer     iTimerDin  @ ; ( -- u )
: ps2? iPs2 @ dup $ff and swap $100 and if [-1] else drop 0 then ; hidden ( -- c -1 | 0 : PS/2 version of rx? )
: input rx? if [-1] else ps2? then ; hidden ( -- c -1 | 0 : UART and PS/2 Input )
: vga.at-xy 256* or oVgaCursor ! ; hidden ( x y -- : set terminal cursor to x-y position )
: vgaX* dup 6 lshift swap 4 lshift + ; hidden ( n -- n : 80* )
: vgaTextSizeMod dup vgaTextSize u> if vgaTextSize - then ; hidden

: vga.page ( -- : clear the VGA display )
	0 cursor !
	vgaTextSize
	begin
		dup
	while
		dup =bl swap vga! 1-
	repeat drop ; hidden

( @todo Vector these words, or look at the state of _emit, _echo and _key
and take appropriate action
@bug ansi.page and ansi.at-xy should only write over the UART, otherwise
this causes major problems and blows up the return stack )
: page   ( ansi.page ) vga.page ; ( -- : clear screen )
: at-xy  ( 2dup ansi.at-xy )  vga.at-xy ;

( @todo This does not handle transitions between screen correctly, it appears
a character is dropped somewhere )
: terminal ( n a -- a : act like a terminal )
	swap
	dup =lf = if drop 0 vgaX um/mod nip 1+ dup 0 swap vga.at-xy vgaX* exit then
	dup =cr = if drop exit then
	dup =bs = if drop 1- exit then
	swap vgaTextSizeMod tuck dup 1+ 0 vgaX um/mod vga.at-xy vga! 1+ ; hidden

: output ( c -- : write to UART and VGA display )
	dup tx!
	( drop exit \ @note The rest of this word is responsible for the large return stack usage )
	cursor @ terminal
	dup 1+ cursor @ u< if drop page else cursor ! then ; hidden

: pace 11 emit ; hidden
: xio  ' accept _expect ! _tap ! _echo ! _prompt ! ; hidden
: file ' pace ' "drop" ' ktap xio ;
: hand ' .ok  '  emit  ' ktap xio ; hidden
: console ' rx? _key? ! ' tx! _emit ! hand ;
: interactive ' input _key? ! ' output _emit ! hand ; hidden
: io! 1 oLfsr ! interactive vgaInit oVgaCtrl ! 0 ien 0 oIrcMask ! ; ( -- : initialize I/O )
: ver $666 ;
: hi io! ( save ) hex cr hi-string print ver <# # # 46 hold # #> type cr here . .free cr ;

( ==================== Advanced I/O Control ========================== )

( ==================== Control Structures ============================ )

( The following section implements the control structures and high level
words used for interpreting Forth. As much error checking is done
as possible so the Forth environment is easy to use. )

: !csp sp@ csp ! ; hidden
: ?csp sp@ csp @ xor if 22 -throw then ; hidden
: +csp csp 1+! ; hidden
: -csp csp 1-! ; hidden
\ : ?depth dup 0= if drop exit then sp@ 1- u> if 4 -throw then ; ( u -- )
: ?compile state @ 0= if 14 -throw then ; hidden ( fail if not compiling )
: ?unique dup last search if drop redefined print cr else drop then ; hidden ( a -- a )
: ?nul count 0= if 16 -throw then 1- ; hidden ( b -- : check for zero length strings )
: ":" align ( save ) !csp here last address ,  token ?nul ?unique count + aligned cp ! ] ;
: "'" token find if cfa else 13 -throw then ; immediate
: ";" ?compile context @ ! ?csp =exit , ( save )  [ ; immediate
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
: recurse ?compile last address cfa compile, ; immediate
: create call ":" ' doVar compile, context @ ! [ ;
: >body ( dup @ $4000 or <> if 31 -throw then ) cell+ ;
: doDoes r> 2/ here 2/ last address cfa dup cell+ doLit ! , ; hidden
: does> ?compile ' doDoes compile, nop ; immediate
: "variable" create 0 , ;
: ":noname" here ] !csp ;
: "for" ?compile =>r , here -csp ; immediate
: doNext r> r> ?dup if 1- >r @ >r exit then cell+ >r ; hidden
: "next" ?compile ' doNext compile, , +csp ; immediate
: "aft" ?compile drop here 0 jump, call "begin" swap ; immediate
: doer create =exit last cfa ! =exit ,  ;
: make
	call "'" call "'" make-callable
	state @
	if
		literal literal ' ! compile,
	else
		swap !
	then ; immediate
: [compile] ?compile  call "'" compile, ; immediate ( -- ; <string> )
: compile ( -- ) r> dup @ , cell+ >r ;

\ : [leave] rdrop rdrop rdrop ; hidden
\ : leave ?compile ' [leave] compile, ; immediate
\ : [do] r> dup >r swap rot >r >r cell+ >r ; hidden
\ : do ?compile ' [do] compile, 0 , here ; immediate
\ : [loop]
\    r> r> 1+ r> 2dup <> if >r >r @ >r exit then
\    >r 1- >r cell+ >r ; hidden
\ : [unloop] r> rdrop rdrop rdrop >r ; hidden
\ : loop ' [loop] compile, dup , ' [unloop] compile, cell- here 2/ swap ! ; immediate
\ : [i] r> r> tuck >r >r ; hidden
\ : i ?compile ' [i] compile, ; immediate

\ : back here cell- @ ; hidden ( a -- : get previous cell )
\ : call? back $e000 and $4000 = ; hidden ( -- f : is call )
\ : merge? back dup $e000 and $6000 = swap $1c and 0= and ; hidden ( -- f : safe to merge exit )
\ : redo here cell- ! ; hidden
\ : merge back $1c or redo ; hidden
\ : tail-call back $1fff and redo ; hidden ( -- : turn previously compiled call into tail call )
\ : compile-exit call? if tail-call else merge? if merge else =exit , then then ; hidden
\ : compile-exit call? if tail-call else merge? if merge then then =exit , ; hidden
\ : "exit" compile-exit ; immediate
\ : "exit" =exit , ; immediate

( Error recovery can be quite difficult when sending Forth large programs
over a serial port. One of the problems is if an error occurs in between a
colon definition the Forth interpreter would signal an error and go back into
command mode, subsequently words which are meant to be compiled are instead
executed which can cause the system to become unstable. There are potential
ways of getting around this:

1. The sender stops upon encountering an error
2. The receiver discards all input until the end of the file [if it can work
out when that is].
3. A third interpreter state in which words are discarded until a closing
';' is encountered.

To keep things simple non of these methods are used, but they highlight ways
in which the problem could be solved. )

( ==================== Control Structures ============================ )

( ==================== Strings ======================================= )

: do$ r> r@ r> count + aligned >r swap >r ; hidden ( -- a )
: $"| do$ nop ; hidden                             ( -- a : do string NB. nop needed to fool optimizer )
: ."| do$ print ; hidden                           ( -- : print string )
: $,' 34 word count + aligned cp ! ; hidden        ( -- )
: $"  ?compile ' $"| compile, $,' ; immediate      ( -- ; <string> )
: ."  ?compile ' ."| compile, $,' ; immediate      ( -- ; <string> )
: abort 0 rp! quit ;                               ( --, R: ??? --- ??? : Abort! )
: abort" ?compile ." ' abort compile, ; immediate

( ==================== Strings ======================================= )


( ==================== Block Word Set ================================ )

: updated? block-dirty @ ; hidden      ( -- f )
: update [-1] block-dirty ! ;          ( -- )
: +block blk @ + ;                     ( -- )
: b/buf block-size ;                   ( -- u )
: clean-buffers 0 block-dirty ! ; hidden
: empty-buffers clean-buffers block-invalid blk ! ;  ( -- )
: save-buffers                         ( -- )
	blk @ block-invalid = updated? 0= or if exit then
	block-buffer b/buf blk @ _bsave @execute throw
	clean-buffers ;
: flush save-buffers empty-buffers ;

( @todo extend various words like '\' to work with blocks, the
source-id word can be used by words to modify their behavior )

: block ( k -- a )
	_binvalid @execute                         ( check validity of block number )
	dup blk @ = if drop block-buffer exit then ( block already loaded )
	flush
	dup >r block-buffer b/buf r> _bload @execute throw
	blk !
	block-buffer ;

\ : loadline block swap c/l * + c/l evaluate ; ( u k -- )
: load block b/buf evaluate ;
: --> 1 +block load ;
: scr blk ;
: pipe 124 emit ; hidden
: border 3 spaces 64 45 nchars cr ; hidden
: list _page @execute block cr border 15 for 15 r@ - 2 u.r pipe dup c/l $type pipe cr c/l + next border drop ;
: thru over - for dup . dup list 1+ nuf? if rdrop drop exit then next drop ; ( k1 k2 -- )
: blank =bl fill ;
: message 16 extract swap block swap c/l * + c/l -trailing $type cr ; ( u -- )

( all words before this are now in the forth vocabulary, it is also set
later on )
.set forth-wordlist $pwd

( ==================== Block Word Set ================================ )

( ==================== See =========================================== )

( @warning This disassembler is experimental, and liable not
to work / break everything it touches )

: bcounter! bcount @ 0= if 2/ over swap -  bcount ! else drop then ; hidden ( u a -- u )
: -bcount   bcount @ if bcount 1-! then ; hidden ( -- )
: abits $1fff and ; hidden

: validate ( cfa pwd -- nfa | 0 )
	tuck cfa <> if drop 0 else nfa then ; hidden

: name ( cfa -- nfa )
	abits 2*
	>r
	last address
	begin
		dup
	while
		address dup r@ swap dup @ address swap within ( simplify? )
		if @ address r@ swap validate rdrop exit then
		address @
	repeat rdrop ; hidden

( : vocs get-order begin ?dup while swap @ space name print 1- repeat cr ; )

: .name name ?dup 0= if see.unknown then print ; hidden
: mask-off 2dup and = ; hidden ( u u -- u f )

i.end2t: 2*
i.end:   5u.r rdrop exit
: i.print print abits ; hidden

( @note A recursive version of 'see' that decompiled no-name words would complicate
things, the 'decompiler' word could be called manually on an address if desired )
: instruction ( decode instruction )
	over >r
	$8000 mask-off if see.lit     print $7fff and      branch i.end then
	$6000 mask-off if see.alu     i.print              branch i.end then
	$4000 mask-off if see.call    i.print dup 2*       5u.r rdrop space .name exit then
	$2000 mask-off if see.0branch i.print r@ bcounter! branch i.end2t then
	                  see.branch  i.print r@ bcounter! branch i.end2t ; hidden

: continue? ( u a -- f : determine whether to continue decompilation  )
	bcount @ if 2drop [-1] exit then
	over $e000 and 0= if u> exit else drop then
	dup ' doVar make-callable = if drop 0 exit then ( print next address ? )
	=exit and =exit <> ; hidden

: decompile ( a -- a : decompile a single instruction )
	dup 5u.r 58 emit dup @ 5u.r space
	dup @ instruction cr
	cell+ ; hidden

: decompiler ( a -- : decompile starting at address )
	0 bcount !
	dup 2/ >r
	begin dup @ r@ continue? while decompile -bcount ( nuf? ) repeat decompile rdrop
	drop ; hidden

: see ( --, <string> : decompile a word )
	token find 0= if 11 -throw then
	cr 58 emit space dup .id space
	dup inline?    if see.inline    print then
	dup immediate? if see.immediate print then
	cr
	cfa decompiler space 59 emit cr ;

.set forth-wordlist $pwd
( ==================== See =========================================== )

( ==================== Miscellaneous ================================= )
( Initial value of timer
  BIT     MEANING
  15   -  Enable Timer
  14   -  Reset Timer Value immediately
  13   -  Interrupt Enable
  12-0 -  Value to compare against )

\ Testing for the interrupt mechanism, interrupts do not
\ work correctly at the moment

( @bug Interrupts work in simulation but not in hardware )
variable icount 0

irq:
	switches led!
	icount 1+!
	exit

.set 12 irq

: irqTest
	$0040 oIrcMask !
	$ffff oTimerCtrl !
	1 ien drop ;

( ==================== Miscellaneous ================================= )

( ==================== Vocabulary Words ============================== )

: find-empty-cell begin dup @ while cell+ repeat ; hidden ( a -- a )

: get-order ( -- widn ... wid1 n : get the current search order )
	context
	find-empty-cell
	dup cell- swap
	context - 2/ dup >r 1- dup 0< if 50 -throw then
	for aft dup @ swap cell- then next @ r> ;

: set-order ( widn ... wid1 n -- : set the current search order )
	dup [-1]  = if drop forth-wordlist 1 set-order exit then
	dup #vocs > if 49 -throw then
	context swap for aft tuck ! cell+ then next 0 swap ! ;

\ : root  -1 set-order ; \ should contain set-order, forth-wordlist, forth, and words
: forth -1 set-order ;
: flash get-order flash-voc swap 1+ set-order ;

: .words space begin dup while dup .id space @ address repeat drop cr ; hidden
: words get-order begin ?dup while swap dup cr u. 58 emit @ .words 1- repeat ;

.set forth-wordlist $pwd

( ==================== Vocabulary Words ============================== )

( ==================== Memory Interface ============================== )

( @note The manual for the Nexys 3 board specifies that there is a PCM
memory device called the NP8P128A13T1760E, this is a device behaves like
normal flash with the addition that individual cells can be written to
without first erasing the block, this is accomplished with an extension
to the Common Flash Interface that most flash devices support. However,
there are boards the PC28F128P33BF60 on in lieu of this, which is a
normal flash device without the "bit alterable write" extension. Normal
flash memory works by erasing a block of data, setting all bits set,
writing the memory works by masking in a value, bits can be cleared in
a memory cell but not set, the can only be set by erasing a block.

@todo Map the SRAM and NVRAM into different parts of the block
number range, 65536 block numbers and index 64MB of memory, the
SRAM and NVRAM are both 16MB in size

The Nexys 3 has three memory devices, two of which are accessed over
a parallel interface. They share the same data and address bus, and
can be selected with a chip select. The signals to be controlled
are:

	+-----+-------------------------+
	| Bit | Description             |
	|-----|-------------------------|
	| 0-9 | Upper Memory Bits       |
	| 10  | Flash chip select       |
	| 11  | SRAM chip select        |
	| 12  | Memory Wait [not used]  |
	| 13  | Flash Reset             |
	| 14  | Output Enable           |
	| 15  | Write Enable            |
	+-----+-------------------------+

The usage of the output enable and write enable are mutually exclusive,
as are both of the chip selects.

)

.set context forth-wordlist
.set forth-wordlist $pwd
.pwd 0

constant memory-upper-mask  $1ff
variable memory-upper       0    ( upper bits of external memory address )
variable memory-select      0    ( SRAM/Flash select SRAM = 0, Flash = 1 )

: mcontrol! ( u -- : write to memory control register )
	$f3ff and
	memory-select @ if $400 else $800 then or  ( select correct memory device )
	memory-upper-mask invert    and            ( mask off control bits )
	memory-upper @ memory-upper-mask and or         ( or in higher address bits )
	oMemControl ! ; hidden            ( and finally write in control )

: m! ( n a -- : write to non-volatile memory )
	oMemAddrLow !
	oMemDout !
	5 40ns
	$8000 mcontrol!
	5 40ns
	$0000 mcontrol! ;

: m@ ( a -- n : read from non-volatile memory )
	oMemAddrLow !
	$4000 mcontrol! ( read enable mode )
	5 40ns
	iMemDin @        ( get input )
	$0000 mcontrol! ;

: memory-dump ( a u -- : dump non-volatile memory )
 	cr
 	begin
 		dup
 	while
 		over 5u.r 40 emit over m@ 4 u.r 41 emit over 1+ $7 and 0= if cr then
 		1 /string
 	repeat 2drop cr ;

( @todo The flash word set needs words for reading and decoding status
information of locks and for extracting information from the query registers )

: sram 0 memory-select ! ;
: flash-reset ( -- : reset non-volatile memory )
	$2000 mcontrol!
	5 40ns
	$0000 mcontrol! ; hidden
: flash-status 0 $70 m! 0 m@ dup $2a and if -34 -throw then ; ( -- status )
: flash-read   $ff 0 m! ;      ( -- )
: flash-setup  flush [-1] memory-select ! flash-reset 0 memory-upper ! ;      ( -- )
: flash-wait begin flash-status $80 and until ; hidden 
: flash-clear $50 0 m! ; ( -- clear status )
: flash-write dup $40 swap m! m! flash-wait ; ( u a -- )
: flash-read-id   $90 0 m! ; ( -- read id mode )
: flash-unlock 0 memory-upper substitute >r dup $60 swap m! $d0 swap m! r> memory-upper ! ; ( ba -- )
: flash-erase 0 memory-upper substitute >r flash-clear dup $20 swap m! $d0 swap m! flash-wait r> memory-upper ! ; ( ba -- )
: flash-query $98 0 m! ; ( -- : query mode )

: flash->sram ( a a : transfer flash memory cell to SRAM )
	[-1] memory-select ! flash-clear flash-read
	m@ 0 memory-select ! swap m! ; hidden

( @todo This should accept to double width address and a double width
length, so large sections of memory can be transfered, also a transfer
in the opposite direction should be programmed )
: transfer ( a a u -- : transfer memory block from Flash to SRAM )
	?dup 0= if 2drop exit then
	1-
	for
		2dup
		flash->sram
		1+ swap 1+ swap
	next 2drop ;
.set flash-voc $pwd

: minvalid ( k -- k : is 'k' a valid block number, throw on error )
	dup block-invalid = if 35 -throw then ; hidden

: c>m swap @ swap m! ; hidden      ( a a --  )
: m>c m@ swap ! ; hidden ( a a -- )

: mblock ( a u k -- f )
	minvalid
	512 um* memory-upper ! >r
	begin
		dup
	while
		over r@ _blockop @execute r> 1+ >r
		cell /string
	repeat
	rdrop 2drop 0 ; hidden

: memory-save ' c>m _blockop ! mblock ; hidden
: memory-load ' m>c _blockop ! mblock ; hidden

( ==================== Memory Interface ============================== )

( ==================== Users and Passwords =========================== )

\ @todo modify emit/ktap to transmit '*' instead of password characters,
\ and make a password cracker
\ @todo Move to block storage

\ location user>     "user> "
\ location password> "password> "
\ location user0.prev 0
\ location user0.pass $576F    ( guest )
\ location user0.name "guest"
\ location user1.prev 0        .set user1.prev user0.prev
\ location user1.pass $89CD    ( dangerzone )
\ location user1.name "archer"
\ location user2.prev 0        .set user2.prev user1.prev
\ location user2.pass $8E60    ( sterling )
\ location user2.name "lana"
\
\ location user.pwd  0 .set user.pwd user2.prev
\
\ : generate count dup >r crc r> ccitt ; hidden ( a -- u )
\
\ : mk.user ( -- ; <string1> <string2> : make a new user with a password  )
\ 	here user.pwd @ , user.pwd !
\ 	here 0 ,
\ 	=bl word count 1+ allot align drop
\ 	token generate swap ! ;
\
\ : ls.user ( -- : list all users )
\ 	cr
\ 	user.pwd @
\ 	begin
\ 		dup
\ 	while
\ 		dup 2 cells + space print cr
\ 		@
\ 	repeat drop cr ;
\
\ : find.user ( a -- u | 0 : find a user )
\ 	>r
\ 	user.pwd @
\ 	begin
\ 		dup
\ 	while
\ 		dup 2 cells + count r@ count =string if rdrop exit then
\ 		@
\ 	repeat rdrop drop 0 ;
\
\ : _password? ( u -- : <string> )
\ 	>r
\ 	begin
\ 		password> print
\ 		query
\ 		token cr generate r@ =  ( 1000 ms )
\ 	until rdrop ; hidden
\
\ : fake.password password> print query token drop cr ( 1000 ms ) ; hidden
\ : _user? begin user> print query token cr find.user ?dup until ; hidden
\ : retry >r begin r@ catch 0= until rdrop ; ( xt -- )
\ : user?     ' _user?     retry ; hidden
\ : password? ' _password? retry ; hidden
\ : login cr user? cell+ @ password? .ok ;

( ==================== Users and Passwords =========================== )

( ==================== Startup Code ================================== )

start:
.set entry start
	hi
	cpu-id segments!
	loading-string print
	0 0 $2000 transfer
	\ 7 load $b load $c load decimal
	
	.ok
	\ login 0 load 1 list
	_boot @execute  ( _boot contains zero by default, does nothing )
	branch quitLoop ( jump to main interpreter loop if _boot returned )

( ==================== Startup Code ================================== )

.set cp  $pc

.set _key?     input       ( execution vector of ?key,   default to input. )
.set _emit     output      ( execution vector of emit,   default to output )
.set _expect   accept      ( execution vector of expect, default to 'accept'. )
.set _tap      ktap        ( execution vector of tap,    default the ktap. )
.set _echo     output      ( execution vector of echo,   default to output. )
.set _prompt   .ok         ( execution vector of prompt, default to '.ok'. )
.set _boot     0           ( @execute does nothing if zero )
.set _bload    memory-load ( execution vector of _bload, used in block )
.set _bsave    memory-save ( execution vector of _bsave, used in block )
.set _binvalid minvalid    ( execution vector of _invalid, used in block )
.set _page     page        ( execution vector of _page, used in list )
.set _message  message     ( execution vector of _message, used in on-error)
