( This program is written in a pseudo Forth like language, it is not
Forth and does not behave like it, it just looks like it. This should
be thought of as assembly code and not Forth.

A lot of the code has been taken verbatim from "The Zen of eForth by
C. H. Ting". Some routines have been adapted from the j1eforth implementation
available at https://github.com/samawati/j1eforth.

For a grammar of the language look into the file "h2.c".

Execution begins at a label called "start".

TODO:
* Turn this into a literate file
* Fix interrupt code
* There is a bit of confusion over what words accept as execution tokens,
some take pointers to the CFA of a word, other the PWD field
* Load initial VGA screen from NVRAM?
* Reimplement 'hide'
* Implement user variables, and copy variables to user area on cold boot
* Display contents of first block on the screen at start up
* Fix interrupts
* A peephole optimizer and a super optimizer for the CPU could be created.
* A quasi-graphics mode using the block characters could be made, it could
then be used for some primitive games
* Vectored words use @execute, which does nothing if the value to execute is
zero, this can cause problems with words that drop or produce values.
* Memory allocator, simple block based file system and utilities,
soft floating point or fixed pointer library, elementary math functions, ...
 - http://www.drdobbs.com/cpp/optimizing-math-intensive-applications-w/207000448
 - https://stackoverflow.com/questions/2187379/floating-point-library-for-embedded-application

* For tiny math routines, look at:
http://files.righto.com/calculator/sinclair_scientific_simulator.html
https://en.wikipedia.org/wiki/Sinclair_Scientific
Also:
https://groups.google.com/forum/#!topic/comp.lang.forth/_bx4dJFb9R0
http://www.figuk.plus.com/build/arith.htm

Forth To Do:
* vocabularies, make/doer, ...
* Fix PARSE )

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

constant c/l           64    ( characters per line in a block )
constant l/b           16    ( lines in a block )

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
constant vgaInit       $7A

constant vgaX          80
constant vgaY          40
constant vgaTextSize   3200

( ======================== System Constants ================= )

( ======================== System Variables ================= )

( Execution vectors for changing the behaviour of the program,
they are set at the end of this file )
variable _key?      0  ( -- c -1 | 0 : new character available? )
variable _emit      0  ( c -- : emit character )
variable _expect    0  ( "accept" vector )
variable _tap       0  ( "tap" vector, for terminal handling )
variable _echo      0  ( c -- : emit character )
variable _prompt    0  ( -- : display prompt )
variable _boot      0  ( -- : execute program at startup )
location _bload     0  ( a u k -- f : load block )
location _bsave     0  ( a u k -- f : save block )
location _binvalid  0  ( k -- k : throws error if k invalid )
location _page      0  ( -- : clear screen )

location pwd        0  ( Present Word Variable: Set at end of file )
location cp         0  ( Dictionary Pointer: Set at end of file )
location csp        0  ( current data stack pointer - for error checking )
location _id        0  ( used for source id )
location rendezvous 0  ( saved cp and pwd )
.allocate cell
location seed       1  ( seed used for the PRNG )
location cursor     0  ( index into VGA text memory )
location handler    0  ( current handler for throw/catch )
location version $666  ( eForth version information )
variable >in        0  ( Hold character pointer when parsing input )
variable state      0  ( compiler state variable )
variable hld        0  ( Pointer into hold area for numeric output )
variable base       10 ( Current output radix )
variable span       0  ( Hold character count received by expect   )
variable #tib       0  ( Current count of terminal input buffer    )
location tib-buf    0  ( ... and address )
.set tib-buf $pc       ( set tib-buf to current dictionary location )
.allocate tib-length   ( allocate enough for the terminal input buffer )
.allocate cell         ( plus one extra cell for safety )
location word-buf   0  ( transient word buffer starts here )
.allocate 34           ( allocate room for it )

constant block-invalid   -1 ( block invalid number )
constant block-size    1024 ( size of a block )
variable blk             -1 ( current blk loaded )
location block-dirty      0 ( -1 if loaded block buffer is modified )
location block-buffer     0 ( block buffer starts here )
.allocate block-size

location _test            0 ( used in skip/test )
location tmp              0 ( used in the parser )

location .s-string        " <sp"      ( used by .s )
location see.unknown      "(no-name)" ( used by 'see' for calls to anonymous words )
location see.lit          "LIT"       ( decompilation -> literal )
location see.alu          "ALU"       ( decompilation -> ALU operation )
location see.call         "CAL"       ( decompilation -> Call )
location see.branch       "BRN"       ( decompilation -> Branch )
location see.0branch      "BRZ"       ( decompilation -> 0 Branch )
location see.immediate    " immediate " ( used by "see", for immediate words )
location see.inline       " inline "    ( used by "see", for inline words )
location OK               "ok"          ( used by "prompt" )
location redefined        " redefined" 
location border-string    "+---|---"
location hi-string        "eFORTH V"

( ======================== System Variables ================= )

( ======================== Forth Kernel ===================== )

: [-1] -1 ; hidden
: ! store drop ;           ( n a -- )
: 256* 8 lshift ; hidden   ( u -- u )
: 256/ 8 rshift ; hidden   ( u -- u )
: 1+ 1 + ;                 ( n -- n )
: negate invert 1 + ;      ( n -- n )
: - invert 1 + + ;         ( n n -- n )
\ : 2+ 2 + ;               ( n -- n )
\ : 2- 2 - ;               ( n -- n )
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
: 1+!  1 swap +! ; hidden  ( a -- )
: 1-! [-1] swap +! ; hidden  ( a -- )
: execute >r ;             ( cfa -- )
: c@ dup ( -2 and ) @ swap 1 and if 8 rshift else $ff and then ; ( b -- c )
: c!                       ( c b -- )
	swap $ff and dup 8 lshift or swap
	swap over dup ( -2 and ) @ swap 1 and 0 = $ff xor
	>r over xor r> and xor swap ( -2 and ) store drop ;
\ : c, cp @ c! cp 1+! ;    ( c -- )

: !io vgaInit oVgaCtrl ! 0 ien 0 oIrcMask ! ; ( -- : initialize I/O )

: rx? ( -- c -1 | 0 : read in a character of input from UART )
	iUart @ $0100 and 0=
	if
		$0400 oUart ! iUart @ $ff and [-1]
	else
		0
	then ;

: 40ns begin dup while 1- repeat drop ; hidden ( n -- : wait for 'n'*40ns + 30us )
: ms for 25000 40ns next ; ( n -- : wait for 'n' milliseconds )

\ : simulation? cpu-id $dead = ; ( -- f : are we in the matrix? )

( @bug Waiting until the TX FIFO is not full does not work in the
hardware, something must be broken, it works fine in the simulation
and speeds things up greatly )
: tx! ( c -- : write a character to UART )
	\ begin iUart @ $1000 and 0= until ( Wait until TX FIFO is not full )
	begin iUart @ $0800 and until ( Wait until TX FIFO is empty )
	$2000 or oUart ! ;            ( Write character out )

: um+ ( w w -- w carry )
	over over + >r
	r@ 0 < invert >r
	over over and
	0 < r> or >r
	or 0 < r> and invert 1 +
	r> swap ;

: rp! ( n -- , R: ??? -- ??? : set the return stack pointer )
	r> swap begin dup rp@ = 0= while rdrop repeat drop >r ;

( With the built in words defined in the assembler, and the words
defined so far, all of the primitive words needed by eForth should
be available. "doList" and "doLit" do not need to be implemented. )

( ======================== Forth Kernel ===================== )

( ======================== Word Set ========================= )

\ : 0<= 0> 0= ;                           ( n n -- f )
\ : 0>= 0< 0= ;                           ( n n -- f )
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
\ : not -1 xor ;                          ( n -- n )
: dnegate invert >r invert 1 um+ r> + ;   ( d -- d )
\ : dabs dup 0< if dnegate then ;         ( d -- d )
\ : d+  >r swap >r um+ r> r> + + ;        ( d d -- d )
\ : d=  >r swap r> = >r = r> and ;        ( d d -- f )
\ : d<> d= 0= ;                           ( d d -- f )
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
: /string over min rot over + -rot - ;    ( b u1 u2 -- b u : advance a string u2 characters )
: last pwd @ ;                            ( -- pwd )
: emit _emit @execute ;                   ( c -- : write out a char )
: toggle over @ xor swap ! ;              ( a u -- : xor value at addr with u )
: cr =cr emit =lf emit ;                  ( -- )
: space =bl emit ;                        ( -- )
: pick ?dup if swap >r 1- pick r> swap exit then dup ; ( @bug does not work for high stack depths - mashes the return stack )
\ : roll  dup 0> if swap >r 1- roll r> swap else drop then ;
: ndrop for aft drop then next ;          ( n1 ... nu u -- )
: type begin dup while swap count emit swap 1- repeat 2drop ; ( b u -- : print a string )
: $type begin dup while swap count >char emit swap 1- repeat 2drop ; hidden ( b u -- : print a string )
: print count type ; hidden               ( b -- )
: nuf? ( -- f ) key? if =cr = else 0 then ; ( -- f )
\ : ?exit if rdrop then ;                   ( n --, R: n -- n | )
\ : 2rdrop r> rdrop rdrop >r ;              ( R n n -- )
: decimal? 48 58 within ; hidden            ( c -- f : decimal char? )
: lowercase? [char] a [char] { within ; hidden  ( c -- f : is character lower case? )
: uppercase? [char] A [char] [ within ; hidden  ( c -- f : is character upper case? )
\ : >upper dup lowercase? if =bl xor then ; ( c -- c : convert to upper case )
: >lower dup uppercase? if =bl xor then ; hidden ( c -- c : convert to lower case )
: nchars swap 0 max for aft dup emit then next drop ; hidden ( +n c -- : emit c n times  )
: spaces =bl nchars ;                     ( +n -- )
: cmove for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ; ( b b u -- )
: fill swap for swap aft 2dup c! 1+ then next 2drop ; ( b u c -- )
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
\ : m* 2dup xor 0< >r abs swap abs um* r> if dnegate then ; ( n n -- d )
\ : */mod  >r m* r> m/mod ;  ( n n n -- r q )
\ : */  */mod nip ;          ( n n n -- q )
\ : s>d dup 0< ;             ( n -- d : single to double )

: radix base @ dup 2 - 34 u> if 10 base ! 40 -throw then ; hidden
: digit  9 over < 7 and + 48 + ; hidden      ( u -- c )
: extract  0 swap um/mod swap digit ; hidden ( n base -- n c )
: <#  pad hld ! ;                            ( -- )
: ?hold hld @ cp @ u< if 17 -throw then ; hidden ( -- )
: hold  hld @ 1- dup hld ! ?hold c! ;        ( c -- )
\ : holds begin dup while 1- 2dup + c@ hold repeat 2drop ; ( a u -- )
: #  radix extract hold ;                    ( u -- u )
: #s begin # dup while repeat ;              ( u -- 0 )
: sign  0< if [char] - hold then ;           ( n -- )
: #>  drop hld @ pad over - ;                ( w -- b u )
\ : binary  2 base ! ;                       ( -- )
\ : octal  8 base ! ;                        ( -- )
: decimal  10 base ! ;                       ( -- )
: hex  16 base ! ;                           ( -- )
: str dup >r abs <# #s r> sign #> ; hidden   ( n -- b u : convert a signed integer to a numeric string )
\ :  .r >r str r> over - spaces type ;       ( n n : print n, right justified by +n )
: u.r >r <# #s #> r> over - spaces type ;    ( u +n -- : print u right justified by +n)
: u.  <# #s #> space type ;                  ( u -- : print unsigned number )
:  .  radix 10 xor if u. exit then str space type ; ( n -- print space, signed number )
: ? @ . ;                                    ( a -- : display the contents in a memory cell )
\ : 2. swap . . ;                            ( n n -- )
\ : .base  radix decimal dup . base  ! ;    ( -- )

: ?dictionary dup $3f00 u> if 8 -throw then ; hidden
: , here dup cell+ ?dictionary cp ! ! ; ( u -- )

: pack$ ( b u a -- a ) \ null fill
	aligned dup >r over
	dup 0 cell um/mod drop
	- over +  0 swap !  2dup c!  1+ swap cmove  r> ;

: ^h ( bot eot cur c -- bot eot cur )
	>r over r@ < dup
	if
		=bs dup echo =bl echo echo
	then r> + ;

: tap dup echo over c! 1+ ; ( bot eot cur c -- bot eot cur )

: ktap ( bot eot cur c -- bot eot cur )
	dup =cr xor
	if =bs xor
		if =bl tap else ^h then
		exit
	then drop nip dup ;

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

: =string ( a1 u2 a1 u2 -- f : string equality )
	>r swap r> ( a1 a2 u1 u2 )
	over xor if 3drop  0 exit then
	for ( a1 a2 )
		aft
			count >r swap count r> xor
			if 2drop rdrop 0 exit then
		then
	next 2drop [-1] ; hidden

: address $3fff and ; hidden ( a -- a : mask off address bits )
: nfa address cell+ ; ( pwd -- nfa : move to name field address)
: cfa nfa dup count nip + cell + $fffe and ; ( pwd -- cfa : move to code field address )
: .id nfa print space ; ( pwd -- : print out a word )

: logical 0= 0= ; hidden ( n -- f )
: immediate? @ $4000 and logical ; hidden ( pwd -- f : is immediate? )
: inline?    @ $8000 and logical ; hidden ( pwd -- f : is inline? )

: find ( a -- pwd 1 | pwd -1 | a 0 : find a word in the dictionary )
	>r
	last address
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
	drop r> 0 ;

: words ( -- : list all the words in the dictionary )
	space last address begin dup while dup .id @ address repeat drop cr ;

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

: 5u.r 5 u.r ; hidden
: dm+ 2/ for aft dup @ space 6 u.r cell+ then next ; ( a u -- a )

constant dump-length 16

: dump ( a u -- )
	dump-length /
	for
		cr dump-length 2dup
		over 5 u.r 58 emit space
		dm+ -rot
		2 spaces $type
	next drop ;

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
: skip ' skipTest _test ! lookfor ; hidden
: scan ' scanTest _test ! lookfor ; hidden

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
: .( 41 parse type ; immediate
: "(" 41 parse 2drop ; immediate
: ) ; immediate
: "\" #tib @ >in ! ; immediate
: ?length dup word-length u> if 19 -throw then ; hidden
: word parse ?length here pack$ ;          ( c -- a ; <string> )
: token =bl parse ?length word-buf pack$ ; ( -- a ; <string> )
: char token count drop c@ ;               ( -- c; <string> )
: .s ( -- ) cr sp@ for aft r@ pick . then next .s-string print ;
: unused $4000 here - ; hidden
: .free unused u. ; hidden
: preset sp@ ndrop tib #tib cell+ ! 0 >in ! 0 _id ! ; hidden
: [  0 state ! ; immediate
: ] [-1] state ! ;

: save last cp @ rendezvous 2! ; hidden
: restore rendezvous 2@ cp ! pwd ! ; hidden

: on-error ( n -- : perform actions on error )
	?dup if
		[char] ? emit ( print error message )
		. [char] # cr ( print error number )
		restore       ( restore dictionary to point before error )
		preset        ( reset machine )
		[             ( back into interpret mode )
	then ; hidden

: doLit $8000 or , ; hidden
: literal ( n -- : write a literal into the dictionary )
	dup $8000 and ( n > $7fff ? )
	if
		invert doLit =invert , ( store inversion of n the invert it )
	else
		doLit ( turn into literal, write into dictionary )
	then ; immediate

: make-callable 2/ $4000 or ; hidden ( cfa -- instruction )
: compile, make-callable , ; ( cfa -- : compile a code field address )

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
			drop space print 13 -throw
		then
	then ;

: "immediate" last address $4000 toggle ;
: .ok state @ 0= if space OK print space then cr ;
: eval begin token dup count nip while interpret repeat drop _prompt @execute ; hidden
: quit quitLoop: preset [ begin query ' eval catch on-error again ;

: pace 11 emit ;
: xio  ' accept _expect ! _tap ! _echo ! _prompt ! ;
: file ' pace ' "drop" ' ktap xio ;
: hand ' .ok  '  emit  ' ktap xio ;
: console ' rx? _key? ! ' tx! _emit ! hand ;

( @todo ";" could perform a few optimizations, if the previous word is an ALU
instruction it might be possible to merge the exit into it, if the instruction
does not modify the return stack pointer or set the R->PC flag. Alternatively
if last word called in a function is a call, this can be replaced with a branch
instead

@bug It is possible to create words that are empty strings

@todo Make sure the dictionary pointer does not overflow, or issue diagnostic
when it does )

: !csp sp@ csp ! ; hidden
: ?csp sp@ csp @ xor if 22 -throw then ; hidden
: +csp csp 1+! ; hidden
: -csp csp 1-! ; hidden
: ?compile state @ 0= if 14 -throw then ; hidden ( fail if not compiling )
: ?unique dup find if drop redefined print cr else drop then ; hidden ( a -- a )
: ?nul count 0= if 16 -throw then 1- ; hidden ( b -- : check for zero length strings )
: ":" align save !csp here last address ,  =bl word ?nul ?unique count + aligned cp ! pwd ! ] ;
: "'" token find if cfa else 13 -throw then ; immediate
: ";" ?compile ?csp =exit , save [ ; immediate
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
: tail ?compile last address cfa jump, ; immediate
: create call ":" ' doVar compile, [ ; ( @todo does> )
: >body ( dup @ $4000 or <> if 31 -throw then ) cell+ ;
: doDoes r> 2/ here 2/ last address cfa dup cell+ doLit ! , ; hidden 
: does> ?compile ' doDoes compile, nop ; immediate
: "variable" create 0 , ;
\ : doConst r> @ ; hidden
\ : "constant" create , ' doConst make-callable last address cfa ! ;
: ":noname" here ] !csp ;
: "for" ?compile =>r , here -csp ; immediate
: doNext r> r> ?dup if 1- >r @ >r exit then cell+ >r ; hidden
: "next" ?compile ' doNext compile, , +csp ; immediate

\ : [compile] ?compile  call "'" compile, ; immediate ( -- ; <string> )
\ : compile ( -- ) r> dup @ , cell+ >r ;

: do$ r> r@ r> count + aligned >r swap >r ; hidden ( -- a )
: $"| do$ nop ; hidden                             ( -- a : do string NB. nop needed to fool optimizer )
: ."| do$ print ; hidden                           ( -- : print string )
: $,' 34 word count + aligned cp ! ; hidden        ( -- )
: $"  ?compile ' $"| compile, $,' ; immediate      ( -- ; <string> )
: ."  ?compile ' ."| compile, $,' ; immediate      ( -- ; <string> )
: abort 0 rp! quit ;
: abort" ?compile ." ' abort compile, ; immediate

: updated? block-dirty @ ; ( -- f )
: update [-1] block-dirty ! ; ( -- )
: +block blk @ + ;
: b/buf block-size ;
: empty-buffers block-invalid blk ! ;
: save-buffers
	blk @ block-invalid = updated? 0= or if exit then
	block-buffer b/buf blk @ _bsave @execute throw
	0 block-dirty ! ;
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

\ : buffer block ; ( k -- a )

: evaluate ( a u -- )
	_prompt @ >r  0 _prompt !
	_id     @ >r [-1] _id !
	>in     @ >r  0 >in !
	#tib 2@ >r >r
	#tib 2!
	' eval catch
	r> r> #tib 2!
	r> >in !
	r> _id !
	r> _prompt !
	throw ;

: load block b/buf evaluate ;
: --> 1 +block load ;
: scr blk ;
: pipe 124 emit ; hidden
: border 3 spaces 7 for border-string print next cr ; hidden
: list _page @execute block cr border 15 for 15 r@ - 2 u.r pipe dup c/l $type pipe cr c/l + next border drop ;
: thru over - for dup . dup list 1+ nuf? if rdrop drop exit then next drop ; ( k1 k2 -- )
: blank =bl fill ;
\ : blank-thru over - for dup block b/buf blank update 1+ next drop flush ;

: -trailing ( b u -- b u : remove trailing spaces )
	for
		aft =bl over r@ + c@ <
			if r> 1+ exit then
		then
	next 0 ;

( display a message from a line within a block )
: msg dup 4 rshift block swap 15 and c/l * + c/l -trailing $type cr ; ( u -- )

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

: random seed @ dup 15 lshift ccitt dup seed ! ; ( -- u )

: ver version @ ;
: hi !io save hex cr hi-string print ver <# # # 46 hold # #> type cr here . .free cr ;

\ : square dup * ;
\ : limit rot min max ;
\ : odd 1 and logical ;
\ : evan odd invert ;
\ : nor or invert ;
\ : nand and invert ;
\ : bell 7 emit ;
\ : under >r dup r> ;
\ : 2nip >r >r 2drop r> r> ; ( n1 n2 n3 n4 -- n3 n4 )
\ : 2over >r >r 2dup r> swap >r swap r> r> -rot ; ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
\ : 2swap >r -rot r> -rot ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
\ : 2tuck 2swap 2over ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 n3 n4 )
\ : 4drop 2drop 2drop ; ( n1 n2 n3 n4 -- )
\ : ?if ' "dup" compile, call "if" ; immediate
\ : ?dup-if ' "?dup" compile, call "if" ; immediate
\ : trip dup dup ;
\ : <=> 2dup > if 2drop [-1] exit then < ; ( x y -- z : spaceship operator! )
\ : bounds over + swap ;
\ : average um+ 2 um/mod nip ;
\ : gcd gcdStart: dup if tuck mod branch gcdStart then drop ; ( u1 u2 -- u : greatest common divisor )
\ : lcm 2dup gcd / * ; ( u1 u2 -- u : lowest common multiple of u1 and u2 )
\ ( @todo add log, log2, factorial, etcetera )
\ : ?\ 0= if call "\" then ;
\ : ?( 0= if call "(" then ;
\ : log  >r 0 swap begin swap 1+ swap r@ / dup 0= until drop 1- rdrop ; ( u base -- u )
\ : log2    0 swap begin swap 1+ swap   2/ dup 0= until drop 1- ; ( u -- u )
\ : factorial dup 2 < if drop 1 exit then 1 swap for aft r@ 1+ * then next ;

( ======================== Word Set ========================= )

( ======================== See ============================== )

( @warning This disassembler is experimental, and liable not
to work / break everything it touches )

location bcount 0
: bcounter! bcount @ 0= if 2/ over swap -  bcount ! else drop then ; hidden ( u a -- u )
: -bcount   bcount @ if ( bcount ? ) bcount   1-! then ; hidden ( -- )
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

: .name name ?dup 0= if see.unknown then print ; hidden

: mask-off 2dup and = ; hidden ( u u -- u f )

i.end2t: 2*
i.end:   5u.r rdrop exit
: i.print print abits ; hidden

: instruction ( decode instruction )
	over >r
	$8000 mask-off if see.lit     print $7fff and      branch i.end then
	$6000 mask-off if see.alu     i.print              branch i.end then
	$4000 mask-off if see.call    i.print dup 2*       5u.r rdrop space .name exit then
	$2000 mask-off if see.0branch i.print r@ bcounter! branch i.end2t then
	                  see.branch  i.print r@ bcounter! branch i.end2t ; hidden

: continue? ( u a -- f )
	bcount @ if 2drop [-1] exit then
	over $e000 and 0= if u> exit else drop then
	dup ' doVar make-callable = if drop 0 exit then ( print next address ? )
	=exit and =exit <> ; hidden

: decompile ( a -- a )
	dup 5u.r 58 emit dup @ 5u.r space
	dup @ instruction cr
	cell+ ; hidden

: decompiler ( a -- : decompile starting at address )
	0 bcount !
	dup 2/ >r
	begin dup @ r@ continue? while decompile -bcount ( nuf? ) repeat decompile rdrop
	drop ; hidden

: see
	token find 0= if 11 -throw then
	cr 58 emit space dup .id
	dup inline?    if see.inline    print then
	dup immediate? if see.immediate print then
	cr
	cfa decompiler space 59 emit cr ;

( ======================== See ============================== )

( ======================== Memory Interface ================= )

( @note Flash is word addressable, the RAM is bit addressable? )

variable mwindow  0
variable mram     0
constant mwin-max $1ff

: mcontrol! ( u -- : write to memory control register )
	mram @ if $400 or then   ( select correct memory device )
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
	dup block-invalid = if 35 -throw then ; hidden

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

( ======================== Password/Users =================== )

\ \ @todo modify emit/ktap to transmit '*' instead of password characters,
\ \ and make a password cracker 
\ 
\ location user>     "user> "
\ location password> "password> "
\ 
\ location user0.prev 0      
\ location user0.pass $576F    ( guest )
\ location user0.name "guest"
\ 
\ location user1.prev 0        .set user1.prev user0.prev
\ location user1.pass $89CD    ( dangerzone )
\ location user1.name "archer"
\ 
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
\ 
\ : _user? begin user> print query token cr find.user ?dup until ; hidden
\ 
\ : retry >r begin r@ catch 0= until rdrop ; ( xt -- )
\ 
\ : user?     ' _user?     retry ; hidden
\ : password? ' _password? retry ; hidden
\ 
\ : login cr user? cell+ @ password? ;
 
( ======================== Password/Users =================== )

( ======================== Miscellaneous ==================== )

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

: at-xy 256* or oVgaCursor ! ; ( x y -- : set terminal cursor to x-y position )

: vgaX*  dup  6 lshift swap 4 lshift + ; hidden ( n -- n : 80* )
: vgaTextSizeMod dup vgaTextSize u> if vgaTextSize - then ; hidden

: terminal ( n a -- a : act like a terminal )
	swap
	dup =lf = if drop 0 vgaX um/mod nip 1+ dup 0 swap at-xy vgaX* exit then
	dup =cr = if drop exit then
	dup =bs = if drop 1- exit then
	swap vgaTextSizeMod tuck dup 1+ 0 vgaX um/mod at-xy vga! 1+ ; hidden

: segments! o8SegLED ! ; ( n -- : display a number on the LED 7/8 segment display )
: led!      oLeds ! ; ( n -- : write to LED lights )
: switches  iSwitches  @ ;
: timer!    oTimerCtrl ! ;
: timer     iTimerDin  @ ;

: ps2? ( -- c -1 | 0 : like "rx?" but for the PS/2 keyboard )
	iPs2 @ dup $ff and swap $0100 and if [-1] else drop 0 then ;

: input ( -- c -1 | 0 : look for input from UART or PS/2 keyboard )
	rx? if [-1] else ps2? then ;

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

\ Testing for the interrupt mechanism, interrupts do not
\ work correctly at the moment

variable icount 0

irq:
	0 ien drop
	switches led!
	icount 1+!
	1 ien drop
	exit

.set 12 irq
: irqTest
	$0040 oIrcMask !
	$ffff oTimerCtrl !
	1 ien drop ;


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
location .forth 0
location .editor 0
: forth .forth @ pwd ! ;
: editor .editor @ pwd ! ;
.set .forth $pwd

\ @todo implement vocabularies and put these words in the editor vocabulary,
\ also make an insert mode that reads up to 15 lines of text
: [block] blk @ block ; hidden
: [check] dup b/buf c/l / u>= if 24 -throw then ; hidden
: [line] [check] c/l * [block] + ; hidden
: [clean] ; hidden \ @todo call >char on modified line
: n  1 +block block drop ;
: p [-1] +block block drop ;
: d [line] c/l =bl fill ;
: x [block] b/buf =bl fill ;
: s update save-buffers ;
: q rdrop rdrop ;
: e blk @ load char drop ;
: ia c/l * + [block] + tib >in @ + swap #tib @ >in @ - cmove  call "\" ;
: i 0 swap ia ;
: u update ;
: b block ;
: l blk @ list ;
: yank pad c/l ; hidden
: c [line] yank >r swap r> cmove ; \ n -- yank line number to buffer
: y [line] yank cmove ; \ n -- copy yank buffer to line
: ct swap y c ; \ n1 n2 -- copy line n1 to n2
: ea [line] c/l evaluate ;
: sw 2dup y [line] swap [line] swap c/l cmove c ;
.set .editor $pwd
)

( ======================== Starting Code ==================== )

start:
.set entry start
	!io
	page
	hi
	cpu-id segments!
	\ login
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
.set _bload    mload      ( execution vector of _bload, used in block )
.set _bsave    msave      ( execution vector of _bsave, used in block )
.set _binvalid minvalid   ( execution vector of _invalid, used in block )
.set _page     page       ( execution vector of _page, used in list )
