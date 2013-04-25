: immediate read \ exit br ?br + - * % / lshift rshift 
and or ~ xor 1+ 1- clear 0> = < > @reg @dic @var @ret @str 
!reg !dic !var !ret !str key emit dup drop swap over >r r> 
tail ' , printnum get_word strlen isnumber strnequ _find 
halt kernel

\ Howe Forth Core and Assembler.
\ @author         Richard James Howe.
\ @copyright      Copyright 2013 Richard James Howe.
\ @license        LGPL      
\ @email          howe.r.j.89@gmail.com
\ This notice cannot go at the top of the file, comments
\  will not work until the comment symbol is read in.

: true 1 exit
: false 0 exit

: CPF 13 exit \ Compile Flag 
: state CPF !reg exit
: ; immediate 
	' exit , 
	false state
exit
: r 3 ;	\ Return stack pointer
: v 4 ;	\ Variable stack pointer
: h 5 ;	\ Dictionary pointer
: str 6 ; \ String storage pointer
: here h @reg ;
: pwd 7 ;

\ change to command mode
: [ immediate false state ;
\ change to compile mode
: ] true state ;

: _push 0 ;
: _compile 1 ;
: _run 2 ;
: _define 3 ;
: _immediate 4 ;
: _read 5 ;
: _comment 6 ;
: _exit 7 ;

\ System calls
: reset 0 ;
: fopen 1 ;
: fclose 2 ;
: fflush 3 ;
: remove 4 ;
: rename 5 ;
: rewind 6 ;

\ Constants for system call arguments
: input 0 ;
: output 1 ;
: error 2 ;

: literal immediate _push , , ;

\ Error handling!
: EXF 14 ;
: _on_err read _on_err ;

: on_err ' _on_err EXF !reg ;
on_err

\ ASCII chars

: 'space' 32 ;
: 'esc' 27 ;
: '\n' 10 ;
: '\t' 9 ;
: '"' 34 ;
: ')' 41 ;

: 0= 0 = ;

: cr '\n' emit ;
: tab '\t' emit ;

: prnn 10 swap printnum ;
: . prnn cr ;
: # dup . ;

: tuck swap over ;
: nip swap drop ;
: rot >r swap r> swap ;
: -rot rot rot ;
: <> = 0= ;

: 2drop drop drop ;
: 2dup over over ;

: 2+ 1+ 1+ ;
: 2- 1- 1- ;

: negate -1 * ;

: swap- swap - ;

: if immediate 
	' ?br , 
	here 0 , 
;
: else immediate
	' br ,
	here
	0 ,
	swap dup here swap-
	swap !dic
;
: then immediate 
	dup here 
	swap- swap 
	!dic 
;

: here- here - ;

: begin immediate
	here
;
: until immediate
	' ?br ,
	here- ,
;
: always immediate
	' br ,
	here- ,
;

: while 	immediate
	' ?br ,
	0 ,
	here
;

: repeat	immediate 
	' br ,
	swap here- ,
	dup here swap- 1+ swap 1- !dic
	' drop ,
;

: abs dup 0> if negate then ;
: ?dup dup if dup then ;
: allot here + h !reg ;
: :noname immediate here _run , ] ;
: ? 0= if \ ( bool -- ) ( Conditional evaluation )
        [ '\n' literal ] key <>
        if 
            0 ? 
        then 
    then 
;

: _s \ ( key to halt on -- )
    >r
     begin 
        key dup r> dup >r  = dup 
        if 
            nip
        else 
            swap emit 
        then 
    until 
    r> drop
;

: .( 
    ')' _s
;

\ Print out a string stored in string storage
: prn \ ( str_ptr -- )
    begin
        dup @str dup 0= \ if null
        if
            2drop 1       
        else
            emit 1+ 0
        then
    until
;

\ Store a '"' terminated string in string storage
: _." \ ( -- )
    str @reg 1-
    begin
        key dup >r '"' =
        if
            r> drop 1
        else
            1+ dup r> swap !str 0
        then
    until
    2+
    str !reg
;

: ." immediate
    CPF @reg 0= if
    '"' _s
    else
        _push , str @reg ,
        _."
        ' prn ,
    then
;
 
: :: 	\ compiles a ':'
	[ 
		here 1- h !reg	\ back up dictionary pointer
		_define , 	\ write in primitive for ':'
	] 
;

: create 
	:: 
	false state 
;

: constant immediate
	create 
	_push , , 
	' exit , 
; 

: variable immediate
	create
	_push , 
	here 2+ , \ pointer to allocations
	' exit ,	\ over this
	,		\ <-- points here
;

\ There is no bounds checking at the moment...expects NUMBER INDEX
: array immediate
	create
	_push , 
	here 3 + , \ pointer to allocations
	' + ,		\ over this
	' exit ,	\ over this
	allot		\ <-- points here
;

: ban \ ( x -- )
	begin 
		[ key = literal ] 
		emit 1- dup 0= 
	until 
	cr drop 
;

: 40ban 40 ban ;

: @reg. \ ( x -- )
	@reg .
;

: _show
    2dup - @dic prnn tab 1- dup 0=
;

: show \ ( from to -- )
    40ban
    ." Dictionary contents: " cr
    40ban
    tuck swap-
    begin
        2dup - prnn ." :" tab 
        _show if 2drop cr 40ban exit then 
        _show if 2drop cr 40ban exit then 
        _show if 2drop cr 40ban exit then 
        _show
        cr
    until
    2drop
    40ban
;

: _shstr
    2dup - @str emit tab 1- dup 0=
;
: shstr \ ( from to -- )
    40ban
    ." String Storage contents: " cr
    40ban
    tuck
    swap-
    begin
        2dup - prnn ." :" tab 
        _shstr if 2drop cr 40ban exit then 
        _shstr if 2drop cr 40ban exit then 
        _shstr if 2drop cr 40ban exit then 
        _shstr
        cr
    until
    2drop
    40ban
;

: regs \ ( -- ) \ Print off register contents
    16 @reg 1- 0 \ register 16 holds the maximum number of registers
    begin
        dup prnn ." :" tab dup @reg . 1+
        2dup =
    until
;

3 constant tabvar
: tabbing \ ( counter X -- counter-1 X )
        swap dup if
            tab 
            1-
        else
            cr
            drop
            tabvar
        then swap
;

: words
    tabvar pwd @reg 
    begin
        dup 1+ @dic prn
        tabbing
        @dic dup 0=   
    until
    cr
    2drop
;

\ Store filenames (temporary) here.
str @reg dup 32 + str !reg constant filename

\ file i/o
: foutput
    filename get_word
    filename output fopen kernel 
;

: finput
    filename get_word
    filename input fopen kernel 
;

: ferror
    filename get_word
    filename error fopen kernel 
;

: fremove
    filename get_word
    filename remove kernel
;

: fcopy
    foutput
    finput
    begin
        key emit 0
    until
;

: frewind
    output rewind kernel
;

: [END]
    input fclose kernel 
    output fclose kernel 
    error fclose kernel
    \ drops temporary measure
    2drop drop 
;

: .s
    v @reg tabvar swap
    begin
        dup @var prnn 
        tabbing
        1- dup 0=
    until
    cr
    2drop
;
.( Howe Forth. ) cr
.( Base system loaded ) cr
.( Dictionary pointer: ) here . 

\ ==============================================================================
\ ASSEMBLER
\ ==============================================================================


\ : _.h 16 swap printnum ;
\ : F 15 ;
\ : .h 
\     dup F 12 lshift and 12 rshift _.h 
\     dup F 8 lshift and 8 rshift _.h 
\     dup F 4 lshift and 4 rshift _.h 
\     F and _.h 
\ ;
: _.b 2 swap printnum ;
: .b 
    dup 1 15 lshift and 15 rshift _.b
    dup 1 14 lshift and 14 rshift _.b
    dup 1 13 lshift and 13 rshift _.b
    dup 1 12 lshift and 12 rshift _.b
    dup 1 11 lshift and 11 rshift _.b
    dup 1 10 lshift and 10 rshift _.b
    dup 1 9 lshift and 9 rshift _.b
    dup 1 8 lshift and 8 rshift _.b
    dup 1 7 lshift and 7 rshift _.b
    dup 1 6 lshift and 6 rshift _.b
    dup 1 5 lshift and 5 rshift _.b
    dup 1 4 lshift and 4 rshift _.b
    dup 1 3 lshift and 3 rshift _.b
    dup 1 2 lshift and 2 rshift _.b
    dup 1 1 lshift and 1 rshift _.b
    1 0 lshift and 0 rshift _.b
;

8192 array mem
8191 constant mmax 

: pmem
    0 begin
        dup mem @dic .b cr
        dup mmax = swap 1+ swap
    until
    drop 
;

\ Stack instructions
: mDStk ;
: mRStk 2 lshift ;

0 mDStk constant d+0
1 mDStk constant d+1
3 mDStk constant d-1
2 mDStk constant d-2

0 mRStk constant r+0
1 mRStk constant r+1
3 mRStk constant r-1
2 mRStk constant r-2

\ ALU instructions
:  mALU 8 lshift ;
0  mALU  constant T
1  mALU  constant N
2  mALU  constant T+N
3  mALU  constant T&N
4  mALU  constant T|N
5  mALU  constant T^N
6  mALU  constant ~T
7  mALU  constant N=T
8  mALU  constant N<T
9  mALU  constant N<<T
10 mALU  constant T-1
11 mALU  constant R
12 mALU  constant [T]
13 mALU  constant N<<T
14 mALU  constant depth
15 mALU  constant Nu<T
16 mALU  constant T-N
17 mALU  constant ~(T^N)
\ 18 mALU constant L(T*N)   \ Low bits of multiplication
\ 19 mALU constant H(T*N)   \ High bit of multiplication
20 mALU  constant [T<-IO]  \ Get input
21 mALU  constant N->IO(T) \ Set output

: mLit 1 15 lshift ;
: T->N     1 7 lshift ;
: T->R     1 6 lshift ;
: N->[T]   1 5 lshift ;
: R->PC    1 4 lshift ;

0 variable pc
: @pc pc @dic ;
: !pc pc !dic ;
: pc++ @pc 1+ !pc ;

: !mem(pc++)
    @pc mem !dic pc++
;
: lit  \ ( x -- )
    32767 and mLit or !mem(pc++)
;

: jmp
    8191 and !mem(pc++)
;

: cjmp
    8191 and 1 13 lshift or !mem(pc++)
;

: call
    8191 and 1 14 lshift or !mem(pc++)
;

: alu[ ;
: ]alu
    8191 and 1 13 lshift or 1 14 lshift or !mem(pc++)
;

: _dup      alu[ T T->N d+1 or or ]alu ;
: _over     alu[ N T->N d+1 or or ]alu ;
: _invert   alu[ ~T ]alu ;
: _+        alu[ T+N d-1 or ]alu ;
: _1-       alu[ T-1 ]alu ;
: _=        alu[ N=T d-1 or ]alu ;
: _and      alu[ T&N d-1 or ]alu ;
: _or       alu[ T|N d-1 or ]alu ;
: _xor      alu[ T^N d-1 or ]alu ;
: _swap     alu[ N T->N or ]alu ;
: _nip      alu[ T d-1 or ]alu ;
: _drop     alu[ N d-1 or ]alu ;
: _;        alu[ T R->PC r-1 or or ]alu ;
: _>r       alu[ N T->R d-1 r+1 or or or ]alu ;
: _r>       alu[ R T->N T->R d+1 r-1 or or or or ]alu ;
: _r@       alu[ R T->N T->R d+1 or or or ]alu ;
: _@        alu[ [T] ]alu ;
: _!        alu[ N d-1 N->[T] or or ]alu ;
\ : _*      alu[ L(T*N) d-1 or ]alu ;
: _input    alu[ [T<-IO] ]alu ;
: _output   alu[ N d-2 N->IO(T) or or ]alu ;

: label immediate
	create 
	_push , @pc , 
	' exit , 
;

: start
    1 jmp
;

: stop
    pmem
    halt
;
.( Dictionary pointer: ) here . 
.( Printing stack: ) cr
.s
.( "Forth H2 Assembler" loaded, assembling source now. ) cr
foutput ../vhdl/mem_h2.binary
finput h2.fs

\ ==============================================================================
\ INTRO
\ ==============================================================================
\ ANSI terminal color codes
'esc' emit .( [2J) cr       \ Reset
'esc' emit .( [32m) cr    \ Green fg
\ 'esc' emit .( [40m) cr    \ Black bg
.( Howe Forth ) cr .( Base System Loaded ) cr
.( Memory Usuage: ) here 4 * str @reg + . cr
'esc' emit .( [31m) .( OK ) cr 'esc' emit .( [30m) cr

[END]
