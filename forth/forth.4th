\ Howe Forth: Start up code.
\ @author         Richard James Howe.
\ @copyright      Copyright 2013 Richard James Howe.
\ @license        LGPL      
\ @email          howe.r.j.89@gmail.com

: true 1 exit
: false 0 exit

: cpf 13 exit \ Compile Flag 
: state cpf !reg exit
: ; immediate 
	' exit , 
	false state
exit

\ Register constants
: r 3 ;	        \ Return stack pointer
: v 4 ;	        \ Variable stack pointer
: h 5 ;	        \ Dictionary pointer
: str 6 ;       \ String storage pointer
: pwd 7 ;       \ previous word
: exf 14 ;      \ Pointer to execution token, executed on error.
: iobl 21 ;     \ I/O buf len address
: here h @reg ;

\ Error handling!
: on_err read on_err ;  \ This word is executed when an error occurs.
find on_err exf !reg    \ Write the executable token for on_err into the dictionary.

\ change to command mode
: [ immediate false state ;
\ change to compile mode
: ] true state ;

\ These words represent words with no name in the dictionary, the 'invisible' words.
: _push 0 ;
: _compile 1 ;
: _run 2 ;

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

: literal immediate _push , , ;

\ ASCII chars

: 'esc' 27 ;
: '"' 34 ;
: ')' 41 ;

: 0= 0 = ;

: space 32 emit ;
: cr 10 emit ;
: tab 9 emit ;

: prnn 10 swap printnum ;
: . prnn cr ;
\ : # dup . ;

: tuck swap over ;
: nip swap drop ;
: rot >r swap r> swap ;
\ : <> = 0= ;
\ : ?dup dup if dup then ;
\ : negate -1 * ;
\ : abs dup 0 < if negate then ;
: -rot rot rot ;

: 2drop drop drop ;
: 2dup over over ;

: 2+ 1+ 1+ ;
: 2- 1- 1- ;

: if immediate 
	' ?br , 
	here 0 , 
;

: else immediate
	' br ,
	here
	0 ,
	swap dup here swap -
	swap !
;

: then immediate 
	dup here 
	swap - swap 
	! 
;

: begin immediate
	here
;
: until immediate
	' ?br ,
	here - ,
;

: allot here + h !reg ;
: :noname immediate here _run , ] ;
: ? 0= if \ ( bool -- ) ( Conditional evaluation )
      [ find \ literal ] execute 
    then 
;

: _( \ ( letter bool -- ) \ 
  >r \ Store bool on return stack for simplicity
     begin 
        key 2dup = \ key in a letter, test if it is equal to out terminator
        if
          2drop 1 \ Drop items, quit loop
        else 
          r> dup >r \ test for bool
          if        \ bool == 1, emit letter, bool == 0 drop it.
             emit 
          else 
             drop 
          then 
          0 \ Continue
        then 
    until 
  r> drop \ Return stack back to normal now.
;

: ( immediate ')' 0 _( ;  ( Now we have proper comments )
: .( immediate ')' 1 _( ; ( Print out word )

 ( Print out a string stored in string storage )
: prn ( str_ptr -- )
    begin
        dup @str dup 0= ( if null )
        if
            2drop 1       
        else
            emit 1+ 0
        then
    until
;

 ( Store a '"' terminated string in string storage )
: _." ( -- )
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
    cpf @reg 0= if
    '"' 1 _(
    else
        _push , str @reg ,
        _."
        ' prn ,
    then
;
 
: :: 	\ compiles a ':'
  [ find : , ]
;

\ Helper words for create
: '', ' ' , ;       \ A word that writes ' into the dictionary
: ',, ' , , ;       \ A word that writes , into the dictionary
: 'exit, ' exit , ; \ A word that write exit into the dictionary
: 3+ 2+ 1+ ;

\ The word create involves multiple levels of indirection.
\ It makes a word which has to write in values into
\ another word
: create immediate              \ This is a complicated word! It makes a
                                \ word that makes a word.
  cpf @reg if                   \ Compile time behavour
  ' :: ,                        \ Make the defining word compile a header
  '', _push , ',,               \ Write in push to the creating word
  ' here , ' 3+ , ',,           \ Write in the number we want the created word to push
  '', here 0 , ',,              \ Write in a place holder (0) and push a 
                                \ pointer to to be used by does>
  '', 'exit, ',,                \ Write in an exit in the word we're compiling.
  ' false , ' state ,           \ Make sure to change the state back to command mode
  else                          \ Run time behavour
    ::                          \ Compile a word
    _push ,                     \ Write push into new word
    here 2+ ,                   \ Push a pointer to data field
    'exit,                      \ Write in an exit to new word (data field is after exit)
    false state                 \ Return to command mode.
  then
;

: does> immediate
  'exit,                        \ Write in an exit, we don't want the
                                \ defining to run it, but the *defined* word to.
  here swap !                   \ Patch in the code fields to point to.
  _run ,                        \ Write a run in.
;

: constant create , does> @ ; 
: variable create , does> ;
: array create allot does> + ;

\ Store filenames (temporary) here.
str @reg dup iobl @reg + str !reg constant filename

\ file i/o
: foutput
    filename get_word
    filename output fopen kernel 
;

: finput
    filename get_word
    filename input fopen kernel 
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

: .s
  v @reg 1- dup 0= if exit then
  begin
    dup pick prnn space
    1- dup 0= 
  until
  drop
  cr
;

0 variable i
0 variable j

: i! i ! ;
: j! j ! ;
: i@ i @ ;
: j@ j @ ;

: do immediate
  ' j! ,
  ' i! ,
  here
;

: not
  if 0 else 1 then
;

: >=
  < not
;

: (++i)>=j
  i@ 1+ i! i@ j@ >= 
;

: loop immediate
  ' (++i)>=j ,
  ' ?br ,
  here - ,
;


: _show i@ @ prnn tab i@ 1+ i!  ;
: show \ ( from to -- ) \ Show dictionary storage
  do
        i@ prnn ." :" tab
        _show _show _show _show i@ 1- i!
        cr
  loop
;

: _shstr
    2dup - @str emit tab 1- 
;
: shstr \ ( from to -- ) \ Show string storage contents
    tuck
    swap -
    begin
        _shstr 
        _shstr 
        _shstr 
        _shstr dup 0 <
        cr
    until
    2drop
;

: regs \ ( -- ) \ Print off register contents
    16 @reg 1- 0 \ register 16 holds the maximum number of registers
    begin
        dup prnn ." :" tab dup @reg . 1+
        2dup =
    until
;

: words
    pwd @reg 
    begin
        dup 1+ @ prn
        space
        @ dup @ 0 =   
    until
    cr
    drop
;



\ Shows the header of a word.
: header
  find 2- dup 40 + show
;

\ ANSI terminal color codes
: esc 'esc' emit ;
: rst esc ." [2J" cr ;    \ Clear screen
: clr esc ." [0;0H" cr ;  \ Set cursor to 0,0
: red esc ." [31;1m" ;    \ Change text color to red.
: grn esc ." [32;1m" ;    \ ...to green.
: blu esc ." [34;1m" ;    \ ...to blue.
: nrm esc ." [0m" cr ;    \ ...to the default.

: cursor esc ." [" prnn ." ;" prnn ." H" fflush kernel ;

.( Base system loaded ) cr

\ ==============================================================================
\ ASSEMBLER
\ ==============================================================================

.( H2 Assembler ) cr
.( Consult the manual and TODO files. ) cr

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
        dup mem @ .b cr
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
0  mALU  constant T         ." TOS = "          T       prnn ." , " cr
1  mALU  constant N         ." NOS = "          N       prnn ." , " cr
2  mALU  constant R         ." RET = "          R       prnn ." , " cr
3  mALU  constant [T]       ." fetch = "        [T]     prnn ." , " cr
4  mALU  constant depth     ." depth = "      depth   prnn ." , " cr
5  mALU  constant T|N       ." TorN = "        T|N     prnn ." , " cr
6  mALU  constant T&N       ." TandN = "        T&N     prnn ." , " cr
7  mALU  constant T^N       ." TxorN = "        T^N     prnn ." , " cr
8  mALU  constant ~(T^N)    ." Txnor = "     ~(T^N)  prnn ." , " cr
9  mALU  constant ~T        ." invT = "         ~T      prnn ." , " cr
10 mALU  constant T+N       ." TplusN = "        T+N     prnn ." , " cr
11 mALU  constant N-T       ." NsubT = "        N-T     prnn ." , " cr
12 mALU  constant N<<T      ." NlslT = "       N<<T    prnn ." , " cr
13 mALU  constant N>>T      ." NrslT = "       N>>T    prnn ." , " cr
14 mALU  constant NrolT     ." NrolT = "      NrolT   prnn ." , " cr
15 mALU  constant NrorT     ." NrorT = "      NrorT   prnn ." , " cr
16 mALU  constant L(T)*L(N) ." LmulT = "  L(T)*L(N) prnn ." , " cr
17 mALU  constant Nu<T      ." NuLessT = "       Nu<T    prnn ." , " cr
18 mALU  constant N<T       ." NLessT = "        N<T     prnn ." , " cr
19 mALU  constant N=T       ." NeqT = "        N=T     prnn ." , " cr
20 mALU  constant T<0       ." TlessZ = "        T<0     prnn ." , " cr
21 mALU  constant T=0       ." TeqZ = "        T=0     prnn ." , " cr
22 mALU  constant swapbytes ." swapbytes = "  swapbytes prnn ." , " cr
23 mALU  constant seti      ." seti = "       seti    prnn ." , " cr
24 mALU  constant clri      ." clri = "       clri    prnn ." , " cr
25 mALU  constant T-1       ." TsubOne = "        T-1     prnn ." , " cr
\ ...
30 mALU  constant [T<-IO]  \ Get input
31 mALU  constant N->IO(T) \ Set output

: mLit 1 15 lshift ;
: T->N     1 7 lshift ;
: T->R     1 6 lshift ;
: N->[T]   1 5 lshift ;
: R->PC    1 4 lshift ;

0 variable pc
: @pc pc @ ;
: !pc pc ! ;
: pc++ @pc 1+ !pc ;

: !mem(pc++)
    @pc mem ! pc++
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
: _-        alu[ N-T d-1 or ]alu ;
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
: _!        alu[ N d-2 N->[T] or or ]alu ;
: _*        alu[ L(T)*L(N) d-1 or ]alu ;
: _input    alu[ [T<-IO] ]alu ;
: _output   alu[ N d-2 N->IO(T) or or ]alu ;
: _depth    alu[ depth T->N d+1 or or ]alu ;
: _seti     alu[ seti ]alu ;
: _clri     alu[ clri ]alu ;
: _swapbytes alu[ swapbytes ]alu ;


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
    input fclose kernel
    output fclose kernel
;

: halt
  31 error
;

.( "Forth H2 Assembler" loaded, assembling source now. ) cr
foutput ../vhdl/mem_h2.binary
finput h2.4th

\ Welcome message.
\ rst clr grn
.( Howe Forth ) cr .( ) cr
.( @author         ) blu .( Richard James Howe. ) grn cr
.( @copyright      ) blu .( Copyright 2013 Richard James Howe. ) grn cr
.( @license        ) blu .( LGPL ) grn cr
.( @email          ) blu .( howe.r.j.89@gmail.com ) grn cr
.( Memory Used: ) cr 
.(   Dictionary:   ) blu here 4 * str @reg + . grn
.(   Strings:      ) blu str @reg . grn
grn .( Printing out variable stack: )
blu cr 
.(                 ) .s nrm cr
red .( OK ) nrm
halt
