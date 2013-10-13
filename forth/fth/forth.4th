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

\ These words represent words with no name in the dictionary, 
\ the 'invisible' words.
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
: system 7 ;

\ Constants for system call arguments
: input 0 ;
: output 1 ;

: literal immediate _push , , ;

\ ASCII chars

: 'esc' 27 ;
: '"' 34 ;
: ')' 41 ;

: 0= 0 = ;
: 0< 0 < ;
: 0> 0 > ;


: space 32 emit ;
: cr 10 emit ;
: tab 9 emit ;

: prnn 10 swap printnum ;
: . prnn cr ;

: tuck swap over ;
: nip swap drop ;
: rot >r swap r> swap ;
: <> = 0= ;
: negate -1 * ;
: -rot rot rot ;

: 2drop drop drop ;
: 2dup over over ;
: 2swap rot >r rot r> ;

: 2+ 1+ 1+ ;
: 2* 2 * ;
: 2/ 2 / ; 
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

: ?dup dup if dup then ;
: abs dup 0 < if negate then ;

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
 
: :: 	( compiles a ':' )
  [ find : , ]
;

( Helper words for create )
: '', ' ' , ;       \ A word that writes ' into the dictionary
: ',, ' , , ;       \ A word that writes , into the dictionary
: 'exit, ' exit , ; \ A word that write exit into the dictionary
: 3+ 2+ 1+ ;

( The word create involves multiple levels of indirection.
 It makes a word which has to write in values into
 another word )
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

( Store temporary filenames temporary here. )
str @reg dup iobl @reg + str !reg constant filename

0 variable strvar
: getstr"
  0 strvar !
  begin
    key dup '"' 
    = 
    if
      0 strvar @ filename + !str
      drop 1
    else
      strvar @ filename + !str 
      strvar @ 1+ strvar !
      0 
    then
  until
  filename
;

( file i/o )
: foutput
    filename getword
    filename output fopen kernel 
;

: finput
    filename getword
    filename input fopen kernel 
    drop
;

: fremove
    filename getword
    filename remove kernel
;

: frewind
    output rewind kernel
;

: system"
  getstr"
  system kernel
;

0 variable i
0 variable j

: i! i ! ;
: j! j ! ;
: i@ i @ ;
: j@ j @ ;

: do immediate ( this needs redoing so it can be nested )
  ' j! ,
  ' i! ,
  here
;

: not ( a -- ~a )
  if 0 else 1 then
;

: >= ( a b -- bool )
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

: break ( break out of a do ... loop )
  j@ i!
;

: gcd ( a b -- n ) ( greatest common divisor )
  begin
    dup
    if
      tuck mod 0
    else
      1
    then
  until
  drop
;

( =========================================================================== )

\ : vocabulary create pwd @reg , does> @ pwd !reg ;
\ vocabulary forth

finput ../fth/debug.4th    ( Debugging operations )
finput ../fth/rational.4th ( Real type )
finput ../fth/math.4th     ( Math functions )
finput ../fth/auto.4th     ( Auto generated constants )
finput ../fth/memory.4th   ( Memory allocation and manipulation )
finput ../fth/assembler.4th
foutput ../../vhdl/mem_h2.binary \ Output closed by h2.4th
finput ../fth/h2.4th
finput ../fth/welcome.4th

halt
