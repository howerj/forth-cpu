( =========================================================================== )
( === Debugging words ======================================================= )
( =========================================================================== )

: # dup . ; ( print out top of stack without altering it )

: .s ( print out stack without altering it )
  v @reg 1- dup 0= if exit then
  begin
    dup pick prnn space
    1- dup 0= 
  until
  drop
  cr
;

: _show i@ @ prnn tab i@ 1+ i!  ;
: show  ( from to -- ) ( Show dictionary storage )
  do
        i@ prnn ." :" tab
        _show _show _show _show i@ 1- i!
        cr
  loop
;

: _shstr
    2dup - @str emit tab 1- 
;
: shstr  ( from to -- ) ( Show string storage contents )
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

: regs  ( -- ) ( Print off register contents )
    16 @reg 1- 0 \ register 16 holds the maximum number of registers
    begin
        dup prnn ." :" tab dup @reg . 1+
        2dup =
    until
;

: words ( print out a list of defined forth words )
    pwd @reg 
    begin
        dup 1+ @ prn
        space
        @ dup @ 0 =   
    until
    cr
    drop
;

: header ( Shows the header of a word. )
  find 2- dup 40 + show
;
