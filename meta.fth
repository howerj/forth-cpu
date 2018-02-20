0 <ok> ! ( Turn off 'ok' prompt )
\ ========================================================================== \
\                      This file is currently not used!
\ ========================================================================== \

\ NOTE: This file was originally from <https://github.com/howerj/embed>,
\ and is a metacompiler for the 'embed' virtual machine, it will need porting
\ so it works with the H2, however the embed VM is derived from the simulator
\ for the H2, so porting should be fairly easy.
\ @todo Reformat this file for a column width of 64, into Forth blocks.
\ @todo Add the documentation for everything back in
\ Perhaps looking at the original eForth implementations might help as well.

only forth definitions hex
variable meta       ( Metacompilation vocabulary )
meta +order definitions

variable assembler.1   ( Target assembler vocabulary )
variable target.1      ( Target dictionary )
variable tcp           ( Target dictionary pointer )
variable tlast         ( Last defined word in target )
variable tdoVar        ( Location of doVar in target )
variable tdoConst      ( Location of doConst in target )
variable tdoNext       ( Location of doNext in target )
variable tdoPrintString ( Location of .string in target )
variable tdoStringLit  ( Location of string-literal in target )
variable fence         ( Do not peephole optimize before this point )
1984 constant #version ( Version number )
5000 constant #target  ( Memory location where the target image will be built )
2000 constant #max     ( Max number of cells in generated image )
2    constant =cell    ( Target cell size )
-1   constant optimize ( Turn optimizations on [-1] or off [0] )
0    constant swap-endianess ( if true, swap the endianess )
$4280 constant pad-area    ( area for pad storage )
variable header -1 header ! ( If true Headers in the target will be generated )

1   constant verbose   ( verbosity level, higher is more verbose )
#target #max 0 fill    ( Erase the target memory location )

: ]asm assembler.1 +order ; immediate ( -- )
: a: get-current assembler.1 set-current : ; ( "name" -- wid link )
: a; [compile] ; set-current ; immediate ( wid link -- )

: ( [char] ) parse 2drop ; immediate
: \ source drop @ >in ! ; immediate
: there tcp @ ;         ( -- a : target dictionary pointer value )
: tc! #target + c! ;    ( u a -- )
: tc@ #target + c@ ;    ( a -- u )
: [last] tlast @ ;      ( -- a )
: low  swap-endianess 0= if 1+ then ; ( b -- b )
: high swap-endianess    if 1+ then ; ( b -- b )
: t! over ff and over high tc! swap 8 rshift swap low tc! ; ( u a -- )
: t@ dup high tc@ swap low tc@ 8 lshift or ; ( a -- u )
: 2/ 1 rshift ;                ( u -- u )
: talign there 1 and tcp +! ;  ( -- )
: tc, there tc! 1 tcp +! ;     ( c -- )
: t,  there t!  =cell tcp +! ; ( u -- )
: tallot tcp +! ;              ( n -- )
: update-fence there fence ! ; ( -- )
: $literal                     ( <string>, -- )
  [char] " word count dup tc, 1- for count tc, next drop talign update-fence ;
: tcells =cell * ;             ( u -- a )
: tbody 1 tcells + ;           ( a -- a )
: meta! ! ;                    ( u a -- )
: dump-hex #target there 16 + dump ; ( -- )
: locations ( -- : list all words and locations in target dictionary )
  target.1 @ 
  begin 
    dup 
  while 
    dup 
    nfa count type space dup
    cfa >body @ u. cr
    $3fff and @ 
  repeat drop ;

: display ( -- : display metacompilation and target information )
  verbose 0= if exit then
  hex
  ." COMPILATION COMPLETE" cr
  verbose 1 u> if 
    dump-hex cr 
    ." TARGET DICTIONARY: " cr
    locations
  then
  ." HOST: "       here        . cr
  ." TARGET: "     there       . cr
  ." HEADER: "     #target 20 dump cr ;

: checksum #target there crc ; ( -- u : calculate CRC of target image )

: save-hex ( -- : save target binary to file )
   #target #target there + (save) throw ;

: finished ( -- : save target image and display statistics )
   display
   only forth definitions hex
   ." SAVING... " save-hex ." DONE! " cr
   ." STACK> " .s cr ;

: [a] ( "name" -- : find word and compile an assembler word )
  token assembler.1 search-wordlist 0= abort" [a]? " 
  cfa compile, ; immediate

: asm[ assembler.1 -order ; immediate ( -- )

\ There are five types of instructions, which are differentiated from each
\ other by the top bits of the instruction. 

a: #literal $8000 a; ( literal instruction - top bit set )
a: #alu     $6000 a; ( ALU instruction, further encoding below... )
a: #call    $4000 a; ( function call instruction )
a: #?branch $2000 a; ( branch if zero instruction )
a: #branch  $0000 a; ( unconditional branch )

\ ALU Operations
a: #t      0000 a; ( T = t )
a: #n      0100 a; ( T = n )
a: #r      0200 a; ( T = Top of Return Stack )
a: #[t]    0300 a; ( T = memory[t] )
a: #n->[t] 0400 a; ( memory[t] = n )
a: #t+n    0500 a; ( n = n+t, T = carry )
a: #t*n    0600 a; ( n = n*t, T = upper bits of multiplication )
a: #t&n    0700 a; ( T = T and N )
a: #t|n    0800 a; ( T = T  or N )
a: #t^n    0900 a; ( T = T xor N )
a: #~t     0a00 a; ( Invert T )
a: #t-1    0b00 a; ( T == t - 1 )
a: #t==0   0c00 a; ( T == 0? )
a: #t==n   0d00 a; ( T = n == t? )
a: #nu<t   0e00 a; ( T = n < t )
a: #n<t    0f00 a; ( T = n < t, signed version )
a: #n>>t   1000 a; ( T = n right shift by t places )
a: #n<<t   1100 a; ( T = n left  shift by t places )
a: #sp@    1200 a; ( T = variable stack depth )
a: #rp@    1300 a; ( T = return stack depth )
a: #sp!    1400 a; ( set variable stack depth )
a: #rp!    1500 a; ( set return stack depth )
a: #save   1600 a; ( Save memory disk: n = start, T = end, T' = error )
a: #tx     1700 a; ( Transmit Byte: t = byte, T' = error )
a: #rx     1800 a; ( Block until byte received, T = byte/error )
a: #u/mod  1900 a; ( Remainder/Divide: )
a: #/mod   1a00 a; ( Signed Remainder/Divide: )
a: #bye    1b00 a; ( Exit Interpreter )

\ The Stack Delta Operations occur after the ALU operations have been executed.
\ They affect either the Return or the Variable Stack. An ALU instruction
\ without one of these operations (generally) do not affect the stacks.
a: d+1     0001 or a; ( increment variable stack by one )
a: d-1     0003 or a; ( decrement variable stack by one )
a: d-2     0002 or a; ( decrement variable stack by two )
a: r+1     0004 or a; ( increment variable stack by one )
a: r-1     000c or a; ( decrement variable stack by one )
a: r-2     0008 or a; ( decrement variable stack by two )

\ All of these instructions execute after the ALU and stack delta operations
\ have been performed except r->pc, which occurs before. They form part of
\ an ALU operation.
a: r->pc   0010 or a; ( Set Program Counter to Top of Return Stack )
a: n->t    0020 or a; ( Set Top of Variable Stack to Next on Variable Stack )
a: t->r    0040 or a; ( Set Top of Return Stack to Top on Variable Stack )
a: t->n    0080 or a; ( Set Next on Variable Stack to Top on Variable Stack )

\ There are five types of instructions; ALU operations, branches,
\ conditional branches, function calls and literals. ALU instructions
\ comprise of an ALU operation, stack effects and register move bits. Function
\ returns are part of the ALU operation instruction set.

: ?set dup $e000 and abort" argument too large " ;
a: branch  2/ ?set [a] #branch  or t, a; ( a -- : an Unconditional branch )
a: ?branch 2/ ?set [a] #?branch or t, a; ( a -- : Conditional branch )
a: call    2/ ?set [a] #call    or t, a; ( a -- : Function call )
a: ALU        ?set [a] #alu     or    a; ( u -- : Make ALU instruction )
a: alu                    [a] ALU  t, a; ( u -- : ALU operation )
a: literal ( n -- : compile a number into target )
  dup [a] #literal and if   ( numbers above $7fff take up two instructions )
    invert recurse  ( the number is inverted, an literal is called again )
    [a] #~t [a] alu ( then an invert instruction is compiled into the target )
  else
    [a] #literal or t, ( numbers below $8000 are single instructions )
  then a;
a: return ( -- : Compile a return into the target )
   [a] #t [a] r->pc [a] r-1 [a] alu a;

: previous there =cell - ;                      ( -- a )
: lookback previous t@ ;                        ( -- u )
: call? lookback $e000 and [a] #call = ;        ( -- t )
: call>goto previous dup t@ $1fff and swap t! ; ( -- )
: fence? fence @  previous u> ;                 ( -- t )
: safe? lookback $e000 and [a] #alu = lookback $001c and 0= and ; ( -- t )
: alu>return previous dup t@ [a] r->pc [a] r-1 swap t! ; ( -- )
: exit-optimize                                 ( -- )
  fence? if [a] return exit then
  call?  if call>goto  exit then
  safe?  if alu>return exit then
  [a] return ;
: exit, exit-optimize update-fence ;            ( -- )

: compile-only tlast @ t@ $8000 or tlast @ t! ; ( -- )
: immediate tlast @ t@ $4000 or tlast @ t! ;    ( -- )

\ create a word in the metacompilers dictionary, not the targets
: tcreate get-current >r target.1 set-current create r> set-current ;

: thead ( b u -- : compile word header into target dictionary )
  header @ 0= if 2drop exit then
  talign
  there [last] t, tlast ! 
  there #target + pack$ c@ 1+ aligned tcp +! talign ;

: lookahead ( -- b u : parse a word, but leave it in the input stream )
  >in @ >r bl parse r> >in ! ;
 
: literal [a] literal ;                      ( u -- )
: h: ( -- : create a word with no name in the target dictionary )
 ' literal <literal> !
 $f00d tcreate there , update-fence does> @ [a] call ;

: t: ( "name", -- : creates a word in the target dictionary )
  lookahead thead h: ;

\ @warning: Only use 'fallthrough' to fallthrough to words defined with 'h:'.
: fallthrough;
  ' (literal) <literal> !
  $f00d <> if source type cr 1 abort" unstructured! " then ;
: t; 
  fallthrough; optimize if exit, else [a] return then ;

: fetch-xt @ dup 0= abort" (null) " ; ( a -- xt )

: tconstant ( "name", n -- , Run Time: -- n )
  >r
  lookahead
  thead
  there tdoConst fetch-xt [a] call r> t, >r
  tcreate r> ,
  does> @ tbody t@ [a] literal ;

: tvariable ( "name", n -- , Run Time: -- a )
  >r
  lookahead
  thead
  there tdoVar fetch-xt [a] call r> t, >r
  tcreate r> ,
  does> @ tbody [a] literal ;

: tlocation ( "name", n -- : Reserve space in target for a memory location )
  there swap t, tcreate , does> @ [a] literal ;

: [t] ( "name", -- a : get the address of a target word )
  token target.1 search-wordlist 0= abort" [t]? "
  cfa >body @ ;

\ @warning only use "[v]" on variables, not tlocations 
: [v] [t] =cell + ; ( "name", -- a )

: xchange ( "name1", "name2", -- : exchange target vocabularies )
  [last] [t] t! [t] t@ tlast meta! ; 

: begin  there update-fence ;                ( -- a )
: until  [a] ?branch ;                       ( a -- )
: if     there update-fence 0 [a] ?branch  ; ( -- a )
: skip   there update-fence 0 [a] branch ;   ( -- a )
: then   begin 2/ over t@ or swap t! ;       ( a -- )
: else   skip swap then ;                    ( a -- a )
: while  if swap ;                           ( a -- a a )
: repeat [a] branch then update-fence ;      ( a -- )
: again  [a] branch update-fence ;           ( a -- )
: aft    drop skip begin swap ;              ( a -- a )
: constant tcreate , does> @ literal ;       ( "name", a -- )
: [char] char literal ;                      ( "name" )
: postpone [t] [a] call ;                    ( "name", -- )
: next tdoNext fetch-xt [a] call t, update-fence ; ( a -- )
: exit exit, ;                               ( -- )
: ' [t] literal ;                            ( "name", -- )
: ." tdoPrintString fetch-xt [a] call $literal ; ( "string", -- )
: $" tdoStringLit   fetch-xt [a] call $literal ; ( "string", -- )

: nop     ]asm  #t       alu asm[ ;
: dup     ]asm  #t       t->n   d+1   alu asm[ ;
: over    ]asm  #n       t->n   d+1   alu asm[ ;
: invert  ]asm  #~t      alu asm[ ;
: um+     ]asm  #t+n     alu asm[ ;
: +       ]asm  #t+n     n->t   d-1   alu asm[ ;
: um*     ]asm  #t*n     alu asm[    ;
: *       ]asm  #t*n     n->t   d-1   alu asm[ ;
: swap    ]asm  #n       t->n   alu asm[ ;
: nip     ]asm  #t       d-1    alu asm[ ;
: drop    ]asm  #n       d-1    alu asm[ ;
: >r      ]asm  #n       t->r   d-1   r+1   alu asm[ ;
: r>      ]asm  #r       t->n   d+1   r-1   alu asm[ ;
: r@      ]asm  #r       t->n   d+1   alu asm[ ;
: @       ]asm  #[t]     alu asm[ ;
: !       ]asm  #n->[t]  d-1    alu asm[ ;
: rshift  ]asm  #n>>t    d-1    alu asm[ ;
: lshift  ]asm  #n<<t    d-1    alu asm[ ;
: =       ]asm  #t==n    d-1    alu asm[ ;
: u<      ]asm  #nu<t    d-1    alu asm[ ;
: <       ]asm  #n<t     d-1    alu asm[ ;
: and     ]asm  #t&n     d-1    alu asm[ ;
: xor     ]asm  #t^n     d-1    alu asm[ ;
: or      ]asm  #t|n     d-1    alu asm[ ;
: sp@     ]asm  #sp@     t->n   d+1   alu asm[ ;
: sp!     ]asm  #sp!     alu asm[ ;
: 1-      ]asm  #t-1     alu asm[ ;
: rp@     ]asm  #rp@     t->n   d+1   alu asm[ ;
: rp!     ]asm  #rp!     d-1    alu asm[ ;
: 0=      ]asm  #t==0    alu asm[ ;
: (bye)   ]asm  #bye     alu asm[ ;
: rx?     ]asm  #rx      t->n   d+1   alu asm[ ;
: tx!     ]asm  #tx      n->t   d-1   alu asm[ ;
: (save)  ]asm  #save    d-1    alu asm[ ;
: u/mod   ]asm  #u/mod   t->n   alu asm[ ;
\ : u/    ]asm  #u/mod   d-1    alu asm[ ;
\ : umod  ]asm  #u/mod   n->t   d-1   alu asm[ ;
: /mod    ]asm  #/mod    t->n   alu asm[ ;
: /       ]asm  #/mod    d-1    alu asm[ ;
: mod     ]asm  #/mod    n->t   d-1   alu asm[ ;
: rdrop   ]asm  #t       r-1    alu asm[ ;
\ Some words can be implemented in a single instruction which have no
\ analogue within Forth.
: dup-@   ]asm  #[t]     t->n   d+1 alu asm[ ;
: dup>r   ]asm  #t       t->r   r+1 alu asm[ ;
: 2dup=   ]asm  #t==n    t->n   d+1 alu asm[ ;
: 2dupxor ]asm #t^n     t->n   d+1 alu asm[ ;
: rxchg   ]asm  #r       t->r       alu asm[ ;

\ 'for' needs the new definition of '>r' to work correctly.
: for >r begin ;

: meta: : ;
\ : :noname h: ;
: : t: ;
meta: ; t; ;
hide meta:
hide t:
hide t;

]asm #~t              ALU asm[ constant =invert ( invert instruction )
]asm #t  r->pc    r-1 ALU asm[ constant =exit   ( return/exit instruction )
]asm #n  t->r d-1 r+1 ALU asm[ constant =>r     ( to r. stk. instruction )
$20   constant =bl         ( blank, or space )
$d    constant =cr         ( carriage return )
$a    constant =lf         ( line feed )
$8    constant =bs         ( back space )
$1b   constant =escape     ( escape character )

$10   constant dump-width  ( number of columns for 'dump' )
$50   constant tib-length  ( size of terminal input buffer )
$1f   constant word-length ( maximum length of a word )

$40   constant c/l         ( characters per line in a block )
$10   constant l/b         ( lines in a block )
$4400 constant sp0         ( start of variable stack )
$7fff constant rp0         ( start of return stack )
$2bad constant magic       ( magic number for compiler security )
$f    constant #highest    ( highest bit in cell )

( Volatile variables )
$4000 constant <test>      ( used in skip/test )
$4002 constant last-def    ( last, possibly unlinked, word definition )
$4006 constant id          ( used for source id )
$4008 constant seed        ( seed used for the PRNG )
$400A constant handler     ( current handler for throw/catch )
$400C constant block-dirty ( -1 if loaded block buffer is modified )
$4010 constant <key>       ( -- c : new character, blocking input )
$4012 constant <emit>      ( c -- : emit character )
$4014 constant <expect>    ( "accept" vector )
\ $4016 constant <tap>     ( "tap" vector, for terminal handling )
\ $4018 constant <echo>    ( c -- : emit character )
\ $4020 constant <ok>      ( -- : display prompt )
\ $4022 constant _literal  ( u -- u | : handles literals )
$4110 constant context     ( holds current context for search order )
$4122 constant #tib        ( Current count of terminal input buffer )
$4124 constant tib-buf     ( ... and address )
$4126 constant tib-start   ( backup tib-buf value )
\ $4280 == pad-area    

$c    constant header-length  ( location of length in header )
$e    constant header-crc     ( location of CRC in header )
$14   constant header-options ( location of options bits in header )

target.1 +order         ( Add target word dictionary to search order )
meta -order meta +order ( Reorder so 'meta' has a higher priority )
forth-wordlist   -order ( Remove normal Forth words to prevent accidents )

0        t, \  $0: First instruction executed, jump to start / reset vector
0        t, \  $2: Instruction exception vector
$4689    t, \  $4: 0x89 'F'
$4854    t, \  $6: 'T'  'H'
$0a0d    t, \  $8: '\r' '\n'
$0a1a    t, \  $A: ^Z   '\n'
0        t, \  $C: For Length
0        t, \  $E: For CRC
$0001    t, \ $10: Endianess check
#version t, \ $12: Version information
$0001    t, \ $14: Header options

h: doVar   r> ;    ( -- a : push return address and exit to caller )
h: doConst r> @ ;  ( -- u : push value at return address and exit to caller )

[t] doVar tdoVar meta!
[t] doConst tdoConst meta!

0 tlocation cp                ( Dictionary Pointer: Set at end of file )
0 tlocation root-voc          ( root vocabulary )
0 tlocation editor-voc        ( editor vocabulary )
\ 0 tlocation assembler-voc   ( assembler vocabulary )
0 tlocation _forth-wordlist   ( set at the end near the end of the file )
0 tlocation current           ( WID to add definitions to )

\ : nop      nop      ; ( -- : do nothing )
: dup      dup      ; ( n -- n n : duplicate value on top of stack )
: over     over     ; ( n1 n2 -- n1 n2 n1 : duplicate second value on stack )
: invert   invert   ; ( u -- u : bitwise invert of value on top of stack )
: um+      um+      ; ( u u -- u carry : addition with carry )
: +        +        ; ( u u -- u : addition without carry )
: um*      um*      ; ( u u -- ud : multiplication  )
: *        *        ; ( u u -- u : multiplication )
: swap     swap     ; ( n1 n2 -- n2 n1 : swap two values on stack )
: nip      nip      ; ( n1 n2 -- n2 : remove second item on stack )
: drop     drop     ; ( n -- : remove item on stack )
: @        @        ; ( a -- u : load value at address )
: !        !        ; ( u a -- : store 'u' at address 'a' )
: rshift   rshift   ; ( u1 u2 -- u : shift u2 by u1 places to the right )
: lshift   lshift   ; ( u1 u2 -- u : shift u2 by u1 places to the left )
: =        =        ; ( u1 u2 -- t : does u2 equal u1? )
: u<       u<       ; ( u1 u2 -- t : is u2 less than u1 )
: <        <        ; ( u1 u2 -- t : is u2 less than u1, signed version )
: and      and      ; ( u u -- u : bitwise and )
: xor      xor      ; ( u u -- u : bitwise exclusive or )
: or       or       ; ( u u -- u : bitwise or )
\ : sp@    sp@      ; ( ??? -- u : get stack depth )
\ : sp!    sp!      ; ( u -- ??? : set stack depth )
: 1-       1-       ; ( u -- u : decrement top of stack )
: 0=       0=       ; ( u -- t : if top of stack equal to zero )
: (bye)    (bye)    ; ( u -- !!! : exit VM with 'u' as return value )
: rx?      rx?      ; ( -- c | -1 : fetch a single character, or EOF )
: tx!      tx!      ; ( c -- : transmit single character )
: (save)   (save)   ; ( u1 u2 -- u : save memory from u1 to u2 inclusive )
: u/mod    u/mod    ; ( u1 u2 -- rem div : unsigned divide/modulo )
: /mod     /mod     ; ( u1 u2 -- rem div : signed divide/modulo )
: /        /        ; ( u1 u2 -- u : u1 divided by u2 )
: mod      mod      ; ( u1 u2 -- u : remainder of u1 divided by u2 )


\       : m* 2dup xor 0< >r abs swap abs um* r> if dnegate then ; ( n n -- d )
\       : */mod  >r m* r> m/mod ;    ( n n n -- r q )
\       : */  */mod nip ;            ( n n n -- q )

 
there constant inline-start 
\ : rp@ rp@   fallthrough; compile-only ( -- u )
\ : rp! rp!   fallthrough; compile-only ( u --, R: --- ??? )
: exit  exit  fallthrough; compile-only ( -- )
: >r    >r    fallthrough; compile-only ( u --, R: -- u )
: r>    r>    fallthrough; compile-only ( -- u, R: u -- )
: r@    r@    fallthrough; compile-only ( -- u )
: rdrop rdrop fallthrough; compile-only ( --, R: u -- )
there constant inline-end 

\ [last] [t] assembler-voc t!

$2       tconstant cell  ( size of a cell in bytes )
$0       tvariable >in   ( Hold character pointer when parsing input )
$0       tvariable state ( compiler state variable )
$0       tvariable hld   ( Pointer into hold area for numeric output )
$10      tvariable base  ( Current output radix )
$0       tvariable span  ( Hold character count received by expect   )
$8       tconstant #vocs ( number of vocabularies in allowed )
$400     tconstant b/buf ( size of a block )
0        tvariable blk   ( current blk loaded, set in 'cold' )
#version constant  ver   ( eForth version )
pad-area tconstant pad   ( pad variable - offset into temporary storage )
0        tvariable <literal> ( holds execution vector for literal )
0        tvariable <boot>  ( -- : execute program at startup )
0        tvariable <ok>
 
h: [-1] -1 ;                 ( -- -1 : space saving measure, push -1 )
h: 0x8000 $8000 ;            ( -- $8000 : space saving measure, push $8000 )
h: 2drop-0 drop fallthrough; ( n n -- 0 )
h: drop-0 drop fallthrough;  ( n -- 0 )
h: 0x0000 $0000 ;            ( -- $0000 : space/optimization, push $0000 )
h: state@ state @ ;          ( -- u )
h: first-bit 1 and ;         ( u -- u )
h: in! >in ! ;               ( u -- )
h: in@ >in @ ;               ( -- u )

: 2drop drop drop ;         ( n n -- )
: 1+ 1 + ;                  ( n -- n : increment a value  )
: negate invert 1+ ;        ( n -- n : negate a number )
: - negate + ;              ( n1 n2 -- n : subtract n1 from n2 )
h: over- over - ;           ( u u -- u u )
h: over+ over + ;           ( u1 u2 -- u1 u1+2 )
: aligned dup first-bit + ; ( b -- a )
: bye 0 (bye) ;             ( -- : leave the interpreter )
h: cell- cell - ;           ( a -- a : adjust address to previous cell )
: cell+ cell + ;            ( a -- a : move address forward to next cell )
: cells 1 lshift ;          ( n -- n : convert cells count to address count )
: chars 1 rshift ;          ( n -- n : convert bytes to number of cells )
: ?dup dup if dup exit then ; ( n -- 0 | n n : duplicate non zero value )
: >  swap < ;               ( n1 n2 -- t : signed greater than, n1 > n2 )
: u> swap u< ;              ( u1 u2 -- t : unsigned greater than, u1 > u2 )
h: u>= u< invert ;          ( u1 u2 -- t : unsigned greater/equal )
: <> = invert ;             ( n n -- t : not equal )
: 0<> 0= invert ;           ( n n -- t : not equal  to zero )
: 0> 0 > ;                  ( n -- t : greater than zero? )
: 0< 0 < ;                  ( n -- t : less than zero? )
: 2dup over over ;          ( n1 n2 -- n1 n2 n1 n2 )
: tuck swap over ;          ( n1 n2 -- n2 n1 n2 )
: +! tuck @ +  fallthrough; ( n a -- : increment value at 'a' by 'n' )
h: swap! swap ! ;           ( a u -- )
: 1+!  1 swap +! ;          ( a -- : increment value at address by 1 )
: 1-! [-1] swap +! ;        ( a -- : decrement value at address by 1 )
: 2! ( d a -- ) tuck ! cell+ ! ;      ( n n a -- )
: 2@ ( a -- d ) dup cell+ @ swap @ ;  ( a -- n n )
: get-current current @ ;             ( -- wid )
: set-current current ! ;             ( wid -- )
: bl =bl ;                            ( -- c )
: within over- >r - r> u< ;           ( u lo hi -- t )
: abs dup 0< if negate exit then ;    ( n -- u )
: tib #tib cell+ @ ;                  ( -- a )
: source #tib 2@ ;                    ( -- a u )
: source-id id @ ;                    ( -- 0 | -1 )
: d0= 0= swap 0= and ;                ( d -- t )
: dnegate invert >r invert 1 um+ r> + ; ( d -- d )
\ : even first-bit 0= ;               ( u -- t )
\ : odd even 0= ;                     ( u -- t )

: execute >r ;                   ( cfa -- : execute a function )
h: @execute @ ?dup if >r then ;  ( cfa -- )

: c@ dup-@ swap first-bit ( b -- c : load character from address  )
   if
      8 rshift exit
   then
   $ff and ;                   
: c!                      ( c b -- : store character at address )
  swap $ff and dup 8 lshift or swap
  swap over dup @ swap first-bit 0= $ff xor
  >r over xor r> and xor swap ! ; 
  >
h: command? state@ 0= ;               ( -- t )

: here cp @ ;                         ( -- a )
: align here fallthrough;             ( -- )
h: cp! aligned cp ! ;                 ( n -- )

: allot cp +! ;                        ( n -- )

: rot >r swap r> swap ;               ( n1 n2 n3 -- n2 n3 n1 )
: -rot swap >r swap r> ;              ( n1 n2 n3 -- n3 n1 n2 )

h: 2>r rxchg swap >r >r ;              ( u1 u2 --, R: -- u1 u2 )
h: 2r> r> r> swap rxchg nop ;          ( -- u1 u2, R: u1 u2 -- )

h: doNext 2r> ?dup if 1- >r @ >r exit then cell+ >r ;
[t] doNext tdoNext meta!

: min 2dup < fallthrough;             ( n n -- n )
h: mux if drop exit then nip ;        ( n1 n2 b -- n : multiplex operation )
: max 2dup > mux ;                    ( n n -- n )


: key <key> @execute dup [-1] = if bye 0x0000 exit then ; ( -- c )

: /string over min rot over+ -rot - ;  ( b u1 u2 -- b u : advance string u2 )
h: +string 1 /string ;                 ( b u -- b u : )
: count dup 1+ swap c@ ;               ( b -- b u )
h: string@ over c@ ;                   ( b u -- b u c )

h: ccitt ( crc c -- crc : crc polynomial $1021 AKA "x16 + x12 + x5 + 1" )
  over $8 rshift xor   ( crc x )
  dup  $4 rshift xor   ( crc x )
  dup  $5 lshift xor   ( crc x )
  dup  $c lshift xor   ( crc x )
  swap $8 lshift xor ; ( crc )

: crc ( b u -- u : calculate ccitt-ffff CRC )
  $ffff >r
  begin
    dup
  while
   string@ r> swap ccitt >r 1 /string
  repeat 2drop r> ;



\ : random ( -- u : pseudo random number )
\  seed @ 0= seed toggle seed @ 0 ccitt dup seed ! ; 

h: @address @ fallthrough;             ( a -- a )
h: address $3fff and ;                 ( a -- a : mask off address bits )

h: last get-current @address ;         ( -- pwd )

\ h: echo <echo> @execute ;            ( c -- )
: emit <emit> @execute ;               ( c -- : write out a char )
: cr =cr emit =lf emit ;               ( -- : emit a newline )
h: colon [char] : emit ;               ( -- )
: space =bl emit ;                     ( -- : emit a space )
h: spaces =bl fallthrough;             ( +n -- )
h: nchars                              ( +n c -- : emit c n times )
  swap 0 max for aft dup emit then next drop ;

\   : pick ?dup if swap >r 1- pick r> swap exit then dup ; 
\ 

: depth sp@ sp0 - chars ;             ( -- u : get current depth )
: pick cells sp@ swap - @ ;           ( vn...v0 u -- vn...v0 vu )

h: >char $7f and dup $7f =bl within if drop [char] _ then ; ( c -- c )
: type 0 fallthrough;                  ( b u -- )
h: typist                              ( b u f -- : print a string )
  >r begin dup while
    swap count r@
    if
      >char
    then
    emit
    swap 1-
  repeat
  rdrop 2drop ;
h: print count type ;                    ( b -- )
h: $type [-1] typist ;                   ( b u --  )

: cmove for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ; ( b b u -- )
: fill swap for swap aft 2dup c! 1+ then next 2drop ; ( b u c -- )

h: ndrop for aft drop then next ; ( 0u....nu n -- : drop n cells )

: catch ( i*x xt -- j*x 0 | i*x n )
  sp@ >r
  handler @ >r
  rp@ handler !
  execute
  r> handler !
  r> drop-0 ;

: throw ( k*x n -- k*x | i*x n )
  ?dup if
    handler @ rp!
    r> handler !
    rxchg ( 'rxchg' is equivalent to 'r> swap >r' )
    sp! drop r>
  then ;

h: -throw negate throw ;  ( u -- : negate and throw )
[t] -throw 2/ 1 tcells t! 

h: 1depth 1 fallthrough; ( ??? -- : check depth is at least one )
h: ?ndepth depth 1- u> if 4 -throw exit then ; ( ??? n -- check depth )
h: 2depth 2 ?ndepth ;    ( ??? -- :  check depth is at least two )

\ @todo implement a more efficient version of 'um/mod' using built in division
: um/mod ( ud u -- r q )
  ?dup 0= if $a -throw exit then
  2dup u<
  if negate #highest
    for >r dup um+ >r >r dup um+ r> + dup
      r> r@ swap >r um+ r> or
      if >r drop 1+ r> else drop then r>
    next
    drop swap exit
  then drop 2drop [-1] dup ;

: decimal  $a base ! ;                      ( -- )
: hex     $10 base ! ;                      ( -- )
h: radix base @ dup 2 - $22 u> if hex $28 -throw exit then ; ( -- u )

\ @todo Check '?hold' works correctly
: hold  hld @ 1- dup hld ! c! fallthrough;              ( c -- )
h: ?hold hld @ pad $100 + u> if $11 -throw exit then ;  ( -- )
h: extract dup >r um/mod r> swap >r um/mod r> rot ;     ( ud ud -- ud u )
h: digit  9 over < 7 and + [char] 0 + ;                 ( u -- c )

: #> 2drop hld @ pad over - ;                       ( w -- b u )
: # 2depth 0 base @ extract digit hold ;            ( d -- d )
: #s begin # 2dup d0= until ;                       ( d -- 0 )
: <# pad hld ! ;                                    ( -- )

: sign  0< if [char] - hold exit then ;     ( n -- )
h: str ( n -- b u : convert a signed integer to a numeric string )
  dup>r abs 0 <# #s r> sign #> ;

h: (u.) 0 <# #s #> ;             ( u -- b u : turn 'u' into number string )
: u.r >r (u.) r> fallthrough;    ( u +n -- : print u right justified by +n)
h: adjust over- spaces type ;    ( b n n -- )
h: 5u.r 5 u.r ;                  ( u -- )
\ :  .r >r str r> adjust ;       ( n n -- : print n, right justified by +n )
: u.  (u.) space type ;          ( u -- : print unsigned number )
:  .  radix $a xor if u. exit then str space type ; ( n -- print number )
\ : d. base @ >r decimal  . r> base ! ;
\ : h. base @ >r hex     u. r> base ! ;

\ 'holds' and '.base' can be defined as so:
\ 
\ : holds begin dup while 1- 2dup + c@ hold repeat 2drop ;
\ : .base base @ dup decimal base ! ; ( -- )
\ 

h: unused $4000 here - ;         ( -- u : unused program space )
h: .free unused u. ;             ( -- : print unused program space )

: pack$ ( b u a -- a ) \ null fill
  aligned dup>r over
  dup cell negate and ( align down )
  - over+ 0 swap! 2dup c! 1+ swap cmove r> ;

: =string ( a1 u2 a1 u2 -- t : string equality )
  >r swap r> ( a1 a2 u1 u2 )
  over xor if drop 2drop-0 exit then
  for ( a1 a2 )
    aft
      count >r swap count r> xor
      if rdrop 2drop-0 exit then
    then
  next 2drop [-1] ;

h: tap ( dup echo ) over c! 1+ ; ( bot eot cur c -- bot eot cur )

: accept ( b u -- b u )
  over+ over
  begin
    2dupxor
  while
    key dup =lf xor if tap else drop nip dup then
    \ The alternative 'accept' code replaces the line above:
    \  
    \   key  dup =bl - 95 u< if tap else <tap> @execute then
    \ 
  repeat drop over- ;

: expect <expect> @execute span ! drop ;                     ( b u -- )
: query tib tib-length <expect> @execute #tib ! drop-0 in! ; ( -- )

: nfa address cell+ ; ( pwd -- nfa : move to name field address)
: cfa nfa dup c@ + cell+ $fffe and ;        ( pwd -- cfa )

\ '.id' prints out a words name field.

h: .id nfa print ;                          ( pwd -- : print out a word )

\ 'immediate?', 'compile-only?' and 'inline?' are the tests words that
\ all take a pointer to the 'PWD' field.

h: immediate? @ $4000 and fallthrough;      ( pwd -- t : immediate word? )
h: logical 0= 0= ;                          ( n -- t )
h: compile-only? @ 0x8000 and logical ;     ( pwd -- t : is compile only? )
h: inline? inline-start inline-end within ; ( pwd -- t : is word inline? )

h: searcher ( a wid -- pwd pwd 1 | pwd pwd -1 | 0 a 0 : find a word in a WID )
  swap >r dup
  begin
    dup
  while
    dup nfa count r@ count =string
    if ( found! )
      dup immediate? if 1 else [-1] then
      rdrop exit
    then
    nip dup @address
  repeat
  rdrop 2drop-0 ;

h: finder ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 : find a word dictionary )
  >r
  context
  begin
    dup-@
  while
    dup-@ @ r@ swap searcher ?dup
    if
      >r rot drop r> rdrop exit
    then
    cell+
  repeat drop-0 r> 0x0000 ;

: search-wordlist searcher rot drop ; ( a wid -- pwd 1 | pwd -1 | a 0 )
: find ( a -- pwd 1 | pwd -1 | a 0 : find a word in the dictionary )
  finder rot drop ;

h: decimal?   [char] 0 [char] : within ; ( c -- t : decimal char? )
h: lowercase? [char] a [char] { within ; ( c -- t )
h: uppercase? [char] A [char] [ within ; ( c -- t )
h: >lower                                ( c -- c : convert to lower case )
  dup uppercase? if =bl xor exit then ;

h: numeric? ( char -- n|-1 : convert character in 0-9 a-z range to number )
  >lower
  dup lowercase? if $57 - exit then ( 97 = 'a', +10 as 'a' == 10 )
  dup decimal?   if [char] 0 - exit then 
  drop [-1] ;

h: digit? >lower numeric? base @ u< ; ( c -- t : is char a digit given base )
 

\ @todo Fix (number) to work with double cell numbers
h: (number) ( n b u -- n b u : convert string to number )
  begin
    ( get next character )
    2dup 2>r drop c@ dup digit? ( n char bool, Rt: b u )
    if   ( n char )
      swap base @ * swap numeric? + ( accumulate number )
    else ( n char )
      drop
      2r> ( restore string )
      nop exit
    then
    2r> ( restore string )
    +string dup 0= ( advance string and test for end )
  until ;

h: negative? ( b u -- t : is >number negative? )
  string@ [char] - = if +string [-1] exit then 0x0000 ; 

h: base? ( b u -- )
  string@ [char] $ = ( $hex )
  if
    +string hex exit
  then ( #decimal )
  string@ [char] # = if +string decimal exit then ;

\ @todo '>number' should accept a double cell number and return one

\ : digit? ( c base -- u f ) 
\  >r [char] 0 - 9 over <
\  if 7 - dup 10 < or then dup r> u< ;

\ : >number ( ud a u -- ud a u ) 
\  begin dup
\  while >r  dup >r c@ base @ digit?
\  while swap base @ um* drop rot base @ um* d+ r> char+ r> 1 -
\  repeat drop r> r> then ;

: >number ( n b u -- n b u : convert string )
  radix >r
  negative? >r
  base?
  (number)
  r> if rot negate -rot then
  r> base ! ;

h: number? 0 -rot >number nip 0= ; ( b u -- n f : is number? )

h: -trailing ( b u -- b u : remove trailing spaces )
  for
    aft =bl over r@ + c@ <
      if r> 1+ exit then
    then
  next 0x0000 ;

h: lookfor ( b u c -- b u : skip until <test> succeeds )
  >r
  begin
    dup
  while
    string@ r@ - r@ =bl = <test> @execute if rdrop exit then
    +string
  repeat rdrop ;

h: skipTest if 0> exit then 0<> ; ( n f -- t )
h: scanTest skipTest invert ; ( n f -- t )
h: skipper ' skipTest <test> ! lookfor ; ( b u c -- u c )
h: scanner ' scanTest <test> ! lookfor ; ( b u c -- u c )

h: parser ( b u c -- b u delta )
  >r over r> swap 2>r 
  r@ skipper 2dup
  r> scanner swap r> - >r - r> 1+ ;

: parse ( c -- b u ; <string> )
   >r tib in@ + #tib @ in@ - r> parser >in +! -trailing 0 max ;
: ) ; immediate ( -- : do nothing )
:  ( [char] ) parse 2drop ; immediate \ ) ( parse until matching paren )
: .( [char] ) parse type ; ( print out text until matching parenthesis )
: \ #tib @ in! ; immediate ( comment until new line )
h: ?length dup word-length u> if $13 -throw exit then ;
: word 1depth parse ?length here pack$ ; ( c -- a ; <string> )
: token =bl word ;                       ( -- a )
: char token count drop c@ ;             ( -- c; <string> )

\ ## The Interpreter

h: ?dictionary dup $3f00 u> if 8 -throw exit then ;
: , here dup cell+ ?dictionary cp! ! ; ( u -- : store 'u' in dictionary )
: c, here ?dictionary c! cp 1+! ;      ( c -- : store 'c' in the dictionary )
h: doLit 0x8000 or , ;                 ( n+ -- : compile literal )
: literal ( n -- : write a literal into the dictionary )
  dup 0x8000 and ( n > $7fff ? )
  if
    invert doLit =invert , exit ( store inversion of n the invert it )
  then
  doLit ( turn into literal, write into dictionary )
  ; compile-only immediate

h: make-callable chars $4000 or ; ( cfa -- instruction )
: compile, make-callable , ; ( cfa -- : compile a code field address )
h: $compile dup inline? if cfa @ , exit then cfa compile, ; ( pwd -- )
h: not-found source type $d -throw ; ( -- : throw 'word not found' )

h: ?compile dup compile-only? if source type $e -throw exit then ;
: (literal) state@ if postpone literal exit then ; ( u -- u | )
: interpret ( ??? a -- ??? : The command/compiler loop )
  find ?dup if
    state@
    if
      0> if cfa execute exit then ( <- immediate word )
      $compile exit               ( <- compiling word )
    then
    drop ?compile cfa execute exit
  then 
  \ not a word
  dup count number? if
    nip <literal> @execute exit
  then
  not-found ;

\ NB. 'compile' only works for words, instructions, and numbers below $8000
: compile  r> dup-@ , cell+ >r ; compile-only ( --:Compile next compiled word )
: immediate $4000 last fallthrough; ( -- : previous word immediate )
h: toggle tuck @ xor swap! ;        ( u a -- : xor value at addr with u )
\ : compile-only $8000 last toggle ;

: smudge last fallthrough;
h: (smudge) nfa $80 swap toggle ; ( pwd -- )

h: do$ r> r@ r> count + aligned >r swap >r ; ( -- a )
h: string-literal do$ nop ; ( -- a : do string NB. nop to fool optimizer )
h: .string do$ print ; ( -- : print string  )

[t] .string        tdoPrintString meta!
[t] string-literal tdoStringLit   meta!

h: parse-string [char] " word count + cp! ;              ( -- )
( <string>, --, Run: -- b )
: $"  compile string-literal parse-string ; immediate compile-only 
: ."  compile .string parse-string ; immediate compile-only ( <string>, -- )
: abort [-1] (bye) ;                                           ( -- )
h: ?abort swap if print cr abort else drop then ;              ( u a -- )
h: (abort) do$ ?abort ;                                        ( -- )
: abort" compile (abort) parse-string ; immediate compile-only ( u -- ) 

h: preset tib-start #tib cell+ ! 0 in! 0 id ! ;
: ] [-1] state ! ;
: [   0  state ! ; immediate

h: ?error ( n -- : perform actions on error )
  ?dup if
    .             ( print error number )
    [char] ? emit ( print '?' )
    cr
    sp0 sp!       ( empty stack )
    preset        ( reset I/O streams )
    [             ( back into interpret mode )
    exit
  then ;

h: (ok) command? if ."  ok  " cr exit then ;  ( -- )
h: ?depth sp@ sp0 u< if 4 -throw exit then ;  ( u -- : depth check )
h: eval ( -- )
  begin
    token dup c@ 
  while 
    interpret ?depth 
  repeat drop <ok> @execute ; 

: quit preset [ begin query ' eval catch ?error again ; ( -- )

h: get-input source in@ id @ <ok> @ ;    ( -- n1...n5 )
h: set-input <ok> ! id ! in! #tib 2! ;   ( n1...n5 -- )
: evaluate ( a u -- )
  get-input 2>r 2>r >r
  0 [-1] 0 set-input
  ' eval catch
  r> 2r> 2r> set-input
  throw ;

h: io! preset fallthrough;  ( -- : initialize I/O )
h: console ' rx? <key> ! ' tx! <emit> ! fallthrough;
h: hand ' (ok)  ( ' drop <-- was emit )  ( ' ktap ) fallthrough;
h: xio  ' accept <expect> ! ( <tap> ! ) ( <echo> ! ) <ok> ! ;
\ h: pace 11 emit ;
\ t: file ' pace ' drop ' ktap xio ;

h: ?check ( magic-number -- : check for magic number on the stack )
   magic <> if $16 -throw exit then ;
h: ?unique ( a -- a : print a message if a word definition is not unique )
  dup last @ searcher
  if
    ( source type )
    space
    2drop last-def @ nfa print  ."  redefined " cr exit
  then ;
h: ?nul ( b -- : check for zero length strings )
   count 0= if $a -throw exit then 1- ;

h: find-token token find 0= if not-found exit then ; ( -- pwd,  <string> )
h: find-cfa find-token cfa ;                         ( -- xt, <string> )
: ' find-cfa state@ if postpone literal exit then ; immediate
: [compile] find-cfa compile, ; immediate compile-only  ( --, <string> )
: [char] char postpone literal ; immediate compile-only ( --, <string> : )
\ h: ?quit command? if $38 -throw exit then ;
: ; ( ?quit ) ?check =exit , [ fallthrough; immediate compile-only
h: get-current! ?dup if get-current ! exit then ; ( -- wid )
: : align here dup last-def ! ( "name", -- colon-sys )
    last , token ?nul ?unique count + cp! magic ] ;
: begin here  ; immediate compile-only      ( -- a )
: until chars $2000 or , ; immediate compile-only  ( a -- )
: again chars , ; immediate compile-only ( a -- )
h: here-0 here 0x0000 ;
h: >mark here-0 postpone again ; 
: if here-0 postpone until ; immediate compile-only
: then fallthrough; immediate compile-only
h: >resolve here chars over @ or swap! ;
: else >mark swap >resolve ; immediate compile-only
: while postpone if ; immediate compile-only
: repeat swap postpone again postpone then ; immediate compile-only
h: last-cfa last-def @ cfa ;  ( -- u )
: recurse last-cfa compile, ; immediate compile-only
: tail last-cfa postpone again ; immediate compile-only
: create postpone : drop compile doVar get-current ! [ ;
: >body cell+ ; ( a -- a )
h: doDoes r> chars here chars last-cfa dup cell+ doLit ! , ;
: does> compile doDoes nop ; immediate compile-only
: variable create 0 , ;
: constant create ' doConst make-callable here cell- ! , ;
: :noname here-0 magic ]  ;
: for =>r , here ; immediate compile-only
: next compile doNext , ; immediate compile-only
: aft drop >mark postpone begin swap ; immediate compile-only
\ : doer create =exit last-cfa ! =exit ,  ;
\ : make ( "name1", "name2", -- : make name1 do name2 )
\  find-cfa find-cfa make-callable
\  state@
\  if
\    postpone literal postpone literal compile ! nop exit
\  then
\  swap! ; immediate

\ @todo Use a 'SMUDGE' system to hide the current word from the search order
\ @todo improve 'hide' so it unlinks a word from the linked list
: hide find-token (smudge) ; ( --, <string> : hide word by name )

h: find-empty-cell 0 fallthrough; ( a -- )
h: find-cell >r begin dup-@ r@ <> while cell+ repeat rdrop ; ( u a -- a )

: get-order ( -- widn ... wid1 n : get the current search order )
  context
  find-empty-cell
  dup cell- swap
  context - chars dup>r 1- dup 0< if $32 -throw exit then
  for aft dup-@ swap cell- then next @ r> ;

xchange _forth-wordlist root-voc
: forth-wordlist _forth-wordlist ;

: set-order ( widn ... wid1 n -- : set the current search order )
  dup [-1] = if drop root-voc 1 set-order exit then
  dup #vocs > if $31 -throw exit then
  context swap for aft tuck ! cell+ then next 0 swap! ;

: forth root-voc forth-wordlist 2 set-order ; ( -- )

\ The name fields length in a counted string is used to store a bit 
\ indicating the word is hidden. This is the highest bit in the count byte.
h: not-hidden? nfa c@ $80 and 0= ; ( pwd -- )
h: .words space 
    begin 
      dup 
    while dup not-hidden? if dup .id space then @address repeat drop cr ;
: words
  get-order begin ?dup while swap dup cr u. colon @ .words 1- repeat ;

xchange root-voc _forth-wordlist

\ : previous get-order swap drop 1- set-order ; ( -- )
\ : also get-order over swap 1+ set-order ;     ( wid -- )
: only [-1] set-order ;                         ( -- )
\ : order get-order for aft . then next cr ;    ( -- )
\ : anonymous get-order 1+ here 1 cells allot swap set-order ; ( -- )
: definitions context @ set-current ;           ( -- )
h: (order)                                      ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r (order) over r@ xor
    if
      1+ r> -rot exit
    then rdrop
  then ;
: -order get-order (order) nip set-order ;             ( wid -- )
: +order dup>r -order get-order r> swap 1+ set-order ; ( wid -- )

: editor decimal editor-voc +order ;                   ( -- )

: update [-1] block-dirty ! ; ( -- )
h: blk-@ blk @ ;              ( -- k : retrieve current loaded block )
h: +block blk-@ + ;           ( -- )
: save 0 here (save) throw ;  ( -- : save blocks )
: flush block-dirty @ if 0 [-1] (save) throw exit then ; ( -- )

: block ( k -- a )
  1depth
  dup $3f u> if $23 -throw exit then
  dup blk !
  $a lshift ( <-- b/buf * ) ;

h: c/l* ( c/l * ) 6 lshift ;            ( u -- u )
h: c/l/ ( c/l / ) 6 rshift ;            ( u -- u )
h: line swap block swap c/l* + c/l ;    ( k u -- a u )
h: loadline line evaluate ;             ( k u -- )
: load 0 l/b 1- for 2dup 2>r loadline 2r> 1+ next 2drop ; ( k -- )
h: pipe $7c emit ;                      ( -- )
\ h: .line line -trailing $type ;       ( k u -- )
h: .border 3 spaces c/l $2d nchars cr ; ( -- )
h: #line dup 2 u.r ;                    ( u -- u : print line number )
\ : thru over- for dup load 1+ next drop ; ( k1 k2 -- )
h: blank =bl fill ;                     ( b u -- )
\ : message l/b extract .line cr ;      ( u -- )
h: retrieve block drop ;                ( k -- )
: list                                  ( k -- )
  dup retrieve
  cr
  .border
  0 begin
    dup l/b <
  while
    2dup #line pipe line $type pipe cr 1+
  repeat .border 2drop ; 

\ t: index ( k1 k2 -- : show titles for block k1 to k2 )
\  over- cr
\  for
\    dup 5u.r space pipe space dup 0 .line cr 1+
\  next drop ;

h: check-header? header-options @ first-bit 0= ; ( -- t )
h: disable-check $1 header-options toggle ;      ( -- )

\ 'bist' checks the length field in the header matches 'here' and that the
\ CRC in the header matches the CRC it calculates in the image, it has to
\ zero the CRC field out first.

h: bist ( -- u : built in self test )
  check-header? if 0x0000 exit then       ( is checking disabled? Success? )
  header-length @ here xor if 2 exit then ( length check )
  header-crc @ 0 header-crc !             ( retrieve and zero CRC )
  0 here crc xor if 3 exit then           ( check CRC )
  disable-check 0x0000 ;                  ( disable check, success )

: cold ( -- : performs a cold boot  )
   bist ?dup if negate (bye) exit then
   $10 block b/buf 0 fill
   $12 retrieve io! 
   forth sp0 sp! 
   <boot> @execute bye ;

\ 'hi' prints out the welcome message, 

h: hi hex cr ." eFORTH V " ver 0 u.r cr here . .free cr [ ; ( -- )
h: normal-running hi quit ;                 ( -- : boot word )

h: validate tuck cfa <> if drop-0 exit then nfa ; ( cfa pwd -- nfa | 0 )

h: search-for-cfa ( wid cfa -- nfa : search for CFA in a word list )
  address cells >r
  begin
    dup
  while
    address dup r@ swap dup-@ address swap within ( simplify? )
    ( @bug does not continue with search if validate fails )
    if @address r> swap validate exit then 
    address @
  repeat rdrop ;

h: name ( cwf -- a | 0 )
   >r
   get-order 
   begin 
     dup 
   while 
     swap r@ search-for-cfa ?dup if >r 1- ndrop r> rdrop exit then 
   1- repeat rdrop ;

h: .name name ?dup 0= if $" ???" then print ;
h: ?instruction ( i m e -- i 0 | e -1 )
   >r over and r> tuck = if nip [-1] exit then drop-0 ;

h: .instruction ( u -- u )
   0x8000  0x8000 ?instruction if ." LIT " exit then
   $6000   $6000  ?instruction if ." ALU " exit then
   $6000   $4000  ?instruction if ." CAL " exit then
   $6000   $2000  ?instruction if ." BRZ " exit then
   drop-0 ." BRN " ;

: decompile ( u -- : decompile instruction )
   dup .instruction $4000 =
   if space .name exit then drop ;

h: decompiler ( previous current -- : decompile starting at address )
  >r
   begin dup r@ u< while
     dup 5u.r colon space
     dup-@
     dup 5u.r space decompile cr cell+
   repeat rdrop drop ;

: see ( --, <string> : decompile a word )
  token finder 0= if not-found exit then
  swap      2dup= if drop here then >r
  cr colon space dup .id space dup cr
  cfa r> decompiler space $3b emit
  dup compile-only? if ."  compile-only  " then 
  dup inline?       if ."  inline  "       then
      immediate?    if ."  immediate  "    then cr ;

: .s cr depth for aft r@ pick . then next ."  <sp " ;          ( -- )
h: dm+ chars for aft dup-@ space 5u.r cell+ then next ;        ( a u -- a )
\ h: dc+ chars for aft dup-@ space decompile cell+ then next ; ( a u -- a )

: dump ( a u -- )
  $10 + \ align up by dump-width
  4 rshift ( <-- equivalent to "dump-width /" )
  for
    aft
      cr dump-width 2dup
      over 5u.r colon space
      dm+ ( dump-width dc+ ) \ <-- dc+ is optional
      -rot
      2 spaces $type
    then
  next drop ;

[last]              [t] _forth-wordlist t!
[t] _forth-wordlist [t] current         t!

0 tlast meta!
h: [block] blk-@ block ;       ( k -- a : loaded block address )
h: [check] dup b/buf c/l/ u>= if $18 -throw exit then ;
h: [line] [check] c/l* [block] + ; ( u -- a )
: b retrieve ;                 ( k -- : load a block )
: l blk-@ list ;               ( -- : list current block )
: n  1 +block b l ;            ( -- : load and list next block )
: p [-1] +block b l ;          ( -- : load and list previous block )
: d [line] c/l blank ;         ( u -- : delete line )
: x [block] b/buf blank ;      ( -- : erase loaded block )
: s update flush ;             ( -- : flush changes to disk )
: q editor-voc -order ;        ( -- : quit editor )
: e q blk-@ load editor ;      ( -- : evaluate block )
: ia c/l* + [block] + source drop in@ + ( u u -- )
   swap source nip in@ - cmove postpone \ ;
: i 0 swap ia ;                ( u -- )
\ : u update ;                 ( -- : set block set as dirty )
\ : w words ;
\ : yank pad c/l ; 
\ : c [line] yank >r swap r> cmove ;
\ : y [line] yank cmove ;
\ : ct swap y c ;
\ : ea [line] c/l evaluate ;
\ : sw 2dup y [line] swap [line] swap c/l cmove c ;
[last] [t] editor-voc t! 0 tlast meta!

\ ## Final Touches

there           [t] cp t!
[t] (literal) [v] <literal> t! ( set literal execution vector )
[t] cold 2/ 0 t! ( set starting word )
[t] normal-running [v] <boot> t!

there    6 tcells t! \ Set Length First!
checksum 7 tcells t! \ Calculate image CRC

finished
bye

