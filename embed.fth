system +order 0 <ok> !
\ TODO
\ - Add documentation for this file
\   - Include how to integrate this project into another one
\ - Add references back in, along with documentation.
\   - J1, eForth documents, Novix Chip
\ - Multi-tasking would be nice, www.ultratechnology.com/p21fchp7.html.
\ - Implement bit-banging version of UART, reliant only from an interrupt
\ from a timer, and the UART Pins. Debouncing and the FIFO for both TX and
\ RX should be done in software.
\ - Multi-tasking would require implementing USER variables
\ - Combine return stack effecting words with the previous instruction if
\ possible: "1- dup >r" can be combined for example.

.( FORTH META COMPILATION START ) cr

\ ============================================================================
\ # The Meta-Compiler
\ 
only forth definitions hex
system +order 0 <ok> !
variable meta          ( Metacompilation vocabulary )
meta +order definitions
variable assembler.1         ( Target assembler vocabulary )
variable target.1            ( Target dictionary )
variable tcp                 ( Target dictionary pointer )
variable tlast               ( Last defined word in target )
variable tdoVar              ( Location of doVar in target )
variable tdoConst            ( Location of doConst in target )
variable tdoNext             ( Location of doNext in target )
variable tdoPrintString      ( Location of .string in target )
variable tdoStringLit        ( Location of string-literal in target )
variable fence               ( Do not peephole optimize before this point )
5000 constant #target        ( Location where target image will be built )
2    constant =cell          ( Target cell size )
$3A00 constant pad-area      ( area for pad storage )
( 1   constant verbose ( verbosity level, higher is more verbose )
#target 2000 0 fill    ( Erase the target memory location )
: ]asm assembler.1 +order ; immediate    ( -- )
: a: current @ assembler.1 current ! : ; ( "name" -- wid link )
: a; [compile] ; current ! ; immediate   ( wid link -- )
: ( [char] ) parse 2drop ; immediate ( "comment" -- discard until parenthesis )
: \ source drop @ >in ! ; immediate  ( "comment" -- discard until end of line )
: there tcp @ ;                      ( -- a : target dictionary pointer value )
: tc! #target + c! ;                 ( u a -- : store character in target )
: tc@ #target + c@ ;                 ( a -- u : retrieve character in target )
: [last] tlast @ ;                   ( -- a : last defined word in target )
: t! over $FF and over tc! swap 8 rshift swap 1+ tc! ; ( u a -- )
: t@ dup tc@ swap 1+ tc@ 8 lshift or ; ( a -- u )
: 2/ 1 rshift ;                ( u -- u : non-standard definition divide by 2 )
\ : 2* 1 lshift ;                ( u -- u : multiple by two, non-standard )
: talign there 1 and tcp +! ;  ( -- : align target dictionary pointer value )
: tc, there tc! 1 tcp +! ;     ( c -- : write byte into target dictionary )
: t,  there t!  =cell tcp +! ; ( u -- : write cell into target dictionary )
\ : tallot tcp +! ;            ( n -- : allocate memory in target dictionary )
: update-fence there fence ! ; ( -- : update optimizer fence location )
: $literal                     ( <string>, -- )
  [char] " word count dup tc, 1- for count tc, next drop talign update-fence ;
: tcells =cell * ;             ( u -- a )
: tbody 1 tcells + ;           ( a -- a )
: tcfa cfa ;                   ( PWD -- CFA )
: tnfa nfa ;                   ( PWD -- NFA )
: meta! ! ;                    ( u a --  )
: checksum #target there crc ; ( -- u : calculate CRC of target image )
: finished ( -- : save target image and display statistics )
   hex
  ." COMPLETE" cr
  ." HOST:   " here  . cr
  ." TARGET: " there . cr
   only forth definitions hex
   ." SAVING..."  #target #target there + (save) throw ." DONE" cr
   ." STACK>" .s cr ;
: [a] ( "name" -- : find word and compile an assembler word )
  bl word assembler.1 search-wordlist 0= abort" [a]? "
  cfa compile, ; immediate
: asm[ assembler.1 -order ; immediate ( -- )
a: #literal $8000 a; ( literal instruction - top bit set )
a: #alu     $6000 a; ( ALU instruction, further encoding below... )
a: #call    $4000 a; ( function call instruction )
a: #?branch $2000 a; ( branch if zero instruction )
a: #branch  $0000 a; ( unconditional branch )

a: #t      $0000 a; ( T = t )
a: #n      $0100 a; ( T = n )
a: #t+n    $0200 a; ( T = t+n )
a: #t&n    $0300 a; ( T = t&n )
a: #t|n    $0400 a; ( T = t|n )
a: #t^n    $0500 a; ( T = t^n )
a: #~t     $0600 a; ( T = ~t )
a: #t==n   $0700 a; ( T = n == t? )
a: #n<t    $0800 a; ( T = n < t, signed version )
a: #n>>t   $0900 a; ( T = n right shift by t places )
a: #t-1    $0A00 a; ( T == t - 1 )
a: #r      $0B00 a; ( T = Top of Return Stack )
a: #[t]    $0C00 a; ( T = memory[t] )
a: #n<<t   $0D00 a; ( T = n << t )
a: #sp@    $0E00 a; ( T = depth )
a: #nu<t   $0F00 a; ( T = n < t, unsigned )
a: #cpu!   $1000 a; ( T = set interrupts )
a: #cpu?   $1100 a; ( T = interrupts set? )
a: #rp@    $1200 a; ( T = r-depth )
a: #t==0   $1300 a; ( T = t == 0? )
a: #cpu-id $1400 a; ( T = CPU ID )
\ a: #alu-lit $1500 a; ( T = instruction, hidden )

a: d+1     $0001 or a; ( increment variable stack by one )
a: d-1     $0003 or a; ( decrement variable stack by one )
a: d-2     $0002 or a; ( decrement variable stack by two, not used )
a: r+1     $0004 or a; ( increment variable stack by one )
a: r-1     $000C or a; ( decrement variable stack by one )
a: r-2     $0008 or a; ( decrement variable stack by two, not used )
a: r->pc   $0010 or a; ( Set Program Counter to Top of Return Stack )
a: n->[t]  $0020 or a; ( Set Next on Variable Stack to Top on Variable Stack )
a: t->r    $0040 or a; ( Set Top of Return Stack to Top on Variable Stack )
a: t->n    $0080 or a; ( Set Next on Variable Stack to Top on Variable Stack )
: ?set dup $E000 and abort" argument too large" ; ( u -- )
a: branch  2/ ?set [a] #branch  or t, a; ( a -- : an Unconditional branch )
a: ?branch 2/ ?set [a] #?branch or t, a; ( a -- : Conditional branch )
a: call    2/ ?set [a] #call    or t, a; ( a -- : Function call )
a: ALU        ?set [a] #alu     or    a; ( u -- : Make ALU instruction )
a: alu                    [a] ALU  t, a; ( u -- : ALU operation )
a: literal ( n -- : compile a number into target )
  dup [a] #literal and if   ( numbers above $7FFF take up two instructions )
    invert recurse  ( the number is inverted, and 'literal' is called again )
    [a] #~t [a] alu ( then an invert instruction is compiled into the target )
  else
    [a] #literal or t, ( numbers below $8000 are single instructions )
  then a;
a: return ( -- : Compile a return into the target )
   [a] #t [a] r->pc [a] r-1 [a] alu a;
: previous there =cell - ;                      ( -- a )
: lookback previous t@ ;                        ( -- u )
: call? lookback $E000 and [a] #call = ;        ( -- t )
: call>goto previous dup t@ $1FFF and swap t! ; ( -- )
: fence? fence @  previous u> ;                 ( -- t )
: safe? lookback $E000 and [a] #alu = lookback $001C and 0= and ; ( -- t )
: alu>return previous dup t@ [a] r->pc [a] r-1 swap t! ; ( -- )
: exit-optimize                                 ( -- )
  fence? if [a] return exit then
  call?  if call>goto  exit then
  safe?  if alu>return exit then
  [a] return ;
: exit, exit-optimize update-fence ;            ( -- )
: compile-only tlast @ tnfa t@ $20 or tlast @ tnfa t! ; ( -- )
: immediate    tlast @ tnfa t@ $40 or tlast @ tnfa t! ; ( -- )
: mcreate current @ >r target.1 current ! create r> current ! ;
: thead ( b u -- : compile word header into target dictionary )
  talign
  there [last] t, tlast !
  there #target + pack$ c@ 1+ aligned tcp +! talign ;
: lookahead ( -- b u : parse a word, but leave it in the input stream )
  >in @ >r bl parse r> >in ! ;
: literal [a] literal ;       ( u --  )
: [ ' literal   <literal> ! ; ( -- )
: ] ' (literal) <literal> ! ; ( -- )
: h: ( -- : create a word with no name in the target dictionary )
 [compile] [
 $F00D mcreate there , update-fence does> @ [a] call ;
: t: ( "name", -- : creates a word in the target dictionary )
  lookahead thead h: ;
: ?unstructured $F00D xor if source type cr 1 abort" unstructured! " then ;
: fallthrough; [compile] ] ?unstructured ; ( u -- )
: t; fallthrough; exit, ; \ "[a] return" <- unoptimized version
: ;; t; ?unstructured ;
: fetch-xt @ dup 0= abort" (null) " ; ( a -- xt )
: tconstant ( "name", n --, Run Time: -- )
  >r
  lookahead
  thead
  there tdoConst fetch-xt [a] call r> t, >r
  mcreate r> ,
  does> @ tbody t@ [a] literal ;
: tvariable ( "name", n -- , Run Time: -- a )
  >r
  lookahead
  thead
  there tdoVar fetch-xt [a] call r> t, >r
  mcreate r> ,
  does> @ tbody [a] literal ;
: tlocation ( "name", n -- : Reserve space in target for a memory location )
  there swap t, mcreate , does> @ [a] literal ;
: [t] ( "name", -- a : get the address of a target word )
  bl word target.1 search-wordlist 0= abort" [t]?"
  cfa >body @ ;
: [f] ( "name", -- execute word in host Forth vocabulary )
  bl word forth-wordlist search-wordlist 0= abort" [f]?"
  cfa execute ;
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
: constant mcreate , does> @ literal ;       ( "name", a -- )
: [char] char literal ;                      ( "name" )
: postpone [t] [a] call ;                    ( "name", -- )
: next tdoNext fetch-xt [a] call t, update-fence ; ( a -- )
: exit exit, ;                               ( -- )
: ' [t] literal ;                            ( "name", -- )
: recurse tlast @ tcfa [a] call ;            ( -- )
: ." tdoPrintString fetch-xt [a] call $literal ; ( "string", -- )
: $" tdoStringLit   fetch-xt [a] call $literal ; ( "string", -- )
: nop      ]asm #t                             alu asm[ ;
: dup      ]asm #t      t->n               d+1 alu asm[ ;
: over     ]asm #n      t->n               d+1 alu asm[ ;
: invert   ]asm #~t                            alu asm[ ;
: +        ]asm #t+n                       d-1 alu asm[ ;
: swap     ]asm #n      t->n                   alu asm[ ;
: nip      ]asm #t                         d-1 alu asm[ ;
: drop     ]asm #n                         d-1 alu asm[ ;
: >r       ]asm #n             t->r    r+1 d-1 alu asm[ ;
: r>       ]asm #r      t->n           r-1 d+1 alu asm[ ;
: r@       ]asm #r      t->n               d+1 alu asm[ ;
: @        ]asm #[t]                           alu asm[ ;
: rshift   ]asm #n>>t                      d-1 alu asm[ ;
: lshift   ]asm #n<<t                      d-1 alu asm[ ;
: =        ]asm #t==n                      d-1 alu asm[ ;
: u<       ]asm #nu<t                      d-1 alu asm[ ;
: <        ]asm #n<t                       d-1 alu asm[ ;
: and      ]asm #t&n                       d-1 alu asm[ ;
: xor      ]asm #t^n                       d-1 alu asm[ ;
: or       ]asm #t|n                       d-1 alu asm[ ;
: sp@      ]asm #sp@    t->n               d+1 alu asm[ ;
: 1-       ]asm #t-1                           alu asm[ ;
: rp@      ]asm #rp@    t->n               d+1 alu asm[ ;
: 0=       ]asm #t==0                          alu asm[ ;
: rdrop    ]asm #t                     r-1     alu asm[ ;
\ : 2rdrop ]asm #t                     r-2     alu asm[ ;
\ : rdrop>r ]asm #n            t->r        d-1 alu asm[ ;
: dup>r    ]asm #t             t->r    r+1     alu asm[ ;
: over-and ]asm #t&n                           alu asm[ ;
: over+    ]asm #t+n                           alu asm[ ;
\ : over-xor ]asm #t^n                         alu asm[ ;
: >r-dup   ]asm #n             t->r    r+1     alu asm[ ;
: 2dup=    ]asm #t==n  t->n                d+1 alu asm[ ;
: 2dup<    ]asm #n<t   t->n                d+1 alu asm[ ;
: 2dupu<   ]asm #nu<t  t->n                d+1 alu asm[ ;
: 2dup-xor ]asm #t^n   t->n                d+1 alu asm[ ;
: store    ]asm #n     n->[t]              d-1 alu asm[ ;
: tuck!    ]asm #t     n->[t]              d-1 alu asm[ ;
: cpu-id   ]asm #cpu-id                    d+1 alu asm[ ;
: cpu!     ]asm #cpu!                      d-1 alu asm[ ;
: cpu?     ]asm #cpu?  t->n                d+1 alu asm[ ;
: dup@     ]asm #[t]   t->n                d+1 alu asm[ ;
: r>rdrop  ]asm #r     t->n            r-2 d+1 alu asm[ ;
: rxchg    ]asm #r             t->r            alu asm[ ;
: nip-nip  ]asm #t                         d-2 alu asm[ ;
: for >r begin ;
: meta: : ;
( : :noname h: ; )
: : t: ;
meta: ; t; ;
hide meta:
hide t:
hide t;
]asm #~t              ALU asm[ constant =invert ( invert instruction )
]asm #t  r->pc    r-1 ALU asm[ constant =exit   ( return/exit instruction )
]asm #n  t->r d-1 r+1 ALU asm[ constant =>r     ( to r. stk. instruction )
$20   constant =bl         ( blank, or space )
$D    constant =cr         ( carriage return )
$A    constant =lf         ( line feed )
$8    constant =bs         ( back space )
$7F   constant =del        ( delete key )
\ $1B   constant =escape     ( escape character )
$50   constant tib-length  ( size of terminal input buffer )
$40   constant word-length ( maximum length of a word )
$40   constant c/l         ( characters per line in a block )
$10   constant l/b         ( lines in a block )
$2BAD constant magic       ( magic number for compiler security )
( Volatile variables )
$3B02 constant last-def    ( last, possibly unlinked, word definition )
$3B06 constant id          ( used for source id )
$3B0A constant handler     ( current handler for throw/catch )
$3B0C constant block-dirty ( -1 if loaded block buffer is modified )
$3B14 constant <expect>    ( "accept" vector )
$3B16 constant <tap>       ( "tap" vector, for terminal handling )
$3B18 constant <echo>      ( c -- : emit character )
$3B1A constant context     ( holds current context for search order )
  ( area for context is #vocs large )
$3B2A constant #tib        ( Current count of terminal input buffer )
$3B2C constant tib-buf     ( ... and address )
$3B2E constant tib-start   ( backup tib-buf value )
$3C00 constant block-buffer ( block buffer is stored here )

$10   constant header-length ( location of length in header )
$12   constant header-crc    ( location of CRC in header )
$14   constant header-options ( location of options bits in header )
target.1         +order ( Add target word dictionary to search order )
meta -order meta +order ( Reorder so *meta* has a higher priority )
system           -order ( Remove system vocabulary to previously accidents )
forth-wordlist   -order ( Remove normal Forth words to prevent accidents )

\ ============================================================================
\ # Start of Image

0        t, \  $0: IRQ-0; Reset Vector / maskable IRQ for Down D-PAD button
0        t, \  $2: IRQ-1; UART RX FIFO NOT EMPTY
0        t, \  $4: IRQ-2; UART RX FIFO FULL
0        t, \  $6: IRQ-3; UART TX FIFO NOT EMPTY

0        t, \  $8: IRQ-4; UART TX FIFO FULL
0        t, \  $A: IRQ-5; PS/2 KEYBOARD (NEW CHARACTER)
0        t, \  $C: IRQ-6; TIMER
0        t, \  $E: IRQ-7; D-PAD BUTTON CHANGED

0        t, \ $10: For Length of Forth image, different from VM size
0        t, \ $12: For CRC of Forth image, not entire VM memory
$0001    t, \ $14: Header options
h: doVar   r> ;    ( -- a : push return address and exit to caller )
h: doConst r> @ ;  ( -- u : push value at return address and exit to caller )
[t] doVar   tdoVar   meta!
[t] doConst tdoConst meta!
0 tlocation root-voc     ( root vocabulary )
0 tlocation editor-voc   ( editor vocabulary )
pad-area tconstant pad   ( pad variable - offset into temporary storage )
$8       constant  #vocs ( number of vocabularies in allowed )
$2       tconstant cell  ( size of a cell in bytes )
$400     tconstant b/buf ( size of a block )
0        tlocation #irq  ( Used in example IRQ, count of IRQs )
0        tlocation cp    ( Dictionary Pointer: Set at end of file )
0        tlocation _forth-wordlist ( set at the end near the end of the file )
0        tlocation _system ( system specific vocabulary )
$0       tvariable >in   ( Hold character pointer when parsing input )
1        tlocation seed1 ( PRNG seed; never set to zero )
1        tlocation seed2 ( PRNG seed; never set to zero )
$0       tvariable state ( compiler state variable )
$0       tvariable hld   ( Pointer into hold area for numeric output )
$A       tvariable base  ( Current output radix )
$0       tvariable span  ( Hold character count received by expect   )
0        tvariable blk   ( current blk loaded, set in *cold* )
$FFFF    tvariable dpl   ( number of places after fraction )
0        tvariable current   ( WID to add definitions to )
xchange _forth-wordlist _system
0        tvariable <save>    ( holds execution vector for saving blocks )
0        tvariable <block>   ( holds execution vector for block )
0        tvariable <literal> ( holds execution vector for literal )
0        tvariable <ok>      ( prompt execution vector )
0        tvariable <key>     ( -- c : new character, blocking input )
0        tvariable <emit>    ( c -- : emit character )
( : nop    nop      ; ( -- : do nothing )
: cpu-id  cpu-id    ; ( -- u : returns CPU ID )
: cpu?    cpu?      ; ( -- u : returns CPU status )
: cpu!    cpu!      ; ( u -- : sets CPU status )
xchange _system _forth-wordlist
: dup      dup      ; ( n -- n n : duplicate value on top of stack )
: over     over     ; ( n1 n2 -- n1 n2 n1 : duplicate second value on stack )
: invert   invert   ; ( u -- u : bitwise invert of value on top of stack )
: +        +        ; ( u u -- u : addition without carry )
: swap     swap     ; ( n1 n2 -- n2 n1 : swap two values on stack )
: nip      nip      ; ( n1 n2 -- n2 : remove second item on stack )
: drop     drop     ; ( n -- : remove item on stack )
: @        @        ; ( a -- u : load value at address )
: !        store drop ; ( u a -- : store *u* at address *a* )
: rshift   rshift   ; ( u1 u2 -- u : shift u2 by u1 places to the right )
: lshift   lshift   ; ( u1 u2 -- u : shift u2 by u1 places to the left )
: =        =        ; ( u1 u2 -- t : does u2 equal u1? )
: u<       u<       ; ( u1 u2 -- t : is u2 less than u1 )
: <        <        ; ( u1 u2 -- t : is u2 less than u1, signed version )
: and      and      ; ( u u -- u : bitwise and )
: xor      xor      ; ( u u -- u : bitwise exclusive or )
: or       or       ; ( u u -- u : bitwise or )
: 1-       1-       ; ( u -- u : decrement top of stack )
: 0=       0=       ; ( u -- t : if top of stack equal to zero )
: sp@      sp@      ; ( -- u : stack position [equivalent to depth] )
there constant inline-start
( : rp@ rp@   fallthrough; compile-only ( -- u )
: exit  exit  fallthrough; compile-only ( -- )
: >r    >r    fallthrough; compile-only ( u --, R: -- u )
: r>    r>    fallthrough; compile-only ( -- u, R: u -- )
: r@    r@    fallthrough; compile-only ( -- u )
: rdrop rdrop fallthrough; compile-only ( --, R: u -- )
there constant inline-end

\ ============================================================================
\ # Core Words

( 0        tvariable hidden  ( vocabulary for hidden words )
h: [-1] -1 ;                 ( -- -1 : space saving measure, push -1 )
h: 0x8000 $8000 ;            ( -- $8000 : space saving measure, push $8000 )
h: 2drop-0 drop fallthrough; ( n n -- 0 )
h: drop-0 drop fallthrough;  ( n -- 0 )
h: 0x0000 $0000 ;            ( -- $0000 : space/optimization, push $0000 )
h: state@ state @ ;          ( -- u )
h: first-bit 1 and ;         ( u -- u )
h: in@ >in @ ;               ( -- u )
h: base@ base @ ;            ( -- u )
h: hld@ hld @ ;              ( -- u )
h: blk-@ blk @ ;

h: ?exit if rdrop exit then ; ( u --, R: xt -- xt| : conditional return )
: 2drop drop drop ;         ( n n -- )
: 1+ 1 + ;                  ( n -- n : increment a value  )
: negate 1- invert ;        ( n -- n : negate a number )
: - negate + ;              ( n1 n2 -- n : subtract n1 from n2 )
h: over- over - ;           ( u u -- u u )
: aligned dup first-bit + ; ( b -- a )
h: cell- cell - ;           ( a -- a : adjust address to previous cell )
: cell+  cell + ;           ( a -- a : move address forward to next cell )
: cells 1 lshift ;          ( n -- n : convert cells count to address count )
: chars 1 rshift ;          ( n -- n : convert bytes to number of cells )
: ?dup dup if dup exit then ; ( n -- 0 | n n : duplicate non zero value )
: >  swap  < ;              ( n1 n2 -- t : signed greater than, n1 > n2 )
: u> swap u< ;              ( u1 u2 -- t : unsigned greater than, u1 > u2 )
:  <>  = invert ;           ( n n -- t : not equal )
: 0<> 0= invert ;           ( n n -- t : not equal  to zero )
: 0> 0 > ;                  ( n -- t : greater than zero? )
: 0< 0 < ;                  ( n -- t : less than zero? )
: 2dup over over ;          ( n1 n2 -- n1 n2 n1 n2 )
: tuck swap over ;          ( n1 n2 -- n2 n1 n2 )
: +! tuck @ +  fallthrough; ( n a -- : increment value at *a* by *n* )
h: swap! swap ! ;           ( a u -- )
h: zero 0 swap! ;           ( a -- : zero value at address )
: 1+!   1  h: s+! swap +! ;; ( a -- : increment value at address by 1 )
: 1-! [-1] s+! ;            ( a -- : decrement value at address by 1 )
: 2! ( d a -- ) tuck! cell+ ! ;      ( n n a -- )
: 2@ ( a -- d ) dup cell+ @ swap @ ;  ( a -- n n )
h: get-current current @ ;            ( -- wid )
: bl =bl ;                            ( -- c )
: within over- >r - r> u< ;           ( u lo hi -- t )
h: s>d dup 0< ;                       ( n -- d )
: abs s>d if negate exit then ;       ( n -- u )
: source #tib 2@ ;                    ( -- a u )
h: tib source drop ;                  ( -- a )
: source-id id @ ;                    ( -- 0 | -1 )
: rot >r swap r> swap ;               ( n1 n2 n3 -- n2 n3 n1 )
: -rot rot rot ;                      ( n1 n2 n3 -- n3 n1 n2 )
h: rot-drop rot drop ;                ( n1 n2 n3 -- n2 n3 )
h: d0= or 0= ;                        ( d -- t )
( : 2swap >r -rot r> -rot ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
( : d< rot   )
(     2dup   )
(     > if = nip-nip if 0 exit then [-1] exit then 2drop u< ; ( d -- f )
( : d>  2swap d< ;                    ( d -- t )
( : du> 2swap du< ;                   ( d -- t )
( : d=  rot = -rot = and ;            ( d d -- t )
( : d- dnegate d+ ;                   ( d d -- d )
( : dabs  s>d if dnegate exit then ;  ( d -- ud )
( : even first-bit 0= ;               ( u -- t )
( : odd even 0= ;                     ( u -- t )
( : pow2? dup dup 1- and 0= and ;     ( u -- u|0 : is u a power of 2? )
( : opposite? xor 0< ;                ( n n -- f : true if opposite signs )
: execute >r ;                   ( cfa -- : execute a function )
h: @execute @ ?dup if execute exit then ;  ( cfa -- )
: c@ dup@ swap first-bit 3 lshift rshift h: lsb $FF and ;; ( b--c: char load )
: c! ( c b -- : store character at address )
  tuck first-bit 3 lshift dup>r swap lsb swap
  lshift over @
  $FF r> 8 xor lshift and or swap! ;
: here cp @ ;                         ( -- a )
: align here fallthrough;             ( -- )
h: cp! aligned cp ! ;                 ( n -- )
: allot cp +! ;                       ( n -- )
h: 2>r rxchg swap >r >r ;        ( u1 u2 --, R: -- u1 u2 )
h: 2r> r> r> swap rxchg nop ;    ( -- u1 u2, R: u1 u2 -- )
h: doNext 2r> ?dup if 1- >r @ >r exit then cell+ >r ;
[t] doNext tdoNext meta!
: min 2dup< fallthrough;              ( n n -- n )
h: mux if drop exit then nip ;        ( n1 n2 b -- n : multiplex operation )
: max 2dup > mux ;                    ( n n -- n )
( : 2over 2>r 2dup 2r> 2swap ; )
( : 2nip 2>r 2drop 2r> nop ; )
( : 4dup 2over 2over ; )
( : dmin 4dup d< if 2drop exit else 2nip ; )
( : dmax 4dup d> if 2drop exit else 2nip ; )
\ : um+ ( w w -- w carry )
\	over over+ >r
\	r@ 0 < invert >r
\	over over-and
\	0 < r> or >r
\	or 0 < r> and 1- invert
\	r> swap ;
\ h: upivot ( u1 u2 -- min max ) 2dupu< if exit then swap ;
: um+ 2dupu< 0= if swap then over+ swap over swap u< 1 and ; ( u u -- u carry )

h: dnegate invert >r invert 1 um+ r> + ; ( d -- d )
h: d+ >r swap >r um+ r> + r> + ;         ( d d -- d )
: um* ( u u -- ud )
	0 swap ( u1 0 u2 ) $F
	for dup um+ >r >r-dup um+ r> + r>
		if >r over um+ r> + then
	next rot drop ;
: *    um* drop ;            ( n n -- n )
h: rp! ( n -- , R: ??? -- ??? : set the return stack pointer )
	r> swap begin dup rp@ = 0= while rdrop repeat drop >r ;
: key? <key> @execute ;                   ( -- c -1 | 0 )
: key begin key? until ;                  ( -- c )
: /string over min rot over+ -rot - ;  ( b u1 u2 -- b u : advance string u2 )
h: +string 1 /string ;                 ( b u -- b u : )
: count dup 1+ swap c@ ;               ( b -- b u )
h: string@ over c@ ;                   ( b u -- b u c )
xchange _forth-wordlist _system
h: lshift-xor lshift xor ;
h: rshift-xor rshift xor ;
: crc ( b u -- u : calculate ccitt-ffff CRC )
  [-1] ( -1 = 0xffff ) >r
  begin
    ?dup
  while
   string@ r> swap 
     ( CCITT polynomial $1021, or "x16 + x12 + x5 + 1" )
     over $8 rshift-xor ( crc x )
     dup  $4 rshift-xor ( crc x )
     dup  $5 lshift-xor ( crc x )
     dup  $C lshift-xor ( crc x )
     swap $8 lshift-xor ( crc )
   >r +string
  repeat r> nip ;
xchange _system _forth-wordlist
h: last get-current @ ;         ( -- pwd )
h: echo <echo> @execute ;              ( c -- )
: emit <emit> @execute ;               ( c -- : write out a char )
: cr =cr emit =lf emit ;               ( -- : emit a newline )
: space     1 fallthrough;             ( -- : emit a space )
h: spaces =bl fallthrough;             ( +n -- )
h: nchars                              ( +n c -- : emit c n times )
   swap 0 max for aft dup emit then next drop ;
h: colon-space [char] : emit space ;   ( -- )
\ This version of pick mashes the return stack for large pick depths, an
\ alternative would be to move non-destructively up and down the stack with
\ special instructions.
( vn...v0 u -- vn...v0 vu )
: pick ?dup if swap >r 1- pick r> swap exit then dup ; 
h: ndrop for aft drop then next ;
h: >char dup $7F =bl within if drop [char] _ then ; ( c -- c )
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
h: $type [-1] typist ;                   ( b u --  )
: cmove for aft >r-dup c@ r@ c! 1+ r> 1+ then next 2drop ; ( b b u -- )
: fill  swap for swap aft 2dup c! 1+ then next 2drop ;     ( b u c -- )

: catch  ( xt -- exception# | 0 : return addr on stack )
  sp@ >r        ( xt : save data stack depth )
  handler @ >r  ( xt : and previous handler )
  rp@ handler ! ( xt : set current handler )
  execute       (      execute returns if no throw )
  r> handler !  (      restore previous handler )
  rdrop         (      discard saved stack ptr )
  0x0000 ;      ( 0  : normal completion )

: throw  ( ??? exception# -- ??? exception# )
  ?dup if ( exc# \ 0 throw is no-op )
    handler @ rp! ( exc# : restore prev return stack )
    r> handler !  ( exc# : restore prev handler )
    rxchg         ( saved-sp : exc# on return stack )
    sp@ swap - ndrop r>   ( exc# : restore stack )
    ( return to the caller of catch because return )
    ( stack is restored to the state that existed )
    ( when catch began execution )
  then ;
h: -throw negate throw ;  ( u -- : negate and throw )
[t] -throw 2/ 4 tcells t!
: um/mod ( ud u -- ur uq )
  ?dup 0= if $A -throw exit then
  2dupu<
  if negate $F
    for >r-dup um+ >r >r-dup um+ r> + dup
      r> r@ swap >r um+ r> or
      if >r drop 1 + r> else drop then r>
    next
    drop swap exit
  then 2drop drop [-1] dup ;
: m/mod ( d n -- r q ) \ floored division
  dup 0< dup>r
  if
    negate >r dnegate r>
  then
  >r-dup 0< if r@ + then r> um/mod r>
  if swap negate swap exit then ;
\ : */ >r um* r> m/mod nip ; ( n n n -- )
: /mod  over 0< swap m/mod ; ( n n -- r q )
: mod  /mod drop ;           ( n n -- r )
: /    /mod nip ;            ( n n -- q )
h: 1depth 1 fallthrough; ( ??? -- : check depth is at least one )
h: ?depth dup 0= if drop exit then sp@ 1- u> if 4 -throw exit then ; ( u -- )
: decimal  $A fallthrough;  ( -- : set base to decimal )
h: base!  base ! ;          ( u -- : set base )
: hex     $10 base! ;                      ( -- )
\ h: radix base@ dup 2 - $23 u< ?exit decimal $28 -throw ; ( -- u )
: hold hld@ 1- dup hld ! c! hld@ pad $80 - u> ?exit $11 -throw ; ( c -- )
h: extract dup>r um/mod rxchg um/mod r> rot ;  ( ud ud -- ud u )
h: digit 9 over < 7 and + [char] 0 + ;         ( u -- c )
: #> 2drop hld@ pad over- ;                ( w -- b u )
: #  2 ?depth 0 base@ extract digit hold ;  ( d -- d )
: #s begin # 2dup d0= until ;               ( d -- 0 )
: <# pad hld ! ;                            ( -- )
: sign 0< 0= ?exit [char] - hold ; ( n -- )
h: (.) ( n -- b u : convert a signed integer to a numeric string )
  dup>r abs 0 <# #s r> sign #> ;
h: (u.) 0 <# #s #> ;             ( u -- b u : turn *u* into number string )
: u.r >r (u.) r> fallthrough;    ( u +n -- : print u right justified by +n)
h: adjust over- spaces type ;    ( b n n -- )
h: d5u.r dup fallthrough;        ( u -- u )
h: 5u.r space 5 u.r space ;      ( u -- )
( :  .r >r (.)( r> adjust ;      ( n n -- : print n, right justified by +n )
: u.  (u.) h: blt space type ;;  ( u -- : print unsigned number )
:  .  (.) blt ;                  ( n -- print number )
( : >base swap base @ >r base ! execute r> base ! ; )
( : d. $a  '  . >base ; )
( : h. $10 ' u. >base ; )
h: adown cell negate and ; ( a -- a : align down )
xchange _forth-wordlist _system
: pack$ ( b u a -- a ) \ null fill
  aligned dup>r over
  dup adown
  - over+ zero 2dup c! 1+ swap ( 2dup 0 fill ) cmove r> ;
xchange _system _forth-wordlist
: compare ( a1 u1 a2 u2 -- n : string equality )
  rot
  over- ?dup if >r 2drop r> nip exit then
  for ( a1 a2 )
    aft
      count rot count rot - ?dup
      if rdrop nip-nip exit then
    then
  next 2drop-0 ;
h: ^h ( bot eot cur -- bot eot cur )
  >r over r@ < dup
  if
    =bs dup echo =bl echo echo
  then r> + ;
h: tap dup echo over c! 1+ ; ( bot eot cur c -- bot eot cur )
h: delete? dup =bs = swap =del = or 0= ; ( c -- t : delete key pressed? )
h: ktap                                  ( bot eot cur c -- bot eot cur )
 dup =cr xor
 if delete? \ =bs xor
   if =bl tap exit then ^h exit
 then drop nip dup ;
\ h: ktap? dup =bl - $5F u< swap =del xor and ; ( c -- t : possible ktap? )
: accept ( b u -- b u )
  over+ over
  begin
    2dup-xor
  while
    key dup =bl - $5F u< if tap else <tap> @execute then
  repeat drop over- ;
: expect <expect> @execute span ! drop ;   ( b u -- )
: query tib tib-length <expect> @execute #tib ! drop-0 fallthrough;
h: in! >in ! ;                             ( u -- )
h: word.count count fallthrough; ( nfa -- u : get a words length )
h: word.length $1F and ;
xchange _forth-wordlist _system
: nfa cell+ ; ( pwd -- nfa : move to name field address)
: cfa nfa dup c@ word.length + cell+ adown ; ( pwd -- cfa )
xchange _system _forth-wordlist
h: .id nfa word.count type space ; ( pwd -- : print out a word )
h: immediate? nfa $40 fallthrough; ( pwd -- t : is word immediate? )
h: set? swap @ and 0<> ;           ( a u -- t : is any of 'u' set? )
h: compile-only? nfa $20 set? ;    ( pwd -- t : is word compile only? )
h: inline? inline-start inline-end within ; ( pwd -- t : is word inline? )
h: (search-wordlist) ( a wid -- PWD PWD 1|PWD PWD -1|0 a 0: find word in WID )
  swap >r-dup
  begin
    dup
  while
    dup nfa count $9F ( $1F:word-length + $80:hidden ) and r@ count compare 0=
    if ( found! )
      rdrop
      dup immediate? 1 or negate exit
    then
    nip dup@
  repeat
  rdrop 2drop-0 ;
h: (find) ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 : find a word dictionary )
  >r
  context
  begin
    dup@
  while
    dup@ @ r@ swap (search-wordlist) ?dup
    if
      >r rot-drop r>rdrop exit
    then
    cell+
  repeat drop-0 r> 0x0000 ;
: search-wordlist (search-wordlist) rot-drop ; ( a wid -- PWD 1|PWD -1|a 0 )
: find ( a -- pwd 1 | pwd -1 | a 0 : find a word in the dictionary )
  (find) rot-drop ;
h: digit? ( c base -- u f )
  >r [char] 0 - 9 over <
  if
    7 -
    dup $A < or
  then dup r> u< ;
: >number ( ud b u -- ud b u : convert string to number )
  begin
    ( get next character )
    2dup 2>r drop c@ base@ digit?
    0= if                                 ( d char )
      drop                                ( d char -- d )
      2r>                                 ( restore string )
      nop exit                            ( ..exit )
    then                                  ( d char )
    swap base@ um* drop rot base@ um* d+  ( accumulate digit )
    2r>                                   ( restore string )
    +string dup 0=                         ( advance string and test for end )
  until ;
h: number? ( a u -- d -1 | a u 0 )
  [-1] dpl !
  base@ >r
  string@ [char] - = dup>r if     +string then
  string@ [char] $ =       if hex +string then
  2>r 0 dup 2r>
  begin
    >number dup
  while string@ [char] .  ( fsp @ ) xor
    if rot-drop rot r> 2drop-0 r> base! exit then
    1- dpl ! 1+ dpl @
  repeat 2drop r> if dnegate then r> base! [-1] ;
h: -trailing ( b u -- b u : remove trailing spaces )
  for
    aft =bl over r@ + c@ <
      if r> 1+ exit then
    then
  next 0x0000 ;
h: lookfor ( b u c xt -- b u : skip until *xt* test succeeds )
  swap >r -rot
  begin
    dup
  while
    string@ r@ - r@ =bl = 4 pick execute
    if rdrop rot-drop exit then
    +string
  repeat rdrop rot-drop ;
h: no-match if 0> exit then 0<> ; ( n f -- t )
h: match no-match invert ;        ( n f -- t )
h: parser ( b u c -- b u delta )
  >r over r> swap 2>r
  r@ ' no-match lookfor 2dup
  r> ' match    lookfor swap r> - >r - r> 1+ ;
: parse ( c -- b u ; <string> )
   >r tib in@ + #tib @ in@ - r@ parser >in +!
   r> =bl = if -trailing then 0 max ;
: ) ; immediate ( -- : do nothing )
:  ( [char] ) parse 2drop ; immediate \ ) ( parse until matching paren )
: .( [char] ) parse type ; ( print out text until matching parenthesis )
: \ #tib @ in! ; immediate ( comment until new line )
h: ?length dup word-length u< ?exit $13 -throw ;
: word 1depth parse ?length here pack$ ; ( c -- a ; <string> )
h: token =bl word ;                      ( -- a )
: char token count drop c@ ;             ( -- c; <string> )
h: ?dictionary dup $3B00 u< ?exit 8 -throw ;
: , here dup cell+ ?dictionary cp! ! ; ( u -- : store *u* in dictionary )
: 2, , , ;                             ( u u -- )
: c, here ?dictionary c! cp 1+! ;      ( c -- : store *c* in the dictionary )
h: doLit 0x8000 or , ;                 ( n+ -- : compile literal )
: literal ( n -- : write a literal into the dictionary )
  dup 0x8000 and ( n > $7FFF ? )
  if
    invert doLit =invert , exit ( store inversion of n the invert it )
  then
  doLit ; compile-only immediate ( turn into literal, write into dictionary )
h: make-callable chars $4000 or ;    ( cfa -- instruction )
: compile, make-callable , ;         ( cfa -- : compile a code field address )
h: $compile dup inline? if cfa @ , exit then cfa compile, ; ( pwd -- )
h: not-found source type $D -throw ; ( -- : throw 'word not found' )
h: ?compile dup compile-only? 0= ?exit source type $E -throw ;
xchange _forth-wordlist _system
: (literal) state@ 0= ?exit postpone literal ; ( u -- u | )
xchange _system _forth-wordlist
: interpret ( ??? a -- ??? : The command/compiler loop )
  \ dup count type space ( <- for tracing the parser )
  find ?dup if
    state@
    if
      0> if cfa execute exit then \ <- immediate word are executed
      $compile exit               \ <- compiling word are...compiled.
    then
    drop ?compile     \ <- check it's not a compile only word word
    cfa execute exit  \ <- if its not, execute it, then exit *interpreter*
  then
  \ not a word
  dup>r count number? if rdrop \ it's a number!
    dpl @ 0< if \ <- dpl will -1 if its a single cell number
       drop     \ drop high cell from 'number?' for single cell output
    else        \ <- dpl is not -1, it's a double cell number
       state@ if swap then
       <literal> @execute \ <literal> is executed twice if it's a double
    then
    <literal> @execute exit
  then
  r> not-found ; \ not a word/number, it's an error! NB. We could vector this
: compile  r> dup@ , cell+ >r ; compile-only ( --:Compile next compiled word )
: immediate $40 last nfa fallthrough; ( -- : previous word immediate )
h: toggle tuck @ xor swap! ;        ( u a -- : xor value at addr with u )
h: count+ count + ;         ( b -- b : advance address over counted string )
h: do$ 2r> dup count+ aligned >r swap >r ; ( -- a )
h: string-literal do$ nop ; ( -- a : do string NB. nop to fool optimizer )
h: .string do$ fallthrough; ( -- : print string  )
h: print count type ;                    ( b -- )
[t] .string        tdoPrintString meta!
[t] string-literal tdoStringLit   meta!
( <string>, --, Run: -- b )
: $" compile string-literal fallthrough; immediate compile-only
h: parse-string [char] " word count+ cp! ; ( ccc" -- )
: ." compile .string parse-string ; immediate compile-only ( <string>, -- )
: abort [-1] throw ;                                    ( -- )
h: ?abort swap if print cr abort exit then drop ;              ( u a -- )
h: (abort) do$ ?abort ;                                        ( -- )
: abort" compile (abort) parse-string ; immediate compile-only ( u -- )

\ See:
\ <https://b2d-f9r.blogspot.com/2010/08/16-bit-xorshift-rng-now-with-more.html>
\
\ For a super tiny N-bit PRNG use; "x+=(x*x) | 5;", you can only use the
\ highest bit however.
\ See: <http://www.woodmann.com/forum/showthread.php?3100-super-tiny-PRNG>
: random 
  seed1 @ dup 5 lshift-xor
  seed2 @ seed1 !
  dup 3 rshift-xor
  seed2 @ dup 1 rshift-xor xor dup seed2 !  ;

h: 40ns begin dup while 1- repeat drop ; ( n -- : wait for 'n'*40ns + 30us )
: ms for 25000 40ns next ; ( n -- : wait for 'n' milliseconds )

\ ============================================================================
\ # I/O wordset


xchange _forth-wordlist _system
: segments! $400E ! ; ( u -- : write to 4 7-segment hex displays )
: led!      $4006 ! ; ( u -- : write to 8 LEDs )
: switches  $4006 @ ; ( -- u : retrieve switch on/off for 8 switches )
: timer!    $4004 ! ; ( u -- : set timer and timer control )
: timer     $4004 @ ; ( -- u : get timer and timer control )
\ NB. Perhaps this could be vectored?
h: (irq) 
  ( cpu? 0 cpu! )
  switches led! 
   #irq dup@ segments! 1+!
  ( cpu! ) ;
[t] (irq) 2/ $C t!
: irq $0040 $4010 ! [-1] timer! 1 cpu! ;

\ FIFO: Write Read Enable after Read
\ h: uart? ( uart-register -- c -1 | 0 : generic UART input functions )
\ dup@ $0100 and if drop 0x0000 exit then dup@ $FF and swap $0400 swap! [-1] ; 
\ FIFO: Write Read Enable before Read
h: uart? ( uart-register -- c -1 | 0 : generic UART input functions )
 dup@ $0100 and if drop-0 exit then dup $0400 swap! @ lsb [-1] ; 

: rx?  $4000 uart? if [-1] exit then $4002 uart? ; ( -- c -1|0: rx uart/ps2 )
h: uart! ( c uart-register -- )
	begin dup@ $1000 and 0= until swap $2000 or swap! ;
: tx! dup $4002 uart! ( VGA/VT-100 ) $4000 uart! ( UART )  ;
h: (ok) state@ if cr exit then ."  ok" cr ;  ( -- : default state aware prompt )
h: preset tib-start #tib cell+ ! 0 in! id zero ;  ( -- : reset input )
: io! \ preset 
  ( 115200 / 9600 )
  $35    ( $28B ) $4012 ! ( set TX baud rate - 54 )
  $34    ( $28A ) $4014 ! ( set RX baud rate - 53, hack! )
  $8080 $4016 ! ( set UART control register; 8 bits, 1 stop, no parity )
  0 timer! 0 led! cpu-id segments!
  0 cpu! 0 $4010 ! ( set IRQ mask )
  fallthrough;  ( -- : initialize I/O )
h: console ' rx? <key> ! ' tx! <emit> ! fallthrough;
h: hand
   ' (ok) 
   ' emit ' ktap
   fallthrough;
h: xio  ' accept <expect> ! <tap> ! <echo> ! <ok> ! ;
h: pace [char] . emit ;
: file ' pace ' drop ' ktap xio ;

\ ============================================================================
\ # Evaluation, Control Structures and Vocabulary Words

\ h: pace 11 emit ;
\ : file ' pace ' drop ' ktap xio ;
xchange _system _forth-wordlist
: ] [-1] state !    ;                                ( -- : compile mode )
: [      state zero ; immediate                      ( -- : command mode )
h: empty sp@ ndrop ; ( 0..n -- : empty variable stack )
h: ?error ( n -- : perform actions on error )
  ?dup 0= ?exit
  .             ( print error number )
  [char] ? emit ( print '?' )
  cr            ( and terminate the line )
  empty         ( empty the variable stack )
  fallthrough;
h: prequit      ( perform actions needed to start 'quit' off )
  preset        ( reset I/O streams )
  postpone [ ;  ( back into interpret mode )
h: eval ( -- : evaluation loop, get token, evaluate, loop, prompt )
  begin
    token dup c@
  while
    interpret 0 ?depth
  repeat drop <ok> @execute ;
: quit prequit begin query ' eval catch ?error again ;
h: get-input source in@ source-id <ok> @ ; ( -- n1...n5 )
h: set-input <ok> ! id ! in! #tib 2! ;     ( n1...n5 -- )
: evaluate ( a u -- )
  get-input 2>r 2>r >r
  0 [-1] 0 set-input
  ' eval catch
  r> 2r> 2r> set-input
  throw ;
h: ?check ( magic-number -- : check for magic number on the stack )
   magic = ?exit $16 -throw ;
h: ?unique ( a -- a : print a message if a word definition is not unique )
  dup get-current (search-wordlist) 0= ?exit
    ( source type )
  space
  2drop last-def @ .id ." redefined" cr ;
h: ?nul ( b -- : check for zero length strings )
   dup c@ ?exit $A -throw ;
h: find-token token find fallthrough; ( -- pwd,  <string> )
h: ?not-found ?exit not-found ;       ( t -- )
h: find-cfa find-token cfa ;          ( -- xt, <string> )
: ' find-cfa state@ if postpone literal exit then ; immediate
: [compile] find-cfa compile, ; immediate compile-only  ( --, <string> )
: [char] char postpone literal ; immediate compile-only ( --, <string> )
: ; ?check =exit , postpone [ ( -- wid )
( h: get-current! ) ?dup if get-current ! exit then ;  immediate compile-only
: : align here dup last-def ! ( "name", -- colon-sys )
  last , token ?nul ?unique count+ cp! magic postpone ] ;
: begin here    ; immediate compile-only ( -- a )
: again chars , ; immediate compile-only ( a -- )
: until $4000 or postpone again ; immediate compile-only ( a --, NB. again !? )
h: here-0 here 0x0000 ;
h: >mark here-0 postpone again ;
: if here-0 postpone until ; immediate compile-only
( : unless ' 0= compile, postpone if ; immediate compile-only )
: then fallthrough; immediate compile-only
h: >resolve here chars over @ or swap! ;
: else >mark swap >resolve ; immediate compile-only
: while postpone if ; immediate compile-only
: repeat swap postpone again postpone then ; immediate compile-only
h: last-cfa last-def @ cfa ;  ( -- u )
: recurse last-cfa compile, ; immediate compile-only
: create postpone : drop compile doVar get-current ! postpone [ ;
: >body cell+ ; ( a -- a )
h: doDoes r> chars here chars last-cfa dup cell+ doLit h: !, ! , ;;
: does> compile doDoes nop ; immediate compile-only
: variable create 0 , ;
: constant create ' doConst make-callable here cell- !, ;
: :noname here-0 magic postpone ] ; ( NB. need postpone! )
: for =>r , here ; immediate compile-only
: next compile doNext , ; immediate compile-only
: aft drop >mark postpone begin swap ; immediate compile-only
: marker ( "name", -- : create an eraser )
  here >r current @ dup@ r> 
  create , 2,
  doDoes ( <- meta-compiler version of does> ) 
  dup cell+ 2@ swap! @ here - allot ;

\ h: (do) r@ swap rot >r >r cell+ >r ; ( hi lo -- index )
\ : do compile (do) 0 , here ; compile-only immediate ( hi lo -- )
\ h: (leave) rdrop rdrop rdrop ; compile-only
\ : leave compile (leave) nop ; compile-only immediate
\ h: (loop)
\    r> r> 1+ r> 2dup-xor if
\     >r >r @ >r exit
\    then >r 1- >r cell+ >r ; compile-only
\ h: (unloop) r>rdrop rdrop rdrop >r ; compile-only
\ : unloop compile (unloop) nop ; compile-only immediate
\ h: (?do)
\   2dup-xor if r@ swap rot >r >r cell+ >r exit then 2drop ; compile-only
\ : ?do compile (?do) 0 , here ; compile-only immediate ( hi lo -- )
\ : loop  compile (loop) dup , compile (unloop) cell- here chars ( -- )
\     swap! ; compile-only immediate
\ h: (+loop)
\    r> swap r> r> 2dup - >r
\    2 pick r@ + r@ xor 0< 0=
\    3 pick r> xor 0< 0= or if
\     >r + >r @ >r exit
\    then >r >r drop cell+ >r ; compile-only
\ : +loop ( n -- ) compile (+loop) dup , compile
\   (unloop) cell- here chars swap! ; compile-only immediate
\ h: (i)  2r> tuck 2>r nop ; compile-only ( -- index )
\ : i  compile (i) nop ; compile-only immediate ( -- index )

xchange _forth-wordlist _system
: hide find-token nfa $80 swap toggle ; ( --, <string> : hide word by name )
xchange _system _forth-wordlist
: get-order ( -- widn ... wid1 n : get the current search order )
  context
  0
  ( : find-cell ) >r begin dup@ r@ xor while cell+ repeat rdrop ( ; u a -- a )
  dup cell- swap
  context - chars dup>r 1- s>d if $32 -throw exit then
  for aft dup@ swap cell- then next @ r> ;
xchange _forth-wordlist root-voc
: forth-wordlist _forth-wordlist ; ( -- wid : push forth vocabulary )
: system _system ;                 ( -- wid : push system vocabulary )
: set-order ( widn ... wid1 n -- : set the current search order )
  dup [-1] = if drop root-voc 1 set-order exit then ( NB. Recursion! )
  dup #vocs > if $31 -throw exit then
  context swap for aft tuck! cell+ then next zero ;
: forth root-voc forth-wordlist 2 set-order ; ( -- )
: words
  get-order begin ?dup while 
  swap dup cr u. colon-space @ 
    begin
      ?dup
    while dup 
      nfa c@ $80 and 0= ( <- not-hidden? )
      if dup .id then @ 
    repeat cr 
  1- repeat ;
xchange root-voc _forth-wordlist
( : previous get-order nip 1- set-order ; ( -- )
( : also get-order over swap 1+ set-order ;     ( wid -- )
: only [-1] set-order ;                         ( -- )
( : order get-order for aft . then next cr ;    ( -- )
( : anonymous get-order 1+ here 1 cells allot swap set-order ; ( -- )
: definitions context @ current ! ( <- set current ) ; ( -- )
h: (order)                                      ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r (order) over r@ xor
    if
      1+ r> -rot exit
    then rdrop
  then ;
: -order get-order (order) nip set-order ;             ( wid -- )
: +order dup>r -order get-order r> swap 1+ set-order ; ( wid -- )
: editor editor-voc +order ; ( -- : load editor vocabulary )

\ ============================================================================
\ # Block Wordset

\ h: updated? block-dirty @ ;          ( -- f )
: update [-1] block-dirty ! ;          ( -- )
\ h: +block blk-@ + ;                  ( n -- k )
\ h: clean-buffers 0 block-dirty ! ;
\ h: empty-buffers clean-buffers 0 blk ! ;  ( -- )
: flush 
  blk-@ 0= block-dirty @ d0= ?exit
  block-buffer b/buf blk-@ <save> @execute throw
  0 block-buffer ! ( <- clean-buffer )
  0 h: blk! blk ! ;;        ( <- empty-buffers )
h: ?block if $23 -throw exit then ;
: block ( k -- a )
  1depth
  dup 0= ?block ( check validity of block number )
  dup blk-@ = if drop block-buffer exit then ( block already loaded )
  flush
  dup>r block-buffer b/buf r> <block> @execute throw 
  blk!
  block-buffer ;

\ xchange _forth-wordlist _system
\ : (block) ( a u k -- f )
\	1- dup $C u> ?block
\	$A lshift ( <- b/buf * )
\	-rot cmove 0x0000 ;
\ : (save) 2drop drop-0 ; ( a u k -- f )
\ xchange _system _forth-wordlist

\ ============================================================================
\ # Memory Interface

\ The manual for the Nexys 3 board specifies that there is a PCM
\ memory device called the NP8P128A13T1760E, this is a device behaves like
\ normal flash with the addition that individual cells can be written to
\ without first erasing the block, this is accomplished with an extension
\ to the Common Flash Interface that most flash devices support. However,
\ there are boards with the PC28F128P33BF60 on in lieu of this, which is a
\ normal flash device without the "bit alterable write" extension. Normal
\ flash memory works by erasing a block of data, which sets all the bits. 
\ Writing to the memory works by masking in a value as bits can be cleared in
\ a memory cell but not set, they can only be set by erasing a block.
\ 
\ The Nexys 3 has three memory devices, two of which are accessed over
\ a parallel interface. They share the same data and address bus, and
\ can be selected with a chip select. The signals to be controlled
\ are:
\ 
\ 	+-----+-------------------------+
\ 	| Bit | Description             |
\ 	|-----|-------------------------|
\ 	| 0-9 | Upper Memory Bits       |
\ 	| 10  | Flash chip select       |
\ 	| 11  | SRAM chip select        |
\ 	| 12  | Memory Wait [not used]  |
\ 	| 13  | Flash Reset             |
\ 	| 14  | Output Enable           |
\ 	| 15  | Write Enable            |
\ 	+-----+-------------------------+
\ 
\ The usage of the output enable and write enable are mutually exclusive,
\ as are both of the chip selects.  )
\ 
xchange _forth-wordlist _system

0 tlocation memory-upper  ( upper bits of external memory address )
0 tlocation memory-select ( SRAM/Flash select SRAM = $400, Flash = 0 )
0 tlocation _blockop

h: mdelay 5 40ns ; 

h: mcontrol! ( u -- : write to memory control register )
  $F3FF and
  memory-select @ $400 + or               ( select correct memory device )
  $1FF invert and            ( mask off control bits )
  memory-upper @ $1FF and or ( or in higher address bits )
  $400A ! ; ( and finally write in control )

: m! ( n a -- : write to non-volatile memory )
  $400C !
  $4008 !
  mdelay
  0x8000 mcontrol!
  mdelay
  $0000 mcontrol! ;

: m@ ( a -- n : read from non-volatile memory )
  $400C !
  $4000 mcontrol! ( read enable mode )
  mdelay
  $4008 @        ( get input )
  $0000 mcontrol! ;

: sram   $400 memory-select ! ;
h: nvram $000 memory-select ! ; 
h: block-mode memory-upper @ memory-upper zero ; ( -- hi )
h: flash-reset ( -- : reset non-volatile memory )
  $2000 mcontrol! mdelay $0000 mcontrol! ; 
h: flash! dup>r m! r> m! ; ( u u a )
: flash-status nvram $70 0 m! 0 m@ ; ( -- status )
: flash-read   $FF 0 m! ;      ( -- )
: flash-setup  memory-select @ 
   if flush then nvram flash-reset block-mode drop 20 ms ;
h: flash-wait begin flash-status $80 and until ; 
: flash-clear $50 0 m! ; ( -- clear status )
: flash-write $40 swap flash! flash-wait ; ( u a -- )
\ : flash-unlock block-mode >r $d0 swap $60 swap flash! r> memory-upper ! ; 
\ : flash-lock block-mode >r $01 swap $60 swap flash! r> memory-upper ! ;
\ : flash-lock-down block-mode >r $2f swap $60 swap flash! r> memory-upper ! ;
: flash-erase block-mode >r flash-clear $D0 swap $20 swap 
    flash! flash-wait r> memory-upper ! ; ( ba -- )
: flash-query $98 0 m! ; ( -- : query mode )

( -- read id mode : does the same as flash-query on the PC28F128P33BF60 )
\ : flash-read-id   $90 0 m! ; 

h: flash->sram ( a a : transfer flash memory cell to SRAM )
  nvram flash-clear flash-read
  m@ sram swap m! ;

: transfer ( a a u -- : transfer memory block from Flash to SRAM )
  ?dup 0= if 2drop exit then
  1-
  for
    2dup
    flash->sram
    cell+ swap cell+ swap
  next 2drop ;
\ .set flash-voc $pwd

h: c>m swap @ swap m! ; ( a a --  )
h: m>c m@ swap! ;      ( a a -- )

h: mblock ( a u k -- f )
  1- b/buf um* memory-upper ! >r
  begin
    dup
  while
    over r@ _blockop @execute r> cell+ >r
    cell /string
  repeat
  rdrop 2drop-0 ;

: (save)  ' c>m _blockop ! mblock ; 
: (block) ' m>c _blockop ! mblock ; 

\ ============================================================================
\ # ANSI Escape Sequences

\ TODO Add to a VT100/ANSI Escape Sequence wordset

h: CSI $1B emit [char] [ emit ;                     ( -- )
h: 10u. base@ >r decimal 0 <# #s #> type r> base! ; ( u -- )
: ansi swap CSI 10u. emit ;                         ( n c -- )
xchange _system _forth-wordlist
: at-xy CSI 10u. $3B emit 10u. [char] H emit ; ( x y -- ) \ <at-xy> @execute
: page 2 [char] J ansi 1 1 at-xy ;             ( -- )     \ <page>  @execute
xchange _forth-wordlist _system
: sgr   [char] m ansi ; ( -- : emit an SGR )
: up    [char] A ansi ; ( u -- : move the cursor up )
: down  [char] B ansi ; ( u -- : move the cursor down )
: right [char] C ansi ; ( u -- : move the cursor right )
: left  [char] D ansi ; ( u -- : move the cursor left )

\ 0 constant black 1 constant red 2 constant green 4 constant blue
\ red green        + constant yellow
\     green blue   + constant cyan
\ red       blue   + constant magenta
\ red green blue + + constant white
\ : background $A + ;
\ : color $1E + sgr ;

h: tableu ( -- )
   $7 for
     $A right $7 r@ - $28 + dup sgr u. colon-space
     $7   for $7 r@ - $1E + dup sgr u. next cr
   next ;
 
: table page $2 down tableu 1 sgr $2 down tableu 0 sgr ; ( -- )
\ : nuf? key? if drop [-1] exit then 0x0000 ; ( -- f )
xchange _system _forth-wordlist

\ ============================================================================
\ # Higher Level Block Words

h: c/l* ( c/l * ) 6 lshift ;             ( u -- u )
h: line c/l* swap block + c/l ;          ( k u -- a u )
\ h: loadline line evaluate ;            ( k u -- )
: load 0 $F for 2dup 2>r line evaluate 2r> 1+ next 2drop ; ( k -- )
h: pipe [char] | emit ;                  ( -- )
\ h: .line line -trailing $type ;        ( k u -- )
h: .border 3 spaces c/l [char] - nchars cr ; ( -- )
: thru over- for dup load 1+ next drop ; ( k1 k2 -- )
( : message l/b extract .line cr ;       ( u -- )
h: retrieve block drop ;                 ( k -- )
: list                                   ( k -- )
  dup retrieve
  cr
  .border
  0 begin
    dup l/b <
  while
    2dup dup 2 u.r pipe line $type pipe cr 1+
  repeat .border 2drop ;

\ : list  block l/b for dup l/b 1- r@ - c/l * + c/l type cr next ;

: index ( k1 k2 -- : show titles for block k1 to k2 )
  over- cr
  for
    dup 5u.r pipe space dup 0 line $type cr 1+
  next drop ;
( : --> blk-@ 1+ load ; immediate )
\ ============================================================================
\ # Boot Sequence

\ We could select different behaviors depending on what switches were set at
\ at start up, some for debugging, others for functionality.

h: bist ( -- u : built in self test )
  header-options @ first-bit 0= if 0x0000 exit then ( is checking disabled? )
  header-length @ here xor if 2 exit then ( length check )
  header-crc @ header-crc zero            ( retrieve and zero CRC )
  0 here crc xor if 3 exit then           ( check CRC )
  1 header-options toggle 0x0000 ;        ( disable check, success )
: bye fallthrough;
h: cold ( -- : performs a cold boot  )
  bist ?dup if negate dup bye exit then
  io! forth
  empty 0 rp!
  hex cr ." eFORTH v" cpu-id 0 u.r cr here .  $4000 here - u. cr 
  ." loading..."
  0 0 0x8000 transfer (ok)
  1 block c@ 32 127 within ( <- ASCII? ) if 
    (ok) ( 1 load ) else ." failed" cr 
  then quit ;

\ ============================================================================
\ # Decompiler / Tools

h: validate over cfa xor if drop-0 exit then nfa ; ( pwd cfa -- nfa | 0 )
h: search-for-cfa ( wid cfa -- nfa | 0 : search for CFA in a word list )
  cells $1FFF and >r
  begin
    dup
  while
    dup@ over r@ -rot  within
    if dup@ r@ validate ?dup if rdrop nip exit then then
    @
  repeat rdrop ;
h: name ( cwf -- a | 0 )
   >r
   get-order
   begin
     dup
   while
     swap r@ search-for-cfa ?dup if >r 1- ndrop r>rdrop exit then
   1- repeat rdrop ;
( h: neg? dup 2 and if $FFFE or then ;  )
( h: .alu  ( u -- )
(   dup 8 rshift $1F and 5u.r )
(   dup $80 and if ." t->n  " then   )
(   dup $40 and if ." t->r  " then  )
(   dup $10 and if ." r->pc " then  )
(   dup $0C and [char] r emit 2 rshift neg? . space )
(       $03 and [char] d emit          neg? . ;  )
h: ?instruction ( i m e -- i t )
   >r over-and r> = ;
( a -- : find word by address, and print )
h: .name dup $1FFF and cells 5u.r ( a -- )
         name ?dup if word.count type then ;
h: decompile ( u -- : decompile a single instruction )
   0x8000 0x8000 ?instruction if [char] L emit $7FFF and 5u.r exit then
    $6000  $6000 ?instruction if [char] A emit  drop ( .alu ) exit then
    $6000  $4000 ?instruction if [char] C emit .name exit then
    $6000  $2000 ?instruction if [char] Z emit .name exit then
   [char] B emit .name ;
h: decompiler ( previous current -- : decompile starting at address )
  >r
  begin dup r@ u< while
    d5u.r colon-space
    dup@
    d5u.r decompile cr cell+
  repeat rdrop drop ;
: see ( --, <string> : decompile a word )
  token (find) ?not-found
  swap 2dup= if drop here then >r
  cr colon-space dup .id dup cr
  cfa r> decompiler space [char] ; emit
  dup compile-only? if ."  compile-only" then
  dup inline?       if ."  inline"       then
      immediate?    if ."  immediate"    then cr ;
: .s ( -- ) cr sp@ for aft r@ pick . then next ."  <sp" cr ; ( -- )
: dump ( a u -- )
  $10 + \ align up by dump width
  4 rshift ( <-- equivalent to "dump-width /" )
  for
    aft
      cr $10 2dup
      over 5u.r colon-space
      chars for aft dup@ 5u.r cell+ then next ( <- dm+; [ a u -- ] )
      -rot
      2 spaces $type
    then
  next drop ;
\ ============================================================================
\ # Block Editor

[last]              [t] _forth-wordlist t!
[t] _forth-wordlist [v] current         t!
0 tlast meta!
h: [block] blk-@ block ;       ( k -- a : loaded block address )
h: [check] dup b/buf 6 rshift u< ?exit $18 -throw ;
h: [line] [check] c/l* [block] + ; ( u -- a )
: b retrieve ;                 ( k -- : Load a block )
: l blk-@ list ;               ( -- : list current block )
: n   1  h: +blv blk-@ + b l ;;   ( -- : load and list Next block )
: p [-1]    +blv ;             ( -- : load and list Previous block )
: d [block] b/buf h: blank =bl fill ;;   ( -- : Zero/blank loaded block )
: k [line] c/l blank ;         ( u -- : delete/Kill line )
: s update flush ;             ( -- : Save changes to disk )
: q editor-voc -order ;        ( -- : Quit editor )
: x q blk-@ load editor ;      ( -- : eXecute/evaluate block )
: ia c/l* + [block] + tib in@ + ( u u -- Insert At )
   swap source nip in@ - cmove postpone \ ;
: i 0 swap ia ;                ( u -- : Insert line )
( : u update ;                 ( -- : set block set as dirty )
( : w words ; )
( : yank pad c/l ; )
( : c [line] yank >r swap r> cmove ; )
( : y [line] yank cmove ; )
( : ct swap y c ; )
( : xa [line] c/l evaluate ; )
( : sw 2dup y [line] swap [line] swap c/l cmove c ; )
[last] [t] editor-voc t! 0 tlast meta!

\ ============================================================================
\ # Final Touches
there [t] cp t!
[t] (literal)      [v] <literal> t! ( set literal execution vector )
[t] (block)        [v] <block>   t! ( set block execution vector )
[t] (save)         [v] <save>    t! ( set block execution vector )
[t] cold 2/            0         t! ( set starting word in boot-loader )
there    [t] header-length t! \ Set Length First!
checksum [t] header-crc t!    \ Calculate image CRC
finished
bye
