system +order 0 <ok> !
\ Author:     Richard James Howe
\ License:    MIT 
\ Repository: https://github.com/howerj/forth-cpu
\ 

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
: store    ]asm #n     n->[t]              d-1 alu asm[ ;
: tuck!    ]asm #t     n->[t]              d-1 alu asm[ ;
: cpu-id   ]asm #cpu-id                    d+1 alu asm[ ;
: cpu!     ]asm #cpu!                      d-1 alu asm[ ;
: cpu?     ]asm #cpu?  t->n                d+1 alu asm[ ;
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

h: doVar   r> ;
h: doConst r> @ ;
[t] doVar   tdoVar   meta!
[t] doConst tdoConst meta!
h: ! store drop ;
h: stop begin again ;
h: main $1 $4006 ! stop nop ;

[t] main 2/  0 t! ( set jump to main )
finished          ( save image )
bye               ( quit )
