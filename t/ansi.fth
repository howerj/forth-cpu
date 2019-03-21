\ RJH - ANSI TERMINAL TESTS / 2019. TESTED IN GFORTH
\ Based around the definition for 'F' found here:
\ https://www.forth.com/starting-forth/1-forth-stacks-dictionary/
\
\ This file is used to prepare test vectors for the VT-100 terminal emulator,
\ a mini-language is make for drawing text and displaying colors.
\
ONLY FORTH DEFINITIONS DECIMAL

WORDLIST CONSTANT COOL-TEXT ( <_< - way cool )
COOL-TEXT >ORDER DEFINITIONS
VARIABLE COOL-CHARACTER CHAR * COOL-CHARACTER !
: BYE BYE ;
: CR $D EMIT $A EMIT ;
: {{ CR COOL-TEXT >ORDER DUP ;
: >_> CHAR COOL-CHARACTER ! ;
: <_< [CHAR] * COOL-CHARACTER ! ;
: \ POSTPONE \ ;
: .( POSTPONE .( ;

: CSI $1B EMIT [CHAR] [ EMIT ; 
: 10U. BASE @ >R DECIMAL 0 <# #S #> TYPE R> BASE ! ;
: ANSI SWAP CSI 10U. EMIT ; ( N C -- )
: AT-XY CSI 10U. $3B EMIT 10U. [CHAR] H EMIT ;
: PAGE 2 [CHAR] J ANSI 1 1 AT-XY ;
: SGR [CHAR] m ANSI ;

: UP    [CHAR] A ANSI ;
: DOWN  [CHAR] B ANSI ;
: RIGHT [CHAR] C ANSI ;
: LEFT  [CHAR] D ANSI ;

0 CONSTANT BLACK 1 CONSTANT RED 2 CONSTANT GREEN 4 CONSTANT BLUE
RED GREEN        + CONSTANT YELLOW
    GREEN BLUE   + CONSTANT CYAN
RED       BLUE   + CONSTANT MAGENTA
RED GREEN BLUE + + CONSTANT WHITE
: BACKGROUND $A + ;
: COLOR $1E + SGR ;

: BLINKY $5 SGR ;
: UNBLINK $19 SGR ;
: BOLD   $1  SGR ;
: NORMAL $16 SGR ;
: REVERSE $7 SGR ;
: ESREVER $1B SGR ;

: DOT     COOL-CHARACTER @ EMIT ;
: DOTS    0 DO  DOT   LOOP ;
: BLIP    1 DOWN DUP RIGHT    DOT DUP 2 * LEFT ;
: BAR     1 DOWN DUP RIGHT 5 DOTS DUP 2 * LEFT ;
: F       BAR BLIP BAR BLIP BLIP DROP 5 UP ;
: C       BAR BLIP BLIP BLIP BAR DROP 5 UP ;
: E       DUP F C ;
: I       BLIP BLIP BLIP BLIP BLIP DROP 5 UP ;
: O       DUP C 4 + I ;
: A       DUP F 4 + I ;
: _       4 DOWN BAR DROP 5 UP ;
: TOP     BAR DROP 1 UP ;
: U       DUP I DUP _ 4 + I ;
: J       DUP _ 4 + I ;
: L       DUP I _ ;
: W       DUP L DUP 2 + I 4 + I ;
: M       DUP I DUP 2 + I DUP 4 + I TOP ;
: -       2 DOWN BAR DROP 3 UP ;
: H       DUP I DUP - 4 + I ;
: T       DUP TOP 2 + I ;
: D       DUP I DROP ;

: |       7 + DUP ;
: }} DROP 6 DOWN PREVIOUS ;
SEAL
