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
0  mALU  constant T         ." TOS = "          T         prnn ." , " cr
1  mALU  constant N         ." NOS = "          N         prnn ." , " cr
2  mALU  constant R         ." RET = "          R         prnn ." , " cr
3  mALU  constant [T]       ." fetch = "        [T]       prnn ." , " cr
4  mALU  constant depth     ." depth = "        depth     prnn ." , " cr
5  mALU  constant T|N       ." TorN = "         T|N       prnn ." , " cr
6  mALU  constant T&N       ." TandN = "        T&N       prnn ." , " cr
7  mALU  constant T^N       ." TxorN = "        T^N       prnn ." , " cr
8  mALU  constant ~(T^N)    ." Txnor = "        ~(T^N)    prnn ." , " cr
9  mALU  constant ~T        ." invT = "         ~T        prnn ." , " cr
10 mALU  constant T+N       ." TplusN = "       T+N       prnn ." , " cr
11 mALU  constant N-T       ." NsubT = "        N-T       prnn ." , " cr
12 mALU  constant N<<T      ." NlslT = "        N<<T      prnn ." , " cr
13 mALU  constant N>>T      ." NrslT = "        N>>T      prnn ." , " cr
14 mALU  constant NrolT     ." NrolT = "        NrolT     prnn ." , " cr
15 mALU  constant NrorT     ." NrorT = "        NrorT     prnn ." , " cr
16 mALU  constant L(T)*L(N) ." LmulT = "        L(T)*L(N) prnn ." , " cr
17 mALU  constant Nu<T      ." NuLessT = "      Nu<T      prnn ." , " cr
18 mALU  constant N<T       ." NLessT = "       N<T       prnn ." , " cr
19 mALU  constant N=T       ." NeqT = "         N=T       prnn ." , " cr
20 mALU  constant T<0       ." TlessZ = "       T<0       prnn ." , " cr
21 mALU  constant T=0       ." TeqZ = "         T=0       prnn ." , " cr
22 mALU  constant swapbytes ." swapbytes = "    swapbytes prnn ." , " cr
23 mALU  constant togglei   ." togglei = "      togglei   prnn ." , " cr
24 mALU  constant T-1       ." TsubOne = "      T-1       prnn ." , " cr
25 mALU  constant clr       ." clr = "          clr       prnn ." , " cr
26 mALU  constant setcarry  ." setcarry = "     setcarry  prnn ." , " cr
27 mALU  constant flags     ." flags = "        flags     prnn ." , " cr

: mLit     1 15 lshift ;
: T->N     1 7  lshift ;
: T->R     1 6  lshift ;
: N->[T]   1 5  lshift ;
: R->PC    1 4  lshift ;

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
: _depth    alu[ depth T->N d+1 or or ]alu ;
: _togglei  alu[ togglei ]alu ;
: _swapbytes alu[ swapbytes ]alu ;

: label immediate
	create 
	_push , @pc , 
	' exit , 
;

: start
    5 jmp
;

: stop
    pmem
    input fclose kernel
    output fclose kernel
;

: halt
 31 error
;
