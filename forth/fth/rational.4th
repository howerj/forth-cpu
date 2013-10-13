( =========================================================================== )
( == Words for manipulating rational types ================================== )
( =========================================================================== )

: simplify ( a b -- a/gcd{a,b} b/gcd{a/b} )
  2dup
  gcd
  tuck
  /
  -rot
  /
  swap \ ? check this
;

: crossmultiply ( a b c d -- a*d b*d c*b d*b )
  rot   ( a c d b )
  2dup  ( a c d b d b )
  *     ( a c d b d*b )
  >r    ( a c d b , d*b )
  rot   ( a d b c , d*b )
  *     ( a d b*c , d*b )
  -rot  ( b*c a d , d*b )
  *     ( b*c a*d , d*b )
  r>    ( b*c a*d d*b )
  tuck  ( b*c d*b a*d d*b )
  2swap ( done! )
;

: ratmul ( a/b c/d -- {a/b}*{c/d} )
  rot
  *
  -rot
  *
  swap
  simplify
;

: ratdiv ( a/b c/d -- {a/b}/{c/d} )
  swap
  ratmul
;

: ratadd ( a/b c/d -- {a/b}+{c/d} )
  crossmultiply
  rot
  drop ( or check if equal, if not there is an error )
  -rot
  +
  swap
  simplify
;

: ratsub ( a/b c/d -- {a/b}-{c/d} )
  crossmultiply
  rot
  drop ( or check if equal, if not there is an error )
  -rot
  -
  swap
  simplify
;

: ratprint ( a/b -- )
  swap
  prnn ."  / " prnn cr
;

: ratequal ( a/b c/d -- bool )
  rot = 
  -rot
  + 2 = 
;

: ratgreater ( a/b c/d -- bool )
  crossmultiply \ lazy way of doing things...
  rot
  2drop
  >
;

: ratless ( a/b c/d -- bool )
  crossmultiply \ lazy way of doing things...
  rot
  2drop
  <
;
