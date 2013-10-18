( From http://mathforum.org/library/drmath/view/62672.html, slighty modified )

: /mod
  2dup
  mod -rot /
; 

0 array sine
   0 ,  175 ,  349 ,  523 ,
 698 ,  872 , 1045 , 1219 ,
1392 , 1564 , 1736 , 1908 ,
2079 , 2250 , 2419 , 2588 ,
2756 , 2924 , 3090 , 3256 ,
3420 , 3584 , 3746 , 3907 ,
4067 , 4226 , 4384 , 4540 ,
4695 , 4848 , 5000 , 5150 ,
5299 , 5446 , 5592 , 5736 ,
5878 , 6018 , 6157 , 6293 ,
6428 , 6561 , 6691 , 6820 ,
6947 , 7071 , 7193 , 7314 ,
7431 , 7547 , 7660 , 7771 ,
7880 , 7986 , 8090 , 8192 ,
8290 , 8387 , 8480 , 8572 ,
8660 , 8746 , 8829 , 8910 ,
8988 , 9063 , 9135 , 9205 ,
9272 , 9336 , 9397 , 9455 ,
9511 , 9563 , 9613 , 9659 ,
9703 , 9744 , 9781 , 9816 ,
9848 , 9877 , 9903 , 9925 ,
9945 , 9962 , 9976 , 9986 ,
9994 , 9998 , 10000 ,

: (sin) ( n -- n' )
  dup 90 > if 180 swap - then sine 
;

: sin ( n -- n' )
  360 mod dup 0<
  if 360 + then dup 180 > 
  if 180 - (sin) @ negate else (sin) @ then 
;

: cos ( n -- n' )  360 mod 90 + sin ;

: 32log ( n -- n' )
  0
  begin
  swap 1 rshift dup if
      swap 1+ 0
    else
      drop 1
    then
  until
;

: logical ( number -- bool )
  not not
;

: signum ( number -- signum )
  dup dup
  0> if 2drop  1 exit then
  0= if drop   0 exit then
  0< if       -1 exit then
;


\ 0 variable anon 
\ : map ( num ... 0 xt -- num ... 0 )
\   anon !            ( store filter function )
\   v @reg 1- 0 swap  ( 0 to number of items in the stack )
\   do  
\     j@ i@ -  pick anon @ execute i@ 1+ !var
\   loop
\   anon @ execute
\ ; ( example> 1 2 3 :noname 2 + ; map .s )

: range   ( nX nY -- nX nX+1 ... nY ) 1+ do i@ loop ;
: repeat  ( n0 X -- n0 ... nX ) 0 swap 1- do dup loop ;
: sum 0   ( n0 ... nX X -- sum<0..X> ) swap 1- do + loop ;
: mul 0   ( n0 ... nX X -- mul<0..X> ) swap 1- do * loop ;
: average ( n0 ... nX X -- sum<0..X>/X ) dup >r sum r> / ;
: auto    ( -- stkp ) v @reg 1- ;

: <= > not ;
: >= < not ;

: factorial ( n -- n! ) 
  abs dup 1 
  <= if 
    1 
  else 
    dup >r 1 swap range r> mul 
  then 
;

( TODO : Reverse, filter, reduce )
