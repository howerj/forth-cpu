( malloc / free / memcpy / memset )

512 constant BLKSZ  ( size of a block )
128 constant BLKCNT ( number of blocks )
2   constant RCDSZ  ( size of a record )

0   constant RCD_BEGIN  ( end of the malloc block )
1   constant RCD_END    ( end of the malloc block )
2   constant MAL_FREE   ( the pointer is free )
3   constant MAL_TAKEN  ( only one block is taken )
4   constant MAL_BEGIN  ( the beginning of an allocated contigous section )
5   constant MAL_MID    ( ... allocated contigous section )
6   constant MAL_END    ( ... end of an allocated contigous section )

RCDSZ BLKSZ *   array FREELIST ( pointers to blocks and their flags )
BLKSZ BLKCNT *  array HEAP    ( the heap! )

0 HEAP constant HEAP_BEGIN    ( the beginning of the heap )

: G_RCD ( record_number -- pointer flag )
  2* FREELIST dup 1+ @ swap @ swap
;

: S_RCD ( pointer flag record_number -- )
  2* FREELIST dup 1+  -rot ! !
;

( Initialize the heap )
: INIT_HEAP ( -- )
  0 BLKCNT do 
    i@ BLKSZ * HEAP MAL_FREE i@  S_RCD
  loop
;

: _malloc ( -- ptr )
  0 BLKCNT do
    i@ G_RCD swap 
    MAL_FREE = if
      dup MAL_TAKEN i@ S_RCD
      break
    else
      drop
    then
  loop
  i@ BLKCNT = if 0 then 
;

: _free ( ptr -- )
  dup HEAP_BEGIN - BLKSZ / MAL_FREE swap S_RCD
;

: malloc ( count -- ptr ) BLKSZ mod ;
: free ( ptr -- ) ;

: memcpy ( des_ptr src_ptr count -- )
  over +
  do
    dup i@ @ swap ! 1+
  loop
  drop
;

: memset ( ptr val count -- )
  rot dup rot +
  do
    dup i@ !
  loop
;

: calcfrag \ 1 - ( largest block of free memory / total free memory )
;
: defrag ( -- ) ; 

INIT_HEAP
