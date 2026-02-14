\ BLOCK SUBSYSTEM
create blk( 64 allot
here @ value blk)
-1 value blk>
: >blk ( n -- addr ) 10 lshift $7e00 + ;
: blk@ ( n -- ) dup to blk> >blk blk( 64 move ;
: blk! ( -- ) blk( blk> >blk 64 move ;
: load ( n -- ) >blk line ! 16 count ! ;
: thru ( n1 n2 -- ) over - 1+ count ! >blk line ! ;
: .2 ( n -- ) cr dup 10 < if space then . space ;
: list >blk 16 >r begin 16 r@ - .2 dup emitln 64 + next drop ;
: index 16 >r begin 16 r@ - .2 dup >blk emitln 1+ next drop ;
: wipe ( n -- ) >blk 1024 0 fill ;
: copy ( s d -- ) >blk swap >blk swap 1024 move ; hide .2
: flush 32 >r begin r@ 1- write next ;

\ OTHER
: >r? ( b R: a -- R: a b ) r> r> rot >r >r >r ;
: r>? ( R: a b -- b R: a ) r> r> r> nrot >r >r ;
: dump ( addr u -- ) >r begin cr ':' emit dup .x space
4 >r begin dup c@ dup .x >r? 1+ dup c@ dup .x >r? 1+ space next
8 >r begin r>? next 8 >r begin dup $20 < if drop '.' then emit
next next drop ; hide >r? hide r>?

: memory-used here @ $0500 - 1000 / . ;
: memory-free $7c00 here @ - 1000 / . ;
: hello ." Welcome to TANUKI OS" cr
  memory-used ." kB used " memory-free ." kB free" ;
page hello forget memory-used



\ BLOCK EDITOR
: hello ." Hello, world" ;
