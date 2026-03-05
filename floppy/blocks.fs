\ BLOCK SUBSYSTEM
create blk( 1024 allot here @ value blk) -1 value blk>
: >blk ( n -- addr ) 10 lshift $7e00 + ;
: blk@ ( n -- ) dup to blk> >blk blk( 1024 move ;
: blk! ( -- ) blk( blk> >blk 1024 move blk> write ;
: load ( n -- ) >blk line ! 16 count ! ;
: thru ( n1 n2 -- ) over - 1+ 4 lshift count ! >blk line ! ;
: .2 ( n -- ) cr dup 10 < if space then . space ;
: display 16 >r begin 16 r@ - .2 dup emitln 64 + next drop ;
: list >blk display ;
: index 16 >r begin dup .2 dup >blk emitln 1+ next drop ;
: wipe ( n -- ) >blk 1024 0 fill ;
: copy ( s d -- ) >blk swap >blk swap 1024 move ;
: flush 32 >r begin r@ 1- write next ;
hide .2

\ MISCELLANEOUS
: >r? ( b R: a -- R: a b ) r> r> rot >r >r >r ;
: r>? ( R: a b -- b R: a ) r> r> r> nrot >r >r ;
: dump ( addr u -- ) >r begin cr ':' emit dup .x space
4 >r begin dup c@ dup .x >r? 1+ dup c@ dup .x >r? 1+ space next
8 >r begin r>? next 8 >r begin dup $20 < if drop '.' then emit
next next drop ; hide >r? hide r>?
: memused here @ $0500 - 1000 / . ;
: memfree $7c00 here @ - 1000 / . ;
: memstat memused ." kB used " memfree ." kB free" ;
hide memused hide memfree
: hello ." Welcome to TANUKI OS" cr memstat ;
page hello forget hello
: ED 2 3 thru ;   \ load in editor
: ASM 4 31 thru ; \ load in assembler
ASM
\ ED: BLOCK EDITOR
\ convert line number to addr in blk buffer
: >line ( n -- addr ) 6 lshift blk( + ;
\ get user input, skip " ok\n" prompt
: >s ( -- ) cr ." > " ['] fdusr 19 + execute ;
: \s ( -- ) in) 1+ >in ! ; \ ignore rest of interpret line
: shift-line    ( u -- ) >line dup 64 + 64 move ; \ downwards
: unshift-line  ( u -- ) >line dup 64 - 64 move ; \ upwards
: delete-line   ( u -- ) >line 64 SPC fill ;
: shift-block   ( u -- ) 14
  begin 2dup > if 2drop exit then dup shift-line 1- again ;
: unshift-block ( u -- ) 1
  begin 2dup < if 2drop exit then dup unshift-line 1+ again ;
: delete-block  ( u -- ) 1+ 15 swap
  begin 2dup < if 2drop exit then dup unshift-line 1+ again ;

\ ED: USER COMMANDS
: c  ( n -- ) >s >line in( swap 64 move \s ;    \ change
: ,c ( n -- ) begin dup c 1+ dup 15 > until ;   \ multiline
: a  ( n -- ) dup shift-block c ;               \ append
: i  ( n -- ) dup unshift-block c ;             \ insert
: d  ( n -- ) dup delete-block 15 delete-line ; \ delete
: ,d   ( -- ) blk( 1024 SPC fill ;              \ delete all
: p  ( n -- ) >line cr emitln ;                 \ print
: ,p   ( -- ) blk( display ;                    \ print all
: w    ( -- ) blk! ;                            \ write
: ed ( n -- ) blk@ ;                            \ edit

: run ( -- ) blk( line ! 16 count ! ; \ interpret blk buffer



\ ASM: ASSEMBLER
\ [pre][op][modr/m][disp][imm] instruction format
( 7 6 | 5 4 3 | 2 1 0 ) \ modr/m byte layout
( mod |  reg  |  r/m  )
: mod-mem 0 ; : mod-d8 1 ; : mod-d16 2 ; : mod-reg 3 ;
: reg-al  0 ; : reg-cl 1 ; : reg-dl  2 ; : reg-bl  3 ;
: reg-ah  4 ; : reg-ch 5 ; : reg-dh  6 ; : reg-bh  7 ;
: reg-ax  0 ; : reg-cx 1 ; : reg-dx  2 ; : reg-bx  3 ;
: reg-sp  4 ; : reg-bp 5 ; : reg-si  6 ; : reg-di  7 ;
: r/m-[bx+si] 0 ; : r/m-[bx+di] 1 ; : r/m-[bp+si] 2 ;
: r/m-[bp+di] 3 ; : r/m-[si]    4 ; : r/m-[di]    5 ;
: r/m-[bp]    6 ; : r/m-ptr     6 ; : r/m-[bx]    7 ;
: modr/m ( mod reg r/m -- modr/m )
  rot 6 lshift rot 3 lshift or or ;

\ ASM: ARGUMENTS
( 7 6 5 | 4      | 3     | 2 1 0 ) \ arg byte layout
( type  | unused | size  | r/m   ) \ args = src|dest (M|L)
: type-r8 1 ; : type-r16 2 ; : type-mem 3 ; : size-short 0 ;
: type-d8 4 ; : type-d16 5 ; : type-ptr 6 ; : size-long  1 ;

0 value args
: >args ( arg -- ) args L>M or to args ;
: src ( -- arg ) args M>L ; : dest ( -- arg ) args L>M M>L ;
: >type ( arg -- type ) 5 rshift ; : >r/m ( arg -- r/m ) 7 and ;
: >size ( arg -- size ) 3 rshift 1 and ;
\ by default, assembler assumes LONG word size
: fmt-arg ( r/m type -- arg ) 5 lshift or $08 or ;
: fmt-arg>args ( r/m type -- ) fmt-arg >args ;
: ?-r8>args  ( r/m -- ) type-r8  fmt-arg>args ;
: ?-r16>args ( r/m -- ) type-r16 fmt-arg>args ;
: ?-mem>args ( r/m -- ) type-mem fmt-arg>args ;
: ?-d8>args  ( r/m -- ) type-d8  fmt-arg>args ;
: ?-d16>args ( r/m -- ) type-d16 fmt-arg>args ;
: within ( n lo hi -- f ) rot dup nrot - 0>= nrot -^ 0>= and ;
: byte? ( n -- f ) -128 127 within ;
: ?-d?>args ( disp r/m -- disp )
  over byte? if ?-d8>args else ?-d16>args then ;

\ 8-bit register   \ 16-bit registers
: AL 0 ?-r8>args ; : AX 0 ?-r16>args ;
: CL 1 ?-r8>args ; : CX 1 ?-r16>args ;
: DL 2 ?-r8>args ; : DX 2 ?-r16>args ;
: BL 3 ?-r8>args ; : BX 3 ?-r16>args ;
: AH 4 ?-r8>args ; : SP 4 ?-r16>args ;
: CH 5 ?-r8>args ; : BP 5 ?-r16>args ;
: DH 6 ?-r8>args ; : SI 6 ?-r16>args ;
: BH 7 ?-r8>args ; : DI 7 ?-r16>args ;
\ memory dereferences (and with displacements)
: [BX+SI]  0 ?-mem>args ; : +[BX+SI] 0 ?-d?>args ;
: [BX+DI]  1 ?-mem>args ; : +[BX+DI] 1 ?-d?>args ;
: [BP+SI]  2 ?-mem>args ; : +[BP+SI] 2 ?-d?>args ;
: [BP+DI]  3 ?-mem>args ; : +[BP+DI] 3 ?-d?>args ;
: [SI]     4 ?-mem>args ; : +[SI]    4 ?-d?>args ;
: [DI]     5 ?-mem>args ; : +[DI]    5 ?-d?>args ;
: [BP]   0 6 ?-d8>args  ; : +[BP]    6 ?-d?>args ;
: [BX]     7 ?-mem>args ; : +[BX]    7 ?-d?>args ;
\ miscellaneous
: PTR 6 type-ptr fmt-arg>args ;
: SHORT $fff7 args and to args ; \ set size arg as 0
: LONG  $0008 args or  to args ; \ set size arg as 1

\ argument parsing
: r8?  >type type-r8  = ; : r16? >type type-r16 = ; : imm? 0= ;
: mem? >type type-mem = ; : d8?  >type type-d8  = ; : s src   ;
: d16? >type type-d16 = ; : ptr? >type type-ptr = ; : d dest  ;
: imm-r8?  s imm? d r8?  and ; : imm-r16? s imm? d r16? and ;
: imm-mem? s imm? d mem? and ; : imm-d8?  s imm? d d8?  and ;
: imm-d16? s imm? d d16? and ; : imm-ptr? s imm? d ptr? and ;
: r8-r8?   s r8?  d r8?  and ; : r16-r16? s r16? d r16? and ;
: r8-mem?  s r8?  d mem? and ; : r8-d8?   s r8?  d d8?  and ;
: r8-d16?  s r8?  d d16? and ; : r8-ptr?  s r8?  d ptr? and ;
: r16-mem? s r16? d mem? and ; : r16-d8?  s r16? d d8?  and ;
: r16-d16? s r16? d d16? and ; : r16-ptr? s r16? d ptr? and ;
: mem-r8?  s mem? d r8?  and ; : d8-r8?   s d8?  d r8?  and ;
: d16-r8?  s d16? d r8?  and ; : ptr-r8?  s ptr? d r8?  and ;
: mem-r16? s mem? d r16? and ; : d8-r16?  s d8?  d r16? and ;
: d16-r16? s d16? d r16? and ; : ptr-r16? s ptr? d r16? and ;
: a? >r/m 0= ; : cl? dup r8? swap >r/m 1 = and ;
: short? dest >size 0= ;

0 value op : >op to op ;
0 value /n : >/n to /n ;
0 value ex : >ex to ex ;

: modr/m, ( mod reg r/m -- ) modr/m c, ;
: ds,  ( mod -- ) d >r/m s >r/m modr/m, ;
: sd,  ( mod -- ) s >r/m d >r/m modr/m, ;
: d, ( mod n -- ) d >r/m modr/m, ;
: nd,  ( mod -- ) /n d, ;

: !exit 0 to args rdrop ;
: !abort curword type abort" : failed to assemble" ;

: ^ s d L>M or to args ;
: reg, ( op -- ) c, mod-reg sd,    ;
: mem, ( op -- ) c, mod-mem sd,    ;
: d8,  ( op -- ) c, mod-d8  sd, c, ;
: d16, ( op -- ) c, mod-d16 sd,  , ;
: ptr, ( op -- ) c, mod-mem sd,  , ;

: reg-nd, ( op -- ) c, mod-reg nd,    ;
: mem-nd, ( op -- ) c, mod-mem nd,    ;
:  d8-nd, ( op -- ) c, mod-d8  nd, c, ;
: d16-nd, ( op -- ) c, mod-d16 nd,  , ;
: ptr-nd, ( op -- ) c, mod-mem nd,  , ;

: MOV ( imm? disp? -- )
  imm-r8?  if $b0 d >r/m + c, c, !exit then
  imm-r16? if $b8 d >r/m + c,  , !exit then
  imm-mem? short? and if $c6 mem, c, !exit then
  imm-d8?  short? and if $c6  d8, c, !exit then
  imm-d16? short? and if $c6 d16, c, !exit then
  imm-mem? if $c7 mem, , !exit then
  imm-d8?  if $c7 d8,  , !exit then
  imm-d16? if $c7 d16, , !exit then
   r8-r8?  if   $88 reg, !exit then
  r16-r16? if   $89 reg, !exit then
   r8-mem? if   $88 mem, !exit then
   r8-d8?  if   $88  d8, !exit then
   r8-d16? if   $88 d16, !exit then
  r16-mem? if   $89 mem, !exit then
  r16-d8?  if   $89  d8, !exit then
  r16-d16? if   $89 d16, !exit then
  mem-r8?  if ^ $8a mem, !exit then
   d8-r8?  if ^ $8a  d8, !exit then
  d16-r8?  if ^ $8a d16, !exit then
  mem-r16? if ^ $8b mem, !exit then
   d8-r16? if ^ $8b  d8, !exit then
  d16-r16? if ^ $8b d16, !exit then
  ptr-r8?  d a? and if $a0 c, , !exit then
  ptr-r16? d a? and if $a1 c, , !exit then
   r8-ptr? s a? and if $a2 c, , !exit then
  r16-ptr? s a? and if $a3 c, , !exit then
   r8-ptr? if   $88 mem, , !exit then
  r16-ptr? if   $89 mem, , !exit then
  ptr-r8?  if ^ $8a mem, , !exit then
  ptr-r16? if ^ $8b mem, , !exit then
  !abort ;

: math, ( imm? disp? -- ) \ requires: op /n
  imm-r8?  d a? and if op 4 + c, c, !exit then
  imm-r16? d a? and if op 5 + c,  , !exit then
  imm-r8?  if $80 c, mod-reg /n d, c, !exit then \ nd, ?
  imm-r16? if $81 c, mod-reg /n d,  , !exit then \ nd, ?
  r8-r8?   if   op     reg, !exit then
  r8-mem?  if   op     mem, !exit then
  r8-d8?   if   op     d8,  !exit then
  r8-d16?  if   op     d16, !exit then
  r8-ptr?  if   op     ptr, !exit then
  r16-r16? if   op 1+  reg, !exit then
  r16-mem? if   op 1+  mem, !exit then
  r16-d8?  if   op 1+  d8,  !exit then
  r16-d16? if   op 1+  d16, !exit then
  r16-ptr? if   op 1+  ptr, !exit then
  mem-r8?  if ^ op 2+  mem, !exit then
  d8-r8?   if ^ op 2+  d8,  !exit then
  d16-r8?  if ^ op 2+  d16, !exit then
  ptr-r8?  if ^ op 2+  ptr, !exit then
  mem-r16? if ^ op 3 + mem, !exit then
  d8-r16?  if ^ op 3 + d8,  !exit then
  d16-r16? if ^ op 3 + d16, !exit then
  ptr-r16? if ^ op 3 + ptr, !exit then
  !abort ;

: ADD 0 >/n $00 >op math, ; : OR  1 >/n $08 >op math, ;
: ADC 2 >/n $10 >op math, ; : SBB 3 >/n $18 >op math, ;
: AND 4 >/n $20 >op math, ; : SUB 5 >/n $28 >op math, ;
: XOR 6 >/n $30 >op math, ; : CMP 7 >/n $38 >op math, ;

: acc, ( disp? -- )
  d r16? if op d >r/m + c, !exit then
  d r8?  if $fe reg-nd, !exit then
  d mem? if short? if $fe else $ff then mem-nd, !exit then
  d d8?  if short? if $fe else $ff then  d8-nd, !exit then
  d d16? if short? if $fe else $ff then d16-nd, !exit then
  d ptr? if short? if $fe else $ff then ptr-nd, !exit then
  !abort ;

: INC 0 >/n $40 >op acc, ;
: DEC 1 >/n $48 >op acc, ;

: stack, ( -- )
  d r16? if ex d >r/m + c, !exit then
  d mem? if op mem-nd, !exit then
  d d8?  if op  d8-nd, !exit then
  d d16? if op d16-nd, !exit then
  d ptr? if op ptr-nd, !exit then
  !abort ;

: PUSH 6 >/n $ff >op $50 >ex stack, ;
: POP  0 >/n $8f >op $58 >ex stack, ;

: gen,? ( disp? -- f ) \ required: op
   r8-r8?  if   op    reg, -1 !exit then
   r8-mem? if   op    mem, -1 !exit then
   r8-d8?  if   op     d8, -1 !exit then
   r8-d16? if   op    d16, -1 !exit then
   r8-ptr? if   op    ptr, -1 !exit then
  mem-r8?  if ^ op    mem, -1 !exit then
   d8-r8?  if ^ op     d8, -1 !exit then
  d16-r8?  if ^ op    d16, -1 !exit then
  ptr-r8?  if ^ op    ptr, -1 !exit then
  r16-r16? if   op 1+ reg, -1 !exit then
  r16-mem? if   op 1+ mem, -1 !exit then
  r16-d8?  if   op 1+  d8, -1 !exit then
  r16-d16? if   op 1+ d16, -1 !exit then
  r16-ptr? if   op 1+ ptr, -1 !exit then
  mem-r16? if ^ op 1+ mem, -1 !exit then
   d8-r16? if ^ op 1+  d8, -1 !exit then
  d16-r16? if ^ op 1+ d16, -1 !exit then
  ptr-r16? if ^ op 1+ ptr, -1 !exit then 0 ;

: NOP ( -- ) $90 c, ;

: XCHG ( disp? -- ) $86 >op
  r16-r16? s a? and if $90 d >r/m + c, !exit then
  r16-r16? d a? and if $90 s >r/m + c, !exit then
  gen,? if !exit then !abort ;

: TEST ( imm? disp? -- ) 0 >/n $84 >op
   imm-r8?  d a? and if $a8 c, c, !exit then
   imm-r16? d a? and if $a9 c,  , !exit then 
   imm-r8?  if $f6 reg-nd, c, !exit then
   imm-r16? if $f7 reg-nd,  , !exit then
   imm-mem? short? and if $f6 mem-nd, c, !exit then
   imm-d8?  short? and if $f6  d8-nd, c, !exit then
   imm-d16? short? and if $f6 d16-nd, c, !exit then
   imm-ptr? short? and if $f6 ptr-nd, c, !exit then
   imm-mem? if $f7 mem-nd, , !exit then
   imm-d8?  if $f7  d8-nd, , !exit then
   imm-d16? if $f7 d16-nd, , !exit then
   imm-ptr? if $f7 ptr-nd, , !exit then
   gen,? if !exit then !abort ;

: opp, ( disp? -- ) \ requires: /n
  d r8?  if $f6 reg-nd, !exit then
  d r16? if $f7 reg-nd, !exit then
  d mem? if short? if $f6 else $f7 then mem-nd, !exit then
  d d8?  if short? if $f6 else $f7 then  d8-nd, !exit then
  d d16? if short? if $f6 else $f7 then d16-nd, !exit then
  d ptr? if short? if $f6 else $f7 then ptr-nd, !exit then
  !abort ;

: NOT 2 >/n opp, ;
: NEG 3 >/n opp, ;

: al? ( arg -- f ) dup >type type-r8  = swap >r/m 0=  and ;
: ax? ( arg -- f ) dup >type type-r16 = swap >r/m 0=  and ;
: dx? ( arg -- f ) dup >type type-r16 = swap >r/m 2 = and ;

: IN
  s imm? d al? and if $e4 c, c, !exit then
  s imm? d ax? and if $e5 c, c, !exit then
  s dx?  d al? and if $ec c,    !exit then
  s dx?  d ax? and if $ed c,    !exit then
  !abort ;

: OUT
  s imm? d al? and if $e6 c, c, !exit then
  s imm? d ax? and if $e7 c, c, !exit then
  s al?  d dx? and if $ee c,    !exit then
  s ax?  d dx? and if $ef c,    !exit then
  !abort ;

: mul, ( disp? -- )
  d r8?  if $f6 reg-nd, !exit then
  d r16? if $f7 reg-nd, !exit then
  d mem? if short? if $f6 else $f7 then mem-nd, !exit then
  d d8?  if short? if $f6 else $f7 then  d8-nd, !exit then
  d d16? if short? if $f6 else $f7 then d16-nd, !exit then
  !abort ; 

:  MUL 4 >/n mul, ; : IMUL 5 >/n mul, ;
:  DIV 6 >/n mul, ; : IDIV 7 >/n mul, ;

: 2c, c, c, ;
: JO  $70 2c, ; : JNO $71 2c, ; : JB  $72 2c, ; : JNB $73 2c, ;
: JZ  $74 2c, ; : JNZ $75 2c, ; : JBE $76 2c, ; : JA  $77 2c, ;
: JS  $78 2c, ; : JNS $79 2c, ; : JPE $7a 2c, ; : JPO $7b 2c, ;
: JL  $7c 2c, ; : JGE $7d 2c, ; : JLE $7e 2c, ; : JG  $7f 2c, ;
: REPNZ $f2 c, ; : REPZ  $f3 c, ;
: MOVSB $a4 c, ; : MOVSW $a5 c, ; : LOOPNZ $e0 2c, ;
: CMPSB $a6 c, ; : CMPSW $a7 c, ; : LOOPZ  $e1 2c, ;
: STOSB $aa c, ; : STOSW $ab c, ; : LOOP   $e2 2c, ;
: LODSB $ac c, ; : LODSW $ad c, ; : JCXZ   $e3 2c, ;
: SCASB $ae c, ; : SCASW $af c, ; : INT    $cd 2c, ;
: CBW $98 c, ; : CWD $99 c, ; : HLT $f3 c, ; : CMC  $f4 c, ;
: CLC $f8 c, ; : STC $f9 c, ; : CLI $fa c, ; : RET  $c3 c, ; 
: STI $fb c, ; : CLD $fc c, ; : STD $fd c, ; : +RET $c2 c, , ;

: grp2, ( imm? -- )
  dup 1 = if
  imm-r8?  if drop $d0 reg-nd, !exit then
  imm-r16? if drop $d1 reg-nd, !exit then
 imm-mem? if drop short? if $d0 else $d1 then mem-nd, !exit then
  then over 1 = if
  imm-d8?  if nip short? if $d0 else $d1 then  d8-nd, !exit then
  imm-d16? if nip short? if $d0 else $d1 then d16-nd, !exit then
  imm-ptr? if nip short? if $d0 else $d1 then ptr-nd, !exit then
  then s >r/m 1 = if
  s r8? d r16? and if $d3 reg-nd, !exit then
  r8-r8?  if $d2 reg-nd, !exit then
  r8-mem? if short? if $d2 else $d3 then mem-nd, !exit then
  r8-d8?  if short? if $d2 else $d3 then  d8-nd, !exit then
  r8-d16? if short? if $d2 else $d3 then d16-nd, !exit then
  r8-ptr? if short? if $d2 else $d3 then ptr-nd, !exit then
  then !abort ;
: ROL 0 >/n grp2, ; : ROR 1 >/n grp2, ; : RCL 2 >/n grp2, ;
: RCR 3 >/n grp2, ; : SHL 4 >/n grp2, ; : SHR 5 >/n grp2, ;
: SAR 7 >/n grp2, ;

: jmp,? ( disp? -- ) \ requires: /n
  d r16? if $ff reg-nd, -1 !exit then
  d mem? if $ff mem-nd, -1 !exit then
  d d8?  if $ff  d8-nd, -1 !exit then
  d d16? if $ff d16-nd, -1 !exit then
  d ptr? if $ff ptr-nd, -1 !exit then 0 ;

: CALL ( disp? -- ) 2 >/n
  d imm? if $e8 c, , !exit then
  jmp,? if !exit then !abort ;

: JMP ( imm? disp? -- ) 4 >/n
  d imm? over byte? and if $eb c, c, !exit then
  d imm? if $e9 c, , !exit then
  jmp,? if !exit then !abort ;
