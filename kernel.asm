BITS 16
CPU 8086
org 0x0500

; ==============================================================
; FLOPPY PARAMETERS
; --------------------------------------------------------------
%define BOOTDRIVE 0x7c3e

; 360KB 5.25" double density
%define MAX_CYLINDER 40
%define MAX_HEAD 2
%define MAX_SECTOR 9

; ==============================================================
; GLOSSARY OF SHORTHANDS
; --------------------------------------------------------------
; mem	- memory
; prev	- previous
; char	- character
; addr	- address
; len	- length
; cur	- current
; in	- input
; buf	- buffer
; ptr	- pointer
; PS	- parameter stack
; RS	- return stack
; PSP	- parameter stack pointer
; RSP	- return stack pointer
; blk	- block
; usr	- user
; LSB	- least significant byte
; MSB	- most significant byte
; ==============================================================

%define BS 08 ; backspace
%define LF 10 ; line feed
%define CR 13 ; carriage feed
%define SP 32 ; space

; ==============================================================
; STACK INFORMATION
; --------------------------------------------------------------
; PS and RS occupy a sector (512 bytes) in memory from $FE00 to
; $FFFF. PSP is set to $FDFF and RSP to $0000, but they do not
; touch those address as long as an underflow does not occur.
; --------------------------------------------------------------
; PS grows "up" toward high memory.
; RS grows "down" towards low memory.
; --------------------------------------------------------------
; LOW MEMORY                                         HIGH MEMORY
; $FE00 [ ... PSP ->     (free space)     <- RSP ... ... ] $FFFF
; ==============================================================

; PUSH MACRO
%macro spush 1 ; src
	add	bp, 2
	mov	word [bp], %1
%endmacro

; POP MACRO
%macro spop 1 ; src
	mov	word %1, [bp]
	sub	bp, 2
%endmacro

%define PS0 0xfdff	; PS origin
%define RS0 0x0000	; RS origin

start:	mov bp, PS0	; initialise PS
	mov sp, RS0	; initialise RS
	jmp interpret

; --------------------------------------------------------------
; WORD STRUCTURE
; --------------------------------------------------------------

%macro wordinit 2
	db %1		; name (n bytes)
	dw 0		; link (2 bytes)
	db %2		; len  (1 byte)
	%push dict
	%$link:
%endmacro

%macro wordlink 2
	db %1		; name (n bytes)
	dw %$link	; link (2 bytes)
	db %2		; len  (1 byte)
	%pop
	%push dict
	%$link:
%endmacro		; load (n bytes)

%define FLAG 0x80	; immediate flag
%define MASK 0x7f	; word length mask

; --------------------------------------------------------------
; SYSTEM VARIABLES
; --------------------------------------------------------------

state:	db 0		; interpret (0) or compile mode (-1)
latest:	dw noop		; last defined word of the dictionary
here:	dw tail		; next avaliable space in dictionary

inptr:	dw inbuf	; currently read character in buffer
inbuf:	times 65 db 0	; input buffer
curchar:	dw 0	; last read word addr
curlen:		db 0	; last read word len

line:	dw 0x7e00	; pointer to current line
count:	dw 32		; number of lines to read

; --------------------------------------------------------------
; THE DICTIONARY - CORE
; --------------------------------------------------------------

; key ( -- c )
; fetch char c from direct input
wordinit 'key', 3
key:
	mov	ah, 0x00
	int	0x16
	spush	ax	; ah: scan code, al: ascii char
	ret

; --------------------------------------------------------------

; emit ( c -- )
; spit char c to output stream
wordlink 'emit', 4
emit:
	spop	ax
	mov	ah, 0x0e
	int	0x10
	ret

; --------------------------------------------------------------

; type ( sa sl -- )
wordlink 'type', 4
type:
	spop cx
	spop si
.loop:	lodsb
	mov ah, 0x0e
	int 0x10
	loop .loop
.done:	ret

; --------------------------------------------------------------

; abort ( -- )
; reset PS and call quit
wordlink 'abort', 5
abort:	mov bp, PS0	; reset PSP
	jmp quit

; --------------------------------------------------------------

; quit ( -- )
; return to user prompt
wordlink 'quit', 4
quit:	mov sp, RS0			; reset RSP
	mov byte [state], 0		; interpret mode
	mov word [count], 0		; set to user input
	mov word [inptr], inbuf+64	; clear input buffer
	jmp interpret

; --------------------------------------------------------------

; interpret ( -- )
; core forth loop
wordlink 'interpret', 9
interpret:
	cmp bp, PS0
	jb errstack

	; is a number?
	call rdword	; ( -- sa sl)
	call parse	; ( sa sl -- n? f )
	spop ax		; ( n? f -- n? )
	test ax, ax	
	jnz .num

	; is a word?
	call curword	; ( -- sa sl )
	call find	; ( sa sl -- w )
	mov ax, [bp]
	test ax, ax
	jnz .word

	jmp errword	; error: word not found

.num:	mov al, [state]
	test al, al	; immediate mode?
	jz interpret	; leave number in PS

	call literal	; ( n -- )
	jmp interpret

.word:	mov al, [state]
	test al, al	; immediate mode?
	jz .run		; execute

	; immediate word?
	mov bx, [bp]		; addr of word
	mov al, [bx - 1]	; fetch byte length+flag
	and al, FLAG		; test immediate flag
	test al, al
	jnz .run

	; compile word
	call compile		; ( w -- )
	jmp interpret

.run:	call execute
	jmp interpret

; --------------------------------------------------------------
; ERROR MESSAGES
; --------------------------------------------------------------

; abort with "<this> word not found"
errword: call cr
	call curword
	call type
	spush .err
	spush 15
	call type
	jmp abort
.err:	db ' word not found'

; abort with "stack out of bounds"
errstack:
	spush .err
	spush 21
	call type
	jmp abort
.err:	db 0x0d, 0x0a, 'stack out of bounds'

; --------------------------------------------------------------

; word ( -- sa sl )
; read one word from input buffer
wordlink 'word', 4
rdword:	call rdchar	; ( -- c )
	spop ax		; ( c -- )
	cmp ax, SP	; whitespace character?
	jbe rdword	; skip until non-whitespace char read
	mov ax, [inptr]
	dec ax
	spush ax	; ( -- sa )
	mov [curchar], ax	; update current word addr
	xor bx, bx

.loop:	inc bx
	call rdchar
	spop ax
	cmp ax, SP
	ja .loop

	spush bx	; ( -- sa sl )
	mov [curlen], bl
	ret

; --------------------------------------------------------------

; curword ( -- sa sl )
; fetch the last read word
wordlink 'curword', 7
curword:
	mov ax, [curchar]
	spush ax
	mov al, [curlen]
	xor ah, ah
	spush ax
	ret

; --------------------------------------------------------------

; rdchar ( -- c )
; read one char from input buffer
wordlink 'char', 4
rdchar:	mov si, [inptr]
	cmp si, inbuf+64	; end of line?
	jbe .skip
	call fdln		; feed new line
	mov si, inbuf
.skip:	lodsb			; al = char
	xor ah, ah
	spush ax
	mov word [inptr], si
	ret

; --------------------------------------------------------------

; fdln ( -- )
; "read line", feed a line to inbuf
wordlink 'fdln', 4
fdln:	mov ax, [count]		; number of lines left to read?
	test ax, ax
	jz fdusr		; read line from user input
	dec ax
	mov [count], ax
	jmp fdblk		; read line from block memory

; --------------------------------------------------------------

; fdusr ( -- )
; feed a line from user input to inbuf
fdusr:	spush .ok
	spush 5
	call type

	mov di, inbuf
.next	cmp di, inbuf+64	; buffer full?
	je .eol			; ignore input

	; pool key from user
	mov ah, 0x00
	int 0x16

	cmp al, CR		; enter?
	je .cr			; submit line

	cmp al, BS		; backspace?
	je .bs			; delete key

	cmp al, SP		; control character?
	jb .next		; do nothing

	stosb			; store byte in al via di

	mov ah, 0x0e		; emit character
	int 0x10
	jmp .next

.eol:	mov ah, 0x00
	int 0x16

	cmp al, CR
	je .cr

	cmp al, BS
	je .bs

	jmp .next

.bs:	cmp di, inbuf		; empty buffer?
	je .next		; do nothing

	dec di			; clear prev char in memory
	mov byte [di], 0

	spush .del		; clear printed char
	spush 3
	call type
	jmp .next

.cr:	call space		; visually pad input with space
	xor al, al		; fill rest of inbuf with null
.nul:	stosb
	cmp di, inbuf+64
	jbe .nul
	ret

.ok:	db ' ok', 13, 10
.del:	db BS, SP, BS

; --------------------------------------------------------------

; fdblk ( -- )
; feed a line from block memory to inbuf
wordlink 'fdblk', 5
fdblk:	mov word si, [line]
	mov di, inbuf
	mov cx, 64
	rep movsb
	mov word [line], si	; progress 'line' to next line
	ret			; in block memory

; --------------------------------------------------------------

; parse ( sa sl -- n? f )
; interpret string as a number
wordlink 'parse', 5
parse:		
	spop cx
	spop si
	xor ax, ax	; char read
	xor bx, bx	; result

	lodsb
	dec cx
	cmp al, "'"
	je .char
	cmp al, '$'
	je .hex	
	cmp al, '-'
	je .neg	
	inc cx
	jmp .skip
	
	; character literal
.char:	cmp cx, 2
	jne .nan
	lodsb
	mov bl, al
	lodsb
	cmp al, "'"
	jne .nan
	jmp .num

	; hexadecimal
.hex:	lodsb
	add bx, bx	; multiply by 16
	add bx, bx
	add bx, bx
	add bx, bx
	cmp al, '0'
	jb .nan
	cmp al, '9'
	jbe .digit
	cmp al, 'a'
	jb .nan
	cmp al, 'f'
	jbe .alpha
	jmp .nan
.digit: sub al, '0'
	jmp .add
.alpha: sub al, 'a'-10
.add:	add bx, ax
	loop .hex
	jmp .num

	; negative decimal
.neg:	lodsb
	; mul bx, 10 -- not a 8086 cpu instruction
	add bx, bx
	mov dx, bx
	add bx, bx
	add bx, bx
	add bx, dx
	sub al, '0'
	jb .nan
	cmp al, 10
	jae .nan
	sub bx, ax
	loop .neg
	jmp .num

	; unsigned decimal
.pos:	lodsb
	; mul bx, 10
	add bx, bx
	mov dx, bx
	add bx, bx
	add bx, bx
	add bx, dx
.skip:	sub al, '0'
	jb .nan
	cmp al, 10
	jae .nan
	add bx, ax
	loop .pos

	; is a number
.num:	spush	bx
	spush	-1
	ret

	; not a number
.nan:	spush	0
	ret

; --------------------------------------------------------------

; literal ( n -- )
; compiile n as a literal
wordlink 'literal', 7
literal:
	mov word di, [here]
	mov al, 0xe8
	stosb			; call opcode
	mov ax, .lit
	sub ax, di
	sub ax, 2
	stosw			; rel16 addr of lit
	spop ax
	stosw			; literal value
	mov word [here], di
	ret

; lit ( -- n ) runtime
.lit:	pop bx
	mov ax, [bx]
	spush ax
	add bx, 2		; skip literal (2 bytes)
	jmp bx			; return

; --------------------------------------------------------------
; (before)		(after)
; ... <-- here		...
;			call		<-- 1 byte
;			rel16		<-- 2 bytes
;			number		<-- 2 bytes, lit value
;			...		<-- here
; --------------------------------------------------------------

; find ( sa sl -- w )
; search for counted string in dictionary
wordlink 'find', 4
find:
	spop ax
	spop dx
	mov bx, [latest]

	; iterate through dictionary
.loop:	mov cl, [bx-1]	; fetch name length
	and cl, 0x7f	; ignore immediate flag
	cmp al, cl	; same length?
	jne .skip

	mov si, dx	; si = addr of curword
	mov di, bx
	sub di, 3
	sub di, ax	; di = addr of matching word
	mov cx, ax	; cx = length of matching word
	repz cmpsb
	jz .found

.skip:	mov bx, [bx-3]	; check next word in dictionary
	test bx, bx	; end of dictionary?
	jnz .loop

.found: spush bx
	ret		; ( -- w )

; --------------------------------------------------------------

; execute ( w -- )
; jump to addr w
wordlink 'execute', 7
execute:
	spop ax
	jmp ax

; --------------------------------------------------------------

; compile ( w -- )
; compile a call to addr w
wordlink 'compile,', 8
compile:
	mov di, [here]
	mov al, 0xe8
	stosb			; compile call
	spop ax			; rel16 = wordref - here - 2
	sub ax, di
	sub ax, 2
	stosw			; compile rel16
	mov [here], di
	ret

; --------------------------------------------------------------
; (before)		(after)
; ... <-- here		...
;			call
;			rel16	<-- relative address (2 bytes)
;			...	<-- here
; ==============================================================
; SYSTEM VARIABLES
; --------------------------------------------------------------

; line ( -- addr )
wordlink 'line', 4
var_line:
	spush line
	ret
; --------------------------------------------------------------

; count ( -- addr )
wordlink 'count', 5
var_count:
	spush count
	ret
; --------------------------------------------------------------

; latest ( -- addr )
wordlink 'latest', 6
var_latest:
	spush latest
	ret
; --------------------------------------------------------------

; here ( -- addr )
wordlink 'here', 4
var_here:
	spush here
	ret
; --------------------------------------------------------------

; inbuf ( -- addr )
wordlink 'inbuf', 5
var_inbuf:
	spush inbuf
	ret
; --------------------------------------------------------------

; >in ( -- addr )
wordlink '>in', 3
var_inptr:
	spush inptr
	ret

; --------------------------------------------------------------
; # DEFINING WORDS
; --------------------------------------------------------------

; header ( sa sl -- )
; append a new header with name s
wordlink 'header', 6
header:	spop bx
	mov cx, bx
	spop si
	mov di, [here]
	rep movsb		; store name
	mov ax, [latest]
	stosw			; store link
	mov al, bl
	stosb			; store len+flag
	mov [latest], di	; update latest
	mov [here], di		; update here
	ret

; --------------------------------------------------------------
; (before)		(after)
; ... <-- here		...
;			name		<-- x bytes
;			link		<-- 2 bytes
;			len + flag	<-- 1 byte
;			...		<-- here
; --------------------------------------------------------------

; [ ( -- ) *I*
; to immediate mode
wordlink '[', 1 | FLAG
to_immediate:
	mov	byte [state], 0
	ret

; --------------------------------------------------------------

; ] ( -- )
; to compile mode
wordlink ']', 1
to_compile:
	mov	byte [state], -1
	ret

; --------------------------------------------------------------

; exit ( -- )
; compile exit from a word
wordlink 'exit', 4 | FLAG
exit:
	mov	di, [here]
	mov	al, 0xc3 ; ret
	stosb
	mov	[here], di
	ret

; --------------------------------------------------------------

; immediate ( -- )
; make latest word an immediate word
wordlink 'immediate', 9 | FLAG
immediate:
	mov bx, [latest]
	dec bx
	mov al, [bx]
	xor al, FLAG
	mov [bx], al
	ret

; --------------------------------------------------------------

; : ... ( -- )
; create a new word definition with name ...
wordlink ':', 1
colon:	call	rdword		; ( -- sa sl )
	call	header		; ( sa sl -- )
	call	to_compile
	ret

; --------------------------------------------------------------

; ; ( -- )
; end current word definition
wordlink ';', 1 | FLAG
semicolon:
	call exit		; compile an exit from a word
	call to_immediate	; set state to immediate mode
	ret

; --------------------------------------------------------------

; create x ( -- ), run-time: ( -- addr )
; append a new header with name x
wordlink 'create', 6
create:	call rdword		; ( -- sa sl )
	call header		; ( sa sl -- )
	mov ax, [here]
	add ax, 6
	spush ax
	call literal		; ( addr -- )
	call exit
	ret

; --------------------------------------------------------------
; (before)		(after)
; ... <-- here		...
;			name		<-- x bytes
;			link		<-- 2 bytes
;			len + flag	<-- 1 byte
;			call		<-- 1 byte
;			lit		<-- 2 bytes
;			addr		<-- 2 bytes
;			ret		<-- 1 byte
;			...		<-- addr points to here
; --------------------------------------------------------------

; [compile] x ( -- ) *I*
wordlink '[compile]', 9 | FLAG
compile_immediate:
	call rdword	; ( -- sa sl )
	call find	; ( sa sl -- w )
	mov ax, [bp]
	test ax, ax
	jz errword	; abort with "<this> word not found"
	call compile	; ( w -- )
	ret

; --------------------------------------------------------------
; TO TEST
; --------------------------------------------------------------

; metacompile x ( -- ) *I*
; write wordref as a literal, such that
; it compiles x when executed
wordlink 'compile', 7 | FLAG
metacompile:
	call rdword	; ( -- sa sl )
	call find	; ( sa sl -- w )
	mov ax, [bp]
	test ax, ax
	jz errword	; abort with "<this> word not found"
	call literal	; ( w -- )
	spush compile	; ( -- w )
	call compile	; ( w -- )
	ret

; --------------------------------------------------------------

; value x ( n -- )
; create cell x that returns its value
wordlink 'value', 5
value:	call rdword
	call header
	call literal
	call exit
	ret

; ==============================================================
; # ENTRY MANAGEMENT
; --------------------------------------------------------------

; ' x ( -- w )
; find x in dictionary, return w if found and null otherwise
wordlink "'", 1
tick:	call rdword
	call find
	ret 

; ['] x ( -- ) *I*
; like ', but compile w as a literal
wordlink "[']", 3 | FLAG
tick_immediate:
	call tick
	call literal
	ret

; forget x ( -- )
; rewind dictionary up to x's previous entry
wordlink 'forget', 6
forget:	call tick
	spop bx
	test bx, bx
	jz errword
	mov ax, [bx - 3]	; link to previous entry
	mov [latest], ax
	mov al, [bx - 1]
	and al, MASK
	xor ah, ah
	sub bx, ax
	sub bx, 3
	mov [here], bx
	ret

; hide x ( -- )
wordlink 'hide', 4
hide:	call tick
	spop bx
	test bx, bx
	jz errword
	mov [bx - 1], 0
	ret

; --------------------------------------------------------------
; # FLOW
; --------------------------------------------------------------

; SAVE A BYTE, REMOVE REL16 JMP?

; compile-time ( -- addr )
; run-time ( -- )
wordlink 'if', 2 | FLAG
if:	spush .if
	call compile
	mov di, [here]
	mov al, 0xe9
	stosb
	spush di
	add di, 2
	mov [here], di
	ret

.if:	spop ax
	test ax, ax
	jz .jmp
	pop ax
	add ax, 3
	jmp ax
.jmp:	ret

; --------------------------------------------------------------
; (before)		(after)
; ... <-- here		...
;			call	<-- 1 byte
;			.if	<-- 2 bytes
;			rel16	<-- 2 bytes
;			...	<-- here, start of if-case
; --------------------------------------------------------------
                                                                 
; compile-time ( addr1 -- addr2 )
; run-time ( -- )
wordlink 'else', 4 | FLAG
else:	mov di, [here]
	mov al, 0xe9
	stosb
	spush di
	add di, 2
	mov [here], di
	call swap
	jmp then

; --------------------------------------------------------------
; (before)		(after)
; ... <-- here		...	<-- end of if-case
;			jmp	<-- 1 byte
;			rel16	<-- 2 bytes
;			...	<-- here, start of else-case
; --------------------------------------------------------------

; compile-time ( addr -- )
; run-time ( -- )
wordlink 'then', 4 | FLAG
then:	spop bx
	mov ax, [here]
	sub ax, bx
	sub ax, 2
	mov [bx], ax
	ret

; --------------------------------------------------------------
; (before)		(after)
; ... <-- here		...	<-- here
; --------------------------------------------------------------

; compile-time	( -- a )
; run-time	( -- )
wordlink 'begin', 5 | FLAG
begin:	mov ax, [here]
	spush ax
	ret

; --------------------------------------------------------------

; compile-time	( a -- )
; run-time	( -- )
wordlink 'again', 5 | FLAG
again:	mov di, [here]
	mov al, 0xe9
	stosb			; jmp
	spop ax
	sub ax, di
	sub ax, 2
	stosw			; rel16
	mov [here], di
	ret

; --------------------------------------------------------------

; compile-time	( a -- )
; run-time	( f -- )
wordlink 'until', 5 | FLAG
until:	spush .until
	call compile
	jmp again

.until:	spop ax
	test ax, ax
	jz .loop
	pop ax		; skip jmp rel16 (3 bytes)
	add ax, 3
	jmp ax
.loop	ret

; --------------------------------------------------------------

; compile-time	( a -- )
; run-time	( R: n -- )
wordlink 'next', 4 | FLAG
next:	spush .next
	call compile
	jmp again

.next:	pop	bx
	pop	ax
	dec	ax
	push	ax
	test	ax, ax
	jnz	.loop
	pop	ax
	add	bx, 3
.loop:	jmp	bx

; --------------------------------------------------------------

; leave ( R:x n -- R:x 1 )
; in a begin-next loop, exit at the next jump
wordlink 'leave', 5
leave:	pop	bx
	pop	ax
	mov	ax, 1
	push	ax
	jmp	bx

; --------------------------------------------------------------

; recurse ( -- )
; for recursive word definitions
wordlink 'recurse', 7 | FLAG
recurse:
	mov di, [here]
	mov al, 0xe9
	stosb
	mov ax, [latest]
	mov bx, di ; [here]
	sub ax, bx
	sub ax, 2
	stosw
	mov [here], di
	ret

; --------------------------------------------------------------

; ( ( -- ) *I*
; ignore input until ')' is read
wordlink '(', 1 | FLAG
paren:	call	rdchar
	spop	ax
	cmp	ax, ')'
	jne	paren
	ret

; --------------------------------------------------------------

; \ ( -- ) *I*
; ignore input until end of line
wordlink '\', 1 | FLAG
backslash:
	mov word [inptr], inbuf+64
	ret

; --------------------------------------------------------------

; abort" ..." ( -- ) *I*
; compiles ." followed by abort
wordlink 'abort"', 6 | FLAG
abort_quote:
	call dot_quote
	spush abort
	call compile
	ret

; --------------------------------------------------------------
; PARAMETER STACK
; --------------------------------------------------------------

; drop ( a -- )
wordlink 'drop', 4
drop:	sub bp, 2
	ret

; --------------------------------------------------------------

; dup ( a -- a a )
wordlink 'dup', 3
dup:	mov ax, [bp]
	spush ax
	ret

; --------------------------------------------------------------

; ?dup ( a -- ?a a )
wordlink '?dup', 4
?dup:	mov ax, [bp]
	test ax, ax
	jz .zero
	spush ax
.zero:	ret

; --------------------------------------------------------------

; nip ( a b -- b )
wordlink 'nip', 3
nip:	spop ax
	mov [bp], ax
	ret

; --------------------------------------------------------------

; over ( a b -- a b a )
wordlink 'over', 4
over:	mov ax, [bp - 2]
	spush ax
	ret

; --------------------------------------------------------------

; rot ( a b c -- b c a )
wordlink 'rot', 3
rot:	mov ax, [bp - 4]
	mov bx, [bp - 2]
	mov cx, [bp]
	mov [bp - 4], bx
	mov [bp - 2], cx
	mov [bp], ax
	ret

; --------------------------------------------------------------

; nrot ( a b c -- c a b )
wordlink 'nrot', 4
nrot:	mov ax, [bp - 4]
	mov bx, [bp - 2]
	mov cx, [bp]
	mov [bp - 4], cx
	mov [bp - 2], ax
	mov [bp], bx
	ret

; --------------------------------------------------------------

; swap ( a b -- b a )
wordlink 'swap', 4
swap:	mov ax, [bp - 2]
	mov bx, [bp]
	mov [bp - 2], bx
	mov [bp], ax
	ret

; --------------------------------------------------------------

; tuck ( a b -- b a b )
wordlink 'tuck', 4
tuck:	mov ax, [bp - 2]
	mov bx, [bp]
	mov [bp - 2], bx
	mov [bp], ax
	spush bx
	ret

; --------------------------------------------------------------

; 2drop ( a a -- )
wordlink '2drop', 5
two_drop:
	sub bp, 4
	ret

; --------------------------------------------------------------

; 2dup ( a b -- a b a b )
wordlink '2dup', 4
two_dup:
	mov ax, [bp - 2]
	mov bx, [bp]
	add bp, 4
	mov [bp - 2], ax
	mov [bp], bx
	ret

; --------------------------------------------------------------
; RETURN STACK
; --------------------------------------------------------------

; >r ( n -- R:n )
wordlink '>r', 2
to_r:	spop ax
	pop bx
	push ax
	jmp bx

; --------------------------------------------------------------

; r> ( R:n -- n )
wordlink 'r>', 2
r_from:	pop bx
	pop ax
	spush ax
	jmp bx

; --------------------------------------------------------------

; r@ ( R:n -- n R:n )
wordlink 'r@', 2
r_fetch:
	mov bx, sp
	mov bx, [bx + 2]
	spush bx
	ret

; --------------------------------------------------------------

; rdrop ( R:n -- )
wordlink 'rdrop', 5
rdrop:
	pop bx
	pop ax
	jmp bx

; --------------------------------------------------------------
; STACKS META
; --------------------------------------------------------------

; .<> ( n -- ) KERNEL ONLY
; emit n as decimal within arrow brackets
dot_bracket:
	spush '<'
	call emit
	call dot
	spush '>'
	call emit
	ret

; --------------------------------------------------------------

; .s ( -- )
; prints contents of PS: "<depth> content (decimal)"
wordlink '.s', 2
dot_s:	call depth
	call dup
	call dot_bracket
	spop cx
	test cx, cx
	jz .done

	mov si, PS0 + 2
.next:	lodsw
	spush ax
	call space
	call dot
	loop .next

.done:	ret

; --------------------------------------------------------------

; .r ( -- )
; prints contents of RS: "<depth> content (hex)"
wordlink '.r', 2
dot_r:	call rdepth
	call dup
	call dot_bracket
	spop cx
	test cx, cx
	jz .done

	std
	mov si, RS0 - 2
.next:	lodsw
	spush ax
	call space
	push cx		; cx gets disturbed by "shr ax, cx"
	call dot_word
	pop cx
	loop .next
	cld

.done:	ret

; --------------------------------------------------------------

; depth ( -- n )
; size of PS by cell count
wordlink 'depth', 5
depth:	mov ax, PS0
	sub ax, bp
	neg ax
	shr ax, 1
	spush ax
	ret

; --------------------------------------------------------------

; rdepth ( -- n )
; size of RS by cell count
wordlink 'rdepth', 6
rdepth:	mov ax, RS0
	sub ax, sp
	shr ax, 1
	spush ax
	ret

; ==============================================================
; MEMORY
; --------------------------------------------------------------

; ( addr - n )
; fetch value stored at addr a
wordlink '@', 1
fetch:	mov bx, [bp]
	mov bx, [bx]
	mov [bp], bx
	ret

; --------------------------------------------------------------

; ( n addr -- )
; store n at addr
wordlink '!', 1
store:	spop bx
	spop ax
	mov [bx], ax
	ret

; --------------------------------------------------------------

; ( n addr -- ) 
; increase value at addr by n
wordlink '+!', 2
plus_store:
	spop bx
	spop ax
	add [bx], ax
	ret

; --------------------------------------------------------------

; , ( n -- )
; write n in here and advance it
wordlink ',', 1
comma:	spop ax
	mov di, [here]
	stosw
	mov [here], di
	ret

; --------------------------------------------------------------

; c@ ( addr -- char )
; fetch byte at addr
wordlink 'c@', 2
c_fetch:
	mov bx, [bp]
	mov bl, [bx]
	xor bh, bh
	mov [bp], bx
	ret

; --------------------------------------------------------------

; c@+ ( addr -- addr+1 char )
; fetch byte from addr and incr a
wordlink 'c@+', 3
c_fetch_plus:
	mov bx, [bp]
	mov al, [bx]
	xor ah, ah
	inc bx
	mov [bp], bx
	spush ax
	ret

; --------------------------------------------------------------

; c! ( char addr -- )
; store byte at addr
wordlink 'c!', 2
c_store:
	spop bx
	spop ax
	mov [bx], al
	ret

; --------------------------------------------------------------

; c!+ ( char addr -- addr+1 )
; store byte in addr and incr addr
wordlink 'c!+', 3
c_store_plus:
	spop bx
	mov ax, [bp]
	mov [bx], al
	inc bx
	mov [bp], bx
	ret

; --------------------------------------------------------------

; c, ( char -- )
; write byte to here and incr here by 1
wordlink 'c,', 2
c_comma:
	spop ax
	mov bx, [here]
	mov [bx], al
	inc bx
	mov [here], bx
	ret

; -------------------------------------------------------------- 

; compare ( addr1 addr2 u -- f )
; compare u bytes between addr1 addr2, return true if equal
wordlink 'compare', 7
compare:
	spop cx
	spop si
	spop di
	xor ax, ax	; flag
	repe cmpsb
	jnz .false
	not ax
.false:	spush ax
	ret

; --------------------------------------------------------------

; allot ( n -- )
; move here by n bytes
wordlink 'allot', 5
allot:	spop ax
	mov bx, [here]
	add bx, ax
	mov [here], bx
	ret

; --------------------------------------------------------------

; fill ( addr u char -- )
; fill u bytes from addr with char
wordlink 'fill', 4
fill:	spop ax		; char
	spop cx		; count
	spop di		; destination
	rep stosb
	ret

; --------------------------------------------------------------

; move ( addr1 addr2 u -- )
; copu u bytes from addr1 to addr2
wordlink 'move', 4
move:	spop cx		; count
	spop di		; destination
	spop si		; source
	rep movsb
	ret

; --------------------------------------------------------------

; to x ( n -- )
; set x to value n, see word 'value'
wordlink 'to', 2 | FLAG
to_value:
	call rdword		; ( n? -- n? sa sl )
	call find		; ( n? sa sl -- n? w )
	mov ax, [bp]
	test ax, ax
	jz errword

	; immediate or compile mode?
	mov al, [state]
	test al, al
	jz .imm

	call literal		; ( w -- )
	spush .imm		; ( -- w )
	call compile		; ( w -- )
	ret

.imm:	spop bx			; ( n w -- n )
	spop ax			; ( n -- )
	add bx, 3
	mov [bx], ax
	ret

; --------------------------------------------------------------
; (structure of a "value" word)
; ...
; name		<-- n bytes
; link		<-- 2 bytes
; len+flag	<-- 1 byte
; call lit	<-- 3 bytes
; n		<-- 2 bytes	"to" word reassigns this value
; ret		<-- 1 byte
; ...
; --------------------------------------------------------------
; ARITHMETIC / BITS
; --------------------------------------------------------------

; + ( a b -- a+b )
wordlink '+', 1
plus:	spop bx
	mov ax, [bp]
	add ax, bx
	mov [bp], ax
	ret

; --------------------------------------------------------------

; - ( a b -- a-b )
wordlink '-', 1
minus:	spop bx
	mov ax, [bp]
	sub ax, bx
	mov [bp], ax
	ret

; --------------------------------------------------------------

; -^ ( a b -- b-a )
wordlink '-^', 2
minus_inverse:
	spop bx
	mov ax, [bp]
	sub bx, ax
	mov [bp], bx
	ret

; --------------------------------------------------------------

; * ( a b -- a*b )
wordlink '*', 1
mul:	spop bx
	mov ax, [bp]
	mul bx
	mov [bp], ax
	ret

; --------------------------------------------------------------

; / ( a b -- a/b )
wordlink '/', 1
div:	spop bx
	mov ax, [bp]
	xor dx, dx
	div bx
	mov [bp], ax
	ret

; --------------------------------------------------------------

; 1+ ( n -- n+1 )
wordlink '1+', 2
incr:	mov ax, [bp]
	inc ax
	mov [bp], ax
	ret

; --------------------------------------------------------------

; 1- ( n -- n-1 )
wordlink '1-', 2
decr:	mov ax, [bp]
	dec ax
	mov [bp], ax
	ret

; --------------------------------------------------------------

; 2+ ( n -- n+2 )
wordlink '2+', 2
incr2:	mov ax, [bp]
	add ax, 2
	mov [bp], ax
	ret

; --------------------------------------------------------------

; 2- ( n -- n-2 )
wordlink '2-', 2
decr2:	mov ax, [bp]
	sub ax, 2
	mov [bp], ax
	ret

; --------------------------------------------------------------

; 2* ( n -- n*2 )
wordlink '2*', 2
shiftl:	mov ax, [bp]
	add ax, ax
	mov [bp], ax
	ret

; --------------------------------------------------------------

; 2/ ( n -- n/2 )
wordlink '2/', 2
shiftr:	mov ax, [bp]
	shr ax, 1
	mov [bp], ax
	ret

; --------------------------------------------------------------

; L>M ( n -- n<<8 )
wordlink 'L>M', 3
shiftl8:
	mov ax, [bp]
	mov ah, al
	xor al, al
	mov [bp], ax
	ret

; --------------------------------------------------------------

; M>L ( n -- n>>8 )
wordlink 'M>L', 3
shiftr8:
	mov ax, [bp]
	mov al, ah
	xor ah, ah
	mov [bp], ax
	ret

; --------------------------------------------------------------

; L|M ( n -- L|M )
wordlink 'L|M', 3
bytesplit:
	mov bx, [bp]
	xor ah, ah
	mov al, bl
	mov [bp], ax
	mov al, bh
	spush ax
	ret

; --------------------------------------------------------------

; sort ( n1 n2 -- low high )
wordlink 'sort', 4
sort:	mov ax, [bp - 2]
	mov bx, [bp]
	cmp ax, bx
	jle .done
	mov [bp - 2], bx
	mov [bp], ax
.done	ret

; --------------------------------------------------------------

; max ( n1 n2 -- high )
wordlink 'max', 3
max:	spop bx
	mov ax, [bp]
	cmp bx, ax
	jle .done
	mov [bp], bx
.done	ret

; --------------------------------------------------------------

; min ( n1 n2 -- low )
wordlink 'min', 3
min:	spop bx
	mov ax, [bp]
	cmp ax, bx
	jle .done
	mov [bp], bx
.done	ret

; --------------------------------------------------------------

; mod ( a b -- a%b )
wordlink 'mod', 3
mod:	spop bx
	mov ax, [bp]
	xor dx, dx
	div bx
	mov [bp], dx
	ret

; --------------------------------------------------------------

; divmod ( a b -- rem quot )
wordlink '/mod', 4
divmod:	mov ax, [bp - 2]
	mov bx, [bp]
	xor dx, dx
	div bx
	mov [bp - 2], dx
	mov [bp], ax
	ret

; --------------------------------------------------------------

; not ( a -- ~a )
wordlink 'not', 3
not:	not word [bp]
	ret

; --------------------------------------------------------------

; and ( a b -- a&b )
wordlink 'and', 3
and:	spop ax
	and word [bp], ax
	ret

; --------------------------------------------------------------

; or ( a b -- a|b )
wordlink 'or', 2
or:	spop ax
	or word [bp], ax
	ret

; --------------------------------------------------------------

; xor ( a b -- a^b )
wordlink 'xor', 3
xor:	spop ax
	xor word [bp], ax
	ret

; --------------------------------------------------------------

; lshift ( n u -- n<<u )
wordlink 'lshift', 6
lshift:	spop cx
	shl word [bp], cx
	ret

; --------------------------------------------------------------

; rshift ( n u -- n>>u )
wordlink 'rshift', 6
rshift:	spop cx
	shr word [bp], cx
	ret

; --------------------------------------------------------------
; LOGIC
; --------------------------------------------------------------

; = ( n1 n2 -- f )
wordlink '=', 1
equ:	spop ax
	xor ax, [bp]
	neg ax
	sbb ax, ax
	not ax
	mov [bp], ax
	ret

; --------------------------------------------------------------

; < ( n1 n2 -- f )
wordlink '<', 1
lt:	spop bx
	xor ax, ax
	cmp bx, [bp]
	jle .done
	not ax
.done:	mov [bp], ax
	ret

; --------------------------------------------------------------

; > ( n1 n2 -- f )
wordlink '>', 1
gt:	spop bx
	xor ax, ax
	cmp bx, [bp]
	jge .done
	not ax
.done:	mov [bp], ax
	ret

; --------------------------------------------------------------

; <= ( n1 n2 -- f )
wordlink '<=', 2
lte:	spop bx
	xor ax, ax
	cmp bx, [bp]
	jl .done
	not ax
.done:	mov [bp], ax
	ret

; --------------------------------------------------------------

; >= ( n1 n2 -- f )
wordlink '>=', 2
gte:	spop bx
	xor ax, ax
	cmp bx, [bp]
	jg .done
	not ax
.done:	mov [bp], ax
	ret

; --------------------------------------------------------------

; 0< ( n -- f )
wordlink '0<', 2
zlt:	mov ax, [bp]
	cwd
	mov [bp], dx
	ret

; --------------------------------------------------------------

; 0>= ( n -- f )
wordlink '0>=', 3
zge:	mov ax, [bp]
	cwd
	not dx
	mov [bp], dx
	ret

; --------------------------------------------------------------

; 0= ( n -- f )
wordlink '0=', 2
zeq:	mov ax, [bp]
	neg ax
	sbb ax, ax
	not ax
	mov [bp], ax
	ret

; ==============================================================
; STRINGS AND LINES
; --------------------------------------------------------------

; s" ..." ( -- ) compile-time 
; ( -- sa sl ) run-time
; compile ... as a counted string to word
wordlink 's"', 2 | FLAG
squote:	mov di, [here]
	mov al, 0xe9
	stosb
	mov ax, di
	add di, 2
	spush di
	spush ax
	xor cx, cx

.next:	call rdchar	; register use: ax, si
	spop ax		; s" breaks upon fdln (feed line)
	cmp al, 34
	je .done
	stosb
	inc cx
	jmp .next

.done	spop bx
	mov ax, di
	sub ax, bx
	sub ax, 2
	mov [bx], ax
	mov [here], di
	call literal
	spush cx
	call literal
	ret

; --------------------------------------------------------------

; s= ( sa1 sl1 sa2 sl2 -- f )
; return whether string s1 == s2
wordlink 's=', 2
s_equal:
	call rot	; ( sa1 sa2 sl2 sl1 )
	call over	; ( sa1 sa2 sl2 sl1 sl2 )
	call equ	; ( sa1 sa2 sl2 f )
	spop ax		; ( sa1 sa2 sl2 )
	test ax, ax
	jz .false
	call compare	; ( f )
	ret
.false:	sub bp, 4		; drop drop
	mov word [bp], 0	; ( 0 )
	ret

; ==============================================================
; NUMBER FORMATTING
; --------------------------------------------------------------

; u. ( n -- )
; print n in its unsigned decimal form
wordlink 'u.', 2
udot:	spop ax
digit:	xor dx, dx
	mov bx, 10
	div bx
	test ax, ax
	jz .zero
	push dx
	call digit

	pop dx
.zero	push ax
	mov al, dl
	add al, '0'
	spush ax
	call emit
	pop ax
	ret

; --------------------------------------------------------------

; . ( n -- )
; print n in its signed decimal form
wordlink '.', 1
dot:	spop ax
	test ax, ax	; signed?
	jns digit	; unsigned
	neg ax
	push ax
	spush '-'
	call emit
	pop ax
	jmp digit

; --------------------------------------------------------------

; ( n -- )
; print n's LSB in 2-wide hex format
wordlink '.x', 2
dot_byte:
	call dup
	spush 4
	call rshift
	call .print
	call .print
	ret

.print:	spop ax
	and al, 0x0f ; mask for LSB
	cmp al, 10
	jb .digit
	add al, 'a'-'0'-10
.digit:	add al, '0'
	mov ah, 0x0e
	int 0x10
	ret

; --------------------------------------------------------------

; ( n -- )
; print n in 4-wide hex format
wordlink '.X', 2
dot_word:
	mov ax, [bp]
	mov al, ah
	spush ax
	call dot_byte
	call dot_byte
	ret

; ==============================================================
; I/O
; --------------------------------------------------------------

; ..." ( -- )
; write ... to here
wordlink ',"', 2 | FLAG
comma_quote:
	mov di, [here]
.next:	call rdchar
	spop ax
	cmp ax, '"'
	je .done
	stosb
	jmp .next
.done:	mov [here], di
	ret

; --------------------------------------------------------------

; ." ..." ( -- )
; print ... during runtime
wordlink '."', 2 | FLAG
dot_quote:
	call squote
	spush type	; metacompile "type"
	call compile
	ret

; --------------------------------------------------------------

; ( addr -- )
; emit line at addr a
wordlink 'emitln', 6
emitln:	spush 64
	call type
	ret

; --------------------------------------------------------------

; ( -- c? f )
; polls the keyboard for a key
wordlink 'key?', 4
key?:	mov ah, 0x01
	int 0x16
	jz .nokey

	mov ah, 0x00	; clear key from input buffer
	int 0x16
	spush ax
	spush -1
	ret

.nokey	spush 0
	ret

; --------------------------------------------------------------

; cr ( - )
; emit new line
wordlink 'cr', 2
cr:
	spush .cr
	spush 2
	call type
	ret
.cr	db CR, LF

; --------------------------------------------------------------

; space ( -- )
; emit a space
wordlink 'space', 5
space:
	spush SP
	call emit
	ret

; --------------------------------------------------------------

; page ( -- )
; clear the screen
wordlink 'page', 4
page:	; scroll window
	mov ax, 0x0600	; AH=06 scroll up, AL=0 = clear
	mov bh, 0x07	; attribute (light gray on black)
	xor cx, cx	; upper-left (row=0, col=0)
	mov dx, 0x184F	; lower-right (row=24, col=79)
	int 0x10

	; reset cursor
	mov ah, 0x02
	xor bx, bx
	xor dx, dx
	int 0x10

	ret

; --------------------------------------------------------------
; ASCII CONSTANTS
; --------------------------------------------------------------

; BS ( -- 8 )
wordlink 'BS', 2
ascii_bs:
	spush BS
	ret

; --------------------------------------------------------------

; LF ( -- 8 )
wordlink 'LF', 2
ascii_lf:
	spush LF
	ret

; --------------------------------------------------------------

; CR ( -- 13 )
wordlink 'CR', 2
ascii_cr:
	spush CR
	ret

; --------------------------------------------------------------

; SP ( -- 8 )
wordlink 'SP', 2
ascii_sp:
	spush SP
	ret

; ==============================================================
; OTHER
; --------------------------------------------------------------

; write ( u -- )
; write block u to disk
wordlink 'write', 5
write:	spop bx
	mov si, bx
	add si, si	; convert block number to sector count
	mov cx, 10
	shl bx, cx
	add bx, 0x7e00	; addr of block in memory

	; beginning of block memory in disk
	mov ch, 1	; cylinder
	mov cl, 1	; sector
	mov dh, 0	; head

.find:	test si, si
	jz .done
	dec si

	inc cl,
	cmp cl, 9
	jbe .find

	mov cl, 1
	inc dh
	cmp dh, 2
	jb .find

	xor dh, dh
	inc ch
	cmp ch, 40
	jb .find

	; CHS is set, now write!
.done:	mov si, 2

.write:	test si, si
	jz .exit
	dec si

	mov ah, 0x03
	mov al, 1
	mov dl, [BOOTDRIVE]
	int 0x13

	jc .errdisk

	add bx, 512

	inc cl
	cmp cl, 9
	jbe .write

	mov cl, 1
	inc dh
	cmp dh, 2
	jb .write

	xor dh, dh
	inc ch
	cmp ch, 40
	jb .write

	jmp .errdisk

.exit:	ret

.errdisk:
	spush .err
	spush 22
	call type
	ret
.err	db 'Error writing to disk.'

; --------------------------------------------------------------

; words ( -- )
wordlink 'words', 5
words:	mov bx, [latest]
	xor ch, ch
.loop:	test bx, bx
	jz .done
	mov cl, [bx - 1]
	and cl, MASK
	test cl, cl
	jz .next
	mov ax, bx
	sub ax, cx
	sub ax, 3
	spush ax
	spush cx
	call type
	call space
.next:	mov bx, [bx - 3]
	jmp .loop
.done:	ret

; --------------------------------------------------------------

wordlink 'boot', 4
boot:	int 0x19

; --------------------------------------------------------------

wordlink 'noop', 4
noop:	ret

; --------------------------------------------------------------

tail:
