BITS 16
CPU 8086
org 0x0500

%macro type 1
	mov si, %1
	call print
%endmacro

%macro string 2
	times %1*512 - 20 - ( $ - $$ ) db 0
s%1:	db 'Loaded: sector ', %2, 13, 10, 0
%endmacro

start:	jmp test

kernel:	db 'KERNEL', 13, 10, 0

test:	type kernel
	type s1
	type s2
	type s3
	type s4
	type s5
	type s6
	type s7
	type s8
	type s9
	type s10
	type s11
	type s12
	type s13
	type s14
	type s15
	type s16
	type s17
	jmp 0:0x7e00

	; print null-terminated string
	; si = string address
print:	lodsb
	or al, al
	jz .done
	mov ah, 0x0e
	int 0x10
	jmp print
.done:	ret

	string 1, '1'
	string 2, '2'
	string 3, '3'
	string 4, '4'
	string 5, '5'
	string 6, '6'
	string 7, '7'
	string 8, '8'
	string 9, '9'
	string 10, '10'
	string 11, '11'
	string 12, '12'
	string 13, '13'
	string 14, '14'
	string 15, '15'
	string 16, '16'
	string 17, '17'
