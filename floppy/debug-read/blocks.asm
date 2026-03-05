BITS 16
CPU 8086
org 0x7e00

%macro type 1
	mov si, %1
	call print
%endmacro

%macro string 2
	times %1*512 - 20 - ( $ - $$ ) db 0
s%1:	db 'Loaded: sector ', %2, 13, 10, 0
%endmacro

start:	jmp test

blocks: db 'BLOCKS', 13, 10, 0

test:	type blocks
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
	type s18
	type s19
	type s20
	type s21
	type s22
	type s23
	type s24
	type s25
	type s26
	type s27
	type s28
	type s29
	type s30
	type s31
	type s32
	type s33
	type s34
	type s35
	type s36
	type s37
	type s38
	type s39
	type s40
	type s41
	type s42
	type s43
	type s44
	type s45
	type s46
	type s47
	type s48
	type s49
	type s50
	type s51
	type s52
	type s53
	type s54
	type s55
	type s56
	type s57
	type s58
	type s59
	type s60
	type s61
	type s62
	type s63
	type s64
	jmp $

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
	string 18, '18'
	string 19, '19'
	string 20, '20'
	string 21, '21'
	string 22, '22'
	string 23, '23'
	string 24, '24'
	string 25, '25'
	string 26, '26'
	string 27, '27'
	string 28, '28'
	string 29, '29'
	string 30, '30'
	string 31, '31'
	string 32, '32'
	string 33, '33'
	string 34, '34'
	string 35, '35'
	string 36, '36'
	string 37, '37'
	string 38, '38'
	string 39, '39'
	string 40, '40'
	string 41, '41'
	string 42, '42'
	string 43, '43'
	string 44, '44'
	string 45, '45'
	string 46, '46'
	string 47, '47'
	string 48, '48'
	string 49, '49'
	string 50, '50'
	string 51, '51'
	string 52, '52'
	string 53, '53'
	string 54, '54'
	string 55, '55'
	string 56, '56'
	string 57, '57'
	string 58, '58'
	string 59, '59'
	string 60, '60'
	string 61, '61'
	string 62, '62'
	string 63, '63'
	string 64, '64'
