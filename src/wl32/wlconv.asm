;*********************************************************************
;*   WLCONV.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          03/16/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   linker text and number conversion routines                      *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLCONV
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLDATA.INC

;*****************************
;* Public declarations       *
;*****************************

; routines
PUBLIC	ByteToHexString
PUBLIC	DwordToDecimalString

; variables
PUBLIC	TempBuffer

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

TempBuffer	DB	128 DUP (?)	; temporary storage buffer

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

ENDS

;*****************************
;* External data             *
;*****************************

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* BYTETOHEXSTRING           *
;*****************************

; byte to hex string conversion
; upon entry al==value, di -> storage for number
; updates di -> first char after last digit (not null terminated)
; destroys ax

ByteToHexString	PROC
	mov	ah,al			; save original value
	shr	al,4			; make 4 high bits relative zero
	call	hexcall		; tricky stuff, call then drop through to get both nybbles of byte
	mov	al,ah			; al holds low word of value
	and	al,0fh			; mask off high bits

hexcall:
	cmp	al,0ah			; see if A-F hex
	jb	hex2			; no
	add	al,7			; alpha adjust

hex2:
	add	al,30h			; convert to ASCII
	stosb				; save to storage
	ret
ByteToHexString	ENDP

;*****************************
;* DWORDTODECIMALSTRING      *
;*****************************

; dword to decimal string conversion
; upon entry eax==value, di -> storage for number
; updates di -> null terminator after last digit
; destroys eax,bx,cx,edx

DwordToDecimalString	PROC
	push	di			; save di -> destination
	mov	di,OFFSET DGROUP:TempBuffer
	xor	cx,cx			; init count of digits in number
	mov	ebx,0ah			; number divisor, constant

divloop:
	xor	edx,edx			; zero high word value
	div	ebx				; divide by 10
	xchg	edx,eax		; quotient into edx, remainder into eax
	or	al,30h			; change remainder to ASCII
	stosb				; save char
	inc	cx				; bump digit count
	mov	eax,edx			; eax holds quotient
	test	eax,eax		; stop dividing when zero
	jne	divloop

; di -> end of reversed number value, cx holds digits in number
	mov	bx,di
	pop	di				; di -> number storage for printing

revloop:
	dec	bx				; bx -> char in reversed number
	mov	al,ds:[bx]		; get char
	stosb				; place in printing string
	loop	revloop		; unrevers all chars in buffer

	mov	BYTE PTR ds:[di],0	; null terminate string
	ret
DwordToDecimalString	ENDP

ENDS

END
