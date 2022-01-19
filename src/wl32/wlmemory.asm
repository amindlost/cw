;*********************************************************************
;*   WLMEMORY.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          11/05/95                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   linker memory manipulation routines                             *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLMEMORY
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Equates                   *
;*****************************

_386SHELL	EQU	1

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLDATA.INC
INCLUDE	WLERRCOD.INC
INCLUDE	CW.INC

;*****************************
;* Public declarations       *
;*****************************

PUBLIC	AllocateMemory,ResizeMemory,ReleaseMemory
PUBLIC	ResizeMemory32
PUBLIC	Alloc16KZeroBlock,Alloc64KIOBlock
PUBLIC	Zero64KIOBlock
PUBLIC	AllocateBigMemory

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

; globals
IOBlockSeg	DW	?	; segment of 64K-16 size i/o block
ZeroBlockSeg	DW	?	; segment of 16K block of zero values

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

EXTRN	LinkerErrorExit:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* ALLOCATEMEMORY            *
;*****************************

; allocate a block of memory
; dx == size of block required in bytes (high word of possible allocation zero'ed)
; returns ax == selector to access block with

AllocateMemory	PROC
	push	cx			; save critical registers
	push	ebx
IFNDEF	_386SHELL
	mov	bx,dx
	add	bx,15
	mov	cl,4
	shr	bx,cl
	mov	ah,48h
	int	21h
	jc	AllocFail
ELSE
	xor	cx,cx			; zero high word of allocation
	sys	GetMem
	jc	AllocFail
	mov	ax,bx			; move selector in bx to ax
ENDIF
	pop	ebx				; restore critical registers
	pop	cx
	ret

; failure to allocate memory
AllocFail:
	mov	al,MEMALLOCFAILERRORCODE
	call	LinkerErrorExit	; no return

AllocateMemory	ENDP

;*****************************
;* ALLOCATEBIGMEMORY         *
;*****************************

; allocate a block of memory
; cx:dx == size of block required in bytes
; returns ax == selector to access block with

AllocateBigMemory	PROC
	push	cx			; save critical registers
	push	ebx
IFNDEF	_386SHELL
	mov	bx,cx
	movzx	ebx,bx
	shl	ebx,16
	mov	bx,dx
	add	ebx,15
	shr	ebx,4
	mov	ah,48h
	int	21h
	jc	AllocBigFail
ELSE
	sys	GetMem
	jc	AllocBigFail
	mov	ax,bx			; move selector in bx to ax
ENDIF
	pop	ebx				; restore critical registers
	pop	cx
	ret

; failure to allocate memory
AllocBigFail:
	mov	al,MEMALLOCFAILERRORCODE
	call	LinkerErrorExit	; no return

AllocateBigMemory	ENDP

;*****************************
;* RESIZEMEMORY              *
;*****************************

; resize allocated memory block
; upon entry ax == selector,
;  dx == new resize value in bytes (high word of possible allocation zero'ed)

ResizeMemory	PROC
	push	cx			; save critical registers
	push	ebx
	push	ax
IFNDEF	_386SHELL
	push	es
	mov	es,ax
	mov	bx,dx
	add	bx,15
	mov	cl,4
	shr	bx,cl
	mov	ah,4ah
	int	21h
	pop	es
	jc	SizeFail
ELSE
	mov	bx,ax			; selector value to bx
	xor	cx,cx			; zero high word of allocation
	sys	ResMem			; resize previously allocated memory block
	jc	SizeFail
ENDIF
	pop	ax				; restore critical registers
	pop	ebx
	pop	cx
	ret

; failure in resize of memory
SizeFail:
	mov	al,MEMSIZEFAILERRORCODE
	call	LinkerErrorExit	; no return

ResizeMemory	ENDP

;*****************************
;* RESIZEMEMORY32            *
;*****************************

; resize allocated memory block, allow >64K
; upon entry ax == selector,
;  edx == new resize value in bytes

ResizeMemory32	PROC
	push	ecx			; save critical registers
	push	ebx
	push	ax
IFNDEF	_386SHELL
	push	es
	mov	es,ax
	mov	bx,dx
	add	bx,15
	shr	bx,4
	mov	ah,4ah
	int	21h
	pop	es
	jc	SizeFail32
ELSE
	mov	ecx,edx
	mov	bx,ax			; selector value to bx
	sys	ResMem32		; resize previously allocated memory block
	jc	SizeFail32
ENDIF
	pop	ax				; restore critical registers
	pop	ebx
	pop	ecx
	ret

; failure in resize of memory
SizeFail32:
	mov	al,MEMSIZEFAILERRORCODE
	call	LinkerErrorExit	; no return

ResizeMemory32	ENDP

;*****************************
;* RELEASEMEMORY             *
;*****************************

; release a block of memory
; upon entry ax==selector of memory block to release

ReleaseMemory	PROC
	push	es			; save critical registers
	push	ebx
IFNDEF	_386SHELL
	mov	es,ax
	mov	ah,49h
	int	21h
	jc	ReleaseFail
ELSE
	mov	bx,ax
	sys	RelSel
	jc	ReleaseFail
ENDIF
	pop	ebx				; restore critical registers
	pop	es
	ret

; failure to allocate memory
ReleaseFail:
	mov	al,MEMRELEASEFAILERRORCODE
	call	LinkerErrorExit	; no return

ReleaseMemory	ENDP

;*****************************
;* ALLOC16KZEROBLOCK         *
;*****************************

; allocate and zero 16K block
; destroys ax,cx,dx,di

Alloc16KZeroBlock	PROC
	push	es			; save critical register
	mov	dx,16384
	call	AllocateMemory
	mov	ZeroBlockSeg,ax
	mov	es,ax
	xor	di,di			; es:di -> block to zero
	mov	cx,4096			; 4K doublewords to zero
	xor	eax,eax
	rep	stosd
	pop	es				; restore critical register
	ret
Alloc16KZeroBlock	ENDP

;*****************************
;* ALLOC64KIOBLOCK           *
;*****************************

; allocate 64K byte block for i/o, zero bytes in it
; destroys ax,cx,dx,di

Alloc64KIOBlock	PROC
	push	es			; save critical register
	mov	cx,1
	xor	dx,dx			; 64K block in cx:dx
	call	AllocateBigMemory
	mov	IOBlockSeg,ax
	call	Zero64KIOBlock	; zero the block
	pop	es				; restore critical register
	ret
Alloc64KIOBlock	ENDP

;*****************************
;* ZERO64KIOBLOCK            *
;*****************************

; zero out the 64K i/o block
; destroys eax,cx,di,es

Zero64KIOBlock	PROC
	mov	es,IOBlockSeg
	xor	di,di			; es:di -> block to zero
	mov	cx,16384		; 16K doublewords to zero
	xor	eax,eax
	rep	stosd
	ret
Zero64KIOBlock	ENDP

ENDS

END
