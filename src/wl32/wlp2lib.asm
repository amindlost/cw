;*********************************************************************
;*   WLP2LIB.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          05/01/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   pass 2 library processing routines                              *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLP2LIB
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

PUBLIC	LIBPass2

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

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

EXTRN	CurrentBaseOBJBuff:WORD
EXTRN	CurrentFileName:BYTE
EXTRN	CurrentLIB:WORD
EXTRN	CurrentLNAMESIndex:WORD
EXTRN	CurrentSegDefCount:WORD
EXTRN	EndOfOBJFlag:BYTE
EXTRN	LastLEDATASegIndex:WORD
EXTRN	LIBDictTablePtr:WORD
EXTRN	OS2ModuleFlag:BYTE
EXTRN	Pass2ModuleCount:WORD
EXTRN	PharLapModuleFlag:BYTE

IFDEF SYMBOLPACK
EXTRN	RelocationAdjustment:DWORD
ENDIF

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	DisplayModuleName:PROC
EXTRN	Process2OBJRecord:PROC

IFDEF SYMBOLPACK
EXTRN	Pass2ClipperCheck:PROC
ENDIF

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* LIBPASS2                  *
;*****************************

; pass 2 processing of library files

LIBPass2	PROC
	xor	ax,ax
	cmp	TotalLIBCount,ax	; see if any library files to process
	je	lpret			; no
	cmp	LIBDictTablePtr,ax	; see if specified libraries were used
	je	lpret			; no

	mov	ProcessingLIBFlag,ON
	mov	CurrentLIB,1	; init current library

lplibloop:
	push	ds
	pop	es				; es -> wl32 data
	mov	bx,CurrentLIB
	dec	bx				; make relative zero
	add	bx,bx			; convert to word offset

	mov	fs,LIBDictTablePtr	; fs:bx -> entry in table of memory block selectors
	mov	ax,fs:[bx]		; ax -> first buffer of library dictionary
	or	ax,ax			; see if any buffer at all (occurs if all symbols resolved before this lib)
	je	lpnextlib		; no buffer, no modules used

	mov	fs,ax			; fs -> first buffer of library dictionary
	test	fs:[LIBDictHeaderStruc.ldhsFlags],MODULEUSEDFLAG	; see if modules used from library
	je	lpnextlib		; no modules used in library
	mov	cx,fs:[LIBDictHeaderStruc.ldhsModUsedCount]	; get count of modules used
	xor	ebx,ebx			; index for module lookup

modloop:
	push	ebx			; save module lookup index
	push	cx			; save module count
	push	fs			; save fs -> library dictionary buffer
	xor	eax,eax

IFDEF SYMBOLPACK
	mov	RelocationAdjustment,eax
ENDIF

	mov	LastLEDATASegIndex,ax	; init last LEDATA segment index value for module
	mov	EndOfOBJFlag,al	; init end of object module flag
	mov	CurrentLNAMESIndex,ax	; init object record indices
	mov	CurrentSegDefCount,ax	; init module-level counters

	mov	fs,fs:[LIBDictHeaderStruc.ldhsModBuffPtr]	; fs -> module buffer pointer table
	mov	fs,fs:[2*ebx]	; fs -> current module buffer base
	mov	CurrentBaseOBJBuff,fs	; save pointer to current buffer base
	mov	si,IOBUFFSYSVARSIZE	; fs:si -> module first memory block first byte (offset for system variables)

	mov	ax,fs:[IOBuffHeaderStruc.ibhsFlags]
	test	ax,ISOS2MODFLAG
	setne	OS2ModuleFlag	; set os/2 flag is module is flagged
	and	ax,ISPHARLAPMODFLAG
	setne	PharLapModuleFlag	; set phar lap flag if module is flagged

; set CurrentFileName buffer
	lgs	bx,fs:[IOBuffHeaderStruc.ibhsFileNamePtr]	; gs:bx -> current file name
	mov	di,OFFSET DGROUP:CurrentFileName

modnameloop:
	mov	al,gs:[bx]		; get file name char
	stosb				; store it
	inc	bx				; move to next slot
	or	al,al			; see if null terminator transferred
	jne	modnameloop
	call	DisplayModuleName	; display library module name, if /i option

IFDEF SYMBOLPACK
	call	Pass2ClipperCheck	; check if module is clipper module
ENDIF

recloop:
	call	Process2OBJRecord	; get current OBJ record, process it pass 2
	cmp	EndOfOBJFlag,OFF	; see if at end of OBJ
	je	recloop			; not at end

;multiple modules in one file check
;@@@ code goes here

	inc	Pass2ModuleCount	; bump pass 2 module counter

	pop	fs				; restore fs -> library dictionary buffer
	pop	cx				; restore count of modules to process
	pop	ebx				; restore module lookup index
	inc	ebx				; move to next module
	loop	modloop		; loop through all of them

lpnextlib:
	inc	CurrentLIB		; bump to next lib
	mov	ax,CurrentLIB
	cmp	ax,TotalLIBCount	; see if next lib exists
	jbe	lplibloop		; yes

lpret:
	ret
LIBPass2	ENDP

ENDS

END
