;*********************************************************************
;*   WLP2RES.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/06/93                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   pass 2 resolution routines                                      *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLP2RES
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

PUBLIC	Pass2Resolution

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

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	WriteMAPFile:PROC
EXTRN	WriteSYMFile:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* PASS2RESOLUTION           *
;*****************************

; second pass resolution processing

Pass2Resolution	PROC
	cmp	IsMAPOption,OFF	; see if map file to be created
	je	pr2				; no
	push	ds
	pop	es				; es -> DGROUP
	call	WriteMAPFile	; write map file

pr2:
	cmp	IsSYMOption,OFF	; see if SYM file to be created
	je	pr3				; no
	push	ds
	pop	es				; es -> DGROUP
	call	WriteSYMFile	; write map file

pr3:
	ret
Pass2Resolution	ENDP

ENDS

END
