;*********************************************************************
;*   WLTERM.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          11/04/93                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   linker termination routines                                     *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLTERM
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

PUBLIC	TerminateToDOS,CleanupForExit

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

beep3   DB  BELL,BELL,BELL

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

EXTRN	DisplayShortString:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* TERMINATETODOS            *
;*****************************

; terminate to DOS, check if should beep first, do so if so
; upon entry al holds the return code
; destroy registers at will

TerminateToDOS	PROC
	mov	ah,4ch			; terminate function code in ah, al holds return code
	push	ax			; save terminate and return code
	cmp	IsExitBeepOption,OFF	; see if should beep before termination
	je	Terminate		; no
	mov	bx,STDOUT
	mov	dx,OFFSET DGROUP:beep3
	mov	cl,3			; beep the speaker three times
	call	DisplayShortString

Terminate:
	pop	ax				; restore terminate and return code
	int	21h
TerminateToDOS	ENDP

;*****************************
;* CLEANUPFOREXIT            *
;*****************************

; clean up any interim system changes made for linker exit

CleanupForExit	PROC

	ret
CleanupForExit	ENDP

ENDS

END
