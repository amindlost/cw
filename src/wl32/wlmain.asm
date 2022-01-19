;*********************************************************************
;*   WLMAIN.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          06/10/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3g                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 4.0+                                      *
;*                                                                   *
;*   main driver for linker                                          *
;*                                                                   *
;*********************************************************************

TITLE	WL32 WLMAIN
PAGE    50,80
DOSSEG

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA,STACK

STACK	SEGMENT PARA STACK USE16 'STACK'
	DB	0c00h DUP (?)
ENDS

;*****************************
;* Equates                   *
;*****************************

;IS32BITAPP	EQU	1

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE	WLDATA.INC

;*****************************
;* Public declarations       *
;*****************************

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

ResolvingSegTextLen	DB	ResolvingSegTextStop-ResolvingSegText
ResolvingSegText	DB	CR,LF,'*** Resolving segment addresses'
ResolvingSegTextStop		=	$

ApplyingOBJTextLen	DB	ApplyingOBJTextStop-ApplyingOBJText
ApplyingOBJText	DB	CR,LF,'*** Applying object module fixups'
ApplyingOBJTextStop		=	$

ApplyingLIBTextLen	DB	ApplyingLIBTextStop-ApplyingLIBText
ApplyingLIBText	DB	CR,LF,'*** Applying library module fixups'
ApplyingLIBTextStop		=	$

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

; globals
IsArgsFlag	DB	0
WarningsCount	DW	0
Pass2Flag	DB	0		; nonzero if on pass 2+ of link

ENDS

;*****************************
;* External data             *
;*****************************

;*****************************
;* Code begins               *
;*****************************

IFDEF IS32BITAPP
; using entry point in 32-bit segment allows WL32 to run as a 32-bit application
_TEXT32	SEGMENT PARA PRIVATE USE32 'CODE'
	ASSUME cs:_TEXT32

start:
	jmp	FAR PTR start16

ENDS
ENDIF

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP,ss:STACK

;*****************************
;* External code routines    *
;*****************************

EXTRN	CleanupForExit:PROC
EXTRN	CreateProgramFile:PROC
EXTRN	DisplayCredits:PROC
EXTRN	DisplayFinalFeedback:PROC
EXTRN	DisplayLinkInfo:PROC
EXTRN	DisplayParseResults:PROC
EXTRN	DisplaySummary:PROC
EXTRN	OBJPass1:PROC,LIBPass1:PROC,Pass1Resolution:PROC
EXTRN	OBJPass2:PROC,LIBPass2:PROC,Pass2Resolution:PROC
EXTRN	ParseCommandLine:PROC
EXTRN	Setup:PROC
EXTRN	TerminateToDOS:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* MAIN                      *
;*****************************

main		PROC

IFDEF	IS32BITAPP
start16:
ELSE
start:
ENDIF

	call	Setup		; get system variables and values, trap control C
	call	DisplayCredits	; display linker credit line
	cmp	IsArgsFlag,FALSE	; check for arguments to linker
	jne	m2				; at least one argument
	call	DisplaySummary	; display summary of linker syntax/commands
	jmp	SHORT ExitLinker	;  and exit program

m2:
	call	ParseCommandLine	; parse linker command line
	cmp	IsParseDisplayOption,OFF	; see if parse display flag set
	je	m3				; no
	call	DisplayParseResults	; display results of parse
	je	ExitLinker

m3:
	call	OBJPass1	; do first pass on object modules
	call	LIBPass1	; do first pass on libraries

	mov	bx,OFFSET DGROUP:ResolvingSegText
	call	DisplayLinkInfo	
	call	Pass1Resolution	; do first pass resolution processing

	mov	bx,OFFSET DGROUP:ApplyingOBJText
	call	DisplayLinkInfo	
	mov	Pass2Flag,ON	; flag doing second pass
	call	OBJPass2	; do second pass on object modules

	mov	bx,OFFSET DGROUP:ApplyingLIBText
	call	DisplayLinkInfo	
	call	LIBPass2	; do second pass on libraries

	call	Pass2Resolution	; do second pass resolution processing
	call	CreateProgramFile	; create the program
	call	DisplayFinalFeedback	; give feedback on warnings, other final info

ExitLinker:
	call	CleanupForExit	; clean up any interim system changes made
	xor	ax,ax			; init return code to zero (ah will be changed)
	cmp	IsWarnRetCode1Option,al	; see if warnings set return code to 1
	je	DoTerminate		; no
	cmp	WarningsCount,ax	; see if any warnings
	je	DoTerminate		; no
	inc	ax				; give return code (errorlevel) 1

DoTerminate:

;	mov	ax,0			; @@@ temporary, force GPF
;	mov	gs,ax
;	mov WORD PTR gs:[1234h],1234h

	call	TerminateToDOS	; no return

main		ENDP

ENDS

END	start
