;*********************************************************************
;*   WLSYSTEM.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          05/17/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   system level initializations and end cleanup                    *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK wlsystem
PAGE    50,80

.MODEL  SMALL

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLGLOBAL.INC

;*****************************
;* Public declarations       *
;*****************************

PUBLIC	DOSInit
PUBLIC	GetEMSStatus
PUBLIC	GetXMSStatus
PUBLIC	NormalExit

;*****************************
;* Uninitialized data        *
;*****************************

.DATA?

; doubleword values
OldCtrlCHandler	DD	?

;*****************************
;* Constant data             *
;*****************************

.CONST

EMMName	DB	'EMMXXXX0',0

TermTextLength	DB	TermTextStop-TermText
TermText		DB	CR,LF,'Link terminated by user.'
TermTextStop	=	$

;*****************************
;* Code                      *
;*****************************

.CODE

;*****************************
;* External code procedures  *
;*****************************

EXTRN	DeleteILFFile:NEAR
EXTRN	DeleteQLKFile:NEAR
EXTRN	DeleteTempFile:NEAR
EXTRN	WriteToStdout:NEAR

;*****************************
;* DOSInit                   *
;*****************************

; initial linker DOS variable and vectoring setup
; set ds -> DGROUP
; destroys all registers

DOSInit	PROC
	cld						; make all string operations increment
	mov	PSP,es				; save PSP value

	mov ax,3300h			; get Ctrl-Break flag
	int 21h					; status returned in dl register
	mov CtrlBreakStatus,dl	; save status

	mov ah,30h				; get MS-DOS version number
	int 21h
	cmp al,2				; make sure at least 2.x
	jae i2					; yes
	int 20h					; force immediate 1.x compatible termination
i2:
	mov DosVersion,al		; save major dos version

	mov ax,3523h			; get old ctrl-c handler address
	int 21h
	mov WORD PTR OldCtrlCHandler,bx	; save offset
	mov WORD PTR OldCtrlCHandler+2,es	; save segment

	push	cs
	pop	ds
	mov dx,OFFSET NewCtrlBreakHandler	; ds:dx -> control-c handler
	mov ax,2523h			; set ctrl-c handler
	int 21h

	mov ax,DGROUP
	mov ds,ax				; restore ds -> warplink data
	ret
DOSInit	ENDP

;*****************************
;* NewCtrlBreakHandler       *
;*****************************

; control break handler
; destroys all registers

NewCtrlBreakHandler	PROC
	mov ax,DGROUP
	mov ds,ax				; make sure ds -> warplink data

	lds dx,OldCtrlCHandler	; ds:dx -> old cntrl-cl handler
	mov ax,2523h			; set ctrl-c handler
	int 21h

	mov ax,DGROUP
	mov ds,ax				; ds -> warplink data

	call	DeleteILFFile	; delete existing temporary files
	call	DeleteTempFile
	call	DeleteQLKFile
	call    FreeEMSXMS		; free up allocated EMS and XMS

; write terminated feedback
cb_feed:
	mov bx,OFFSET DGROUP:TermText
	call	WriteToStdout	; write feedback to stdout

	jmp DWORD PTR OldCtrlCHandler	; route to old control-c handler
NewCtrlBreakHandler	ENDP

;*****************************
;* NormalExit                *
;*****************************

; normal WarpLink exit, cleanup system, reset variables

NormalExit	PROC
	mov dl,CtrlBreakStatus	; get previous status of Ctrl-Break flag
	mov ax,3301h			; set Ctrl-Break flag to previous status
	int 21h

	call    FreeEMSXMS

	mov	al,DOSReturnCode
	mov	ah,4ch				; terminate to DOS
	int 21h

	ret
NormalExit	ENDP

;*****************************
;* GetEMSStatus              *
;*****************************

; check if EMS and can be used
; returns carry flag set if no, reset if yes
; destroys ax,bx,cx,dx

GetEMSStatus	PROC
	cmp	EMSOptionSet,0		; see if /xp or /xp3 option set
	je	ges_noems			; no, don't use EMS

	mov dx,OFFSET EMMName	; ds:dx -> device name
	mov ax,3d00h			; open for reading
	int 21h
	jc  ges_noems 			; open failed

	mov bx,ax     			; bx holds handle
	mov ax,4400h  			; IOCTL get device info
	int 21h
	jc  ges_noems 			; IOCTL call failed

	and dl,80h    			; get high bit, set if char device
	je  ges_noems 			; file device

	mov ax,4407h  			; IOCTL get output status
	int 21h
	jc  ges_noems 			; IOCTL call failed

	mov ah,3eh    			; close file
	int 21h
	jc  ges_noems 			; close failed

	mov ah,40h    			; get EMS system status
	int 67h       			; call EMM
	or  ah,ah     			; check for EMM error
	jne ges_noems 			; EMM error occurred

	mov ah,46h    			; get EMM version
	int 67h
	or  ah,ah     			; check for error
	jne ges_noems 			; error occurred
	mov	EMSVersion,al		; save EMS version
	cmp al,30h    			; must be EMS version 3.0 or greater
	jb	ges_noems 			; bad version
	cmp	XP3OptionSet,0		; see if forced EMS version 3.0 compatibility
	je	ges_pf				; no, get page frame
	mov	EMSVersion,30h		; force EMS version 3.0

ges_pf:
	mov ah,41h    			; get page frame address
	int 67h
	or  ah,ah
	jne ges_noems 			; EMM error occurred
	mov EMSBase,bx			; save EMS base (page frame)

; success
	clc						; clear carry flag to show no error
	ret

ges_noems:
	stc						; set carry flag to show error
	ret
GetEMSStatus	ENDP

;*****************************
;* GetXMSStatus              *
;*****************************

; check if XMS and can be used, get XMM entry point
; returns carry flag set if no, reset if yes
; destroys ax,bx,cx,dx,es

GetXMSStatus	PROC
	cmp XTOptionSet,0		; see if XMS option set
	je  gxs_noxms			; no

	mov ax,4300h			; check if XMM is present
	int 2fh					; multiplex interrupt
	cmp al,80h				; check if driver present
	je  gxs_getaddr			; yes, get entry point address

; no, or can't use, XMS
gxs_noxms:
	stc						; set carry flag to show error
	ret

gxs_getaddr:
	mov ax,4310h			; get XMS driver entry point
	int 2fh
	or	ax,ax
	je	gxs_noxms			; error occurred
	mov WORD PTR XMSAddr,bx	; save it
	mov WORD PTR XMSAddr+2,es
	clc						; clear carry flag to show no error
	ret
GetXMSStatus	ENDP

;*****************************
;* FreeEMSXMS                *
;*****************************

; free any used EMS/XMS
; destroys ax,bx,dx

FreeEMSXMS	PROC
	cmp XTOptionSet,0		; see if XMS option set
	je  fex_ems				; no

	mov	dx,XMSHandle
	or	dx,dx				; see if EMS handle has been zeroed
	je	fex_ems				; yes, ignore free
	mov ah,0ah				; free extended memory block
	call	DWORD PTR XMSAddr

fex_ems:
	cmp	EMSOptionSet,0		; see if /xp or /xp3 option set
	je  fex_ret				; no

	mov	dx,EMSHandle
	or	dx,dx				; see if EMS handle has been zeroed
	je	fex_ret				; yes, ignore free
	mov ah,45h				; release handle and memory pages
	int 67h

fex_ret:
	ret
FreeEMSXMS	ENDP

END
