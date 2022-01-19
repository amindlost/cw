;*********************************************************************
;*   WLFEED.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/02/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.31                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   linker feedback routines                                        *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLFEED
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Equates                   *
;*****************************

DWORDPARAMETER	EQU	4
STRINGPARAMETER	EQU	2

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLDATA.INC

;*****************************
;* Public declarations       *
;*****************************

PUBLIC	DisplayCredits
PUBLIC	DisplayFinalFeedback
PUBLIC	DisplayLinkInfo
PUBLIC	DisplayModuleName
PUBLIC	DisplayParseResults
PUBLIC	DisplayProcFileFeedback
PUBLIC	DisplayReadFileFeedback
PUBLIC	DisplaySummary
PUBLIC	DisplaySegmentName
PUBLIC	DisplayShortString
PUBLIC	DisplayTextStringCRLF
PUBLIC	DisplayTextStringNoCRLF
PUBLIC	DisplayVarStringCRLF
PUBLIC	DisplayVarStringNoCRLF

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

NumberBuff2	DB	5 DUP (?)	; temporary buffer for decimal ASCII value (reversed)

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

CreditTextLen	DB	CreditTextStop-CreditText
CreditText		DB	'CauseWay WL32'
IFDEF CLIPPER
				DB	' for Clipper'
IFNDEF NATHAN
				DB	' Version 1.32'
ELSE
				DB	' Version 1.32n
ENDIF
ENDIF
IFDEF CLARION
				DB	' for Clarion 2.1'
				DB	' Version 1.32'
ENDIF
IFDEF WATCOM_ASM
				DB	' Version 1.32'
ENDIF
				DB	' Copyright 1993-99 Michael Devore.',CR,LF,'All rights reserved.'
CreditTextStop	=	$

SuccessTextLen	DB	SuccessTextStop-SuccessText
SuccessText		DB	CR,LF,'Link of executable file successfully completed.'
SuccessTextStop	=	$

Summary1TextLen	DW	Summary1TextStop-Summary1Text
Summary1Text	DB	'WL32'
				DB	' STANDARD LINK OPTIONS:'
CRLFText		DB	CR,LF	; double duty as printable CR/LF
				DB	CR,LF
				DB	' /3p         Create protected mode 3P-format executable without DOS extender'
				DB	CR,LF
SpaceSlash		DB	' /b          Beep the speaker at linker completion'
				DB	CR,LF
IFDEF WATCOM_ASM
				DB	' /cs         perform Case Sensitive symbols link'
				DB	CR,LF
				DB	' /ds         set DS to SS at startup'
				DB	CR,LF
ENDIF
				DB	' /ex         create DOS EXE-format file'
				DB	CR,LF
IFDEF WATCOM_ASM
				DB	' /f          create FLAT memory model executable'
				DB	CR,LF
ENDIF
				DB	' /fl         use Fast Load EXE file DOS extender feature'
				DB	CR,LF
IFDEF CLIPPER
				DB	' /fx         use alternate FiXup logic (req. for CA Clipper Tools)'
				DB	CR,LF
ENDIF
				DB	' /i          display link process Information'
				DB	CR,LF
				DB	' /il         display link process Information, Limit information displayed'
				DB	CR,LF
				DB	' /lc:<name>  Link options Configuration file name'
				DB	CR,LF
IFNDEF CLARION
				DB	' /ls         use alternate Library Search logic'
				DB	CR,LF
ENDIF
				DB	' /m          create MAP file'
				DB	CR,LF
COMMENT !
				DB	' /nc         do Not display Copyright or successful link message'
				DB	CR,LF
END COMMENT !
				DB	' /nd         do Not use Default library names in object modules'
				DB	CR,LF
				DB	' /nwd        do Not Warn on Duplicate symbols'
				DB	CR,LF
				DB	' /nwld        do Not Warn on Library only Duplicate symbols'
				DB	CR,LF
;@@@				DB	' /s          Symbol names are case sensitive when linking'
;@@@				DB	CR,LF
;@@@				DB	' /sp         Symbol table Pack of Clipper-compiled routines'
;@@@				DB	CR,LF
				DB	' /st:<size>  set program STack size in bytes'
				DB	CR,LF
				DB	' /sy         create SYM file for CWD debugger'
				DB	CR,LF
;@@@				DB	' /ud:<setting>  User Defined link option setting'
;@@@				DB	CR,LF
				DB	' /w1         Warnings generate exit code of 1, not zero'
				DB	CR,LF
;@@@				DB	' /wn         Warnings are Not displayed by linker'
;@@@				DB	CR,LF
				DB	' /wu         issue Warning on Unknown linker options or commands'
IFDEF WATCOM_ASM
				DB	CR,LF
				DB	' /zu         Zero fill Uninitialized segments'
ENDIF
Summary1TextStop	=	$

EXETextLen	DB	EXETextStop-EXEText
EXEText	DB	'EXE file name: '
EXETextStop		=	$

MAPTextLen	DB	MAPTextStop-MAPText
MAPText	DB	'MAP file name: '
MAPTextStop		=	$

OBJTextLen	DB	OBJTextStop-OBJText
OBJText	DB	'Object module file names: '
OBJTextStop		=	$

LIBTextLen	DB	LIBTextStop-LIBText
LIBText	DB	'Library file names: '
LIBTextStop		=	$

OptionTextLen	DB	OptionTextStop-OptionText
OptionText	DB	'Link Option Settings:',CR,LF
OptionTextStop		=	$

ReadFileTextLen	DB	ReadFileTextStop-ReadFileText
ReadFileText	DB	CR,LF,'*** Reading file: '
ReadFileTextStop		=	$

ReadModTextLen	DB	ReadModTextStop-ReadModText
ReadModText	DB	CR,LF,'*** Reading library module: '
ReadModTextStop		=	$

ProcessFileTextLen	DB	ProcessFileTextStop-ProcessFileText
ProcessFileText	DB	CR,LF,'*** Processing file: '
ProcessFileTextStop		=	$

WriteSegTextLen	DB	WriteSegTextStop-WriteSegText
WriteSegText	DB	CR,LF,'*** Writing segment: '
WriteSegTextStop		=	$

WarnCountTextLen	DB	WarnCountTextStop-WarnCountText
WarnCountText	DB	CR,LF,'Total number of warnings: '
WarnCountTextStop		=	$

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

NumberBuff	DB	6 DUP (0)	; buffer for decimal ASCII value

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	CurrentFileName:BYTE
EXTRN	InText:BYTE
EXTRN	NumberBuffer:BYTE
EXTRN	OBJNameSelector:WORD,LIBNameSelector:WORD
EXTRN	OptionList:WORD
EXTRN	ProcessModText:BYTE
EXTRN	WorkingBuffer:BYTE

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	NormalGSBXSource:PROC
EXTRN	DwordToDecimalString:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* DISPLAYCREDITS            *
;*****************************

; display linker credit/copyright line

DisplayCredits	PROC
;	cmp	IsNoCopyrightOption,0	; see if copyright display shut off
;	jne	dcret
	mov	bx,OFFSET DGROUP:CreditText
	call	DisplayTextStringCRLF

dcret:
	ret
DisplayCredits	ENDP

;*****************************
;* DISPLAYSUMMARY            *
;*****************************

; display linker command summary

DisplaySummary	PROC

; display built in link options
	mov	bx,OFFSET DGROUP:Summary1Text
	call	DisplayLongStringCRLF
	ret
DisplaySummary	ENDP

;*****************************
;* DISPLAYSHORTSTRING        *
;*****************************

; display a string of <256 characters to stdout
; upon entry cl==string length, dx -> string
; destroys ax

DisplayShortString	PROC
	xor	ch,ch			; zap high byte of length word
	mov	bx,STDOUT
	mov	ah,40h
	int	21h
	ret
DisplayShortString	ENDP

;*****************************
;* DISPLAYTEXTSTRINGNOCRLF   *
;*****************************

; display text string with no CR/LF at end
; upon entry ds:bx -> string text, immediately preceded by one-byte length of string,
; destroys ax,bx,cx,dx
; returns bx == stdout, ch==0 for additional printing

DisplayTextStringNoCRLF	PROC
	mov	cl,[bx-1]		; get length of string to print
	mov	dx,bx			; ds:dx -> string to print
	call	DisplayShortString
	ret
DisplayTextStringNoCRLF	ENDP

;*****************************
;* DISPLAYTEXTSTRINGCRLF     *
;*****************************

; display text string with CR/LF at end, plus blank line CR/LF
; upon entry bx -> string text, immediately preceded by one-byte length of string,
;  ds -> DGROUP
; destroys ax,bx,cx,dx

DisplayTextStringCRLF	PROC
	call	DisplayTextStringNoCRLF

; write string terminating CR/LF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

; write blank line CR/LF
	call	DisplayShortString
	ret
DisplayTextStringCRLF	ENDP

;*****************************
;* DISPLAYVARSTRINGNOCRLF    *
;*****************************

; display variable text string with no CR/LF at end
; upon entry ds:bx -> string text
; returns bx == stdout, ch==0 for additional printing

DisplayVarStringNoCRLF	PROC
	mov	dx,bx			; dx -> string

dvsloop:
	cmp	BYTE PTR [bx],0	; see if at null terminator
	je	foundnull		; yes
	inc	bx				; move to next char to print
	jmp	SHORT dvsloop

foundnull:
	mov	cx,bx			; get offset in string to null terminator
	sub	cx,dx			; cx holds number of bytes to print
	mov	bx,STDOUT		; write to standard output device
	mov	ah,40h			; write to device
	int	21h
	ret
DisplayVarStringNoCRLF	ENDP

;*****************************
;* DISPLAYVARSTRINGCRLF      *
;*****************************

; display variable text string with CR/LF at end, plus blank line CR/LF
; upon entry bx -> string text
;  ds -> DGROUP
; destroys ax,bx,cx,dx

DisplayVarStringCRLF	PROC
	call	DisplayVarStringNoCRLF

; write string terminating CR/LF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

; write blank line CR/LF
	call	DisplayShortString
	ret
DisplayVarStringCRLF	ENDP

;*****************************
;* DISPLAYLONGSTRINGCRLF     *
;*****************************

; display long text string with CR/LF at end, plus blank line CR/LF
; upon entry bx -> string text, immediately preceded by two-byte length of string,
;  ds -> DGROUP
; destroys ax,bx,cx,dx

DisplayLongStringCRLF	PROC
	mov	cx,[bx-2]		; get length of string to print
	mov	dx,bx			; ds:dx -> string to print
	mov	bx,STDOUT		; write to standard output device
	mov	ah,40h			; write to device
	int	21h

; write string terminating CR/LF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

; write blank line CR/LF
	call	DisplayShortString
	ret
DisplayLongStringCRLF	ENDP

;*****************************
;* DISPLAYFINALFEEDBACK      *
;*****************************

; display final linker feedback (# of warnings, etc.)

DisplayFinalFeedback	PROC
	cmp	WarningsCount,0	; see if any warnings
	je	dff2			; no
	mov	bx,OFFSET DGROUP:WarnCountText
	call	DisplayTextStringNoCRLF

	mov di,OFFSET DGROUP:NumberBuff2	; point to temporary number buffer
	mov ax,ds
	mov es,ax			; es -> warplink data
	xor cx,cx			; init count of digits
	mov ax,WarningsCount	; get count of warning messages

dffdivloop:
	xor  dx,dx			; zero high word value
	mov  bx,0AH			; divide by 10
	div  bx
	xchg    dx,ax		; swap quotient into dx, remainder into ax
	or   al,30H			; make remainder into ASCII number
	stosb 				; save char to buffer
	inc cx				; bump count of digits
	xchg    dx,ax		; restore quotient to ax
	or   ax,ax			; check if quotient is zero
	jne  dffdivloop		; no, continue dividing

	mov si,di
	mov di,OFFSET DGROUP:NumberBuff ; place to put unreversed number

dffrevloop:
	dec si				; si -> char in reversed number buffer
	mov al,[si]			; get reversed char
	stosb 				; put in unreversed buffer
	loop    dffrevloop ; unreverse as many chars as in number

	mov bx,OFFSET DGROUP:NumberBuff ;  ds:bx -> string to write
	call	DisplayVarStringCRLF

dff2:
	cmp	IsNoCopyrightOption,0	; see if copyright display shut off
	jne	dffret
	mov	bx,OFFSET DGROUP:SuccessText
	call	DisplayTextStringCRLF

dffret:
	ret
DisplayFinalFeedback	ENDP

;*****************************
;* DISPLAYPARSERESULTS       *
;*****************************

; display results of linker parsing
; destroys ax,bx,cx,dx,si,di

DisplayParseResults	PROC

; display link option settings
	mov	bx,OFFSET DGROUP:OptionText
	call	DisplayTextStringNoCRLF

	mov	si,OFFSET DGROUP:OptionList
	mov	bx,STDOUT

optloop:
	cmp	WORD PTR ds:[si],-1	; see if at end of options
	je	endopt			; yes
	mov	di,ds:[si+OPTLISTOFFOPTPTR]	; get pointer to option flag byte
	cmp	BYTE PTR ds:[di],0	; see if option is set
	je	nextopt			; no

	mov	dx,OFFSET DGROUP:SpaceSlash	; precede option with space and slash
	mov	cl,2
	call	DisplayShortString

	mov	di,ds:[si]		; di -> option text string with length byte prefix
	mov	cl,ds:[di]		; cl holds length byte
	mov	dx,di
	inc	dx				; dx -> option text
	call	DisplayShortString

	test	WORD PTR ds:[si+OPTLISTOFFARGFLAGS],STRINGPARAMETER	; see if string parameter
	je	chkword			; no, check if word parameter
	mov	bx,ds:[si+4]	; bx -> string
	call	DisplayVarStringNoCRLF

chkword:
	test	WORD PTR ds:[si+OPTLISTOFFARGFLAGS],DWORDPARAMETER	; see if dword parameter
	je	doterm			; no

; show the dword parameter value
	mov	di,ds:[si+OPTLISTOFFARGPTR]	; di -> dword value
	mov	eax,ds:[di]
	mov	di,OFFSET DGROUP:NumberBuffer
	call	DwordToDecimalString	; convert dword value to string
	mov	bx,OFFSET DGROUP:NumberBuffer
	call	DisplayVarStringNoCRLF	; show string-ized word value

; write string terminating CR/LF
doterm:
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

nextopt:
	add	si,OPTLISTSIZE			; move to next option entry
	jmp	SHORT optloop

endopt:
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

; display EXE name
exedisp:
	mov	bx,OFFSET DGROUP:EXEText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:EXEFileName
	call	DisplayVarStringCRLF

; display MAP name
	cmp	IsMAPOption,OFF	; see if MAP file
	je	dispobj			; no
	mov	bx,OFFSET DGROUP:MAPText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:MAPFileName
	call	DisplayVarStringCRLF

; display object module names
dispobj:
	mov	bx,OFFSET DGROUP:OBJText
	call	DisplayTextStringNoCRLF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString
	mov	cx,TotalOBJCount
	jcxz	displib		; no object modules

	xor	si,si

omodloop:
	push	cx			; save object module count left to print
	mov	di,OFFSET DGROUP:WorkingBuffer
	mov	ds,OBJNameSelector

; ds:si -> object name in storage
; es:di -> destination for name for printing
ocharloop:
	movsb
	cmp	BYTE PTR ds:[si-1],0	; see if null terminator transferred
	jne	ocharloop		; no
	push	DGROUP
	pop	ds				; restore ds -> wl32 data
	mov	bx,OFFSET DGROUP:WorkingBuffer	; bx -> string to print
	call	DisplayVarStringNoCRLF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString
	pop	cx				; get object modules left to print
	loop	omodloop

	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

; display library file names
displib:
	mov	bx,OFFSET DGROUP:LIBText
	call	DisplayTextStringNoCRLF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString
	mov	cx,TotalLIBCount
	jcxz	dpret		; no library files

	xor	si,si

lmodloop:
	push	cx			; save object module count left to print
	mov	di,OFFSET DGROUP:WorkingBuffer
	mov	ds,LIBNameSelector

; ds:si -> object name in storage
; es:di -> destination for name for printing
lcharloop:
	movsb
	cmp	BYTE PTR ds:[si-1],0	; see if null terminator transferred
	jne	lcharloop		; no
	push	DGROUP
	pop	ds				; restore ds -> wl32 data
	mov	bx,OFFSET DGROUP:WorkingBuffer	; bx -> string to print
	call	DisplayVarStringNoCRLF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString
	pop	cx				; get object modules left to print
	loop	lmodloop

	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

dpret:
	ret
DisplayParseResults	ENDP

;*****************************
;* DISPLAYREADFILEFEEDBACK   *
;*****************************

; display reading file message for /i option, if set
; destroys ax,bx,dx

DisplayReadFileFeedback	PROC
	cmp	IsLinkInfoLimitOption,OFF	; see if displaying limited link information
	jne	drf2			; yes
	cmp	IsLinkInfoOption,OFF	; see if displaying link information
	je	drfret			; no

drf2:
	push	cx			; save critical register
	mov	bx,OFFSET DGROUP:ReadFileText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:CurrentFileName
	call	DisplayVarStringNoCRLF
	pop	cx				; restore critical register

drfret:
	ret
DisplayReadFileFeedback	ENDP

;*****************************
;* DISPLAYPROCFILEFEEDBACK   *
;*****************************

; display processing file message for /i option, if set
; destroys ax,bx,cx,dx

DisplayProcFileFeedback	PROC
	cmp	IsLinkInfoLimitOption,OFF	; see if displaying limited link information
	jne	dpfret			; yes, don't show this
	cmp	IsLinkInfoOption,OFF	; see if displaying link information
	je	dpfret			; no
	mov	bx,OFFSET DGROUP:ProcessFileText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:CurrentFileName
	call	DisplayVarStringNoCRLF

dpfret:
	ret
DisplayProcFileFeedback	ENDP

;*****************************
;* DISPLAYLINKINFO           *
;*****************************

; display information message for /i option, if set
; upon entry bx -> message
; destroys bx

DisplayLinkInfo	PROC
	cmp	IsLinkInfoLimitOption,OFF	; see if displaying limited link information
	jne	dli2			; yes, show this
	cmp	IsLinkInfoOption,OFF	; see if displaying link information
	je	dliret			; no

dli2:
	push	ax
	push	cx
	push	dx
	call	DisplayTextStringNoCRLF
	pop	dx
	pop	cx
	pop	ax

dliret:
	ret
DisplayLinkInfo	ENDP

;*****************************
;* DISPLAYMODULENAME         *
;*****************************

; display information message for /i option, if set
; upon entry fs -> i/o buffer base
; destroys ax,bx,cx,dx,gs

DisplayModuleName	PROC
	cmp	IsLinkInfoLimitOption,OFF	; see if displaying limited link information
	jne	dmnret			; yes, don't show this
	cmp	IsLinkInfoOption,OFF	; see if displaying link information
	je	dmnret			; no

	mov	bx,OFFSET DGROUP:ReadModText
	cmp	Pass2Flag,OFF	; see if on pass 2+
	je	dmn2			; no
	mov	bx,OFFSET DGROUP:ProcessModText

dmn2:
	call	DisplayTextStringNoCRLF
	lgs	bx,fs:[IOBuffHeaderStruc.ibhsModNamePtr]	; gs:bx -> module name
	call	NormalGSBXSource	; make sure name is normalized
	push	ds			; save ds -> wl32 data
	push	gs
	pop	ds				; ds:bx -> string to print with length byte prepended
	inc	bx				; point bx past length byte
	call	DisplayTextStringNoCRLF
	pop	ds				; restore ds -> wl32 data
	mov	bx,OFFSET DGROUP:InText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:CurrentFileName
	call	DisplayVarStringNoCRLF

dmnret:
	ret
DisplayModuleName	ENDP

;*****************************
;* DISPLAYSEGMENTNAME        *
;*****************************

; display information message for /i option, if set
; upon entry gs:bx -> master segdef entry
; destroys ax,cx,dx

DisplaySegmentName	PROC
	cmp	IsLinkInfoLimitOption,OFF	; see if displaying limited link information
	jne	dsnret			; yes, don't show this
	cmp	IsLinkInfoOption,OFF	; see if displaying link information
	je	dsnret			; no
	push	gs			; save critical registers
	push	bx
	mov	bx,OFFSET DGROUP:WriteSegText
	call	DisplayTextStringNoCRLF
	pop	bx				; restore bx, save back to stack
	push	bx
	lgs	bx,gs:[bx+MasterSegDefRecStruc.mssNamePtr]	; gs:bx -> segment name
	call	NormalGSBXSource	; make sure name is normalized
	push	ds			; save ds -> wl32 data
	push	gs
	pop	ds				; ds:bx -> string to print with length byte prepended
	inc	bx				; point bx past length byte
	call	DisplayTextStringNoCRLF
	pop	ds				; restore ds -> wl32 data
	pop	bx				; restore critical registers
	pop	gs

dsnret:
	ret
DisplaySegmentName	ENDP

ENDS

END
