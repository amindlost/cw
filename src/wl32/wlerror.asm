;*********************************************************************
;*   WLERROR.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/15/95                                         *
;*   Model:         Small                                            *
;*   Version:       1.3d                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   error handling routines                                         *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLERROR
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Equates                   *
;*****************************

TEXTSTRINGPRINT	EQU	1
WORDVALUEPRINT	EQU	2
ANYTEXTPRINT	EQU	0ffh
FILENAMESTRING	EQU	80h

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLDATA.INC
INCLUDE	WLERRCOD.INC

;*****************************
;* Public declarations       *
;*****************************

; routines
PUBLIC	BadOBJModuleExit
PUBLIC	DOSErrorExit
PUBLIC	InternalErrorExit
PUBLIC	LinkerErrorExit
PUBLIC	MultipleDefSymWarn
PUBLIC	NormalizeErrorExit
PUBLIC	UnknownOptionWarn
PUBLIC	UnresExternalWarn

; variables
PUBLIC	NumberBuffer

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

NumberBuffer	DB	12 DUP (?)	; number to display storage buffer
ErrorWordValue	DW	?	; error routine word value to display

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

ErrorTableLen	DW	ErrorTableStop-ErrorTable
ErrorTable		=	$
	DB	INTERNALERRORCODE
	DB	DOSVERSIONERRORCODE
	DB	RSPLINELENERRORCODE
	DB	BADOPTIONERRORCODE
	DB	RSPNESTLEVELERRORCODE
	DB	MEMALLOCFAILERRORCODE
	DB	MEMSIZEFAILERRORCODE
	DB	NOOBJFILEERRORCODE
	DB	BADOBJRECERRORCODE
	DB	UNSUPOBJRECERRORCODE
	DB	MEMRELEASEFAILERRORCODE
	DB	BADOBJRECLENERRORCODE
	DB	POORFORMOBJERRORCODE
	DB	SEGLEN64KERRORCODE
	DB	BADLIBERRORCODE
	DB	SEG32BITEXEERRORCODE
	DB	CONFIGLINELENERRORCODE
	DB	BADCONFIGLINEERRORCODE
	DB	BADSYMBOLTOKENERRORCODE
ErrorTableStop	=	$

; MUST be in sync with ErrorTable
; first byte is flag byte, second word is pointer to text to print
; set bit 0 then dx -> additional string to print
; set bit 7 the file name to print
ErrorInfo	=	$
	DB	WORDVALUEPRINT
	DW	OFFSET DGROUP:InternalText
	DB	0
	DW	OFFSET DGROUP:DOSVersionText
	DB	TEXTSTRINGPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:RSPLineLenText
	DB	TEXTSTRINGPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:BadOptionText
	DB	TEXTSTRINGPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:RSPNestLevelText
	DB	0
	DW	OFFSET DGROUP:AllocFailText
	DB	0
	DW	OFFSET DGROUP:SizeFailText
	DB	0
	DW	OFFSET DGROUP:NoOBJFileText
	DB	TEXTSTRINGPRINT OR WORDVALUEPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:BadOBJRecText
	DB	TEXTSTRINGPRINT OR WORDVALUEPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:UnsupOBJRecText
	DB	0
	DW	OFFSET DGROUP:ReleaseFailText
	DB	TEXTSTRINGPRINT OR WORDVALUEPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:BadOBJLenText
	DB	TEXTSTRINGPRINT OR WORDVALUEPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:PoorFormOBJText
	DB	TEXTSTRINGPRINT
	DW	OFFSET DGROUP:SegLen64KText
	DB	TEXTSTRINGPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:BadLIBText
	DB	TEXTSTRINGPRINT
	DW	OFFSET DGROUP:Seg32BitEXEText
	DB	TEXTSTRINGPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:ConfigLineLenText
	DB	TEXTSTRINGPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:BadConfigLineText
	DB	TEXTSTRINGPRINT OR WORDVALUEPRINT OR FILENAMESTRING
	DW	OFFSET DGROUP:BadSymbolTokenText

; end specific error code feedback messages

InternalTextLen	DB	InternalTextStop-InternalText
InternalText	DB	'Internal linker error occurred during linking process'
InternalTextStop	=	$

DOSVersionTextLen	DB	DOSVersionTextStop-DOSVersionText
CRLFText	LABEL	BYTE
DOSVersionText	DB	CR,LF,'MS-DOS or PC-DOS version must be 3.0 or above.'
DOSVersionTextStop	=	$

RSPLineLenTextLen	DB	RSPLineLenTextStop-RSPLineLenText
RSPLineLenText	DB	'Response file line length exceeds 253 characters in '
RSPLineLenTextStop	=	$

BadOptionTextLen	DB	BadOptionTextStop-BadOptionText
BadOptionText	DB	'Invalid option: '
BadOptionTextStop	=	$

RSPNestLevelTextLen	DB	RSPNestLevelTextStop-RSPNestLevelText
RSPNestLevelText	DB	'Response files nested more than 10 levels deep in '
RSPNestLevelTextStop	=	$

AllocFailLen	DB	AllocFailStop-AllocFailText
AllocFailText	DB	'Allocate memory attempt failed.'
SpaceText		DB	'  '	; cheap way to reference text of two spaces
				DB	'Probably out of virtual memory.'
AllocFailStop	=	$

SizeFailLen	DB	SizeFailStop-SizeFailText
SizeFailText	DB	'Resize memory attempt failed.  Probably out of virtual memory.'
SizeFailStop	=	$

NoOBJFileLen	DB	NoOBJFileStop-NoOBJFileText
NoOBJFileText	DB	'At least one object module file must be specified when linking.'
NoOBJFileStop	=	$

BadOBJRecLen	DB	BadOBJRecStop-BadOBJRecText
BadOBJRecText	DB	'Bad object record type in '
BadOBJRecStop	=	$

PoorFormOBJLen	DB	PoorFormOBJStop-PoorFormOBJText
PoorFormOBJText	DB	'Poorly formed object record in '
PoorFormOBJStop	=	$

SegLen64KLen	DB	SegLen64KStop-SegLen64KText
SegLen64KText	DB	'Segment size exceeds 64K: '
SegLen64KStop	=	$

BadLIBLen	DB	BadLIBStop-BadLIBText
BadLIBText	DB	'Bad or invalid library (.LIB) file format in '
BadLIBStop	=	$

Seg32BitEXELen	DB	Seg32BitEXEStop-Seg32BitEXEText
Seg32BitEXEText	DB	'32-bit segment in DOS EXE file: '
Seg32BitEXEStop	=	$

ConfigLineLenTextLen	DB	ConfigLineLenTextStop-ConfigLineLenText
ConfigLineLenText	DB	'Configuration file line length exceeds 125 characters in '
ConfigLineLenTextStop	=	$

BadConfigLineTextLen	DB	BadConfigLineTextStop-BadConfigLineText
BadConfigLineText	DB	'Invalid configuration file line: '
BadConfigLineTextStop	=	$

BadSymbolTokenTextLen	DB	BadSymbolTokenTextStop-BadSymbolTokenText
BadSymbolTokenText	DB	'Bad Clipper symbol token in '
BadSymbolTokenTextStop	=	$

; end specific error code feedback messages

UnsupOBJRecLen	DB	UnsupOBJRecStop-UnsupOBJRecText
UnsupOBJRecText	DB	'Unsupported object record type in '
UnsupOBJRecStop	=	$

ReleaseFailLen	DB	ReleaseFailStop-ReleaseFailText
ReleaseFailText	DB	'Release memory attempt failed.'
ReleaseFailStop	=	$

BadOBJLenLen	DB	BadOBJLenStop-BadOBJLenText
BadOBJLenText	DB	'Object record length too large in '
BadOBJLenStop	=	$

UnknownTextLen	DB	UnknownTextStop-UnknownText
UnknownText		DB	'Unknown error occurred during linking process.'
UnknownTextStop	=	$

FatalLinkerTextLen	DB	FatalLinkerTextStop-FatalLinkerText
FatalLinkerText	DB	'FATAL error encountered using '
				DB	'WL32'
				DB	'.'
				DB	CR,LF,'Link terminated.  No executable file created.'
FatalLinkerTextStop	=	$

ValueTextLen	DB	ValueTextStop-ValueText
ValueText		DB	', value '
ValueTextStop	=	$

DOSErrorTextLen		DB	DOSErrorTextStop-DOSErrorText
DOSErrorText		DB	'DOS error'
DOSErrorTextStop	=	$

DOSErrorTextTable	=	$
	DB	2				; file not found
	DB	FILENAMESTRING	; flag file name associated
	DW	OFFSET DGROUP:FileNotFoundText
	DB	3				; path not found
	DB	FILENAMESTRING
	DW	OFFSET DGROUP:PathNotFoundText
	DB	4				; too many open files, no handles left
	DB	FILENAMESTRING
	DW	OFFSET DGROUP:NoHandlesText
	DB	5				; access denied
	DB	FILENAMESTRING
	DW	OFFSET DGROUP:AccessDeniedText
	DB	8				; insufficient memory
	DB	0				; no file name
	DW	OFFSET DGROUP:OutOfMemoryText
	DW	-1				; flags end of table

FileNotFoundTextLen	DB	FileNotFoundTextStop-FileNotFoundText
FileNotFoundText	DB	'File not found'
FileNotFoundTextStop	=	$

PathNotFoundTextLen	DB	PathNotFoundTextStop-PathNotFoundText
PathNotFoundText	DB	'Path not found'
PathNotFoundTextStop	=	$

NoHandlesTextLen	DB	NoHandlesTextStop-NoHandlesText
NoHandlesText	DB	'Too many open files, no handles left'
NoHandlesTextStop	=	$

AccessDeniedTextLen	DB	AccessDeniedTextStop-AccessDeniedText
AccessDeniedText	DB	'Access denied'
AccessDeniedTextStop	=	$

OutOfMemoryTextLen	DB	OutOfMemoryTextStop-OutOfMemoryText
OutOfMemoryText	DB	'Out of memory'
OutOfMemoryTextStop	=	$

WhichFileTextLen	DB	WhichFileTextStop-WhichFileText
WhichFileText		DB	', File: '
WhichFileTextStop	=	$

LeftParenText	DB	'('
RightParenText	DB	')',0	; must have terminating zero for final print

SymDefTextLen	DB	SymDefTextStop-SymDefText
SymDefText		DB	'Symbol defined more than once: '
SymDefTextStop	=	$

DefinedInTextLen	DB	DefinedInTextStop-DefinedInText
DefinedInText	DB	13,10,'Defined in '
DefinedInTextStop	=	$

DuplicatedInTextLen	DB	DuplicatedInTextStop-DuplicatedInText
DuplicatedInText	DB	', duplicated in '
DuplicatedInTextStop	=	$

UnresTextLen	DB	UnresTextStop-UnresText
UnresText		DB	'Unresolved externally declared symbol: '
UnresTextStop	=	$

DeclaredInTextLen	DB	DeclaredInTextStop-DeclaredInText
DeclaredInText	DB	13,10,'Declared in '
DeclaredInTextStop	=	$

UnknownOptionTextLen	DB	UnknownOptionTextStop-UnknownOptionText
UnknownOptionText		DB	'Unknown linker option or command, ignored: '
UnknownOptionTextStop	=	$

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	CompBuffSource:BYTE
EXTRN	CurrentFileName:BYTE
EXTRN	FirstLIBModCount:DWORD
EXTRN	LIBDictTablePtr:WORD
EXTRN	ModuleCount:DWORD
EXTRN	OBJBuffSelTablePtr:WORD
EXTRN	UnresSymPtr:DWORD

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	ByteToHexString:PROC
EXTRN	DisplayTextStringCRLF:PROC
EXTRN	DisplayTextStringNoCRLF:PROC
EXTRN	DisplayVarStringCRLF:PROC
EXTRN	DisplayVarStringNoCRLF:PROC
EXTRN	DisplayShortString:PROC
EXTRN	NormalGSBXSource:PROC
EXTRN	TerminateToDOS:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* LINKERERROREXIT           *
;*****************************

; fatal linker error
; upon entry al holds the error code, dx -> file name, if any
; destroy registers at will

LinkerErrorExit	PROC
	push	DGROUP
	pop	ds				; ds -> wl32 data
	call	LinkerErrorFeedback
;@@@	call	CleanupForExit	; clean up any interim system changes made
	call	TerminateToDOS	; no return

LinkerErrorExit	ENDP

;*****************************
;* LINKERERRORFEEDBACK       *
;*****************************

; display linker error
; upon entry al holds the error code, dx -> text string, if any
;  cx holds error value
; save ax,ds destroy other registers at will

LinkerErrorFeedback	PROC
	push	ax			; save error code

; write string terminating CR/LF
	push	dx			; save critical registers
	push	cx
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString
	pop	cx				; restore critical registers
	pop	dx
	pop	ax
	push	ax			; restore ax

	push	ds
	pop	es				; es -> wl32 data
	mov	ErrorWordValue,cx	; save associated error value, if any
	mov	di,OFFSET DGROUP:ErrorTable	; es:di -> lookup table for errors
	mov	cx,ds:[di-2]	; get number of entries in table
	repne	scasb
	je	founderr		; found the error entry

; error entry not found, unknown error code
	mov	bx,OFFSET DGROUP:UnknownText

ledisp:
	call	DisplayTextStringCRLF

; display fatal error message, exit
lefatal:
	mov	bx,OFFSET DGROUP:FatalLinkerText
	call	DisplayTextStringCRLF
	pop	ax				; restore error code
	ret

; found error entry in ErrorTable, process it for feedback
; di -> entry just past match
founderr:
	dec	di				; di -> matching entry
	sub	di,OFFSET DGROUP:ErrorTable	; di == error code offset
	mov	bx,di
	add	di,di			; word offset
	add	di,bx			; 3 byte (byte+word) offset
	add	di,OFFSET DGROUP:ErrorInfo	; di -> entry in ErrorInfo table
	mov	si,dx			; si -> additional string to print, if any
	mov	bx,ds:[di+1]	; bx -> initial string to print
	test	BYTE PTR ds:[di],ANYTEXTPRINT
	je	ledisp			; no additional text to print, print only and continue

; additional text strings, don't print with CR/LF
	call	DisplayTextStringNoCRLF
	test	BYTE PTR ds:[di],TEXTSTRINGPRINT
	je	le2				; no text string
	mov	bx,si			; bx -> second string to print
	test	BYTE PTR ds:[di],WORDVALUEPRINT
	jne	leno1			; value string, no crlf after text
	call	DisplayVarStringCRLF	; display variable length text string
	jmp	SHORT le2

leno1:
	call	DisplayVarStringNoCRLF	; display variable length text string

le2:
	test	BYTE PTR ds:[di],WORDVALUEPRINT
	je	le3				; no word value string to print

; print word value in hex, value in cx
	mov	bx,OFFSET DGROUP:ValueText
	call	DisplayTextStringNoCRLF	; print 'value' string
	push	di			; save -> error flags
	mov	bx,OFFSET DGROUP:NumberBuffer	; bx -> string to print
	mov	di,bx			; di -> number storage
	mov	al,BYTE PTR ErrorWordValue+1
	call	ByteToHexString	; convert byte in al to hex
	mov	al,BYTE PTR ErrorWordValue
	call	ByteToHexString	; convert byte in al to hex
	mov	ax,'h'			; put hex 'h' identifier and null terminator on number
	stosw
	call	DisplayVarStringCRLF	; display variable length text string
	pop	di				; restore di -> error flags

le3:

;@@@ code goes here

	jmp	SHORT lefatal	; display fatal error message, exit

LinkerErrorFeedback	ENDP

;*****************************
;* DOSERROREXIT              *
;*****************************

; fatal DOS error
; upon entry al holds the error code, dx -> file name, if any
; destroy registers at will

DOSErrorExit	PROC
	push	DGROUP
	pop	ds				; ensure that ds -> data
	call	DOSErrorFeedback
;@@@	call	CleanupForExit	; clean up any interim system changes made
	call	TerminateToDOS	; no return

DOSErrorExit	ENDP

;*****************************
;* DOSERRORFEEDBACK          *
;*****************************

; display linker error
; upon entry al holds the error code, dx -> file name, if any
; save ax
; destroys bx,cx,dx,si,di

DOSErrorFeedback	PROC
	push	ax			; save error code
	push	ds
	pop	es				; es -> wl32 data

	mov	si,dx			; si -> file name to print, if any

; write string terminating CR/LF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

	mov	bx,OFFSET DGROUP:DOSErrorText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:ValueText
	call	DisplayTextStringNoCRLF	; print 'value' string

	pop	ax				; ax == error code (al only)
	push	ax			; restore to stack
	mov	di,OFFSET DGROUP:NumberBuffer	; di -> number storage
	mov	bx,di			; bx -> string to print
	call	ByteToHexString	; convert byte in al to hex
	mov	ax,'h'			; put hex 'h' identifier and null terminator on number
	stosw

; check for extra explanatory information
	pop	ax				; ax == error code (al only)
	push	ax			; restore to stack
	mov	di,OFFSET DGROUP:DOSErrorTextTable

defloop:
	cmp	WORD PTR ds:[di],-1	; see if at end of table to check
	je	defcrlf			; yes, no string to bring, print number with crlf
	cmp	al,ds:[di]		; see if entry in table
	je	deffound		; yes
	add	di,4			; move to next entry
	jmp	SHORT defloop

; extra info to display with DOS error
deffound:
	call	DisplayVarStringNoCRLF	; display error value

; see if file name to print
	test	BYTE PTR ds:[di+1],FILENAMESTRING
	je	defmess			; no file name to print
	mov	bx,OFFSET DGROUP:WhichFileText
	call	DisplayTextStringNoCRLF	; display file text
	mov	bx,si
	call	DisplayVarStringNoCRLF	; display file name
	mov	dx,OFFSET DGROUP:SpaceText
	mov	cl,2
	call	DisplayShortString

; display explanatory message in parentheses following filename or value
defmess:
	mov	dx,OFFSET DGROUP:LeftParenText
	mov	cl,1
	call	DisplayShortString
	mov	bx,ds:[di+2]	; bx -> explanatory message
	call	DisplayTextStringNoCRLF	; show it
	mov	bx,OFFSET DGROUP:RightParenText
	call	DisplayVarStringCRLF	; display final paren and do cr/lf
	jmp	SHORT deffatal	; done, display fatal message

defcrlf:
	call	DisplayVarStringCRLF	; display error value

deffatal:
	mov	bx,OFFSET DGROUP:FatalLinkerText
	call	DisplayTextStringCRLF

	pop	ax				; restore error code
	ret
DOSErrorFeedback	ENDP

;*****************************
;* BADOBJMODULEEXIT          *
;*****************************

; fatal linker error, poorly formed object module
; upon entry cl holds the error value
; set al to proper error code, dx -> obj file name, zero ch,
;  transfer to LinkerErrorExit
; destroy registers at will

BadOBJModuleExit	PROC
	xor	ch,ch			; zero high word of error value
	mov	al,POORFORMOBJERRORCODE
	mov	dx,OFFSET DGROUP:CurrentFileName
	call	LinkerErrorExit
BadOBJModuleExit	ENDP

;*****************************
;* INTERNALERROREXIT         *
;*****************************

; fatal linker error, internal error
; upon entry cl holds the error value
; set al to proper error code, zero ch,
;  transfer to LinkerErrorExit
; destroy registers at will

InternalErrorExit	PROC
	xor	ch,ch			; zero high word of error value
	mov	al,INTERNALERRORCODE
	call	LinkerErrorExit
InternalErrorExit	ENDP

;*****************************
;* NORMALIZEERROREXIT        *
;*****************************

; normalize text string, do linker error exit
; upon entry al holds error code, gs:bx -> non-normalized text string

NormalizeErrorExit	PROC
	push	ax			; save error code
	call	NormalGSBXSource	; normalize text string in gs:bx to CompBuffSource
	mov	ax,gs
	cmp	ax,DGROUP		; see if segment changed (to DGROUP during normalization)
	je	nee2			; yes

; gs:bx -> original string, move to CompBuffSource
	push	ds
	pop	es
	mov	di,OFFSET DGROUP:CompBuffSource	; es:di -> destination
	mov	cl,gs:[bx]		; get count of bytes to transfer
	xor	ch,ch			; zap high byte
	jcxz	nee2		; null name
	mov	si,bx			; gs:si -> name with length byte prefixed
	inc	si				; si -> name

; transfer all name chars
neeloop:
	lods	BYTE PTR gs:[si]
	stosb
	loop	neeloop
	xor	al,al
	stosb				; null terminate the string

nee2:
	pop	ax				; restore error code
	mov	dx,OFFSET DGROUP:CompBuffSource	; dx -> string to print
	call	LinkerErrorExit	; no return

NormalizeErrorExit	ENDP

;*****************************
;* NORMALIZEWARNSTRING      *
;*****************************

; normalize warning text string
; upon entry gs:bx -> non-normalized text string, es=ds=DGROUP
; destroys ax,bx,cx,si,gs

NormalizeWarnString	PROC
	push	di			; save critical registers

	push	gs			; save original gs pointer
	call	NormalGSBXSource	; normalize text string in gs:bx to CompBuffSource
	mov	ax,gs			; ax == new gs
	pop	cx				; cx == original gs
	cmp	ax,cx			; see if segment changed
	jne	nws2			; yes, move to DGROUP occurred

; gs:bx -> original string, move to CompBuffSource
	mov	di,OFFSET DGROUP:CompBuffSource	; es:di -> destination
	mov	cl,gs:[bx]		; get count of bytes to transfer
	xor	ch,ch			; zap high byte
	jcxz	nws2		; null name
	mov	si,bx			; gs:si -> name with length byte prefixed
	inc	si				; si -> name

; transfer all name chars
nwsloop:
	lods	BYTE PTR gs:[si]
	stosb
	loop	nwsloop
	xor	al,al
	stosb				; null terminate the string

nws2:
	pop	di				; restore critical register
	ret
NormalizeWarnString	ENDP

;*****************************
;* MULTIPLEDEFSYMWARN        *
;*****************************

; symbol defined more than once
; display message "Symbol defined more than once: <symbol name>
; Defined in <file name>, duplicated in <filename>
; upon entry gs:di -> old public symbol info, es=ds=DGROUP
; destroys ax,bx,dx

MultipleDefSymWarn	PROC
	push	cx			; save critical registers
	push	si
	push	di
	push	fs
	push	gs
	xor	al,al
	cmp	IsNoWarnDupeOption,al	; see if warning about duplicates shut off
	jne	mdsret			; yes
	cmp	IsNoWarnLIBDupeOption,al	; see if warning about library duplicates shut off
	je	mss2			; no
	cmp	ProcessingLIBFlag,al
	jne	mdsret			; no warning on lib duplicates and processing LIB

; write string terminating CR/LF
mss2:
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

	inc	WarningsCount	; bump count of warnings
	mov	bx,OFFSET DGROUP:SymDefText
	call	DisplayTextStringNoCRLF
	push	gs			; save -> public symbol info
	lgs	bx,gs:[di+PubSymRecStruc.pssNamePtr]	; gs:bx -> symbol name
	call	NormalizeWarnString	; make sure name doesn't straddle i/o buffer, put into DGROUP
	mov	bx,OFFSET DGROUP:CompBuffSource
	call	DisplayVarStringNoCRLF	; display symbol name
	pop	gs				; restore gs -> public symbol info

	mov	bx,OFFSET DGROUP:DefinedInText
	call	DisplayTextStringNoCRLF
	mov	eax,gs:[di+PubSymRecStruc.pssModuleCount]
	cmp	eax,FirstLIBModCount	; see if module is in library
	jb	mssobj			; no
	mov	gs,LIBDictTablePtr
	xor	ebx,ebx			; init library count
	push	es			; save critical register

mssloop:
	mov	fs,gs:[2*ebx]		; fs -> library dictionary
	mov	di,fs:[LIBDictHeaderStruc.ldhsModIDPtr]
	or	di,di			; see if any library modules loaded from library yet
	je	mssnextlib		; no
	mov	es,di
	xor	di,di			; es:di -> modules to check
	mov	cx,fs:[LIBDictHeaderStruc.ldhsModUsedCount]	; number of modules to search
	repne	scasd		; search for module
	je	mssfound		; found the module

mssnextlib:
	inc	ebx				; move to next library
	jmp	SHORT mssloop

; di holds offset into module buffer for module lookup (di-4)/2
;  since dword past match and dword search versus word entry on lookup
mssfound:
	sub	di,4
	shr	di,1			; di holds true offset to module
	mov	gs,fs:[LIBDictHeaderStruc.ldhsModBuffPtr]	; gs -> module i/o buffer
	mov	gs,gs:[di]
	pop	es				; restore critical register
	jmp	SHORT mssname

mssobj:
	mov	bx,ax			; bx holds module count (assumed <64K)
	dec	bx				; make relative zero
	add	bx,bx			; word per module
	mov	gs,OBJBuffSelTablePtr
	mov	gs,gs:[bx]		; gs -> base of first symbol defined object module

mssname:
	lgs	si,gs:[IOBuffHeaderStruc.ibhsFileNamePtr]	; gs:si -> file name
	mov	bx,OFFSET DGROUP:CompBuffSource	; string to printer after transfer
	mov	di,bx			; es:di -> string destination

mdsloop:
	lods	BYTE PTR gs:[si]
	stosb
	or	al,al			; see if null terminator transferred yet
	jne	mdsloop			; no
	call	DisplayVarStringNoCRLF

	mov	bx,OFFSET DGROUP:DuplicatedInText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:CurrentFileName
	call	DisplayVarStringCRLF

mdsret:
	pop	gs				; restore critical registers
	pop	fs
	pop	di
	pop	si
	pop	cx
	ret
MultipleDefSymWarn	ENDP

;*****************************
;* UNRESEXTERNALWARN         *
;*****************************

; unresolved external used in fixup
; display message "Unresolved externally declared symbol: <symbol name>
; Declared in <file name>
; upon entry UnresSymPtr -> symbol entry
; maintain cx,si,gs

UnresExternalWarn	PROC
	push	cx			; save critical registers
	push	si
	push	gs

	lgs	bx,UnresSymPtr	; gs:bx -> symbol info
	test	gs:[bx+PubSymRecStruc.pssFlags],UNRESFEEDSYMBOLFLAG
	jne	uewret			; feedback already given

; write string terminating CR/LF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

	inc	WarningsCount	; bump count of warnings
	mov	bx,OFFSET DGROUP:UnresText
	call	DisplayTextStringNoCRLF

	lgs	bx,UnresSymPtr	; gs:bx -> symbol info
	or		gs:[bx+PubSymRecStruc.pssFlags],UNRESFEEDSYMBOLFLAG	; flag feedback given
	lgs	bx,gs:[bx+PubSymRecStruc.pssNamePtr]	; gs:bx -> symbol name
	call	NormalizeWarnString	; make sure name doesn't straddle i/o buffer, put into DGROUP
	mov	bx,OFFSET DGROUP:CompBuffSource
	call	DisplayVarStringNoCRLF	; display symbol name

	mov	bx,OFFSET DGROUP:DeclaredInText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:CurrentFileName
	call	DisplayVarStringCRLF

uewret:
	pop	gs				; restore critical register
	pop	si
	pop	cx
	ret
UnresExternalWarn	ENDP

;*****************************
;* UNKNOWNOPTIONWARN         *
;*****************************

; unknown linker option
; display message "Unknown linker option or command, ignored: <option/command>"
; upon entry dx -> option/command
; maintain si, es

UnknownOptionWarn	PROC
	push	dx

; write string terminating CR/LF
	mov	dx,OFFSET DGROUP:CRLFText
	mov	cl,2
	call	DisplayShortString

	mov	bx,OFFSET DGROUP:UnknownOptionText
	call	DisplayTextStringNoCRLF
	pop	bx				; bx -> option/command string
	call	DisplayVarStringCRLF
	ret
UnknownOptionWarn	ENDP

ENDS

END
