;*********************************************************************
;*   WLPARCON.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/02/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   linker configuration file parsing routines                      *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLPARCON
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Equates                   *
;*****************************

IGNOREIFONFLAG	EQU	8000h

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE	WLERRCOD.INC
INCLUDE WLDATA.INC

;*****************************
;* Public declarations       *
;*****************************

; procedures
PUBLIC	ParseConfigFile
PUBLIC	SaveMorphData

; debug
PUBLIC	ParseConfigCommand

; variables
PUBLIC	MorphDataBlock
PUBLIC	MorphDataCount

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

ConfigFileHandle	DW	?	; linker configuration file handle
CurrentConfigFileNamePtr	DW	?	; pointer to configuration file name

ConfigFileName	LABEL	BYTE	; configuration file name if prepended EXE path
ConfigFileString	DB	128 DUP (?)	; configuration file line buffer

MorphDataEntry	DW	3 DUP (?)

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

AssignCommands	DW	15
				DW	OFFSET DGROUP:IGNOREText
				DW	IGNOREProc
				DW	OFFSET DGROUP:TERMINATIONText
				DW	TERMINATIONProc
				DW	OFFSET DGROUP:COMMENTText
				DW	COMMENTProc
				DW	OFFSET DGROUP:DEFAULTText
				DW	DEFAULTProc
				DW	OFFSET DGROUP:OBJNAMEText
				DW	OBJNAMEProc
				DW	OFFSET DGROUP:LIBNAMEText
				DW	LIBNAMEProc
				DW	OFFSET DGROUP:LIBSEARCHText
				DW	LIBSEARCHProc
				DW	OFFSET DGROUP:NULLText
				DW	NULLProc
				DW	OFFSET DGROUP:EXENAMEText
				DW	EXENAMEProc
				DW	OFFSET DGROUP:FILEDELETEText
				DW	FILEDELETEProc
				DW	OFFSET DGROUP:OBJADDText
				DW	OBJADDProc
				DW	OFFSET DGROUP:LIBADDText
				DW	LIBADDProc
				DW	OFFSET DGROUP:RESPONSEEXTText
				DW	RESPONSEEXTProc
				DW	OFFSET DGROUP:OBJAPPENDText
				DW	OBJAPPENDProc
				DW	OFFSET DGROUP:ISLIBText
				DW	ISLIBProc

IGNORETextLen	DB	IGNORETextStop-IGNOREText
IGNOREText	DB	'=IGNORE'
IGNORETextStop	=	$

TERMINATIONTextLen	DB	TERMINATIONTextStop-TERMINATIONText
TERMINATIONText	DB	'=TERMINATION'
TERMINATIONTextStop	=	$

COMMENTTextLen	DB	COMMENTTextStop-COMMENTText
COMMENTText	DB	'=COMMENT'
COMMENTTextStop	=	$

DEFAULTTextLen	DB	DEFAULTTextStop-DEFAULTText
DEFAULTText	DB	'=DEFAULT'
DEFAULTTextStop	=	$

OBJNAMETextLen	DB	OBJNAMETextStop-OBJNAMEText
OBJNAMEText	DB	'=OBJNAME'
OBJNAMETextStop	=	$

LIBNAMETextLen	DB	LIBNAMETextStop-LIBNAMEText
LIBNAMEText	DB	'=LIBNAME'
LIBNAMETextStop	=	$

LIBSEARCHTextLen	DB	LIBSEARCHTextStop-LIBSEARCHText
LIBSEARCHText	DB	'=LIBSEARCH'
LIBSEARCHTextStop	=	$

NULLTextLen	DB	NULLTextStop-NULLText
NULLText	DB	'=NULL'
NULLTextStop	=	$

EXENAMETextLen	DB	EXENAMETextStop-EXENAMEText
EXENAMEText	DB	'=EXENAME'
EXENAMETextStop	=	$

FILEDELETETextLen	DB	FILEDELETETextStop-FILEDELETEText
FILEDELETEText	DB	'=FILEDELETE'
FILEDELETETextStop	=	$

OBJADDTextLen	DB	OBJADDTextStop-OBJADDText
OBJADDText	DB	'=OBJADD'
OBJADDTextStop	=	$

LIBADDTextLen	DB	LIBADDTextStop-LIBADDText
LIBADDText	DB	'=LIBADD'
LIBADDTextStop	=	$

RESPONSEEXTTextLen	DB	RESPONSEEXTTextStop-RESPONSEEXTText
RESPONSEEXTText	DB	'=RESPONSEEXT'
RESPONSEEXTTextStop	=	$

OBJAPPENDTextLen	DB	OBJAPPENDTextStop-OBJAPPENDText
OBJAPPENDText	DB	'=OBJAPPEND'
OBJAPPENDTextStop	=	$

ISLIBTextLen	DB	ISLIBTextStop-ISLIBText
ISLIBText	DB	'=ISLIB'
ISLIBTextStop	=	$

StandAloneCommands	DW	1
				DW	OFFSET DGROUP:FREEFORMATText
				DW	FREEFORMATProc

FREEFORMATTextLen	DB	FREEFORMATTextStop-FREEFORMATText
FREEFORMATText	DB	'FREEFORMAT'
FREEFORMATTextStop	=	$

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

IgnoreCharCount	DW	0	; running ignore char count, used as offset, will wrap after 15
MorphBlockSize	DW	0	; current size of morphing data block
MorphBlockUsed	DW	0	; used size of morphing data block
MorphDataBlock	DW	0	; selector of morphing data block
						; entries in block follow this format
						; word -- length of entry
						; word -- flags
						; word -- pointer to option entry morphed to (if any)
						; variable -- option/command text string
MorphDataCount	DW	0	; count of entries in morphing data block

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	CommandLinePos:WORD
EXTRN	DefaultLIBAddFlag:BYTE
EXTRN	FileListFlag:BYTE
EXTRN	FreeFormatFlag:BYTE
EXTRN	LinkFileExtensionText:BYTE
EXTRN	OptionList:WORD
EXTRN	ParseTermChar:BYTE
EXTRN	RSPFileCommentChar:BYTE
EXTRN	RSPFileIgnoreChar:BYTE
EXTRN	RSPFileNestLevel:WORD

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	AllocateMemory:PROC,ResizeMemory:PROC
EXTRN	CheckCurrentDirectory:PROC
EXTRN	DOSErrorExit:PROC
EXTRN	InternalErrorExit:PROC
EXTRN	LinkerErrorExit:PROC
EXTRN	OpenFile:PROC,ReadFile:PROC
EXTRN	ParseLinkOption:PROC
EXTRN	SaveOBJLIBFileName:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* PARSECONFIGFILE           *
;*****************************

; open, read/parse linker option configuration file, close
; upon entry ds:bx -> option entry, es -> DGROUP
; maintain ds,es,si

ParseConfigFile	PROC
	push	si			; save critical register
	push	bx			; save bx -> option
	or	WORD PTR ds:[bx+OPTLISTOFFARGFLAGS],IGNOREIFONFLAG	; turn on ignore flag
	mov	dx,ds:[bx+OPTLISTOFFARGPTR]	; ds:dx -> file name
	mov	CurrentConfigFileNamePtr,dx	; keep pointer to configuration file name in case of error
	call	CheckCurrentDirectory
	jnc	pcfopen			; configuration file is present

; check EXE directory for configuration file, if no explicit path given
	mov	di,dx
	cmp	BYTE PTR ds:[di+1],':'	; see if drive spec
	je	pcfopen			; yes, force error
	mov	al,'\'

pcfscanloop:
	scasb
	je	pcfopen			; pathspec
	cmp	BYTE PTR ds:[di-1],0	; see if at end of configuration file name
	jne	pcfscanloop			; no

; seek out EXE pathspec from environment block, use it with configuration file name
	mov	ax,PSP
	mov	ds,ax
	mov	ds,ds:[2ch]
	xor	si,si			; ds:si -> start of environment

pcfendloop:
	lodsb				; get environment char
	or	al,ds:[si]		; merge in next char
	jne	pcfendloop		; not at end of environment block
	add	si,3			; si -> start of EXE file path
	mov	di,si			; save -> start

pcfpathloop:
	lodsb				; get char of file path
	or	al,al			; see if at end
	je	calcpath		; yes
	cmp	al,'\'			; see if directory indicator
	jne	pcfpathloop		; no
	mov	bx,si			; save -> char past directory
	jmp	SHORT pcfpathloop

calcpath:
	mov	si,di			; ds:si -> start of file path
	mov	di,OFFSET DGROUP:ConfigFileName	; es:di -> configuration file name slot

pcfcalcloop:
	movsb
	cmp	si,bx			; see if at end of path
	jne	pcfcalcloop		; no
	push	es
	pop	ds				; ds -> wl32 data
	mov	si,dx			; ds:si -> configuration file name without path

pcfappendloop:
	movsb
	cmp	BYTE PTR [si-1],0	; transfer through null terminator
	jne	pcfappendloop
	mov	dx,OFFSET DGROUP:ConfigFileName	; ds:dx -> configuration file with pathspec

pcfopen:
	mov	al,40h			; read-only, deny none access
	call	OpenFile	; open response file, ax == handle
	mov	ConfigFileHandle,ax

pcfreadloop:
	call	ReadConfigFile	; read config file
	mov	si,OFFSET DGROUP:ConfigFileString

pcflineloop:
	lodsb				; get first char of line
	or	al,al			; see if at end of file
	je	pcfend			; yes
	cmp	al,27			; see if past end of line
	je	pcfreadloop		; yes, read next lines, if any, from configuration file
	cmp	al,' '			; see if whitespace
	ja	pcfcommand		; no, command line
	cmp	al,CR			; see if completely blank line
	jne	pcflineloop		; not yet
	inc	si				; bump past following LF
	jmp	SHORT pcflineloop

; ds:si -> configuration file command line+1
pcfcommand:
	call	ParseConfigCommand	; parse out the command
	jmp	SHORT pcflineloop	; get next command, if any

pcfend:
	mov	bx,ConfigFileHandle
	mov	ah,3eh			; close file
	int	21h
	pop	bx				; restore bx -> option
	and	WORD PTR ds:[bx+OPTLISTOFFARGFLAGS],NOT IGNOREIFONFLAG	; shut off ignore flag
	pop	si				; restore critical register
	ret
ParseConfigFile	ENDP

;*****************************
;* READCONFIGFILE            *
;*****************************

; read from linker configuration file
; destroys ax,bx,cx,dx,di

ReadConfigFile	PROC
	mov	dx,OFFSET DGROUP:ConfigFileString	; ds:dx -> read buffer
	mov	cx,127			; read 127 bytes of response file
	mov	bx,ConfigFileHandle
	call	ReadFile	; read the file

; if short read (ax<127) then end of file reached, place zero terminator
	cmp	ax,127			; see if short read
	jb	ShortRead		; yes

; scan for final CR/LF pair, place ESC char after it
	mov	cx,126			; scan 126 bytes of response file characters (not final byte)
	mov	di,dx			; es:di -> buffer
	mov	al,CR
	repne	scasb		; look for CR
	jne	lineerr			; not found, response file line too long (>125 chars not counting CR/LF)

ScanLoop:
	mov	dx,di			; dx -> CR char+1 (should be LF)
	jcxz	ScanDone	; no more chars to scan
	repne	scasb		; look for more CR's
	je	ScanLoop		; found one

ScanDone:
	mov	di,dx			; get LF offset
	mov	BYTE PTR ds:[di+1],27	; write ESC past LF character

;  file seek to character position after final CR/LF
	sub	dx,OFFSET DGROUP:ConfigFileString	; dx == relative offset in buffer
	mov	ax,126
	sub	ax,dx			; ax == chars to reread on next pass (seek back from)
	je	rcfret			; no bytes to reread
	mov	dx,ax			; chars to reread in dx
	neg	dx				; make it a negative number
	mov	cx,0ffffh		; sign extend thru cx
	mov	ax,4201h		; move file pointer from current location
	int	21h				; rewind file

rcfret:
	ret

; linker error, response file line too long
lineerr:
	mov	al,CONFIGLINELENERRORCODE
	mov	dx,CurrentConfigFileNamePtr	; dx -> file name
	call	LinkerErrorExit

; read to end of response file
ShortRead:
	mov	bx,ax			; offset into buffer
	mov	BYTE PTR ds:[bx+OFFSET DGROUP:ConfigFileString],0	; null terminate after final character read
	jmp	SHORT rcfret

ReadConfigFile	ENDP

;*****************************
;* PARSECONFIGCOMMAND        *
;*****************************

; parse out configuration file command
; upon entry ds:si -> configuration file line+1
; updates si -> past command
; destroys all registers except segment registers

ParseConfigCommand	PROC
	dec	si				; back up to first char of configuration file line
	mov	dx,si			; keep -> start of configuration file line
	cmp	WORD PTR ds:[si],'/'+(256*'/')	; check for // comment
	jne	pccendloop
	mov	dx,0ffffh

pccendloop:
	lodsb				; scan to end of command
	cmp	al,' '
	jae	pccendloop		; not at end of command
	dec	si				; si -> char past end command
	push	si			; save -> past command
	cmp	dx,0ffffh		; // comment
	je	pccret
;	xor	bx,bx			; init found = position

pccstartloop:
	dec	si				; si -> final command char
	cmp	si,dx			; see if at start of command line

;	ja	pccchkeq		; no
;	or	bx,bx			; see if previous '=' found
;	je	pccnoarg		; no, no argument command
;	mov	si,bx			; si -> last '='
;	jmp	SHORT pccchkobj
	jbe	pccnoarg		; at start of command line

pccchkeq:
	cmp	BYTE PTR ds:[si],'='	; see if reached assignment
	jne	pccstartloop	; no
;	mov	bx,si			; keep -> '='
;	jmp	SHORT pccstartloop

pccchkobj:
	cmp	DWORD PTR ds:[si+1],'JBO+'
	jne	pccnotobj
	cmp	BYTE PTR ds:[si+5],':'
	jne	pccbadcom

	add	si,6			; si -> first char of filename
	call	FileAddOBJProc	; process file add command
	jmp	SHORT pccret	; processing successful

pccnotobj:
;	cmp	DWORD PTR ds:[si+1],'BIL+'
;	jne	pccnotlib
;	cmp	BYTE PTR ds:[si+5],':'
;	jne	pccbadcom
;@@@ code goes here
;	jmp	SHORT pccret	; processing successful

pccnotlib:
	cmp	BYTE PTR ds:[si+1],'@'	; see if environment variable dependent link file
	jne	pccchkev
	inc	si				; si -> '@'
	call	EvarLinkFileProc	; process file add command
	jmp	SHORT pccret	; processing successful

pccchkev:
	cmp	WORD PTR ds:[si+1],'+'+(256*'/')
	jne	pccnotadd
	
; add option morphing
	add	si,3			; si -> first char of option
	call	AddOptionProc	; process option
	jc	pccbadcom		; bad option listed
	jmp	SHORT pccret	; processing successful

pccnotadd:
	cmp	BYTE PTR ds:[si+1],'/'	; see if option morphing
	jne	pccnotopt		; no
	add	si,2			; si -> first char of option
	call	OptionProc	; process option
	jc	pccbadcom		; bad option listed
	jmp	SHORT pccret	; processing successful

pccnotopt:
	mov	bx,OFFSET DGROUP:AssignCommands

; ds:si -> start of command
pccsearch:
	mov	ax,si			; save -> start of command
	mov	bp,[bx]			; get number of assignment commands
	sub	bx,2			; adjust for entry bx 2-word increment

pcccomploop:
	mov	si,ax			; ds:si -> start of command
	add	bx,4			; bx -> assignment command string
	mov	di,[bx]			; es:di -> assignment command string to try
	xor	ch,ch
	mov	cl,ds:[di-1]	; cx holds # of chars in string
	repe	cmpsb		; see if match
	je	pccmatch		; found a match
	dec	bp				; drop count of strings to check
	jne	pcccomploop		; more strings to check

; matched no strings, bad command, dx -> configuration file line
pccbadcom:
	mov	si,dx

pccbadloop:
	lodsb				; scan to end of command
	cmp	al,' '
	ja	pccbadloop		; not at end of command
	mov	BYTE PTR ds:[si-1],0	; null terminate line
	mov	al,BADCONFIGLINEERRORCODE
	call	LinkerErrorExit

; matched existing command
pccmatch:
	call	WORD PTR ds:[bx+2]	; route to appropriate routine

pccret:
	pop	si				; si -> past command
	ret

; at start of command, this command has no argument translations
pccnoarg:
	mov	bx,OFFSET DGROUP:StandAloneCommands
	jmp	SHORT pccsearch	; search for command

ParseConfigCommand	ENDP

;*****************************
;* OPTIONPROC                *
;*****************************

; process option
; upon entry ds:si -> first char of option past '/', dx -> configuration line
; returns carry flag set if bad option, reset if valid option
; maintain dx

OptionProc	PROC
	push	dx			; save critical register

	mov	bx,OFFSET DGROUP:OptionList	; ds:bx -> link options
	mov	dx,si			; dx -> start of option

opmainloop:
	mov	di,ds:[bx]		; ds:di -> option text string prefixed by length byte
	cmp	di,-1			; see if end of option list
	je	opfail			; yes

	mov	cl,ds:[di]
	xor	ch,ch			; cx holds length of text string
	inc	di				; es:di -> option on record
	mov	si,dx			; ds:si -> option to match

opcharloop:
	lodsb
	cmp	al,'a'			; check lower bounds
	jb	op2				; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	op2				; not lowercase
	sub	al,20h			; force lowercase to uppercase

op2:
	scasb				; see if matches option text string
	jne	opnext			; no, try next
	loop	opcharloop	; yes, check next option char

; options matched thru all chars, see if any more chars in given option string
	cmp	BYTE PTR ds:[si],' '
	jbe	opsuccess		; no, successful match

opnext:
	add	bx,OPTLISTSIZE		; move to next option to check
	jmp	SHORT opmainloop	; reset pointers

; valid option
opsuccess:
	mov	si,dx
	sub	si,2			; si -> '='
	mov	BYTE PTR ds:[si],0	; null terminate after option chars (replace '=')
	dec	si				; si -> last char of default option
	pop	dx				; dx -> start of configuration line
	push	dx			; save back to stack
	mov	al,' '

opbackloop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	opsave			; yes
	cmp	[si-1],al		; see if at start of default option
	je	opsave			; yes
	dec	si
	jmp	SHORT opbackloop

opsave:
	mov	eax,OPTIONFLAG	; set flags
	mov	dx,bx			; dx -> option entry
	call	SaveMorphData	; save the morphing data/string
	clc

opret:
	pop	dx				; restore critical register
	ret

; invalid option
opfail:
	stc
	jmp	SHORT opret

OptionProc	ENDP

;*****************************
;* ADDOPTIONPROC             *
;*****************************

; process add option
; upon entry ds:si -> first char of option past '/+', dx -> configuration line
; returns carry flag set if bad option, reset if valid option
; maintain dx

AddOptionProc	PROC
	push	dx			; save critical register

	mov	bx,OFFSET DGROUP:OptionList	; ds:bx -> link options
	mov	dx,si			; dx -> start of option

aopmainloop:
	mov	di,ds:[bx]		; ds:di -> option text string prefixed by length byte
	cmp	di,-1			; see if end of option list
	je	aopfail			; yes

	mov	cl,ds:[di]
	xor	ch,ch			; cx holds length of text string
	inc	di				; es:di -> option on record
	mov	si,dx			; ds:si -> option to match

aopcharloop:
	lodsb
	cmp	al,'a'			; check lower bounds
	jb	aop2				; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	aop2				; not lowercase
	sub	al,20h			; force lowercase to uppercase

aop2:
	scasb				; see if matches option text string
	jne	aopnext			; no, try next
	loop	aopcharloop	; yes, check next option char

; options matched thru all chars, see if any more chars in given option string
	cmp	BYTE PTR ds:[si],' '
	jbe	aopsuccess		; no, successful match

aopnext:
	add	bx,OPTLISTSIZE		; move to next option to check
	jmp	SHORT aopmainloop	; reset pointers

; valid option
aopsuccess:
	mov	si,dx
	sub	si,3			; si -> '='
	mov	BYTE PTR ds:[si],0	; null terminate after option chars (replace '=')
	dec	si				; si -> last char of default option
	pop	dx				; dx -> start of configuration line
	push	dx			; save back to stack
	mov	al,' '

aopbackloop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	aopsave			; yes
	cmp	[si-1],al		; see if at start of default option
	je	aopsave			; yes
	dec	si
	jmp	SHORT aopbackloop

aopsave:
	mov	eax,ADDOPTIONFLAG OR NOTCOMMANDFLAG OR STACKABLEFLAG	; set flags
	mov	dx,bx			; dx -> option entry
	call	SaveMorphData	; save the morphing data/string
	clc

aopret:
	pop	dx				; restore critical register
	ret

; invalid option
aopfail:
	stc
	jmp	SHORT aopret

AddOptionProc	ENDP

;*****************************
;* IGNOREPROC                *
;*****************************

; process IGNORE command
; upon entry ds:ax -> start of command

IGNOREProc	PROC
	mov	si,ax
	dec	si				; si -> ignore char
	mov	al,ds:[si]		; al holds ignore char
	mov	si,IgnoreCharCount
	mov	[si+OFFSET DGROUP:RSPFileIgnoreChar],al	; save the char to ignore
	mov	ax,si
	inc	ax
	and	al,0fh			; make maximum ignore count 16 (15 relative 0)
	mov	IgnoreCharCount,ax	; update ignore char count
	ret
IGNOREProc	ENDP

;*****************************
;* TERMINATIONPROC           *
;*****************************

; process TERMINATION command
; upon entry ds:ax -> start of command

TERMINATIONProc	PROC
	mov	si,ax
	dec	si				; si -> termination char
	mov	al,ds:[si]		; al holds termination char
	mov	ParseTermChar,al	; save it
	ret
TERMINATIONProc	ENDP

;*****************************
;* COMMENTPROC               *
;*****************************

; process COMMENT command
; upon entry ds:ax -> start of command

COMMENTProc	PROC
	mov	si,ax
	dec	si				; si -> comment char
	mov	al,ds:[si]		; al holds comment char
	mov	RSPFileCommentChar,al	; save it
	ret
COMMENTProc	ENDP

;*****************************
;* DEFAULTPROC               *
;*****************************

; process DEFAULT command
; upon entry ds:ax -> start of command, dx -> configuration line

DEFAULTProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after option chars (replace '=')
	dec	si				; si -> last char of default option
	mov	al,'/'

dploop:
	cmp	[si],al			; see if at start of default option
	je	dpstart			; yes
	cmp	si,dx			; see if at beginning of configuration line
	jbe	dpbad			; yes, bad option listed as default (no '/')
	dec	si
	jmp	SHORT dploop

; ds:si -> start of default option
; check if valid, set if so, error out if not
dpstart:
	lodsb				; al =='/', ds:si -> first option char
	push	RSPFileNestLevel	; save temporarily modified variables
	push	CommandLinePos
	mov	RSPFileNestLevel,0	; temporarily zero nest level
	call	ParseLinkOption	; parse the link option
	pop	CommandLinePos	; restore temporarily modified variables
	pop	RSPFileNestLevel
	ret

dpbad:
	jmp	NEAR PTR pccbadcom	; list bad command

DEFAULTProc	ENDP

;*****************************
;* FREEFORMATPROC            *
;*****************************

; process FREEFORMAT command

FREEFORMATProc	PROC
	mov	FreeFormatFlag,ON	; set freeformat flag
	ret
FREEFORMATProc	ENDP

;*****************************
;* OBJNAMEPROC               *
;*****************************

; process OBJNAME command
; upon entry ds:ax -> start of command, dx -> configuration line

OBJNAMEProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after name chars (replace '=')
	dec	si				; si -> last char of name
	mov	al,' '

onploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	onpsave			; yes
	cmp	[si-1],al		; see if at start of name
	je	onpsave			; yes
	dec	si
	jmp	SHORT onploop

; ds:si -> start of objname morph string to save
; check if valid, set if so, error out if not
onpsave:
	mov	eax,FREEFORMATONLYFLAG OR OBJNAMEFLAG	; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	ret
OBJNAMEProc	ENDP

;*****************************
;* LIBNAMEPROC               *
;*****************************

; process LIBNAME command
; upon entry ds:ax -> start of command, dx -> configuration line

LIBNAMEProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after name (replace '=')
	dec	si				; si -> last char of name
	mov	al,' '

lnploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	lnpsave			; yes
	cmp	[si-1],al		; see if at start of name
	je	lnpsave			; yes
	dec	si
	jmp	SHORT lnploop

; ds:si -> start of libname morph string to save
; check if valid, set if so, error out if not
lnpsave:
	mov	eax,FREEFORMATONLYFLAG OR LIBNAMEFLAG	; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	ret
LIBNAMEProc	ENDP

;*****************************
;* LIBSEARCHPROC         *
;*****************************

; process LIBSEARCH command
; upon entry ds:ax -> start of command, dx -> configuration line

LIBSEARCHProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after name (replace '=')
	dec	si				; si -> last char of name
	mov	al,' '

lnsploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	lnspsave		; yes
	cmp	[si-1],al		; see if at start of name
	je	lnspsave		; yes
	dec	si
	jmp	SHORT lnsploop

; ds:si -> start of libname morph string to save
; check if valid, set if so, error out if not
lnspsave:
	mov	eax,FREEFORMATONLYFLAG OR LIBNAMEFLAG OR LIBSEARCHFLAG	; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	ret
LIBSEARCHProc	ENDP

;*****************************
;* NULLPROC               *
;*****************************

; process NULL command
; upon entry ds:ax -> start of command, dx -> configuration line

NULLProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after null chars (replace '=')
	dec	si				; si -> last char of null
	mov	al,' '

nploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	npsave			; yes
	cmp	[si-1],al		; see if at start of null
	je	npsave			; yes
	dec	si
	jmp	SHORT nploop

; ds:si -> start of libname morph string to save
; check if valid, set if so, error out if not
npsave:
	xor	eax,eax			; no flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	ret
NULLProc	ENDP

;*****************************
;* EXENAMEPROC               *
;*****************************

; process EXENAME command
; upon entry ds:ax -> start of command, dx -> configuration line

EXENAMEProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after name chars (replace '=')
	dec	si				; si -> last char of name
	mov	al,' '

enploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	enpsave			; yes
	cmp	[si-1],al		; see if at start of name
	je	enpsave			; yes
	dec	si
	jmp	SHORT enploop

; ds:si -> start of exename morph string to save
; check if valid, set if so, error out if not
enpsave:
	mov	eax,FREEFORMATONLYFLAG OR EXENAMEFLAG	; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	ret
EXENAMEProc	ENDP

;*****************************
;* FILEDELETEPROC            *
;*****************************

; process FILEDELETE command
; upon entry ds:ax -> start of command, dx -> configuration line

FILEDELETEProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after name chars (replace '=')
	dec	si				; si -> last char of name
	mov	al,' '

fdploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	fdpsave			; yes
	cmp	[si-1],al		; see if at start of name
	je	fdpsave			; yes
	dec	si
	jmp	SHORT fdploop

; ds:si -> start of delete file string to save
; check if valid, set if so, error out if not
fdpsave:
	mov	eax,FILEDELETEFLAG OR NOTCOMMANDFLAG OR STACKABLEFLAG ; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	ret
FILEDELETEProc	ENDP

;*****************************
;* OBJADDPROC                *
;*****************************

; process OBJADD command
; upon entry ds:ax -> start of command, dx -> configuration line

OBJADDProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after name chars (replace '=')
	dec	si				; si -> last char of name
	mov	al,' '

oaploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	oapsave			; yes
	cmp	[si-1],al		; see if at start of name
	je	oapsave			; yes
	dec	si
	jmp	SHORT oaploop

; ds:si -> start of objname morph string to save
; check if valid, set if so, error out if not
oapsave:
	push	si			; save -> object module name
	mov	eax,FREEFORMATONLYFLAG OR OBJADDFLAG	; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	pop	si				; si -> library name
	mov	al,FreeFormatFlag
	mov	ah,FileListFlag	; save original flag values
	push	ax
	mov	FreeFormatFlag,ON
	mov	FileListFlag,1	; flag object module
	lodsb				; setup al,si for SaveOBJLIBFileName call
	call	SaveOBJLIBFileName
	pop	ax
	mov	FileListFlag,ah
	mov	FreeFormatFlag,al
	ret
OBJADDProc	ENDP

;*****************************
;* OBJAPPENDPROC             *
;*****************************

; process OBJAPPEND command
; upon entry ds:ax -> start of command, dx -> configuration line

OBJAPPENDProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after name chars (replace '=')
	dec	si				; si -> last char of name
	mov	al,' '

opploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	oppsave			; yes
	cmp	[si-1],al		; see if at start of name
	je	oppsave			; yes
	dec	si
	jmp	SHORT opploop

; ds:si -> start of delete file string to save
; check if valid, set if so, error out if not
oppsave:
	mov	eax,OBJAPPENDFLAG ; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	ret
OBJAPPENDProc	ENDP

;*****************************
;* ISLIBPROC                 *
;*****************************

; process ISLIB command
; upon entry ds:ax -> start of command, dx -> configuration line

ISLIBProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after name chars (replace '=')
	dec	si				; si -> last char of name
	mov	al,' '

ilploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	ilpsave			; yes
	cmp	[si-1],al		; see if at start of name
	je	ilpsave			; yes
	dec	si
	jmp	SHORT ilploop

; ds:si -> start of is library file string to save
; check if valid, set if so, error out if not
ilpsave:
	mov	eax,ISLIBFLAG OR NOTCOMMANDFLAG OR STACKABLEFLAG	; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	ret
ISLIBProc	ENDP

;*****************************
;* LIBADDPROC                *
;*****************************

; process LIBADD command
; upon entry ds:ax -> start of command, dx -> configuration line

LIBADDProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after name chars (replace '=')
	dec	si				; si -> last char of name
	mov	al,' '

laploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	lapsave			; yes
	cmp	[si-1],al		; see if at start of name
	je	lapsave			; yes
	dec	si
	jmp	SHORT laploop

; ds:si -> start of libname morph string to save
; check if valid, set if so, error out if not
lapsave:
	push	si			; save -> library name
	mov	eax,FREEFORMATONLYFLAG OR LIBADDFLAG	; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string
	pop	si				; si -> library name
	mov	al,FreeFormatFlag
	mov	ah,FileListFlag	; save original flag values
	push	ax
	mov	FreeFormatFlag,ON
	mov	FileListFlag,2	; flag library
	mov	DefaultLIBAddFlag,OFF	; flag not a default library
	lodsb				; setup al,si for SaveOBJLIBFileName call
	call	SaveOBJLIBFileName
	pop	ax
	mov	FileListFlag,ah
	mov	FreeFormatFlag,al
	ret
LIBADDProc	ENDP

;*****************************
;* RESPONSEEXTPROC           *
;*****************************

; process RESPONSEEXT command
; upon entry ds:ax -> start of command, dx -> configuration line

RESPONSEEXTProc	PROC
	mov	si,ax
	mov	BYTE PTR ds:[si],0	; null terminate after null chars (replace '=')
	dec	si				; si -> last char of null
	mov	al,' '

reploop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	repsave			; yes
	cmp	[si-1],al		; see if at start of null
	je	repsave			; yes
	dec	si
	jmp	SHORT reploop

; ds:si -> start of default link file extension
repsave:
	push	es			; save critical register
	push	ds
	pop	es
	mov	di,OFFSET DGROUP:LinkFileExtensionText	; es:di -> link file extension storage
	movsd				; save extension
	pop	es				; restore critical register
	ret
RESPONSEEXTProc	ENDP

;*****************************
;* FILEADDOBJPROC            *
;*****************************

; process +OBJ: command
; upon entry ds:si -> first char of filename past '+OBJ:',
;  dx -> configuration line
; maintain dx

FileAddOBJProc	PROC
	push	dx			; save critical register
	push	MorphBlockUsed	; save -> start of current morphing storage
	mov	eax,PARAMETERFLAG	; flag parameter pointed to
	xor	dx,dx			; no option pointer
	push	si			; save -> start of filename
	call	SaveMorphData	; save the morphing data string

	pop	si				; si -> start of filename
	sub	si,6			; si -> '='
	mov	BYTE PTR ds:[si],0	; null terminate after option chars (replace '=')
	dec	si				; si -> last char of default option
	pop	ax				; get morphing pointer out of way
	pop	dx				; dx -> start of configuration line
	push	dx			; save configuration -> back to stack
	push	ax			; restore morphing pointer
	mov	al,' '

faopbackloop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	faopsave		; yes
	cmp	[si-1],al		; see if at start of default option
	je	faopsave		; yes
	dec	si
	jmp	SHORT faopbackloop

faopsave:
	mov	eax,FILEADDOBJFLAG OR NOTCOMMANDFLAG OR STACKABLEFLAG	; set flags
	pop	dx				; previously current morphing location (OBJ filename)
	add	dx,8			; adjust past system words
	call	SaveMorphData	; save the morphing data/string

	pop	dx				; restore critical register
	ret
FileAddOBJProc	ENDP

;*****************************
;* EVARLINKFILEPROC          *
;*****************************

; process @<linkfile> command
; upon entry ds:si -> @
;  dx -> configuration line
; maintain dx

EvarLinkFileProc	PROC
	push	dx			; save critical register
	push	MorphBlockUsed	; save -> start of current morphing storage
	mov	eax,PARAMETERFLAG	; flag parameter pointed to
	xor	dx,dx			; no option pointer
	push	si			; save -> @ preceded filename
	call	SaveMorphData	; save the morphing data string

	pop	si				; si -> @ preceded filename
	dec	si				; si -> '='
	mov	BYTE PTR ds:[si],0	; null terminate after option chars (replace '=')
	dec	si				; si -> last char of default option
	pop	ax				; get morphing pointer out of way
	pop	dx				; dx -> start of configuration line
	push	dx			; save configuration -> back to stack
	push	ax			; restore morphing pointer
	mov	al,' '

elfpbackloop:
	cmp	si,dx			; see if at beginning of configuration line
	jbe	elfpsave		; yes
	cmp	[si-1],al		; see if at start of default option
	je	elfpsave		; yes
	dec	si
	jmp	SHORT elfpbackloop

elfpsave:
	mov	eax,EVARLINKFILEFLAG OR NOTCOMMANDFLAG	; set flags
	pop	dx				; previously current morphing location (e-var setting)
	add	dx,8			; adjust past system words
	call	SaveMorphData	; save the morphing data/string

	pop	dx				; restore critical register
	ret
EvarLinkFileProc	ENDP

;*****************************
;* SAVEMORPHDATA             *
;*****************************

; save morphing data string
; upon entry ds:si -> string, eax holds flags, dx holds option pointer, if any
; destroys ax,bx,cx,dx,di,si

SaveMorphData	PROC
	push	es			; save critical register
	push	dx			; save option pointer
	push	eax			; save flags
	cmp	MorphDataBlock,0	; see if need to allocate data block
	jne	smd2			; no
	mov	dx,2048
	mov	MorphBlockSize,dx	; save current morphing data block size
	call	AllocateMemory	; allocate the block
	mov	MorphDataBlock,ax	; save block selector

smd2:
	xor	ax,ax
	mov	cl,255			; zero terminator known <255 bytes away
	mov	di,si			; es:di -> morph string
	repne	scasb

	cmp	BYTE PTR ds:[di-2],'*'	; see if flush to end of line
	jne	smdchkplus		; no
	pop	eax				; get flags
	or	eax,FLUSHLINEFLAG	; add flush line flag
	push	eax			; save modified flag status back to stack
	jmp	SHORT smdcount

smdchkplus:
	cmp	BYTE PTR ds:[di-2],'+'	; see if flush to end of option
	jne	smdcount		; no
	pop	eax				; get flags
	or	eax,FLUSHOPTIONFLAG	; add flush option flag
	push	eax			; save modified flag status back to stack

smdcount:
	sub	di,si			; di holds count of chars in string, including null terminator
	add	di,8			; adjust for four system words
	mov	cx,di			; save size of entry
	add	di,MorphBlockUsed	; add in bytes already used
	cmp	di,MorphBlockSize	; see if greater than current morph block size
	jbe	smd3			; no
	mov	dx,di
	mov	MorphBlockSize,dx	; save current morphing data block size
	mov	ax,MorphDataBlock	; ax holds block selector, dx holds new size
	call	ResizeMemory	; resize memory block

smd3:
	mov	es,MorphDataBlock
	mov	di,MorphBlockUsed	; es:di -> storage for new morphing entry
	mov	ax,cx
	stosw				; save size of entry
	pop	eax				; get morph data flags
	stosd				; save flags
	pop	ax				; get option pointer, if any
	stosw				; save option pointer
	mov	bx,si			; bx -> start of option

smdloop:
	lodsb				; get char of string
	cmp	al,'a'			; check lower bounds
	jb	smdsave			; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	smdsave			; not lowercase
	sub	al,20h			; force lowercase to uppercase

smdsave:
	stosb				; save uppercased char
	or	al,al			; see if null terminator transferred
	jne	smdloop			; no

	add	MorphBlockUsed,cx	; update used amount
	inc	MorphDataCount	; bump count of morphed data entries
	pop	es				; restore critical register
	ret
SaveMorphData	ENDP

ENDS

END
