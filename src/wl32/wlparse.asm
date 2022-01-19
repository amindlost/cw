;*********************************************************************
;*   WLPARSE.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          06/18/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3h                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   linker parsing routines                                         *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLPARSE
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Equates                   *
;*****************************

NOPARAMETER		EQU	0
DWORDPARAMETER	EQU	4
STRINGPARAMETER	EQU	2
ANYPARAMETER	EQU	(DWORDPARAMETER OR STRINGPARAMETER)
GOBBLESPACEFLAG	EQU	4000h
IGNOREIFONFLAG	EQU	8000h

MAXOPTIONLEN		EQU	32
MAXDWORDPARAMLEN	EQU	10
MAXSTRINGPARAMLEN	EQU	80

FREEFORMATOBJPARSE	EQU	1
FREEFORMATLIBPARSE	EQU	2

; extra processing option flags
LINKCONFIGOPTIONSET	EQU	1

;DEBUG	EQU	1

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
PUBLIC	CheckCurrentDirectory
PUBLIC	CheckExtension
PUBLIC	ParseCommandLine
PUBLIC	ParseLinkOption
PUBLIC	SaveOBJLIBFileName

; variables
PUBLIC	CommandLinePos
PUBLIC	CommandLineString
PUBLIC	CompBuffSource,CompBuffDest
PUBLIC	FileListFlag,FreeFormatFlag
PUBLIC	LIBAtFrontFlag
PUBLIC	LIBFlagSelector
PUBLIC	LinkFileExtensionText
PUBLIC	MAPFileBuffer
PUBLIC	OBJNameSelector,LIBNameSelector
PUBLIC	OptionList
PUBLIC	ParseTermChar
PUBLIC	RSPFileCommentChar
PUBLIC	RSPFileIgnoreChar
PUBLIC	RSPFileNestLevel
PUBLIC	WorkingBuffer,CurrentFileName

IFDEF CLIPPER
PUBLIC	IsSpecialFixupOption
ENDIF

IFDEF WATCOM_ASM
PUBLIC	IsFlatOption
PUBLIC	IsDStoSSOption
ENDIF

IFDEF DLLSUPPORT
PUBLIC	DLLFileName
ENDIF

; for debugger

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

; globals
StackValue	DD	?		; user set stack value

; locals
CurrentFileName	=	$	; reuse buffer for file names in later processing
CommandLineString	DB	128 DUP (?)	; command line passed to linker

CompBuffDest	=	$	; reuse buffer for name compares
RSPFileName	DB	MAXRSPFILENESTLEVEL DUP (82 DUP (?))	; response file names

CompBuffSource	=	$	; reuse buffer for name compares
MAPFileBuffer	=	$+256	; reuse buffer for map file
RSPFileString	DB	MAXRSPFILENESTLEVEL DUP (256 DUP (?))	; response files line buffers

WorkingBuffer	DB	256 DUP (?)	; buffer for file names or commands being parsed
WordBuffer	DB	11	DUP (?)	; temporary buffer for parsing word value

CommandLinePos	DW	?	; command line buffer position
CurrentRSPFileNamePtr	DW	?	; pointer to current response file name
DestinationStart	DW	?	; destination string start offset
LIBNameBlkSize	DW	?	; current size of LIB names block
OBJNameBlkSize	DW	?	; current size of OBJ names block
ParseStartPos	DW	?	; start of linker command being parsed

RSPFileHandle	DW	MAXRSPFILENESTLEVEL DUP (?)	; handles of response files
RSPFilePos	DW	MAXRSPFILENESTLEVEL DUP (?)	; response file line buffer position

FoundInMorphDataFlag	DD	?	; nonzero if string found in morph data block
FoundInMorphOptionPtr	DW	?	; option pointer, if any, for morph data block string
ShortcutFlag	DB	?	; nonzero if command has reached shortcut (valid abbreviation) point

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

; option list has the following format:
; first word is pointer to the option text string
; second word is pointer to option flag byte
; third word is pointer to argument storage (none, word, or string)
; fourth word is argument flags (whether no arguments, word or string parameter)
; fifth dword is [d]word parameter low boundary
; sixth dword is [d]word parameter high boundary
; seventh dword if extra processing flag
OptionList	=	$		; start of built-in options to check
; /3p option
	DW	OFFSET DGROUP:ThreePText,OFFSET DGROUP:IsThreePOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /b option
	DW	OFFSET DGROUP:ExitBeepText,OFFSET DGROUP:IsExitBeepOption,0
	DW	NOPARAMETER
	DD	0,0,0
IFDEF WATCOM_ASM
; /cs option
	DW	OFFSET DGROUP:CaseSensitiveText,OFFSET DGROUP:IsCaseSensitiveOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /ds option
	DW	OFFSET DGROUP:DStoSSText,OFFSET DGROUP:IsDStoSSOption,0
	DW	NOPARAMETER
	DD	0,0,0
ENDIF
; /ex option
	DW	OFFSET DGROUP:CreateEXEText,OFFSET DGROUP:IsCreateEXEOption,0
	DW	NOPARAMETER
	DD	0,0,0
IFDEF WATCOM_ASM
; /f option
	DW	OFFSET DGROUP:FlatText,OFFSET DGROUP:IsFlatOption,0
	DW	NOPARAMETER
	DD	0,0,0
ENDIF
; /fl option
	DW	OFFSET DGROUP:FastLoadText,OFFSET DGROUP:IsFastLoadOption,0
	DW	NOPARAMETER
	DD	0,0,0
IFDEF CLIPPER
; /fx option
	DW	OFFSET DGROUP:SpecialFixupText,OFFSET DGROUP:IsSpecialFixupOption,0
	DW	NOPARAMETER
	DD	0,0,0
ENDIF
; /i option
	DW	OFFSET DGROUP:LinkInfoText,OFFSET DGROUP:IsLinkInfoOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /il option
	DW	OFFSET DGROUP:LinkInfoLimitText,OFFSET DGROUP:IsLinkInfoLimitOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /lc:<name> option
	DW	OFFSET DGROUP:LinkConfigText,OFFSET DGROUP:IsLinkConfigOption,OFFSET DGROUP:LinkConfigFileName
	DW	STRINGPARAMETER
	DD	0,0,LINKCONFIGOPTIONSET
; /ls option
	DW	OFFSET DGROUP:LIBSearchText,OFFSET DGROUP:IsLibSearchOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /m option
	DW	OFFSET DGROUP:MAPFileText,OFFSET DGROUP:IsMAPOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /nc option
	DW	OFFSET DGROUP:NoCopyrightText,OFFSET DGROUP:IsNoCopyrightOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /nd option
	DW	OFFSET DGROUP:NoDefaultLIBText,OFFSET DGROUP:IsNoDefaultLIBOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /nwd option
	DW	OFFSET DGROUP:NoWarnDupeText,OFFSET DGROUP:IsNoWarnDupeOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /nwdl option
	DW	OFFSET DGROUP:NoWarnLIBDupeText,OFFSET DGROUP:IsNoWarnLIBDupeOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /pd option
	DW	OFFSET DGROUP:ParseDisplayText,OFFSET DGROUP:IsParseDisplayOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /st:<size> option
	DW	OFFSET DGROUP:StackText,OFFSET DGROUP:IsStackOption,OFFSET DGROUP:StackValue
	DW	DWORDPARAMETER OR GOBBLESPACEFLAG
	DD	1,4*(16*65536)-1,0
; /sy option
	DW	OFFSET DGROUP:SYMFileText,OFFSET DGROUP:IsSYMOption,0
	DW	NOPARAMETER
	DD	0,0,0
; /w1 option
	DW	OFFSET DGROUP:WarnRetCode1Text,OFFSET DGROUP:IsWarnRetCode1Option,0
	DW	NOPARAMETER
	DD	0,0,0
; /wu option
	DW	OFFSET DGROUP:WarnUnknownText,OFFSET DGROUP:IsWarnUnknownOption,0
	DW	NOPARAMETER
	DD	0,0,0
IFDEF WATCOM_ASM
; /zu option
	DW	OFFSET DGROUP:ZeroUninitText,OFFSET DGROUP:IsZeroUninitOption,0
	DW	NOPARAMETER
	DD	0,0,0
ENDIF
; end of options, -1 pointer to text flags
	DW	-1

; built-in link option text strings
ThreePText		DB	2,'3P'
ExitBeepText	DB	1,'B'
CreateEXEText	DB	2,'EX'
FastLoadText	DB	2,'FL'
LIBSearchText	DB	2,'LS'
LinkInfoText	DB	1,'I'
LinkInfoLimitText	DB	2,'IL'
LinkConfigText	DB	3,'LC:'
MapFileText	DB	1,'M'
NoCopyrightText	DB	2,'NC'
NoDefaultLIBText	DB	2,'ND'
NoWarnDupeText	DB	3,'NWD'
NoWarnLIBDupeText	DB	4,'NWLD'
ParseDisplayText	DB	2,'PD'
StackText	DB	3,'ST:'
SYMFileText	DB	2,'SY'
WarnRetCode1Text	DB	2,'W1'
WarnUnknownText	DB	2,'WU'

IFDEF CLIPPER
SpecialFixupText	DB	2,'FX'
ENDIF

IFDEF WATCOM_ASM
CaseSensitiveText	DB	2,'CS'
DStoSSText		DB	2,'DS'
FlatText		DB	1,'F'
ZeroUninitText	DB	2,'ZU'
ENDIF

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

; globals
IsThreePOption	DB	0	; /3p option setting
IsExitBeepOption	DB	0	; /b option setting
IsCreateEXEOption	DB	0	; /ex option setting
IsFastLoadOption	DB	0	; /fl option setting
IFNDEF CLARION
IsLIBSearchOption	DB	0	; /ls option setting
ELSE
IsLIBSearchOption	DB	1	; /ls option setting
ENDIF
IsLinkInfoLimitOption	DB	0	; /il option setting
IsLinkInfoOption	DB	0	; /i option setting
IsLinkConfigOption	DB	0	; /lc option setting
IsMAPOption	DB	0		; /m option setting
IsNoCopyrightOption	DB	0	; /nc option setting
IsNoDefaultLIBOption	DB	0	; /nd option setting
IsNoWarnDupeOption	DB	0	; /nwd option setting
IsNoWarnLIBDupeOption	DB	0	; /nwld option setting
IsParseDisplayOption	DB	0	; /pd option setting
IsStackOption	DB	0	; /st option setting
IsSYMOption	DB	0		; /sy option setting
IsWarnRetCode1Option	DB	0	; /w1 option setting
IsWarnUnknownOption	DB	0	; /wu option setting
LIBAtFrontFlag	DB	0	; nonzero if placing library at front of library list
OBJBecomesLIBFlag	DB	0	; nonzero if OBJ becomes library file due to ISLIB
TotalLIBCount	DW	0	; total library file count
TotalOBJCount	DW	0	; total object module count
EXEFileName	DB	85 DUP (0)	; EXE file name
MAPFileName	DB	85 DUP (0)	; MAP file name
SYMFileName	DB	85 DUP (0)	; SYM file name

IFDEF DLLSUPPORT
DLLFileName	DB	13 DUP (0)	; DLL file name
ENDIF

IFDEF CLIPPER
IsSpecialFixupOption	DB	0	; /fx option setting
ENDIF

IFDEF WATCOM_ASM
IsCaseSensitiveOption	DB	0	; /cs option setting
IsDStoSSOption	DB	0	; /ds option setting
IsFlatOption	DB	0	; /f option setting
IsZeroUninitOption	DB	0	; /zu option setting
ENDIF

; locals
ContinueParseModeFlag	DB	0	; flag to continue current parse mode
FileListFlag	DB	0	; 1 if free format parsing of OBJs, 2 if LIBs, 0 otherwise
FreeFormatFlag	DB	0	; nonzero if free format link response file mode
IsUserDefinedOptions	DB	0	; nonzero if user defined link options
ParseMode	DB	0		; parse mode, 0==obj, 1==exe, 2==map, 3==lib
RSPFileCommentChar	DB	'#'	; comment character in reponse file
ParseTermChar	DB	0	; parse termination character

RSPFileIgnoreChar	DB	16 DUP (0)	; characters to ignore in response file

ParseLIBNameOff	DW	0	; offset to library file name storage within segment
LIBFlagSelector	DW	0	; library flags selector
LIBNameSelector	DW	0	; library file name storage selector
OBJNameOffset	DW	0	; offset to object module name storage within segment
OBJNameSelector	DW	0	; object module name storage selector
RSPFileNestLevel	DW	0	; nest level of response file being read

LinkConfigFileName	DB	82 DUP (0)	; linker configuration file name

OBJExtensionText	DB	'.OBJ'
LIBExtensionText	DB	'.LIB'

IFDEF DLLSUPPORT
DLLExtensionText	DB	'.DLL'
ENDIF

LinkFileExtensionText	DB	'    '
OBJEVarText		DB	'OBJ='
LIBEVarText		DB	'LIB='

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	DefaultLIBAddFlag:BYTE
EXTRN	MorphDataBlock:WORD
EXTRN	MorphDataCount:WORD
EXTRN	TempBuffer:BYTE

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	AllocateMemory:PROC,ResizeMemory:PROC
EXTRN	DOSErrorExit:PROC
EXTRN	InternalErrorExit:PROC
EXTRN	LinkerErrorExit:PROC
EXTRN	OpenFile:PROC,ReadFile:PROC
EXTRN	ParseConfigFile:PROC
EXTRN	SaveMorphData:PROC
EXTRN	UnknownOptionWarn:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* PARSECOMMANDLINE          *
;*****************************

; parse linker command line
; es set to ds at start, safe to assume es=ds, save/restore es if modified

ParseCommandLine	PROC

pcltop:
	push	ds
	pop	es				; es -> wl32 data for duration of parse
	mov	si,OFFSET DGROUP:CommandLineString
	mov	CommandLinePos,si

; parse loop, shut off continue parse mode flag
ParseLoopNoCont:
	mov	ContinueParseModeFlag,OFF	; reset flag in case set, KEEP FLAGS STATUS

; parse loop, don't modify continue parse mode flag
ParseLoopCont:
	mov	ParseStartPos,si	; save start of command in case of error
	call	ReadChar	; read command line or response file character

; al holds first command/name char
; if char is 0 (end of file) then move up one level of command processing, quit at highest
	or	al,al			; see if end of file char
	jne	chkeol		; no
	cmp	RSPFileNestLevel,0	; see if at highest level (parsing command line)
	je	parsedone	; yes, parse complete, do any end processing

	call	UpRSPNestLevel	; go up one response file nesting level
	jmp	SHORT ParseLoopNoCont

; if char is CR (end of line),
;    if not freeformat and not parse mode continue flag then increment parse mode
;    if freeformat, reset file list flag
chkeol:
	cmp	al,CR			; see if end of line character
	jne	chkwhite
	inc	si				; move past following LF char
	mov	FileListFlag,OFF	; reset file list flag
	cmp	FreeFormatFlag,OFF	; see if free format parsing
	jne	ParseLoopNoCont		; yes

; not freeformat, if continue parse mode flag not set then increment parse mode
	cmp	ContinueParseModeFlag,OFF	; see if continuing parse mode flag
	jne	ParseLoopNoCont	; yes

bumpmode:
	inc	ParseMode		; bump the parsing mode
	cmp	ParseMode,4		; see if previously parsing libraries
	jb	ParseLoopNoCont	; no
	jmp	NEAR PTR parsedone	; yes, end parsing of link file

; if char is whitespace then ignore
chkwhite:
	cmp	al,' '			; see if whitespace character
	jbe	ParseLoopCont	; yes, ignore it

; if char is '/' then parse as option
	cmp	al,'/'			; see if start of option
	jne	chkrsp			; no
	cmp	BYTE PTR ds:[si],'/'	; peek at next char
	je	iscomment		; // is a comment line
	call	ParseLinkOption
	jmp	SHORT ParseLoopNoCont

; if char is '@' then parse as response file
chkrsp:
	cmp	al,'@'			; see if response file
	jne	chkcomma		; no
	mov	FileListFlag,OFF	; reset file list flag
	call	ParseRSPFile
	jmp	SHORT ParseLoopNoCont

; if char is ',' and not freeformat then increment parse mode (obj,exe,map,lib)
; if char is ',' and freeformat then ignore
chkcomma:
	cmp	al,','			; see if comma
	jne	chkplus		; no
	cmp	FreeFormatFlag,OFF	; see if free format parsing
	jne	ParseLoopNoCont	; yes, ignore comma
	jmp	SHORT bumpmode	; bump parse mode

; if char is '+' and not freeformat then set continue parse mode flag
; if char is '+' and freeformat then ignore
chkplus:
	cmp	al,'+'			; see if plus sign
	jne	chkcomment		; no
	cmp	FreeFormatFlag,OFF	; see if free format parsing
	jne	ParseLoopNoCont	; yes, ignore plus
	mov	ContinueParseModeFlag,ON	; flag continue current parse mode
	jmp	SHORT ParseLoopCont	; don't reset flag after setting it

; if char is <comment character> then ignore rest of line
chkcomment:
	cmp	al,RSPFileCommentChar	; see if comment char
	jne	chkignore		; no

iscomment:
	mov	FileListFlag,OFF	; reset file list flag
	call	ScanToEOL	; scan past comment line to next line, if any
	jmp	NEAR PTR ParseLoopCont

; if char is <any ignore character> then ignore
chkignore:
	mov	di,OFFSET DGROUP:RSPFileIgnoreChar
	mov	cx,16			; number of possible ignore characters
	repne	scasb
	jne	chkterm			; not a ignore char
	jmp	NEAR PTR ParseLoopCont	; ignore char doesn't affect continue parse mode flag

; if char is <termination character> then stop parsing
chkterm:
	cmp	al,ParseTermChar	; see if termination char
	jne	chkfile1		; no
	call	CloseAllRSPFiles	; close all the open response files, done parsing
	jmp	SHORT parsedone

; if file list flag and freeformat then read file name, keep as appropriate OBJ or LIB type
chkfile1:
	cmp	FreeFormatFlag,0
	je	chkexeobj		; not free format
	cmp	FileListFlag,1
	je	chkfile2		; reading file list
	cmp	FileListFlag,2
	jne	chkexeobj		; not reading file (library) list

chkfile2:
	mov	DefaultLIBAddFlag,OFF
	call	SaveOBJLIBFileName	; save the OBJ or LIB file name
	jmp toplnocont

; if not freeformat and ParseMode==1 (EXE) then save as EXE file name
; if not freeformat and ParseMode==2 (OBJ) then save as MAP file name
chkexeobj:
	cmp	FreeFormatFlag,OFF
	jne	chkcom			; free format
	cmp	ParseMode,1
	jne	chkobj			; not EXE file name
	call	SaveEXEFileName
	jmp	SHORT toplnocont

chkobj:
	cmp	ParseMode,2
	jne	chkcom			; not MAP file name
	call	SaveMAPFileName
	jmp	SHORT toplnocont

; al holds first command/name char, ds:si -> remaining chars
; read full string, see if command
chkcom:
	call	CheckCommandString
	jnc	toplnocont			; is a command, successfully parsed

; not command and freeformat, ignore, but check and see if warning displayed
	cmp	FreeFormatFlag,OFF	; see if freeformat
	je	pclsavefile		; no
	mov	dx,si
	dec	dx				; dx -> start of unknown option/command
	call	ScanToEOL	; scan to end of line
	cmp	BYTE PTR ds:[si-2],CR	; see if need to null terminate line string
	jne	pclwarnchk		; no
	mov	BYTE PTR ds:[si-2],0

pclwarnchk:
	cmp	IsWarnUnknownOption,OFF	; see if warn on unknown option
	je	toplnocont			; no

; dx -> null terminated command line string
	call	UnknownOptionWarn
	jmp	SHORT toplnocont

; not command and not freeformat, read as file name for appropriate parse mode
pclsavefile:
	mov	DefaultLIBAddFlag,OFF
	call	SaveOBJLIBFileName	; no, save the OBJ or LIB file name

toplnocont:
	jmp	NEAR PTR ParseLoopNoCont

; parsing of command line/response files done, do end processing,
; setup default file names if none assigned, word-align stack,
; check if no object modules specified
parsedone:
	call	AddEvarLinkFiles	; see if more link files specified by morphing
	jc	pcltop			; yes, parse them too

	cmp	IsStackOption,OFF	; see if stack explicitly set
	je	pdobjchk		; no
	mov	eax,StackValue
	inc	eax
	and	al,0feh			; make stack even
	mov	StackValue,eax	; save back to value

pdobjchk:

; check for OBJAPPEND object modules to add
	call	AddOBJAPPENDModules	; add object modules from OBJAPPEND

	cmp	TotalOBJCount,0	; see if any object modules listed
	jne	pdexechk		; yes
	cmp	IsParseDisplayOption,OFF	; see if parse display option set
	jne	pdret			; yes, no object module list needed
	mov	al,NOOBJFILEERRORCODE	; no object modules listed
	call	LinkerErrorExit

pdexechk:
	cmp	BYTE PTR EXEFileName,0	; see if EXE file name already set
	jne	pdmapchk		; yes
	call	SetEXEFileName	; set EXE file name to default

pdmapchk:
	cmp	BYTE PTR MAPFileName,0	; see if MAP file name already set
	jne	pdsym			; yes
	cmp	IsMAPOption,OFF	; see if map option not on
	je	pdsym			; not on, no map file
	call	SetMAPFileName	; set MAP file name to default

pdsym:
	cmp	IsSYMOption,OFF	; see if SYM option not on
	je	pcldll			; not on, no SYM file
	call	SetSYMFileName	; set SYM file name to default

pcldll:

IFDEF DLLSUPPORT
	call	SetDLLFileName	; set DLL file name to EXE file name without path
ENDIF

pdret:
	ret
ParseCommandLine	ENDP

;*****************************
;* READCHAR                  *
;*****************************

; read command line or response file character
; returns character in al
; updates si char pointer
; destroys ax,dx

ReadChar	PROC
	push	bx
	cmp	RSPFileNestLevel,0	; see if reading from command line
	je	readcom			; yes

ReadLoop:
	lodsb				; get char from response file buffer
	cmp	al,27			; see if past end of line (set to ESC by ReadRSPLine)
	je	readrsp			; yes, read next line, if any from response file

	mov	bx,RSPFileNestLevel
	dec	bx				; make relative 0
	add	bx,bx			; convert to word offset
	mov	ds:[RSPFilePos+bx],si	; save updated buffer position
	jmp	SHORT rcret

readrsp:
	call	ReadRSPFile	; read from response file, setup for new line
	jmp	SHORT ReadLoop

readcom:
	lodsb				; get char from command line buffer
	mov	CommandLinePos,si

rcret:
	pop	bx
	ret
ReadChar	ENDP

;*****************************
;* READRSPFILE               *
;*****************************

; read from response file, setup for new line
; updates si char pointer
; destroys ax,bx,dx

ReadRSPFile	PROC
	push	cx
	push	di
	mov	di,RSPFileNestLevel
	dec	di				; make relative 0
	add	di,di			; convert to word offset

	mov	dx,di
	mov	cl,7
	shl	dx,cl			; x128 (x256 offset since already word offset)
	mov	CurrentRSPFileNamePtr,dx	; keep pointer to RSPFileName in case of error
	add	CurrentRSPFileNamePtr,OFFSET DGROUP:RSPFileName
	mov	si,OFFSET DGROUP:RSPFileString
	add	si,dx			; si -> start of response file line buffer
	mov	ds:[RSPFilePos+di],si

	mov	dx,si			; ds:dx -> read buffer
	mov	cx,255			; read 255 bytes of response file
	mov	bx,ds:[RSPFileHandle+di]	; file handle
	call	ReadFile	; read the file

; if short read (ax<255) then end of file reached, place zero terminator
	cmp	ax,255			; see if short read
	jb	ShortRead		; yes

; scan for final CR/LF pair, place ESC char after it
	mov	cx,254			; scan 254 bytes of response file characters (not final byte)
	mov	di,si			; es:di -> buffer
	mov	al,CR
	repne	scasb		; look for CR
	jne	lineerr			; not found, response file line too long (>253 chars not counting CR/LF)

ScanLoop:
	mov	dx,di			; dx -> CR char+1 (should be LF)
	jcxz	ScanDone	; no more chars to scan
	repne	scasb		; look for more CR's
	je	ScanLoop		; found one

ScanDone:
	mov	di,dx			; get LF offset
	mov	BYTE PTR ds:[di+1],27	; write ESC past LF character

;  file seek to character position after final CR/LF
	sub	dx,si			; dx == relative offset in buffer
	mov	ax,254
	sub	ax,dx			; ax == chars to reread on next pass (seek back from)
	je	rrfret			; no bytes to reread
	mov	dx,ax			; chars to reread in dx
	neg	dx				; make it a negative number
	mov	cx,0ffffh		; sign extend thru cx
	mov	ax,4201h		; move file pointer from current location
	int	21h				; rewind file

rrfret:
	pop	di
	pop	cx
	ret

; linker error, response file line too long
lineerr:
	mov	al,RSPLINELENERRORCODE
	mov	dx,CurrentRSPFileNamePtr	; dx -> file name
	call	LinkerErrorExit

; read to end of response file
ShortRead:
	mov	bx,ax			; offset into buffer
	mov	BYTE PTR ds:[si+bx],0	; null terminate after final character read
	jmp	SHORT rrfret

ReadRSPFile	ENDP

;*****************************
;* UPRSPNESTLEVEL            *
;*****************************

; go up one response file nesting level
; updates si pointer
; destroys ax,bx,si

UpRSPNestLevel	PROC

; close current response file
	mov	bx,RSPFileNestLevel
	dec	bx				; make relative 0
	add	bx,bx			; convert to word offset
	mov	bx,ds:[RSPFileHandle+bx]	; file handle
	mov	ah,3eh			; close file
	int	21h

	dec	RSPFileNestLevel	; reduce level of nesting
	je	up2				; back at command line

; update response file name pointer
	mov	ax,RSPFileNestLevel
	dec	ax				; make relative 0
	push	cx			; save critical register (faster/smaller than manual ax shifts)
	mov	cl,8
	shl	ax,cl			; x256
	pop	cx				; restore critical register
	mov	CurrentRSPFileNamePtr,ax	; keep pointer to RSPFileName in case of error
	add	CurrentRSPFileNamePtr,OFFSET DGROUP:RSPFileName

; update si read pointer
	mov	bx,RSPFileNestLevel
	dec	bx				; make relative 0
	add	bx,bx			; convert to word offset
	mov	si,ds:[RSPFilePos+bx]
	ret

up2:
	mov	si,CommandLinePos
	ret
UpRSPNestLevel	ENDP

;*****************************
;* PARSERSPFILE              *
;*****************************

; parse response file name, open file, read it
; upon entry si -> first char of name
; updates si to char following name
; destroys ax,bx,cx,di

ParseRSPFile	PROC
	mov	di,RSPFileNestLevel	; next nest level relative 0
	cmp	di,MAXRSPFILENESTLEVEL	; see if nested too deep
	jb	parse2			; no

; response files nested too deep
	mov	al,RSPNESTLEVELERRORCODE
	mov	dx,CurrentRSPFileNamePtr	; dx -> file name
	call	LinkerErrorExit

parse2:
	mov	cl,8			; x256
	shl	di,cl			; es:di -> response file name destination offset
	add	di,OFFSET DGROUP:RSPFileName
	call	GetFileNameString	; get file name string
	jcxz	badname		; null file name

here99:
	mov	dx,DestinationStart	; ds:dx -> file name
	push	si			; save critical register
	mov	bx,dx			; bx -> file name
	mov	si,OFFSET DGROUP:LinkFileExtensionText
	call	CheckDefaultExtension	; check for extension, add default if required

	call	CheckCurrentDirectory	; check if file exists
	jnc	parseopen		; file found in current directory
	mov	di,dx			; di -> filename

IFDEF DEBUG
	mov	bx,1
	mov	cx,10
	mov	ah,40h
	int	21h
ENDIF

	mov	bx,OFFSET DGROUP:LIBEVarText	; bx -> environment variable to search for paths
	push	dx			; save -> file name
	call	SearchEVarDirectory	; search environment variable directories for file
	pop	dx				; dx -> filename with path

IFDEF DEBUG
	mov	bx,1
	mov	cx,19
	mov	ah,40h
	int	21h
ENDIF

parseopen:
	pop	si				; restore critical register
	mov	al,40h			; read-only, deny none access
	call	OpenFile	; open response file, ax == handle

	mov	bx,RSPFileNestLevel	; next nest level relative 0
	add	bx,bx			; convert to word offset
	mov	ds:[RSPFileHandle+bx],ax	; save file handle
	inc	RSPFileNestLevel	; update response file nest level
	call	ReadRSPFile	; read in the first response file line
	ret

; null file name, error condition, list as bad option
badname:
	mov	dx,OFFSET DGROUP:DestinationStart	; dx -> destination start
	mov	bx,dx
	mov	WORD PTR ds:[bx],'@'	; store '@' and null terminator at destination start
	mov	al,BADOPTIONERRORCODE
	call	LinkerErrorExit

ParseRSPFile	ENDP

;*****************************
;* GETFILENAMESTRING         *
;*****************************

; get file name string
; upon entry es:di -> destination, ds:si -> source
; updates si char pointer, DestinationStart variable
; upon exit cx holds # of file name chars,
; destroys ax,bx,cx,dx,di

GetFileNameString	PROC
	xor	cx,cx			; init count of chars read
	mov	DestinationStart,di	; keep pointer to start of destination string in case of error

GetNameLoop:
	mov	al,ds:[si]		; peek at next file name char
;@@@	cmp	al,27			; see if need to read next char from response file
;@@@	je	getread			; ESC flags yes
	cmp	al,' '			; check for whitespace, end of file condition
	jbe	getexit
	cmp	al,'/'			; stop at option char
	je	getexit
	cmp	al,';'			; stop at semicolon char
	je	getexit
	cmp	al,','			; stop at comma char
	je	getexit
	cmp	al,'+'			; stop at plus char
	je	getexit
	cmp	al,ParseTermChar	; stop at termination char
	je	getexit

getread:
	call	ReadChar	; get response file char

; force name to all caps
	cmp	al,'a'			; check lower bounds
	jb	getignchk		; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	getignchk		; not lowercase
	sub	al,20h			; force lowercase to uppercase

; check if ignore char, if so, don't store it or increment file name count
getignchk:
	mov	bx,OFFSET DGROUP:RSPFileIgnoreChar
	mov	dl,16			; up to sixteen ignore chars

IgnoreLoop:
	cmp	al,ds:[bx]		; see if matches ignore char
	je	GetNameLoop		; yes
	inc	bx				; bump to next ignore char
	dec	dl
	jne	IgnoreLoop		; loop thru all ignore chars

	stosb				; save the char
	inc	cx				; bump count of chars in file name
	cmp	cl,80			; see if 80 character name length exceeded
	jbe	GetNameLoop		; no

; too many characters in file name
	mov	BYTE PTR es:[di],0	; null terminate file name
	mov	al,2			; list error as DOS file not found
	mov	bx,OFFSET DGROUP:DestinationStart	; bx -> file name
	call	DOSErrorExit

getexit:
	mov	BYTE PTR es:[di],0	; null terminate file name
	ret

GetFileNameString	ENDP

;*****************************
;* SCANTOEOL                 *
;*****************************

; scan to end of linker response file line
; update si char pointer
; destroys ax,bx,dx

ScanToEOL	PROC

eolloop:
	mov	al,ds:[si]		; get next char to be read
	or	al,al			; see if end of file
	je	scanret			; yes
	call	ReadChar	; get response file character
	cmp	al,CR			; see if at end of line
	jne	eolloop			; not yet
	inc	si				; scan past LF

scanret:
	ret
ScanToEOL	ENDP

;*****************************
;* CLOSEALLRSPFILES          *
;*****************************

; close all response files, parsing done
; destroys ax,bx,cx,di

CloseAllRSPFiles	PROC
	mov	cx,RSPFileNestLevel	; cx == number of files to close
	jcxz	closeret	; no files to close
	mov	di,OFFSET DGROUP:RSPFileHandle

CloseLoop:
	mov	bx,ds:[di]
	mov	ah,3eh			; close file
	int	21h
	add	di,2			; move to next handle to close
	loop	CloseLoop

closeret:
	ret
CloseAllRSPFiles	ENDP

;*****************************
;* PARSELINKOPTION           *
;*****************************

; parse link option
; upon entry si -> first char of option, al=='/'
; updates si to char following option
; destroys ax,bx,cx,dx,di

ParseLinkOption	PROC
	push	si			; save si -> first char of option

; read option string to working buffer
	mov	di,OFFSET DGROUP:WorkingBuffer
	stosb				; place leading '/' in buffer in case of error
	xor	cx,cx			; init count of chars in option string

GetOptionLoop:
	mov	al,ds:[si]		; peek at next option char
;@@@	cmp	al,27			; see if need to read next char from response file
;@@@	je	optread			; ESC flags yes
	cmp	al,' '			; check for whitespace, end of file condition
	jbe	optend
	cmp	al,'/'			; stop at option char
	je	optend
	cmp	al,','			; stop at comma char
	je	optend
	cmp	al,';'			; stop at semicolon char
	je	optend
	cmp	al,'+'			; stop at plus char
	je	optend
	cmp	al,ParseTermChar	; stop at termination char
	je	optend

optread:
	call	ReadChar	; get response file char
	stosb				; save the char
	inc	cx				; bump count of chars in option string
	cmp	al,':'			; stop at option ':' char, always prior to argument
	je	optend
	cmp	cl,MAXOPTIONLEN	; see if maximum option length exceeded
	jbe	GetOptionLoop	; no

; bad option, listed in working buffer
BadOptionWB:
	mov	dx,OFFSET DGROUP:WorkingBuffer	; dx -> bad option string
	mov	al,BADOPTIONERRORCODE
	call	LinkerErrorExit

optend:
	push	si			; save critical register
	mov	BYTE PTR ds:[di],0	; null terminate the option string

; check for built-in (standard '/') link options first
	mov	bx,OFFSET DGROUP:OptionList	; es:di -> link options

optmainloop:
	mov	si,ds:[bx]		; ds:si -> option text string prefixed by length byte
	cmp	si,-1			; see if end of option list
	je	chkudf			; not a standard option, check user-defined options

	lodsb				; al holds length of text string
	mov	di,OFFSET DGROUP:WorkingBuffer+1	; es:di -> read-in option string (past '/')
	mov	cl,al
	xor	ch,ch			; zero high byte of length word

; ds:si -> option string to match
; es:di -> given option string
optloop:
	lodsb				; get uppercase option to match in al
	mov	ah,ds:[di]		; get given option char in ah
	inc	di				; move to next char, if any
	cmp	ah,'a'			; check lowercase lower boundary
	jb	optcmp1			; not lowercase
	cmp	ah,'z'			; check lowercase upper boundary
	ja	optcmp1			; not lowercase
	sub	ah,20h			; conver to uppercase

; al holds option char to match, ah holds uppercased given option char
optcmp1:
	cmp	al,ah			; see if match
	jne	optfail			; no
	loop	optloop		; yes, keep checking

; options matched thru all chars, see if any more chars in given option string
	cmp	BYTE PTR ds:[di],0	; see if matched to null terminator
	je	optmatch		; yes, options matched

; options failed on match
optfail:
	add	bx,OPTLISTSIZE	; move to next option to check
	jmp	SHORT optmainloop	; reset pointers

; found the proper option
optmatch:
	pop	si				; restore si -> option parameter, if any
	call	SetLinkOption	; set the proper link option, get any parameters
	pop	ax				; flush -> start of option from stack
	jmp	SHORT optret	; done

; didn't match any standard link options
; check for user-defined link options
chkudf:
	pop	ax				; flush -> option parameter, if any
	pop	si				; si -> start of option
	dec	si				; si -> '/' of option
	lodsb				; al holds first char of option ('/'), si -> remaining chars
	call	CheckCommandString
	jc	BadOptionWB		; bad link option

optret:
	ret

ParseLinkOption	ENDP

;*****************************
;* SETLINKOPTION             *
;*****************************

; set the proper link option, get any parameters
; upon entry ds:bx -> option entry, si-> parameter text
; updates si
; destroys ax,cx,dx,di

SetLinkOption	PROC
	mov	di,ds:[bx+OPTLISTOFFOPTPTR]	; ds:di -> option
	test	WORD PTR ds:[bx+OPTLISTOFFARGFLAGS],IGNOREIFONFLAG	; see if ignore parameters since already processed
	jne	setret			; yes, ignore this option
	mov	BYTE PTR ds:[di],ON	; turn on option
	test	WORD PTR ds:[bx+OPTLISTOFFARGFLAGS],ANYPARAMETER	; see if any parameters to option
	je	setret			; no parameters

	call	GetOptionParameter	; get option parameter
	or	ax,ax			; see if extra processing flagged
	je	setret			; no
	cmp	ax,LINKCONFIGOPTIONSET
	jne	set2			; not link configuration processing
	call	ParseConfigFile

set2:

setret:
	ret
SetLinkOption	ENDP

;*****************************
;* GETOPTIONPARAMETER        *
;*****************************

; get option parameter
; upon entry ds:bx -> option entry, si-> parameter text
; updates si
; returns ax == extra processing flags
; destroys ax,cx,dx

GetOptionParameter	PROC
	xor	cx,cx			; init count of chars in option string
	test	WORD PTR ds:[bx+OPTLISTOFFARGFLAGS],STRINGPARAMETER	; see if string parameter
	jne	gostring		; yes
	mov	di,OFFSET DGROUP:WordBuffer	; use temporary buffer to hold word value before processing
	test	WORD PTR ds:[bx+OPTLISTOFFARGFLAGS],GOBBLESPACEFLAG	; see if gobble space/tab char
	je	GetParamLoop	; no

gobloop:
	mov	al,ds:[si]		; peek at next option char
	cmp	al,' '			; see if nonwhitespace
	ja	GetParamLoop	; yes
	je	gogobble		; space char, gobble it
	cmp	al,9			; see if tab char
	jne	GetParamLoop	; neither space nor tab

gogobble:
	inc	si				; bump to next position
	jmp	SHORT gobloop

; read option string to appropriate buffer
gostring:
	mov	di,ds:[bx+OPTLISTOFFARGPTR]	; di -> parameter storage

GetParamLoop:
	mov	al,ds:[si]		; peek at next option char
;@@@	cmp	al,27			; see if need to read next char from response file
;@@@	je	paramread		; ESC flags yes
	cmp	al,' '			; check for whitespace, end of file condition
	jbe	paramend
	cmp	al,'/'			; stop at option char
	je	paramend
	cmp	al,','			; stop at comma char
	je	paramend
	cmp	al,';'			; stop at semicolon char
	je	paramend
	cmp	al,'+'			; stop at plus char
	je	paramend
	cmp	al,ParseTermChar	; stop at termination char
	je	paramend

paramread:
	call	ReadChar	; get response file char
	stosb				; save the char
	inc	cx				; bump count of chars in option string
	test	WORD PTR ds:[bx+OPTLISTOFFARGFLAGS],STRINGPARAMETER	; see if string parameter
	jne	strchk			; yes

; word value parameter
	cmp	cl,MAXDWORDPARAMLEN	; see if maximum parameter length exceeded
	jbe	GetParamLoop	; no

	mov	BYTE PTR ds:[di],0	; null terminate parameter

badval:
	mov	si,OFFSET DGROUP:WordBuffer	; si -> parameter for error
	jmp	SHORT tobowb	; bad option

strchk:
	cmp	cl,MAXSTRINGPARAMLEN	; see if maximum parameter length exceeded
	jbe	GetParamLoop	; no
	mov	BYTE PTR ds:[di],0	; null terminate parameter
	mov	si,ds:[bx+OPTLISTOFFARGPTR]	; si -> parameter

; transfer bad option parameters into working buffer
; si -> parameter
tobowb:
	mov	di,OFFSET DGROUP:WorkingBuffer
	xor	al,al
	mov	cx,255			; null terminator known to be within 255 chars
	repne	scasb		; di -> char after null terminator
	dec	di				; di -> null terminator

errloop:
	movsb				; transfer parameter char
	cmp	di,OFFSET DGROUP:WorkingBuffer+126	; see if enough room for another parameter char
	jb	errloop			; yes
	jmp	NEAR PTR BadOptionWB	; no, show bad option feedback

paramend:
	mov	BYTE PTR ds:[di],0	; null terminate string
	jcxz	badval		; invalid null length option
	test	WORD PTR ds:[bx+OPTLISTOFFARGFLAGS],STRINGPARAMETER	; see if string parameter
	jne	goextra			; yes

; process dword value, check for out of boundary limits
	xor	eax,eax			; init value held in eax
	mov	di,OFFSET DGROUP:WordBuffer	; di -> word value string buffer

eatloop:
	cmp	BYTE PTR ds:[di],' '	; eat prepended space
	jne	dwordloop		; not a space value
	inc	di				; move to next parameter slot
	jmp	SHORT eatloop

dwordloop:
	xor	edx,edx			; zero high bytes
	mov	dl,ds:[di]		; get char value
	cmp	dl,'0'			; must be numeric
	jb	badval
	cmp	dl,'9'			; must be numeric
	ja	badval
	and	dl,0fh			; strip ASCII value
	add	eax,edx			; add to previous word value
	jc	badval			; overflow, value too large
	cmp	BYTE PTR ds:[di+1],0	; see if at one's digit
	je	chkbound		; yes, check boundary of value
	mov	edx,10
	mul	edx				; shift value by 1 digit (*10)
	test	edx,edx		; see if overflow (>1 dword value)
	jne	badval			; overflow occurred
	inc	di				; move to next digit in string
	jmp	SHORT dwordloop

; check boundary of dword value in eax
chkbound:
	cmp	eax,ds:[bx+OPTLISTOFFPARAMLOW]	; compare value to low boundary
	jb	badval			; out of bounds
	cmp	eax,ds:[bx+OPTLISTOFFPARAMHIGH]	; compare value to high boundary
	ja	badval			; out of bounds

; save dword value to proper variable
	mov	di,ds:[bx+OPTLISTOFFARGPTR]	; di -> parameter value variable
	mov	ds:[di],eax

goextra:
	mov	ax,ds:[bx+OPTLISTOFFEXTRAFLAG]	; get extra processing flags in ax for return
	ret
GetOptionParameter	ENDP

;*****************************
;* SAVEOBJLIBFILENAME        *
;*****************************

; save the OBJ or LIB file name
; upon entry al holds first char of name, si -> following chars, if any
; 	DefaultLIBAddFlag set if default library addition (don't add if already exists)
; updates si
; destroys ax,bx,cx,dx,di

SaveOBJLIBFileName	PROC
	mov	di,OFFSET DGROUP:WorkingBuffer	; di -> destination

; force name to all caps
	cmp	al,'a'			; check lower bounds
	jb	solsave			; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	solsave			; not lowercase
	sub	al,20h			; force lowercase to uppercase

solsave:
	stosb				; save first char of file name
	call	GetFileNameString	; non-null name by definition, no null check needed
	inc	cx				; bump count of chars in file name to account for first char
	push	si			; save critical register

; If not freeformat look for ':'.  If ':' is not in second or third position
; (third because could be overlaid file with leading parenthesis or bracket),
;  as occurs with WarpLink library module command, ignore the file name
	mov	dx,cx			; save file name char count for later use
	cmp	FreeFormatFlag,OFF	; see if freeformat
	jne	solff			; yes

; cx holds file name char count from GetFileNameString
	mov	di,OFFSET DGROUP:WorkingBuffer	; di -> destination
	mov	al,':'

solloop:
	jcxz	sol2		; no more char positions to check
	repne	scasb		; look for ':'
	jne	sol2			; not found
	mov	bx,di
	sub	bx,OFFSET DGROUP:WorkingBuffer
	cmp	bl,2
	je	solloop			; ':' in drive position, keep checking name
	cmp	bl,3
	je	solloop			; ':' in drive position with presumed overlay indicator
	jmp	NEAR PTR solret	; ':' not in drive position, WarpLink module command, ignore

; positional parsing of file name, ParseMode determines OBJ or LIB (0 or 3)
sol2:
	cmp	ParseMode,3		; see if library
	je	sollib			; yes
	jmp	SHORT solobj	; object module processing

; freeformat parsing of file name, FileListFlag determines OBJ or LIB (1 or 2)
solff:
	cmp	FileListFlag,2	; see if library
	je	sollib			; yes

; saving object file name
; WorkingBuffer -> file name to store
; dx == count of file name chars to store
solobj:
	call	FindOBJFile	; find the object module, make sure exists, update with OBJ e-var path if necessary
	jc	solret			; flush this file

; check if OBJ became LIB, if so transfer to library code
	cmp	OBJBecomesLIBFlag,OFF
	jne	solinclib

	inc	TotalOBJCount	; bump total object module count
	cmp	OBJNameSelector,0	; see if OBJ file name memory block allocated
	jne	soloext			; yes, extend by number of file name chars+1 for storage

; allocate memory to store object module names
	push	dx
	mov	dx,2048			; initially allocate 2K block for storage
	mov	OBJNameBlkSize,dx
	call	AllocateMemory	; allocate memory
	mov	OBJNameSelector,ax	; save selector
	pop	dx

; dx == count of bytes in file name not including null terminator byte
soloext:
	mov	ax,OBJNameSelector	; get selector of memory block to expand
	inc	dx				; adjust for null terminator byte
	push	OBJNameOffset	; save old storage offset
	push	dx			; save total length of chars to transfer including null terminator
	add	dx,OBJNameOffset	; compute new segment length low word
	jc	internal1		; improper overflow
	mov	OBJNameOffset,dx	; update offset for next object module name
	mov	di,OBJNameBlkSize	; get current size of block in di
	cmp	dx,di			; see if block used > block size
	jbe	sol3			; no
	add	di,2048
	jc	internal1		; improper overflow
	mov	dx,di			; new block size
	mov	OBJNameBlkSize,dx	; update current block size
	call	ResizeMemory	; resize memory block
	jmp	NEAR PTR sol3

sollib:
	cmp	DefaultLIBAddFlag,OFF	; see if adding default library
	je	solfindlib		; no

; adding default library, don't do it if already existent
	mov	si,OFFSET DGROUP:LIBExtensionText
	mov	bx,OFFSET DGROUP:WorkingBuffer
	call	CheckDefaultExtension	; check for extension, add default if required

	cmp	TotalLIBCount,0	; see if any previous libraries
	je	solfindlib		; no, automatic add

	push	es
	mov	es,LIBNameSelector
	xor	dx,dx
	mov	di,dx			; es:di -> library file name storage

soldefreset:
	mov	si,OFFSET DGROUP:WorkingBuffer	; si -> default library file name

soldefloop:
	lodsb
	or	al,al			; see if match to zero
	je	soldefmatch		; yes
	scasb
	je	soldefloop		; match on file name char

; try next char
	cmp	BYTE PTR es:[di-1],0	; see if at next library
	jne	soldefreset		; no
	inc	dx				; bump count of lib strings checked
	cmp	dx,TotalLIBCount	; see if total lib names checked against default
	jne	soldefreset		; no
	pop	es				; all names checked, this is a new file name
	jmp	SHORT solfindlib

; internal error (overflow of segment length on names, shouldn't occur in real life)
internal1:
	mov	cl,1
	call	InternalErrorExit	; no return

; names matched, check if not partial (e.g. MAIN.LIB and SMAIN.LIB)
soldefmatch:
	cmp	BYTE PTR es:[di],0	; see if stored library at null terminator as well
	jne	soldefreset		; no, not a match
	sub	si,OFFSET DGROUP:WorkingBuffer	; si holds chars to back up
	sub	si,1			; adjust back for final lodsb si increment
	sub	di,si
	jne	solnotfirst		; not the first name

soldefdiscard:
	pop	es
	jmp	NEAR PTR solret	; duplicate, discard

solnotfirst:
	mov	al,es:[di-1]	; get immediately previous byte
	or	al,al			; see if library null terminator immediately previous
	je	soldefdiscard	; yes, this was a match
	cmp	al,'\'			; see if dirspec previous
	je	soldefdiscard	; yes, this was a match
	cmp	al,':'			; see if pathspec previous
	je	soldefdiscard	; yes, this was a match
	jmp	SHORT soldefreset	; no match, keep trying

; save library file name
; WorkingBuffer -> file name to store
solfindlib:
	call	FindLIBFile	; find the library file, make sure exists, update with LIB e-var path if necessary
	jc	solret			; flush this file

solinclib:
	mov	OBJBecomesLIBFlag,OFF	; re-init OBJ becoming library file flag
	inc	TotalLIBCount	; bump total library file count

	cmp	LIBFlagSelector,0	; see if LIB flag selector allocated
	jne	solchklib		; yes

; allocate memory to store library flags
	push	dx
	mov	dx,SIZELIBFLAGBLK
	mov	LIBNameBlkSize,dx
	call	AllocateMemory	; allocate memory
	mov	LIBFlagSelector,ax	; save selector
	push	es			; zero block
	xor	di,di
	mov	es,ax
	mov	cx,SIZELIBFLAGBLK/4
	xor	eax,eax
	rep	stosd
	pop	es
	pop	dx

solchklib:
	cmp	LIBNameSelector,0	; see if LIB file name selector allocated
	jne	sollext			; yes, extend by number of file name chars+1 for storage

; allocate memory to store library file names
	push	dx
	mov	dx,2048			; initially allocate 2K block for storage
	mov	LIBNameBlkSize,dx
	call	AllocateMemory	; allocate memory
	mov	LIBNameSelector,ax	; save selector
	pop	dx

; dx == count of bytes in file name not including null terminator byte
sollext:
	mov	ax,LIBNameSelector	; get selector of memory block to expand
	inc	dx				; adjust for null terminator byte
	push	ParseLIBNameOff	; save old storage offset
	push	dx			; save total length of chars to transfer including null terminator
	add	dx,ParseLIBNameOff	; compute new segment length low word
	jc	internal1		; improper overflow
	mov	ParseLIBNameOff,dx	; update offset for next object module name
	mov	di,LIBNameBlkSize	; get current size of block in di

; dx == new block used size, di == current block size, ax == selector
	cmp	dx,di			; see if block used > block size
	jbe	sol3			; no
	add	di,2048
	jc	internal1		; improper overflow
	mov	dx,di			; new block size
	mov	LIBNameBlkSize,dx	; update current block size
	call	ResizeMemory	; resize memory block

sol3:
	pop	cx				; restore transfer chars to cx

; transfer file name -> WorkingBuffer to selector memory
	pop	di				; di -> offset within memory to store name
	push	es			; save es -> data
	mov	es,ax			; es -> selector

	cmp	LIBAtFrontFlag,0	; see if adding library name to front of block
	je	solnametran		; no

; shift library flags back one word entry
	push	cx
	push	di
	push	ds
	push	es
	mov	cx,(SIZELIBFLAGBLK/2)-1
	std					; reverse string moves so no overwrites
	mov	si,SIZELIBFLAGBLK-4
	mov	di,SIZELIBFLAGBLK-2
	mov	es,LIBFlagSelector
	mov	ds,LIBFlagSelector
	rep	movsw
	cld					; restore strings moves to increment
	or	WORD PTR ds:[0],USEALLMODULESFLAG	; set use all modules flag for new library
	pop	es
	pop	ds
	pop	di
	pop	cx				; restore count of chars in new lib name

	cmp	TotalLIBCount,1	; see if first library
	jbe	solflagoff

; shift current library names to back
libshift:
	push	cx			; save count of chars in new library name
	push	ds
	std					; reverse string moves so no overwrites
	mov	si,di
	dec	si				; si -> last char previously saved
	xchg	cx,di		; cx holds count of chars to move
	add	di,cx
	inc	cx				; adjust for relative zero offset for transfer count
	dec	di				; di -> last slot in expanded block
	push	es
	pop	ds				; ds -> library name block
	shr	cx,1			; convert byte count to write to words (no need for movsd optimization)
	rep	movsw
	inc	si
	inc	di				; position for leftover byte
	rcl	cx,1			; pick up carry
	rep	movsb			; transfer leftover byte, if any
	pop	ds
	pop	cx				; restore count of chars in new lib name
	xor	di,di			; di -> start of block
	cld					; restore strings moves to increment

solflagoff:
	mov	LIBAtFrontFlag,0	; reset library placed at front flag

solnametran:
	mov	si,OFFSET DGROUP:WorkingBuffer

; es:di -> string destination
; ds:si -> string source
	shr	cx,1			; convert byte count to write to words
	rep	movsw
	rcl	cx,1			; pick up carry
	rep	movsb			; transfer leftover byte, if any
	pop	es				; restore es -> data

solret:
	pop	si				; restore critical register
	ret
SaveOBJLIBFileName	ENDP

;*****************************
;* SAVEEXEFILENAME           *
;*****************************

; save EXE file
; upon entry al holds first char of name, si -> following chars, if any
; updates si
; destroys ax,bx,cx,dx,di

SaveEXEFileName	PROC
	mov	di,OFFSET DGROUP:EXEFileName	; di -> destination

; force name to all caps
	cmp	al,'a'			; check lower bounds
	jb	sefsave			; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	sefsave			; not lowercase
	sub	al,20h			; force lowercase to uppercase

sefsave:
	stosb				; save first char of file name
	call	GetFileNameString	; non-null name by definition, no null check needed
	mov	bx,OFFSET DGROUP:EXEFileName	; di -> file name
	call	CheckExtension	; see if extension was given for file
	jnc	sexret			; extension exists

; add '.EXE' extension, bx -> '.' position
	mov	WORD PTR ds:[bx],'.'+('E'*256)
	mov	WORD PTR ds:[bx+2],'X'+('E'*256)

sexret:
	ret
SaveEXEFileName	ENDP

;*****************************
;* SAVEMAPFILENAME           *
;*****************************

; save MAP file
; upon entry al holds first char of name, si -> following chars, if any
; updates si
; destroys ax,bx,cx,dx,di

SaveMAPFileName	PROC
	mov	IsMAPOption,ON	; flag that map option turned on (by specifying map file name)
	mov	di,OFFSET DGROUP:MAPFileName	; di -> destination

; force name to all caps
	cmp	al,'a'			; check lower bounds
	jb	smfsave			; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	smfsave			; not lowercase
	sub	al,20h			; force lowercase to uppercase

smfsave:
	stosb				; save first char of file name
	call	GetFileNameString	; non-null name by definition, no null check needed
	mov	bx,OFFSET DGROUP:MAPFileName	; di -> file name
	call	CheckExtension	; see if extension was given for file
	jnc	sobret			; extension exists

; add 'MAP' extension, bx -> '.' position
	mov	WORD PTR ds:[bx],'.'+('M'*256)
	mov	WORD PTR ds:[bx+2],'A'+('P'*256)

sobret:
	ret
SaveMAPFileName	ENDP

;*****************************
;* CHECKEXTENSION            *
;*****************************

; check the file's extension, if none set carry flag, reset otherwise
; upon entry bx -> file name
; returns bx -> null terminator of file name if no extension
; destroys ax,bx

CheckExtension PROC
	push	dx			; save critical register
	mov	ax,bx			; save -> start of file name

celoop1:
	cmp	BYTE PTR [bx],0	; check for end of file
	je	ce2				; found it
	inc	bx
	jmp	SHORT celoop1

ce2:
	mov	dx,bx			; save -> null terminator

celoop2:
	dec	bx				; backup to previous char of file name
	cmp	ax,bx			; see if at first char of name
	jae	noext			; yes, no extension on file
	cmp	BYTE PTR [bx],'\'	; backslash flags beginning of filename (path precedes)
	je	noext			; no extension if path reached before period
	cmp	BYTE PTR [bx],'.'	; period indicates file extension exists
	jne	celoop2			; not a period, keep scanning back
	clc					; flag extension exists

ceret:
	pop	dx				; restore critical register
	ret

noext:
	mov	bx,dx			; bx -> null terminator of name
	stc					; flag no extension
	jmp	SHORT ceret

CheckExtension ENDP

;*****************************
;* SETEXEFILENAME            *
;*****************************

; set EXE file name to default first object module name with .EXE extension
; destroys ax,bx,cx,dx,si,di

SetEXEFileName	PROC
	push	ds
	mov	ds,OBJNameSelector
	xor	si,si
	mov	di,OFFSET DGROUP:EXEFileName

; ds:si -> name of first object module, es:di -> file name storage
	xor	bx,bx			; index into strings
	mov	dx,bx			; dx holds position of last found '.'

; find end of file name string, not counting extension
seloop:
	mov	al,ds:[bx+si]	; get char from file name
	or	al,al			; see if null terminator
	je	atend			; zero, end of file name string
	cmp	al,'\'			; backslash resets last found period position (for relative directories)
	jne	notbs			; not a backslash
	xor	dx,dx			; reset last '.' position
	jmp	SHORT senext

notbs:
	cmp	al,'.'			; see if char is a period
	jne	senext			; no
	mov	dx,bx			; track last found period

senext:
	inc	bx				; bump offset into strings
	jmp	SHORT seloop

atend:
	or	dx,dx			; see if extension '.' was found
	je	se2				; no
	mov	bx,dx			; terminate name at '.'

se2:
	mov	cx,bx			; # of file name chars to transfer
	rep	movsb			; transfer file name
	pop	ds				; restore ds -> wl32 data

; add '.EXE' extension
	mov	WORD PTR ds:[di],'.'+('E'*256)
	mov	WORD PTR ds:[di+2],'X'+('E'*256)
	ret
SetEXEFileName	ENDP

;*****************************
;* SETMAPFILENAME            *
;*****************************

; set MAP file name to default (EXE name with .MAP extension)
; destroys ax,cx,si,di

SetMAPFileName	PROC
	mov	di,OFFSET DGROUP:EXEFileName
	mov	si,di

; 04/29/94
	xor	al,al
	mov	cx,86
	repne	scasb
	dec	di
	mov	cx,di
	sub	cx,OFFSET DGROUP:EXEFileName
	std
	dec	di

	mov	al,'.'
;	mov	cx,86			; name length always <=85
	repne	scasb		; find end of EXE file name

; 04/29/94
	cld
	add	di,2

	mov	cx,di
	sub	cx,OFFSET DGROUP:EXEFileName	; compute nonextension chars in name count
	mov	di,OFFSET DGROUP:MAPFileName
	rep	movsb			; save map file name without EXE extension

; add .MAP extension
	mov	WORD PTR ds:[di],'M'+('A'*256)
	mov	BYTE PTR ds:[di+2],'P'
	ret
SetMAPFileName	ENDP

;*****************************
;* SETSYMFILENAME            *
;*****************************

; set SYM file name to default (EXE name with .SYM extension)
; destroys ax,cx,si,di

SetSYMFileName	PROC
	mov	di,OFFSET DGROUP:EXEFileName
	mov	si,di

; 04/29/94
	xor	al,al
	mov	cx,86
	repne	scasb
	dec	di
	mov	cx,di
	sub	cx,OFFSET DGROUP:EXEFileName
	std
	dec	di

	mov	al,'.'
;	mov	cx,85			; name length always <=85
	repne	scasb		; find end of EXE file name

; 04/29/94
	cld
	add	di,2

	mov	cx,di
	sub	cx,OFFSET DGROUP:EXEFileName	; compute nonextension chars in name count
	mov	di,OFFSET DGROUP:SYMFileName
	rep	movsb			; save sym file name without EXE extension

; add .SYM extension
	mov	WORD PTR ds:[di],'S'+('Y'*256)
	mov	BYTE PTR ds:[di+2],'M'
	ret
SetSYMFileName	ENDP

IFDEF DLLSUPPORT

;*****************************
;* SETDLLFILENAME            *
;*****************************

; set DLL file name to EXE name without path
; destroys ax,cx,si,di

SetDLLFileName	PROC
	mov	di,OFFSET DGROUP:EXEFileName
	xor	al,al
	mov	cx,86
	repne	scasb
	dec	di				; di -> last char of EXE file name

; scan out base name without path
sdfscanloop:
	cmp	BYTE PTR ds:[di],'\'
	je	sdfdone			; at end of base name
	cmp	BYTE PTR ds:[di],':'
	je	sdfdone
	dec	di
	cmp	di,OFFSET DGROUP:EXEFileName
	jae	sdfscanloop

; di -> character preceding start of base name
sdfdone:
	inc	di				; di -> first char of base name
	mov	si,OFFSET DGROUP:DLLFileName

; transfer base name chars until null terminator or period
sdftransloop:
	mov	al,ds:[di]
	test	al,al
	je	sdfnull
	cmp	al,'.'			; don't add extension on DLL file name
	je	sdfnull
	mov	ds:[si],al
	inc	di
	inc	si
	jmp	sdftransloop

sdfnull:
	mov	BYTE PTR ds:[si],0	; null terminate

	ret
SetDLLFileName	ENDP

ENDIF

;*****************************
;* FINDOBJFILE               *
;*****************************

; find object module file listed in WorkingBuffer, add default extension, make sure exists,
; update WorkingBuffer with LIB e-var path if necessary
; return file char length in dx, carry flag reset if keep file, set if flush
; upon entry WorkingBuffer -> filename
; destroys ax,bx,cx,dx,di,si

FindOBJFile	PROC
	mov	si,OFFSET DGROUP:OBJExtensionText
	mov	bx,OFFSET DGROUP:WorkingBuffer
	call	CheckDefaultExtension	; check for extension, add default if required

	call	CheckFileMorphing	; see if morphing operations associated with file
	jnc	fo2				; no morphing operations

	call	CheckAddOption	; check if adding option, set if so
	call	CheckFileAddOBJ	; check if adding OBJ file
	call	CheckFileDelete	; check if need to delete file
	jc	foret			; file is to be deleted, indicate as a flush with carry flag set

; check if OBJ should become LIB, set flag if so
	call	CheckIsLIB

fo2:
	cmp	IsParseDisplayOption,OFF	; see if parsing display (file needn't exist)
	jne	fodone			; yes, don't check for file existence
	mov	dx,OFFSET DGROUP:WorkingBuffer
	call	CheckCurrentDirectory	; check current directory for file
	jnc	focount			; file found in current directory
	mov	bx,OFFSET DGROUP:OBJEVarText	; bx -> environment variable to search for paths

foshared:
	mov	di,OFFSET DGROUP:WorkingBuffer	; di -> filename
	call	SearchEVarDirectory	; search environment variable directories for file

focount:
	xor	dx,dx
	mov	si,OFFSET DGROUP:WorkingBuffer
	dec	dx				; pre-adjust for null terminator count

; count chars in file name
foloop:
	lodsb
	inc	dx
	or	al,al
	jne	foloop

fodone:
	clc					; flag keep file

foret:
	ret
FindOBJFile	ENDP

;*****************************
;* FINDLIBFILE               *
;*****************************

; find library file listed in WorkingBuffer, add default extension, make sure exists,
; update WorkingBuffer with LIB e-var path if necessary
; return file char length in dx, carry flag reset if keep file, set if flush
; upon entry WorkingBuffer -> filename
; destroys ax,bx,cx,dx,di,si

FindLIBFile	PROC
	mov	si,OFFSET DGROUP:LIBExtensionText
	mov	bx,OFFSET DGROUP:WorkingBuffer
	call	CheckDefaultExtension	; check for extension, add default if required

	call	CheckFileMorphing	; see if morphing operations associated with file
	jnc	fl2				; no morphing operations

	call	CheckAddOption	; check if adding option, set if so
	call	CheckFileAddOBJ	; check if adding OBJ file
	call	CheckFileDelete	; check if need to delete file
	jc	foret			; file is to be deleted, indicate as a flush with carry flag set

fl2:
	cmp	IsParseDisplayOption,OFF	; see if parsing display (file needn't exist)
	jne	fodone			; yes, don't check for file existence
	mov	dx,OFFSET DGROUP:WorkingBuffer
	call	CheckCurrentDirectory	; check current directory for file
	jnc	focount			; file found in current directory, count chars and return
	mov	bx,OFFSET DGROUP:LIBEVarText	; bx -> environment variable to search for paths
	jmp	SHORT foshared	; jump to code shared with find OBJ file

FindLIBFile	ENDP

;*****************************
;* CHECKFILEMORPHING         *
;*****************************

; check if file is in morphing operations list
; upon entry WorkingBuffer -> file name, possibly with pathspec
; return FoundInMorphDataFlag set to flag if found
; return carry set if found, reset if not
; maintain es

CheckFileMorphing	PROC
	push	es			; save critical register
	push	ds
	pop	es
	mov	FoundInMorphDataFlag,0	; init flag variable
	cmp	MorphDataBlock,0	; see if any morphing commands
	je	cmnone			; no

	mov	cx,255			; null terminator known to be within 255 chars
	mov	di,OFFSET DGROUP:WorkingBuffer
	xor	al,al
	repne	scasb		; di -> char after null terminator
	dec	di				; di -> null terminator

cmloop:
	cmp	di,OFFSET DGROUP:WorkingBuffer	; see if backed up to start of filename
	jbe	cm2			; yes
	cmp	BYTE PTR ds:[di-1],':'	; see if start of drivespec
	je	cm2			; yes
	cmp	BYTE PTR ds:[di-1],'\'	; see if start of pathspec
	je	cm2			; yes
	dec	di				; back up to previous char
	jmp	SHORT cmloop

; di -> start of base filename without drive or pathspec
cm2:
	mov	si,di			; ds:si -> base filename
	lodsb				; al holds first char, ds:si -> 2nd through nth char
	call	CheckCommandString	; check configuration morphing data
	test	FoundInMorphDataFlag,FILEDELETEFLAG OR ISLIBFLAG OR ADDOPTIONFLAG OR FILEADDOBJFLAG
	je	cmnone			; no associated morphing operations for file

	stc					; flag morphing operation
	pop	es				; restore critical register
	ret

cmnone:
	clc					; flag no morphing-associated operations for file
	pop	es				; restore critical register
	ret
CheckFileMorphing	ENDP

;*****************************
;* CHECKFILEDELETE           *
;*****************************

; check if file should be deleted from link list
; (listed in configuration file as FILEDELETE)
; upon entry WorkingBuffer -> file name, possibly with pathspec
; return carry set if delete, reset if not
; maintain es

CheckFileDelete	PROC
	test	FoundInMorphDataFlag,FILEDELETEFLAG	; see if found in morphing data and file delete
	je	cfdsave			; no

; file was found in morphing data, delete it
	stc					; flag delete
	ret

cfdsave:
	clc					; flag no delete
	ret
CheckFileDelete	ENDP

;*****************************
;* CHECKISLIB                *
;*****************************

; check if file should be change from OBJ to library
; (listed in configuration file as ISLIB)
; upon entry WorkingBuffer -> file name, possibly with pathspec
; maintain es

CheckIsLIB	PROC
	test	FoundInMorphDataFlag,ISLIBFLAG	; see if found in morphing data and ISLIB
	je	cilret			; no

; file was found in morphing data, flag as library
	mov	OBJBecomesLIBFlag,ON	; set OBJ becoming library file flag

cilret:
	ret
CheckIsLIB	ENDP

;*****************************
;* CHECKADDOPTION            *
;*****************************

; check if option should be added
; (listed in configuration file as ADDOPTION)
; upon entry WorkingBuffer -> file name without pathspec
; set option if appropriate
; maintain es

CheckAddOption	PROC
	test	FoundInMorphDataFlag,ADDOPTIONFLAG	; see if found in morphing data and add option
	je	caoret			; no

; option was found in morphing data, set it
	mov	bx,FoundInMorphOptionPtr	; ds:bx -> option
	mov	si,OFFSET DGROUP:ZeroValue	; si -> known null (option parameters fail)
	call	SetLinkOption	; set the option specified

caoret:
	ret
CheckAddOption	ENDP

;*****************************
;* CHECKFILEADDOBJ           *
;*****************************

; check if OBJ should be added (appended) as per file name
; (listed in configuration file as +OBJ:)
; upon entry WorkingBuffer -> file name without pathspec
; set option if appropriate
; maintain es

CheckFileAddOBJ	PROC
	test	FoundInMorphDataFlag,FILEADDOBJFLAG	; see if found in morphing data and add option
	je	cfaret			; no

	push	es			; save critical registers
	push	ds
	pop	es
	push	ds
	mov	si,FoundInMorphOptionPtr	; ds:si -> OBJ name to add
	mov	ds,MorphDataBlock
	mov	di,OFFSET DGROUP:TempBuffer

; copy filename at ds:si to TempBuffer at es:di
cfaloop:
	movsb
	cmp	BYTE PTR ds:[si-1],0	; see if null terminator transferred
	jne	cfaloop			; not yet

	pop	ds				; restore critical registers
	pop	es
	mov	si,OFFSET DGROUP:TempBuffer	; ds:si -> filename
	mov	eax,OBJAPPENDFLAG ; set flags
	xor	dx,dx			; no option pointer
	call	SaveMorphData	; save the morphing data/string (OBJAPPEND)

cfaret:
	ret
CheckFileAddOBJ	ENDP

;*****************************
;* ADDEVARLINKFILES          *
;*****************************

; add environment variable dependent link files to processing
; return carry set if match found, reset if not
; if found, copy link file into CommandLineString for further processing
; maintain ds,es

AddEvarLinkFiles	PROC
	push	es			; save critical register
	mov	ax,MorphDataBlock
	or	ax,ax			; see if any morphing commands
	je	aelfail			; no
	mov	es,ax
	mov	cx,MorphDataCount	; cx holds count of entries in block
	xor	bx,bx

; es:bx -> entry
aelentloop:
	mov	di,bx			; di -> entry
	test	DWORD PTR es:[di+2],EVARLINKFILEFLAG	; see if proper command
	je	aelnext

aelstart:
	add	di,8			; es:di -> start of e-var setting

; check the environment block for matching on morph block setting
    push    ds			; save critical registers
	push	es
	push	bx
    xor si,si			; starting location for target string check
	mov	ds,PSP
    mov ax,ds:[2ch]		; get environment segment from offset 2ch in PSP
    mov ds,ax			; ds -> environment segment

find_evar:
    xor bx,bx			; offset into target string

find_loop:
	lodsb				; get byte from environment string, point to next
    cmp al,es:[bx+di]	; does environment char match morph string char
    je  byte_match		; yes, try next location
    or  al,ds:[si]		; two zero values in a row mean the end of the environment
    jne find_evar		; not the end of the environment
    jmp SHORT ael3		; at end of environment, no morph environment char

; check that morph e-var is not part of another environment string
byte_match:
    or  bx,bx			; bx is zero if first char is matched
    jne ael2			; not first char, test already done
    cmp si,1			; si equals one if e-var is first string in environment block
    je  ael2			; no previous environment string
    cmp BYTE PTR ds:[si-2],0	; check if char before e-var was nonzero
    jne find_evar		; yes, e-var is a subset of another string, keep looking

ael2:
    inc bx				; a match, move to next byte of target string
    cmp BYTE PTR es:[bx+di],0	; see if at end of target
    jne find_loop		; not yet, keep comparing
    jmp SHORT aelfound	; matched

ael3:
	pop	bx				; restore critical registers
	pop	es
	pop	ds

; move to next entry
aelnext:
	add	bx,es:[bx]		; bx -> next entry
	loop aelentloop		; check all entries for match

aelfail:
	clc					; flag failure
	pop	es				; restore es
	ret

; transfer link file name to CommandLineString
aelfound:
	pop	bx				; restore critical registers
	pop	ds				; ds -> morphing data block (reversed es,ds)
	pop	es				; es -> DGROUP
	and	DWORD PTR ds:[bx+2],NOT EVARLINKFILEFLAG	; shut off further processing
	mov	si,ds:[bx+6]	; ds:si -> link file name
	mov	di,OFFSET DGROUP:CommandLineString

aelnameloop:
	lodsb				; get morphing data block character
	stosb
	or	al,al
	jne	aelnameloop

	push	es
	pop	ds				; restore ds -> DGROUP

; successful match on environment variable
	stc					; flag success
	pop	es
	ret
AddEvarLinkFiles	ENDP

;*****************************
;* CHECKDEFAULTEXTENSION     *
;*****************************

; check for file extensions, add default extension if required
; upon entry si -> default extension, bx -> file name
; destroys ax,cx,di,si

CheckDefaultExtension	PROC
	mov	di,bx			; save pointer to name
	call	CheckExtension	; see if need to add default extension
	jnc	cdret				; extension already exists
	mov	bx,di			; restore bx -> name
	xor	al,al
	mov	cx,255			; name known <255 chars in length
	repne	scasb		; make di -> char past null terminator
	dec	di				; di -> null terminator
	movsd				; add default extension
	mov	BYTE PTR ds:[di],0
	mov	ax,di
	sub	ax,bx			; see if name is too long
	cmp	ax,80
	jbe	cdret			; name length is ok

	mov	dx,OFFSET DGROUP:WorkingBuffer	; dx -> file name

FileNotFoundError:
	mov	al,2			; list error as DOS file not found
	call	DOSErrorExit

cdret:
	ret
CheckDefaultExtension	ENDP

;*****************************
;* CHECKCURRENTDIRECTORY     *
;*****************************

; check current directory for file
; returns carry flag set if not found, reset if found
; upon entry dx -> file name
; destroys ax

CheckCurrentDirectory	PROC
	mov	ax,4300h		; get file attributes (check file existence)
	int	21h
	jnc	ccdfound		; file found ok

; file not found, check if meets error code requirements, if so then return
;  with carry flag set, otherwise abort with DOS error
	cmp	ax,2			; file not found
	je	ccdfail
	cmp	ax,3			; path not found
	je	ccdfail
	cmp	ax,5			; access denied
	je	ccdfail
	call	DOSErrorExit	; DOS error unrelated to nonpresent file

ccdfail:
	stc					; flag file not found
	ret

ccdfound:
	clc					; flag file found
	ret
CheckCurrentDirectory	ENDP

;*****************************
;* SEARCHEVARDIRECTORY       *
;*****************************

; file not found in current directory,
; search environment variable directories for file (OBJ= or LIB=)
; upon entry di -> filename, bx -> environment variable string
; return di -> updated file with path
; destroys ax,bx,cx,dx,si,di

SearchEVarDirectory	PROC
	push	es			; save critical register
	push	di			; save -> file name
	cmp	BYTE PTR ds:[di],'.'	; see if file already has path, if so force error
	jne	sev2

tofnfe:
	pop	dx				; dx -> file name
	jmp	NEAR PTR FileNotFoundError

sev2:
	cmp	BYTE PTR ds:[di],'\'	; check for unallowed filespec
	je	tofnfe
	cmp	BYTE PTR ds:[di+1],':'	; check for drivespec
	je	tofnfe

; see if environment variable is present
	xor	si,si
	mov	es,PSP
	mov	es,es:[2ch]		; es:si -> environment block strings

sevscan:
	xor	di,di			; offset into target string to match (LIB= or OBJ=)

matchloop:
	lods	BYTE PTR es:[0]	; get byte from environment string
	cmp	al,ds:[bx+di]	; see if matches target
	je	bytematch		; yes
	or	al,es:[si]		; two zero values in a row mean end of environment
	jne	sevscan			; no target match, more chars left
tofnfe2:
	jmp	SHORT tofnfe	; end of environment, no match, search failed

; check that environment string match is not part of another environment string
bytematch:
	or	di,di			; di is zero if matching first char
	jne	sevnext			; not matching first char
	cmp	si,1			; si==1 if first string in environment block
	je	sevnext			; first string, not a part of another string by definition
	cmp	BYTE PTR es:[si-2],0	; char before environment string in block must be nonzero
	jne	sevscan			; nonzero, subset of another string, keep scanning for match

sevnext:
	inc	di				; match, move to next byte of target
	cmp	di,4			; check if all bytes matched (all extension are four chars)
	jb	matchloop		; not yet

pathloop:
	mov	di,OFFSET DGROUP:TempBuffer	; di will -> file name with path

transloop:
	lods	BYTE PTR es:[0]	; get path character

; force name to all caps
	cmp	al,'a'			; check lower bounds
	jb	sevtermchk		; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	sevtermchk		; not lowercase
	sub	al,20h			; force lowercase to uppercase

sevtermchk:
	cmp	al,';'			; see if terminator character
	je	prefcomp		; yes, prefix complete
	cmp	al,' '			; whitespace also terminates
	jbe	prefcomp
	mov	ds:[di],al		; save path character
	inc	di				; bump name storage slot
	jmp	SHORT transloop

prefcomp:
	cmp	BYTE PTR ds:[di-1],'\'	; check if backslash in place
	je	sev4			; yes
	mov	BYTE PTR ds:[di],'\'	; place backslash
	inc	di				; di -> char past backslash

sev4:
	mov	ax,si			; save -> current e-var parse offset
	pop	si				; si -> original file name
	push	si			; restore to stack
	push	ax			; save -> current e-var offset
	push	es			; save es -> environment block
	push	ds
	pop	es				; es -> wl32 data

; ds:si -> original file name
; es:di -> file name with path
nameloop:
	lodsb

; force name to all caps
	cmp	al,'a'			; check lower bounds
	jb	sevstore		; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	sevstore		; not lowercase
	sub	al,20h			; force lowercase to uppercase

sevstore:
	stosb
	cmp	BYTE PTR ds:[si-1],0	; see if null terminator transferred
	jne	nameloop		; no, keep transferring
	pop	es				; es -> environment block

sevsearch:
	mov	dx,OFFSET DGROUP:TempBuffer	; get filename with path
	call	CheckCurrentDirectory	; check for file in this directory path
	jnc	sevfound		; found, transfer name to appropriate storage

	pop	si				; restore si -> current e-var offset
	cmp	BYTE PTR es:[si-1],0	; see if final terminator
	je	tofnfe2			; yes
	cmp	BYTE PTR es:[si],0	; see if final terminator
	jne	pathloop		; no, try next path
	jmp	SHORT tofnfe2	; out of directories to search, file not found

; TempBuffer holds full file name, transfer to di ->
sevfound:
	pop	ax				; trash e-var position stored on stack
	pop	di				; di -> file name storage
	pop	es				; restore es -> wl32 data
	mov	si,OFFSET DGROUP:TempBuffer

IFDEF DEBUG
	mov	dx,si			; dx -> filename with path
	mov	bx,1
	mov	cx,20
	mov	ah,40h
	int	21h
ENDIF

newloop:
	movsb
	cmp	BYTE PTR ds:[si-1],0	; see if null terminator transferred
	jne	newloop			; not yet

IFDEF DEBUG
	mov	dx,di			; dx -> filename with path
	mov	bx,1
	mov	cx,20
	mov	ah,40h
	int	21h
ENDIF

	ret
SearchEVarDirectory	ENDP

;*****************************
;* CHECKCOMMANDSTRING        *
;*****************************

; check if string is a morphing command/option
; upon entry ds:si -> second char of string, al == first of string
; returns carry flag set if not a valid command string,
;  carry flag reset and and si updated if command
; sets FoundInMorphDataFlag if string found in data block regardless of
;  command validity (even if NOTCOMMANDFLAG set), reset otherwise
; sets FoundInMorphOptionPtr if string found, only valid if add option
; maintain ds,es, maintain ax,si if not command otherwise ax,si destroyed
; destroys bx,cx,dx,di

CheckCommandString	PROC
	push	es			; save critical register
	push	ax
	push	si			; save si, al original values
	mov	dx,MorphDataBlock
	or	dx,dx			; see if any morphing commands
	je	ccsfail			; no
	mov	cx,MorphDataCount	; cx holds count of entries in block
	mov	es,MorphDataBlock
	xor	di,di
	movzx	ebx,di
	mov	FoundInMorphDataFlag,ebx	; turn off found in morphed data block flag
	mov	FoundInMorphOptionPtr,bx	; turn off found in morphed data option pointer

; es:bx -> entry
ccsentloop:
	pop	si				; restore si -> second char
	push	si			; back on stack
	dec	si
	lodsb				; al holds first char of string
	mov	di,bx			; di -> entry
	test	DWORD PTR es:[di+2],FREEFORMATONLYFLAG	; see if freeformat exclusive
	je	ccsstart		; no
	cmp	FreeFormatFlag,OFF	; see if freeformat
	je	ccsnext			; no, don't try this entry, freeformat exclusive

ccsstart:
	add	di,8			; di -> start of morphing string to try

	cmp	al,'a'			; check lower bounds
	jb	ccsfirst		; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	ccsfirst		; not lowercase
	sub	al,20h			; force lowercase to uppercase

ccsfirst:
	scasb				; see if first char matches
	jne	ccsnext			; no

; ds:si -> string given to linker, upper/lowercase, stop scan character terminated
; es:di -> morphed data string, uppercase, null terminated, possible bracketed optional chars
	mov	ShortcutFlag,OFF	; init flags

ccscharloop:
	cmp	BYTE PTR es:[di],'['	; see if start of shortcut valid
	jne	ccscomp			; no
	inc	di				; bump past shortcut flag
	mov	ShortcutFlag,ON	; flag shortcut valid

ccscomp:
	lodsb				; get char to match
	cmp	al,'a'			; check lower bounds
	jb	ccscomp2		; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	ccscomp2		; not lowercase
	sub	al,20h			; force lowercase to uppercase

ccscomp2:
	scasb				; see if matches
	je	ccscharok		; yes, okay so far
	cmp	al,' '			; see if end of command string, stop at whitespace
	jbe	ccsendstring	; yes
	cmp	al,'/'			; stop at option char
	je	ccsendstring
	cmp	al,','			; stop at comma char
	je	ccsendstring
	cmp	al,';'			; stop at semicolon char
	je	ccsendstring
	cmp	al,'+'			; stop at plus char
	je	ccsendstring
	cmp	al,ParseTermChar	; stop at termination char
	jne	ccsnext			; not end of command string, not a match

ccsendstring:
	dec	si				; backup si to ending char
	cmp	ShortcutFlag,0	; see if shortcut ok
	jne	ccsmatch		; yes

; move to next entry
ccsnext:
	add	bx,es:[bx]		; bx -> next entry
	loop ccsentloop		; check all entries for match

; check for STACKABLE command that may have matched previously
	cmp	FoundInMorphDataFlag,0
	jne	ccschkcom		; nonzero morphing data flag, previous STACKABLE command

ccsfail:
	pop	si				; restore si, al original values
	pop	ax

ccsfail2:
	pop	es				; restore es
	stc					; flag not a command
	ret

ccscharok:
	mov	al,es:[di]
	or	al,al			; see if end of command and morph string
	je	ccsmatch		; yes
	cmp	al,'*'			; end of morph string if wildcard
	je	ccsmatch
	cmp	al,'+'			; end of morph string if wildcard
	je	ccsmatch
	jmp	SHORT ccscharloop	; keep checking

; strings match to at least minimum number of chars
; set appropriate flags
; ds:si -> char past terminating char
ccsmatch:
	mov	ax,es:[bx+6]	; get option pointer
	cmp	FoundInMorphOptionPtr,0	; see if previous pointer to option
	jne	ccsgetflag		; yes, don't overwrite it
	mov	FoundInMorphOptionPtr,ax	; keep pointer to option, if any

ccsgetflag:
	mov	eax,es:[bx+2]	; get flags
	or	FoundInMorphDataFlag,eax	; merge in found in morphed data block flag
	test	eax,STACKABLEFLAG	; see if more than one string possible
	jne	ccsnext			; yes, try next command

ccschkcom:
	test	FoundInMorphDataFlag,NOTCOMMANDFLAG	; see if not a command
	jne	ccsfail			; yes, force failure
	test	eax,OBJNAMEFLAG	; see if OBJNAME command
	je	ccsmatch2		; no
	mov	FileListFlag,1	; flag parsing objs
	jmp	SHORT ccssuccess

ccsmatch2:
	test	eax,LIBNAMEFLAG	; see if LIBNAME command
	je	ccsmatch3		; no
	test	eax,LIBSEARCHFLAG	; see if LIBSEARCH command
	je	ccsnotsearch	; no

	mov	IsLIBSearchOption,ON	; turn on /ls LIB search option

ccsnotsearch:
	mov	FileListFlag,2	; flag parsing libss
	jmp	SHORT ccssuccess

ccsmatch3:
	test	eax,EXENAMEFLAG	; see if EXENAME command
	je	ccsmatch4		; no

ccsexeloop:
	lodsb				; get char
	cmp	al,CR			; see if at end of line
	je	ccssuccess		; yes, don't parse out nonexistent name
	or	al,al			; see if end of file
	je	ccssuccess
	cmp	al,' '
	jbe	ccsexeloop		; not at EXE name yet
	push	es			; save es -> morph data block
	push	ds
	pop	es				; es -> wl32 data for SaveEXEFileName procedure
	call	SaveEXEFileName
	pop	es				; restore es -> morph data block
	jmp	SHORT ccssuccess

ccsmatch4:
	test	eax,OPTIONFLAG	; see if option
	je	ccsmatch5		; no

	push	bx			; save critical register
	mov	bx,es:[bx+6]	; ds:bx -> option
	push	es			; save es -> morph data block
	push	ds
	pop	es				; es -> wl32 data for SaveEXEFileName procedure
	call	SetLinkOption	; set the option specified
	pop	es				; restore es -> morph data block
	pop	bx				; restore critical register
	jmp	SHORT ccssuccess

ccsmatch5:

;@@@ code goes here

ccssuccess:
	mov	eax,es:[bx+2]	; get flags
	test	eax,FLUSHLINEFLAG; see if flushing line
	je	ccschkflop		; no, check if flushing option
	pop	si				; restore si, al original values
	pop	ax
	call	ScanToEOL	; scan to the end of the line
	jmp	SHORT ccssucret

ccschkflop:
	test	eax,FLUSHOPTIONFLAG; see if flushing option
	je	ccskill			; no, check if flushing option

ccsoptloop:
	mov	al,ds:[si]		; get character
	cmp	al,' '			; see if end of command string, stop at whitespace
	jbe	ccskill			; yes
	cmp	al,'/'			; stop at option char
	je	ccskill
	cmp	al,','			; stop at comma char
	je	ccskill
	cmp	al,';'			; stop at semicolon char
	je	ccskill
	cmp	al,'+'			; stop at plus char
	je	ccskill
	inc	si				; move to next character in string
	jmp	SHORT ccsoptloop

ccskill:
	add	sp,4			; kill si,ax original values

ccssucret:
	pop	es				; restore es
	clc					; flag a command
	ret
CheckCommandString	ENDP

;*****************************
;* ADDOBJAPPENDMODULES       *
;*****************************

; add OBJAPPEND modules to link list
; maintain ds,es

AddOBJAPPENDModules	PROC
	push	es			; save critical register
	mov	ax,MorphDataBlock
	or	ax,ax			; see if any morphing commands
	je	aoaret			; no
	mov	es,ax
	mov	cx,MorphDataCount	; cx holds count of entries in block
	xor	bx,bx

; es:bx -> entry
aoaentloop:
	mov	si,bx			; si -> entry
	test	DWORD PTR es:[si+2],OBJAPPENDFLAG	; see if proper command
	je	aoanext

aoastart:
	add	si,8			; si -> start of file name

	mov	FreeFormatFlag,ON
	mov	FileListFlag,1	; flag object module
	push	bx			; save critical registers
	push	cx
	push	es

; transfer name down to DGROUP for SaveOBJLIBFileName
	push	ds
	push	es
	pop	ds				; ds -> morphing data block
	pop	es				; es -> DGROUP
	mov	di,OFFSET DGROUP:CompBuffDest

aoanameloop:
	lodsb				; get morphing data block character
	stosb
	or	al,al
	jne	aoanameloop

	push	es
	pop	ds				; ds -> DGROUP
	mov	si,OFFSET DGROUP:CompBuffDest
	lodsb				; setup al,si for SaveOBJLIBFileName call
	call	SaveOBJLIBFileName
	pop	es
	pop	cx				; restore critical registers
	pop	bx

; move to next entry
aoanext:
	add	bx,es:[bx]		; bx -> next entry
	loop aoaentloop		; check all entries for match

aoaret:
	pop	es				; restore es
	ret
AddOBJAPPENDModules	ENDP

ENDS

END
