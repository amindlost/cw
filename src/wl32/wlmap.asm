;*********************************************************************
;*   WLMAP.ASM                                                       *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          06/18/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3h                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   map file routines                                               *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLMAP
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Equates                   *
;*****************************

MAPSYMFILEBUFFERSIZE	EQU	8000h
SYMSIGSIZE	EQU	8

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLDATA.INC

;*****************************
;* Public declarations       *
;*****************************

PUBLIC	WriteMAPFile
PUBLIC	WriteSYMFile

PUBLIC	MapPublics

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

MAPFileHandle	DW	?	; map file handle
SYMFileHandle	DW	?	; SYM file handle
PrintMasterEntry	DW	?	; entry in grpdef/pubsym block of group/public being printed

PubNameSymbolCount	DD	?	; count of public name symbols

; variables used in shell sort
iValue	DD	?
vValue	DD	?

SymbolStruc	STRUC
	ssSymbolNext	DD	?	; offset to next symbol, (-1 if no next)
	ssSymbolType	DB	?	; 0 for normal seg:off or 1 for absolute, ie, no seg.
	ssSymbolDword	DD	?	; symbols offset value.
	ssSymbolSeg	DW	?	; symbols seg value, 0-?? same as for relocations.
	ssSymbolTLen	DB	?	; Length of symbol name text.
	ssSymbolText	DB	?	; symbol name text, variable size, no terminator byte, no length byte
SymbolStruc	ENDS

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

ProgramTextLen	DB	ProgramTextStop-ProgramText
ProgramText	DB	CR,LF,,' PROGRAM: '
ProgramTextStop	=	$

DateTextLen	DB	DateTextStop-DateText
DateText	DB	CR,LF,,' DATE:    '
DateTextStop	=	$

TimeTextLen	DB	TimeTextStop-TimeText
TimeText	DB	CR,LF,,' TIME:    '
TimeTextStop	=	$

MapSegTextLen	DB	MapSegTextStop-MapSegText
MapSegText	DB	CR,LF,CR,LF,' Start     Stop      Length    Name                    Class          Count',CR,LF
MapSegTextStop	=	$

OrgGroupTextLen	DB	OrgGroupTextStop-OrgGroupText
OrgGroupText	DB	CR,LF,' Origin      Group',CR,LF
OrgGroupTextStop	=	$

EntryTextLen	DB	EntryTextStop-EntryText
EntryText		DB	CR,LF,' Program entry point at '
EntryTextStop	=	$

CreatingMAPTextLen	DB	CreatingMAPTextStop-CreatingMAPText
CreatingMAPText	DB	CR,LF,'*** Creating map file'
CreatingMAPTextStop		=	$

CreatingSYMTextLen	DB	CreatingSYMTextStop-CreatingSYMText
CreatingSYMText	DB	CR,LF,'*** Creating SYM file'
CreatingSYMTextStop		=	$

AddressTextLen	DB	AddressTextStop-AddressText
AddressText		DB	CR,LF,' Address                 Publics by address',13,10,13,10
AddressTextStop	=	$

PubNameTextLen	DB	PubNameTextStop-PubNameText
PubNameText		DB	CR,LF,' Address                 Publics by name',13,10,13,10
PubNameTextStop	=	$

SYMSigText	DB	'CWDSYM02'

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

MAPSYMBufferOffset	DW	0	; current MAP/SYM buffer write offset
MAPSYMBufferSelector	DW	0	; MAP/SYM buffer selector
PubNameSymPtrSeg	DW	0	; segment of public name symbol pointers
SegmentNumber	DW	1	; segment number in map file
SYMLastEntryFPos	DD	0	; pointer to start of last written entry
;TotalMAPSYMBufferOffset	DD	0	; total offset of MAP/SYM buffer even after flushes

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	CompBuffSource:BYTE
EXTRN	CurrentOBJ:WORD,CurrentLIB:WORD
EXTRN	EntryOffsetValue:DWORD
EXTRN	EntrySegmentValue:DWORD
EXTRN	FirstGrpDefBlkPtr:WORD
EXTRN	FirstPubSymBlkPtr:WORD
EXTRN	FirstSegment:DWORD
EXTRN	LIBDictTablePtr:WORD
EXTRN	MAPFileBuffer:BYTE
EXTRN	OBJBuffSelTablePtr:WORD

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	AllocateMemory:PROC
EXTRN	AllocateBigMemory:PROC,ReleaseMemory:PROC
EXTRN	CreateFile:PROC
EXTRN	DisplayLinkInfo:PROC
EXTRN	NormalDSSISource:PROC,NormalESDIDest:PROC
EXTRN	WriteFile:PROC,WriteFileVarString:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* WRITEMAPFILE              *
;*****************************

; create and write info to map file

WriteMapFile	PROC
	mov	dx,MAPSYMFILEBUFFERSIZE
	call	AllocateMemory
	mov	MAPSYMBufferSelector,ax
	mov	bx,OFFSET DGROUP:CreatingMAPText
	call	DisplayLinkInfo
	call	InitMAPFile
	call	MapSegments
	call	MapGroups
	call	MapPublics
	call	MapEntry
	mov	bx,MAPFileHandle
	mov	ah,3eh			; close map file
	int	21h
	ret
WriteMapFile	ENDP

;*****************************
;* WRITESYMFILE              *
;*****************************

; create and write info to SYM file
; es -> DGROUP

WriteSYMFile	PROC
	cmp	MAPSYMBufferSelector,0	; see if buffer allocated yet
	jne	wsfinit			; yes
	mov	dx,MAPSYMFILEBUFFERSIZE
	call	AllocateMemory
	mov	MAPSYMBufferSelector,ax

wsfinit:
	mov	PubNameSymbolCount,0
	mov	bx,OFFSET DGROUP:CreatingSYMText
	call	DisplayLinkInfo
	mov	dx,OFFSET DGROUP:SYMFileName
	call	CreateFile
	mov	SYMFileHandle,ax	; save SYM file handle

; write SYM file signature
	mov	bx,ax
	mov	dx,OFFSET DGROUP:SYMSigText
	mov	cx,SYMSIGSIZE
	call	WriteFile

; write SYM file line numbers, if any
	mov	CurrentOBJ,1	; init current object module number
	mov	CurrentLIB,0	; init current library to zero flag value

wsfobjloop:
	mov	si,CurrentOBJ
	dec	si				; make relative zero
	add	si,si			; convert to word offset
	mov	fs,OBJBuffSelTablePtr	; fs:bx -> entry in table of memory block selectors
	mov	fs,fs:[si]
	jmp	SHORT wsfchkline

wsflibinit:
	xor	ax,ax
	cmp	TotalLIBCount,ax	; see if any library files to process
	je	wsfchksym		; no
	cmp	LIBDictTablePtr,ax	; see if specified libraries were used
	je	wsfchksym		; no

	mov	CurrentLIB,1	; init current library

wsflibloop:
	mov	bx,CurrentLIB
	dec	bx				; make relative zero
	add	bx,bx			; convert to word offset

	mov	fs,LIBDictTablePtr	; fs:bx -> entry in table of memory block selectors
	mov	ax,fs:[bx]		; ax -> first buffer of library dictionary
	or	ax,ax			; see if any buffer at all (occurs if all symbols resolved before this lib)
	je	wsfnextlib		; no buffer, no modules used

	mov	fs,ax			; fs -> first buffer of library dictionary
	test	fs:[LIBDictHeaderStruc.ldhsFlags],MODULEUSEDFLAG	; see if modules used from library
	je	wsfnextlib		; no modules used in library
	mov	cx,fs:[LIBDictHeaderStruc.ldhsModUsedCount]	; get count of modules used
	xor	ebx,ebx			; index for module lookup

wsfmodloop:
	push	ebx			; save module lookup index
	push	cx			; save module count
	push	fs			; save fs -> library dictionary buffer
	mov	fs,fs:[LIBDictHeaderStruc.ldhsModBuffPtr]	; fs -> module buffer pointer table
	mov	fs,fs:[2*ebx]	; fs -> current module buffer base

; fs -> buffer base of object or library module
wsfchkline:
	mov	ax,fs:[IOBuffHeaderStruc.ibhsSourceLinePtr]
	or	ax,ax			; see if any source lines
	je	wsfnextfile		; no

wsfsegloop:
	mov	gs,ax			; gs -> source line block
	mov	di,OFFSET DGROUP:MAPFileBuffer	; init di -> write buffer
	mov	ds:[di+SymbolStruc.ssSymbolType],0ffh	; flag source line
	mov	ds:[di+SymbolStruc.ssSymbolDword],0	; zero symbol offset value

	add	di,OFFSET SymbolStruc.ssSymbolTLen	; di -> start of name storage
	lds	si,fs:[IOBuffHeaderStruc.ibhsModNamePtr]	; ds:si -> module name
	call	NormalDSSISource	; normalize string

; remove any leading path or drivespec, 06/18/96
	mov	dx,si
	inc	dx			; dx -> name past length byte
	xor	ax,ax
	mov	al,ds:[si]
	test	al,al
	je	wsfback		; zero length name
	inc	ax			; adjust for length byte
	add	si,ax		; si -> past end byte of name

wsfback:
	xor	cx,cx

wsfremloop:
	dec	si
	cmp	si,dx
	jb	wsfremdone
	cmp	BYTE PTR ds:[si],'\'
	je	wsfremdone
	cmp	BYTE PTR ds:[si],':'
	je	wsfremdone
	inc	cx
	jmp	wsfremloop

wsfremdone:
	inc	si
	mov	al,cl			; get name length byte

;	lodsb				; get name length byte
;	mov	cl,al
;	xor	ch,ch			; cx holds length of name string
	
	stosb				; save symbol name text byte
	push	 cx			; transfer public symbol name
	shr	cx,2
	rep	movsd
	pop	cx
	and	cx,3
	rep	movsb
	push	es
	pop	ds				; restore ds -> wl32 data

	mov	ax,gs:[SourceLineBlkStruc.ssbSegmentID]
	mov	WORD PTR ds:[OFFSET DGROUP:MAPFileBuffer+OFFSET SymbolStruc.ssSymbolSeg],ax

; add up total number of line numbers for segment
	xor	esi,esi
	push	gs			; save gs -> first source line block

wsflineloop:
	add	esi,gs:[SourceLineBlkStruc.ssbCount]
	cmp	gs:[SourceLineBlkStruc.ssbNextContPtr],0
	je	wsfgottot
	mov	gs,gs:[SourceLineBlkStruc.ssbNextContPtr]
	jmp	SHORT wsflineloop

; esi holds total entry count for segment
wsfgottot:
	pop	gs				; restore gs -> first source line block
	mov	eax,esi
	stosd				; save number of entries following file name
	mov	cx,di
	mov	dx,OFFSET DGROUP:MAPFileBuffer	; dx -> write buffer
	sub	cx,dx			; cx == number of chars in SYM entry header
	movzx	ecx,cx
	shl	esi,3			; x8, convert 2 dwords/entry to bytes
	add	esi,ecx			; compute total SYM entry byte count
	mov	DWORD PTR ds:[OFFSET DGROUP:MAPFileBuffer+OFFSET SymbolStruc.ssSymbolNext],esi	; save offset to next symbol

	mov	bx,SYMFileHandle
	call	WriteFile	; write SYM entry header

	add	SYMLastEntryFPos,esi	; update last entry position
	push	gs			; save gs -> first source line block

; write the remainder of the SYM entry
wsfwriteloop:
	mov	cx,WORD PTR gs:[SourceLineBlkStruc.ssbCount] ; known word value
	shl	cx,3			; x8, still known word value
	mov	dx,SOURCELINESYSVARSIZE
	push	gs
	pop	ds				; ds:dx -> entries to write
	call	WriteFile

	cmp	gs:[SourceLineBlkStruc.ssbNextContPtr],0
	je	wsfwritedone
	mov	gs,gs:[SourceLineBlkStruc.ssbNextContPtr]
	jmp	SHORT wsfwriteloop

wsfwritedone:
	pop	gs				; restore gs -> first source line block
	push	es
	pop	ds				; restore ds -> wl32 data

; move to next source line segment, if any
wsfnextseg:
	mov	ax,gs:[SourceLineBlkStruc.ssbNextSegPtr]	; get -> next segment
	or	ax,ax			; see if exists
	jne	wsfsegloop		; yes

wsfnextfile:
	cmp	CurrentLIB,0	; see if processing library
	jne	wsfnextmod		; yes
	inc	CurrentOBJ		; bump count of current object module
	mov	ax,CurrentOBJ	; get new current object module
	cmp	ax,TotalOBJCount	; see if parsed all object modules
	jbe	wsfobjloop
	jmp	NEAR PTR wsflibinit	; now do library modules, if any

wsfnextmod:
	pop	fs				; restore fs -> library dictionary buffer
	pop	cx				; restore count of modules to process
	pop	ebx				; restore module lookup index
	inc	ebx				; move to next module
	dec	cx
	jne	wsfmodloop		; loop through all of them

wsfnextlib:
	inc	CurrentLIB		; bump to next lib
	mov	ax,CurrentLIB
	cmp	ax,TotalLIBCount	; see if next lib exists
	jbe	wsflibloop		; yes

wsfchksym:
	cmp	FirstPubSymBlkPtr,0	; see if first public symbol block pointer null, no public symbols
	je	wsfdone			; yes
;	je	wsfret			; yes

; allocate segment to hold public symbol pointers
	mov	eax,PublicSymbolCount
	add	eax,TotalCommunalCount
	inc	eax				; make allocated array base 1 (don't use [0])
	add	eax,4			; adjust for possible DGROUP symbols
	shl	eax,2			; dword per symbol
	mov	dx,ax
	shr	eax,16
	mov	cx,ax			; cx:dx == bytes to allocate
	call	AllocateBigMemory
	mov	PubNameSymPtrSeg,ax
	mov	gs,ax			; gs -> symbol pointers

	mov	ax,FirstPubSymBlkPtr	; init ax to first publics block
	mov	edx,1			; gs:edx -> symbol pointer block (start 1, not 0)

wsfblkloop:
	or	ax,ax			; see if next segment exists
	je	wsfsort			; no
	mov	fs,ax			; fs -> current printing public block
	mov	bx,PUBSYMSYSVARSIZE	; fs:bx -> first public entry in block
	xor	cx,cx			; init public entry being printed

wsfentloop:
	mov	ax,cx			; get current printing entry number
	cmp	ax,MAXCOUNTPUBSYMBLK	; see if room for more entries in block (might not be all used)
	jae	wsfnextblk		; no
	cmp	ax,fs:[PubSymBlkStruc.psbCount]	; see if end entry in block
	jae	wsfnextblk		; yes, try next block, if any (shouldn't be)
	test	fs:[bx+PubSymRecStruc.pssFlags],LOCALSYMBOLFLAG
	jne	wsfnextent		; don't show locals in public symbol list
	test	fs:[bx+PubSymRecStruc.pssFlags],(ABSOLUTESYMBOLFLAG OR PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	wsfnextent		; unresolved, don't show

; save -> symbol
	mov	gs:[4*edx],bx
	mov	gs:[4*edx+2],fs
	inc	PubNameSymbolCount	; bump count of publics to show
	inc	edx				; move to next pointer slot

wsfnextent:
	add	bx,SIZE PubSymRecStruc	; fs:bx -> next public entry
	inc	cx				; track current printing entry count
	jmp	NEAR PTR wsfentloop

wsfnextblk:
	mov	ax,fs:[PubSymBlkStruc.psbNextPtr]	; ax -> next block
	jmp	NEAR PTR wsfblkloop

wsfsort:
	call	SortMapSymbolName

	mov	ebp,1			; ebp -> symbol pointer, start at 1, not 0

wsfshowloop:
	mov	fs,PubNameSymPtrSeg
	lfs	bx,fs:[4*ebp]
	inc	ebp				; ebp -> next pointer entry, if any
	mov	di,OFFSET DGROUP:MAPFileBuffer	; init di -> write buffer
	test	fs:[bx+PubSymRecStruc.pssFlags],ABSOLUTESYMBOLFLAG	; see if absolute symbol
	je	wsfnotabs	; not absolute

; absolute symbol
	mov	ds:[di+SymbolStruc.ssSymbolType],1	; flag absolute
	mov	ax,fs:[bx+WORD PTR PubSymRecStruc.pssIndSegDefPtr]	; get frame number
	mov	ds:[di+SymbolStruc.ssSymbolSeg],ax	; save frame
	mov	eax,fs:[bx+PubSymRecStruc.pssOffset]	; get public offset
	mov	ds:[di+SymbolStruc.ssSymbolDword],eax	; save symbol offset value
	jmp	SHORT wsf2

wsfnotabs:
	mov	ds:[di+SymbolStruc.ssSymbolType],0	; flag not absolute
	lgs	si,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; gs:si -> individual segdef
	mov	eax,gs:[si+IndSegDefRecStruc.isdrSegOffset]	; get individual segment offset
	lgs	si,gs:[si+IndSegDefRecStruc.isdrMasterPtr]	; gs:si -> master segdef
	add	eax,fs:[bx+PubSymRecStruc.pssOffset]	; add in public offset
	mov	ds:[di+SymbolStruc.ssSymbolDword],eax	; save symbol offset value
	mov	ax,gs:[si+MasterSegDefRecStruc.mssSegmentID]	; get segment identifier
	mov	ds:[di+SymbolStruc.ssSymbolSeg],ax	; save segment identifier for symbol

wsf2:
	add	di,OFFSET SymbolStruc.ssSymbolTLen	; di -> start of name storage
	lds	si,fs:[bx+PubSymRecStruc.pssNamePtr]
	call	NormalDSSISource	; normalize string
	lodsb				; get name length byte
	stosb				; save symbol name text byte
	mov	cl,al
	xor	ch,ch			; cx holds length of name string
	jcxz	wsf3		; no name to print
	push	 cx			; transfer public symbol name
	shr	cx,2
	rep	movsd
	pop	cx
	and	cx,3
	rep	movsb

wsf3:
	push	DGROUP
	pop	ds				; restore ds -> wl32 data

	mov	bx,SYMFileHandle
	mov	cx,di
	mov	dx,OFFSET DGROUP:MAPFileBuffer
	sub	cx,dx			; cx == number of printable chars
	mov	di,dx			; di -> write buffer

	movzx	ecx,cx
;	mov	eax,TotalMAPSYMBufferOffset
;	add	eax,SYMSIGSIZE
;	add	eax,ecx
	mov	ds:[di+SymbolStruc.ssSymbolNext],ecx	; save offset to next symbol

	call	MAPSYMBufferFile	; write to main buffer

	dec	PubNameSymbolCount
	je	wsfdone
	add	SYMLastEntryFPos,ecx	; update last entry position
	jmp	NEAR PTR wsfshowloop

; seek to last entry, write -1 for next symbol offset
wsfdone:
	mov	bx,SYMFileHandle
	call	MAPSYMBufferFlush	; flush main buffer

	mov	dx,WORD PTR SYMLastEntryFPos
	mov	cx,WORD PTR SYMLastEntryFPos+2

	add	dx,SYMSIGSIZE
	adc	cx,0

	mov	ax,4200h		; move file pointer relative start of file
	int	21h

	mov	dx,OFFSET DGROUP:MAPFileBuffer
	mov	di,dx			; di -> write buffer
	mov	DWORD PTR ds:[di],-1	; no next symbol, -1 offset (relative zero offset within symbol entry)
	mov	cx,4			; dword to write
	call	WriteFile

	mov	ax,PubNameSymPtrSeg	; memory for pointers no longer needed, free it
	or	ax,ax
	je	wsfret			; never allocated
	call	ReleaseMemory

wsfret:
	mov	bx,SYMFileHandle
	mov	ah,3eh			; close SYM file
	int	21h
	ret
WriteSYMFile	ENDP

;*****************************
;* INITMAPFILE               *
;*****************************

; open map file and write header

InitMapFile	PROC
	mov	dx,OFFSET DGROUP:MAPFileName
	call	CreateFile
	mov	MAPFileHandle,ax	; save map file handle

	mov	si,OFFSET DGROUP:ProgramText
	mov	cl,[si-1]
	xor	ch,ch
	mov	dx,si
	mov	bx,MAPFileHandle
	call	WriteFile

	mov	dx,bx			; dx holds file handle
	mov	bx,OFFSET DGROUP:EXEFileName
	call	WriteFileVarString

	mov	si,OFFSET DGROUP:DateText
	mov	cl,[si-1]
	xor	ch,ch
	mov	dx,si
	mov	bx,MAPFileHandle
	call	WriteFile

	mov	di,OFFSET DGROUP:MAPFileBuffer
	mov	ah,2ah			; get date
	int	21h
	mov	ax,cx			; ax==year
	xchg	ax,dx		; dx==year, ax==month and day
	mov	bx,ax
	mov	al,bh			; al==month
	aam					; ax==month in BCD
	mov	cx,"00"
	add	ax,cx			; ax==month in decimal
	xchg	al,ah
	stosw				; write month to string
	mov	al,'/'
	stosb
	mov		al,bl		; al==day
	aam					; ax==day in bcd
	add	ax,cx
	xchg	al,ah
	stosw				; write day to string
	mov	al,'/'
	stosb
	mov	ax,dx			; ax==year
	mov	dl,100
	div	dl				; ah==last two digits of year
	mov	al,ah
	aam
	add	ax,cx			; ax==year in decimal
	xchg	al,ah
	stosw				; write year to string
	mov	dx,OFFSET DGROUP:MAPFileBuffer
	mov	cx,8
	mov	bx,MAPFileHandle
	call	WriteFile

	mov	si,OFFSET DGROUP:TimeText
	mov	cl,[si-1]
	xor	ch,ch
	mov	dx,si
	mov	bx,MAPFileHandle
	call	WriteFile

	mov	di,OFFSET DGROUP:MAPFileBuffer
	mov	ah,2ch			; get time
	int	21h
	mov	al,ch			; al==hours
	mov	dl,'p'			; preset for 'pm'
	cmp	al,12			; see if pm
	jae	asctime2		; yes
	mov	dl,'a'			; set for 'am'
asctime2:
	cmp	al,13			; see if >=1pm
	jb	asctime3		; no
	sub	al,12			; convert to 12 hour clock
asctime3:
	or	al,al			; see if 12am
	jne	asctime4		; no
	add	al,12			; 0 hour==12
asctime4:
	aam					; hours to BCD
	add	ax,"00"			; hours to decimal digits
	xchg	al,ah
	stosw
	mov	al,":"
	stosb
	mov	al,cl			; al==minutes
	aam					; minutes to BCD
	add	ax,"00"
	xchg	al,ah
	stosw
	mov	al," "
	stosb
	mov	ah,"m"
	mov	al,dl			; ax== am/pm indicator
	stosw
	mov	dx,OFFSET DGROUP:MAPFileBuffer
	mov	cx,8
	mov	bx,MAPFileHandle
	call	WriteFile

	mov	si,OFFSET DGROUP:MapSegText
	mov	cl,[si-1]
	xor	ch,ch
	mov	dx,si
	mov	bx,MAPFileHandle
	call	WriteFile
	ret
InitMapFile	ENDP

;*****************************
;* MAPSEGMENTS               *
;*****************************

; display segments in map file

MapSegments	PROC
	mov	bx,WORD PTR FirstSegment	; ax:bx -> first segment in program
	mov	ax,WORD PTR FirstSegment+2

; ax:bx -> current printing segment in program
msprintloop:
	or	ax,ax			; see if next segment exists
	je	msret			; no
	mov	fs,ax			; fs:bx -> current printing segment master segdef block

	test	fs:[bx+MasterSegDefRecStruc.mssFlags],(DEBUGSYMBOLSSEGMENTFLAG OR DEBUGTYPESSEGMENTFLAG)
	jne	msnextent		; debug segment

; init the buffer for segment printing
	mov	cx,76/4
	mov	di,OFFSET DGROUP:MAPFileBuffer
	mov	dx,di
	mov	eax,20202020h
	rep	stosd			; pad maximum written amount of buffer with spaces
	mov	di,dx			; di -> write buffer

	stosb				; leading space
	mov	edx,fs:[bx+MasterSegDefRecStruc.mssSegOffset]	; get segment start
	call	DwordToAsciiHex
	mov	ax,' '*256+'H'	; end with 'H' and following space
	stosw

	mov	edx,fs:[bx+MasterSegDefRecStruc.mssSegLength]	; get segment length
	mov	eax,edx
	add	edx,fs:[bx+MasterSegDefRecStruc.mssSegOffset]	; add in segment start
	or	eax,eax			; see if zero length segment, no adjustment to end
	je	msprintend		; yes
	dec	edx				; adjust segment end for relative zero

msprintend:
	call	DwordToAsciiHex	; print segment end
	mov	ax,' '*256+'H'	; end with 'H' and following space
	stosw

	mov	edx,fs:[bx+MasterSegDefRecStruc.mssSegLength]	; get segment length
	call	DwordToAsciiHex	; print it
	mov	ax,' '*256+'H'	; end with 'H' and following space
	stosw

	lds	si,fs:[bx+MasterSegDefRecStruc.mssNamePtr]
	call	NormalDSSISource	; normalize string
	lodsb				; get name length byte
	mov	cl,al
	xor	ch,ch			; cx holds length of name string
	jcxz	ms2			; no name to print
	rep	movsb			; transfer segment name

ms2:
	cmp	di,OFFSET DGROUP:MAPFileBuffer+55	; see if at or past class column
	ja	msclass			; yes
	mov	di,OFFSET DGROUP:MAPFileBuffer+55	; skip ahead to class column (pad segment name with spaces)

msclass:
	lds	si,fs:[bx+MasterSegDefRecStruc.mssClassPtr]
	call	NormalDSSISource	; normalize string
	lodsb				; get name length byte
	mov	cl,al			; cx holds length of name string (ch known zero)
	jcxz	ms3			; no name to print
	rep	movsb			; transfer class name

ms3:
	push	DGROUP
	pop	ds				; restore ds -> wl32 data

	cmp	di,OFFSET DGROUP:MAPFileBuffer+71	; see if at or past number column
	ja	msnumber		; yes
	mov	di,OFFSET DGROUP:MAPFileBuffer+71	; skip ahead to number column (pad class name with spaces)

msnumber:
	mov	dx,SegmentNumber
	call	WordToAsciiHex	; print it
	inc	SegmentNumber

	mov	ax,LF*256+CR	; end with CR/LF
	stosw
	mov	cx,di
	mov	dx,OFFSET DGROUP:MAPFileBuffer
	sub	cx,dx			; cx == number of printable chars
	push	bx			; save bx -> segment entry
	mov	bx,MAPFileHandle
	call	MAPSYMBufferFile
	pop	bx				; restore bx -> segment entry

msnextent:
	mov	ax,WORD PTR fs:[bx+MasterSegDefRecStruc.mssNextSegPtr]+2
	mov	bx,WORD PTR fs:[bx+MasterSegDefRecStruc.mssNextSegPtr]
	jmp	NEAR PTR msprintloop

msret:
	mov	bx,MAPFileHandle
	call	MAPSYMBufferFlush	; flush any pending writes
	ret
MapSegments	ENDP

;*****************************
;* MAPSYMBUFFERFILE          *
;*****************************

; write MAP and SYM line info to buffer
; upon entry ds:dx -> source buffer, cx == bytes to write, bx == file handle
; destroys ax

MAPSYMBufferFile	PROC
	push	cx			; save critical registers
	push	si
	push	di
	push	es

	mov	ax,MAPSYMBufferOffset
	add	ax,cx			; get new offset
	cmp	ax,MAPSYMFILEBUFFERSIZE	; check for overflow
	jb	wfbnoflush		; no overflow

; overflow occurred, must flush main MAP/SYM buffer
	call	MAPSYMBufferFlush

; copy MAP/SYM info to main buffer
wfbnoflush:
	mov	es,MAPSYMBufferSelector
	mov	di,MAPSYMBufferOffset	; es:di -> destination in buffer
	add	MAPSYMBufferOffset,cx	; update offset for subsequent writes
;	movzx	ecx,cx
;	add	TotalMAPSYMBufferOffset,ecx	; update total offset
	mov	si,dx
	mov	ax,cx
	shr	cx,2
	rep	movsd			; move 'em out
	mov	cx,ax
	and	cx,3
	rep	movsb			; pick up remainder bytes

wfbret:
	pop	es			; restore critical registers
	pop	di
	pop	si
	pop	cx
	ret
MAPSYMBufferFile	ENDP

;*****************************
;* MAPSYMBUFFERFLUSH         *
;*****************************

; flush main MAP/SYM buffer to file
; upon entry bx == file handle
; destroys ax

MAPSYMBufferFlush	PROC
	push	cx			; save critical registers
	push	dx
	push	ds
	mov	cx,MAPSYMBufferOffset	; number of bytes to write
	mov	MAPSYMBufferOffset,0	; reset write offset
	xor	dx,dx
	mov	ds,MAPSYMBufferSelector	; ds:dx -> source
	call	WriteFile
	pop	ds				; restore critical registers
	pop	dx
	pop	cx
	ret
MAPSYMBufferFlush	ENDP

;*****************************
;* DWORDTOASCIIHEX           *
;*****************************

; convert dword in edx to ascii hex, place in es:di -> buffer
; updates di
; destroys al,cx,edx

DwordToAsciiHex	PROC
	mov	cx,8			; 8 printable chars per dword

dtaloop:
	rol	edx,4			; get next char value (4 MSB)
	mov	al,dl
	and	al,0fh			; mask off extraneous bits
	cmp	al,0ah			; see if more than single digit decimal
	jb	dta2
	add	al,7			; convert to alpabet hex

dta2:
	add	al,30h			; convert to ASCII
	stosb				; save it to buffer
	loop	dtaloop
	ret
DwordToAsciiHex	ENDP

;*****************************
;* WORDTOASCIIHEX            *
;*****************************

; convert word in dx to ascii hex, place in es:di -> buffer
; updates di
; destroys al,cx,edx

WordToAsciiHex	PROC
	mov	cx,4			; 4 printable chars per word

wtaloop:
	rol	dx,4			; get next char value (4 MSB)
	mov	al,dl
	and	al,0fh			; mask off extraneous bits
	cmp	al,0ah			; see if more than single digit decimal
	jb	wta2
	add	al,7			; convert to alpabet hex

wta2:
	add	al,30h			; convert to ASCII
	stosb				; save it to buffer
	loop	wtaloop
	ret
WordToAsciiHex	ENDP

;*****************************
;* MAPGROUPS                 *
;*****************************

; display groups in map file

MapGroups	PROC
	cmp	FirstGrpDefBlkPtr,0	; see if first group block pointer null, no groups
	je	mgret			; yes
	mov	si,OFFSET DGROUP:OrgGroupText
	mov	cl,[si-1]
	xor	ch,ch
	mov	dx,si
	mov	bx,MAPFileHandle
	call	WriteFile
	mov	ax,FirstGrpDefBlkPtr	; init ax to first grpdef block

mgblkloop:
	or	ax,ax			; see if next segment exists
	je	mgret			; no
	mov	fs,ax			; fs -> current printing grpdef block
	mov	bx,GRPDEFSYSVARSIZE	; fs:bx -> first grpdef entry in block
	mov	PrintMasterEntry,0	; init grpdef entry being printed

mgentloop:
	mov	ax,PrintMasterEntry	; get current printing entry number
	cmp	ax,MAXCOUNTGRPDEFBLK	; see if room for more entries in block (might not be all used)
	jae	mgnextblk		; no
	cmp	ax,fs:[GrpDefBlkStruc.gdbCount]	; see if end entry in block
	jae	mgnextblk		; yes, try next block, if any (shouldn't be)

	mov	di,OFFSET DGROUP:MAPFileBuffer	; init di -> write buffer
	mov	al,' '
	stosb				; leading space
	mov	edx,fs:[bx+GrpDefRecStruc.gdrGrpOffset]	; get group start
	shr	edx,4			; convert to paras
	call	DwordToAsciiHex
	mov	ax,'0'*256+':'	; follow with ':0'
	stosw
	mov	al,' '			; following space
	stosb
	mov	ah,al			; two more spaces
	stosw

	lds	si,fs:[bx+GrpDefRecStruc.gdrGrpNamePtr]
	call	NormalDSSISource	; normalize string
	lodsb				; get name length byte
	mov	cl,al
	xor	ch,ch			; cx holds length of name string
	jcxz	mg2			; no name to print
	rep	movsb			; transfer group name

mg2:
	push	DGROUP
	pop	ds				; restore ds -> wl32 data

	mov	ax,LF*256+CR	; end with CR/LF
	stosw
	mov	cx,di
	mov	dx,OFFSET DGROUP:MAPFileBuffer
	sub	cx,dx			; cx == number of printable chars
	push	bx			; save bx -> group entry
	mov	bx,MAPFileHandle
	call	WriteFile
	pop	bx				; restore bx -> group entry

mgnextent:
	add	bx,SIZE GrpDefRecStruc	; fs:bx -> next grpdef entry
	inc	PrintMasterEntry	; track current printing entry count
	jmp	NEAR PTR mgentloop

mgnextblk:
	mov	ax,fs:[GrpDefBlkStruc.gdbNextPtr]	; ax -> next block
	jmp	NEAR PTR mgblkloop

mgret:
	ret
MapGroups	ENDP

;*****************************
;* MAPPUBLICS                *
;*****************************

; map public symbols

MapPublics	PROC
	mov	PubNameSymbolCount,0
	cmp	FirstPubSymBlkPtr,0	; see if first public symbol block pointer null, no public symbols
	je	mpret			; yes

; allocate segment to hold public symbol pointers
	mov	eax,PublicSymbolCount
	add	eax,TotalCommunalCount
	inc	eax				; make allocated array base 1 (don't use [0])
	add	eax,4			; adjust for possible DGROUP symbols
	shl	eax,2			; dword per symbol
	mov	dx,ax
	shr	eax,16
	mov	cx,ax			; cx:dx == bytes to allocate
	call	AllocateBigMemory
	mov	PubNameSymPtrSeg,ax
	mov	gs,ax			; gs -> symbol pointers

	mov	si,OFFSET DGROUP:PubNameText
	mov	cl,[si-1]
	xor	ch,ch
	mov	dx,si
	mov	bx,MAPFileHandle
	call	WriteFile
	mov	ax,FirstPubSymBlkPtr	; init ax to first publics block
	mov	edx,1			; gs:edx -> symbol pointer block (start 1, not 0)

mpblkloop:
	or	ax,ax			; see if next segment exists
	je	mpsort			; no
	mov	fs,ax			; fs -> current printing public block
	mov	bx,PUBSYMSYSVARSIZE	; fs:bx -> first public entry in block
	xor	cx,cx			; init public entry being printed

mpentloop:
	mov	ax,cx			; get current printing entry number
	cmp	ax,MAXCOUNTPUBSYMBLK	; see if room for more entries in block (might not be all used)
	jae	mpnextblk		; no
	cmp	ax,fs:[PubSymBlkStruc.psbCount]	; see if end entry in block
	jae	mpnextblk		; yes, try next block, if any (shouldn't be)
	test	fs:[bx+PubSymRecStruc.pssFlags],(LOCALSYMBOLFLAG OR ABSOLUTESYMBOLFLAG OR WEAKEXTERNFLAG OR LAZYEXTERNFLAG)
	jne	mpnextent		; don't show locals, absolutes, or weak/lazy externals in public symbol list

; save -> symbol
	mov	gs:[4*edx],bx
	mov	gs:[4*edx+2],fs
	inc	PubNameSymbolCount	; bump count of publics to show
	inc	edx				; move to next pointer slot

mpnextent:
	add	bx,SIZE PubSymRecStruc	; fs:bx -> next grpdef entry
	inc	cx				; track current printing entry count
	jmp	NEAR PTR mpentloop

mpnextblk:
	mov	ax,fs:[PubSymBlkStruc.psbNextPtr]	; ax -> next block
	jmp	NEAR PTR mpblkloop

mpsort:
	call	SortMapSymbolName
	call	WriteMapSymbol	; write the name sorted map symbols

	mov	si,OFFSET DGROUP:AddressText
	mov	cl,[si-1]
	xor	ch,ch
	mov	dx,si
	mov	bx,MAPFileHandle
	call	WriteFile
	call	SortMapSymbolAddress
	call	WriteMapSymbol	; write the address sorted map symbols

	mov	ax,PubNameSymPtrSeg	; memory for pointers no longer needed, free it
	call	ReleaseMemory

mpret:
	ret
MapPublics	ENDP

;*****************************
;* SORTMAPSYMBOLNAME         *
;*****************************

; sort map symbols by name
; save ds,es
; follows this C code, with appropriate modifications:
COMMENT !
shellsort(int a[],int N)
{
	int i,j,h,v;

	for(h=1;h<=N/9;h=3*h+1)
		;
	for(;h>0;h/=3){
		for(i=h+1;i<=N;i++){
			v=a[i];
			j=i;
			while(j>h && a[j-h]>v){
				a[j]=a[j-h];
				j-=h;
			}
			a[j]=v;
		}
	}
}
END COMMENT !

SortMapSymbolName	PROC
	push	es			; save critical register
	mov	eax,PubNameSymbolCount
	cmp	eax,1
	jbe	smsret			; sorted by definition

	mov	fs,PubNameSymPtrSeg
	push	ds
	pop	gs			; gs -> DGROUP

; sort the symbol names, fs -> pointer buffer
	xor	ebx,ebx
	mov	edx,ebx
	mov	bl,9
	div	ebx
	mov	ecx,eax			; ecx == quotient, N/9

; for (h=1;h<=N/9;h=3*h+1);
	mov	eax,ebx			; eax==9
	mov	al,1			; eax==1, h
	mov	bl,3			; ebx==3
	xor	edx,edx			; zero for multiply loop

sethloop:
	cmp	eax,ecx			; h<=N/9
	ja	sort2
	mul	ebx				; 3*h, assume 32-bit result (pretty safe bet)
	inc	eax				; 3*h+1
	jmp	SHORT sethloop

; ebx will play role of j, edx will play role of h
sort2:
	mov	edx,eax			; edx == h

; for (;h>0;...
hloop:
	or	edx,edx			; h>0
	je	sortend

; for(i=h+1...
	mov	eax,edx
	inc	eax
	mov	gs:iValue,eax

; for(...;i<=N;...){
iloop:
	mov	eax,gs:iValue
	cmp	eax,gs:PubNameSymbolCount
	ja	nexth

	mov	ecx,fs:[4*eax]
	mov	gs:vValue,ecx	; v=a[i]
	mov	ebx,eax			; j=i

; while(j>h && a[j-h]>v){
whileloop:
	cmp	ebx,edx			; j>h
	jbe	whilefail

	mov	eax,ebx
	sub	eax,edx			; eax==j-h
	xor	ecx,ecx			; zero high bytes of register for following repe

	lds	si,fs:[4*eax]	; ds:si -> a[j-h]
	lds	si,ds:[si+PubSymRecStruc.pssNamePtr]	; ds:si -> public name
	les	di,gs:vValue	; es:di==v
	les	di,es:[di+PubSymRecStruc.pssNamePtr]	; es:di -> public name

; a[j-h] > v
	cmp	si,SIZEIOBUFFBLK
	jb	firstlen

;need to normalize name
	call	NormalDSSISource
	mov	cl,ds:[si]		; get length of first name
	jmp	SHORT sort3

firstlen:
	mov	cl,ds:[si]		; get length of first name
	mov	ax,si
	add	ax,cx
	cmp	ax,SIZEIOBUFFBLK
	jae	smsdssi			; need to normalize name

sort3:
	cmp	di,SIZEIOBUFFBLK
	jb	secondlen

; need to normalize name
	call	NormalESDIDest
	mov	al,es:[di]	; get length of second name
	mov	ah,ch		; zero ah
	mov	ch,al		; temporary storage of al, length of second name
	jmp	SHORT sort4

secondlen:
	mov	al,es:[di]	; get length of second name
	mov	ah,ch		; zero ah
	mov	ch,al		; temporary storage of al, length of second name
	add	ax,di
	cmp	ax,SIZEIOBUFFBLK
	jae	smsesdi			; need to normalize name

sort4:
	mov	al,ch		; length of second name to al
	xor	ch,ch		; restore ch to zero
	mov	ah,cl		; length of first name to ah
	cmpsb			; compare name lengths
	je	namechk		; same
	cmp	al,ah		; see if second name shorter than first
	ja	namechk		; no, (equals does swap, faster than branch)
	mov	cl,al		; update compare length to shortest name

namechk:
	repe	cmpsb	; check name
	jb	whilefail	; first < second, a[j-h]<v
	jne	dochange	; second > first, modify a[j]

; matched through length of name
	cmp	al,ah		; see if second name shorter than first
	jae	whilefail	; no, a[j-h]<=v

dochange:
	mov	eax,ebx
	sub	eax,edx			; eax==j-h
	mov	eax,fs:[4*eax]	; eax==a[j-h]
	mov	fs:[4*ebx],eax	; a[j]=a[j-h]
	sub	ebx,edx			; j-=h
	jmp	NEAR PTR whileloop

whilefail:
	mov	eax,gs:vValue
	mov	fs:[4*ebx],eax	; a[j]=v

; for(...;i++){
	inc	gs:iValue
	jmp	NEAR PTR iloop

; for (...;h/=3){
nexth:
	mov	eax,edx
	xor	edx,edx
	mov	ecx,edx
	mov	cl,3
	div	ecx
	mov	edx,eax
	jmp	NEAR PTR hloop

sortend:
	mov	ax,DGROUP
	mov	ds,ax

smsret:
	pop	es				; restore critical register
	ret

smsdssi:
	call	NormalDSSISource
	jmp	NEAR PTR sort3

smsesdi:
	call	NormalESDIDest
	jmp	NEAR PTR sort4

SortMapSymbolName	ENDP

;*****************************
;* SORTMAPSYMBOLADDRESS      *
;*****************************

; sort map symbols by name
; save ds,es
; follows this C code, with appropriate modifications:
COMMENT !
shellsort(int a[],int N)
{
	int i,j,h,v;

	for(h=1;h<=N/9;h=3*h+1)
		;
	for(;h>0;h/=3){
		for(i=h+1;i<=N;i++){
			v=a[i];
			j=i;
			while(j>h && a[j-h]>v){
				a[j]=a[j-h];
				j-=h;
			}
			a[j]=v;
		}
	}
}
END COMMENT !

SortMapSymbolAddress	PROC
	push	es			; save critical register
	mov	eax,PubNameSymbolCount
	cmp	eax,1
	jbe	smaret			; sorted by definition

	mov	fs,PubNameSymPtrSeg
	push	ds
	pop	gs			; gs -> DGROUP

; sort the symbol names, fs -> pointer buffer
	xor	ebx,ebx
	mov	edx,ebx
	mov	bl,9
	div	ebx
	mov	ecx,eax			; ecx == quotient, N/9

; for (h=1;h<=N/9;h=3*h+1);
	mov	eax,ebx			; eax==9
	mov	al,1			; eax==1, h
	mov	bl,3			; ebx==3
	xor	edx,edx			; zero for multiply loop

smasethloop:
	cmp	eax,ecx			; h<=N/9
	ja	smasort2
	mul	ebx				; 3*h, assume 32-bit result (pretty safe bet)
	inc	eax				; 3*h+1
	jmp	SHORT smasethloop

; ebx will play role of j, edx will play role of h
smasort2:
	mov	edx,eax			; edx == h

; for (;h>0;...
smahloop:
	or	edx,edx			; h>0
	je	smasortend

; for(i=h+1...
	mov	eax,edx
	inc	eax
	mov	gs:iValue,eax

; for(...;i<=N;...){
smailoop:
	mov	eax,gs:iValue
	cmp	eax,gs:PubNameSymbolCount
	ja	smanexth

	mov	ecx,fs:[4*eax]
	mov	gs:vValue,ecx	; v=a[i]
	mov	ebx,eax			; j=i

; while(j>h && a[j-h]>v){
smawhileloop:
	cmp	ebx,edx			; j>h
	jbe	smawhilefail

	mov	eax,ebx
	sub	eax,edx			; eax==j-h

	lds	si,fs:[4*eax]	; ds:si -> a[j-h]
	les	di,gs:vValue	; es:di -> v

; a[j-h] > v
	xor	eax,eax			; zero out public offset of first element
	test	ds:[si+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	sma2			; unresolved, zero offset
	mov	eax,ds:[si+PubSymRecStruc.pssOffset]	; get in public offset
	lds	si,ds:[si+PubSymRecStruc.pssIndSegDefPtr]	; ds:si -> individual segdef
	add	eax,ds:[si+IndSegDefRecStruc.isdrSegOffset]	; add in individual segment offset
	lds	si,ds:[si+IndSegDefRecStruc.isdrMasterPtr]	; ds:si -> master segdef
	add	eax,ds:[si+MasterSegDefRecStruc.mssSegOffset]	; add offset from start of program

sma2:
	xor	ecx,ecx			; zero out public offset of second element
	test	es:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	sma3			; unresolved, zero offset
	mov	ecx,es:[di+PubSymRecStruc.pssOffset]	; get in public offset
	les	di,es:[di+PubSymRecStruc.pssIndSegDefPtr]	; es:di -> individual segdef
	add	ecx,es:[di+IndSegDefRecStruc.isdrSegOffset]	; add in individual segment offset
	les	di,es:[di+IndSegDefRecStruc.isdrMasterPtr]	; es:di -> master segdef
	add	ecx,es:[di+MasterSegDefRecStruc.mssSegOffset]	; add offset from start of program

sma3:
	cmp	eax,ecx			; compare public symbol absolute offsets
	jb	smawhilefail	; first < second, a[j-h]<v

; change the elements
	mov	eax,ebx
	sub	eax,edx			; eax==j-h
	mov	eax,fs:[4*eax]	; eax==a[j-h]
	mov	fs:[4*ebx],eax	; a[j]=a[j-h]
	sub	ebx,edx			; j-=h
	jmp	SHORT smawhileloop

smawhilefail:
	mov	eax,gs:vValue
	mov	fs:[4*ebx],eax	; a[j]=v

; for(...;i++){
	inc	gs:iValue
	jmp	NEAR PTR smailoop

; for (...;h/=3){
smanexth:
	mov	eax,edx
	xor	edx,edx
	mov	ecx,edx
	mov	cl,3
	div	ecx
	mov	edx,eax
	jmp	NEAR PTR smahloop

smasortend:
	mov	ax,DGROUP
	mov	ds,ax

smaret:
	pop	es				; restore critical register
	ret
SortMapSymbolAddress	ENDP

;*****************************
;* WRITEMAPSYMBOL            *
;*****************************

; write map symbol

WriteMAPSymbol	PROC
	push	PubNameSymbolCount	; save critical variable
	mov	ebp,1			; ebp -> symbol pointer, start at 1, not 0

wmsshowloop:
	mov	fs,PubNameSymPtrSeg
	lfs	bx,fs:[4*ebp]
	inc	ebp				; ebp -> next pointer entry, if any
	mov	di,OFFSET DGROUP:MAPFileBuffer	; init di -> write buffer
	mov	al,' '
	stosb				; leading space
	xor	edx,edx			; zero out public offset
	test	fs:[bx+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	wmsseg			; unresolved, zero offset
	lgs	si,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; gs:si -> individual segdef
	lgs	si,gs:[si+IndSegDefRecStruc.isdrMasterPtr]	; gs:si -> master segdef
	mov	edx,gs:[si+MasterSegDefRecStruc.mssSegOffset]	; get offset from start of program
	shr	edx,4			; convert to paras

wmsseg:
	call	DwordToAsciiHex
	mov	al,':'
	stosb
	xor	edx,edx
	test	fs:[bx+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	wmsoff			; unresolved, zero offset
	mov	dl,BYTE PTR gs:[si+MasterSegDefRecStruc.mssSegOffset]	; get low byte offset from start of program
	and	dl,0fh			; get master segment offset from para align (should always be zero in 3P mode)
	lgs	si,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; gs:si -> individual segdef
	add	edx,gs:[si+IndSegDefRecStruc.isdrSegOffset]	; add in individual segment offset
	add	edx,fs:[bx+PubSymRecStruc.pssOffset]	; add in public offset

wmsoff:
	call	DwordToAsciiHex
	mov	eax,'    '
	stosd				; create seven blank spaces
	stosw
	stosb

	xor	ch,ch			; cx holds length of name string
	lds	si,fs:[bx+PubSymRecStruc.pssNamePtr]
	cmp	si,SIZEIOBUFFBLK
	jb	wmsgetlen		; need not normalize name

	call	NormalDSSISource	; normalize string
	mov	cl,ds:[si]		; get length byte
	jmp	SHORT wmstrans

wmsgetlen:
	mov	cl,ds:[si]		; get length byte
	jcxz	wms2			; no name to print
	mov	ax,si
	add	ax,cx
	cmp	ax,SIZEIOBUFFBLK
	jb	wmstrans			; need not normalize name
	call	NormalDSSISource	; normalize string

wmstrans:
	inc	si				; si -> past length byte
	rep	movsb			; transfer public symbol name

wms2:
	push	DGROUP
	pop	ds				; restore ds -> wl32 data

	mov	ax,LF*256+CR	; end with CR/LF
	stosw
	mov	cx,di
	mov	dx,OFFSET DGROUP:MAPFileBuffer
	sub	cx,dx			; cx == number of printable chars
	mov	bx,MAPFileHandle
	call	WriteFile
	dec	PubNameSymbolCount	; drop count of publics to show
	jne	wmsshowloop		; more to show

	pop	PubNameSymbolCount	; restore critical variable
	ret
WriteMAPSymbol	ENDP

;*****************************
;* MAPENTRY                  *
;*****************************

; print program entry point in map file

MapEntry	PROC
	mov	bx,OFFSET DGROUP:EntryText
	mov	cl,[bx-1]
	xor	ch,ch
	mov	dx,bx
	mov	bx,MAPFileHandle
	call	WriteFile
	mov	di,OFFSET DGROUP:MAPFileBuffer	; init di -> write buffer
	mov	edx,EntrySegmentValue
	call	DwordToAsciiHex
	mov	al,':'			; following colon
	stosb
	mov	edx,EntryOffsetValue
	call	DwordToAsciiHex
	mov	ax,LF*256+CR	; end with CR/LF
	stosw
	mov	cx,di
	mov	dx,OFFSET DGROUP:MAPFileBuffer
	sub	cx,dx			; cx == number of printable chars
	mov	bx,MAPFileHandle
	call	WriteFile
	ret
MapEntry	ENDP

ENDS

END
