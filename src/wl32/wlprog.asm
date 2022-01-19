;*********************************************************************
;*   WLPROG.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          10/01/99                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   build program into protected mode or EXE file                   *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLPROG
PAGE    50,80

.MODEL  SMALL
.386P					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLDATA.INC

;*****************************
;* Public declarations       *
;*****************************

; procedures
PUBLIC	CreateProgramFile

; public for debugger
PUBLIC	WriteEXEBody,Write3PRelEntries

IFDEF DLLSUPPORT
PUBLIC	WriteExportEntries
PUBLIC	WriteImportEntries
ENDIF

; variables
PUBLIC	DebugSegmentCount
PUBLIC	ExecutableFileHandle
PUBLIC	ExecutableHeaderSize

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

BlockCount	DW	?		; LIDATA block count
;DataOffset	DW	?		; LIDATA data offset
DataOffset32	DD	?	; LIDATA32 data offset
DataSegmentIndex	DW	?	; LEDATA/LIDATA segment index
ExecutableFileHandle	DW	?	; executable file handle, EXE or P3
ExecutableHeaderSize	DD	?	; size of executable file header
FilePositionBookmark	DD	?	; file position bookmark for building import info
PreviousFilePosition	DD	?	; file position previous to export/import info
PublicNameIndex	DW	?	; current COMDAT public name index
RepeatCount	DW	?		; LIDATA repeat count
RepeatCount32	DD	?	; LIDATA32 repeat count
WL32FileHandle	DW	?	; file handle for reading WL32 file (for DOS extender)

; variables used in shell sort
iValue	DD	?
vValue	DD	?

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

CreatingEXETextLen	DB	CreatingEXETextStop-CreatingEXEText
CreatingEXEText	DB	CR,LF,'*** Creating executable file'
CreatingEXETextStop		=	$

_CODEText	DB	"_"		; uses next four bytes of "CODE" after "_"
CODEText	DB	"CODE"
MISCText	DB	"MISC"
CLARIONText	DB	"CLARION"
STACKText	DB	"STACK"
COText	=	$			; use first two bytes of "CONST"
CONSTText	DB	"CONST"
MSGText		DB	"MSG"
DATAText	DB	"DATA"
_CLA_MAINText	DB	"_CLA_MAIN"
_BIN_SEGText	DB	"_BIN_SEG"
NEARText	DB	4,'NEAR'	; flat segment class

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

EXEHeaderStruc	STRUC
	ehSig1	DB	?		; EXE file signature bytes
	ehSig2	DB	?
	ehFileLenMod512	DW	?	; length of file modulo 512
	ehFileSizePages	DW	?	; size of file in 512-byte pages, including header
	ehNumRelEntries	DW	?	; number of relocation entries
	ehHeaderSize	DW	?	; size of header in paragraphs
	ehMinAlloc	DW	?	; minimum number of paragraphs needed above program image
	ehMaxAlloc	DW	?	; minimum number of paragraphs needed above program image
	ehSS	DW	?		; SS at entry
	ehSP	DW	?		; SP at entry
	ehChecksum	DW	?	; word checksum (unused)
	ehIP	DW	?		; contents of IP register at entry
	ehCS	DW	?		; contents of CS at entry
	ehRelOffset	DW	?	; offset of first relocation item in file
	ehOvlNumber	DB	?	; overlay number (unused)
	ehFiller	DB	3 DUP (?)	; unused
EXEHeaderStruc	ENDS

ExeHeader	EXEHeaderStruc	<'M','Z',0,1,0,32,0,0ffffh,0,0,0,0,0,1eh,0>

NewHeaderStruc	STRUC
	NewID		db	'3'	; signature bytes
				DB	'P'
;@@@	NewHeaderSize	dd	0	; byte size of header data.
	NewSize	dd	0	; byte size of header data+exe image data.
	NewLength	dd 0	; byte size of exe image data.
	NewAlloc	dd 0	; byte size of program memory needed.
	NewSegments	dw 0	; number of segment definitions.
	NewRelocs	dd 0	; number of relocation table entries.
	NewEntryEIP	dd 0	; entry offset.
	NewEntryCS	dw 0	; segment list entry number for entry CS.
	NewEntryESP	dd 0	; ESP offset.
	NewEntrySS	dw 0	; segment list entry number for SS.
	NewEntryFlags	dd 0	; Control flags
							; bit 0 & 15 set if 16-bit, reset if 32-bit
	NewAutoStack	dd	0	; auto stack size
	NewAutoDS	dw	0	; auto ds segment number+1
	NewExports	dd	0	; length of EXPORT section
	NewImports	dd	0	; length of IMPORT section
	NewImportModCnt	dd	0	; number of IMPORT modules
	NewEntryReserved	db	64-NewEntryReserved dup (0)
NewHeaderStruc	ENDS

NewHeader	NewHeaderStruc	<'3','P',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>

FlatSegmentInfoStruc	STRUC
	fsisDataOffset	DD	0	; segment start offset
	fsisDataLenType	DD	0	; segment length+type
	fsisCodeOffset	DD	0	; segment start offset
	fsisCodeLenType	DD	0	; segment length+type
FlatSegmentInfoStruc	ENDS

FlatSegmentInfo	FlatSegmentInfoStruc	<0,0,0,0>

DebugSegmentCount	DW	0	; count of debug segments
DOSExtenderSize	DD	0	; size of DOS extender
FlatSegmentFlag	DB	0	; nonzero if flag segment

IFDEF CLIPPER
DGROUPSegLenAdjust	DD	0	; dgroup segment adjustment length for image size adjustment
ENDIF

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	CurrentFileName:BYTE
EXTRN	DGROUPPtr:DWORD
EXTRN	_ENDSegDefPtr:DWORD
EXTRN	EntryOffsetValue:DWORD
EXTRN	EntrySegmentID:WORD
EXTRN	EntrySegmentValue:WORD
EXTRN	FirstGrpDefBlkPtr:WORD
EXTRN	FirstRelocEntryBlkPtr:WORD
EXTRN	FirstSegment:DWORD
EXTRN	HighestOffsetWritten:DWORD
EXTRN	IsAny32BitSegFlag:BYTE
EXTRN	OS2ModuleFlag:BYTE
EXTRN	PharLapModuleFlag:BYTE
EXTRN	ProgramImageSize:DWORD
EXTRN	RelocEntryCount:DWORD
EXTRN	SearchExistSymFlag:BYTE
EXTRN	TotalRelSegCount:WORD
EXTRN	WorkingBuffer:BYTE

IFDEF DLLSUPPORT
EXTRN	FirstEXPDEFBlkPtr:WORD
EXTRN	FirstIMPDEFBlkPtr:WORD
EXTRN	IMPDEFFixupSel:WORD
EXTRN	LastIMPDEFModule:DWORD
EXTRN	TotalIMPDEFFixupCount:DWORD
EXTRN	TotalIMPDEFFixupSize:DWORD
EXTRN	TotalIMPDEFFuncCount:DWORD
ENDIF

IFDEF SYMBOLPACK
EXTRN	ClipperSymSegIndex:WORD
EXTRN	KnownClipperMod:BYTE
EXTRN	NewLEDATAOffset:WORD
ENDIF

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	AllocateBigMemory:PROC
EXTRN	CaselessStrCmp:PROC
EXTRN	CreateFile:PROC
EXTRN	DisplayLinkInfo:PROC
EXTRN	DisplaySegmentName:PROC
EXTRN	FlushIOBuffer:PROC
EXTRN	GetPubSymEntry:PROC
EXTRN	InternalErrorExit:PROC
EXTRN	NormalESDIDest:PROC,NormalGSBXSource:PROC
EXTRN	OpenFile:PROC
EXTRN	ReadByte:PROC,ReadByteDecCX:PROC
EXTRN	ReadFile:PROC
EXTRN	ReadWordCX:PROC,ReadIndexDecCX:PROC,ReadWordDecCX:PROC
EXTRN	ReadDwordDecCX:PROC
EXTRN	ReleaseMemory:PROC
EXTRN	ScanAhead:PROC
EXTRN	SeekToEndOfFile:PROC
EXTRN	WriteFile:PROC
EXTRN	WriteToIOBuffer:PROC
EXTRN	Zero64KIOBlock:PROC

IFDEF SYMBOLPACK
EXTRN	FixupClipperTokens:PROC
EXTRN	ProcessSymbolTable:PROC
ENDIF

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* CREATEPROGRAMFILE         *
;*****************************

; create the program

CreateProgramFile	PROC
	call	OpenExecutableFile	; open executable file
	mov	bx,OFFSET DGROUP:CreatingEXEText
	call	DisplayLinkInfo
	cmp	IsCreateEXEOption,OFF	; see if building EXE file
	je	cpfprot			; no, creating protected mode file

	call	WriteEXEHeader	; write the EXE header, dummy placeholders
	call	WriteEXERelEntries	; write EXE relocation entries
	call	PadEXEHeader	; pad EXE header to proper amount

	mov	eax,RelocEntryCount
	shl	eax,2			; dword per relocation entry (x4)
	mov	dx,EXEHeader.ehRelOffset	; get header system bytes, not relocation entries
	movzx	edx,dx
	add	eax,edx			; add system bytes to relocation entry bytes for total used header size
	add	eax,511			; round up to next 512-byte boundary
	and	ax,0fe00h		; mask size to boundary
	mov	ExecutableHeaderSize,eax	; save executable file header size
	call	WriteEXEBody	; write the EXE body, program image
	call	SetupEXEHeader	; setup EXE header
	call	WriteEXEHeader	; write true EXE header values
	ret

; creating 3P protected mode EXE file
cpfprot:
	call	WriteDOSExtender	; write DOS extender from start of WL32.EXE
	call	Write3PHeader	; write the 3P header, dummy placeholders

; check if 4G flat model
; 4G flat if 1 segment, class "near", 32-bit
	cmp	TotalRelSegCount,1	; see if only one segment
	jne	cpfprot2		; no
	cmp	IsAny32BitSegFlag,OFF	; see if any 32-bit segments (one by definition)
	je	cpfprot2		; no
	lgs	bx,FirstSegment	; gs:bx -> segment master entry
	mov	si,OFFSET DGROUP:NEARText
	les	di,gs:[bx+MasterSegDefRecStruc.mssClassPtr]	; es:di -> class name
	call	NormalESDIDest	; normalize the class name
	cmp	BYTE PTR es:[di],4	; see if length matches
	jne	cpfprot2		; no
	inc	di				; bump past length
	call	CaselessStrCmp
	jc	cpfprot2		; no match on class name

; 4G flat model
; write two segments:
; seg 0 class data, base 0, limit -1
; seg 1 class code, base 0, limit segment length rounded up to dword
; set FlatSegmentFlag=ON (Segment ID=0, Segment length=FlatStackSegLen)
	mov	FlatSegmentFlag,ON
	mov	NewHeader.NewEntryCS,1
	mov	NewHeader.NewEntrySS,0	; save SS value
	inc	TotalRelSegCount	; bump segment count to 2

	mov	ecx,0fffffffch	; maxium segment length
	mov	eax,gs:[bx+MasterSegDefRecStruc.mssSegLength]
	add	eax,3			; round up
	jnc	cpfflat2		; no overflow
	mov	eax,ecx			; set to highest segment length

cpfflat2:
	and	al,0fch			; round to dword
	mov	FlatSegmentInfo.fsisCodeLenType,eax

	cmp	IsStackOption,OFF	; see if stack option
	jne	cpfstack		; yes
	add	eax,1024		; make stack size default 1K
	jnc	cpfflat3
	mov	eax,ecx			; make stack highest segment length
	jmp	cpfflat3

cpfstack:
	mov	edx,StackValue	; user set stack value is word sized
	add	eax,edx
	jnc	cpfflat3
	mov	eax,ecx			; make stack highest segment length

cpfflat3:
	mov	NewHeader.NewEntryESP,eax	; save ESP value
	mov	ProgramImageSize,eax	; update program image size

	mov	eax,FlatSegmentInfo.fsisCodeLenType
	cmp	eax,100000h		; see if >=1M (need 4K byte granularity)
	jb	cpfflat4		; no
	shl	eax,0ch			; convert to 4K pages
	or	eax,GRANULARITY4K	; set 4K granularity bit

cpfflat4:
	or	eax,CODESEGMENTTYPE	; flag as code segment

IFDEF WATCOM_ASM
	cmp	IsFlatOption,0
	je	cpfflat5		; NEAR, but not flat
	or	eax,TRUEFLATSEGMENTFLAG	; flag as true flat segment (not near)

cpfflat5:
ENDIF

	mov	FlatSegmentInfo.fsisCodeLenType,eax

	mov	eax,0fffffh	; segment length is maximum
	or	eax,GRANULARITY4K OR DATASEGMENTTYPE	; merge in 4K granularity and segment type

IFDEF WATCOM_ASM
	cmp	IsFlatOption,0
	je	cpfflat6		; NEAR, but not flat
	or	eax,TRUEFLATSEGMENTFLAG	; flag as true flat segment (not near)

cpfflat6:
ENDIF

	mov	FlatSegmentInfo.fsisDataLenType,eax

	mov	bx,ExecutableFileHandle
	mov	dx,OFFSET DGROUP:FlatSegmentInfo
	mov	cx,16			; two entries
	call	WriteFile	; write the segment entries for flat model
	jmp	cpfprotrel	; bypass nonflat code routines

cpfprot2:
IFNDEF CLIPPER
	cmp	IsAny32BitSegFlag,OFF	; see if any 32-bit segments
	je	cpfseggrp		; no

; at least one 32-bit segment, stack segment must be 32-bit as well
	cmp	WORD PTR _ENDSegDefPtr+2,0	; see if stack segment in program
	je	cpfseggrp			; no

	lfs	bx,_ENDSegDefPtr	; fs:bx -> master segdef of stack segment
	or	WORD PTR fs:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG	; flag 32-bit segment
ENDIF

cpfseggrp:
	call	SetSegGrpLength	; set first segment of groups with group length
	call	Write3PSegEntries	; write segment entries

cpfprotrel:
	call	Write3PRelEntries	; write relocation entries

IFDEF DLLSUPPORT
	call	WriteExportEntries	; write export entry info
	call	WriteImportEntries	; write import entry info
ENDIF

;	mov	eax,RelocEntryCount
;	shl	eax,2			; dword per relocation entry (x4)
;	add	eax,SIZE NewHeaderStruc	; add in sysvar header size
;	mov	dx,TotalRelSegCount	; get count of segments
;	movzx	edx,dx
;	shl	edx,3			; 8 bytes (2 dwords) per segment
;	add	eax,edx
;	add	eax,DOSExtenderSize	; adjust for prepended DOS extender
	call	SeekToEndOfFile	; get header size
	push	dx
	push	ax
	pop	eax

	mov	ExecutableHeaderSize,eax	; save executable file header size
	call	Zero64KIOBlock	; zero out the 64K i/o block
	call	WriteEXEBody	; write the 3P body (same as EXE), program image
	call	Setup3PHeader	; setup 3P header
	call	Write3PHeader	; write true 3P header values
	ret
CreateProgramFile	ENDP

;*****************************
;* SETSEGGRPLENGTH           *
;*****************************

; set first segment of groups with group length

SetSegGrpLength	PROC
	mov	ax,FirstGrpDefBlkPtr
	or	ax,ax			; see if first group block pointer null, no groups
	je	ssgret			; yes

ssgblkloop:
	or	ax,ax			; see if next segment exists
	je	ssgret			; no
	mov	fs,ax			; fs -> current grpdef block
	mov	bx,GRPDEFSYSVARSIZE	; fs:bx -> first grpdef entry in block
	xor	dx,dx			; init grpdef entry being checked

ssgentloop:
	mov	ax,dx			; get current grpdef entry
	cmp	ax,MAXCOUNTGRPDEFBLK	; see if room for more entries in block (might not be all used)
	jae	ssgnextblk		; no
	cmp	ax,fs:[GrpDefBlkStruc.gdbCount]	; see if end entry in block
	jae	ssgnextblk		; yes, try next block, if any (shouldn't be)

; adjust first segment of group to have same length as group
	test	fs:[bx+GrpDefRecStruc.gdrGrpFlags],GRPOFFSETSETFLAG
	je	ssgnextent		; no segment associated with group
	lgs	si,fs:[bx+GrpDefRecStruc.gdrFirstSegPtr]	; gs:si -> first segment of group
	mov	eax,fs:[bx+GrpDefRecStruc.gdrGrpLen]	; get group length

; if using Clipper, set DGROUP to 64K
IFDEF CLIPPER
	mov	cx,fs			; fs:bx -> group entry
	shl	ecx,16
	mov	cx,bx			; ecx -> group entry
	cmp	ecx,DGROUPPtr	; see if DGROUP
	jne	ssglen			; no
	mov	eax,gs:[si+MasterSegDefRecStruc.mssSegLength]	; get old segment length
	mov	ecx,10000h		; 64K DGROUP
	sub	ecx,eax			; ecx holds expansion amount of DGROUP
	mov	DGROUPSegLenAdjust,ecx	; save expansion amount of DGROUP so can adjust memory image
	mov	eax,10000h		; 64K DGROUP
ENDIF

ssglen:
	mov	gs:[si+MasterSegDefRecStruc.mssSegLength],eax	; update segment length

ssgnextent:
	add	bx,SIZE GrpDefRecStruc	; fs:bx -> next grpdef entry
	inc	dx				; bump current grpdef entry
	jmp	ssgentloop

ssgnextblk:
	mov	ax,fs:[GrpDefBlkStruc.gdbNextPtr]	; ax -> next block
	jmp	ssgblkloop

ssgret:
	ret
SetSegGrpLength	ENDP

;*****************************
;* WRITE3PSEGENTRIES         *
;*****************************

; write 3P segment entries (2 dwords per segment)
;  don't write debug segments

Write3PSegEntries	PROC
	mov	bx,WORD PTR FirstSegment	; ax:bx -> first segment in program
	mov	ax,WORD PTR FirstSegment+2
	xor	di,di
	mov	es,IOBlockSeg	; es:di -> i/o buffer block

; ax:bx -> current segment in program
w3pssegloop:
	or	ax,ax			; see if next segment exists
	je	w3psdone		; no
	mov	fs,ax			; fs:bx -> current segment master segdef


; check if debug segment, if so, then increment debug segment count
;  and don't save
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],(DEBUGSYMBOLSSEGMENTFLAG OR DEBUGTYPESSEGMENTFLAG)
	jne	w3psnextent		; debug segment

w3pssave:
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegOffset]
	stosd				; save segment offset for segment entry
	call	Get3PSegmentType	; get type of segment (code/data/stack/const)

; if Clarion program set all global data segments to size
; ProgramImageSize-segment start or 64K if >64K
IFDEF CLARION
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],CLARIONGLOBALDATAFLAG
	je	wp3sgran		; not a global data segment
	push	eax			; save critical register
	mov	eax,ProgramImageSize	; compute new segment size
	sub	eax,fs:[bx+MasterSegDefRecStruc.mssSegOffset]
	cmp	eax,65536
	jb	clar2
	mov	eax,65536

clar2:
	mov	fs:[bx+MasterSegDefRecStruc.mssSegLength],eax
	pop	eax				; restore critical register

wp3sgran:
ENDIF

; eax holds segment type
	mov	edx,fs:[bx+MasterSegDefRecStruc.mssSegLength]
	cmp	edx,100000h		; see if >=1M (need 4K byte granularity)
	jb	w3ps2			; no
	shr	edx,0ch			; convert to 4K pages
	or	edx,GRANULARITY4K	; set 4K granularity bit

w3ps2:
	or	eax,edx			; merge segment type with segment length+granularity
	mov	edx,FORCEDBITRESETFLAG	; assume 16-bit segment
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG	; see if 32-bit segment
	je	w3pssize		; no
	mov	edx,FORCEDBITSETFLAG	; 32-bit segment

w3pssize:
	or	eax,edx			; merge in segment default size (16/32 bit)

IFDEF WATCOM_ASM
	cmp	IsFlatOption,0	; see if flat option turned on
	je	w3pscont		; no
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG	; see if 32-bit segment
	je	w3pscont		; no
	or	eax,TRUEFLATSEGMENTFLAG	; flag as true flat segment (not near)

w3pscont:
ENDIF

	stosd				; save segment length+flags

w3psnextent:
	mov	ax,WORD PTR fs:[bx+MasterSegDefRecStruc.mssNextSegPtr]+2
	mov	bx,WORD PTR fs:[bx+MasterSegDefRecStruc.mssNextSegPtr]
	jmp	w3pssegloop

; done processing segments, write them out
w3psdone:
	mov	cx,TotalRelSegCount	; get count of segments

	sub	cx,DebugSegmentCount	; subtract off debug segments

	shl	cx,3			; 8 bytes (2 dwords) per segment
	mov	bx,ExecutableFileHandle
	xor	dx,dx
	push	ds			; save ds -> wl32 data
	mov	ds,IOBlockSeg	; ds:dx -> i/o buffer block
	call	WriteFile	; write the segment entries
	pop	ds				; restore ds -> wl32 data

w3psret:
	ret
Write3PSegEntries	ENDP

;*****************************
;* GET3PSEGMENTTYPE          *
;*****************************

; get type of segment (code/data/stack/const)
; CODE segment if:
;   class *CODE
;   class CO*, segment name CO*
;   class MISC, segment name MISC
;   class CLARION, segment name _CODE*
;   class CLARION, segment name _CLA_MAIN*
;   class CLARION, segment name *_BIN_SEG
; STACK segment if:
;   class *STACK
; CONST segment if:
;   class *CONST
;   class *MSG
;   class *DATA, segment name *CONST
; DATA segment if no match of above
; upon entry fs:bx -> segdef entry
; return in segment type flag eax
; maintain bx,edx,di,es,fs
; destroys si,bp

Get3PSegmentType	PROC
	push	es			; save critical registers
	push	bx
	push	di
	les	di,fs:[bx+MasterSegDefRecStruc.mssNamePtr]
	lgs	bx,fs:[bx+MasterSegDefRecStruc.mssClassPtr]
	cmp	bx,SIZEIOBUFFBLK-MAXOBJRECNAME	; check for possible overflow, normalize name if so
	jb	g3ps2			; no normalization needed
	call	NormalGSBXSource

; gs:bx -> class name
g3ps2:
	mov	bp,bx			; save gs:bp -> start of class name
	inc	bp				; gs:bp -> past length byte
	xor	al,al
	mov	cl,gs:[bx]		; get length byte of class name
	xor	ch,ch
	add	bx,cx
	inc	bx

; gs:bx -> null terminator after class
; check *CODE class
	cmp	cl,4			; class name must be at least four chars
	jb	g3pscl2			; no
	mov	eax,DWORD PTR CODEText	; get chars of "CODE"
	cmp	eax,gs:[bx-4]	; see if matches
	jne	g3pscl2

g3p2iscode:
	mov	eax,CODESEGMENTTYPE
	jmp	NEAR PTR g3psret	; segment is STACK class

; es:di -> segment name, not normalized
g3pscl2:
	cmp	di,SIZEIOBUFFBLK-MAXOBJRECNAME	; check for possible overflow, normalize name if so
	jb	g3ps3			; no normalization needed
	call	NormalESDIDest

g3ps3:
	mov	si,di			; save es:si -> start of segment name
	inc	si				; es:di -> past length byte
	mov	ch,es:[di]		; get length byte of segment name
	mov	al,ch
	xor	ah,ah
	add	di,ax
	inc	di

; gs:bx -> null terminator of class name
; gs:bp -> start of class name
; es:di -> null terminator of segment name
; es:si -> start of segment name
; cl holds class name length
; ch holds segment name length
; check CO* class, CO* name
	cmp	cl,2			; class name must be at least two chars
	jb	g3pscl3
	cmp	ch,2			; segment name must be at least two chars
	jb	g3pscl3
	mov	ax,WORD PTR COText
	cmp	ax,gs:[bp]		; see if class matches "CO"
	jne	g3pscl3
	cmp	ax,es:[si]		; see if name matches "CO"
	je	g3p2iscode		; yes

; check MISC class, MISC name
g3pscl3:
	cmp	cl,4			; class name must be four chars
	jne	g3pscl4
	cmp	ch,4			; segment name must be four chars
	jne	g3pscl4
	mov	eax,DWORD PTR MISCText
	cmp	eax,gs:[bp]		; see if class matches "MISC"
	jne	g3pscl4
	cmp	eax,es:[si]		; see if name matches "MISC"
	je	g3p2iscode		; yes

; check CLARION class, _CODE* name
g3pscl4:
	cmp	cl,7			; class name must be seven chars
	jne	g3psstack		; no, won't match other CLARION check either
	mov	eax,DWORD PTR CLARIONText
	cmp	eax,gs:[bp]		; see if matches "CLAR"
	jne	g3psstack		; no, won't match other CLARION check either
	mov	ax,WORD PTR CLARIONText+4
	cmp	ax,gs:[bp+4]	; see if matches "IO"
	jne	g3psstack		; no, won't match other CLARION check either
	mov	al,CLARIONText+6
	cmp	al,gs:[bp+6]	; see if matches "N"
	jne	g3psstack		; no, won't match other CLARION check either
	cmp	ch,5			; see if segment name at least five chars
	jb	g3pscl5			; no
	mov	eax,DWORD PTR _CODETexT
	cmp	eax,es:[si]		; see if matches "_COD"
	jne	g3pscl5			; no
	mov	al,_CODEText+4
	cmp	al,es:[si+4]	; see if matches "E"
	je	g3p2iscode		; yes

; check CLARION class, _CLA_MAIN* name
g3pscl5:
	cmp	ch,9			; see if segment name at least nine chars
	jb	g3pscl6			; no
	mov	eax,DWORD PTR _CLA_MAINText
	cmp	eax,es:[si]		; see if matches "_CLA"
	jne	g3pscl6			; no
	mov	eax,DWORD PTR _CLA_MAINText+4
	cmp	eax,es:[si+4]	; see if matches "_MAI"
	jne	g3pscl6			; no
	mov	al,_CLA_MAINText+8
	cmp	al,es:[si+8]	; see if matches "N"
	je	g3p2iscode		; yes

; check CLARION class, *_BIN_SEG name
g3pscl6:
	cmp	ch,8			; see if segment name at least eight chars
	jb	g3psstack		; no
	mov	eax,DWORD PTR _BIN_SEGText
	cmp	eax,es:[di-8]	; see if matches _BIN
	jne	g3psstack		; no
	mov	eax,DWORD PTR _BIN_SEGText+4
	cmp	eax,es:[di-4]	; see if matches _SEG
	je	g3p2iscode		; yes

; gs:bx -> null terminator of class name
; es:di -> null terminator of segment name
; es:si -> start of segment name
; cl holds class name length
; ch holds segment name length
; check *STACK class
g3psstack:
	cmp	cl,5			; class name must be at least five chars
	jb	g3psconstchk	; no
	mov	eax,DWORD PTR STACKText	; get first four chars of "STACK"
	cmp	eax,gs:[bx-5]	; see if matches "STAC"
	jne	g3psconstchk
	mov	al,STACKText+4	; get last char of "STACK"
	cmp	al,gs:[bx-1]
	jne	g3psconstchk	; not class STACK
	mov	eax,STACKSEGMENTTYPE
	jmp	g3psret	; segment is STACK class

; gs:bx -> null terminator of class name
; es:di -> null terminator of segment name
; es:si -> start of segment name
; cl holds class name length
; ch holds segment name length
; check *CONST class
g3psconstchk:
	cmp	cl,5			; class name must be at least five chars
	jb	g3psconst2		; no
	mov	eax,DWORD PTR CONSTText
	cmp	eax,gs:[bx-5]	; see if matches "CONS"
	jne	g3psconst2		; no
	mov	al,CONSTText+4
	cmp	al,gs:[bx-1]	; see if matches "T"
	jne	g3psconst2		; no

g3psisconst:
	mov	eax,CONSTSEGMENTTYPE
	jmp	g3psret	; segment is STACK class

; check *MSG class
g3psconst2:
	cmp	cl,3			; class name must be at least three chars
	jb	g3psconst3		; no
	mov	ax,WORD PTR MSGText
	cmp	ax,gs:[bx-3]	; see if matches "MS"
	jne	g3psconst3		; no
	mov	al,MSGText+2
	cmp	al,gs:[bx-1]	; see if matches "G"
	je	g3psisconst		; yes

; check *DATA class, name *CONST
g3psconst3:
	cmp	cl,4			; class name must be at least three chars
	jb	g3psisdata		; no
	mov	eax,DWORD PTR DATAText
	cmp	eax,gs:[bx-4]	; see if matches "DATA"
	jne	g3psisdata		; no
	cmp	ch,5			; segment name must be at least five chars
	jb	g3psisdata		; no
	mov	eax,DWORD PTR CONSTText
	cmp	eax,es:[di-5]	; see if matches "CONS"
	jne	g3psisdata		; no
	mov	al,CONSTText+4
	cmp	al,es:[di-1]	; see if matches "T"
	je	g3psisconst		; yes

; default to DATA segment type
g3psisdata:
	mov	eax,DATASEGMENTTYPE

g3psret:
	pop	di				; restore critical registers
	pop	bx
	pop	es
	ret
Get3PSegmentType	ENDP

;*****************************
;* WRITE3PHEADER             *
;*****************************

; write 3P header (sysvars only)

Write3PHeader	PROC
	mov	bx,ExecutableFileHandle	; rewind file in case not at beginning
	mov	dx,WORD PTR DOSExtenderSize
	mov	cx,WORD PTR DOSExtenderSize+2
	mov	ax,4200h		; move file pointer relative start of file
	int	21h

	mov	dx,OFFSET DGROUP:NewHeader
	mov	cx,SIZE NewHeaderStruc
	call	WriteFile
	ret
Write3PHeader	ENDP

;*****************************
;* SETUP3PHEADER             *
;*****************************

; setup EXE header variables

Setup3PHeader	PROC
	mov	eax,EntryOffsetValue	; set program starting address
	mov	NewHeader.NewEntryEIP,eax

	cmp	FlatSegmentFlag,OFF	; see if flat segment mode
	jne	s3ph2			; yes, all of this has been set up
	mov	ax,EntrySegmentID
	mov	NewHeader.NewEntryCS,ax

	cmp	WORD PTR _ENDSegDefPtr+2,0	; see if stack segment in program
	je	s3ph2			; no

	lfs	bx,_ENDSegDefPtr	; fs:bx -> master segdef of stack segment
	mov	ax,fs:[bx+MasterSegDefRecStruc.mssSegmentID]
	mov	NewHeader.NewEntrySS,ax	; save SS value

IFDEF WATCOM_ASM
	cmp	IsDStoSSOption,OFF	; see if set DS to SS option specified
	je	s3phsp			; no
	inc	eax				; make segment ID relative 1
	mov	NewHeader.NewAutoDS,ax	; save auto DS value
ENDIF

s3phsp:
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegLength]
	mov	NewHeader.NewEntryESP,eax	; save SP value

s3ph2:
	mov	eax,RelocEntryCount
	mov	NewHeader.NewRelocs,eax	; set number of relocation entries

	shl	eax,2			; dword per relocation entry (x4)
	add	eax,SIZE NewHeaderStruc	; add in sysvar header size
	mov	dx,TotalRelSegCount	; get count of segments

	sub	dx,DebugSegmentCount

	mov	NewHeader.NewSegments,dx	; save
	movzx	edx,dx
	shl	edx,3			; 8 bytes (2 dwords) per segment
	add	eax,edx

	add	eax,NewHeader.NewExports	; add in size of export section
	add	eax,NewHeader.NewImports	; add in size of import section

;@@@	mov	NewHeader.NewHeaderSize,eax
	add	eax,HighestOffsetWritten
	mov	NewHeader.NewSize,eax

	mov	eax,HighestOffsetWritten
	mov	NewHeader.NewLength,eax	; save size of EXE image data

	mov	eax,ProgramImageSize
IFDEF CLIPPER
	add	eax,DGROUPSegLenAdjust	; adjust program image for DGROUP expansion
ENDIF
	mov	NewHeader.NewAlloc,eax	; save size of program memory needed

	xor	eax,eax

IFNDEF CLIPPER
	cmp	IsAny32BitSegFlag,OFF	; see if any 32-bit segments (assume all if so)
	jne	s3ph3			; yes, control flags remain zero
ENDIF
	or	eax,4001h		; set bit 0 and 14 of control flags

s3ph3:
	cmp	IsFastLoadOption,OFF	; see if fast load option specified
	je	s3ph4			; no
	or	eax,40000000h	; set bit 30 of control flags

s3ph4:
	mov	NewHeader.NewEntryFlags,eax	; set control flags

	ret
Setup3PHeader	ENDP

;*****************************
;* WRITE3PRELENTRIES         *
;*****************************

; write 3P relocation entries, sorted for fasting loading, plus
; allows fast-load option

Write3PRelEntries	PROC
	mov	ecx,RelocEntryCount
	or	ecx,ecx
	je	w3prret			; no relocation entries

	shl	ecx,2			; convert entries to bytes
	mov	esi,ecx			; save bytes to convert/store/write
	add	ecx,4			; add one entry's worth for relative 1 adjustmet
	mov	dx,cx
	shr	ecx,16			; byte count in cx:dx
	call	AllocateBigMemory
	mov	es,ax			; es -> entry storage
	mov	edi,4			; edi offsets into storage, start at relative 1 (dword)

	mov	ax,FirstRelocEntryBlkPtr	; ax -> first relocation entry block

w3prloop:
	or	ax,ax			; see if more relocation entries
	je	w3prsort		; no
	mov	fs,ax			; fs -> relocation entry block
	mov	ecx,esi			; get bytes to write
	cmp	ecx,(SIZERELOCENTRYBLK-RELOCENTRYSYSVARSIZE)/2	; see if more than one block left
	jbe	w3pr2			; no
	mov	cx,(SIZERELOCENTRYBLK-RELOCENTRYSYSVARSIZE)/2

; cx holds bytes to convert and store for this block
; fs -> relocation entry block
; convert entries from 32-bit seg: 32-bit offset to 32-bit offset and store
; es:edi -> storage
w3pr2:
	mov	dx,cx			; save bytes converted/stored
	shr	cx,2			; cx holds entries in block
	mov	bx,RELOCENTRYSYSVARSIZE	; fs:bx -> relocation entries

w3prcsloop:
	mov	eax,fs:[bx+4]	; get segment value
	shl	eax,4			; convert to offset

w3compabs:
	add	eax,fs:[bx]		; compute absolute 32-bit offset
	DB	67h				; make 32-bit stosd
	stosd				; save offset
	add	bx,8			; move to next entry, if any
	dec	cx				; drop count of entries to update
	jne	w3prcsloop		; more to convert and store

	mov	ax,fs:[RelocEntryBlkStruc.rebNextPtr]	; ax -> next block, if any

; release old block's memory, never used again, can help performance
	push	ax
	mov	ax,fs
	call	ReleaseMemory
	pop	ax				; restore ax -> next block

	movzx	edx,dx		; extend bytes converted/stored to 32-bits
	sub	esi,edx			; subtract bytes converted off of total bytes
	jne	w3prloop		; loop for next block to convert/store

; sort the block of relocation entries
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
w3prsort:
	mov	eax,RelocEntryCount
	cmp	eax,1
	jbe	w3prwrite		; sorted by definition

	push	ds
	pop	gs				; gs -> DGROUP

; sort the offsets, es -> pointer buffer
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

shlsethloop:
	cmp	eax,ecx			; h<=N/9
	ja	shlsort2
	mul	ebx				; 3*h, assume 32-bit result (pretty safe bet)
	inc	eax				; 3*h+1
	jmp	shlsethloop

; ebx will play role of j, edx will play role of h
shlsort2:
	mov	edx,eax			; edx == h

; for (;h>0;...
shlhloop:
	or	edx,edx			; h>0
	je	shlsortend

; for(i=h+1...
	mov	eax,edx
	inc	eax
	mov	gs:iValue,eax

; for(...;i<=N;...){
shliloop:
	mov	eax,gs:iValue
	cmp	eax,gs:RelocEntryCount
	ja	shlnexth

	mov	ecx,es:[4*eax]
	mov	gs:vValue,ecx	; v=a[i]
	mov	ebx,eax			; j=i

; while(j>h && a[j-h]>v){
shlwhileloop:
	cmp	ebx,edx			; j>h
	jbe	shlwhilefail

	mov	eax,ebx
	sub	eax,edx			; eax==j-h

; a[j-h] > v
	mov	eax,es:[4*eax]	; a[j-h], first offset
	cmp	eax,gs:vValue	; compare absolute offsets
	jb	shlwhilefail	; first < second, a[j-h]<v

; change the element
	mov	es:[4*ebx],eax	; a[j]=a[j-h]
	sub	ebx,edx			; j-=h
	jmp	shlwhileloop

shlwhilefail:
	mov	eax,gs:vValue
	mov	es:[4*ebx],eax	; a[j]=v

; for(...;i++){
	inc	gs:iValue
	jmp	NEAR PTR shliloop

; for (...;h/=3){
shlnexth:
	mov	eax,edx
	xor	edx,edx
	mov	ecx,edx
	mov	cl,3
	div	ecx
	mov	edx,eax
	jmp	NEAR PTR shlhloop

shlsortend:
	mov	ax,DGROUP
	mov	ds,ax

w3prwrite:
	mov	esi,RelocEntryCount
	shl	esi,2			; convert entries to bytes
	mov	edx,4			; init relocation entry offset, relative 1 (dword)
	mov	bx,ExecutableFileHandle
	push	ds			; save ds -> wl32 data
	push	es
	pop	ds				; ds:dx -> bytes to write

w3prwrloop:
	mov	ecx,esi			; get bytes to write
	cmp	ecx,0fff0h		; see if more than 64K-16 left
	jbe	w3prwrite2		; no
	mov	cx,0fff0h		; set to maximum

w3prwrite2:
	call	WriteFile	; write the relocation entries
	movzx	eax,ax		; extend bytes written to 32-bits
	add	edx,eax
	sub	esi,eax			; subtract bytes written off of bytes to write
	jne	w3prwrloop		; not all written, keep looping

	pop	ds				; restore ds -> wl32 data
	mov	ax,es			; memory for relocation sort table no longer required, free it
	push	ds
	pop	es				; es -> wl32 data
	call	ReleaseMemory

w3prret:
	ret
Write3PRelEntries	ENDP

IFDEF DLLSUPPORT

;*****************************
;* WRITEEXPORTENTRIES        *
;*****************************

; export section is in the following format:
;  dword number of entries (table start)
;   number of entries+1 of dword offsets of export record (relative table start)
;     export record has the following format:
;       dword offset value
;       word segment number
;       byte function name string length
;       variable name string
;     first export record is module's: name length, module name ONLY
; **** IMPORTANT NOTE ***
; EXPDEF's are NOT explicitly sorted by ordinal number, if exists
; WL32 assumes that EXPDEF's are presented in ordinal order by default
; 3PLIB always does this, so it's not currently not a problem
; It may become a problem when supporting other environments, however.

WriteExportEntries	PROC
	cmp	TotalEXPDEFCount,0	; see if any exports
	je	weeret			; no

; seek to end of file (end of relocation entries)
	mov	bx,ExecutableFileHandle
	call	SeekToEndOfFile

	mov	WORD PTR PreviousFilePosition,ax
	mov	WORD PTR PreviousFilePosition+2,dx

	mov	ecx,TotalEXPDEFCount
	mov	esi,ecx			; save count
	shl	ecx,2			; dword offset entry per expdef
	add	ecx,(4+4)		; add in fixed length amounts
	mov	edi,ecx			; edi holds bytes of info
	mov	ebp,ecx			; ebp, as well
	mov	dx,cx
	shr	ecx,16			; byte count in cx:dx
	call	AllocateBigMemory
	push	ds
	mov	ds,ax			; ds -> entry storage
	xor	edx,edx			; edx -> offset

; do a dummy placeholder write for expdef header info
weewloop1:
	mov	ecx,edi
	cmp	ecx,0fff0h		; see if past max bytes to write
	jbe	weewrite1		; no
	mov	cx,0fff0h		; set bytes to write to max

weewrite1:
	call	WriteFile
	movzx	eax,ax
	sub	edi,eax			; update bytes to write
	jne	weewloop1		; don't need to update offset since it's dummy data
	push	ds
	pop	es				; es -> expdef storage
	pop	ds				; restore ds -> wl32 data

	xor	edi,edi			; edi offsets into expdef entry info
	mov	es:[edi],esi	; expdef count in position zero
	mov	es:[edi+4],ebp	; save offset to module name entry

;	mov	DWORD PTR WorkingBuffer,0	; module name dummy offset value
;	mov	WORD PTR WorkingBuffer+4,0	; dummy segment value
	xor	cx,cx
	mov	edx,OFFSET DGROUP:DLLFileName

weecountloop:
	mov	al,ds:[edx]		; get file name char
	test	al,al
	je	weeendfn		; at end of file name
	inc	cx				; bump count of chars in file name
	inc	edx
	jmp	weecountloop

weeendfn:
;	mov	BYTE PTR WorkingBuffer+6,cl	; save count of file name
	mov	BYTE PTR WorkingBuffer,cl	; save count of file name
;	mov	cx,7			; seven bytes to write for module name entry
	mov	cx,1			; write module name length byte

	mov	dx,OFFSET DGROUP:WorkingBuffer
	call	WriteFile
	mov	dx,OFFSET DGROUP:DLLFileName
;	mov	cl,BYTE PTR WorkingBuffer+6	; ch known zero
	mov	cl,BYTE PTR WorkingBuffer	; ch known zero
	call	WriteFile	; write module name

	mov	SearchExistSymFlag,ON	; search for existing symbols, only
	mov	ax,FirstEXPDEFBlkPtr	; init ax to first expdef block

weeblkloop:
	mov	fs,ax			; fs -> current expdef block
	mov	bx,EXPDEFSYSVARSIZE	; fs:bx -> first expdef entry in block
	xor	cx,cx			; init expdef entry count in block

weeentloop:
	push	fs		; save critical registers
	push	bx
	push	cx
	push	si
	push	di

	push	bx			; save fs:bx -> entry
	push	fs
	lfs	si,fs:[bx+EXPDEFRecStruc.edsInternalNamePtr]
	push	fs			; save original pointer
	push	si
	call	ReadByte	; get name length
	pop	si				; restore original pointer
	pop	fs
	or	al,al			; see if zero length (use exported name for internal)
	jne	weefind			; no
	pop	fs				; fs -> expdef block
	push	fs
	lfs	si,fs:[bx+EXPDEFRecStruc.edsExportedNamePtr]

weefind:
	call	GetPubSymEntry	; find the internal symbol name, return gs:di -> entry
	jc	weebad			; symbol name not found

; gs:di -> public symbol entry
	lfs	si,gs:[di+PubSymRecStruc.pssIndSegDefPtr]	; fs:si -> individual segdef
	mov	eax,fs:[si+IndSegDefRecStruc.isdrSegOffset]	; get individual segment offset
	add	eax,gs:[di+PubSymRecStruc.pssOffset]	; add in public offset
	mov	DWORD PTR WorkingBuffer,eax	; save exported offset value
	lfs	si,fs:[si+IndSegDefRecStruc.isdrMasterPtr]	; fs:si -> master segdef
	mov	ax,fs:[si+MasterSegDefRecStruc.mssSegmentID]	; get segment identifier
	mov	WORD PTR WorkingBuffer+4,ax	; save exported segment value

	pop	fs			; restore fs:bx -> expdef entry
	pop	bx
	lfs	si,fs:[bx+EXPDEFRecStruc.edsExportedNamePtr]	; fs:si -> exported name
	mov	di,OFFSET DGROUP:WorkingBuffer+6
	call	ReadByte	; get symbol name length
	mov	ds:[di],al
	inc	di
	xor	cx,cx		; symbol length in cx
	or	cl,al
	je	weenextent

weenameloop:
	call	ReadByte	; get name char

IFDEF WATCOM_ASM
	cmp	IsCaseSensitiveOption,OFF
	jne	weetrans
ENDIF

	cmp	al,'a'			; convert lowercase to upper
	jb	weetrans
	cmp	al,'z'
	ja	weetrans
	sub	al,20h

weetrans:
	mov	ds:[di],al		; transfer to buffer to write
	inc	di
	dec	cx
	jne	weenameloop

	pop	di				; restore header pointer
	push	di

; esi holds count of entries
; es:[4*edi+8] -> current expdef header offset entry
	mov	bx,ExecutableFileHandle
	call	SeekToEndOfFile
	push	dx
	push	ax
	pop	edx
	sub	edx,PreviousFilePosition	; make relative to expdef section start
	mov	es:[4*edi+8],edx

	movzx	cx,BYTE PTR WorkingBuffer+6	; get name length
	add	cx,7		; add in offset, segment, length overhead
	mov	dx,OFFSET DGROUP:WorkingBuffer
	call	WriteFile	; write the expdef entry

weenextent:
	pop	di			; restore critical registers
	pop	si
	pop	cx
	pop	bx
	pop	fs

	add	bx,SIZE EXPDEFRecStruc	; fs:bx -> next public entry
	inc	cx				; bump current expdef in block
	inc	edi				; point to next entry, if any
	dec	esi				; drop count of entries to process
	je	weeupsize		; no more entries
	cmp	cx,MAXCOUNTEXPDEFBLK	; see if more entries in block (might not be all used)
	jb	weeentloop		; more entries in block
	mov	ax,fs:[EXPDEFBlkStruc.edbNextPtr]	; ax -> next block
	jmp	weeblkloop

weeupsize:
	mov	bx,ExecutableFileHandle
	call	SeekToEndOfFile	; get new file size after expdef entry additions
	push	dx
	push	ax
	pop	edx
	sub	edx,PreviousFilePosition	; compute size of expdef section
	mov	NewHeader.NewExports,edx	; set size of expdef section

	mov	dx,WORD PTR PreviousFilePosition
	mov	cx,WORD PTR PreviousFilePosition+2
	mov	ax,4200h
	int	21h				; position to start of expdef header

	push	ds
	push	es
	pop	ds				; ds -> expdef storage
	xor	edx,edx			; edx -> offset

; write the updated expdef header info
; ebp stills holds total expdef header bytes
weewloop2:
	mov	ecx,ebp
	cmp	ecx,0fff0h		; see if past max bytes to write
	jbe	weewrite2		; no
	mov	cx,0fff0h		; set bytes to write to max

weewrite2:
	call	WriteFile
	movzx	eax,ax
	add	edx,eax			; update offset
	sub	ebp,eax			; update bytes to write
	jne	weewloop2
	pop	ds				; restore ds -> wl32 data

	mov	ax,es			; memory for export entry table no longer required, free it
	push	ds
	pop	es				; es -> wl32 data
	call	ReleaseMemory
	call	SeekToEndOfFile
	mov	SearchExistSymFlag,OFF	; reset search for existing symbols flag

weeret:
	ret

; export definition without associated pubdef symbol
weebad:
	mov	cl,4
	call	InternalErrorExit	; no return

WriteExportEntries	ENDP

;*****************************
;* WRITEIMPORTENTRIES        *
;*****************************

; import section is in the following format:
;   dword offset to list of module names data
;   dword offset to list of function names data
;   dword offset of fixup data
;     dword number of modules (table start)
;       dword offsets to module names (relative to start of table)
;         list of module names, with prepended string length byte
;     dword number of function names (table start)
;       dword offset to function names (relative to start of table)
;         list of function names, with prepended string length byte
;     dword number of fixup entries
;       fixup data:
;         byte - fixup type
;                bit 7 == ordinal/name flag, set if ordinal
;                bit 6 == self-relative flag, set if self-relative
;                bit 5-0 == fixup type
;                  value (not bit) 0 == 16-bit offset
;                  value (not bit) 1 == 32-bit offset
;                  value (not bit) 2 == 16-bit seg:offset
;                  value (not bit) 3 == 32-bit seg:offset
;                  value (not bit) 4 == 16-bit segment only
;         dword - fixup length/offset
;                 bit 31-30 == module number length in bytes relative 0
;                 bit 29-28 == function number length in bytes relative 0
;                 bit 27-0 == fixup offset within program image
;         variable (1-4 bytes) == module name number
;         variable (1-4 bytes) == function name number

WriteImportEntries	PROC
	cmp	TotalIMPDEFCount,0	; see if any imports
	je	wieret			; no

; seek to end of file (end of import entries)
	mov	bx,ExecutableFileHandle
	call	SeekToEndOfFile
	mov	WORD PTR PreviousFilePosition,ax
	mov	WORD PTR PreviousFilePosition+2,dx

; do a dummy placeholder write for impdef header info
	mov	dx,OFFSET DGROUP:WorkingBuffer
	mov	cx,12
	call	WriteFile

	mov	DWORD PTR WorkingBuffer,12	; module name offset always 12 bytes

; save pointer to start of module count/offset pointer table
	call	SeekToEndOfFile
	mov	WORD PTR FilePositionBookmark,ax
	mov	WORD PTR FilePositionBookmark+2,dx

	mov	eax,LastIMPDEFModule	; last module number is total relative 0
	inc	eax				; make relative 1
	mov	NewHeader.NewImportModCnt,eax	; save count of import modules

; allocate for module count and offset pointer table
	mov	ecx,eax
	shl	ecx,2			; dword offset entry per module
	add	ecx,4			; adjust for module count dword
	mov	edi,ecx			; save total byte count of table
	mov	dx,cx
	shr	ecx,16			; byte count in cx:dx
	call	AllocateBigMemory
	push	ds
	mov	ds,ax			; ds -> entry storage
	xor	edx,edx			; edx -> offset

; do a dummy placeholder write for table
tabwloop1:
	mov	ecx,edi
	cmp	ecx,0fff0h		; see if past max bytes to write
	jbe	tabwrite1		; no
	mov	cx,0fff0h		; set bytes to write to max

tabwrite1:
	call	WriteFile
	movzx	eax,ax
	sub	edi,eax			; update bytes to write
	jne	tabwloop1		; don't need to update offset since it's dummy data
	push	ds
	pop	es				; es -> module table storage
	pop	ds				; restore ds -> wl32 data

	mov	eax,NewHeader.NewImportModCnt
	xor	esi,esi			; es:esi -> module count/offset table
	mov	es:[esi],eax	; module count

;	mov	ebp,eax
	mov	ebp,TotalIMPDEFCount

	mov	ax,FirstIMPDEFBlkPtr	; init ax to first impdef block

wieblkloop1:
	mov	gs,ax			; gs -> current impdef block
	mov	bx,IMPDEFSYSVARSIZE	; gs:bx -> first impdef entry in block
	xor	cx,cx			; init impdef entry count in block

wieentloop1:
	push	cx			; save impdef entry count
	push	bx			; save -> impdef entry
	test	BYTE PTR gs:[bx+IMPDEFRecStruc.idsGeneralFlags],NEWMODULENAMEFLAG
	je	wienextent1		; not a new module name

	push	si
	lfs	si,gs:[bx+IMPDEFRecStruc.idsModuleNamePtr]	; fs:si -> module name
	call	ReadByte	; get name length
	mov	di,OFFSET DGROUP:WorkingBuffer+12	; di -> module name storage
	mov	ds:[di],al
	inc	di
	xor	cx,cx		; symbol length in cx
	or	cl,al
	je	wiesaveptr1

wienameloop1:
	call	ReadByte	; get name char

IFDEF WATCOM_ASM
	cmp	IsCaseSensitiveOption,OFF
	jne	wietrans1
ENDIF

	cmp	al,'a'			; convert lowercase to upper
	jb	wietrans1
	cmp	al,'z'
	ja	wietrans1
	sub	al,20h

wietrans1:
	mov	ds:[di],al		; transfer to buffer to write
	inc	di
	dec	cx
	jne	wienameloop1

wiesaveptr1:
	pop	si
	mov	bx,ExecutableFileHandle
	call	SeekToEndOfFile
	push	dx
	push	ax
	pop	edx
;	sub	edx,PreviousFilePosition	; make relative to impdef section start
	sub	edx,FilePositionBookmark	; make relative to table start
	mov	es:[4*esi+4],edx	; save pointer in module name offset table
	inc	esi			; bump to next offset entry

	movzx	cx,BYTE PTR WorkingBuffer+12	; get name length
	inc	cx			; length byte overhead
	cmp	cx,4		; must be four bytes
	jae	wiewrimp1
	mov	cx,4		; force four bytes

wiewrimp1:
	mov	dx,OFFSET DGROUP:WorkingBuffer+12
	call	WriteFile	; write the impdef entry

wienextent1:
	pop	bx				; restore bx -> impdef entry
	pop	cx				; restore impdef entry count
	add	bx,SIZE IMPDEFRecStruc	; gs:bx -> next impdef entry
	inc	cx				; bump current impdef in block
	dec	ebp				; drop count of entries to process
	je	wiemoddone		; no more entries
	cmp	cx,MAXCOUNTIMPDEFBLK	; see if more entries in block (might not be all used)
	jb	wieentloop1		; more entries in block
	mov	ax,gs:[IMPDEFBlkStruc.idbNextPtr]	; ax -> next block
	jmp	wieblkloop1

; position back and update module table info
wiemoddone:
	mov	bx,ExecutableFileHandle
	mov	dx,WORD PTR FilePositionBookmark
	mov	cx,WORD PTR FilePositionBookmark+2
	mov	ax,4200h
	int	21h				; position to start of module table header

	push	ds
	push	es
	pop	ds				; ds -> module table header storage
	xor	edx,edx			; edx -> offset

; write the updated module table info
	mov	ax,es
	movzx	eax,ax
	lsl	ebp,eax
	inc	ebp				; ebp == total table size

tabwloop2:
	mov	ecx,ebp
	cmp	ecx,0fff0h		; see if past max bytes to write
	jbe	tabwrite2		; no
	mov	cx,0fff0h		; set bytes to write to max

tabwrite2:
	call	WriteFile
	movzx	eax,ax
	add	edx,eax			; update offset
	sub	ebp,eax			; update bytes to write
	jne	tabwloop2
	pop	ds				; restore ds -> wl32 data

	mov	ax,es			; memory for export entry table no longer required, free it
	push	ds
	pop	es				; es -> wl32 data
	call	ReleaseMemory

	call	SeekToEndOfFile
	mov	WORD PTR FilePositionBookmark,ax	; save pointer to start of function offset table
	mov	WORD PTR FilePositionBookmark+2,dx
	push	dx
	push	ax
	pop	edx
	sub	edx,PreviousFilePosition	; make relative to impdef section start
	mov	DWORD PTR WorkingBuffer+4,edx

; allocate for function count and offset pointer table
	mov	ecx,TotalIMPDEFFuncCount	; total count of functions
	shl	ecx,2			; dword offset entry per function
	add	ecx,4			; adjust for functoin count dword
	mov	edi,ecx			; save total byte count of table
	mov	dx,cx
	shr	ecx,16			; byte count in cx:dx
	call	AllocateBigMemory
	push	ds
	mov	ds,ax			; ds -> entry storage
	xor	edx,edx			; edx -> offset

; do a dummy placeholder write for table
tabwloop3:
	mov	ecx,edi
	cmp	ecx,0fff0h		; see if past max bytes to write
	jbe	tabwrite3		; no
	mov	cx,0fff0h		; set bytes to write to max

tabwrite3:
	call	WriteFile
	movzx	eax,ax
	sub	edi,eax			; update bytes to write
	jne	tabwloop3		; don't need to update offset since it's dummy data
	push	ds
	pop	es				; es -> module table storage
	pop	ds				; restore ds -> wl32 data

;	mov	ebp,TotalIMPDEFFuncCount	; total count of functions
	mov	ebp,TotalIMPDEFCount

	xor	esi,esi			; es:esi -> function count/offset table
	mov	eax,TotalIMPDEFFuncCount
	mov	es:[esi],eax	; function count
	mov	ax,FirstIMPDEFBlkPtr	; init ax to first impdef block

wieblkloop2:
	mov	gs,ax			; gs -> current impdef block
	mov	bx,IMPDEFSYSVARSIZE	; gs:bx -> first impdef entry in block
	xor	cx,cx			; init impdef entry count in block

wieentloop2:
	push	cx			; save impdef entry count
	push	bx			; save -> impdef entry
	test	BYTE PTR gs:[bx+IMPDEFRecStruc.idsGeneralFlags],FLUSHIMPDEFFLAG
	jne	wienextent2		; flush this import
	cmp	BYTE PTR gs:[bx+IMPDEFRecStruc.idsOrdinalFlag],1
	je	wienextent2		; ordinal import, no need to keep name

	push	si
	lfs	si,gs:[bx+IMPDEFRecStruc.idsInternalNamePtr]	; fs:si -> function name
	call	ReadByte	; get name length
	mov	di,OFFSET DGROUP:WorkingBuffer+12	; di -> module name storage
	mov	ds:[di],al
	inc	di
	xor	cx,cx		; symbol length in cx
	or	cl,al
	je	wiesaveptr2

wienameloop2:
	call	ReadByte	; get name char

IFDEF WATCOM_ASM
	cmp	IsCaseSensitiveOption,OFF
	jne	wietrans2
ENDIF

	cmp	al,'a'			; convert lowercase to upper
	jb	wietrans2
	cmp	al,'z'
	ja	wietrans2
	sub	al,20h

wietrans2:
	mov	ds:[di],al		; transfer to buffer to write
	inc	di
	dec	cx
	jne	wienameloop2

wiesaveptr2:
	pop	si
	mov	bx,ExecutableFileHandle
	call	SeekToEndOfFile
	push	dx
	push	ax
	pop	edx
;	sub	edx,PreviousFilePosition	; make relative to impdef section start
	sub	edx,FilePositionBookmark	; make relative to table start
	mov	es:[4*esi+4],edx	; save pointer in function name offset table
	inc	esi			; bump to next offset entry

	movzx	cx,BYTE PTR WorkingBuffer+12	; get name length
	inc	cx			; length byte overhead
	cmp	cx,4		; must be four bytes
	jae	wiewrimp2
	mov	cx,4		; force four bytes

wiewrimp2:
	mov	dx,OFFSET DGROUP:WorkingBuffer+12
	call	WriteFile	; write the impdef entry

wienextent2:
	pop	bx				; restore bx -> impdef entry
	pop	cx				; restore impdef entry count
	add	bx,SIZE IMPDEFRecStruc	; gs:bx -> next impdef entry
	inc	cx				; bump current impdef in block
	dec	ebp				; drop count of entries to process
	jle	wiefuncdone	; no more entries
	cmp	cx,MAXCOUNTIMPDEFBLK	; see if more entries in block (might not be all used)
	jb	wieentloop2		; more entries in block
	mov	ax,gs:[IMPDEFBlkStruc.idbNextPtr]	; ax -> next block
	jmp	wieblkloop2

; position back and update function table info
wiefuncdone:
	mov	bx,ExecutableFileHandle
	mov	dx,WORD PTR FilePositionBookmark
	mov	cx,WORD PTR FilePositionBookmark+2
	mov	ax,4200h
	int	21h				; position to start of module table header

	push	ds
	push	es
	pop	ds				; ds -> module table header storage
	xor	edx,edx			; edx -> offset

; write the updated function table info
	mov	ax,es
	movzx	eax,ax
	lsl	ebp,eax
	inc	ebp				; ebp == total table size

tabwloop4:
	mov	ecx,ebp
	cmp	ecx,0fff0h		; see if past max bytes to write
	jbe	tabwrite4		; no
	mov	cx,0fff0h		; set bytes to write to max

tabwrite4:
	call	WriteFile
	movzx	eax,ax
	add	edx,eax			; update offset
	sub	ebp,eax			; update bytes to write
	jne	tabwloop4
	pop	ds				; restore ds -> wl32 data

	mov	ax,es			; memory for function table no longer required, free it
	push	ds
	pop	es				; es -> wl32 data
	call	ReleaseMemory

; write fixup entry info
	call	SeekToEndOfFile
	push	dx
	push	ax
	pop	edx
	sub	edx,PreviousFilePosition	; make relative to impdef section start
	mov	DWORD PTR WorkingBuffer+8,edx

;  write count of fixups
	mov	eax,TotalIMPDEFFixupCount
	mov	DWORD PTR WorkingBuffer+12,eax
	mov	dx,OFFSET DGROUP:WorkingBuffer+12
	mov	cx,4
	mov	bx,ExecutableFileHandle
	call	WriteFile

; write fixup data
	push	ds
	mov	edi,TotalIMPDEFFixupSize
	mov	ds,IMPDEFFixupSel	; ds -> fixup entry storage
	xor	edx,edx			; edx -> offset

wiewloop:
	mov	ecx,edi
	cmp	ecx,0fff0h		; see if past max bytes to write
	jbe	wiewrite		; no
	mov	cx,0fff0h		; set bytes to write to max

wiewrite:
	call	WriteFile
	movzx	eax,ax
	add	edx,eax			; update offset
	sub	edi,eax			; update bytes to write
	jne	wiewloop
	pop	ds				; restore ds -> wl32 data

	call	SeekToEndOfFile	; get new file size after impdef entry additions
	push	dx
	push	ax
	pop	edx
	sub	edx,PreviousFilePosition	; compute size of impdef section
	mov	NewHeader.NewImports,edx	; set size of impdef section

	mov	dx,WORD PTR PreviousFilePosition
	mov	cx,WORD PTR PreviousFilePosition+2
	mov	ax,4200h
	int	21h				; position to start of impdef header

; write the updated impdef header info
	mov	dx,OFFSET DGROUP:WorkingBuffer
	mov	cx,12
	call	WriteFile
	call	SeekToEndOfFile

wieret:
	ret
WriteImportEntries	ENDP

ENDIF

;*****************************
;* WRITEDOSEXTENDER          *
;*****************************

; write DOS extender from start of WL32.EXE
; destroys ax,bx,cx,dx,di,si,es

WriteDOSExtender	PROC
	xor	ax,ax
	mov	di,ax
	mov	es,PSP
	mov	es,es:[2ch]		; es:di -> environment block strings

wdeenvloop:
	mov	cx,07fffh		; must find end before 32K
	repne	scasb		; find end of environment string
	scasb				; see if end of environment block
	jne	wdeenvloop		; no

	add	di,2			; adjust di past word value, es:di -> WL32.EXE's name
	push	ds
	push	es
	pop	ds
	pop	es				; es -> wl32 data, ds -> environment block
	push	es			; save -> wl32 data back to stack
	mov	si,di			; ds:si -> WL32 name
	mov	di,OFFSET DGROUP:CurrentFileName
	mov	dx,di
	xor	al,al

; place WL32 file name in CurrentFileName
wdenameloop:
	movsb
	cmp	al,ds:[si-1]	; see if null terminator transferred
	jne	wdenameloop		; no

; dx -> file name
	pop	ds				; restore ds -> wl32 data
	mov	al,40h			; read only, deny none access
	call	OpenFile
	mov	bx,ax			; save file handle
	mov	WL32FileHandle,ax

	cmp	IsThreePOption,OFF	; see if creating 3P file only (no DOS extender header)
	jne	wderet			; yes, leave DOS extender size at 0, no extender setup

; read the EXE header of WL32
	mov	cx,SIZE EXEHeaderStruc
	mov	dx,OFFSET DGROUP:EXEHeader
	call	ReadFile

; rewind back to start of file
	xor	cx,cx
	mov	dx,cx
	mov	ax,4200h		; move file pointer relative start of file
	int	21h

; compute DOS extender size
	mov	ax,EXEHeader.ehFileSizePages
	movzx	eax,ax
	shl	eax,9			; convert 512-byte pages to bytes
	mov	dx,EXEHeader.ehFileLenMod512
	movzx	edx,dx
	add	eax,edx
	or	edx,edx			; see if exact boundary match
	je	wde2			; yes
	sub	eax,512			; back off page rounding adjustment

; eax holds size of EXE Header
wde2:
	mov	DOSExtenderSize,eax	; save size of DOS extender
	mov	esi,eax

wdefileloop:
	mov	ecx,esi			; get bytes to write
	cmp	ecx,0fff0h		; see if past max bytes to read/write
	jbe	wderead			; no
	mov	cx,0fff0h		; set bytes to write to max

wderead:
	xor	dx,dx
	mov	bx,WL32FileHandle
	push	ds
	mov	ds,IOBlockSeg	; ds:dx -> i/o buffer block
	call	ReadFile	; read DOS extender
	pop	ds				; restore ds -> wl32 data
	movzx	eax,ax
	sub	esi,eax			; update bytes to read/write
	mov	bx,ExecutableFileHandle
	push	ds
	mov	ds,IOBlockSeg	; ds:dx -> i/o buffer block
	call	WriteFile	; write DOS extender
	pop	ds				; restore ds -> wl32 data
	or	esi,esi			; see if more bytes to read/write
	jne	wdefileloop		; yes

; close the WL32 file
	mov	bx,WL32FileHandle
	mov	ah,3eh
	int	21h

wderet:
	ret
WriteDOSExtender	ENDP

;*****************************
;* OPENEXECUTABLEFILE        *
;*****************************

; open the executable file, either EXE or protected P3

OpenExecutableFile	PROC
	mov	dx,OFFSET DGROUP:EXEFileName	; dx -> file name
	call	CreateFile
	mov	ExecutableFileHandle,ax	; keep file handle
	ret
OpenExecutableFile	ENDP

;*****************************
;* SETUPEXEHEADER            *
;*****************************

; setup EXE header variables

SetupEXEHeader	PROC
	mov	ax,WORD PTR EntrySegmentValue	; set program starting address
	mov	EXEHeader.ehCS,ax
	mov	ax,WORD PTR EntryOffsetValue
	mov	EXEHeader.ehIP,ax

	cmp	WORD PTR _ENDSegDefPtr+2,0	; see if stack segment in program
	je	seh2			; no
	lfs	bx,_ENDSegDefPtr	; fs:bx -> master segdef of stack segment
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegOffset]
	mov	dl,al			; keep low byte
	and	dl,0fh			; keep nonsegment value
	xor	dh,dh			; dx holds offset from canonical segment
	shr	eax,4			; convert to paragraphs
	mov	EXEHeader.ehSS,ax	; save EXE SS value

	mov	ax,fs:[bx+WORD PTR MasterSegDefRecStruc.mssSegLength]

; covered in segment resolution phase by adjusting stack segment length
COMMENT !
	cmp	IsStackOption,OFF	; see if stack option
	je	sehstack		; no
	cmp	ax,StackValue	; see if current stack < option stack
	jae	sehstack		; no
	movzx	eax,ax
	sub	ProgramImageSize,eax	; update program's stack value, image size
	mov	ax,StackValue	; update stack length with option stack size
	mov	fs:[bx+WORD PTR MasterSegDefRecStruc.mssSegLength],ax
	add	ProgramImageSize,eax	; update program image size
END COMMENT !

sehstack:
	add	ax,dx			; compute SP value
	mov	EXEHeader.ehSP,ax	; save EXE SP value

seh2:
	mov	eax,RelocEntryCount
	mov	EXEHeader.ehNumRelEntries,ax	; set number of relocation entries

	shl	eax,2			; dword per relocation entry (x4)
	mov	dx,EXEHeader.ehRelOffset	; get header system bytes, not relocation entries
	movzx	edx,dx
	add	eax,edx			; add system bytes to relocation entry bytes for total used header size
	add	eax,511			; round up to next 512-byte boundary
	and	ax,0fe00h		; mask size to boundary
	mov	edx,eax			; save header size in bytes
	shr	eax,4			; convert to paragraphs
	mov	EXEHeader.ehHeaderSize,ax

	mov	eax,edx			; get header size to bytes
	add	eax,HighestOffsetWritten	; add in highest offset written to with data
	mov	dx,ax
	and	dx,511			; dx holds size of file modulo 512
	mov	EXEHeader.ehFileLenMod512,dx
	add	eax,511			; round up to next 512-byte page
	shr	eax,9			; convert file size to 512-byte pages
	mov	EXEHeader.ehFileSizePages,ax

; compute minimum number of paragraphs needed above program by subtracting
; the highest written offset from the program image size and rounding to
; next highest paragraph boundary
	mov	eax,ProgramImageSize
	sub	eax,HighestOffsetWritten
	add	eax,14			; adjust offset to relative 1, round up to next paragraph
	shr	eax,4			; convert to paragraphs
	mov	EXEHeader.ehMinAlloc,ax

	ret
SetupEXEHeader	ENDP

;*****************************
;* WRITEEXEHEADER            *
;*****************************

; write EXE header

WriteEXEHeader	PROC
	mov	bx,ExecutableFileHandle	; rewind file in case not at beginning
	xor	cx,cx
	mov	dx,cx
	mov	ax,4200h		; move file pointer relative start of file
	int	21h

	mov	dx,OFFSET DGROUP:EXEHeader
	mov	cx,SIZE EXEHeaderStruc
	call	WriteFile
	ret
WriteEXEHeader	ENDP

;*****************************
;* WRITEEXERELENTRIES        *
;*****************************

; write EXE relocation entries

WriteEXERelEntries	PROC
	mov	esi,RelocEntryCount
	or	esi,esi
	je	werret			; no relocation entries

	shl	esi,2			; convert entries to write to bytes to write
	mov	ax,FirstRelocEntryBlkPtr	; ax -> first relocation entry block

werloop:
	or	ax,ax			; see if more relocation entries
	je	werret			; no
	mov	fs,ax			; fs -> relocation entry block
	mov	ecx,esi			; get bytes to write
	cmp	ecx,(SIZERELOCENTRYBLK-RELOCENTRYSYSVARSIZE)/2	; see if more than one block left to write
	jbe	wer2			; no
	mov	cx,(SIZERELOCENTRYBLK-RELOCENTRYSYSVARSIZE)/2

wer2:

; adjust for 2 dword entry in relocation table, 01/26/94
	mov	dx,cx
	shr	dx,2			; di holds entries in block
	mov	bx,RELOCENTRYSYSVARSIZE	; fs:bx -> relocation entries
	mov	di,bx
	push	dx			; keep entry count, bx pointer
	push	bx

; 01/26/94, mov segment low word into offset high word for each entry
weradjsegloop:
	mov	ax,fs:[di+4]	; get segment low word
	mov	fs:[bx+2],ax	; overwrite offset high word
	add	bx,8			; bx -> next entry
	add	di,8			; di -> next entry
	dec	dx				; drop count
	jne	weradjsegloop

; 01/26/94
	pop	bx				; bx -> relocation entries
	pop	dx				; dx holds count
	mov	di,bx

; 01/26/94, compact word seg: word off pairs over old invalid 32-bit segment value
weradjentloop:
	add	di,8			; di -> second entry, if any
	add	bx,4			; bx -> high dword of first entry
	dec	dx				; drop count
	je	werwrite		; no more entries to adjust
	mov	eax,fs:[di]		; get low dword of second entry
	mov	fs:[bx],eax		; overwrite high dword of first entry with low dword of second
	jmp	weradjentloop

werwrite:
	mov	dx,RELOCENTRYSYSVARSIZE	; start write past sysvars
	mov	bx,ExecutableFileHandle
	push	ds			; save ds -> wl32 data
	push	fs
	pop	ds				; ds:dx -> bytes to write
	call	WriteFile	; write the relocation entries
	pop	ds				; restore ds -> wl32 data
	movzx	eax,ax		; extend bytes written to 32-bits
	sub	esi,eax			; subtract bytes written off of bytes to write
	mov	ax,fs:[RelocEntryBlkStruc.rebNextPtr]	; ax -> next block, if any, KEEP FLAGS
	jne	werloop			; loop for next block to write

werret:
	ret
WriteEXERelEntries	ENDP

;*****************************
;* PADEXEHEADER              *
;*****************************

; zero pad EXE header to ehHeaderSize*16-byte boundary

PadEXEHeader	PROC
	mov	ax,WORD PTR RelocEntryCount
	shl	ax,2			; dword per entry (x4)
	add	ax,EXEHeader.ehRelOffset	; add in nonrelocation entries
	mov	cx,EXEHeader.ehHeaderSize
	shl	cx,4			; *16
	mov	dx,cx
	dec	dx				; remainder bits below boundary
	and	ax,dx			; compute bytes off proper boundary
	je	pehret			; evenly on boundary already

	sub	cx,ax			; cx holds # bytes needed to round to boundary
	xor	dx,dx
	mov	bx,ExecutableFileHandle
	push	ds
	mov	ds,ZeroBlockSeg	; ds:dx -> zero block segment for writing zero values
	call	WriteFile
	pop	ds

pehret:
	ret
PadEXEHeader	ENDP

;*****************************
;* WRITEEXEBODY              *
;*****************************

; write EXE body (program image)
; step through each master segdef entry
; write data associated with individual segdef

WriteEXEBody	PROC
	mov	ax,WORD PTR FirstSegment+2
	mov	bx,WORD PTR FirstSegment

; ax:bx -> master segdef entry, if any
webmainloop:
	test	ax,ax		; see if segment exists
	je	webdone			; no, done
	mov	gs,ax			; gs:bx -> master segdef entry
	test	gs:[bx+MasterSegDefRecStruc.mssFlags],ASSOCIATEDDATAFLAG	; see if data for segment
	je	webnextseg		; no data from segment, no file write needed
	test	gs:[bx+MasterSegDefRecStruc.mssFlags],(DEBUGTYPESSEGMENTFLAG OR DEBUGSYMBOLSSEGMENTFLAG)
	jne	webnextseg		; debug segment, ignore
	call	DisplaySegmentName	; display segment name being processed

	mov	ax,gs:[bx+WORD PTR MasterSegDefRecStruc.mssFirstIndSegPtr+2]
	mov	di,gs:[bx+WORD PTR MasterSegDefRecStruc.mssFirstIndSegPtr]

; ax:di -> individual segdef entry
webindloop:
	test	ax,ax		; see if individual segdef exists
	je	webnextseg		; no
	mov	es,ax			; es:di -> individual segdef entry

; see if data for segment
	test	es:[di+IndSegDefRecStruc.isdrFlags],ASSOCIATEDDATAFLAG
	je	webnextind		; no data from segment, no file write needed

	mov	PharLapModuleFlag,0	; init phar lap flag
	mov	OS2ModuleFlag,0	; init os/2 flag
	test	es:[di+IndSegDefRecStruc.isdrFlags],CREATEDSEGMENTFLAG
	jne	webinit			; no obj base buffer, not a clipper or phar lap module
	mov	ax,WORD PTR es:[di+IndSegDefRecStruc.isdrModulePtr]	; ax -> obj base buffer
	mov	fs,ax
	test	fs:[IOBuffHeaderStruc.ibhsFlags],ISPHARLAPMODFLAG	; see if phar lap module
	setne	PharLapModuleFlag
	test	fs:[IOBuffHeaderStruc.ibhsFlags],ISOS2MODFLAG	; see if phar lap module
	setne	OS2ModuleFlag

IFDEF SYMBOLPACK
	mov	NewLEDATAOffset,0	; init new LEDATA offset after compression
	mov	KnownClipperMod,OFF	; init clipper module flag
	test	fs:[IOBuffHeaderStruc.ibhsFlags],ISCLIPPERMODFLAG	; see if data for segment
	je	webinit			; not a clipper module
	mov	KnownClipperMod,ON
	mov	fs,fs:[IOBuffHeaderStruc.ibhsClipModPtr]	; fs -> clipper module entry
	mov	ax,fs:[ClipperModEntStruc.cmsSegDefIndex]	; get symbols segdef index
	mov	ClipperSymSegIndex,ax	; save SYMBOLS segment index

ENDIF

webinit:
	mov	PublicNameIndex,0	; init public name index for COMDAT's
	lfs	si,es:[di+IndSegDefRecStruc.isdrDataPtr]	; fs:si -> first L?DATA for segment
	call	ReadByte	; read type of data record
	mov	dh,al			; save record value
	call	ReadWordCX	; read record length into cx
	cmp	dh,COMDAT		; first record is COMDAT
	je	webcomdat
	cmp	dh,COMDAT32		; first record is COMDAT32
	je	webcomdat32
	call	ReadIndexDecCX	; read segment index
	mov	DataSegmentIndex,ax	; save it
	cmp	dh,LEDATA
	je	webisled		; first record is LEDATA
	cmp	dh,LEDATA32
	je	webisled32		; first record is LEDATA32
	cmp	dh,LIDATA
	je	webislid		; first record is LIDATA
	jmp	webislid32		; first record is LIDATA32 (assumed)

webrecloop:
	call	ReadByte	; read type of data record
	call	ReadWordCX	; read record length into cx

webchkled:
	cmp	al,LEDATA
	jne	webchkled32
	call	ReadIndexDecCX	; read segment index
	cmp	ax,DataSegmentIndex	; must match
	jne	webscan			; scan failed

webisled:
	cmp	DataSegmentIndex,-1	; see if explicit non-L?DATA segment
	je	webscan			; yes, bypass checks for L?DATA
	mov	PublicNameIndex,-1	; flag no comdats for this segment
	call	WriteLEDATABytes	; ledata record
	jmp	webscan

webchkled32:
	cmp	al,LEDATA32
	jne	webchklid
	call	ReadIndexDecCX	; read segment index
	cmp	ax,DataSegmentIndex	; must match
	jne	webscan			; scan failed

webisled32:
	cmp	DataSegmentIndex,-1	; see if explicit non-L?DATA segment
	je	webscan			; yes, bypass checks for L?DATA
	mov	PublicNameIndex,-1	; flag no comdats for this segment
	call	WriteLEDATA32Bytes	; ledata32 record
	jmp	webscan

webchklid:
	cmp	al,LIDATA
	jne	webchklid32
	call	ReadIndexDecCX	; read segment index
	cmp	ax,DataSegmentIndex	; must match
	jne	webscan			; scan failed

webislid:
	cmp	DataSegmentIndex,-1	; see if explicit non-L?DATA segment
	je	webscan			; yes, bypass checks for L?DATA
	mov	PublicNameIndex,-1	; flag no comdats for this segment
	call	WriteLIDATABytes	; lidata record
	jmp	webscan

webchklid32:
	cmp	al,LIDATA32
	jne	webchkcom
	call	ReadIndexDecCX	; read segment index
	cmp	ax,DataSegmentIndex	; must match
	jne	webscan			; scan failed

webislid32:
	cmp	DataSegmentIndex,-1	; see if explicit non-L?DATA segment
	je	webscan			; yes, bypass checks for L?DATA
	mov	PublicNameIndex,-1	; flag no comdats for this segment
	call	WriteLIDATA32Bytes	; lidata32 record
	jmp	webscan

webchkcom:
	cmp	al,COMDAT		; see if comdat record
	je	webcomdat		; yes
	cmp	al,COMDAT32		; see if comdat32 record
	je	webcomdat32		; yes

webchkmod:
	cmp	al,MODEND		; see if end record
	je	webnextind		; yes
	cmp	al,MODEND32		; see if 32-bit end record
	je	webnextind		; yes

COMMENT !
; *** debugging code ***
	cmp	al,FIXUPP
	je	webscan
	cmp	al,PUBDEF
	je	webscan
	nop					; breakpoint here
; **********************
END COMMENT !

webscan:
	call	ScanAhead
	jmp	webrecloop

webnextind:
	mov	ax,es:[di+WORD PTR IndSegDefRecStruc.isdrNextIndSegPtr+2]
	mov	di,es:[di+WORD PTR IndSegDefRecStruc.isdrNextIndSegPtr]	; ax:di -> next individaul segdef entry
	jmp	NEAR PTR webindloop

webnextseg:
	mov	ax,gs:[bx+WORD PTR MasterSegDefRecStruc.mssNextSegPtr+2]
	mov	bx,gs:[bx+WORD PTR MasterSegDefRecStruc.mssNextSegPtr]	; ax:bx -> next master segdef entry, if any
	jmp	NEAR PTR webmainloop

; comdat32 record
webcomdat32:
	cmp	PublicNameIndex,-1	; see if explicit non-COMDAT segment
	je	webscan			; yes, bypass checks for comdat
	call	ReadByteDecCX	; scan past flags
	call	ReadByteDecCX	; get past attributes
	push	ax				; save attributes
	call	ReadByteDecCX	; scan past align
	call	ReadDwordDecCX	; read enumerated data offset
	jmp	webcomshare		; jump to code shared with 16-bit comdat

; comdat record
webcomdat:
	cmp	PublicNameIndex,-1	; see if explicit non-COMDAT segment
	je	webscan			; yes, bypass checks for comdat
	call	ReadByteDecCX	; scan past flags
	call	ReadByteDecCX	; get attributes
	push	ax				; save attributes
	call	ReadByteDecCX	; scan past align
	call	ReadWordDecCX	; read enumerated data offset
	movzx	eax,ax		; convert 16-bit offset to 32-bit

webcomshare:
	mov	DataSegmentIndex,-1	; flag no L?DATA records for this segment
	pop	dx
	push	eax			; save offset
	push	dx			; save attributes
	call	ReadIndexDecCX	; scan past type index
	pop	dx				; dx == attributes
	test	dl,COMDATALLOCTYPEFIELD	; mask attributes to allocation type
	jne	webpubind			; allocation not explicit (nonzero)

; public base fields present
	call	ReadIndexDecCX	; scan past group index
	call	ReadIndexDecCX	; get segment index
	test	ax,ax			; see if segment index is zero
	jne	webpubind			; no frame number
	call	ReadWordDecCX	; scan past frame number

webpubind:
	call	ReadIndexDecCX	; get public name index
	cmp	PublicNameIndex,0	; see if any public name index currently
	jne	chkpni			; yes
	mov	PublicNameIndex,ax	; set public name index

chkpni:
	cmp	ax,PublicNameIndex	; see if this COMDAT matches desired (matching public name index)
	pop	eax				; restore offset, KEEP FLAGS
	jne	webscan			; match failed
	call	WriteCOMDATBytes	; comdat record
	jmp	webscan

webdone:

IFDEF WATCOM_ASM
	cmp	IsZeroUninitOption,OFF	; see if zero'ing uninitialize data
	je	webflush		; no
	cmp	_ENDSegDefPtr,0
	je	webflush		; no stack segment

; force zero'ing uninitialized data by writing to last byte of stack
	lfs	si,_ENDSegDefPtr
	les	di,fs:[si+MasterSegDefRecStruc.mssFirstIndSegPtr]	; es:di -> first segdef
	mov	eax,fs:[si+MasterSegDefRecStruc.mssSegLength]
	dec	eax				; init segment write offset to length-1
	push	ds
	pop	fs
	mov	si,OFFSET DGROUP:ZeroValue	; fs:si -> known zero value (dummy source buffer)
	mov	cx,1			; one byte to write
	call	WriteToIOBuffer	; write the bytes out to the i/o Buffer
ENDIF

webflush:
	call	FlushIOBuffer	; flush IO buffer of impending writes
	ret
WriteEXEBody	ENDP

;*****************************
;* WRITECOMDATBYTES          *
;*****************************

; write COMDAT bytes to EXE file, if match
; upon entry fs:si -> read buffer, es:di -> individual segdef entry.
;  cx holds record length, eax holds data offset
; updates cx,si,fs
; destroys ax,dx

WriteCOMDATBytes	PROC
	dec	cx				; adjust for checksum byte
	call	WriteToIOBuffer	; write the bytes out to the i/o Buffer
	inc	cx				; restore checksum byte to length
	ret
WriteCOMDATBytes	ENDP

;*****************************
;* WRITELEDATABYTES          *
;*****************************

; write LEDATA bytes to EXE file
; upon entry fs:si -> read buffer, es:di -> individual segdef entry.
;  cx holds record length
; updates cx,si,fs
; destroys ax,dx

WriteLEDATABytes	PROC
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	jne	WriteLEDATA32Bytes	; phar lap flag, process as LEDATA32
	call	ReadWordDecCX	; read enumerated data offset
	dec	cx				; adjust for checksum byte
	movzx	eax,ax		; convert offset to 32-bit value

IFDEF SYMBOLPACK
	push	fs			; save register states as compression may modify them
	push	si
	push	cx
	cmp	KnownClipperMod,OFF	; see if clipper module
	je	wled2			; no

; Clipper module
; check if Clipper code (LEDATA segment index < SYMBOLS segment index) OR
; if SYMBOLS data (LEDATA segment index == SYMBOLS segment index)
	mov	dx,DataSegmentIndex
	cmp	dx,ClipperSymSegIndex
	ja	wled2
	je	wledsym

; clipper code
	call	FixupClipperTokens	; fixup tokens in Clipper code to new symbol positions
	jmp	wled2

; symbols data
wledsym:
	call	ProcessSymbolTable	; process symbol table data for compression

wled2:
ENDIF

	call	WriteToIOBuffer	; write the bytes out to the i/o Buffer

IFDEF SYMBOLPACK
	pop	cx				; restore registers to pre-compression states, even if no compression
	pop	si
	pop	fs
ENDIF

	inc	cx				; restore checksum byte to length
	ret
WriteLEDATABytes	ENDP

;*****************************
;* WRITELEDATA32BYTES        *
;*****************************

; write LEDATA32 bytes to EXE file
; upon entry fs:si -> read buffer, es:di -> individual segdef entry.
;  cx holds record length
; updates cx,si,fs
; destroys ax,dx

WriteLEDATA32Bytes	PROC
	call	ReadDwordDecCX	; read enumerated data offset (32-bit)
	dec	cx				; adjust for checksum byte
	call	WriteToIOBuffer	; write the bytes out to the i/o Buffer
	inc	cx				; restore checksum byte to length
	ret
WriteLEDATA32Bytes	ENDP

;*****************************
;* WRITELIDATABYTES          *
;*****************************

; write LIDATA bytes to EXE file
; upon entry fs:si -> read buffer, es:di -> individual segdef entry.
;  cx holds record length
; updates cx,si,fs
; maintain bx,di,ds,es,gs
; destroys ax,dx

WriteLIDATABytes	PROC
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	jne	WriteLIDATA32Bytes	; phar lap flag, process as LIDATA32
	call	ReadWordDecCX	; read iterated data offset
	movzx	eax,ax
	mov	DataOffset32,eax	; save it
	dec	cx				; adjust for checksum byte

wlidloop:
	jcxz	wlidret		; no more bytes to write
	call	RecurseLIDATA	; use recursive routine to extract data out of lidata record
	jmp	wlidloop

wlidret:
	inc	cx				; restore checksum byte to length
	ret
WriteLIDATABytes	ENDP

;*****************************
;* RECURSELIDATA             *
;*****************************

; use recursion to get all data out of LIDATA record
; upon entry fs:si -> current data byte, cx == size of data block,
; destroys ax,dx

RecurseLIDATA  PROC
	call	ReadWordDecCx	; get repeat count
	mov	RepeatCount,ax	; save it
	call	ReadWordDecCx	; get block count
	mov	BlockCount,ax	; save it
	push	si			; save buffer position
	push	fs			; 10/01/99, save segment in case of buffer block spanning
						;  (this should be added to other L?DATA recursion routines)
	push	cx			; save record iterated data block length

rlidreploop:
	pop	cx				; restore record length
	pop	fs
	pop	si				; restore buffer position
	push	si			; put values back on stack
	push	fs
	push	cx

	cmp	BlockCount,0	; see if nested iterated data blocks
	je	rliddata		; no

; nested iterated data blocks
	mov	ax,BlockCount	; get number of blocks to loop through

rlidblkloop:
	push	ax			; save current number of blocks left to loop through
	push	RepeatCount	; save repeat value
	push	BlockCount	; save block value
	call	RecurseLIDATA	; nest down one level
	pop	BlockCount		; save block value
	pop	RepeatCount		; restore repeat value
	pop	ax				; restore number of blocks left to loop through
	dec	ax				; one iteration complete
	jne	rlidblkloop		; loop not complete

; block loop is complete, do next iteration of repeat loop
rlidnextrep:
	dec	RepeatCount
	jne	rlidreploop		; more repeat iterations to do

;	add	sp,4			; trash old record length and record position values on stack
	add	sp,6			; trash old record length and record position values on stack

	ret					; ; repeat loop complete, return to next highest level or calling procedure

; block size is zero, data follows
rliddata:
	call	ReadByteDecCx	; get length of data to write

; write the data bytes
	push	cx			; save record's total data length
	mov	cl,al
	xor	ch,ch			; data bytes to write in cx
	push	cx			; save data bytes written
	mov	eax,DataOffset32	; data offset in ax
;	movzx	eax,ax		; convert to 32-bit value
	call	WriteToIOBuffer	; write the bytes out to the i/o Buffer
	pop	ax				; get data bytes written
	movzx	eax,ax		; convert to 32-bit
	add	DataOffset32,eax	; update data offset with bytes written
	mov	cx,ax			; bytes written count to cx
	call	ScanAhead	; move source buffer ahead bytes written
	mov	ax,cx			; bytes written count to ax
	pop	cx				; restore record's total data length
	sub	cx,ax			; subtract off data bytes written
	jmp	rlidnextrep

RecurseLIDATA  ENDP

;*****************************
;* WRITELIDATA32BYTES        *
;*****************************

; write LIDATA32 bytes to EXE file
; upon entry fs:si -> read buffer, es:di -> individual segdef entry.
;  cx holds record length
; updates cx,si,fs
; maintain bx,di,ds,es,gs
; destroys eax,dx

WriteLIDATA32Bytes	PROC
	call	ReadDwordDecCX	; read iterated data offset
	mov	DataOffset32,eax	; save it
	dec	cx				; adjust for checksum byte

wlid3loop:
	jcxz	wlid3ret	; no more bytes to write
	call	RecurseLIDATA32	; use recursive routine to extract data out of lidata record
	jmp	wlid3loop

wlid3ret:
	inc	cx				; restore checksum byte to length
	ret
WriteLIDATA32Bytes	ENDP

;*****************************
;* RECURSELIDATA32           *
;*****************************

; use recursion to get all data out of LIDATA32 record
; upon entry fs:si -> current data byte, cx == size of data block,
; destroys eax,dx

RecurseLIDATA32  PROC
	cmp	OS2ModuleFlag,OFF	; see if processing os/2 flagged module
	jne	rlid3rep		; yes, don't do special phar lap exception
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	jne	rlid3phar		; yes, repeat count is only 16-bit

rlid3rep:
	call	ReadDwordDecCx	; get repeat count

rlid3scount:
	mov	RepeatCount32,eax	; save it
	call	ReadWordDecCx	; get block count
	mov	BlockCount,ax	; save it
	push	si			; save buffer position
	push	cx			; save record iterated data block length

rlid3reploop:
	pop	cx				; restore record length
	pop	si				; restore buffer position
	push	si			; put values back on stack
	push	cx

	cmp	BlockCount,0	; see if nested iterated data blocks
	je	rlid3data		; no

; nested iterated data blocks
	mov	ax,BlockCount	; get number of blocks to loop through

rlid3blkloop:
	push	ax			; save current number of blocks left to loop through
	push	RepeatCount32	; save repeat value
	push	BlockCount	; save block value
	call	RecurseLIDATA32	; nest down one level
	pop	BlockCount		; save block value
	pop	RepeatCount32	; restore repeat value
	pop	ax				; restore number of blocks left to loop through
	dec	ax				; one iteration complete
	jne	rlid3blkloop		; loop not complete

; block loop is complete, do next iteration of repeat loop
rlid3nextrep:
	dec	RepeatCount32
	jne	rlid3reploop		; more repeat iterations to do

	add	sp,4			; trash old record length and record position values on stack
	ret					; ; repeat loop complete, return to next highest level or calling procedure

; block size is zero, data follows
rlid3data:
	call	ReadByteDecCx	; get length of data to write

; write the data bytes
	push	cx			; save record's total data length
	mov	cl,al
	xor	ch,ch			; data bytes to write in cx
	push	cx			; save data bytes written
	mov	eax,DataOffset32	; data offset in eax
	call	WriteToIOBuffer	; write the bytes out to the i/o Buffer
	pop	ax				; get data bytes written
	movzx	eax,ax		; convert to 32-bit register
	add	DataOffset32,eax	; update data offset with bytes written
	mov	cx,ax			; bytes written count to cx
	call	ScanAhead	; move source buffer ahead bytes written
	mov	ax,cx			; bytes written count to ax
	pop	cx				; restore record's total data length
	sub	cx,ax			; subtract off data bytes written
	jmp	rlid3nextrep

; phar lap module, 16-bit repeat count
rlid3phar:
	call	ReadWordDecCx	; get repeat count
	movzx	eax,ax		; extend to 32-bit for normal processing
	jmp	rlid3scount

RecurseLIDATA32  ENDP

ENDS

END
