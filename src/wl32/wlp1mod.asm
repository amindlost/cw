;*********************************************************************
;*   WLP1MOD.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/11/97                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     TASM 2.5                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   pass 1 object module processing routines                        *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLP1MOD
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;DEBUG	EQU	1

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
PUBLIC	OBJPass1
PUBLIC	Process1OBJRecord

; public for debugger
PUBLIC	Pass1AliasProc,COMENTClassPharLapProc
PUBLIC	LibModuleName,ModuleName

IFDEF DLLSUPPORT
PUBLIC	Pass1IMPDEFProc
ENDIF

; variables
PUBLIC	ACBPByte,Is32BitSeg,IsAbsoluteSeg
PUBLIC	BAKPATTableSel
PUBLIC	COMDATFlags
PUBLIC	CurrentLNAMESIndex
PUBLIC	CurrentOBJ
PUBLIC	CurrentSegDefCount,CurrentGrpDefCount,CurrentFixSymCount
PUBLIC	CurrentBAKPATCount,CurrentLEDATACount
PUBLIC	DefaultLIBAddFlag
PUBLIC	EndOfOBJFlag
PUBLIC	FlatModuleFlag
PUBLIC	GroupNameIndex
PUBLIC	GrpDefSegmentIndex
PUBLIC	IsLocalSymbol
PUBLIC	InText
PUBLIC	LEDATATableSel
PUBLIC	LNAMESIndexSel
PUBLIC	ModuleCount
PUBLIC	OBJRecTable
PUBLIC	ProcessModText
PUBLIC	PharLapModuleFlag
PUBLIC	SearchExistSymFlag
PUBLIC	SegFrameNumber
PUBLIC	SegmentNameIndex,ClassNameIndex
PUBLIC	SegmentLength

IFDEF SYMBOLPACK
PUBLIC	ClipperSymSegIndex
PUBLIC	CompressionInForce
PUBLIC	CurrentSymbolSegPtr
PUBLIC	IsCompressedSegment
PUBLIC	KnownClipperMod
PUBLIC	MayBeClipperMod
PUBLIC	NeedCompSymProcess
PUBLIC	SymbolTableOverflow
ENDIF

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

ACBPByte	DB	?		; current SEGDEF ACBP byte
BAKPATLocType	DB	?	; BAKPAT location type
COMDATAlign	DB	?		; comdat alignment
COMDATFlags	DB	?		; comdat record flags
COMDATAttributes	DB	?	; comdat attributes
CommunalDataType	DB	?	; communal variable data type
DefaultLIBAddFlag	DB	?	; nonzero if adding default library to list
EndOfOBJFlag	DB	?	; nonzero if end of OBJ has been encountered
FlatModuleFlag	DB	?	; nonzero if flat module
Is32BitSeg	DB	?		; nonzero if 32-bit segment
IsAbsoluteSeg	DB	?	; nonzero if absolute segment being parsed
IsLocalSymbol	DB	?	; nonzero if symbol is local
PharLapModuleFlag	DB	?	; nonzero if special Phar Lap flag set for module
ProcessingLIBFlag	DB	?	; nonzero if processing library
SearchExistSymFlag	DB	?	; nonzero if search only for existing symbol when doing symbol lookups

ClassNameIndex	DW	?	; current class name index
COMDATSegIndex	DW	?	; comdat segment index
CurrentBAKPATCount	DW	?	; count of current bakpat
CurrentFixSymCount	DW	?	; count of current fixupp referenceable symbol (extdef/comdef & local variants)
CurrentGrpDefCount	DW	?	; count of current grpdef
CurrentLEDATACount	DD	?	; count of current LEDATA
CurrentOBJ	DW	?		; count of current object module
CurrentSegDefCount	DW	?	; count of current segdef
GroupNameIndex	DW	?	; current group name index
GrpDefSegmentIndex	DW	?	; current group segment index
SegFrameNumber	DW	?	; absolute segment frame number
SegmentNameIndex	DW	?	; current segment name index, also used as segment index in LEDATA/BAKPAT
SymbolFrame	DW	?		; frame for absolute public symbole
SymbolGroupIndex	DW	?	; public symbol group index
SymbolSegIndex	DW	?	; public symbol segment index

COMDATOffset	DD	?	; comdat offset
ModuleCount	DD	?		; count of modules parsed
SegmentLength	DD	?	; length of current segment
SymbolOffset	DD	?	; pubdef symbol offset

ModuleName	DB	128 DUP (?)	; name from T-module name field of THEADR or LHEADR record
LIBModuleName	DB	128 DUP (?)	; library module name from COMENT record

IFDEF SYMBOLPACK
IsCompressedSegment	DB	?	; nonzero if compressed symbol segment
KnownClipperMod	DB	?	; nonzero if known Clipper module
MayBeClipperMod	DB	?	; nonzero if module may be Clipper-compiled
NeedCompSymProcess	DB	?	; nonzero if need to process compressed symbol segment

ClipperSymSegIndex	DW	?	; SYMBOLS segment index

CurrentSymbolSegPtr	DD	?	; pointer to current SYMBOLS segment entry
ENDIF

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

OBJRecTableLen	DW	OBJRecTableStop-OBJRecTable
OBJRecTable		=	$
	DB	LEDATA
	DB	LEDATA32
	DB	FIXUPP
	DB	FIXUPP32
	DB	EXTDEF
	DB	PUBDEF
	DB	PUBDEF32
	DB	LEXTDEF1
	DB	LEXTDEF2
	DB	LPUBDEF
	DB	LPUBDEF32
	DB	SEGDEF
	DB	SEGDEF32
	DB	COMDEF
	DB	LCOMDEF
	DB	LIDATA
	DB	LIDATA32
	DB	LNAMES
	DB	COMENT
	DB	CEXTDEF
	DB	COMDAT
	DB	COMDAT32
	DB	BAKPAT
	DB	BAKPAT32
	DB	NBKPAT
	DB	NBKPAT32
	DB	GRPDEF
	DB	THEADR
	DB	LHEADR
	DB	MODEND
	DB	MODEND32
	DB	LINNUM
	DB	LINNUM32
	DB	LINSYM
	DB	LINSYM32
; unsupported record types
	DB	ALIAS
; ignored record types
	DB	TYPDEF
	DB	BLKDEF
	DB	BLKEND
	DB	DEBSYM
	DB	LOCSYM
; library linked as object module
	DB	MSLIBR
OBJRecTableStop	=	$

; MUST be in sync with OBJRecTable
; routine vectors for object record types
Pass1OBJRecVector	=	$
	DW	Pass1LEDATAProc
	DW	Pass1LEDATA32Proc
	DW	Pass1FIXUPPProc
	DW	Pass1FIXUPP32Proc
	DW	Pass1EXTDEFProc
	DW	Pass1PUBDEFProc
	DW	Pass1PUBDEF32Proc
	DW	Pass1LEXTDEFProc	; LEXTDEF1 and LEXTDEF2 are syntactically identical
	DW	Pass1LEXTDEFProc
	DW	Pass1LPUBDEFProc
	DW	Pass1LPUBDEF32Proc
	DW	Pass1SEGDEFProc
	DW	Pass1SEGDEF32Proc
	DW	Pass1COMDEFProc
	DW	Pass1LCOMDEFProc
	DW	Pass1LIDATAProc
	DW	Pass1LIDATA32Proc
	DW	Pass1LNAMESProc
	DW	Pass1COMENTProc
	DW	Pass1CEXTDEFProc
	DW	Pass1COMDATProc
	DW	Pass1COMDAT32Proc
	DW	Pass1BAKPATProc
	DW	Pass1BAKPAT32Proc
	DW	Pass1NBKPATProc
	DW	Pass1NBKPAT32Proc
	DW	Pass1GRPDEFProc
	DW	Pass1THEADRProc
	DW	Pass1LHEADRProc
	DW	Pass1MODENDProc
	DW	Pass1MODEND32Proc
	DW	Pass1LINNUMProc
	DW	Pass1LINNUM32Proc
	DW	Pass1LINSYMProc
	DW	Pass1LINSYM32Proc
; unsupported record types
	DW	Pass1AliasProc
; ignored record types
	DW	Pass1IgnoreOBJRecord
	DW	Pass1IgnoreOBJRecord
	DW	Pass1IgnoreOBJRecord
	DW	Pass1IgnoreOBJRecord
	DW	Pass1IgnoreOBJRecord
; library linked as object module
	DW	Pass1MSLIBRProc

COMENTClassTableLen	DW	COMENTClassTableStop-COMENTClassTable
COMENTClassTable		=	$
	DB	COMENTLIBSEARCH1FLAG
	DB	COMENTLIBSEARCH2FLAG
	DB	COMENTDOSSEGFLAG
	DB	COMENTLINKPASSFLAG
	DB	COMENTLIBMODFLAG
	DB	COMENTEXESTRFLAG
	DB	COMENTWKEXTFLAG
	DB	COMENTLZEXTFLAG
	DB	COMENTPHARLAPFLAG

IFDEF	DLLSUPPORT
	DB	COMENTOMFEXT
ENDIF

COMENTClassTableStop	=	$

; MUST be in sync with COMENTClassTable
; routine vectors for COMENT record types
COMENTClassVector	=	$
	DW	COMENTClassLibSearchProc
	DW	COMENTClassLibSearchProc
	DW	COMENTClassDOSSEGProc
	DW	COMENTClassLinkPassProc
	DW	COMENTClassLibModProc
	DW	COMENTClassEXESTRProc
	DW	COMENTClassWKEXTProc
	DW	COMENTClassLZEXTProc
	DW	COMENTClassPharLapProc

IFDEF	DLLSUPPORT
	DW	COMENTClassOMFEXTProc
ENDIF

ZeroValue	DD	0	; known zero value, for pointers to null strings

ProcessModTextLen	DB	ProcessModTextStop-ProcessModText
ProcessModText	DB	CR,LF,'*** Processing library module: '
ProcessModTextStop		=	$

InTextLen	DB	InTextStop-InText
InText	DB	' in '
InTextStop		=	$

IFDEF SYMBOLPACK
Clipper5Text	DB	'CLIPPER5'
Clipper5Text2	DB	7,'C50R100'
SYMBOLSText		DB	7,'SYMBOLS'
;__PLANKTONText	DB	10,'__PLANKTON'
ENDIF

IFDEF DEBUG
CRLFText		DB	CR,LF
ENDIF

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

; globals
PublicSymbolCount	DD	0	; count of public symbols in program
ResolvedSymbolCount	DD	0	; count of resolved symbols in program

BAKPATTableSel	DW	0	; selector of block containing BAKPAT pointers
CurrentLNAMESIndex	DW	0	; current index of LNAMES
LEDATATableSel	DW	0	; selector of block containing LEDATA pointers
LNAMESIndexSel	DW	0	; selector of block containing LNAMES pointers
MaxBAKPATTableAlloc	DW	0	; maximum allocate for BAKPAT pointer block
MaximumLNAMESAlloc	DW	0	; maximum allocation for LNAMES block

LastCOMDATModule	DD	0	; last module number containing comdat
FirstModCOMDATPtr	DD	0	; pointer to first comdat entry of module
MaxLEDATATableAlloc	DD	0	; maximum allocate for LEDATA pointer block
TotalCommunalCount	DD	0	; total count of communal variables

IFDEF DLLSUPPORT
TotalEXPDEFCount	DD	0	; total count of EXPDEFs
TotalIMPDEFCount	DD	0	; total count of IMPDEFs
ENDIF

IFDEF SYMBOLPACK
SymbolTableOverflow	DB	0	; nonzero if symbol table overflowed (>64K)
CompressionInForce	DB	0	; nonzer if clipper compression is in force (any Clipper module present)
ENDIF

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	CompBuffSource:BYTE
EXTRN	CurrentBaseOBJBuff:WORD
EXTRN	CurrentFileName:BYTE
EXTRN	FileListFlag:BYTE
EXTRN	FreeFormatFlag:BYTE
EXTRN	GrpDefNamePtr:DWORD
EXTRN	LIBAtFrontFlag:BYTE
EXTRN	OBJRecPtr:DWORD

IFDEF SYMBOLPACK
EXTRN	CurrentModSymCount:WORD
EXTRN	DataRecordOffset:WORD
EXTRN	StartSymbolCount:WORD
EXTRN	UniqueSymbolCount:WORD
ENDIF

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	AllocateMemory:PROC,ResizeMemory:PROC,ReleaseMemory:PROC
EXTRN	Alloc16KZeroBlock:PROC,Alloc64KIOBlock:PROC
EXTRN	BadOBJModuleExit:PROC,LinkerErrorExit:PROC
EXTRN	ConvertToUpperCase:PROC
EXTRN	DisplayShortString:PROC
EXTRN	DisplayTextStringNoCRLF:PROC
EXTRN	DisplayVarStringNoCRLF:PROC
EXTRN	GetComDatEntry:PROC
EXTRN	GetPubSymEntry:PROC
EXTRN	MultipleDefSymWarn:PROC
EXTRN	NormalESDIDest:PROC
EXTRN	OpenCurrentOBJ:PROC,OpenCurrentLIB:PROC
EXTRN	ProcessBAKPATRecords:PROC
EXTRN	ReadByte:PROC,ReadByteDecCX:PROC,ReadWord:PROC,ReadWordCX:PROC
EXTRN	ReadDword:PROC,ReadDwordDecCx:PROC,ReadDwordECX:PROC
EXTRN	ReadFile:PROC
EXTRN	ReadIndex:PROC,ReadIndexDecCX:PROC
EXTRN	ReadNameString:PROC
EXTRN	ReadWordDecCX:PROC
EXTRN	ResizeMemory32:PROC
EXTRN	ScanAhead:PROC
EXTRN	SetOBJBuffer:PROC

EXTRN	GetGrpDefEntry:PROC,AssignSegToGroup:PROC
EXTRN	GetMasterSegDefEntry:PROC
EXTRN	MakeIndSegDefEntry:PROC
EXTRN	NormalGSBXSource:PROC
EXTRN	SaveOBJLIBFileName:PROC
EXTRN	SetGrpPtrTableEntry:PROC
EXTRN	SetSymPtrTableEntry:PROC

IFDEF	DLLSUPPORT
EXTRN	GetEXPDEFEntry:PROC
EXTRN	GetIMPDEFEntry:PROC
ENDIF

IFDEF SYMBOLPACK
EXTRN	CompressedSymbolFixup:PROC
EXTRN	CreateClipperModEntry:PROC
EXTRN	ParseClipperSymbols:PROC
EXTRN	ProcessCompressedSymbols:PROC
ENDIF

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* OBJPASS1                  *
;*****************************

; pass 1 processing of object modules

OBJPass1	PROC
	mov	ProcessingLIBFlag,0
	mov	eax,1
	mov	ModuleCount,eax	; init module count
	mov	CurrentOBJ,ax	; init current object module number
	call	Alloc16KZeroBlock	; allocate and zero 16K block
	call	Alloc64KIOBlock	; allocate 64K byte block for i/o

objloop:
	push	ds
	pop	es				; es -> wl32 data
	call	OpenCurrentOBJ
	call	SetOBJBuffer	; setup for reading current OBJ module from buffer

	xor	eax,eax
	mov	EndOfOBJFlag,al	; init end of object module flag
	mov	PharLapModuleFlag,al	; init special Phar Lap flag
	mov	FlatModuleFlag,al	; init flat module flag
	mov	CurrentLNAMESIndex,ax	; init object record indices
	mov	CurrentSegDefCount,ax	; init module-level counters
	mov	CurrentGrpDefCount,ax
	mov	CurrentFixSymCount,ax
	mov	CurrentBAKPATCount,ax
	mov	CurrentLEDATACount,eax

IFDEF SYMBOLPACK
	mov	IsCompressedSegment,al
	mov	MayBeClipperMod,al
	mov	KnownClipperMod,al
	mov	NeedCompSymProcess,al
	mov	CurrentModSymCount,ax
	mov	ax,UniqueSymbolCount
	mov	StartSymbolCount,ax
ENDIF

recloop:
	call	Process1OBJRecord	; get current OBJ record, process it pass 1
	cmp	EndOfOBJFlag,OFF	; see if at end of OBJ
	je	recloop			; not at end

;multiple modules in one file check
;@@@ code goes here

	call	ProcessBAKPATRecords

	inc	CurrentOBJ		; bump count of current object module
	inc	ModuleCount		; bump count of modules parsed
	mov	ax,CurrentOBJ	; get new current object module
	cmp	ax,TotalOBJCount	; see if parsed all object modules
	jbe	objloop
	ret
OBJPass1	ENDP

;*****************************
;* PROCESS1OBJRECORD         *
;*****************************

; get and do pass 1 processing of object record

Process1OBJRecord	PROC
	mov	WORD PTR OBJRecPtr,si	; save pointer to record
	mov	WORD PTR OBJRecPtr+2,fs
	call	ReadByte	; read byte of current file into al register (record type)
	cmp	al,LOWESTOBJRECTYPE	; do boundary checks on valid record types
	jb	badrec
	cmp	al,HIGHESTOBJRECTYPE
	ja	badrec
	call	ReadWordCX	; read word of current file into cx register (record length)


; MED 12/11/97, increase valid size for huge Clarion program
;	cmp	cx,4000h		; object records can't be >16K (never will normally, purely a blown file check)
	cmp	cx,5000h		; object records can't be >20K (never will normally, purely a blown file check)

	jae	badlen
	push	fs			; save -> current read position in fs:si
	push	si
	push	cx			; save record length
	mov	di,OFFSET DGROUP:OBJRecTable	; es:di -> lookup table for object record types
	mov	cx,ds:[di-2]	; get number of entries in table
	repne	scasb
	jne	badrec			; record not found in table

	dec	di				; di -> matching entry
	sub	di,OFFSET DGROUP:OBJRecTable	; di == object record type offset
	add	di,di			; word offset
	add	di,OFFSET DGROUP:Pass1OBJRecVector	; di -> entry in OBJRecVector table
	pop	cx				; restore object record length to cx
	push	cx			; save back for later use
	call	ds:[di]		; transfer to appropriate routine
	pop	cx				; restore object record length to cx
	pop	si				; restore old current read position in fs:si
	pop	fs
	call	ScanAhead	; scan past processed bytes to next record, if any
	ret

; record length >=20K
;; record length >=16K
badlen:
	mov	al,BADOBJRECLENERRORCODE

lename:
	mov	dx,OFFSET DGROUP:CurrentFileName
	call	LinkerErrorExit	; no return

; al holds bad record value
badrec:
	call	Pass1BadOBJRecord	; no return

Process1OBJRecord	ENDP

;*****************************
;* PASS1BADOBJRECORD         *
;*****************************

; pass 1 process, unsupported object record
; upon entry al == record value

Pass1BadOBJRecord	PROC
	mov	cl,al
	xor	ch,ch			; cx holds bad value
	mov	al,BADOBJRECERRORCODE
	jmp	lename	; linker error exit with current file name
Pass1BadOBJRecord	ENDP

;*****************************
;* PASS1UNSUPOBJRECORD       *
;*****************************

; pass 1 process, bad object record
; upon entry al == record value

Pass1UnsupOBJRecord	PROC
	mov	cl,al
	xor	ch,ch			; cx holds unsupported value
	mov	al,UNSUPOBJRECERRORCODE
	jmp	lename	; linker error exit with current file name
Pass1UnsupOBJRecord	ENDP

;*****************************
;* PASS1IGNOREOBJRECORD      *
;*****************************

; ignore object record without error

Pass1IgnoreOBJRecord	PROC

;@@@ code goes here

	ret
Pass1IgnoreOBJRecord	ENDP

;*****************************
;* PASS1LEDATAPROC           *
;*****************************

Pass1LEDATAProc	PROC
	call	ReadIndexDecCX	; read segment index
	mov	SegmentNameIndex,ax	; save index

IFDEF SYMBOLPACK
	push	fs
	push	si				; save position -> data offset
	push	cx
ENDIF

	call	ReadWordDecCX	; read data offset
	movzx	eax,ax			; zero extend to 32-bit for saving
	call	SaveLEDATADataPtr	; save pointer to LEDATA in case of BAKPAT's

IFDEF SYMBOLPACK
	pop	cx					; restore position -> data offset
	pop	si
	pop	fs
	mov	IsCompressedSegment,OFF	; flag not compressed segment
	mov	ax,SegmentNameIndex	; get segment index
	cmp	ax,ClipperSymSegIndex	; see if matches SYMBOLS segment index
	jne	p1ledret			; no
	cmp	KnownClipperMod,OFF	; see if known clipper module
	je	p1ledret			; no
	cmp	SymbolTableOverflow,OFF	; see if symbol table overflowed
	jne	p1ledret			; yes
	call	ParseClipperSymbols
ENDIF

p1ledret:
	ret					; no pass 1 processing of LEDATA records
Pass1LEDATAProc	ENDP

;*****************************
;* PASS1LEDATA32PROC         *
;*****************************

Pass1LEDATA32Proc	PROC
	call	ReadIndexDecCX	; read segment index
	mov	SegmentNameIndex,ax	; save index

	call	ReadDwordDecCX	; read data offset
	call	SaveLEDATADataPtr	; save pointer to LEDATA in case of BAKPAT's

	ret					; no pass 1 processing of LEDATA32 records
Pass1LEDATA32Proc	ENDP

;*****************************
;* SAVELEDATADATAPTR         *
;*****************************

; save pointer to LEDATA info for any later BAKPAT records
; upon entry eax==data offset, cx==length of data+1,
;  SegmentNameIndex holds LEDATA segment index
; conserve cx,si,ds,es,fs

SaveLEDATADataPtr	PROC
	push	eax			; save data offset
	cmp	LEDATATableSel,0	; see if selector allocated yet
	je	sldalloc		; not yet, do it

sldloop:
	mov	eax,CurrentLEDATACount
	shl	eax,4			; total size of table, para per entry
	mov	edi,eax
	cmp	eax,MaxLEDATATableAlloc
	jae	sldsize			; larger than current allocation, resize it

sld2:
	mov	gs,LEDATATableSel	; gs -> LEDATA table of pointers block
	mov	ax,SegmentNameIndex
	mov	gs:[edi+LEDATADataPtrStruc.ldpsSegmentIndex],ax
	pop	eax				; get starting data offset
	mov	gs:[edi+LEDATADataPtrStruc.ldpsDataStart],eax
	movzx	edx,cx		; get data byte length+checksum byte
	add	eax,edx			; compute data end
	sub	eax,2			; adjust for checksum byte, make relative 0
	mov	gs:[edi+LEDATADataPtrStruc.ldpsDataEnd],eax	; save ending data offset
	mov	gs:[edi+LEDATADataPtrStruc.ldpsRecOffset],si	; keep offset pointer to LEDATA data
	mov	gs:[edi+LEDATADataPtrStruc.ldpsRecSel],fs	; keep selector pointer

	inc	CurrentLEDATACount	; bump count of LEDATA's/table entry
	ret

; allocate selector for LEDATA data pointers
sldalloc:
	mov	edx,4096		; allocate in 4096-byte chunks
	mov	MaxLEDATATableAlloc,edx	; save allocation amount
	call	AllocateMemory
	mov	LEDATATableSel,ax
	jmp	sldloop

; resize selector to next 4096-byte boundary
sldsize:
	mov	eax,MaxLEDATATableAlloc
	add	eax,4096
	mov	MaxLEDATATableAlloc,eax
	mov	edx,eax
	mov	ax,LEDATATableSel
	call	ResizeMemory32
	jmp	sld2
SaveLEDATADataPtr	ENDP

;*****************************
;* PASS1FIXUPPPROC           *
;*****************************

Pass1FIXUPPProc	PROC

IFDEF SYMBOLPACK
	cmp	IsCompressedSegment,OFF	; see if compressed SYMBOLS segment
	je	p1fret			; no
	call	ReadByteDecCX	; read thread data field/low locat field byte
	mov	dl,al			; save locat low byte
	test	al,FIXUPSUBRECORD	; see if fixup or thread subrecord
	je	p1fret			; thread field, ignore
	call	ReadByteDecCX	; get locat high byte
	and	dl,3			; get two MSB of data record offset
	mov	ah,dl
	mov	DataRecordOffset,ax	; save data record offset
	call	CompressedSymbolFixup

p1fret:
ENDIF

	ret
Pass1FIXUPPProc	ENDP

;*****************************
;* PASS1FIXUPP32PROC         *
;*****************************

Pass1FIXUPP32Proc	PROC
	ret					; no pass 1 processing of FIXUPP32 records
Pass1FIXUPP32Proc	ENDP

;*****************************
;* PASS1EXTDEFPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1EXTDEFProc	PROC
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	dec	cx				; adjust for checksum byte

; fs:si -> symbol name with length byte prepended
p1enameloop:
	or	cx,cx
	je	p1eret			; done

IFDEF SYMBOLPACK
	cmp	MayBeClipperMod,OFF	; see if may be Clipper module
	je	p1esearch		; no
	cmp	KnownClipperMod,OFF	; see if already known Clipper module
	jne	p1esearch		; yes
	cmp	SymbolTableOverflow,OFF	; see if symbol table overflowed
	jne	p1esearch		; yes

; check if name is clipper 5 match
; if so, set KnownClipperMod variable
	push	es			; save critical registers
	push	cx
	push	si

	push	fs
	pop	es
	mov	di,si			; es:di -> name
	call	NormalESDIDest	; normalize es:di to destination name buffer
	inc	di				; scan past length byte, only checking up to eight bytes
	mov	si,OFFSET DGROUP:Clipper5Text
	mov	cx,2			; 8 bytes, 2 dwords
	repz	cmpsd		; compare name to Clipper 5 name
	je	p1eisclip5		; match

	push	fs
	pop	es
	pop	di
	push	di
	call	NormalESDIDest	; normalize es:di to destination name buffer
	mov	si,OFFSET DGROUP:Clipper5Text2
	mov	cx,2			; 8 bytes, 2 dwords
	repz	cmpsd		; compare name to Clipper 5 name
;@@@	je	p1eisclip5		; match

;@@@	push	fs
;@@@	pop	es
;@@@	pop	di
;@@@	push	di
;@@@	call	NormalESDIDest	; normalize es:di to destination name buffer
;@@@	mov	si,OFFSET DGROUP:__PLANKTONText
;@@@	mov	cx,2			; 11 bytes, 2 dwords+3
;@@@	repz	cmpsd		; compare name to Clipper 5 name
;@@@	jne	p1echkdone		; no match
;@@@	mov	cx,3			; 3 leftover bytes
;@@@	repz	cmpsb		; compare name to __PLANKTON dispatch
	jne	p1echkdone		; no match

p1eisclip5:
	mov	KnownClipperMod,ON	; known clipper module
	mov	CompressionInForce,ON	; compression is in force for this application
	call	CreateClipperModEntry

p1echkdone:
	pop	si				; restore critical registers
	pop	cx
	pop	es

p1esearch:
ENDIF

	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	jnc	p1e2			; previous entry

; new public symbol entry
	mov	eax,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax
	inc	PublicSymbolCount	; bump count of unique public symbols
	jmp	p1e3

; see if this symbol was previously a public or an external
; if a public then continue processing,
; if still an external, then save current module count and shut off
;   any weak or lazy flags (they may be turned on later via module count
;   compare in weak and lazy external routines)
p1e2:
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	p1e3
	mov	eax,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax

; MED 10/24/97
IFNDEF NATHAN
	and	gs:[di+PubSymRecStruc.pssFlags],NOT (WEAKEXTERNFLAG OR LAZYEXTERNFLAG)
ENDIF

; save pointer to extdef in table for later fixupp use
p1e3:
	call	SetSymPtrTableEntry
	inc	CurrentFixSymCount	; bump count of fixupp-referenceable symbols in module
	call	ReadByteDecCx	; get symbol name length
	mov	dx,cx			; save record length
	mov	cl,al
	xor	ch,ch			; cx holds name length (bytes to scan past)
	call	ScanAhead
	sub	dx,cx
	mov	cx,dx			; cx holds updated record length
	call ReadIndexDecCx	; read type index, ignore
	jmp	p1enameloop

p1eret:
	ret
Pass1EXTDEFProc	ENDP

;*****************************
;* PASS1PUBDEFPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1PUBDEFProc	PROC
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	jne	Pass1PUBDEF32Proc	; yes, PUBDEF's are processed as PUBDEF32's

	mov	IsLocalSymbol,OFF	; flag not a local symbol
	dec	cx				; adjust for checksum byte
	call	ReadIndexDecCX	; read group index
	mov	SymbolGroupIndex,ax
	call	ReadIndexDecCX	; read segment index
	mov	SymbolSegIndex,ax
	test	ax,ax		; see if segment index zero, frame exists
	jne	p1p2			; no frame number
	call	ReadWordDecCX	; get frame number
	mov	SymbolFrame,ax

; fs:si -> symbol name with length byte prepended
p1p2:
	push	cx			; save remaining length of record
	push	fs			; save pointer to name
	push	si
	call	ReadByte	; get length of name
	mov	cl,al
	xor	ch,ch			; cx holds bytes to scan past for name
	call	ScanAhead	; move fs:si -> public offset
	call	ReadWord
	movzx	eax,ax
	mov	SymbolOffset,eax
	pop	si				; restore fs:si -> symbol name
	pop	fs
	push	fs			; save fs:si -> symbol name back to stack
	push	si
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	jnc	p1pprev			; previous symbol entry

; new public symbol entry
	inc	PublicSymbolCount	; bump count of unique public symbols
	inc	ResolvedSymbolCount	; bump count of resolved symbols
	jmp	p1pset		; new entry

; previous entry, see if public or common duplicate, else assume extdef and setup values
p1pprev:
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	p1pdupe			; previous was public or communal, duplicate
	inc	ResolvedSymbolCount	; bump count of resolved symbols

; set values and pointers in public entry
; gs:di -> public symbol entry
p1pset:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT (WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; shut off weak/lazy flags
	or	gs:[di+PubSymRecStruc.pssFlags],PUBLICSYMBOLFLAG	; flag public symbol
	mov	eax,SymbolOffset
	mov	gs:[di+PubSymRecStruc.pssOffset],eax
	mov	eax,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax
	mov	ax,SymbolSegIndex
	or	ax,ax			; see if absolute public
	je	p1pabs

; lookup and assign individual segdef pointer
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	lfs	si,fs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; fs:si -> first segdef entry
	dec	ax				; make index relative zero
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to symbol's segdef entry
	add	si,ax			; fs:si -> symbol's individual segment entry, not normalized
	cmp	si,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	p1pgetmast		; no

; segment entry has wrapped to next buffer
	sub	si,(SIZEINDSEGDEFblk-indsegdefsysvarsize)
	mov	fs,fs:[IndSegDefBlkStruc.isdbNextPtr]	; get pointer to next block

; fs:si -> individual segdef entry
; gs:di -> public symbol entry
p1pgetmast:
	mov	gs:[di+WORD PTR PubSymRecStruc.pssIndSegDefPtr],si	; save -> segdef entry
	mov	gs:[di+WORD PTR PubSymRecStruc.pssIndSegDefPtr+2],fs
	lfs	si,fs:[si+IndSegDefRecStruc.isdrMasterPtr]	; fs:si -> master segdef entry
	test	fs:[si+MasterSegDefRecStruc.mssFlags],ABSOLUTESEGMENTFLAG
	jne	p1pabs			; absolute public symbol (attached to absolute segment)

	mov	ax,SymbolGroupIndex
	or	ax,ax			; see if group relative public
	je	p1pnext			; no

; lookup and assign group pointer
	dec	ax				; make index relative zero
	shl	ax,2			; x4, dword entry in table
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	lfs	si,fs:[IOBuffHeaderStruc.ibhsPtrToGrpPtrs]	; fs:si -> group pointer table
	add	si,ax			; fs:si -> pointer to group entry, not normalized
	cmp	si,SIZEGRPPTRTABLEBLK	; see if overflow to next block
	jb	p1pgrp			; no

; segment entry has wrapped to next buffer
	sub	si,(SIZEGRPPTRTABLEBLK-GRPPTRTABLESYSVARSIZE)
	mov	fs,fs:[GrpPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block

; fs:si -> pointer to group entry
p1pgrp:
	mov	eax,fs:[si]		; eax -> group entry
	mov	gs:[di+PubSymRecStruc.pssGrpDefPtr],eax
	or	gs:[di+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG	; flag group relative

p1pnext:
	pop	si				; restore fs:si -> symbol name
	pop	fs
	pop	cx				; restore cx==record length
	call	ReadByteDecCX	; length of name
	push	cx
	mov	cl,al			; scan past name
	xor	ch,ch
	mov	dx,cx			; save byte scan count
	call	ScanAhead
	pop	cx
	sub	cx,dx			; update original record length
	call	ReadWordDecCX	; read, discard symbol offset
	call	ReadIndexDecCX	; read, discard type index
	or	cx,cx			; see if any publics left
	jne	p1p2			; yes
	ret

; absolute public symbol
p1pabs:
	mov	ax,SymbolFrame
	movzx	eax,ax
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax	; frame number shares with nonabsolute segdef ptr
	or	gs:[di+PubSymRecStruc.pssFlags],ABSOLUTESYMBOLFLAG	; flag absolute
	jmp	p1pnext

; public symbol already existed, warn about multiple definitions
; ignore new public symbol
; gs:di -> old public symbol info
p1pdupe:
	call	MultipleDefSymWarn	; warn about symbol
	jmp	p1pnext

Pass1PUBDEFProc	ENDP

;*****************************
;* PASS1PUBDEF32PROC         *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1PUBDEF32Proc	PROC
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	dec	cx				; adjust for checksum byte
	call	ReadIndexDecCX	; read group index
	mov	SymbolGroupIndex,ax
	call	ReadIndexDecCX	; read segment index
	mov	SymbolSegIndex,ax
	or	ax,ax			; see if segment index zero, frame exists
	jne	p1p32			; no frame number
	call	ReadWordDecCX	; get frame number
	mov	SymbolFrame,ax

; fs:si -> symbol name with length byte prepended
p1p32:
	push	cx			; save remaining length of record
	push	fs			; save pointer to name
	push	si
	call	ReadByte	; get length of name
	mov	cl,al
	xor	ch,ch			; cx holds bytes to scan past for name
	call	ScanAhead	; move fs:si -> public offset
	call	ReadDword
	mov	SymbolOffset,eax
	pop	si				; restore fs:si -> symbol name
	pop	fs
	push	fs			; save fs:si -> symbol name back to stack
	push	si
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	jnc	p1p3prev			; previous symbol entry

; new public symbol entry
	inc	PublicSymbolCount	; bump count of unique public symbols
	inc	ResolvedSymbolCount	; bump count of resolved symbols
	jmp	p1p3set		; new entry

; previous entry, see if public or common duplicate, else assume extdef and setup values
p1p3prev:
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	p1p3dupe			; previous was public or communal, duplicate
	inc	ResolvedSymbolCount	; bump count of resolved symbols

; set values and pointers in public entry
; gs:di -> public symbol entry
p1p3set:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT (WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; shut off weak/lazy flags
	or	gs:[di+PubSymRecStruc.pssFlags],PUBLICSYMBOLFLAG	; flag public symbol
	mov	eax,SymbolOffset
	mov	gs:[di+PubSymRecStruc.pssOffset],eax
	mov	eax,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax
	mov	ax,SymbolSegIndex
	or	ax,ax			; see if absolute public
	je	p1p3abs

; lookup and assign individual segdef pointer
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	lfs	si,fs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; fs:si -> first segdef entry
	dec	ax				; make index relative zero
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to symbol's segdef entry
	add	si,ax			; fs:si -> symbol's individual segment entry, not normalized
	cmp	si,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	p1p3getmast		; no

; segment entry has wrapped to next buffer
	sub	si,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	fs,fs:[IndSegDefBlkStruc.isdbNextPtr]	; get pointer to next block

; fs:si -> individual segdef entry
; gs:di -> public symbol entry
p1p3getmast:
	mov	gs:[di+WORD PTR PubSymRecStruc.pssIndSegDefPtr],si	; save -> segdef entry
	mov	gs:[di+WORD PTR PubSymRecStruc.pssIndSegDefPtr+2],fs
	lfs	si,fs:[si+IndSegDefRecStruc.isdrMasterPtr]	; fs:si -> master segdef entry
	test	fs:[si+MasterSegDefRecStruc.mssFlags],ABSOLUTESEGMENTFLAG
	jne	p1p3abs			; absolute public symbol (attached to absolute segment)

	mov	ax,SymbolGroupIndex
	or	ax,ax			; see if group relative public
	je	p1p3next			; no

; lookup and assign group pointer
	dec	ax				; make index relative zero
	shl	ax,2			; x4, dword entry in table
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	lfs	si,fs:[IOBuffHeaderStruc.ibhsPtrToGrpPtrs]	; fs:si -> group pointer table
	add	si,ax			; fs:si -> pointer to group entry, not normalized
	cmp	si,SIZEGRPPTRTABLEBLK	; see if overflow to next block
	jb	p1p3grp			; no

; segment entry has wrapped to next buffer
	sub	si,(SIZEGRPPTRTABLEBLK-GRPPTRTABLESYSVARSIZE)
	mov	fs,fs:[GrpPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block

; fs:si -> pointer to group entry
p1p3grp:
	mov	eax,fs:[si]		; eax -> group entry
	mov	gs:[di+PubSymRecStruc.pssGrpDefPtr],eax
	or	gs:[di+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG	; flag group relative

p1p3next:
	pop	si				; restore fs:si -> symbol name
	pop	fs
	pop	cx				; restore cx==record length
	call	ReadByteDecCX	; length of name
	push	cx
	mov	cl,al			; scan past name
	xor	ch,ch
	mov	dx,cx			; save byte scan count
	call	ScanAhead
	pop	cx
	sub	cx,dx			; update original record length
	call	ReadDwordDecCX	; read, discard symbol offset
	call	ReadIndexDecCX	; read, discard type index
	or	cx,cx			; see if any publics left
	jne	p1p32			; yes
	ret

; absolute public symbol
p1p3abs:
	mov	ax,SymbolFrame
	movzx	eax,ax
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax	; frame number shares with nonabsolute segdef ptr
	or	gs:[di+PubSymRecStruc.pssFlags],ABSOLUTESYMBOLFLAG	; flag absolute
	jmp	p1p3next

; public symbol already existed, warn about multiple definitions
; ignore new public symbol
; gs:di -> old public symbol info
p1p3dupe:
	call	MultipleDefSymWarn	; warn about symbol
	jmp	p1p3next

Pass1PUBDEF32Proc	ENDP

;*****************************
;* PASS1LEXTDEFPROC          *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1LEXTDEFProc	PROC
	mov	IsLocalSymbol,ON	; flag as local symbol
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	dec	cx				; adjust for checksum byte

; fs:si -> symbol name with length byte prepended
p1lenameloop:
	jcxz	p1leret		; done
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	or	gs:[di+PubSymRecStruc.pssFlags],LOCALSYMBOLFLAG	; flag as local
	mov	eax,ModuleCount		; save module count for LPUBDEF matchup
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax

; save pointer to extdef in table for later fixupp use
	call	SetSymPtrTableEntry
	inc	CurrentFixSymCount	; bump count of fixupp-referenceable symbols in module
	call	ReadByteDecCx	; get symbol name length
	mov	dx,cx			; save record length
	mov	cl,al
	xor	ch,ch			; cx holds name length (bytes to scan past)
	call	ScanAhead
	sub	dx,cx
	mov	cx,dx			; cx holds updated record length
	call ReadIndexDecCx	; read type index, ignore
	jmp	p1lenameloop

p1leret:
	ret
Pass1LEXTDEFProc	ENDP

;*****************************
;* PASS1LPUBDEFPROC          *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1LPUBDEFProc	PROC
	mov	IsLocalSymbol,ON	; flag as local symbol
	dec	cx				; adjust for checksum byte
	call	ReadIndexDecCX	; read group index
	mov	SymbolGroupIndex,ax
	call	ReadIndexDecCX	; read segment index
	mov	SymbolSegIndex,ax
	or	ax,ax			; see if segment index zero, frame exists
	jne	p1lp2			; no frame number
	call	ReadWordDecCX	; get frame number
	mov	SymbolFrame,ax

; fs:si -> symbol name with length byte prepended
p1lp2:
	push	cx			; save remaining length of record
	push	fs			; save pointer to name
	push	si
	call	ReadByte	; get length of name
	mov	cl,al
	xor	ch,ch			; cx holds bytes to scan past for name
	call	ScanAhead	; move fs:si -> public offset
	call	ReadWord
	movzx	eax,ax
	mov	SymbolOffset,eax
	pop	si				; restore fs:si -> symbol name
	pop	fs
	push	fs			; save fs:si -> symbol name back to stack
	push	si
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public

; set values and pointers in public entry
; gs:di -> public symbol entry
p1lpset:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT (WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; shut off weak/lazy flags
	or	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG or LOCALSYMBOLFLAG)
	mov	eax,SymbolOffset
	mov	gs:[di+PubSymRecStruc.pssOffset],eax
	mov	eax,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax
	mov	ax,SymbolSegIndex
	or	ax,ax			; see if absolute public
	je	p1lpabs

; lookup and assign individual segdef pointer
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	lfs	si,fs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; fs:si -> first segdef entry
	dec	ax				; make index relative zero
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to symbol's segdef entry
	add	si,ax			; fs:si -> symbol's individual segment entry, not normalized
	cmp	si,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	p1lpgetmast		; no

; segment entry has wrapped to next buffer
	sub	si,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	fs,fs:[IndSegDefBlkStruc.isdbNextPtr]	; get pointer to next block

; fs:si -> individual segdef entry
; gs:di -> public symbol entry
p1lpgetmast:
	mov	gs:[di+WORD PTR PubSymRecStruc.pssIndSegDefPtr],si	; save -> segdef entry
	mov	gs:[di+WORD PTR PubSymRecStruc.pssIndSegDefPtr+2],fs
	lfs	si,fs:[si+IndSegDefRecStruc.isdrMasterPtr]	; fs:si -> master segdef entry
	test	fs:[si+MasterSegDefRecStruc.mssFlags],ABSOLUTESEGMENTFLAG
	jne	p1lpabs			; absolute public symbol (attached to absolute segment)

	mov	ax,SymbolGroupIndex
	or	ax,ax			; see if group relative public
	je	p1lpnext		; no

; lookup and assign group pointer
	dec	ax				; make index relative zero
	shl	ax,2			; x4, dword entry in table
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	lfs	si,fs:[IOBuffHeaderStruc.ibhsPtrToGrpPtrs]	; fs:si -> group pointer table
	add	si,ax			; fs:si -> pointer to group entry, not normalized
	cmp	si,SIZEGRPPTRTABLEBLK	; see if overflow to next block
	jb	p1lpgrp			; no

; segment entry has wrapped to next buffer
	sub	si,(SIZEGRPPTRTABLEBLK-GRPPTRTABLESYSVARSIZE)
	mov	fs,fs:[GrpPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block

; fs:si -> pointer to group entry
p1lpgrp:
	mov	eax,fs:[si]		; eax -> group entry
	mov	gs:[di+PubSymRecStruc.pssGrpDefPtr],eax
	or	gs:[di+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG	; flag group relative

p1lpnext:
	pop	si				; restore fs:si -> symbol name
	pop	fs
	pop	cx				; restore cx==record length
	call	ReadByteDecCX	; length of name
	push	cx
	mov	cl,al			; scan past name
	xor	ch,ch
	mov	dx,cx			; save byte scan count
	call	ScanAhead
	pop	cx
	sub	cx,dx			; update original record length
	call	ReadWordDecCX	; read, discard symbol offset
	call	ReadIndexDecCX	; read, discard type index
	or	cx,cx			; see if any publics left
	jne	p1lp2			; yes
	ret

; absolute public symbol
p1lpabs:
	mov	ax,SymbolFrame
	movzx	eax,ax
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax	; frame number shares with nonabsolute segdef ptr
	or	gs:[di+PubSymRecStruc.pssFlags],(ABSOLUTESYMBOLFLAG OR LOCALSYMBOLFLAG)
	jmp	p1lpnext

Pass1LPUBDEFProc	ENDP

;*****************************
;* PASS1LPUBDEF32PROC        *
;*****************************

Pass1LPUBDEF32Proc	PROC
	mov	IsLocalSymbol,ON	; flag as local symbol
	dec	cx				; adjust for checksum byte
	call	ReadIndexDecCX	; read group index
	mov	SymbolGroupIndex,ax
	call	ReadIndexDecCX	; read segment index
	mov	SymbolSegIndex,ax
	or	ax,ax			; see if segment index zero, frame exists
	jne	p1lp322			; no frame number
	call	ReadWordDecCX	; get frame number
	mov	SymbolFrame,ax

; fs:si -> symbol name with length byte prepended
p1lp322:
	push	cx			; save remaining length of record
	push	fs			; save pointer to name
	push	si
	call	ReadByte	; get length of name
	mov	cl,al
	xor	ch,ch			; cx holds bytes to scan past for name
	call	ScanAhead	; move fs:si -> public offset
	call	ReadDword
	mov	SymbolOffset,eax
	pop	si				; restore fs:si -> symbol name
	pop	fs
	push	fs			; save fs:si -> symbol name back to stack
	push	si
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public

; set values and pointers in public entry
; gs:di -> public symbol entry
p1lp32set:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT (WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; shut off weak/lazy flags
	or	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG or LOCALSYMBOLFLAG)
	mov	eax,SymbolOffset
	mov	gs:[di+PubSymRecStruc.pssOffset],eax
	mov	eax,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax
	mov	ax,SymbolSegIndex
	or	ax,ax			; see if absolute public
	je	p1lp32abs

; lookup and assign individual segdef pointer
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	lfs	si,fs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; fs:si -> first segdef entry
	dec	ax				; make index relative zero
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to symbol's segdef entry
	add	si,ax			; fs:si -> symbol's individual segment entry, not normalized
	cmp	si,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	p1lp32getmast		; no

; segment entry has wrapped to next buffer
	sub	si,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	fs,fs:[IndSegDefBlkStruc.isdbNextPtr]	; get pointer to next block

; fs:si -> individual segdef entry
; gs:di -> public symbol entry
p1lp32getmast:
	mov	gs:[di+WORD PTR PubSymRecStruc.pssIndSegDefPtr],si	; save -> segdef entry
	mov	gs:[di+WORD PTR PubSymRecStruc.pssIndSegDefPtr+2],fs
	lfs	si,fs:[si+IndSegDefRecStruc.isdrMasterPtr]	; fs:si -> master segdef entry
	test	fs:[si+MasterSegDefRecStruc.mssFlags],ABSOLUTESEGMENTFLAG
	jne	p1lp32abs			; absolute public symbol (attached to absolute segment)

	mov	ax,SymbolGroupIndex
	or	ax,ax			; see if group relative public
	je	p1lp32next		; no

; lookup and assign group pointer
	dec	ax				; make index relative zero
	shl	ax,2			; x4, dword entry in table
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	lfs	si,fs:[IOBuffHeaderStruc.ibhsPtrToGrpPtrs]	; fs:si -> group pointer table
	add	si,ax			; fs:si -> pointer to group entry, not normalized
	cmp	si,SIZEGRPPTRTABLEBLK	; see if overflow to next block
	jb	p1lp32grp			; no

; segment entry has wrapped to next buffer
	sub	si,(SIZEGRPPTRTABLEBLK-GRPPTRTABLESYSVARSIZE)
	mov	fs,fs:[GrpPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block

; fs:si -> pointer to group entry
p1lp32grp:
	mov	eax,fs:[si]		; eax -> group entry
	mov	gs:[di+PubSymRecStruc.pssGrpDefPtr],eax
	or	gs:[di+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG	; flag group relative

p1lp32next:
	pop	si				; restore fs:si -> symbol name
	pop	fs
	pop	cx				; restore cx==record length
	call	ReadByteDecCX	; length of name
	push	cx
	mov	cl,al			; scan past name
	xor	ch,ch
	mov	dx,cx			; save byte scan count
	call	ScanAhead
	pop	cx
	sub	cx,dx			; update original record length
	call	ReadDwordDecCX	; read, discard symbol offset
	call	ReadIndexDecCX	; read, discard type index
	or	cx,cx			; see if any publics left
	jne	p1lp322			; yes
	ret

; absolute public symbol
p1lp32abs:
	mov	ax,SymbolFrame
	movzx	eax,ax
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax	; frame number shares with nonabsolute segdef ptr
	or	gs:[di+PubSymRecStruc.pssFlags],(ABSOLUTESYMBOLFLAG OR LOCALSYMBOLFLAG)
	jmp	p1lp32next

Pass1LPUBDEF32Proc	ENDP

;*****************************
;* PASS1SEGDEFPROC           *
;*****************************

; upon entry fs:si -> read buffer, cx holds record length

Pass1SEGDEFProc	PROC
	mov	Is32BitSeg,OFF	; flag not 32-bit segment
	call	ReadByte	; get ACBP byte
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap
	jne	pseisphar		; yes
	mov	ACBPByte,al		; save it
	test	al,AFIELDOFACBP	; see if frame number+offset present (if A field is zero)
	sete	IsAbsoluteSeg	; re/set absolute segment flag
	je	pseabs			; absolute segment

pse2:
	call	ReadWord	; get segment length
	movzx	eax,ax		; zero extend to dword value
	mov	SegmentLength,eax	; save segment length
	test	ACBPByte,PFIELDOFACBP	; see if 32-bit segment
	jne	pse32			; yes

	cmp	FlatModuleFlag,OFF	; see if flat module
	jne	pse32			; yes, segment is 32-bit by definition

	test	ACBPByte,BFIELDOFACBP	; see if big (==64K) segment
	je	pse3			; no
	mov	BYTE PTR SegmentLength+2,1	; set segment length to 64K

pse3:
	call	ReadIndex	; get segment name index
	or	ax,ax			; make sure nonzero
	je	poor2
	mov	SegmentNameIndex,ax	; save index

	call	ReadIndex	; get class name index
	or	ax,ax			; make sure nonzero
	je	poor3
	mov	ClassNameIndex,ax	; save index

	call	GetMasterSegDefEntry	; return -> master segment definition entry in gs:di
	call	MakeIndSegDefEntry	; make the individual segdef record entry

IFDEF SYMBOLPACK
	cmp	MayBeClipperMod,OFF	; see if possible clipper module already
	jne	pseinc			; yes
	cmp	SymbolTableOverflow,OFF	; see if symbol table overflowed
	jne	pseinc			; yes

	push	es			; save critical registers
	push	cx
	push	si

	mov	bx,di			; gs:bx -> master segdef entry
	test	gs:[bx+MasterSegDefRecStruc.mssACBPByte],CFIELDOFACBP
	jne	psesymout		; not a private segment

	les	di,gs:[bx+MasterSegDefRecStruc.mssNamePtr]	; es:di -> name
	call	NormalESDIDest	; normalize es:di to destination name buffer
	mov	si,OFFSET DGROUP:SYMBOLSText
	mov	cx,2			; 8 bytes, 2 dwords
	repe	cmpsd		; compare entry name to SYMBOLS
	jne	psesymout		; not equal

	les	di,gs:[bx+MasterSegDefRecStruc.mssClassPtr]	; es:di -> class
	call	NormalESDIDest	; normalize es:di to destination name buffer
	mov	si,OFFSET DGROUP:SYMBOLSText
	mov	cx,2			; 8 bytes, 2 dwords
	repe	cmpsd		; compare entry name to SYMBOLS
	jne	psesymout		; not equal

; segment is name SYMBOLS class SYMBOLS, could be Clipper module
	mov	MayBeClipperMod,ON	; flag possible Clipper
	mov	ax,CurrentSegDefCount
	inc	ax				; make relative 1
	mov	ClipperSymSegIndex,ax	; save symbols segment index
	mov	WORD PTR CurrentSymbolSegPtr,bx	; save -> segdef for possible length modification
	mov	WORD PTR CurrentSymbolSegPtr+2,gs

psesymout:
	pop	si				; restore critical registers
	pop	cx
	pop	es

pseinc:
ENDIF

	inc	CurrentSegDefCount	; bump count of current segdef
	ret

pse32:
	mov	Is32BitSeg,ON	; flag is 32-bit segment
	test	ACBPByte,BFIELDOFACBP	; see if big (==4G) segment
	je	pse3			; no
	mov	SegmentLength,-1	; set segment length to 4G-1
	jmp	pse3

; absolute segment, additional fields
pseabs:
	call	ReadWord	; read segment frame number
	mov	SegFrameNumber,ax
	call	ReadByte	; read offset, ignored
	jmp	pse2		; continue processing

poor2:
	mov	cl,2
	call	BadOBJModuleExit

poor3:
	mov	cl,3
	call	BadOBJModuleExit

; processing phar lap, change ACBP to set P bit and
;  transfer to SEGDEF32 processing
pseisphar:
	or	al,PFIELDOFACBP	; set P bit
	jmp	ps32PharLap		; continue processing in segdef32 routine

Pass1SEGDEFProc	ENDP

;*****************************
;* PASS1SEGDEF32PROC         *
;*****************************

; upon entry fs:si -> read buffer, cx holds record length

Pass1SEGDEF32Proc	PROC
	mov	Is32BitSeg,OFF	; flag not 32-bit segment
	call	ReadByte	; get ACBP byte
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap
	jne	pseisphar		; yes

; entry point for phar lap module processing from SEGDEF procedure
;  al holds (modified with P bit set) ACBP byte
ps32PharLap:
	mov	ACBPByte,al		; save it
	test	al,AFIELDOFACBP	; see if frame number+offset present (if A field is zero)
	sete	IsAbsoluteSeg	; re/set absolute segment flag
	je	ps3abs			; absolute segment

ps32:
	call	ReadDword	; get segment length
	mov	SegmentLength,eax	; save segment length
	test	ACBPByte,PFIELDOFACBP	; see if 32-bit segment
	jne	ps332			; yes

	cmp	FlatModuleFlag,OFF	; see if flat module
	jne	ps332			; yes, segment is 32-bit by definition

	test	ACBPByte,BFIELDOFACBP	; see if big (==4G) segment
	je	ps33			; no
	mov	SegmentLength,-1	; set segment length to 4G-1

ps33:
	call	ReadIndex	; get segment name index
	or	ax,ax			; make sure nonzero
	je	poor16
	mov	SegmentNameIndex,ax	; save index

	call	ReadIndex	; get class name index
	or	ax,ax			; make sure nonzero
	je	poor17
	mov	ClassNameIndex,ax	; save index

	call	GetMasterSegDefEntry	; return -> master segment definition entry in gs:di
	call	MakeIndSegDefEntry	; make the individual segdef record entry
	inc	CurrentSegDefCount	; bump count of current segdef
	ret

ps332:
	mov	Is32BitSeg,ON	; flag is 32-bit segment
	test	ACBPByte,BFIELDOFACBP	; see if big (==4G) segment
	je	ps33			; no
	mov	SegmentLength,-1	; set segment length to 4G-1
	jmp	ps33

; absolute segment, additional fields
ps3abs:
	call	ReadWord	; read segment frame number
	mov	SegFrameNumber,ax
	call	ReadByte	; read offset, ignored
	jmp	ps32		; continue processing

poor16:
	mov	cl,16
	call	BadOBJModuleExit

poor17:
	mov	cl,17
	call	BadOBJModuleExit

Pass1SEGDEF32Proc	ENDP

;*****************************
;* PASS1COMDEFPROC           *
;*****************************

Pass1COMDEFProc	PROC
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	dec	cx				; adjust for checksum byte

; fs:si -> symbol name with length byte prepended
p1cnameloop:
	or	cx,cx
	je	p1cret			; done
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	pushf				; save carry flag status while get other communal info
	call	ReadByteDecCx	; get symbol name length
	mov	dx,cx			; save record length
	mov	cl,al
	xor	ch,ch			; cx holds name length (bytes to scan past)
	call	ScanAhead
	sub	dx,cx
	mov	cx,dx			; cx holds updated record length

	call	ReadIndexDecCX	; read type index, discard
	call	ReadByteDecCX	; read data type, 61h==FAR, 62h==NEAR
	mov	CommunalDataType,al	; keep data type
	cmp	al,61h			; see if far data
	je	p1cfar			; yes
	cmp	al,62h			; see if near data
	jne	poor8			; no, bad value

; near communal
	call	ReadCommunalLen	; communal length in edx
	mov	eax,edx			; communal length in eax
	mov	dx,NEARCOMSYMBOLFLAG	; set flag value
	jmp	p1cchkdupe

; far communal
p1cfar:
	call	ReadCommunalLen	; get number of elements
	mov	ebx,edx
	call	ReadCommunalLen	; get element size

; ebx*edx == communal length, error if overflow
	mov	eax,ebx
	mul	edx				; compute total communal length, in edx:eax
	or	edx,edx			; see if overflow
	jne	poor8			; yes, bad record
	mov	dx,FARCOMSYMBOLFLAG

p1cchkdupe:
	popf				; get symbol status, carry flag from GetPubSymEntry
	jc	p1cchklen		; new symbol

	test	gs:[di+PubSymRecStruc.pssFlags],PUBLICSYMBOLFLAG
	jne	p1cdupe			; symbol already exists as public
	
	test	gs:[di+PubSymRecStruc.pssFlags],(FARCOMSYMBOLFLAG OR NEARCOMSYMBOLFLAG)
	jne	p1cchklen		; already communal

; previously flagged as external, now a communal
	dec	PublicSymbolCount	; drop count of unique public symbols

; save length, if previous communal length save longest
; eax == communal length
p1cchklen:
	mov	ebx,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],ebx	; save defining module
	test	gs:[di+PubSymRecStruc.pssFlags],(FARCOMSYMBOLFLAG OR NEARCOMSYMBOLFLAG)
	je	p1cnewcom		; no previous communal
	cmp	eax,gs:[di+PubSymRecStruc.pssOffset]
	jb	p1cset			; new communal length < old, don't save it
	jmp	p1csavelen	; save the new length

p1cret:
	ret

; new communal, track number
p1cnewcom:
	inc	TotalCommunalCount	; bump total count of communals

p1csavelen:
	mov	gs:[di+PubSymRecStruc.pssOffset],eax	; save communal length in public offset for now

; dx holds near/far flag
p1cset:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT (WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; shut off weak/lazy flags
	or	gs:[di+PubSymRecStruc.pssFlags],dx	; set near/far flag

p1cptr:
	call	SetSymPtrTableEntry	; save pointer to communal for fixup purposes
	inc	CurrentFixSymCount	; bump count of fixupp-referenceable symbols in module
	jmp	p1cnameloop

; public symbol already existed, warn about multiple definitions
; ignore new communal symbol
; gs:di -> old public symbol info
p1cdupe:
	call	MultipleDefSymWarn	; warn about symbol
	jmp	p1cptr		; save pointer to symbol

; bad communal record
poor8:
	mov	cl,8
	call	BadOBJModuleExit

Pass1COMDEFProc	ENDP

;*****************************
;* PASS1LCOMDEFPROC          *
;*****************************

Pass1LCOMDEFProc	PROC
	mov	IsLocalSymbol,ON	; flag as local symbol
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	dec	cx				; adjust for checksum byte

; fs:si -> symbol name with length byte prepended
p1clnameloop:
	or	cx,cx
	je	p1clret			; done
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	pushf				; save carry flag status while get other communal info
	call	ReadByteDecCx	; get symbol name length
	mov	dx,cx			; save record length
	mov	cl,al
	xor	ch,ch			; cx holds name length (bytes to scan past)
	call	ScanAhead
	sub	dx,cx
	mov	cx,dx			; cx holds updated record length

	call	ReadIndexDecCX	; read type index, discard
	call	ReadByteDecCX	; read data type, 61h==FAR, 62h==NEAR
	mov	CommunalDataType,al	; keep data type
	cmp	al,61h			; see if far data
	je	p1clfar			; yes
	cmp	al,62h			; see if near data
	jne	poor9			; no, bad value

; near communal
	call	ReadCommunalLen	; communal length in edx
	mov	eax,edx			; communal length in eax
	mov	dx,NEARCOMSYMBOLFLAG	; set flag value
	jmp	p1clchkdupe

; far communal
p1clfar:
	call	ReadCommunalLen	; get number of elements
	mov	ebx,edx
	call	ReadCommunalLen	; get element size

; ebx*edx == communal length, error if overflow
	mov	eax,ebx
	mul	edx				; compute total communal length, in edx:eax
	or	edx,edx			; see if overflow
	jne	poor9			; yes, bad record
	mov	dx,FARCOMSYMBOLFLAG

p1clchkdupe:
	popf				; get symbol status, carry flag from GetPubSymEntry
	jc	p1clchklen		; new symbol

	test	gs:[di+PubSymRecStruc.pssFlags],PUBLICSYMBOLFLAG
	jne	p1cldupe			; symbol already exists as public

; save length, if previous communal length save longest
p1clchklen:
	mov	ebx,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],ebx	; save defining module
	test	gs:[di+PubSymRecStruc.pssFlags],(FARCOMSYMBOLFLAG OR NEARCOMSYMBOLFLAG)
	je	p1clnewcom		; no previous communal
	cmp	eax,gs:[di+PubSymRecStruc.pssOffset]
	jb	p1clset			; new communal length < old, don't save it
	jmp	p1clsavelen	; save the new length

; new communal, track number
p1clnewcom:
	inc	TotalCommunalCount	; bump total count of communals

p1clsavelen:
	mov	gs:[di+PubSymRecStruc.pssOffset],eax	; save communal length in public offset for now

; dx holds near/far flag
p1clset:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT (WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; shut off weak/lazy flags
	or	gs:[di+PubSymRecStruc.pssFlags],dx	; set near/far flag

p1clptr:
	call	SetSymPtrTableEntry	; save pointer to communal for fixup purposes
	inc	CurrentFixSymCount	; bump count of fixupp-referenceable symbols in module
	jmp	p1clnameloop

p1clret:
	ret

; public symbol already existed, warn about multiple definitions
; ignore new communal symbol
; gs:di -> old public symbol info
p1cldupe:
	call	MultipleDefSymWarn	; warn about symbol
	jmp	p1clptr		; save pointer to symbol

; bad local communal record
poor9:
	mov	cl,9
	call	BadOBJModuleExit

Pass1LCOMDEFProc	ENDP

;*****************************
;* READCOMMUNALLEN           *
;*****************************

; upon entry fs:si -> comdef communal length field
; returns communal length in edx
; updates cx,si,fs
; destroys ax,edx

ReadCommunalLen	PROC
	xor	edx,edx			; zero init length
	call	ReadByteDecCX	; read communal length, or communal length flag
							; valid values are 0-80h,81h,84h,88h
	cmp	al,80h			; see if one byte value (contained in al)
	ja	rcl2			; no
	mov	dl,al
	ret

rcl2:
	cmp	al,81h			; see if two byte value
	jne	rcl3			; no
	call	ReadWordDecCX	; get two byte value
	mov	dx,ax
	ret

rcl3:
	cmp	al,84h			; see if three byte value
	jne	rcl4			; no
	call	ReadWordDecCX
	mov	dx,ax
	call	ReadByteDecCX
	shl	edx,8			; shift one byte value
	mov	dl,al
	ret

rcl4:
	cmp	al,88h			; see if four byte value
	jne	poor8			; no, bad value
	call	ReadWordDecCX
	mov	dx,ax
	call	ReadWordDecCX
	shl	edx,16			; shift one word value
	mov	dx,ax
	ret
ReadCommunalLen	ENDP

;*****************************
;* PASS1LIDATAPROC           *
;*****************************

Pass1LIDATAProc	PROC
	ret					; no pass 1 processing of LIDATA records
Pass1LIDATAProc	ENDP

;*****************************
;* PASS1LIDATA32PROC         *
;*****************************

Pass1LIDATA32Proc	PROC
	ret					; no pass 1 processing of LIDATA32 records
Pass1LIDATA32Proc	ENDP

;*****************************
;* PASS1LNAMESPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1LNAMESProc	PROC
	cmp	LNAMESIndexSel,0	; see if selector allocated yet
	je	plnalloc		; not yet, do it

plnloop:
	cmp	cx,1			; see if at end of record
	jbe	plnret			; yes
	inc	CurrentLNAMESIndex
	mov	ax,CurrentLNAMESIndex
	mov	di,ax			; save index value
	shl	ax,2			; total size of current index pointer table, dword per entry
	cmp	ax,MaximumLNAMESAlloc
	jae	plnsize			; larger than current allocation, resize it

pln2:
	call	ReadByte	; read length byte, force valid pointer if si==SIZEIOBUFFER and pending changeover
	mov	dl,al
	xor	dh,dh			; dx hold name length value
	inc	dx				; adjust for name length byte
	sub	cx,dx			; subtract bytes processed from object record length
	dec	di				; di holds index value relative 0
	shl	di,2			; convert to dword offset
	mov	gs,LNAMESIndexSel	; gs -> LNAMES table of pointers block
	dec	si				; backup si to name length byte (known nonzero offset)
	mov	gs:[di],si		; keep offset pointer to LNAMES name
	mov	gs:[di+2],fs	; keep selector pointer to LNAMES name
	xchg	dx,cx		; cx==name length+length byte dx == object record length
	call	ScanAhead	; scan past current name to next byte
	mov	cx,dx			; cx==object record length
	jmp plnloop	; get pointer to next LNAMES name, if any

plnret:
	ret

; allocate selector for LNAMES index pointers
plnalloc:
	mov	dx,256			; allocate in 256-byte chunks
	mov	MaximumLNAMESAlloc,dx	; save allocation amount
	call	AllocateMemory
	mov	LNAMESIndexSel,ax
	jmp plnloop

; resize selector to next 256-byte boundary
plnsize:
	mov	ax,MaximumLNAMESAlloc
	inc	ah				; quick and sleazy way to add 256 to ax
	jc	poor1			; poorly formed object module, too many lnames
	mov	MaximumLNAMESAlloc,ax
	mov	dx,ax
	mov	ax,LNAMESIndexSel
	call	ResizeMemory
	jmp pln2

poor1:
	mov	cl,1
	call	BadOBJModuleExit

Pass1LNAMESProc	ENDP

;*****************************
;* PASS1COMENTPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1COMENTProc	PROC
	call	ReadByteDecCX	; get comment type, discard
	call	ReadByteDecCX	; get comment class
	mov	di,OFFSET DGROUP:COMENTClassTable	; es:di -> lookup table for object record types
	push	cx			; save record length
	mov	cx,ds:[di-2]	; get number of entries in table
	repne	scasb
	pop	cx				; restore object record length to cx, KEEP FLAG STATUS
	jne	pcpret			; class not found in table

; class found in table, transfer to appropriate code
	dec	di				; di -> matching entry
	sub	di,OFFSET DGROUP:COMENTClassTable	; di == class offset
	add	di,di			; word offset
	add	di,OFFSET DGROUP:COMENTClassVector	; di -> entry in COMENTClassVector table
	call	ds:[di]		; transfer to appropriate routine

pcpret:
	ret
Pass1COMENTProc	ENDP

;*****************************
;* COMENTCLASSLIBSEARCHPROC  *
;*****************************

; process COMENT class for LIBSEARCH (library search name)
; upon entry fs:si -> library name, cx holds length of string+1

COMENTClassLibSearchProc	PROC
	dec	cx				; adjust for checksum byte, cx==length of string
	je	cclsret			; no library name
	cmp	IsNoDefaultLIBOption,OFF	; see if no default libraries
	jne	cclsret			; yes, don't process them

; fs:si -> original string, move to CompBuffSource
	mov	di,OFFSET DGROUP:CompBuffSource	; ds:di -> destination

; transfer all name chars, fs:si -> name
cclsloop:
	cmp	si,SIZEIOBUFFBLK	; see if si is at wrap point
	jb	cclstrans		; no
	mov	si,IOBUFFSYSVARSIZE	; wrap si past sysvars
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; fs -> next block

cclstrans:
	mov	al,fs:[si]
	inc	si
	mov	ds:[di],al
	inc	di
	dec	cx
	jne	cclsloop
	mov	BYTE PTR ds:[di],0	; null terminate the string

ccls2:
	mov	si,OFFSET DGROUP:CompBuffSource	; ds:si -> filename
	lodsb				; al holds first char of filename, si -> remaining name chars
	mov	FileListFlag,2	; do remaining SaveOBJLIBFileName setup
	mov	FreeFormatFlag,ON
	mov	DefaultLIBAddFlag,ON	; flag default library addition
	call	SaveOBJLIBFileName

cclsret:
	ret
COMENTClassLibSearchProc	ENDP

;*****************************
;* COMENTCLASSDOSSEGPROC     *
;*****************************

; process COMENT record class for DOSSEG

COMENTClassDOSSEGProc	PROC
	mov	IsDOSSEG,ON		; set dosseg segment ordering flag
	ret
COMENTClassDOSSEGProc	ENDP

;*****************************
;* COMENTClassLinkPassProc   *
;*****************************

; link pass coment record, ignore

COMENTClassLinkPassProc	PROC
	ret
COMENTClassLinkPassProc	ENDP

;*****************************
;* COMENTClassLibModProc     *
;*****************************

; library module name
; upon entry fs:si -> library name, cx holds length of string+1

COMENTClassLibModProc	PROC
	mov	gs,CurrentBaseOBJBuff
	mov	WORD PTR gs:[IOBuffHeaderStruc.ibhsModNamePtr],si	; save pointer to module name
	mov	WORD PTR gs:[IOBuffHeaderStruc.ibhsModNamePtr+2],fs
	mov	di,OFFSET DGROUP:LIBModuleName
	call	ReadNameString	; read object record name string
	cmp	IsLinkInfoLimitOption,OFF	; see if displaying limited link information
	jne	clmret			; yes, don't display this
	cmp	IsLinkInfoOption,OFF	; see if displaying link information
	je	clmret			; no

	mov	bx,OFFSET DGROUP:ProcessModText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:LIBModuleName
	call	DisplayVarStringNoCRLF
	mov	bx,OFFSET DGROUP:InText
	call	DisplayTextStringNoCRLF
	mov	bx,OFFSET DGROUP:CurrentFileName
	call	DisplayVarStringNoCRLF

clmret:
	ret
COMENTClassLibModProc	ENDP

;*****************************
;* COMENTClassEXESTRProc     *
;*****************************

; embedded text at end of EXE file

COMENTClassEXESTRProc	PROC

;@@@ do nothing for now

	ret
COMENTClassEXESTRProc	ENDP

;*****************************
;* COMENTClassPharLapProc    *
;*****************************

; special Phar Lap flag for module
; further check for '80386' or 'OS220' text following class byte

COMENTClassPharLapProc	PROC
	call	ReadDword
	cmp	eax,'8308'		; see if first four bytes of 80386
	jne	cplos2			; no
	call	ReadByte
	cmp	al,'6'
	jne	cplret			; no match

cplmatch:
	mov	PharLapModuleFlag,ON	; match, assume phar lap module
	mov	gs,CurrentBaseOBJBuff	; flag module for special phar lap processing
	or	gs:[IOBuffHeaderStruc.ibhsFlags],ISPHARLAPMODFLAG

cplret:
	ret

cplos2:
	cmp	eax,'22SO'		; see if first four bytes of OS220
	jne	cplret			; no
	call	ReadByte
	cmp	al,'0'
	jne	cplret			; no match

; consider OS/2 flag as special case of phar lap module, phar lap
; flag is always set when os/2 flag is set, but os/2 flag does not
; perform the special LIDATA32 16-bit repeat count exception
	or	gs:[IOBuffHeaderStruc.ibhsFlags],ISOS2MODFLAG
	jmp	cplmatch

COMENTClassPharLapProc	ENDP

;*****************************
;* COMENTClassWKEXTProc      *
;*****************************

; weak external symbol

COMENTClassWKEXTProc	PROC
	dec	cx				; adjust for checksum byte

cwkloop:
	test	cx,cx
	je	cwkret			; done

	mov	gs,CurrentBaseOBJBuff	; gs -> i/o base buffer for fixup calculations
	lgs	bx,gs:[IOBuffHeaderStruc.ibhsPtrToSymPtrs]	; gs:bx -> symbol pointer table
	call	ReadIndexDecCX	; read weak extern index
	dec	ax				; make relative zero
	shl	ax,2			; dword per entry
	add	bx,ax			;gs:bx -> symbol entry, not normalized
	cmp	bx,SIZESYMPTRTABLEBLK	; see if overflow to next block
	jb	cwk2			; no
	sub	bx,(SIZESYMPTRTABLEBLK-SYMPTRTABLESYSVARSIZE)
	mov	gs,gs:[SymPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block

; gs:bx -> pointer to weak extern entry
cwk2:
	lgs	bx,gs:[bx]		; gs:bx -> symbol entry
	push	gs
	push	bx			; save -> weak entry on stack

	mov	gs,CurrentBaseOBJBuff	; gs -> i/o base buffer for fixup calculations
	lgs	bx,gs:[IOBuffHeaderStruc.ibhsPtrToSymPtrs]	; gs:bx -> symbol pointer table
	call	ReadIndexDecCX	; read default resolution symbol index
	dec	ax				; make relative zero
	shl	ax,2			; dword per entry
	add	bx,ax			; gs:bx -> symbol entry, not normalized
	cmp	bx,SIZESYMPTRTABLEBLK	; see if overflow to next block
	jb	cwk3			; no
	sub	bx,(SIZESYMPTRTABLEBLK-SYMPTRTABLESYSVARSIZE)
	mov	gs,gs:[SymPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block

; gs:bx -> pointer to default resolution symbol
cwk3:
	lgs	bx,gs:[bx]		; gs:bx -> symbol entry
	pop	di
	pop	ax				; ax:di -> weak extern entry
	push	gs			; save -> default resolution symbol
	push	bx
	mov	gs,ax			; gs:di -> weak extern entry
	pop	eax				; eax -> default resolution symbol

; see if could be weak extern
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	cwkloop			; already non-external

; module count (for extdef) must match current module
;  If not, then a symbol has been previously declared strongly
;   and cannot be reset to weak or external
	mov	ebx,ModuleCount
	cmp	ebx,gs:[di+PubSymRecStruc.pssModuleCount]
	jne	cwkloop			; modules don't match

cwkset:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT LAZYEXTERNFLAG
	or	gs:[di+PubSymRecStruc.pssFlags],WEAKEXTERNFLAG	; and turn on is weak flag
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax	; update weak extern default resolution pointer
	jmp cwkloop
	
cwkret:
	ret
COMENTClassWKEXTProc	ENDP

;*****************************
;* COMENTClassLZEXTProc      *
;*****************************

; lazy external symbol

COMENTClassLZEXTProc	PROC
	dec	cx				; adjust for checksum byte

clzloop:
	test	cx,cx
	je	clzret			; done

	mov	gs,CurrentBaseOBJBuff	; gs -> i/o base buffer for fixup calculations
	lgs	bx,gs:[IOBuffHeaderStruc.ibhsPtrToSymPtrs]	; gs:bx -> symbol pointer table
	call	ReadIndexDecCX	; read lazy extern index
	dec	ax				; make relative zero
	shl	ax,2			; dword per entry
	add	bx,ax			;gs:bx -> symbol entry, not normalized
	cmp	bx,SIZESYMPTRTABLEBLK	; see if overflow to next block
	jb	clz2			; no
	sub	bx,(SIZESYMPTRTABLEBLK-SYMPTRTABLESYSVARSIZE)
	mov	gs,gs:[SymPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block

; gs:bx -> pointer to lazy extern entry
clz2:
	lgs	bx,gs:[bx]		; gs:bx -> symbol entry
	push	gs
	push	bx			; save -> lazy entry on stack

	mov	gs,CurrentBaseOBJBuff	; gs -> i/o base buffer for fixup calculations
	lgs	bx,gs:[IOBuffHeaderStruc.ibhsPtrToSymPtrs]	; gs:bx -> symbol pointer table
	call	ReadIndexDecCX	; read default resolution symbol index
	dec	ax				; make relative zero
	shl	ax,2			; dword per entry
	add	bx,ax			; gs:bx -> symbol entry, not normalized
	cmp	bx,SIZESYMPTRTABLEBLK	; see if overflow to next block
	jb	clz3			; no
	sub	bx,(SIZESYMPTRTABLEBLK-SYMPTRTABLESYSVARSIZE)
	mov	gs,gs:[SymPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block

; gs:bx -> pointer to default resolution symbol
clz3:
	lgs	bx,gs:[bx]		; gs:bx -> symbol entry
	pop	di
	pop	ax				; ax:di -> lazy extern entry
	push	gs			; save -> default resolution symbol
	push	bx
	mov	gs,ax			; gs:di -> lazy extern entry
	pop	eax				; eax -> default resolution symbol

; see if could be lazy extern
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	clzloop			; already non-external

; module count must match current module OR symbol must already be flagged
;  weak or lazy.  If not, then a symbol has been previously declared strongly
;  and cannot be reset to weak or external
	test	gs:[di+PubSymRecStruc.pssFlags],(WEAKEXTERNFLAG OR LAZYEXTERNFLAG)
	jne	clzset			; previously weak or external, use latest info
						; spec doesn't say, maybe should stick with old info?
	mov	ebx,ModuleCount
	cmp	ebx,gs:[di+PubSymRecStruc.pssModuleCount]
	jne	clzloop			; modules don't match

clzset:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT WEAKEXTERNFLAG
	or	gs:[di+PubSymRecStruc.pssFlags],LAZYEXTERNFLAG	; and turn on is lazy flag
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax	; update lazy extern default resolution pointer
	jmp clzloop

clzret:
	ret
COMENTClassLZEXTProc	ENDP

;*****************************
;* COMENTClassOMFEXTProc     *
;*****************************

; OMF Extension record

COMENTClassOMFEXTProc	PROC
	dec	cx
	je	omfret
	call	ReadByteDecCX	; get subtype

IFDEF	DLLSUPPORT
	cmp	al,COMENTEXPDEF
	jne	omfchkimp			; not an export definition
	call	Pass1EXPDEFProc
	jmp	omfret

omfchkimp:
	call	Pass1IMPDEFProc
	jmp	omfret
ENDIF

omfret:
	ret
COMENTClassOMFEXTProc	ENDP

IFDEF	DLLSUPPORT

;*****************************
;* PASS1EXPDEFPROC           *
;*****************************

; process export definition

Pass1EXPDEFProc	PROC
	call	GetEXPDEFEntry	; return pointer to EXPDEF entry in gs:di
	call	ReadByte	; get exported flag
	mov	gs:[di+EXPDEFRecStruc.edsExportedFlag],al	; save it

; fs:si -> exported name
	mov	WORD PTR gs:[di+EXPDEFRecStruc.edsExportedNamePtr],si
	mov	WORD PTR gs:[di+EXPDEFRecStruc.edsExportedNamePtr+2],fs
	call	ReadByte	; get exported name char count
	movzx	cx,al
	call	ScanAhead	; scan past exported name

; fs:si -> internal name
	mov	WORD PTR gs:[di+EXPDEFRecStruc.edsInternalNamePtr],si
	mov	WORD PTR gs:[di+EXPDEFRecStruc.edsInternalNamePtr+2],fs
	call	ReadByte	; get internal name char count
	movzx	cx,al
	call	ScanAhead	; scan past internal name

	test	gs:[di+EXPDEFRecStruc.edsExportedFlag],EXPDEFORDINALBIT	; see if ordinal bit set
	je	pedret			; ordinal bit not set, no export ordinal value
	call	ReadWord	; get export ordinal
	mov	gs:[di+EXPDEFRecStruc.edsExportOrdinal],ax

pedret:
	inc	TotalEXPDEFCount	; bump count of EXPDEFs
	ret
Pass1EXPDEFProc	ENDP

;*****************************
;* PASS1IMPDEFPROC           *
;*****************************

; process export definition

Pass1IMPDEFProc	PROC
	call	GetIMPDEFEntry	; return pointer to IMPDEF entry in gs:di
	call	ReadByte	; get ordinal flag
	mov	gs:[di+IMPDEFRecStruc.idsOrdinalFlag],al	; save it

; fs:si -> internal name
	mov	WORD PTR gs:[di+IMPDEFRecStruc.idsInternalNamePtr],si
	mov	WORD PTR gs:[di+IMPDEFRecStruc.idsInternalNamePtr+2],fs
	call	ReadByte	; get internal name char count
	movzx	cx,al
	call	ScanAhead	; scan past internal name

; fs:si -> module name
	mov	WORD PTR gs:[di+IMPDEFRecStruc.idsModuleNamePtr],si
	mov	WORD PTR gs:[di+IMPDEFRecStruc.idsModuleNamePtr+2],fs
	call	ReadByte	; get exported name char count
	movzx	cx,al
	call	ScanAhead	; scan past exported name

; fs:si -> entry ident field
	mov	WORD PTR gs:[di+IMPDEFRecStruc.idsEntryIdentPtr],si
	mov	WORD PTR gs:[di+IMPDEFRecStruc.idsEntryIdentPtr+2],fs

	xor	eax,eax
	mov	gs:[di+IMPDEFRecStruc.idsModuleNumber],eax
	mov	gs:[di+IMPDEFRecStruc.idsFunctionNumber],eax

	inc	TotalIMPDEFCount	; bump count of IMPDEFs
	ret
Pass1IMPDEFProc	ENDP

ENDIF

;*****************************
;* PASS1CEXTDEFPROC          *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1CEXTDEFProc	PROC
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	dec	cx				; adjust for checksum byte

; fs:si -> logical name index
p1cenameloop:
	jcxz	p1ceret		; done
	call ReadIndexDecCx	; read logical name index

	push	fs
	push	si

	mov	fs,LNAMESIndexSel	; gs -> LNAMES table of pointers block
	mov	si,ax			; get logical name index
	dec	si				; make relative zero
	shl	si,2			; convert to dword offset
	lfs	si,fs:[si]		; fs:si -> logical name, from dword entries table

; fs:si -> symbol name with length byte prepended
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	jnc	p1ce2			; previous entry

; new public symbol entry
	inc	PublicSymbolCount	; bump count of unique public symbols

; save pointer to extdef in table for later fixupp use
p1ce2:
	call	SetSymPtrTableEntry
	inc	CurrentFixSymCount	; bump count of fixupp-referenceable symbols in module
	pop	si
	pop	fs				; restore fs:si -> cextdef record
	call ReadIndexDecCx	; read type index, ignore
	jmp p1cenameloop

p1ceret:
	ret
Pass1CEXTDEFProc	ENDP

;*****************************
;* PASS1COMDATPROC           *
;*****************************

Pass1COMDATProc	PROC
	dec	cx				; adjust for checksum byte
	call	ReadByteDecCX	; get comdat flags
	test	al,(COMDATITERATEDFLAG OR COMDATLOCALFLAG)
	je	p1cd2			; no special flags

; unsupported flags
	mov	cl,18
	call	BadOBJModuleExit	; no return

; unsupported attributes
p1cdattrib:
	mov	cl,19
	call	BadOBJModuleExit	; no return

; unsupported align
p1cdalign:
	mov	cl,20
	call	BadOBJModuleExit	; no return

p1cd2:
	mov	COMDATFlags,al

	call	ReadByteDecCX	; get comdat attributes
	cmp	al,(COMDATSELECTPICKANY OR COMDATALLOCFARCODE)
	je	p1cdsaveatt			; acceptable attributes

; check for additional acceptable attributes
	cmp	al,COMDATALLOCFARCODE	; ignore No Match (0) attribute
	je	p1cdsaveatt			; acceptable attributes

	jmp p1cdattrib	; unsupport attributes

p1cdsaveatt:
	mov	COMDATAttributes,al

	call	ReadByteDecCX	; get align
	test	al,(COMDATALIGNBYTE OR COMDATALIGNWORD OR COMDATALIGNPARA OR COMDATALIGN256 OR COMDATALIGNDWORD)
	je	p1cdalign
	mov	COMDATAlign,al

	call	ReadWordDecCX	; get offset
	movzx	eax,ax
	mov	COMDATOffset,eax
	call	ReadIndexDecCX	; scan past type index

;@@@ read public base if necessary

	call	ReadIndexDecCX	; get public name index

	mov	fs,LNAMESIndexSel	; fs -> LNAMES table of pointers block
	mov	si,ax			; get logical name index
	dec	si				; make relative zero
	shl	si,2			; convert to dword offset
	lfs	si,fs:[si]		; fs:si -> logical name, from dword entries table

; fs:si -> symbol name with length byte prepended
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	jnc	p1cdprev			; previous symbol entry

; new public symbol entry
	inc	PublicSymbolCount	; bump count of unique public symbols
	inc	ResolvedSymbolCount	; bump count of resolved symbols
	jmp p1cdset		; new entry

; previous was comdat
p1cdprecom:
	test	COMDATFlags,COMDATCONTINUEFLAG	; see if COMDAT continuation
	je	p1cdret		; no, ignore
	mov	eax,ModuleCount
	cmp	gs:[di+PubSymRecStruc.pssModuleCount],eax
	jne	p1cdret		; not the same module, ignore

; same module, continuation, check if continuation of appropriate comdat
;	push	gs
	mov	ax,gs
	push	ax

	push	di
	lgs	di,FirstModCOMDATPtr	; gs:di -> first module comdat on record
	pop	eax				; eax -> public symbol of this comdat

p1cdpre2:
	cmp	gs:[di+ComDatRecStruc.cdsPubSymPtr],eax	; check if matches
	je	p1cdupdlen		; yes

; need to find matching comdat for this continuation
	add	di,SIZE ComDatRecStruc	; try next comdat
	cmp	di,SIZECOMDATBLK	; see if on next block
	jb	p1cdpre2		; no
	mov	gs,gs:[ComDatBlkStruc.cdbNextPtr]
	mov	di,COMDATSYSVARSIZE	; gs:di -> first entry in next block
	jmp	p1cdpre2

; previous entry, see if public or common duplicate, else assume extdef and setup values
p1cdprev:
	test	gs:[di+PubSymRecStruc.pssFlags],COMDATSYMBOLFLAG
	jne	p1cdprecom			; previous was comdat
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	p1cddupe			; previous was public or communal, duplicate
	inc	ResolvedSymbolCount	; bump count of resolved symbols

; set values and pointers in public entry
; gs:di -> public symbol entry
p1cdset:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT (WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; shut off weak/lazy flags
	or	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR COMDATSYMBOLFLAG)
	mov	gs:[di+PubSymRecStruc.pssOffset],0
	mov	eax,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax

p1cdgetcom:
;	push	gs			; save -> public symbol on stack
	mov	ax,gs
	push	ax

	push	di
	call	GetComDatEntry	; get comdat entry in gs:di
	mov	eax,ModuleCount
	cmp	eax,LastCOMDATModule	; see if first comdat pointer already saved
	je	p1cdsavepub		; yes
	mov	LastCOMDATModule,eax
	mov	WORD PTR FirstModCOMDATPtr,di	; save pointer in case of continuation
	mov	WORD PTR FirstModCOMDATPtr+2,gs

p1cdsavepub:
	pop	eax				; eax -> public symbol
	mov	gs:[di+ComDatRecStruc.cdsPubSymPtr],eax

p1cdupdlen:
	mov	eax,COMDATOffset
	movzx	ecx,cx
	add	eax,ecx

	test	COMDATFlags,COMDATCONTINUEFLAG	; see if COMDAT continuation
	je	p1cd3			; no

; continuation adds to pre-existing length
p1cdcont:
	add	gs:[di+ComDatRecStruc.cdsLength],eax
	jmp p1cdret	; bypass remaining setup code

p1cd3:
	mov	gs:[di+ComDatRecStruc.cdsLength],eax	; update comdat length
	mov	al,COMDATFlags
	mov	gs:[di+ComDatRecStruc.cdsRecordFlags],al
	mov	al,COMDATAttributes
	mov	gs:[di+ComDatRecStruc.cdsAttributes],al
	mov	al,COMDATAlign
	mov	gs:[di+ComDatRecStruc.cdsAlign],al
	mov	eax,OBJRecPtr
	mov	gs:[di+ComDatRecStruc.cdsRecordPtr],eax

p1cdret:
	ret

; public symbol already existed, warn about multiple definitions
; ignore new public symbol
; gs:di -> old public symbol info
p1cddupe:
	call	MultipleDefSymWarn	; warn about symbol
	jmp p1cdret

Pass1COMDATProc	ENDP

;*****************************
;* PASS1COMDAT32PROC         *
;*****************************

Pass1COMDAT32Proc	PROC
	dec	cx				; adjust for checksum byte
	call	ReadByteDecCX	; get comdat flags
	test	al,(COMDATITERATEDFLAG OR COMDATLOCALFLAG)
	je	p1cd322			; no special flags

; unsupported flags
	mov	cl,18
	call	BadOBJModuleExit	; no return

; unsupported attributes
p1cd32attrib:
	mov	cl,19
	call	BadOBJModuleExit	; no return

; unsupported align
p1cd32align:
	mov	cl,20
	call	BadOBJModuleExit	; no return

p1cd322:
	mov	COMDATFlags,al

	call	ReadByteDecCX	; get comdat attributes
	cmp	al,(COMDATSELECTPICKANY OR COMDATALLOCFARCODE)
	je	p1cd32saveatt			; acceptable attributes

; check for additional acceptable attributes
	cmp	al,COMDATALLOCFARCODE	; ignore No Match (0) attribute
	je	p1cd32saveatt	; acceptable attributes

	cmp	al,COMDATSELECTPICKANY	; see if pick any selection/explicit (0) allocation
	je	p1cd32saveatt	; acceptable

	jmp p1cd32attrib	; unsupported attributes

p1cd32saveatt:
	mov	COMDATAttributes,al

	call	ReadByteDecCX	; get align
	cmp	al,5
	ja	p1cd32align		; invalid align type
	mov	COMDATAlign,al

	call	ReadDwordDecCX	; get 32-bit offset
	mov	COMDATOffset,eax
	call	ReadIndexDecCX	; scan past type index

	test	COMDATAttributes,COMDATALLOCTYPEFIELD	; see if explicit allocation type
	jne	p1cd32pubind	; not explicit allocation

	call	ReadIndexDecCX	; read group index, ignore (always?)
	call	ReadIndexDecCX	; read segment index
	mov	COMDATSegIndex,ax
	test	ax,ax		; see if segment index zero, frame exists
	je	p1cd32attrib	; don't allow absolute frame COMDAT's

p1cd32pubind:
	call	ReadIndexDecCX	; get public name index

	mov	fs,LNAMESIndexSel	; fs -> LNAMES table of pointers block
	mov	si,ax			; get logical name index
	dec	si				; make relative zero
	shl	si,2			; convert to dword offset
	lfs	si,fs:[si]		; fs:si -> logical name, from dword entries table

; fs:si -> symbol name with length byte prepended
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	jnc	p1cd32prev			; previous symbol entry

; new public symbol entry
	inc	PublicSymbolCount	; bump count of unique public symbols
	inc	ResolvedSymbolCount	; bump count of resolved symbols
	jmp p1cd32set		; new entry

; previous was comdat
p1cd32precom:
	test	COMDATFlags,COMDATCONTINUEFLAG	; see if COMDAT continuation
	je	p1cd32ret		; no, ignore
	mov	eax,ModuleCount
	cmp	gs:[di+PubSymRecStruc.pssModuleCount],eax
	jne	p1cd32ret		; not the same module, ignore

; same module, continuation, check if continuation of appropriate comdat
;	push	gs
	mov	ax,gs
	push	ax

	push	di
	lgs	di,FirstModCOMDATPtr	; gs:di -> first module comdat on record
	pop	eax				; eax -> public symbol of this comdat

p1cd32pre2:
	cmp	gs:[di+ComDatRecStruc.cdsPubSymPtr],eax	; check if matches
	je	p1cd32updlen	; yes

; need to find matching comdat for this continuation
	add	di,SIZE ComDatRecStruc	; try next comdat
	cmp	di,SIZECOMDATBLK	; see if on next block
	jb	p1cd32pre2		; no
	mov	gs,gs:[ComDatBlkStruc.cdbNextPtr]
	mov	di,COMDATSYSVARSIZE	; gs:di -> first entry in next block
	jmp	p1cd32pre2

; previous entry, see if public or common duplicate, else assume extdef and setup values
p1cd32prev:
	test	gs:[di+PubSymRecStruc.pssFlags],COMDATSYMBOLFLAG
	jne	p1cd32precom		; previous was comdat
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	p1cd32dupe			; previous was public or communal, duplicate
	inc	ResolvedSymbolCount	; bump count of resolved symbols

; set values and pointers in public entry
; gs:di -> public symbol entry
p1cd32set:
	and	gs:[di+PubSymRecStruc.pssFlags],NOT (WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; shut off weak/lazy flags
	or	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR COMDATSYMBOLFLAG)
	mov	gs:[di+PubSymRecStruc.pssOffset],0
	mov	eax,ModuleCount
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax

p1cd32getcom:
;	push	gs			; save -> public symbol on stack
	mov	ax,gs
	push	ax

	push	di
	call	GetComDatEntry	; get comdat entry in gs:di
	mov	eax,ModuleCount
	cmp	eax,LastCOMDATModule	; see if first comdat pointer already saved
	je	p1cd32savepub	; yes
	mov	LastCOMDATModule,eax
	mov	WORD PTR FirstModCOMDATPtr,di	; save pointer in case of continuation
	mov	WORD PTR FirstModCOMDATPtr+2,gs

p1cd32savepub:
	pop	eax				; eax -> public symbol
	mov	gs:[di+ComDatRecStruc.cdsPubSymPtr],eax

	mov	ax,COMDATSegIndex

; lookup and assign individual segdef pointer
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	lfs	si,fs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; fs:si -> first segdef entry
	dec	ax				; make index relative zero
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to symbol's segdef entry
	add	si,ax			; fs:si -> symbol's individual segment entry, not normalized
	cmp	si,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	p1cd32getmast		; no

; segment entry has wrapped to next buffer
	sub	si,(SIZEINDSEGDEFblk-indsegdefsysvarsize)
	mov	fs,fs:[IndSegDefBlkStruc.isdbNextPtr]	; get pointer to next block

; fs:si -> individual segdef entry
; gs:di -> comdat record entry
p1cd32getmast:
	mov	gs:[di+WORD PTR ComDatRecStruc.cdsIndSegDefPtr],si	; save -> segdef entry
	mov	gs:[di+WORD PTR ComDatRecStruc.cdsIndSegDefPtr+2],fs
	or	gs:[di+ComDatRecStruc.cdsFlags],SEGRELCOMDATFLAG	; flag segment relative
	lfs	si,fs:[si+IndSegDefRecStruc.isdrMasterPtr]	; fs:si -> master segdef entry
	test	fs:[si+MasterSegDefRecStruc.mssFlags],ABSOLUTESEGMENTFLAG
	jne	p1cd32attrib	; don't allow absolute frame COMDAT's

p1cd32updlen:
	mov	eax,COMDATOffset
	movzx	ecx,cx
	add	eax,ecx

	test	COMDATFlags,COMDATCONTINUEFLAG	; see if COMDAT continuation
	je	p1cd323			; no

; continuation adds to pre-existing length
	add	gs:[di+ComDatRecStruc.cdsLength],eax
	jmp p1cd32ret	; bypass remaining setup code

p1cd323:
	mov	gs:[di+ComDatRecStruc.cdsLength],eax	; update comdat length
	mov	al,COMDATFlags
	mov	gs:[di+ComDatRecStruc.cdsRecordFlags],al
	mov	al,COMDATAttributes
	mov	gs:[di+ComDatRecStruc.cdsAttributes],al
	mov	al,COMDATAlign
	mov	gs:[di+ComDatRecStruc.cdsAlign],al
	mov	eax,OBJRecPtr
	mov	gs:[di+ComDatRecStruc.cdsRecordPtr],eax

p1cd32ret:
	ret

; public symbol already existed, warn about multiple definitions
; ignore new public symbol
; gs:di -> old public symbol info
p1cd32dupe:
	call	MultipleDefSymWarn	; warn about symbol
	JMP p1cd32ret

Pass1COMDAT32Proc	ENDP

;*****************************
;* PASS1BAKPATPROC           *
;*****************************

Pass1BAKPATProc	PROC
	call	KeepBAKPATRecordInfo
	ret
Pass1BAKPATProc	ENDP

;*****************************
;* PASS1BAKPAT32PROC         *
;*****************************

Pass1BAKPAT32Proc	PROC
	call	KeepBAKPATRecordInfo
	ret
Pass1BAKPAT32Proc	ENDP

;*****************************
;* KEEPBAKPATRECORDINFO      *
;*****************************

; keep record info from BAKPAT record

KeepBAKPATRecordInfo	PROC
	cmp	BAKPATTableSel,0	; see if selector allocated yet
	je	pbpialloc		; not yet, do it

pbpiloop:
	mov	ax,CurrentBAKPATCount
	shl	ax,2			; total size of table, dword per entry
	mov	di,ax
	cmp	ax,MaxBAKPATTableAlloc
	jae	pbpisize		; larger than current allocation, resize it

pbpi2:
	mov	gs,BAKPATTableSel	; gs -> BAKPAT table of pointers block
	mov	eax,OBJRecPtr
	mov	gs:[di],eax		; keep BAKPAT record

	inc	CurrentBAKPATCount	; bump count of BAKPAT's/table entry
	ret

; allocate selector for BAKPAT pointers
pbpialloc:
	mov	dx,256			; allocate in 256-byte chunks
	mov	MaxBAKPATTableAlloc,dx	; save allocation amount
	call	AllocateMemory
	mov	BAKPATTableSel,ax
	jmp pbpiloop

; resize selector to next 256-byte boundary
pbpisize:
	mov	ax,MaxBAKPATTableAlloc
	inc	ah				; quick way to add 256 to ax
	jc	poor22			; poorly formed object module, too many BAKPAT's
	mov	MaxBAKPATTableAlloc,ax
	mov	dx,ax
	mov	ax,BAKPATTableSel
	call	ResizeMemory
	jmp pbpi2

poor22:
	mov	cl,22
	call	BadOBJModuleExit
	ret
KeepBAKPATRecordInfo	ENDP

;*****************************
;* PASS1NBKPATPROC           *
;*****************************

Pass1NBKPATProc	PROC
	call	Pass1UnsupOBJRecord	; unsupported record

Pass1NBKPATProc	ENDP

;*****************************
;* PASS1NBKPAT32PROC         *
;*****************************

Pass1NBKPAT32Proc	PROC
	call	Pass1UnsupOBJRecord	; unsupported record

Pass1NBKPAT32Proc	ENDP

;*****************************
;* PASS1GRPDEFPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1GRPDEFProc	PROC
	dec	cx				; adjust for checksum byte in record
	call	ReadIndexDecCX	; get group name index
	or	ax,ax			; make sure nonzero
	je	poor6
	mov	GroupNameIndex,ax	; save index
	call	GetGrpDefEntry	; get grpdef entry in gs:di
	call	SetGrpPtrTableEntry	; save pointer to entry

IFDEF WATCOM_ASM

; check if group name is 'FLAT' with no segment definitions
; if so, then turn on flat model option and force all segments in
;   module to be members of the FLAT group
	or	cx,cx			; see if any segment definitions
	jne	pgloop			; yes

; convert group name to upper case
	push	si			; save critical register
	lfs	si,GrpDefNamePtr
	call	ConvertToUpperCase
	mov	bx,si			; fs:bx -> uppercased group name
	pop	si				; restore critical register
	cmp	BYTE PTR fs:[bx],4
	jne	pgloop
	cmp	DWORD PTR fs:[bx+1],'TALF'
	jne	pgloop			; flat model

	mov	IsFlatOption,ON
	mov	FlatModuleFlag,ON	; flag flat module
	push	cx			; save critical register
	mov	cx,1			; init segment index to make member of group

pgflatloop:
	cmp	cx,CurrentSegDefCount
	ja	pgflatdone
	mov	GrpDefSegmentIndex,cx	; save segment index
	call	AssignSegToGroup	; assign segment to group
	inc	cx				; bump index of segment to assign to group
	jmp	pgflatloop

pgflatdone:
	pop	cx				; restore critical register
	jmp	pgret
ENDIF

; read type byte and segment definition index
pgloop:
	jcxz	pgret		; no more entries
	call	ReadByteDecCX	; read type (discard)
	call	ReadIndexDecCX	; read segdef index
	mov	GrpDefSegmentIndex,ax	; save segment index
	call	AssignSegToGroup	; assign segment to group
	jmp pgloop	; loop through all entries

pgret:
	inc	CurrentGrpDefCount	; bump count of current grpdef
	ret

; invalid group name index
poor6:
	mov	cl,6
	call	BadOBJModuleExit

Pass1GRPDEFProc	ENDP

;*****************************
;* PASS1THEADRPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1THEADRProc	PROC
	mov	gs,CurrentBaseOBJBuff
	mov	WORD PTR gs:[IOBuffHeaderStruc.ibhsModNamePtr],si	; save pointer to module name
	mov	WORD PTR gs:[IOBuffHeaderStruc.ibhsModNamePtr+2],fs
	mov	di,OFFSET DGROUP:ModuleName
	call	ReadNameString	; read object record name string
	ret
Pass1THEADRProc	ENDP

;*****************************
;* PASS1LHEADRPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass1LHEADRProc	PROC
	mov	gs,CurrentBaseOBJBuff
	mov	WORD PTR gs:[IOBuffHeaderStruc.ibhsModNamePtr],si	; save pointer to module name
	mov	WORD PTR gs:[IOBuffHeaderStruc.ibhsModNamePtr+2],fs
	mov	di,OFFSET DGROUP:ModuleName
	call	ReadNameString	; read object record name string
	ret
Pass1LHEADRProc	ENDP

;*****************************
;* PASS1MODENDPROC           *
;*****************************

Pass1MODENDProc	PROC
	mov	EndOfOBJFlag,ON	; flag at end of module

IFDEF SYMBOLPACK
	cmp	NeedCompSymProcess,OFF	; see if need to processed compressed symbols
	je	pmeret			; no
	call	ProcessCompressedSymbols	; process compressed symbols
ENDIF

pmeret:
	ret					; no further pass 1 processing of MODEND
Pass1MODENDProc	ENDP

;*****************************
;* PASS1MODEND32PROC         *
;*****************************

Pass1MODEND32Proc	PROC
	mov	EndOfOBJFlag,ON	; flag at end of module
	ret					; no further pass 1 processing of MODEND32
Pass1MODEND32Proc	ENDP

;*****************************
;* PASS1LINNUMPROC           *
;*****************************

Pass1LINNUMProc	PROC
	ret					; no pass 1 processing of this object record
Pass1LINNUMProc	ENDP

;*****************************
;* PASS1LINNUM32PROC         *
;*****************************

Pass1LINNUM32Proc	PROC
	ret					; no pass 1 processing of this object record
Pass1LINNUM32Proc	ENDP

;*****************************
;* PASS1LINSYMPROC           *
;*****************************

Pass1LINSYMProc	PROC
	ret					; no pass 1 processing
Pass1LINSYMProc	ENDP

;*****************************
;* PASS1LINSYM32PROC         *
;*****************************

Pass1LINSYM32Proc	PROC
	ret					; no pass 1 processing
Pass1LINSYM32Proc	ENDP

;*****************************
;* PASS1ALIASPROC            *
;*****************************

; upon entry fs:si -> read buffer, cx holds record length
; Emulate aliasing by making the alias symbol a weak external that points
; to the substitute symbol as the default resolution.
; This approach appears to work, I may have to add explicit aliasing support
; later if an incompatibility arises, but I don't see any right now.

Pass1AliasProc	PROC
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	mov	SearchExistSymFlag,OFF	; flag create symbol if doesn't exist

p1aploop:
	cmp	cx,1
	jbe	p1apret			; no more entries

; get the alias symbol
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	jnc	p1ap2			; previous entry

; new public symbol entry, the alias symbol
	inc	PublicSymbolCount	; bump count of unique public symbols

p1ap2:
	call	ReadByteDecCX	; get 1-byte length of alias symbol name
	mov	dx,cx			; save record length
	mov	cl,al
	xor	ch,ch			; cx holds name length (bytes to scan past)
	call	ScanAhead
	sub	dx,cx
	mov	cx,dx			; cx holds updated record length
	cmp	cx,1
	jbe	p1apret			; no more entries (this one prematurely ended)

	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	p1apgobble		; previous alias declaration was public or communal, gobble substitute and ignore
	or	gs:[di+PubSymRecStruc.pssFlags],WEAKEXTERNFLAG	; turn on weak external

	push	gs
	push	di			; save -> alias symbol on stack

; get the substitute symbol
	call	GetPubSymEntry	; search for public symbol, create if none,
							; gs:di -> entry, carry flag set if new public
	jnc	p1ap3			; previous entry

; new public symbol entry
	inc	PublicSymbolCount	; bump count of unique public symbols

p1ap3:
	pop	bx
	pop	ax				; ax:bx -> weak extern entry
	push	gs			; save -> default resolution symbol
	push	di
	mov	gs,ax			; gs:bx -> weak extern entry
	pop	eax				; eax -> default resolution symbol
	or	gs:[bx+PubSymRecStruc.pssFlags],WEAKEXTERNFLAG	; and turn on is weak flag
	mov	gs:[bx+PubSymRecStruc.pssIndSegDefPtr],eax	; update weak extern default resolution pointer

p1apgobble:
	call	ReadByteDecCX	; get 1-byte length of substitute symbol name
	mov	dx,cx			; save record length
	mov	cl,al
	xor	ch,ch			; cx holds name length (bytes to scan past)
	call	ScanAhead
	sub	dx,cx
	mov	cx,dx			; cx holds updated record length
	jmp p1aploop

p1apret:
	ret
Pass1AliasProc	ENDP

;*****************************
;* PASS1MSLIBRPROC           *
;*****************************

; processing library as object module
; ignore, suck out variables and toss into library list
; upon entry si -> read buffer, cx holds record length

Pass1MSLIBRProc	PROC
	mov	gs,CurrentBaseOBJBuff
	mov	EndOfOBJFlag,ON	; flag at end of module
	or	gs:[IOBuffHeaderStruc.ibhsFlags],IGNOREBLOCKFLAG	; flag ignore block
	mov	LIBAtFrontFlag,ON
	mov	FileListFlag,2	; flag for SaveOBJLIBFileName setup
	mov	FreeFormatFlag,ON
	push	si			; save critical register
	mov	si,OFFSET DGROUP:CurrentFileName
	lodsb
	mov	DefaultLIBAddFlag,OFF	; flag non-default library addition
	call	SaveOBJLIBFileName	; move file to library list
	pop	si				; restore critical register

; free the extraneous blocks, shrink main block down to system variable size
pmpchildloop:
	mov	ax,gs:[IOBuffHeaderStruc.ibhsChildPtr]
	or	ax,ax
	je	pmp2			; at final block
	mov	gs,ax			; gs -> child block

; gs -> final block in chain
pmp2:
	mov	ax,gs:[IOBuffHeaderStruc.ibhsParentPtr]
	or	ax,ax
	je	pmp3			; at main block
	push	ax			; save -> parent block
	mov	ax,gs			; ax -> child block
	call	ReleaseMemory	; release child block
	pop	gs				; gs -> parent block
	jmp pmp2

; only main block left, gs -> block, shrink and zero child pointer field
pmp3:
	mov	ax,gs
	mov	dx,IOBUFFSYSVARSIZE
	call	ResizeMemory
	mov	gs:[IOBuffHeaderStruc.ibhsChildPtr],0

	ret
Pass1MSLIBRProc	ENDP

ENDS

END
