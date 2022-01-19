;*********************************************************************
;*   WLP1RES.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          05/15/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   pass 1 resolution routines                                      *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLP1RES
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLDATA.INC
INCLUDE	WLERRCOD.INC

;*****************************
;* Public declarations       *
;*****************************

; procedures
PUBLIC	Pass1Resolution
PUBLIC	CaselessStrCmp
PUBLIC	ProcessBAKPATRecords

; public for debugger
PUBLIC	ResolveCOMDATs,csosegaddr
IFDEF	DLLSUPPORT
PUBLIC	ResolveIMPDEFs
ENDIF

; variables
PUBLIC	DGROUPPtr
PUBLIC	_ENDSegDefPtr
PUBLIC	FirstSegment
PUBLIC	ProgramImageSize

IFDEF DLLSUPPORT
PUBLIC	LastIMPDEFModule
PUBLIC	TotalIMPDEFFuncCount
ENDIF

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

BAKPATLocType	DB	?	; bakpat location type
ClassSelectedFlag	DB	?	; nonzero if class selected for segment resolution
ResOccurredFlag	DB	?	; nonzero if resolution occcured in segment ordering pass

CheckMasterEntry	DW	?	; current master segdef entry within block being checked for resolution
UnresolvedSegCount	DW	?	; count of unresolved segments

CurrentResClassPtr	DD	?	; current resolved segment class name pointer
CurrentResGrpPtr	DD	?	; current resolved segment group entry pointer
FirstSegment	DD	?	; first segment in program after resolution

IFDEF DLLSUPPORT
OrdinalFlag	DB	?		; IMPDEF ordinal flag value
ENDIF

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

_TEXTText	DB	5,'_TEXT'	; segment name to check against for DOSSEG use

; strings used for dosseg segment type computation
BEGDATAText	DB	7,'BEGDATA'
CODEText	DB	4,'CODE'
DGROUPText	DB	6,'DGROUP'
BSSText		DB	3,'BSS'
STACKText	DB	5,'STACK'

; strings used for near communal resolution
c_commonText	DB	8,'c_common'

; strings used for far communal resolution
FAR_BSSText	DB	7,'FAR_BSS'
HUGE_BSSText	DB	8,'HUGE_BSS'

; strings used for DOSSEG symbols
Lower_edataText	DB	6,'_edata'
Upper_EDATAText	DB	6,'_EDATA'
Lower_endText	DB	4,'_end'
Upper_ENDText	DB	4,'_END'

Lower__edataText	DB	7,'__edata'
Upper__EDATAText	DB	7,'__EDATA'
Lower__endText	DB	5,'__end'
Upper__ENDText	DB	5,'__END'

; strings used for debug segments
$$SYMBOLSText	DB	9,'$$SYMBOLS'
$$TYPESText		DB	7,'$$TYPES'
DEBSYMText		DB	6,'DEBSYM'
DEBTYPText		DB	6,'DEBTYP'

; strings used for comdat segments
COMDAT_SEGText	DB	10,'COMDAT_SEG'
FAR_DATAText	DB	8,'FAR_DATA'

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

; globals
DOSSEGPhase	DB	1		; DOSSEG segment ordering phase
						;  1==code, 2==outside dgroup, 3==begdata,
						;  4==not begdata bss stack, 5==bss, 6==stack
IsDOSSEG	DB	0		; nonzero if dosseg segment ordering
StackSegFoundFlag	DB	0	; nonzero if stack segment found in program

SegmentID	DW	0		; segment identifier

DGROUPPtr	DD	0		; pointer to DGROUP group entry, if exists
_EDATASegDefPtr	DD	0	; pointer to segdef of first BSS segment for _edata variable
_ENDSegDefPtr	DD	0	; pointer to segdef of first STACK segment for _end variable
LastSegDefPtr	DD	0	; pointer to last segdef in program, updated as resolution occurs
FarCommSegDefPtr	DD	0	; far communal segdef pointer
FarCodeComDatSegDefPtr	DD	0	; far code comdat segdef pointer
NearCommSegDefPtr	DD	0	; near communal segdef pointer
PrevFarCommunalLen	DD	0	; previous far communal lengths
PrevFarCodeComDatLen	DD	0	; previous far code comdat lengths
RunningCommunalCount	DD	0	; total communal count less resolved communals
ProgramImageSize	LABEL	DWORD	; After final resolution, SegStartOffset will hold program image size
SegStartOffset	DD	0	; segment starting offset within program (updated as resolution occurs)

IFDEF DLLSUPPORT
LastIMPDEFEntryPtr	DD	0	; pointer to last processed IMPDEF entry
LastIMPDEFFunction	DD	0	; last processed impdef function number
LastIMPDEFModule	DD	0	; last processed impdef module number
TotalIMPDEFFuncCount	DD	0	; total amount of IMPDEF functions (instead of ordinals)
TotalIMPDEFOrdCount	DD	0	; total amount of IMPDEF ordinals (instead of functions)
ENDIF

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	ACBPByte:BYTE,Is32BitSeg:BYTE,IsAbsoluteSeg:BYTE
EXTRN	BAKPATTableSel:WORD
EXTRN	CurrentBAKPATCount:WORD
EXTRN	CurrentBaseOBJBuff:WORD
EXTRN	CurrentSegDefCount:WORD,CurrentGrpDefCount:WORD,CurrentFixSymCount:WORD
EXTRN	DebugSegmentCount:WORD
EXTRN	FirstComDatBlkPtr:WORD
EXTRN	FirstGrpDefBlkPtr:WORD
EXTRN	FirstMasterSegDefBlkPtr:WORD
EXTRN	FirstPubSymBlkPtr:WORD
EXTRN	GroupNameIndex:WORD
EXTRN	IsLocalSymbol:BYTE
EXTRN	LastGrpDefBlkPtr:WORD
EXTRN	LEDATATableSel:WORD
EXTRN	LNAMESIndexSel:WORD
EXTRN	ModuleCount:DWORD
EXTRN	SearchExistSymFlag:BYTE
EXTRN	SegmentNameIndex:WORD,ClassNameIndex:WORD
EXTRN	SegmentLength:DWORD
EXTRN	TotalRelSegCount:WORD

IFDEF DLLSUPPORT
EXTRN	FirstIMPDEFBlkPtr:WORD
ENDIF

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	BadOBJModuleExit:PROC
EXTRN	GetGrpDefEntry:PROC
EXTRN	GetMasterSegDefEntry:PROC
EXTRN	GetPubSymEntry:PROC
EXTRN	InternalErrorExit:PROC
EXTRN	MakeIndSegDefEntry:PROC
EXTRN	NormalDSSISource:PROC
EXTRN	NormalESDIDest:PROC
EXTRN	NormalGSBXSource:PROC
EXTRN	NormalizeErrorExit:PROC
EXTRN	ReadByte:PROC
EXTRN	ReadByteDecCX:PROC
EXTRN	ReadWord:PROC
EXTRN	ReadWordCX:PROC
EXTRN	ReadWordDecCX:PROC
EXTRN	ReadIndexDecCX:PROC
EXTRN	ReadDwordDecCX:PROC
EXTRN	SetGrpPtrTableEntry:PROC
EXTRN	WriteMAPFile:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* PROCESSBAKPATRECORDS      *
;*****************************

; process the BAKPAT records for object module
; destroy all registers except DS

ProcessBAKPATRecords	PROC
	mov	cx,CurrentBAKPATCount
	mov	gs,BAKPATTableSel	; gs:di -> table of pointers to BAKPAT records
	push	cx
	mov	di,-4

pbpmain:
	add	di,4			; move to next entry in table, if any
	pop	cx				; restore bakpat count to process
	or	cx,cx
	je	pbpret
	dec	cx				; drop count to process
	push	cx			; save updated bakpat count to process
	lfs	si,gs:[di]		; fs:si -> BAKPAT record
	call	ReadByte
	call	ReadWordCX	; read record length
	dec	cx				; adjust for checksum byte
	mov	bl,al
	call	ReadIndexDecCX	; read segdef index
	mov	SegmentNameIndex,ax
	call	ReadByteDecCX	; read BAKPAT location type
	mov	BAKPATLocType,al

	cmp	bl,BAKPAT		; see if 16-bit BAKPAT
	jne	pbp32loop		; no

pbp16loop:
	jcxz	pbpmain			; no more BAKPAT offset/values
	call	ReadWordDecCX	; read offset
	movzx	ebx,ax
	call	ReadWordDecCX	; read value
	movzx	eax,ax
	call	BackPatchLEDATA
	jmp	SHORT pbp16loop

pbp32loop:
	jcxz	pbpmain			; no more BAKPAT offset/values
	call	ReadDwordDecCX	; read offset
	mov	ebx,eax
	call	ReadDwordDecCX	; read value
	call	BackPatchLEDATA
	jmp	SHORT pbp32loop

pbpret:
	ret
ProcessBAKPATRecords	ENDP

;*****************************
;* BACKPATCHLEDATA           *
;*****************************

; back patch LEDATA's from BAKPAT records
; upon entry ebx==BAKPAT offset, eax==BAKPAT value,
;   SegmentNameIndex==segment index, BAKPATLocType==BAKPAT location type
; conserve cx,si,ds,fs,gs

BackPatchLEDATA	PROC
;	push	cx			; save critical registers
	push	si
	push	fs
;	mov	cx,CurrentBAKPATCount

	push	eax			; save BAKPAT value
	mov	fs,LEDATATableSel
	xor	esi,esi
	mov	ax,SegmentNameIndex

bplmain:
	cmp	fs:[esi+LEDATADataPtrStruc.ldpsSegmentIndex],ax	; see if same index
	je	bplmatchind		; yes

bplnext:
	add	esi,SIZELEDATAPTRENTRY
;	dec	cx
;	jne	bplmain

; didn't find the proper LEDATA, abort with error
;	mov	cl,23
;	call	BadOBJModuleExit
	jmp	SHORT bplmain

bplmatchind:
	cmp	ebx,fs:[esi+LEDATADataPtrStruc.ldpsDataStart]
	jb	bplnext			; offset too low
	cmp	ebx,fs:[esi+LEDATADataPtrStruc.ldpsDataEnd]
	ja	bplnext			; offset too high

; successful match on LEDATA record
	mov	eax,fs:[esi+LEDATADataPtrStruc.ldpsDataStart]
	lfs	si,DWORD PTR fs:[esi+LEDATADataPtrStruc.ldpsRecOffset]	; fs:si -> LEDATA data
	sub	ebx,eax			; make offset relative to LEDATA record data (16-bit)
	add	si,bx
	cmp	si,SIZEIOBUFFBLK	; see if at or past wrap point
	jb	bpl2			; no
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; fs -> next block in chain

; fs:si -> location to adjust
; dword on stack == value to adjust with
; BAKPATLocType==size of adjustment (0==1 byte, 1==2 bytes, 2==4 bytes)
bpl2:
	pop	edx				; edx == value
	mov	ax,dx			; get low word of value
	shr	edx,16			; convert high word to 16-bit dx
	cmp	BAKPATLocType,0	; see if 1 byte
	jne	bpl3			; no
	add	fs:[si],al
	jmp	SHORT bplret

bpl3:
	cmp	BAKPATLocType,1	; see if 2 byte
	jne	bpl4			; no
	add	fs:[si],al
	adc	ah,0			; carry to high byte
	inc	si				; move to next location to fix up
	cmp	si,SIZEIOBUFFBLK	; see if past wrap pointer
	jb	bpl3a			; no
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)

bpl3a:
	add	fs:[si],ah
	jmp	SHORT bplret

bpl4:
	add	fs:[si],al
	adc	ah,0			; carry to high byte
	adc	dx,0			; carry to high word
	inc	si
	cmp	si,SIZEIOBUFFBLK	; see if past wrap pointer
	jb	bpl4a			; no
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)

bpl4a:
	add	fs:[si],ah		; adjust high byte of word
	adc	dx,0			; carry to high word
	inc	si
	cmp	si,SIZEIOBUFFBLK	; see if past wrap pointer
	jb	bpl4b			; no
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)

bpl4b:
	add	fs:[si],dl		; adjust low byte of high word
	adc	dh,0			; carry to high byte of high word
	inc	si
	cmp	si,SIZEIOBUFFBLK	; see if past wrap pointer
	jb	bpl4c			; no
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)

bpl4c:
	add	fs:[si],dh		; adjust high byte of high word

bplret:
	pop	fs				; restore critical registers
	pop	si
;	pop	cx
	ret
BackPatchLEDATA	ENDP

;*****************************
;* PASS1RESOLUTION           *
;*****************************

; first pass resolution processing
; destroy all registers except ds -> DGROUP

Pass1Resolution	PROC
	cmp	IsDOSSEG,OFF	; see if dosseg is set
	je	pr2				; no
	call	DOSSEGTextSegAdjust	; yes, adjust _TEXT segment with 16 prefixed bytes

pr2:
	call	ResolveCommunals	; convert communal variables to proper segments
	call	ResolveCOMDATs	; convert comdat variables to segments
	call	ComputeSegOrder	; compute the proper segment order for program
	call	SetDOSSEGSymbols	; set/create _edata and _end symbols

IFDEF	DLLSUPPORT
	call	ResolveIMPDEFs	; set impdef pointers to symbol entries, set module & function number
ENDIF

	ret
Pass1Resolution	ENDP

IFDEF	DLLSUPPORT

;*****************************
;* RESOLVEIMPDEFS            *
;*****************************

; set impdef pointers to symbol entries, set module & function number

ResolveIMPDEFs	PROC
	cmp	TotalIMPDEFCount,0	; see if any exports
	je	ridret			; no

	mov	esi,TotalIMPDEFCount
	mov	SearchExistSymFlag,ON	; search for existing symbols, only
	mov	ax,FirstIMPDEFBlkPtr	; ax -> first symbol block

ridblkloop:
	mov	fs,ax			; fs -> symbol block
	mov	bx,IMPDEFSYSVARSIZE	; fs:bx -> first symbol entry in block
	xor	cx,cx			; init impdef entry count in block

ridentloop:
	push	cx			; save critical registers
	push	si
	push	di
	push	fs
	push	bx

	mov	al,fs:[bx+IMPDEFRecStruc.idsOrdinalFlag]
	mov	OrdinalFlag,al	; save it

ridnotord:
	lfs	si,fs:[bx+IMPDEFRecStruc.idsInternalNamePtr]
	call	GetPubSymEntry	; find the internal symbol name, return gs:di -> entry
	jc	ridnextent		; not a pre-existing symbol
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	ridimpok		; import not resolved

; this import is no longer valid, mark for flushing
ridflush:
	pop	bx
	pop	fs				; fs:bx -> current impdef entry
	push	fs
	push	bx
	or	fs:[bx+IMPDEFRecStruc.idsGeneralFlags],FLUSHIMPDEFFLAG
	jmp	ridnextent

ridimpok:
	or	gs:[di+PubSymRecStruc.pssFlags],IMPORTSYMBOLFLAG	; flag as import
	pop	eax				; get impdef entry pointer
	push	eax			; restore to stack
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax

; now compute module and function name for impdef
	pop	bx
	pop	gs				; gs:bx -> current impdef entry
	push	gs
	push	bx
	cmp	LastIMPDEFEntryPtr,0	; see if first entry
	jne	ridchkmod		; no
	or	gs:[bx+IMPDEFRecStruc.idsGeneralFlags],NEWMODULENAMEFLAG
	mov	LastIMPDEFFunction,-1	; pre-adjust for function increment
	jmp	ridupd		; save function number in case is an ordinal

; compare this module name to last module name
; if same, keep module number, if not same, module number==old number+1
; function number always increment from previous, use ordinal if flagged
ridchkmod:
	lgs	bx,gs:[bx+IMPDEFRecStruc.idsModuleNamePtr]
	les	di,LastIMPDEFEntryPtr	; es:di -> previous impdef entry
	les	di,es:[di+IMPDEFRecStruc.idsModuleNamePtr]
	call	NormalGSBXSource
	call	NormalESDIDest
	mov	cl,gs:[bx]		; get name length of current
	cmp	cl,es:[di]		; compare to previous
	jne	ridnewmod		; not the same, new module name

ridcmploop:
	mov	al,gs:[bx]
	cmp	al,es:[di]
	jne	ridnewmod		; module names differ
	inc	bx
	inc	di
	dec	cl
	jne	ridcmploop		; more chars to check
	jmp	ridupd			; done, match, do module, function/ordinal update

; different module name, module number==old number+1
ridnewmod:
	pop	bx
	pop	gs				; gs:bx -> current impdef entry
	push	gs
	push	bx
	or	gs:[bx+IMPDEFRecStruc.idsGeneralFlags],NEWMODULENAMEFLAG
	inc	LastIMPDEFModule

ridupd:
	pop	bx
	pop	gs				; gs:bx -> current impdef entry
	push	gs
	push	bx
	mov	eax,LastIMPDEFModule
	mov	gs:[bx+IMPDEFRecStruc.idsModuleNumber],eax
	cmp	OrdinalFlag,0	; see if ordinal instead of function number
	jne	ridsaveord		; yes
	inc	LastIMPDEFFunction	; increment function number
	mov	eax,LastIMPDEFFunction
	mov	gs:[bx+IMPDEFRecStruc.idsFunctionNumber],eax
	inc	TotalIMPDEFFuncCount	; track total functions for 3P header setup
	jmp	ridsavelast

ridsaveord:
	lfs	si,gs:[bx+IMPDEFRecStruc.idsEntryIdentPtr]	; fs:si -> entry ident/ordinal
	call	ReadWord	; get/save ordinal value
	movzx	eax,ax
	mov	gs:[bx+IMPDEFrecStruc.idsFunctionNumber],eax
	inc	TotalIMPDEFOrdCount	; track total ordinals for 3P header setup

ridsavelast:
	pop	eax				; get current impdef entry pointer
	push	eax			; restore to stack
	mov	LastIMPDEFEntryPtr,eax	; save pointer to last processed IMPDEF

ridnextent:
	pop	bx				; restore critical registers
	pop	fs
	pop	di
	pop	si
	pop	cx

	add	bx,SIZE IMPDEFRecStruc	; fs:bx -> next symbol entry
	inc	cx				; bump current entry count
	inc	edi				; point to next entry, if any
	dec	esi				; drop count of entries to process
	je	ridret			; no more entries
	cmp	cx,MAXCOUNTEXPDEFBLK	; see if more entries in block (might not be all used)
	jb	ridentloop		; more entries in block
	mov	ax,fs:[IMPDEFBlkStruc.idbNextPtr]	; ax -> next block
	jmp	ridentloop

ridret:
	ret
ResolveIMPDEFs	ENDP

ENDIF

;*****************************
;* SETDOSSEGSYMBOLS          *
;*****************************

; set/create _edata, __edata, _end and __end symbols if DOSSEG flag

SetDOSSEGSymbols	PROC
	cmp	IsDOSSEG,OFF	; see if dosseg is set
	je	sdsret			; no

; find pointer to DGROUP, if exists
	cmp	LastGrpDefBlkPtr,0	; zero if no previous allocations
	je	sds4			; no previous allocations, block must be allocated

; check if group entry already exists, compare based on name
	mov	ax,FirstGrpDefBlkPtr	; ax -> first grpdef block

sdsmainloop:
	mov	gs,ax			; gs -> grpdef block
	mov	dx,gs:[GrpDefBlkStruc.gdbCount]	; dx holds entry count in block
	mov	bx,GRPDEFSYSVARSIZE	; gs:bx -> first entry

sdsentloop:
	les	di,gs:[bx+GrpDefRecStruc.gdrGrpNamePtr]
	mov	si,OFFSET DGROUP:DGROUPText

; ds:si -> current grpdef name
; es:di -> stored grpdef name
sdslencmp:
	cmpsb				; see if length byte matches
	jne	sdsnextent		; no

	mov	cl,ds:[si-1]
	xor	ch,ch			; get # of bytes to check
	mov	ax,cx			; save count of bytes
	and	cx,1			; get odd byte
	jcxz	sds2			; no odd byte
	cmpsb
	jne	sdsnextent		; no match
sds2:
	mov	cx,ax			; restore bytes to check
	shr	cx,1			; get words to check, odd byte accounted for
	mov	ax,cx			; save words to check
	and	cx,1			; get odd word
	jcxz	sds3			; no odd word
	cmpsw
	jne	sdsnextent		; no match
sds3:
	mov	cx,ax			; restore words to check
	shr	cx,1			; get dwords to check, odd word accounted for
	jcxz	sdsmatch
	repe	cmpsd
	je	sdsmatch			; match

sdsnextent:
	dec	dx				; drop count of entries to check in block
	jne	sdsentloop		; more entries remain

sdsnextblk:
	mov	ax,gs:[GrpDefBlkStruc.gdbNextPtr]	; ax -> next block, if any
	add	bx,SIZE GrpDefRecStruc	; point to next entry
	or	ax,ax
	jne	sdsmainloop		; next block exists
	jmp	SHORT sds4		; no next block, no DGROUP

; found DGROUP, gs:bx -> DGROUP entry
sdsmatch:
	mov	WORD PTR DGROUPPtr,bx
	mov	WORD PTR DGROUPPtr+2,gs

; DGROUP pointer found or nonexistent
sds4:
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	cmp	WORD PTR _EDATASegDefPtr+2,0	; see if BSS segment exists
	je	sds_end			; no

; check if _edata or _EDATA symbol already exists and is public or common
	mov	SearchExistSymFlag,ON	; flag only search for existence
	mov	si,OFFSET DGROUP:Lower_edataText
	push	ds
	pop	fs				; fs:si -> symbol
	call	GetPubSymEntry
	jnc	sds_edataex		; _edata symbol exists
	mov	si,OFFSET DGROUP:Upper_EDATAText
	call	GetPubSymEntry
	jnc	sds_edataex		; _edata symbol exists

; symbol doesn't exist, create it
	mov	SearchExistSymFlag,OFF	; flag creation of entry for new symbol
	call	GetPubSymEntry

; gs:di -> public entry
sds_edataex:
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	sds__edata		; symbol already exists and is defined
	xor	eax,eax			; set variables and pointers in entry
	mov	gs:[di+PubSymRecStruc.pssOffset],eax

	mov	eax,DGROUPPtr
	mov	gs:[di+PubSymRecStruc.pssGrpDefPtr],eax
	or	eax,eax			; see if DGROUP
	je	sdsmod1			; no
	or	gs:[di+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG

sdsmod1:
	mov	gs:[di+PubSymRecStruc.pssModuleCount],-1
	lfs	bx,_EDATASegDefPtr	; fs:bx -> master segdef of _edata symbol
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssFirstIndSegPtr]	; eax -> individual segdef entry
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax
	or	gs:[di+PubSymRecStruc.pssFlags],PUBLICSYMBOLFLAG

; check if __edata or __EDATA symbol already exists and is public or common
sds__edata:
	mov	SearchExistSymFlag,ON	; flag only search for existence
	mov	si,OFFSET DGROUP:Lower__edataText
	push	ds
	pop	fs				; fs:si -> symbol
	call	GetPubSymEntry
	jnc	sds__edataex		; __edata symbol exists
	mov	si,OFFSET DGROUP:Upper__EDATAText
	call	GetPubSymEntry
	jnc	sds__edataex		; __edata symbol exists

; symbol doesn't exist, create it
	mov	SearchExistSymFlag,OFF	; flag creation of entry for new symbol
	call	GetPubSymEntry

sds__edataex:
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	sds_end			; symbol already exists and is defined
	xor	eax,eax			; set variables and pointers in entry
	mov	gs:[di+PubSymRecStruc.pssOffset],eax

	mov	eax,DGROUPPtr
	mov	gs:[di+PubSymRecStruc.pssGrpDefPtr],eax
	or	eax,eax			; see if DGROUP
	je	sdsmod1_		; no
	or	gs:[di+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG

sdsmod1_:
	mov	gs:[di+PubSymRecStruc.pssModuleCount],-1
	lfs	bx,_EDATASegDefPtr	; fs:bx -> master segdef of __edata symbol
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssFirstIndSegPtr]	; eax -> individual segdef entry
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax
	or	gs:[di+PubSymRecStruc.pssFlags],PUBLICSYMBOLFLAG

sds_end:
	cmp	WORD PTR _ENDSegDefPtr+2,0	; see if STACK segment exists
	je	sdsret			; no

; check if _end or _END symbol already exists and is public or common
	mov	SearchExistSymFlag,ON	; flag only search for existence
	mov	si,OFFSET DGROUP:Lower_endText
	push	ds
	pop	fs				; fs:si -> symbol
	call	GetPubSymEntry
	jnc	sds_endex		; _end symbol exists
	mov	si,OFFSET DGROUP:Upper_ENDText
	call	GetPubSymEntry
	jnc	sds_endex		; _end symbol exists

; symbol doesn't exist, create it
	mov	SearchExistSymFlag,OFF	; flag creation of entry for new symbol
	call	GetPubSymEntry

; gs:di -> public entry
sds_endex:
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	sds__end		; symbol already exists and is defined
	xor	eax,eax			; set variables and pointers in entry
	mov	gs:[di+PubSymRecStruc.pssOffset],eax

	mov	eax,DGROUPPtr
	mov	gs:[di+PubSymRecStruc.pssGrpDefPtr],eax
	or	eax,eax			; see if DGROUP
	je	sdsmod2			; no
	or	gs:[di+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG

sdsmod2:
	mov	gs:[di+PubSymRecStruc.pssModuleCount],-1
	lfs	bx,_ENDSegDefPtr	; fs:bx -> master segdef of _end symbol
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssFirstIndSegPtr]	; eax -> individual segdef entry
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax
	or	gs:[di+PubSymRecStruc.pssFlags],PUBLICSYMBOLFLAG

; check if __end or __END symbol already exists and is public or common
sds__end:
	mov	SearchExistSymFlag,ON	; flag only search for existence
	mov	si,OFFSET DGROUP:Lower__endText
	push	ds
	pop	fs				; fs:si -> symbol
	call	GetPubSymEntry
	jnc	sds__endex		; __end symbol exists
	mov	si,OFFSET DGROUP:Upper__ENDText
	call	GetPubSymEntry
	jnc	sds__endex		; __end symbol exists

; symbol doesn't exist, create it
	mov	SearchExistSymFlag,OFF	; flag creation of entry for new symbol
	call	GetPubSymEntry

sds__endex:
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	jne	sdsret			; symbol already exists and is defined
	xor	eax,eax			; set variables and pointers in entry
	mov	gs:[di+PubSymRecStruc.pssOffset],eax

	mov	eax,DGROUPPtr
	mov	gs:[di+PubSymRecStruc.pssGrpDefPtr],eax
	or	eax,eax			; see if DGROUP
	je	sdsmod2_		; no
	or	gs:[di+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG

sdsmod2_:
	mov	gs:[di+PubSymRecStruc.pssModuleCount],-1
	lfs	bx,_ENDSegDefPtr	; fs:bx -> master segdef of __end symbol
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssFirstIndSegPtr]	; eax -> individual segdef entry
	mov	gs:[di+PubSymRecStruc.pssIndSegDefPtr],eax
	or	gs:[di+PubSymRecStruc.pssFlags],PUBLICSYMBOLFLAG

sdsret:
	ret
SetDOSSEGSymbols	ENDP

;*****************************
;* DOSSEGTEXTSEGADJUST       *
;*****************************

; dosseg flag is set, adjust _TEXT segment with 16 prefixed zero bytes
; destroy all registers except ds -> DGROUP

DOSSEGTextSegAdjust	PROC
	mov	ax,FirstMasterSegDefBlkPtr

dtsblkloop:
	mov	fs,ax			; fs -> current master segdef blk
	mov	dx,fs:[MasterSegDefBlkStruc.msdbCount]	; get count of entries in block
	mov	bx,MASTERSEGDEFSYSVARSIZE	; fs:bx -> initial master segdef entry

dtsentloop:
	or	dx,dx			; see if any more entries in block
	je	dtsnextblk		; no match found, try next block, if any

	mov	si,OFFSET DGROUP:_TEXTText	; ds:si -> source string
	les	di,fs:[bx+MasterSegDefRecStruc.mssNamePtr]	; es:di -> segment on record
	cmp	di,SIZEIOBUFFBLK-MAXOBJRECNAME	; see if normalization might be needed
	jb	dtslencmp		; no
	call	NormalESDIDest	; normalize es:di to destination name buffer

; ds:si -> string to match, es:di -> string to try
dtslencmp:
	cmpsb				; see if length byte matches
	jne	dtsnextent		; no

	mov	cl,ds:[si-1]
	xor	ch,ch			; get # of bytes to check
	mov	ax,cx			; save count of bytes
	and	cx,1			; get odd byte
	jcxz	dts2		; no odd byte
	cmpsb
	jne	dtsnextent		; no match
dts2:
	mov	cx,ax			; restore bytes to check
	shr	cx,1			; get words to check, odd byte accounted for
	mov	ax,cx			; save words to check
	and	cx,1			; get odd word
	jcxz	dts3			; no odd word
	cmpsw
	jne	dtsnextent		; no match
dts3:
	mov	cx,ax			; restore words to check
	shr	cx,1			; get dwords to check, odd word accounted for
	jcxz	dtsmatch
	repe	cmpsd
	je	dtsmatch			; match

dtsnextent:
	add	bx,SIZE MasterSegDefRecStruc	; move to next entry in block
	dec	dx				; drop count of entries to check
	jmp	SHORT dtsentloop

dtsnextblk:
	mov	ax,fs:[MasterSegDefBlkStruc.msdbNextPtr]	; get next block, if any
	or	ax,ax			; see if exists
	jne	dtsblkloop		; yes

dtsret:
	ret

; found _TEXT segment, add 16 to total segment length, 16 to each individual segdef offset
; fs:bx -> master segdef entry
dtsmatch:
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegLength]
	mov	ecx,16			; use as constant 16 dword value
	add	eax,ecx
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG
	jne	dtsnochk		; no check on overflow with 32-bit segments
	cmp	eax,65536		; see if overflow on _TEXT segment
	ja	dtsof			; yes

dtsnochk:
	mov	fs:[bx+MasterSegDefRecStruc.mssSegLength],eax
	mov	eax,16
	lfs	bx,fs:[bx+MasterSegDefRecStruc.mssFirstIndSegPtr]	; init fs:bx -> first individual segment entry

; ecx == 16
; fs:bx -> individual segment entry
; add 16 to the offset of each individual segment
dtsdosloop:
	add	fs:[bx+IndSegDefRecStruc.isdrSegOffset],ecx
	cmp	fs:[bx+WORD PTR IndSegDefRecStruc.isdrNextIndSegPtr+2],0	; see if another entry
	je	dtsret			; no, all updated
	lfs	bx,fs:[bx+IndSegDefRecStruc.isdrNextIndSegPtr]	; fs:bx -> next entry
	jmp	SHORT dtsdosloop

; overflow of 64K on 16-bit segment
dtsof:
	lgs	bx,fs:[bx+MasterSegDefRecStruc.mssNamePtr]	; gs:bx -> nonnormalized text string
	mov	al,SEGLEN64KERRORCODE	; flag segment length >64K
	call	NormalizeErrorExit	; normalize text string, do linker error exit

DOSSEGTextSegAdjust	ENDP

;*****************************
;* RESOLVECOMDATS            *
;*****************************

; resolve comdat variable addresses
; adjust or create segments as necessary

ResolveCOMDATs	PROC
	mov	ax,FirstComDatBlkPtr	; ax -> first symbol block

rcdblkloop:
	or	ax,ax			; see if next symbol block exists
	je	rcdret			; no
	mov	fs,ax			; fs -> symbol block

	mov	bx,COMDATSYSVARSIZE	; fs:bx -> first symbol entry in block
	xor	dx,dx			; init symbol entry being scanned

rcdentloop:
	cmp	dx,MAXCOUNTCOMDATBLK	; see if room for more entries in block (might not be all used)
	jae	rcdnextblk		; no
	cmp	dx,fs:[ComDatBlkStruc.cdbCount]	; see if end entry in block
	jae	rcdnextblk		; yes, try next block, if any (shouldn't be)

	push	dx			; save critical registers
	push	fs
	push	bx

	test	fs:[bx+ComDatRecStruc.cdsFlags],SEGRELCOMDATFLAG
	jne	rcdsegrel		; COMDAT is explicit, segment-based

; init variables for segdef procedure calls
	mov	Is32BitSeg,OFF
	mov	IsAbsoluteSeg,OFF
	mov	al,fs:[bx+ComDatRecStruc.cdsAlign]
	shl	al,5			; get align type in proper bits
	mov	ACBPByte,al

	mov	eax,-1
	mov	ModuleCount,eax
	mov	CurrentBaseOBJBuff,ax
	mov	ClassNameIndex,1
	mov	SegmentNameIndex,2
	mov	GroupNameIndex,0
	mov	gs,LNAMESIndexSel
	mov	WORD PTR gs:[0],OFFSET DGROUP:CODEText
	mov	WORD PTR gs:[4],OFFSET DGROUP:COMDAT_SEGText
	mov	ax,DGROUP
	mov	WORD PTR gs:[2],ax
	mov	WORD PTR gs:[6],ax
	mov	eax,fs:[bx+ComDatRecStruc.cdsLength]	; get comdat length
	mov	SegmentLength,eax

	add	eax,PrevFarCodeComDatLen	; add in previous communal lengths
	cmp	eax,65536		; see if >64K total length
	jae	rcdnewseg		; yes, create new segment for far code comdats

	cmp	WORD PTR FarCodeComDatSegDefPtr+2,0	; see if segdef entry allocated yet for far code comdats
	jne	rcd2			; yes

; create segment for new communals
rcdnewseg:
	mov	PrevFarCodeComDatLen,0	; init previous far communal length

; create far communal segdef entry
; segment class CODE, no group
	mov	ACBPByte,01100000b	; para align, private combine
	call	GetMasterSegDefEntry	; return -> master segment definition entry in gs:di
	mov	WORD PTR FarCodeComDatSegDefPtr,di
	mov	WORD PTR FarCodeComDatSegDefPtr+2,gs
	or	gs:[di+MasterSegDefRecStruc.mssFlags],ASSOCIATEDDATAFLAG	; flag associated data

; create segment partition entry
rcd2:
	lgs	di,FarCodeComDatSegDefPtr	; gs:di -> master segdef
	call	MakeIndSegDefEntry	; make the individual segdef record entry

	lfs	bx,gs:[di+MasterSegDefRecStruc.mssLastIndSegPtr]	; get created entry
	or	fs:[bx+IndSegDefRecStruc.isdrFlags],(CREATEDSEGMENTFLAG OR COMDATSEGMENTFLAG)
	pop	eax				; eax -> comdat entry
	push	eax
	mov	fs:[bx+IndSegDefRecStruc.isdrModulePtr],eax	; save pointer to owning comdat

	pop	bx				; fs:bx -> comdat entry
	pop	fs
	mov	edx,fs:[bx+ComDatRecStruc.cdsLength]	; get comdat length
	mov	eax,PrevFarCodeComDatLen
	add	eax,edx				; compute new previous far code comdat length
	mov	PrevFarCodeComDatLen,eax	; save it

rcdgetcptr:
	push	fs
	push	bx			; save -> comdat entry
	mov	eax,gs:[di+MasterSegDefRecStruc.mssLastIndSegPtr]	; save pointer to created segdef entry
	mov	fs:[bx+ComDatRecStruc.cdsCreatedSegDefPtr],eax
	lfs	bx,fs:[bx+ComDatRecStruc.cdsPubSymPtr]	; fs:bx -> comdat symbol
	mov	fs:[bx+PubSymRecStruc.pssIndSegDefPtr],eax
	mov	DWORD PTR fs:[bx+PubSymRecStruc.pssOffset],0	;init public offset

	pop	bx				; restore fs:bx -> comdat entry
	pop	fs
	pop	dx				; restore critical register

rcdnextent:
	add	bx,SIZE ComDatRecStruc	; fs:bx -> next comdat entry
	inc	dx				; bump current entry count
	jmp	rcdentloop

rcdnextblk:
	mov	ax,fs:[ComDatBlkStruc.cdbNextPtr]	; ax -> next block
	jmp	rcdblkloop

; COMDAT is explicit, segment-based
rcdsegrel:
	mov	eax,-1
	mov	ModuleCount,eax
	mov	CurrentBaseOBJBuff,ax
	mov	eax,fs:[bx+ComDatRecStruc.cdsLength]	; get comdat length
	mov	SegmentLength,eax
	mov	IsAbsoluteSeg,OFF

	mov	al,fs:[bx+ComDatRecStruc.cdsAlign]
	lfs	bx,fs:[bx+ComDatRecStruc.cdsIndSegDefPtr]	; fs:bx -> owning segdef
	shl	al,5			; get align type in proper bits
	jne	rcdsegacbp		; alignment not from segment (nonzero)
	mov	al,fs:[bx+IndSegDefRecStruc.isdrACBPByte]	; get align from segdef

; fs:bx -> owning segment partition entry
rcdsegacbp:
	mov	ACBPByte,al		; save ACBP
	lgs	di,fs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; gs:di -> master segdef entry
	call	MakeIndSegDefEntry	; make the individual segdef record entry corresponding to COMDAT

	lfs	bx,gs:[di+MasterSegDefRecStruc.mssLastIndSegPtr]	; get created entry
	or	fs:[bx+IndSegDefRecStruc.isdrFlags],(CREATEDSEGMENTFLAG OR COMDATSEGMENTFLAG)
	pop	eax				; eax -> comdat entry
	push	eax
	mov	fs:[bx+IndSegDefRecStruc.isdrModulePtr],eax	; save pointer to owning comdat

	pop	bx				; fs:bx -> comdat entry
	pop	fs
	jmp	rcdgetcptr

rcdret:
	ret
ResolveCOMDATs	ENDP

;*****************************
;* RESOLVECOMMUNALS          *
;*****************************

; resolve communal variable addresses
; adjust or create segments as necessary

ResolveCommunals	PROC
	cmp	TotalCommunalCount,0	; see if any communals to resolve
	je	rcret			; no

	mov	eax,TotalCommunalCount
	mov	RunningCommunalCount,eax
	mov	ax,FirstPubSymBlkPtr	; ax -> first symbol block

rcblkloop:
	or	ax,ax			; see if next symbol block exists
	je	rcret			; no
	mov	fs,ax			; fs -> symbol block

	mov	bx,PUBSYMSYSVARSIZE	; fs:bx -> first symbol entry in block
	xor	dx,dx			; init symbol entry being scanned
	cmp	RunningCommunalCount,0	; see if any communals left to resolve
	je	rcret			; no

rcentloop:
	cmp	dx,MAXCOUNTPUBSYMBLK	; see if room for more entries in block (might not be all used)
	jae	rcnextblk		; no
	cmp	dx,fs:[PubSymBlkStruc.psbCount]	; see if end entry in block
	jae	rcnextblk		; yes, try next block, if any (shouldn't be)
	test	fs:[bx+PubSymRecStruc.pssFlags],(NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	rcnextent		; not a communal variable

; communal variable
	test	fs:[bx+PubSymRecStruc.pssFlags],FARCOMSYMBOLFLAG	; see if far
	jne	rcfar			; far communal
	call	ResolveNearCommunal	; near communal
	dec	RunningCommunalCount	; drop count of communals, allow testing short-circuit

rcnextent:
	add	bx,SIZE PubSymRecStruc	; fs:bx -> next symbol entry
	inc	dx				; bump current entry count
	jmp	SHORT rcentloop

; far communal
rcfar:
	call	ResolveFarCommunal
	dec	RunningCommunalCount	; drop count of communals, allow testing short-circuit
	jmp	SHORT rcnextent

rcnextblk:
	mov	ax,fs:[MasterSegDefBlkStruc.msdbNextPtr]	; ax -> next block
	jmp	rcblkloop

rcret:
	ret
ResolveCommunals	ENDP

;*****************************
;* RESOLVENEARCOMMUNAL       *
;*****************************

; resolve near communal
; upon entry fs:bx -> symbol entry
; conserve bx,dx,fs

ResolveNearCommunal	PROC
	push	dx			; save critical registers
	push	bx

; init variables for segdef procedure calls
	mov	Is32BitSeg,OFF
	mov	IsAbsoluteSeg,OFF
	mov	ACBPByte,01000000b	; word align, private combine
	mov	eax,-1
	mov	ModuleCount,eax
	mov	CurrentBaseOBJBuff,ax
	mov	ClassNameIndex,1
	mov	SegmentNameIndex,2
	mov	GroupNameIndex,3
	mov	gs,LNAMESIndexSel
	mov	WORD PTR gs:[0],OFFSET DGROUP:BSSText
	mov	WORD PTR gs:[4],OFFSET DGROUP:c_commonText
	mov	WORD PTR gs:[8],OFFSET DGROUP:DGROUPText
	mov	ax,DGROUP
	mov	WORD PTR gs:[2],ax
	mov	WORD PTR gs:[6],ax
	mov	WORD PTR gs:[10],ax
	mov	eax,fs:[bx+PubSymRecStruc.pssOffset]	; get communal length
	mov	SegmentLength,eax
	mov	fs:[bx+PubSymRecStruc.pssOffset],0	; zero public offset

	cmp	WORD PTR NearCommSegDefPtr+2,0	; see if segdef entry allocated yet for near communals
	jne	rnc2			; yes

; create DGROUP if doesn't exist
	call	GetGrpDefEntry	; return grpdef pointer in gs:di
	call	SetGrpPtrTableEntry
	push	gs			; save -> group on stack
	push	di

; create near communal segdef entry
; segment c_common, class BSS, group DGROUP
	mov	ACBPByte,01100000b	; para align, private combine
	call	GetMasterSegDefEntry	; return -> master segment definition entry in gs:di
	mov	WORD PTR NearCommSegDefPtr,di
	mov	WORD PTR NearCommSegDefPtr+2,gs
	pop	eax				; eax -> group
	mov	gs:[di+MasterSegDefRecStruc.mssGroupPtr],eax	; update group pointer
	or	gs:[di+MasterSegDefRecStruc.mssFlags],GROUPMEMBERFLAG	; flag member of group

; create segment partition entry
rnc2:
	lgs	di,NearCommSegDefPtr	; gs:di -> master segdef
	call	MakeIndSegDefEntry	; make the individual segdef record entry
	push	fs
	lfs	bx,gs:[di+MasterSegDefRecStruc.mssLastIndSegPtr]	; get created entry
	or	fs:[bx+IndSegDefRecStruc.isdrFlags],CREATEDSEGMENTFLAG	; flag created segment
	pop	fs

; set individual segdef and group pointers for symbol entry, so flag
	pop	bx				; fs:bx -> symbol entry
	mov	eax,gs:[di+MasterSegDefRecStruc.mssLastIndSegPtr]
	mov	fs:[bx+PubSymRecStruc.pssIndSegDefPtr],eax
	mov	eax,gs:[di+MasterSegDefRecStruc.mssGroupPtr]
	mov	fs:[bx+PubSymRecStruc.pssGrpDefPtr],eax
	or	fs:[bx+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG

	pop	dx				; restore critical register
	ret
ResolveNearCommunal	ENDP

;*****************************
;* RESOLVEFARCOMMUNAL        *
;*****************************

; resolve far communal
; upon entry fs:bx -> symbol entry
; conserve bx,dx,fs

ResolveFarCommunal	PROC
	push	dx			; save critical registers
	push	bx

; init variables for segdef procedure calls
	mov	Is32BitSeg,OFF
	mov	IsAbsoluteSeg,OFF
	mov	ACBPByte,01000000b	; word align, private combine
	mov	eax,-1
	mov	ModuleCount,eax
	mov	CurrentBaseOBJBuff,ax
	mov	ClassNameIndex,1
	mov	SegmentNameIndex,2
	mov	GroupNameIndex,0
	mov	gs,LNAMESIndexSel
	mov	WORD PTR gs:[0],OFFSET DGROUP:FAR_BSSText
	mov	WORD PTR gs:[4],OFFSET DGROUP:FAR_BSSText
	mov	ax,DGROUP
	mov	WORD PTR gs:[2],ax
	mov	WORD PTR gs:[6],ax
	mov	eax,fs:[bx+PubSymRecStruc.pssOffset]	; get communal length
	mov	SegmentLength,eax

	cmp	eax,65536		; see if huge (>64K) segment
	jae	rfchuge			; huge

; far communal variable (<64K)
	add	eax,PrevFarCommunalLen	; add in previous communal lengths
	cmp	eax,65536		; see if >64K total length
	jae	rfcnewseg		; yes, create new segment for far communals

	cmp	WORD PTR FarCommSegDefPtr+2,0	; see if segdef entry allocated yet for far communals
	jne	rfc2			; yes

; create segment for new communals
rfcnewseg:
	mov	PrevFarCommunalLen,0	; init previous far communal length

; create far communal segdef entry
; segment class FAR_BSS, no group
	mov	ACBPByte,01100000b	; para align, private combine
	call	GetMasterSegDefEntry	; return -> master segment definition entry in gs:di
	mov	WORD PTR FarCommSegDefPtr,di
	mov	WORD PTR FarCommSegDefPtr+2,gs

; create segment partition entry
rfc2:
	lgs	di,FarCommSegDefPtr	; gs:di -> master segdef
	call	MakeIndSegDefEntry	; make the individual segdef record entry
	push	fs
	lfs	bx,gs:[di+MasterSegDefRecStruc.mssLastIndSegPtr]	; get created entry
	or	fs:[bx+IndSegDefRecStruc.isdrFlags],CREATEDSEGMENTFLAG	; flag created segment
	pop	fs

	pop	bx				; fs:bx -> symbol entry
	push	bx			; save back to stack
	mov	eax,gs:[di+MasterSegDefRecStruc.mssLastIndSegPtr]	; save pointer to individual segdef entry
	mov	fs:[bx+PubSymRecStruc.pssIndSegDefPtr],eax
	mov	edx,fs:[bx+PubSymRecStruc.pssOffset]	; get communal length
	mov	eax,PrevFarCommunalLen

; set to zero because segment partition adjusts??? MED 02/14/94
;	mov	fs:[bx+PubSymRecStruc.pssOffset],eax	;init public offset
	mov	DWORD PTR fs:[bx+PubSymRecStruc.pssOffset],0	;init public offset

	add	eax,edx				; compute new previous far communal length
	mov	PrevFarCommunalLen,eax	; save it
	jmp	rfcret

rfchuge:
	push	eax			; dword communal length to stack
	pop	dx				; get low word length (64K segment remainder)
	pop	cx				; cx holds # of 64K segments to create
	inc	cx				; adjust for remainder segment

; init variables for segdef procedure calls
	mov	Is32BitSeg,OFF
	mov	IsAbsoluteSeg,OFF
	mov	ACBPByte,01100000b	; para align, private combine
	mov	eax,-1
	mov	ModuleCount,eax
	mov	CurrentBaseOBJBuff,ax
	mov	ClassNameIndex,1
	mov	SegmentNameIndex,2
	mov	GroupNameIndex,0
	mov	gs,LNAMESIndexSel
	mov	WORD PTR gs:[0],OFFSET DGROUP:HUGE_BSSText
	mov	WORD PTR gs:[4],OFFSET DGROUP:HUGE_BSSText
	mov	ax,DGROUP
	mov	WORD PTR gs:[2],ax
	mov	WORD PTR gs:[6],ax
	mov	SegmentLength,65536	; init segment length
	xor	eax,eax
	mov	fs:[bx+PubSymRecStruc.pssOffset],eax	; init public offset
	mov	fs:[bx+PubSymRecStruc.pssIndSegDefPtr],eax	; init individual segdef pointer

rfchugeloop:
	cmp	cx,1			; see if last segment creating loop
	jne	rfccreate		; no, use segment length of 64K
	or	dx,dx			; see if any remainder segment
	je	rfcret			; no
	movzx	edx,dx
	mov	SegmentLength,edx	; segment has remainder length

; create huge communal segdef entry
; segment class HUGE_BSS, no group
rfccreate:
	push	dx			; save critical registers
	push	cx
	call	GetMasterSegDefEntry	; return -> master segment definition entry in gs:di

; create individual segment entry
	call	MakeIndSegDefEntry	; make the individual segdef record entry
	push	fs
	lfs	bx,gs:[di+MasterSegDefRecStruc.mssLastIndSegPtr]	; get created entry
	or	fs:[bx+IndSegDefRecStruc.isdrFlags],CREATEDSEGMENTFLAG	; flag created segment
	pop	fs
	pop	cx				; restore critical registers
	pop	dx
	pop	bx				; fs:bx -> symbol entry
	push	bx			; save back to stack
	cmp	fs:[bx+PubSymRecStruc.pssIndSegDefPtr],0	; see if pointer already assigned
	jne	rfcnext			; yes, don't reassign
	mov	eax,gs:[di+MasterSegDefRecStruc.mssLastIndSegPtr]	; save pointer to individual segdef entry
	mov	fs:[bx+PubSymRecStruc.pssIndSegDefPtr],eax

rfcnext:
	loop	rfchugeloop	; create 64K segments+remainder for size of communal

rfcret:
	pop	bx				; restore critical register
	pop	dx
	ret
ResolveFarCommunal	ENDP

;*****************************
;* COMPUTESEGORDER           *
;*****************************

; compute the proper segment order for program
; destroy all registers except ds -> DGROUP

ComputeSegOrder	PROC
	mov	ax,TotalRelSegCount
	mov	UnresolvedSegCount,ax	; init count of unresolved segments
	mov	ResOccurredFlag,1	; init resolution occurred flag for entry

csoresloop:
	xor	ax,ax
	cmp	UnresolvedSegCount,ax	; see if all segments resolved
	je	csoresdone		; yes
	cmp	ResOccurredFlag,al	; see if segment resolved this pass
	jne	csorespass		; yes
	cmp	IsDOSSEG,al		; no, see if dosseg segment ordering
	je	internal2		; no, internal error
	inc	DOSSEGPhase		; bump phase of DOSSEG ordering
	cmp	DOSSEGPhase,7	; see if phase out of bounds (final DOSSEG phase complete)
	jae	internal2		; yes, internal error

csorespass:
	mov	ResOccurredFlag,al	; init segment resolution flag
	mov	ClassSelectedFlag,al	; init class selected flag
	mov	ax,FirstMasterSegDefBlkPtr	; ax -> segdef block to check

csomastblkloop:
	or	ax,ax			; see if next master segdef block exists
	je	csoresloop		; no, move to next segment to resolve
	mov	fs,ax			; fs -> current resolution check master segdef block
	mov	bx,MASTERSEGDEFSYSVARSIZE	; fs:bx -> first master segdef entry in block
	mov	CheckMasterEntry,0	; init master segdef entry being checked for resolution

csomastentloop:
	mov	ax,CheckMasterEntry	; get current check entry number
	cmp	ax,MAXCOUNTMASTERSEGDEFBLK	; see if room for more entries in block (might not be all used)
	jae	csonextblk		; no
	cmp	ax,fs:[MasterSegDefBlkStruc.msdbCount]	; see if end entry in block
	jae	csonextblk		; yes, try next block, if any (shouldn't be)
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],ABSOLUTESEGMENTFLAG	; see if absolute segment
	jne	csonextent		; don't resolve absolute segments
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],RESOLVEDSEGFLAG	; see if segment already resolved
	je	csonotres		; not resolved yet

csonextent:
	add	bx,SIZE MasterSegDefRecStruc	; fs:bx -> next master segdef entry
	inc	CheckMasterEntry	; track current entry count
	jmp	SHORT csomastentloop

; fs:bx -> master segdef entry
csonotres:
	cmp	ClassSelectedFlag,OFF	; see if class selected
	jne	csoclasssel		; class has been selected

	les	di,fs:[bx+MasterSegDefRecStruc.mssClassPtr]	; es:di -> current class name
	cmp	di,SIZEIOBUFFBLK-MAXOBJRECNAME	; see if name might overflow
	jb	cso2			; no
	call	NormalESDIDest	; normalize es:di to destination name buffer

; es:di -> current class name
cso2:
	cmp	IsDOSSEG,0		; see if DOSSEG segment ordering
	je	csosetclass		; no
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],DOSSEGFLAGBITS	; see if DOSSEG flag bits set
	jne	csophcmp		; yes, check phases for match

	call	GetDOSSEGType	; get class DOSSEG type, return in al
	cmp	al,1			; see if code
	jne	cso3			; no
	mov	ax,DOSSEGCODEFLAG	; set appropriate bits
	jmp	SHORT csosetflag

cso3:
	cmp	al,2			; see if not dgroup
	jne	cso4			; no
	mov	ax,DOSSEGNOTDGROUPFLAG
	jmp	SHORT csosetflag

cso4:
	cmp	al,3			; see if begdata
	jne	cso5			; no
	mov	ax,DOSSEGBEGDATAFLAG
	jmp	SHORT csosetflag

cso5:
	cmp	al,4			; see if other dgroup
	jne	cso6			; no
	mov	ax,DOSSEGOTHERDGROUPFLAG
	jmp	SHORT csosetflag

cso6:
	cmp	al,5			; see if bss
	jne	cso7			; no
	mov	ax,DOSSEGBSSFLAG
	jmp	csobss	; special bss handling

cso7:
	mov	ax,DOSSEGSTACKFLAG

; set the appropriate DOSSEG flag bits, ax holds value
csosetflag:
	or	fs:[bx+MasterSegDefRecStruc.mssFlags],ax

csophcmp:
	mov	ax,fs:[bx+MasterSegDefRecStruc.mssFlags]	; get flag values
	and	ax,DOSSEGFLAGBITS	; mask off all but DOSSEG bits (DOSSEG phase*256)
	cmp	ah,DOSSEGPhase	; see if phase matches type (ah compare does 256 x value)
	jne	csonextent		; no, try next entry

; set selected class to current segdef's class
csosetclass:
	mov	ClassSelectedFlag,ON	; set class selected flag
	mov	WORD PTR CurrentResClassPtr,di	; keep pointer to current resolution class name
	mov	WORD PTR CurrentResClassPtr+2,es
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssGroupPtr]
	mov	CurrentResGrpPtr,eax	; save -> current group pointer

; resolve segment
; fs:bx -> master segdef entry
; es:di -> normalized class name
csodores:
	mov	ResOccurredFlag,1	; flag resolution occurred this pass
	dec	UnresolvedSegCount	; drop count of unresolved segments
	or	fs:[bx+MasterSegDefRecStruc.mssFlags],RESOLVEDSEGFLAG	; flag segment resolved
	mov	ax,SegmentID	; set, update segment identifier
	mov	fs:[bx+MasterSegDefRecStruc.mssSegmentID],ax
	inc	SegmentID
	cmp	WORD PTR LastSegDefPtr+2,0	; see if first segdef
	jne	csouplast		; no
	mov	WORD PTR FirstSegment,bx	; keep pointer to first segment in program
	mov	WORD PTR FirstSegment+2,fs
	jmp	SHORT csosetlast

; update previously last segdef pointer
csouplast:
	lgs	di,LastSegDefPtr	; gs:di -> previously last segdef
	mov	gs:[di+WORD PTR MasterSegDefRecStruc.mssNextSegPtr],bx
	mov	gs:[di+WORD PTR MasterSegDefRecStruc.mssNextSegPtr+2],fs

csosetlast:
	mov	WORD PTR LastSegDefPtr,bx	; keep pointer to last segdef in program
	mov	WORD PTR LastSegDefPtr+2,fs

; see if first stack segment, if so, set StackSegFoundFlag and save pointer in _ENDSegDefPtr
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],STACKSEGMENTFLAG
	je	csosegaddr		; not a stack segment
	cmp	StackSegFoundFlag,0	; see if stack segment already found
	jne	csosegaddr		; yes
	mov	StackSegFoundFlag,1	; flag stack segment found
	mov	WORD PTR _ENDSegDefPtr,bx	; save pointer to segdef of beginning STACK address
	mov	WORD PTR _ENDSegDefPTr+2,fs

	cmp	IsStackOption,OFF	; see if stack size set by user
	je	csosegaddr		; no
	mov	eax,StackValue
IFDEF CLIPPER
	cmp	eax,2048		; let boneheads have down to a 2K stack with Clipper
ELSE
	cmp	eax,fs:[bx+MasterSegDefRecStruc.mssSegLength]	; see if option stack > program stack
ENDIF

	jbe	csosegaddr		; no, use program set stack
	mov	fs:[bx+MasterSegDefRecStruc.mssSegLength],eax	; use user option set stack length

; resolve segment address
; fs:bx -> master segdef entry
csosegaddr:

; check if debug information segment:
;   name $$SYMBOLS, class DEBSYM, combine PRIVATE or
;   name $$TYPES, class DEBTYP, combine PRIVATE
	mov	al,fs:[bx+MasterSegDefRecStruc.mssACBPByte]	; get acbp byte
	and	al,CFIELDOFACBP	; get combine field of current segment
	jne	csonotdebug		; public, not debug

	les	di,CurrentResClassPtr	; es:di -> class name
	mov	eax,DWORD PTR DEBSYMText
	cmp	eax,es:[di]
	jne	csochktyp		; not DEBSYM type
	mov	ax,WORD PTR DEBSYMText+4
	cmp	ax,es:[di+4]
	jne	csochktyp		; not DEBSYM
	mov	al,DEBSYMText+6
	cmp	al,es:[di+6]
	jne	csochktyp		; not DEBSYM

	les	di,fs:[bx+MasterSegDefRecStruc.mssNamePtr]	; es:di -> segment name
	cmp	di,SIZEIOBUFFBLK-MAXOBJRECNAME	; see if name might overflow
	jb	csodebug2		; no
	call	NormalESDIDest	; normalize es:di to destination name buffer

csodebug2:
	mov	eax,DWORD PTR $$SYMBOLSText
	cmp	eax,es:[di]
	jne	csochktyp		; not $$SYMBOLS name
	mov	eax,DWORD PTR $$SYMBOLSText+4
	cmp	eax,es:[di+4]
	jne	csonotdebug		; not $$SYMBOLS, not $$TYPES
	mov	ax,WORD PTR $$SYMBOLSText+8
	cmp	ax,es:[di+8]
	jne	csonotdebug		; not $$SYMBOLS, not $$TYPES

; is $$SYMBOLS debug segment
	or	fs:[bx+MasterSegDefRecStruc.mssFlags],DEBUGSYMBOLSSEGMENTFLAG	; flag debug
	jmp	SHORT csoisdebug

csochktyp:
	les	di,CurrentResClassPtr	; es:di -> class name
	mov	eax,DWORD PTR DEBTYPText
	cmp	eax,es:[di]
	jne	csonotdebug		; not DEBTYP
	mov	ax,WORD PTR DEBTYPText+4
	cmp	ax,es:[di+4]
	jne	csonotdebug		; not DEBTYP
	mov	al,DEBTYPText+6
	cmp	al,es:[di+6]
	jne	csonotdebug		; not DEBTYP

	les	di,fs:[bx+MasterSegDefRecStruc.mssNamePtr]	; es:di -> segment name
	cmp	di,SIZEIOBUFFBLK-MAXOBJRECNAME	; see if name might overflow
	jb	csodebug3		; no
	call	NormalESDIDest	; normalize es:di to destination name buffer

csodebug3:
	mov	eax,DWORD PTR $$TYPESText
	cmp	eax,es:[di]
	jne	csonotdebug		; not $$TYPES name
	mov	eax,DWORD PTR $$TYPESText+4
	cmp	eax,es:[di+4]
	jne	csonotdebug		; not $$TYPES

; is $$TYPES debug segment
	or	fs:[bx+MasterSegDefRecStruc.mssFlags],DEBUGTYPESSEGMENTFLAG	; flag debug

; debug info segment
csoisdebug:
	inc	DebugSegmentCount	; bump count of debug segments
	dec	SegmentID		; reset segment identifier
	mov	fs:[bx+MasterSegDefRecStruc.mssSegmentID],-1

	mov	eax,SegStartOffset
	mov	fs:[bx+MasterSegDefRecStruc.mssSegOffset],eax	; set offset to current
	xor	eax,eax
	mov	fs:[bx+MasterSegDefRecStruc.mssSegLength],eax	; zero segment size
	jmp	csonextent	; move to next segdef entry

csonotdebug:
	xor	ax,ax			; init segment align adjustment
	mov	dh,fs:[bx+MasterSegDefRecStruc.mssACBPByte]	; get acbp byte
	and	dh,AFIELDOFACBP
	cmp	dh,BYTEALIGNSEGMENT	; see if byte aligned segment
	je	csoaladj		; yes, no adjustment needed
	mov	al,BYTE PTR SegStartOffset	; get segment start offset low byte
	xor	ah,ah
	sub	ah,al			; compute segment adjustment byte to 256 roundup
	mov	al,ah
	xor	ah,ah			; ax holds segment adjust value

	cmp	dh,WORDALIGNSEGMENT	; see if word aligned segment
	jne	csopara
	and	al,1			; align to word
	jmp	SHORT csoaladj

csopara:
	cmp	dh,PARAALIGNSEGMENT	; see if paragraph aligned segment
	jne	csodword			; no
	and	al,15			; align to para
	jmp	SHORT csoaladj

csodword:
	cmp	dh,DWORDALIGNSEGMENT	; see if dword aligned segment
	jne	csoaladj			; no
	and	al,3			; align to dword

; ax holds amount to add to segment start offset for alignment adjustment
; dl == ACBPByte
csoaladj:
	movzx	eax,ax
	add	eax,SegStartOffset
	mov	SegStartOffset,eax	; update segment start offset
	mov	fs:[bx+MasterSegDefRecStruc.mssSegOffset],eax
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],GROUPMEMBERFLAG
	je	csoupoff		; not a member of a group

; update group offset, if necessary
	lgs	si,fs:[bx+MasterSegDefRecStruc.mssGroupPtr]	; gs:si -> group
	test	gs:[si+GrpDefRecStruc.gdrGrpFlags],GRPOFFSETSETFLAG	; see if offset set for group
	jne	csogrplen		; yes

; If creating 3P file, make sure that first segment of group is PARA aligned
;  or better, 02/25/96
	cmp	IsCreateEXEOption,OFF	; see if creating EXE file
	jne	csosgo			; yes
	test	eax,0fh		; see if nonpara alignment
	je	csosgo			; no
	
	mov	dh,al
	and	dh,0fh			; isolate odd bytes past para align
	mov	dl,10h
	sub	dl,dh			; dl holds amount needed to pad segdef to next para
	movzx	edx,dl
	add	SegStartOffset,edx	; update segment start offset to next para
	add	fs:[bx+MasterSegDefRecStruc.mssSegOffset],edx
	add	eax,edx
	mov	dl,fs:[bx+MasterSegDefRecStruc.mssACBPByte]	; get master ACBP byte
	and	dl,NOT AFIELDOFACBP	; mask off alignment field of master segdef
	or	dl,PARAALIGNSEGMENT	; merge in new para alignment field

csosgo:
	mov	gs:[si+GrpDefRecStruc.gdrGrpOffset],eax	; set group offset
	or	gs:[si+GrpDefRecStruc.gdrGrpFlags],GRPOFFSETSETFLAG	; flag offset set
	mov	WORD PTR gs:[si+GrpDefRecStruc.gdrFirstSegPtr],bx	; save pointer to first segment
	mov	WORD PTR gs:[si+GrpDefRecStruc.gdrFirstSegPtr+2],fs

; update group length, if necessary
csogrplen:
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegOffset]
	sub	eax,gs:[si+GrpDefRecStruc.gdrGrpOffset]	; compute group and segment start delta
	add	eax,fs:[bx+MasterSegDefRecStruc.mssSegLength]	; add in current segment length
	cmp	eax,gs:[si+GrpDefRecStruc.gdrGrpLen]	; see if group length extended
	jb	csoupoff		; no new group length
	mov	gs:[si+GrpDefRecStruc.gdrGrpLen],eax	; extend group length

csoupoff:
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegLength]	; get segment size
	add	SegStartOffset,eax	; add to segment start offset for next segment
	jmp	csonextent	; move to next segdef entry

csoresdone:
	cmp	StackSegFoundFlag,OFF	; see if stack segment was found
	jne	csoret			; yes

; stack segment not found
;@@@ no warning for now

csoret:
	ret

; class name previously selected, see if matches current
; fs:bx -> master segdef entry
csoclasssel:
	les	di,CurrentResClassPtr	; es:di -> previously selected class name
	lds	si,fs:[bx+MasterSegDefRecStruc.mssClassPtr]	; ds:si -> current class name
	cmp	si,SIZEIOBUFFBLK-MAXOBJRECNAME	; see if name might overflow
	jb	csoclcmp		; no
	call	NormalDSSISource	; normalize ds:si to source name buffer

; ds:si -> current entry class name
; es:di -> previously selected class name
csoclcmp:
	lodsb				; get length byte
	mov	cl,al
	xor	ch,ch			; length in cx
	scasb				; compare to other length byte
	jne	csoclfail		; not a class name match
	jcxz	csoclmatch	; null name, match
	repe	cmpsb
	jne	csoclfail		; not a match

; current and previous class name match
csoclmatch:
	push	DGROUP		; restore ds -> wl32 data
	pop	ds
	cmp	IsDOSSEG,0		; see if DOSSEG segment ordering
	je	csocl2			; no, bypass group pointer check

; check if both have group pointers, group pointers must match
; fs:bx -> current master
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssGroupPtr]
	cmp	eax,CurrentResGrpPtr	; see if pointers match
	jne	csonextent		; no

csocl2:
	les	di,fs:[bx+MasterSegDefRecStruc.mssClassPtr]	; es:di -> current class name
	cmp	di,SIZEIOBUFFBLK-MAXOBJRECNAME	; see if name might overflow
	jb	csodores		; no
	call	NormalESDIDest	; normalize es:di to destination name buffer
	jmp	csodores

; current and previous class name did not match
csoclfail:
	push	DGROUP		; restore ds -> wl32 data
	pop	ds
	jmp	csonextent	; try next entry

csonextblk:
	mov	ax,fs:[MasterSegDefBlkStruc.msdbNextPtr]	; ax -> next block
	jmp	csomastblkloop

; first segment class BSS, keep pointer for _edata variable
csobss:
	cmp	WORD PTR _EDATASegDefPtr+2,0	; see if first BSS segment selected
	jne	csosetflag		; yes
	mov	WORD PTR _EDATASegDefPtr,bx	; save pointer to segdef of beginning BSS address
	mov	WORD PTR _EDATASegDefPTr+2,fs
	jmp	csosetflag

; internal error
internal2:
	mov	cl,2
	call	InternalErrorExit	; no return

ComputeSegOrder	ENDP

;*****************************
;* GETDOSSEGTYPE             *
;*****************************

; get type of segment class for DOSSEG segment ordering
; type == 1 for segment class CODE
;      == 2 for other segments outside of DGROUP
;      == 3 for DGROUP segment class BEGDATA
;      == 4 for DGROUP segment class not equal to BEGDATA, BSS, or STACK
;      == 5 for DGROUP segment class BSS
;      == 6 for DGROUP segment class STACK
; classes are not case sensitive for type (but are for concatenation)
; a suffix of the class name establishes type, e.g. BC_CODE is class CODE
; upon entry es:di -> class name, fs:bx -> master segdef entry
; returns class type in al
; destroys ax,cx,dx,si,bp,gs

GetDOSSEGType	PROC
	push	di			; save critical register
	mov	dl,es:[di]		; get length byte of name
	xor	dh,dh			; length value to dx
	add	di,dx
	mov	bp,di
	cmp	dl,3			; see if string length <4 bytes
	jbe	gdt2			; yes, automatic no match
	sub	di,3			; back up to last four chars in name
	mov	si,OFFSET DGROUP:CODEText
	call	CaselessStrCmp	; see if match
	jc	gdt2			; no match
	mov	al,1			; flag CODE

gdtret:
	pop	di				; restore critical register
	ret

; check if not DGROUP segment
gdt2:
	push	bx			; save critical register
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],GROUPMEMBERFLAG	; see if member of group
	je	gdtnodg			; no, not a dgroup member by definition
	lgs	bx,fs:[bx+MasterSegDefRecStruc.mssGroupPtr]	; gs:bx -> group entry
	lgs	bx,gs:[bx+GrpDefRecStruc.gdrGrpNamePtr]	; gs:bx -> group name, not normalized
	call	NormalGSBXSource	; safe to normalize to source buffer, destination buffer used by ComputeSegOrder

; gs:bx -> group name, normalized
; compare against DGROUP with length prefix
	mov	cx,3			; 7 bytes, 3 words+1
	mov	si,OFFSET DGROUP:DGROUPText

; compare loop for DGROUP group name
gdtloop:
	lodsw
	cmp	ax,gs:[bx]
	jne	gdtnodg			; not a match
	add	bx,2			; move to next 2 chars to compare
	loop	gdtloop
	lodsb				; get leftover byte
	cmp	al,gs:[bx]
	jne	gdtnodg

; segment is a member of dgroup
	pop	bx				; restore critical register
	cmp	dl,6			; see if string 6 or less bytes long
	jbe	gdt4			; yes, automatic no match
	mov	di,bp
	sub	di,6			; back up to last seven chars in name
	mov	si,OFFSET DGROUP:BEGDATAText
	call	CaselessStrCmp	; see if match
	jc	gdt4			; no match
	mov	al,3			; flag BEGDATA
	jmp	SHORT gdtret

; not a DGROUP segment, failed group name match
gdtnodg:
	pop	bx				; restore critical register
	mov	al,2			; flag not DGROUP
	jmp	SHORT gdtret

gdt4:
	cmp	dl,2			; see if string 2 or less bytes long
	jbe	gdt5			; yes, automatic no match
	mov	di,bp
	sub	di,2			; back up to last three chars in name
	mov	si,OFFSET DGROUP:BSSText
	call	CaselessStrCmp	; see if match
	jc	gdt5			; no match
	mov	al,5			; flag BSS
	jmp	SHORT gdtret

gdt5:
	cmp	dl,4			; see if string 4 or less bytes long
	jbe	gdt6			; yes, automatic no match
	mov	di,bp			; di -> char past null terminator
	sub	di,4			; back up to last five chars in name
	mov	si,OFFSET DGROUP:STACKText
	call	CaselessStrCmp	; see if match
	jc	gdt6			; no match
	mov	al,6			; flag STACK
	jmp	SHORT gdtret

gdt6:
	mov	al,4			; DGROUP, not BEGDATA, BSS, or STACK
	jmp	SHORT gdtret

GetDOSSEGType	ENDP

;*****************************
;* CASELESSSTRCMP            *
;*****************************

; case insensitive string compare
; upon entry ds:si -> const string to match (known uppercase), prepend length byte
;  es:di -> string to check (unknown uppercase)
;  first byte of string is length of string
; returns carry flag set if no match, reset if match
; destroys ax,si

CaselessStrCmp	PROC
	push	cx			; save critical register
	push	di
	lodsb				; get length byte
	mov	cl,al
	xor	ch,ch			; length to cx

csloop:
	lodsb				; get char character
	scasb				; see if matches
	je	csnext			; yes
	add	al,20h			; no match on uppercase, make lowercase
	cmp	al,es:[di-1]	; see if matches lowercase
	jne	csfail			; no

csnext:
	loop	csloop

; success, matched all chars in string
	clc					; show successful match
	jmp	SHORT csret

csfail:
	stc					; show failed match

csret:
	pop	di				; restore critical register
	pop	cx
	ret
CaselessStrCmp	ENDP

ENDS

END
