;*********************************************************************
;*   WLP2MOD.ASM
;*
;*   By:            Michael Devore
;*   Date:          11/26/96
;*   Model:         Small
;*   Version:       1.3f
;*   Assembler:     MASM 5.0
;*   Environment:   MS-DOS 3.0+
;*
;*   pass 2 object module processing routines
;*
;*********************************************************************

TITLE   WL32 WLP2MOD
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Equates                   *
;*****************************

LEDATAFLAG	EQU	1
LIDATAFLAG	EQU	2
FLATSEGMENTFIXUPFLAG	EQU	(1 SHL 28)

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
PUBLIC	LastLEDATASegIndex
PUBLIC	OBJPass2
PUBLIC	Pass2ModuleCount
PUBLIC	Process2OBJRecord
PUBLIC	UnresSymPtr

; public for debugger
;PUBLIC	Pass2FIXUPP32Proc
;PUBLIC	RelocRecurseLIDATA
PUBLIC	Pass2MODENDProc
PUBLIC	MakeRelocEntryBlk

; variables
PUBLIC	DataRecordOffset
PUBLIC	EntrySegmentValue,EntryOffsetValue,EntrySegmentID
PUBLIC	HighestOffsetWritten
PUBLIC	FirstRelocEntryBlkPtr
PUBLIC	LineSegmentID
PUBLIC	OBJRecPtr
PUBLIC	OS2ModuleFlag

IFDEF SYMBOLPACK
PUBLIC	LogicalDataRecOff
ENDIF

IFDEF DLLSUPPORT
PUBLIC	IMPDEFFixupSel
PUBLIC	TotalIMPDEFFixupCount
PUBLIC	TotalIMPDEFFixupSize
ENDIF

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

EndData	DB	?			; modend enddata byte
FixData	DB	?			; fixupp fixdata byte
FrameMethod	DB	?		; frame method
LastDataFlag	DB	?	; flags whether last data record was LEDATA or LIDATA or COMDAT
TargetMethod	DB	?	; target method

IsAbsoluteSeg	DB	?	; flags absolute segment
IsAbsoluteSym	DB	?	; flags absolute symbol
IsDebugSeg		DB	?	; flags debug segment
IsNotRelocatable	DB	?	; flags not relocatable segment
IsUnresSymbol	DB	?	; flags unresolved symbol
OS2ModuleFlag	DB	?	; flags os/2 special flagged module

BlockCount	DW	?		; LIDATA block count
ClipperFixupCount	DW	?	; current clipper code fixup count
CurrentRelocOffset	DW	?	; current LIDATA relocation offset for relocation entry
DataFixupCount	DW	?	; count of fixup items in array
DataOffset32	DD	?	; LIDATA32 data offset
DataRecordOffset	DW	?	; fixupp data record offset from locat subrecord
FixupSegmentID	DW	?	; segment ID of fixup for 3P files
FrameDatum	DW	?		; fixupp frame datum
LIDATARelocOffset	DW	?	; LIDATA relocation offset for relocation entry
LineSegmentID	DW	?	; segment ID of line number segment
Locat	DW	?			; fixupp locat field bytes
RepeatCount	DW	?		; LIDATA repeat count
RepeatCount32	DD	?	; LIDATA32 repeat count
TargetDatum	DW	?		; fixupp target datum
TargetSegment	DD	?	; target segment

CanonicFrame	DD	?	; used to compute canonical segment value
LastCOMDATPtr	DD	?	; pointer to last COMDAT parsed symbol entry
LDATAFullRecPtr	DD	?	; L?DATA owning individual segdef entry, full record portion
LDATAIndSegPtr	DD	?	; L?DATA owning individual segdef entry
LogicalDataRecOff	DD	?	; L?DATA data record offset
TargetDisplacement	DD	?	; fixupp target displacement
TargetProgOffset	DD	?	; target program offset
UnresSymPtr	DD	?		; pointer to unresolved symbol

ThreadFRAMEMethod	DB	4 DUP (?)	; fixupp thread frame method, numbers 0-3
ThreadTARGETMethod	DB	4 DUP (?)	; fixupp thread target method, numbers 0-3

ThreadFRAMEDatum	DW	4 DUP (?)	; fixupp thread frame datum index, numbers 0-3
ThreadTARGETDatum	DW	4 DUP (?)	; fixupp thread target datum index, numbers 0-3

LDATADataRecPtr	DD	?	; pointer to start of L?DATA object record data
OBJRecPtr	DD	?		; pointer to start of current object record

RelocationTable	DW	512 DUP (?)	; temporary storage of relocation fixup items
								; 16-bit offset, 16-bit segment (non-EXE only)

IFDEF	DLLSUPPORT
IMPDEFFixupType	DB	?	; IMPDEF fixup type, with flags
IsFrameIMPDEF	DB	?	; nonzero if fixup frame is IMPDEF-based
IsIMPDEFSymbol	DB	?	; nonzero is symbol is IMPDEF based
IsOrdinalFlag	DB	?	; nonzero if impdef symbol has ordinal value
IMPDEFFixupSel	DW	?	; selector of block pointing to IMPDEF fixups
IMPDEFFixupOffset	DD	?	; exe image offset of current IMPDEF fixup
IMPDEFFramePtr	DD	?	; pointer to IMPDEF frame symbol entry
IMPDEFFunctionNumber	DD	?	; function number of impdef
IMPDEFModuleNumber	DD	?	; module number of IMPDEF
IMPDEFTargetPtr	DD	?	; pointer to IMPDEF target symbol entry
MAXIMPDEFFixupAlloc	DD	?	; current maximum IMPDEF fixup allocation
TotalIMPDEFFixupSize	DD	?	; total size of IMPDEF fixup table
ENDIF

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

; MUST be in sync with OBJRecTable
; routine vectors for object record types
Pass2OBJRecVector	=	$
	DW	Pass2LEDATAProc
	DW	Pass2LEDATA32Proc
	DW	Pass2FIXUPPProc
	DW	Pass2FIXUPP32Proc
	DW	Pass2EXTDEFProc
	DW	Pass2PUBDEFProc
	DW	Pass2PUBDEF32Proc
	DW	Pass2LEXTDEFProc	; LEXTDEF1 and LEXTDEF2 are syntactically identical
	DW	Pass2LEXTDEFProc
	DW	Pass2LPUBDEFProc
	DW	Pass2LPUBDEF32Proc
	DW	Pass2SEGDEFProc
	DW	Pass2SEGDEF32Proc
	DW	Pass2COMDEFProc
	DW	Pass2LCOMDEFProc
	DW	Pass2LIDATAProc
	DW	Pass2LIDATA32Proc
	DW	Pass2LNAMESProc
	DW	Pass2COMENTProc
	DW	Pass2CEXTDEFProc
	DW	Pass2COMDATProc
	DW	Pass2COMDAT32Proc
	DW	Pass2BAKPATProc
	DW	Pass2BAKPAT32Proc
	DW	Pass2NBKPATProc
	DW	Pass2NBKPAT32Proc
	DW	Pass2GRPDEFProc
	DW	Pass2THEADRProc
	DW	Pass2LHEADRProc
	DW	Pass2MODENDProc
	DW	Pass2MODEND32Proc
	DW	Pass2LINNUMProc
	DW	Pass2LINNUM32Proc
	DW	Pass2LINSYMProc
	DW	Pass2LINSYM32Proc
; unsupported record types, never occurs because of pass 1 filter
	DW	Pass2AliasProc
; ignored record types
	DW	Pass2IgnoreOBJRecord
	DW	Pass2IgnoreOBJRecord
	DW	Pass2IgnoreOBJRecord
	DW	Pass2IgnoreOBJRecord
	DW	Pass2IgnoreOBJRecord
; library linked as object module
	DW	Pass2MSLIBRProc

; jump table for handling code on target methods 0-3 (highest bit of 3 masked off)
TargetJumpTable	=	$
	DW	gtasegment
	DW	gtagroup
	DW	gtaexternal
	DW	poor10

; jump table for handling code on frame methods 0-7
FrameJumpTable	=	$
	DW	gfasegment
	DW	gfagroup
	DW	gfaexternal
	DW	poor11
	DW	gfacanon
	DW	gfatarg
	DW	poor11
	DW	poor11

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

EntryOffsetValue	DD	0	; program entry offset value
EntrySegmentID	DW	0	; program entry segment ID
EntrySegmentValue	DD	0	; program entry segment value
FirstRelocEntryBlkPtr	DW	0	; first allocated relocation entry block pointer
HighestOffsetWritten	DD	0	; highest address offset written
IsEntryPoint	DB	0	; nonzero if program has entry point
LastLEDATASegIndex	DW	0	; last parsed segment index for LEDATA/LIDATA object record
LastRelocEntryBlkPtr	DW	0	; last allocated relocation entry block pointer
Pass2ModuleCount	DW	0	; pass 2 module counter

IFDEF SYMBOLPACK
ClipperSymbolSegBase	DW	0	; initial clipper symbol segment base
ENDIF

IFDEF DLLSUPPORT
TotalIMPDEFFixupCount	DD	0	; total count of IMPDEF fixups
ENDIF

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	COMDATFlags:BYTE
EXTRN	CurrentBaseOBJBuff:WORD
EXTRN	CurrentFileName:BYTE
EXTRN	CurrentLNAMESIndex:WORD
EXTRN	CurrentOBJ:WORD
EXTRN	CurrentSegDefCount:WORD
EXTRN	EndOfOBJFlag:BYTE
EXTRN	Is32BitSeg:BYTE
EXTRN	IsLocalSymbol:BYTE
EXTRN	LNAMESIndexSel:WORD
EXTRN	OBJBuffSelTablePtr:WORD
EXTRN	OBJRecTable:BYTE
EXTRN	PharLapModuleFlag:BYTE
EXTRN	RelocEntryCount:DWORD
EXTRN	SearchExistSymFlag:BYTE

IFDEF CLIPPER
EXTRN	IsSpecialFixupOption:BYTE
ENDIF

IFDEF SYMBOLPACK
EXTRN	ClipperSymSegIndex:WORD
EXTRN	CompressThisModule:BYTE
EXTRN	RelocationAdjustment:DWORD
ENDIF

IFDEF WATCOM_ASM
EXTRN	IsFlatOption:BYTE
ENDIF

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	AllocateMemory:PROC
EXTRN	BadOBJModuleExit:PROC
EXTRN	DisplayProcFileFeedback:PROC
EXTRN	GetPubSymEntry:PROC
EXTRN	InternalErrorExit:PROC
EXTRN	LinkerErrorExit:PROC
EXTRN	MakeSourceLineBlk:PROC
EXTRN	ReadByte:PROC,ReadByteDecCX:PROC
EXTRN	ReadWord:PROC,ReadWordCX:PROC
EXTRN	ReadDwordECX:PROC
EXTRN	ReadIndex:PROC,ReadIndexDecCX:PROC
EXTRN	ReadNameString:PROC
EXTRN	ReadWordDecCX:PROC,ReadDwordDecCX:PROC
EXTRN	ScanAhead:PROC
EXTRN	SetOBJBuffer:PROC
EXTRN	UnresExternalWarn:PROC

IFDEF SYMBOLPACK
EXTRN	CheckCompressedSymbol:PROC
EXTRN	Pass2ClipperCheck:PROC
EXTRN	ReadDword:PROC
ENDIF

IFDEF DLLSUPPORT
EXTRN	ResizeMemory32:PROC
ENDIF

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* OBJPASS2                  *
;*****************************

; pass 2 processing of object modules

OBJPass2	PROC
	mov	ProcessingLIBFlag,0
	mov	CurrentOBJ,1	; init current object module number

objloop:
	push	ds
	pop	es				; es -> wl32 data
	call	SetOBJBuffer

; set CurrentFileName buffer
	lgs	bx,fs:[IOBuffHeaderStruc.ibhsFileNamePtr]	; gs:bx -> current file name
	mov	di,OFFSET DGROUP:CurrentFileName

objnameloop:
	mov	al,gs:[bx]		; get file name char
	stosb				; store it
	inc	bx				; move to next slot
	or	al,al			; see if null terminator transferred
	jne	objnameloop

	call	DisplayProcFileFeedback
	xor	eax,eax

IFDEF SYMBOLPACK
	mov	RelocationAdjustment,eax
ENDIF

	mov	LastLEDATASegIndex,ax	; init last LEDATA segment index value for module
	mov	EndOfOBJFlag,al	; init end of object module flag
	mov	CurrentLNAMESIndex,ax	; init object record indices
	mov	CurrentSegDefCount,ax	; init module-level counters
	mov	ax,fs:[IOBuffHeaderStruc.ibhsFlags]
	test	ax,IGNOREBLOCKFLAG
	jne	nextobj			; ignore this object module

; consider OS/2 flag as special case of phar lap module, phar lap
; flag is always set when os/2 flag is set, but os/2 flag does not
; perform the special LIDATA32 16-bit repeat count exception
	test	ax,ISOS2MODFLAG
	setne	OS2ModuleFlag	; set os/2 flag is module is flagged
	and	ax,ISPHARLAPMODFLAG
	setne	PharLapModuleFlag	; set phar lap flag if module is flagged

IFDEF SYMBOLPACK
	call	Pass2ClipperCheck	; check if module is clipper module
ENDIF

recloop:
	call	Process2OBJRecord	; get current OBJ record, process it pass 2
	cmp	EndOfOBJFlag,OFF	; see if at end of OBJ
	je	recloop			; not at end

;multiple modules in one file check
;@@@ code goes here

nextobj:
	inc	Pass2ModuleCount	; bump pass 2 module counter
	inc	CurrentOBJ		; bump count of current object module
	mov	ax,CurrentOBJ	; get new current object module
	cmp	ax,TotalOBJCount	; see if parsed all object modules
	jbe	objloop
	ret
OBJPass2	ENDP

;*****************************
;* PROCESS2OBJRECORD         *
;*****************************

; get and do pass 2 processing of object record
; bad record value checking not needed, covered in pass 1

Process2OBJRecord	PROC
	mov	WORD PTR OBJRecPtr,si	; save pointer to record
	mov	WORD PTR OBJRecPtr+2,fs
	call	ReadByte	; read byte of current file into al register (record type)
	call	ReadWordCX	; read word of current file into cx register (record length)
	push	fs			; save -> current read position
	push	si
	push	cx			; save record length
	mov	di,OFFSET DGROUP:OBJRecTable	; es:di -> lookup table for object record types
	mov	cx,ds:[di-2]	; get number of entries in table
	repne	scasb
	jne	internal3		; record not found in table, internal error

	dec	di				; di -> matching entry
	sub	di,OFFSET DGROUP:OBJRecTable	; di == object record type offset
	add	di,di			; word offset
	add	di,OFFSET DGROUP:Pass2OBJRecVector	; di -> entry in OBJRecVector table
	pop	cx				; restore object record length to cx
	push	cx			; save back for later use
	call	ds:[di]		; transfer to appropriate routine
	pop	cx				; restore object record length to cx
	pop	si				; restore old current read position
	pop	fs
	call	ScanAhead	; scan past processed bytes to next record, if any
	ret

; internal error
internal3:
	mov	cl,3
	call	InternalErrorExit	; no return

Process2OBJRecord	ENDP

;*****************************
;* PASS2IGNOREOBJRECORD      *
;*****************************

; ignore object record without error

Pass2IgnoreOBJRecord	PROC
	ret
Pass2IgnoreOBJRecord	ENDP

;*****************************
;* PASS2LEDATAPROC           *
;*****************************

; upon entry fs:si -> read buffer, cx holds record length

Pass2LEDATAProc	PROC
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	jne	Pass2LEDATA32Proc	; yes, LEDATA's are processed as LEDATA32's

	mov	ClipperFixupCount,0	; reset clipper fixup count
	mov	eax,OBJRecPtr
	mov	LDATAFullRecPtr,eax	; save for LIDATA relocation fixups
	mov	LastDataFlag,LEDATA
	call	ReadIndex	; get segment index in ax
	call	ReadWordCX	; get enumerated data offset into cx
	movzx	ecx,cx			; make 32-bit
	mov	LogicalDataRecOff,ecx	; save logical data record offset
	mov	WORD PTR LDATADataRecPtr,si	; save pointer to record data for later fixups
	mov	WORD PTR LDATADataRecPtr+2,fs
	cmp	ax,LastLEDATASegIndex	; see if equal to last segment index (no flagging needed)
	mov	LastLEDATASegIndex,ax	; update last LEDATA segment index, KEEP FLAG STATUS
	je	ple2			; matches, no need to update
	call	SetSegDefDataFlag

ple2:
	ret
Pass2LEDATAProc	ENDP

;*****************************
;* PASS2LEDATA32PROC         *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass2LEDATA32Proc	PROC
	mov	eax,OBJRecPtr
	mov	LDATAFullRecPtr,eax	; save for LIDATA relocation fixups
	mov	LastDataFlag,LEDATA
	call	ReadIndex	; get segment index in ax
	call	ReadDwordECX	; get enumerated data offset into ecx
	mov	LogicalDataRecOff,ecx	; save logical data record offset
	mov	WORD PTR LDATADataRecPtr,si	; save pointer to record data for later fixups
	mov	WORD PTR LDATADataRecPtr+2,fs
	cmp	ax,LastLEDATASegIndex	; see if equal to last segment index (no flagging needed)
	mov	LastLEDATASegIndex,ax	; update last LEDATA segment index, KEEP FLAG STATUS
	je	ple32			; matches, no need to update
	call	SetSegDefDataFlag

ple32:
	ret
Pass2LEDATA32Proc	ENDP

;*****************************
;* SETSEGDEFDATAFLAG         *
;*****************************

; flag individual segdef has associated data
; upon entry ax holds segment index
; destroys ax,bx,dx,gs

SetSegDefDataFlag	PROC
	mov	gs,CurrentBaseOBJBuff	; gs -> i/o buffer base
	lgs	bx,gs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; gs:bx -> first segdef entry
	dec	ax				; make segment index relative zero
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to grouped segdef entry
	add	bx,ax			; gs:bx -> grouped individual segment entry, not normalized
	cmp	bx,SIZEINDSEGDEFBLK		; see if overflow to next block
	jb	ssdsave			; no

; segment entry has wrapped to next buffer
	sub	bx,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	gs,gs:[IndSegDefBlkStruc.isdbNextPtr]	; gs:bx -> individual segment entry, normalized

; save individual segment pointer for L?DATA record
ssdsave:
	mov	WORD PTR LDATAIndSegPtr,bx
	mov	WORD PTR LDATAIndSegPtr+2,gs
	mov	IsDebugSeg,OFF	; init debug segment flag

; see if data flag previously set
	test	gs:[bx+IndSegDefRecStruc.isdrFlags],ASSOCIATEDDATAFLAG
	je	ssdset			; flag not previously set
	ret					; flag previously set, ignore

; set flag to show data associated with segdef in module, keep pointer to first data block
; fs:si -> data block
ssdset:
	or	gs:[bx+IndSegDefRecStruc.isdrFlags],ASSOCIATEDDATAFLAG
	mov	eax,OBJRecPtr
	mov	gs:[bx+IndSegDefRecStruc.isdrDataPtr],eax
	lgs	bx,gs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; gs:bx -> master segdef

	test	gs:[bx+MasterSegDefRecStruc.mssFlags],(DEBUGTYPESSEGMENTFLAG OR DEBUGSYMBOLSSEGMENTFLAG)
	je	ssdchk32		; not a debug segment

; debug segment, so flag, and reset associated data so debug keeps getting flagged
	lgs	bx,LDATAIndSegPtr	; gs:bx -> individual segdef
	and	gs:[bx+IndSegDefRecStruc.isdrFlags],NOT ASSOCIATEDDATAFLAG
	mov	IsDebugSeg,ON
	ret					; don't worry about 32-bit segments if they are debug

ssdchk32:
	or	gs:[bx+MasterSegDefRecStruc.mssFlags],ASSOCIATEDDATAFLAG
	mov	Is32BitSeg,OFF	; init 32-bit segment flag
	test	gs:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG	; flag if 32-bit segment
	je	ssdret			; not 32-bit segment
	mov	Is32BitSeg,ON	; flag 32-bit segment

ssdret:
	ret
SetSegDefDataFlag	ENDP

;*****************************
;* PASS2FIXUPPPROC           *
;*****************************

; upon entry fs:si -> read buffer, cx holds record length

Pass2FIXUPPProc	PROC
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	jne	Pass2FIXUPP32Proc	; yes, FIXUPP's are processed as FIXUPP32's

IFDEF SYMBOLPACK
	mov	RelocationAdjustment,0
ENDIF

	cmp	LastDataFlag,COMDAT
	je	p2fcomdat

p2fchkadj:
	dec	cx				; adjust for checksum byte
	mov	gs,CurrentBaseOBJBuff	; gs -> i/o base buffer for fixup calculations
	mov	DataFixupCount,0	; init count of fixup items in array
	push	fs			; save fs -> record

p2ffixloop:
	or	cx,cx			; see if at end of record
	je	p2fdone			; yes
	pop	fs				; restore fs -> record
	call	ReadByteDecCX	; read thread data field/low locat field byte
	test	al,FIXUPSUBRECORD	; see if fixup or thread subrecord
	jne	p2ffixup		; fixup subrecord

; thread subrecord
	mov	bl,al
	and	bl,THREADNUMOFTHREADDATA	; get thread number in bx
	xor	bh,bh
	test	al,DBITOFTHREADDATA
	jne	p2fframe		; frame thread
	and	al,TARGMETHOFTHREADDATA	; get target method (2 bits only)
	shr	al,2			; convert to relative zero
	mov	[bx+OFFSET DGROUP:ThreadTARGETMethod],al
	call	ReadIndexDecCX	; get thread index
	add	bx,bx			; convert to word offset
	mov	[bx+OFFSET DGROUP:ThreadTARGETDatum],ax
	push	fs			; save fs -> record to stack
	jmp	p2ffixloop

p2fcomdat:
	lgs	di,LastCOMDATPtr	; gs:di -> last COMDAT pointer
	test	gs:[di+PubSymRecStruc.pssFlags],COMDATCOMPLETEFLAG
	jne	p2fret			; processing on this comdat is complete
;	or	gs:[di+PubSymRecStruc.pssFlags],COMDATFIXUPFLAG
	jmp	p2fchkadj	; keep processing

p2fframe:
	and	al,METHODOFTHREADDATA	; get method
	shr	al,2			; convert to relative zero
	mov	[bx+OFFSET DGROUP:ThreadFRAMEMethod],al
	call	ReadIndexDecCX	; get thread datum index
	add	bx,bx			; convert to word offset
	mov	[bx+OFFSET DGROUP:ThreadFRAMEDatum],ax
	push	fs			; save fs -> record to stack
	jmp	p2ffixloop

p2ffixup:
	mov	BYTE PTR Locat,al	; save low byte of locat word
	call	ReadByteDecCX
	mov	BYTE PTR Locat+1,al
	mov	ah,BYTE PTR Locat
	and	ah,3			; get two MSB of data record offset
	mov	al,BYTE PTR Locat+1	; get low byte of data record offset
	mov	DataRecordOffset,ax	; save data record offset from Locat subrecord

;@@@	mov	eax,LogicalDataRecOff	; get enumerated/iterated data record offset
;@@@	mov	TargetDisplacement,eax	; init target displacement with offset
	mov	TargetDisplacement,0	; init target displacement

	call	ReadByteDecCX	; read fix data byte
	mov	FixData,al

; from the FixData subrecord:
;  frame datum index present iff F (80h) bit is reset and Frame (70h) field is 0-2 (0, 10h,20h)
;  target datum index present iff T (8) bit is reset
;  target displacement word present iff P (4) bit is reset
	cmp	al,2fh			; check if F bit reset and Frame field <=20h
	ja	p2ftdat			; no
	call	ReadIndexDecCX	; get frame datum
	mov	FrameDatum,ax

p2ftdat:
	test	FixData,TBITOFFIXDATA	; see if T bit set
	jne	p2ftdisp		; yes, no target datum
	call	ReadIndexDecCX	; get target datum
	mov	TargetDatum,ax

p2ftdisp:
	test	FixData,PBITOFFIXDATA	; see if P bit set
	jne	p2f2			; yes, no target displacement
	call	ReadWordDecCX	; get target displacement
;@@@	movzx	eax,ax
;@@@	add	TargetDisplacement,eax
	cwde
	mov	TargetDisplacement,eax

p2f2:
	mov	al,FixData
	test	al,FBITOFFIXDATA	; see if thread field for frame (F bit)
	je	p2fnothrfr		; no thread field for frame

	and	al,30h			; mask off F bit and high bit of 3-bit frame field for thread value
	shr	al,4			; convert to relative 0
	mov	bl,al
	xor	bh,bh
	mov	al,[bx+OFFSET DGROUP:ThreadFRAMEMethod]
	mov	FrameMethod,al	; save frame method as current
	add	bx,bx			; convert to word offset
	mov	ax,[bx+OFFSET DGROUP:ThreadFRAMEDatum]
	mov	FrameDatum,ax	; save frame datum index as current
	jmp	p2f3

p2fnothrfr:
	and	al,FRAMEFIELDOFFIXDATA	; get frame field
	shr	al,4			; convert to relative zero
	mov	FrameMethod,al	; save frame method

p2f3:
	mov	al,FixData
	test	al,TBITOFFIXDATA	; see if thread field for target (T bit)
	je	p2fnothrtar		; no thread field for target

	and	al,TARGTFIELDOFFIXDATA	; get targt field value in al
	mov	bl,al
	xor	bh,bh
	mov	al,[bx+OFFSET DGROUP:ThreadTARGETMethod]
	mov	TargetMethod,al	; save target method as current
	add	bx,bx			; convert to word offset
	mov	ax,[bx+OFFSET DGROUP:ThreadTARGETDatum]
	mov	TargetDatum,ax	; save target datum index as current
	jmp	p2f4

p2fnothrtar:
	and	al,TARGTFIELDOFFIXDATA	; get targt field value in al
	mov	TargetMethod,al	; save target method

p2f4:
	push	fs			; save fs -> record to stack

	cmp	IsDebugSeg,OFF	; see if debug segment
	jne	p2ffixloop		; yes, only trolling for thread fixups

	xor	al,al
	mov	IsUnresSymbol,al	; init unresolved symbol flag
	mov	IsNotRelocatable,al	; init not relocatable segment flag
	mov	IsAbsoluteSeg,al
	mov	IsAbsoluteSym,al

IFDEF	DLLSUPPORT
	mov	IsIMPDEFSymbol,al	; init impdef symbol flag
	mov	IsFrameIMPDEF,al
ENDIF

	call	GetTargetAddress	; get the fixup target address

	mov	al,IsAbsoluteSeg
	or	al,IsAbsoluteSym	; no frame computation on absolute segment or symbol
	jne	p2fsegrel			; absolute

	call	GetFrameAddress	; the fixup frame address

IFDEF DLLSUPPORT
	cmp	IsIMPDEFSymbol,OFF	; see if fixup on impdef symbol
	je	p2fnotimp			; no
	call	PerformIMPDEFFixup
	jmp	p2ffixloop

p2fnotimp:
ENDIF

	cmp	IsUnresSymbol,OFF	; see if unresolved symbol
	jne	p2funres		; yes

; convert target segment and target program offset to seg:off format
	test	Locat,MBITOFLOCAT	; check M bit for self-relative fixup
	je	p2fselfrel		; M bit reset, self-relative fixup

; fixup segment relative
p2fsegrel:
	mov	eax,TargetSegment
	shl	eax,4			; eax holds byte value of target segment

	mov	edx,TargetProgOffset	; edx holds target program offset
	sub	edx,eax			; edx==difference between frame and absolute program offset

; resolve the fixup, value in edx
p2fres:

; check if fixup overflow, edx should be <=64K and >=-64K
;@@@ code goes here

IFDEF SYMBOLPACK
	cmp	CompressThisModule,OFF	; see if compressing Clipper code
	je	p2fpoint		; no
	mov	ax,LastLEDATASegIndex
	cmp	ax,ClipperSymSegIndex	; see if fixing up a SYMBOLS table during compression
	jne	p2fpoint		; not
	call	CheckCompressedSymbol	; check if fixup of compressed symbols, modify RelocationAdjustment if fixup discarded
	jc	p2ffixloop		; throw away fixup

p2fpoint:
ENDIF

	lfs	bx,LDATADataRecPtr	; fs:bx -> L?DATA record data being fixed up
	add	bx,DataRecordOffset	; fs:bx -> location to be fixed up, not normalized
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jae	p2fnormbx1		; yes, normalize it

; fs:bx -> fixup location
p2fres2:
	mov	al,BYTE PTR Locat
	and	al,LOCFIELDOFLOCAT	; get loc field
	cmp	al,OFFSETLOC	; see offset fixup
	jne	p2fchkloc5		; no

; offset fixup
p2floc:
	add	fs:[bx],dl		; adjust low byte of word
	adc	dh,0			; carry to high byte
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jae	p2fnormbx2		; yes, normalize it

p2floc2:
	add	fs:[bx],dh		; adjust high byte of word
	jmp	p2ffixloop

p2fnormbx2:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2floc2

p2fchkloc5:
	cmp	al,OFFSETLOC5	; see alternate method 5 offset fixup
	je	p2floc

; MED 07/31/96, support 32-bit offset fixup (really dumb compiler, the upper two
;  bytes are ignored and this is really a 16-bit offset fixup)
	cmp	al,OFFSET32LOC	; see if 32-bit fixup to be processed as 16-bit
	je	p2floc

	cmp	al,SEGMENTLOC	; see segment fixup
	jne	p2fchkptr		; no

; segment fixup
p2fseg:
	xor	dx,dx			; adjustment for fixup location to segment portion

; code shared with pointer fixup
p2fsegshared:
	mov	al,IsNotRelocatable
	or	al,IsCreateEXEOption
	je	p2fseg32		; P3 format and not absolute segment, used segment ID for fixup
	mov	ax,WORD PTR TargetSegment
	add	fs:[bx],al		; adjust low byte of word
	adc	ah,0			; carry to high byte
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jae	p2fnormbx3		; yes, normalize it

p2fseg2:
	add	fs:[bx],ah		; adjust high byte of word
	cmp	IsNotRelocatable,OFF	; see if relocatable segment
	jne	p2ffixloop		; no

; relocation entry store for later processing
p2frelproc:
	add	dx,DataRecordOffset	; dx -> relocation fixup location relative L?DATA record
	mov	bx,DataFixupCount
	add	bx,bx			; word per entry
	mov	ds:[bx+WORD PTR RelocationTable],dx	; save relocation entry offset relative L?DATA record
	inc	DataFixupCount	; bump count of data fixups
	jmp	p2ffixloop

; P3 format and not absolute segment, use segment ID for fixup
p2fseg32:
	mov	ax,FixupSegmentID
	mov	fs:[bx],al		; adjust low byte of word
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2fseg32b		; no
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables

p2fseg32b:
	mov	fs:[bx],ah		; adjust high byte of word
	jmp	p2frelproc

p2fnormbx3:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2fseg2

p2fchkptr:
	cmp	al,POINTERLOC	; see if pointer fixup
	jne	p2fchklow		; no

; pointer fixup
	add	fs:[bx],dl		; adjust low byte of word
	adc	dh,0			; carry to high byte
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jae	p2fnormbx4		; yes, normalize it

p2fptr2:
	add	fs:[bx],dh		; adjust high byte of word
	mov	dx,2			; adjustment for fixup location to segment portion
	inc	bx				; move to segment portion of fixup
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2fsegshared	; no
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2fsegshared

p2fnormbx4:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2fptr2

p2fchklow:
	cmp	al,LOWORDERBYTELOC	; see if lower order byte fixup
	jne	poor13			; no bad loc value

; low-order byte fixup
	add	fs:[bx],dl
	jmp	p2ffixloop

p2fnormbx1:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2fres2

; invalid loc field (4,6,7-15)
poor13:
	mov	cl,13
	call	BadOBJModuleExit

; self-relative fixup
p2fselfrel:
	lfs	bx,LDATAIndSegPtr	; fs:bx -> individual segdef entry
	mov	eax,fs:[bx+IndSegDefRecStruc.isdrSegOffset]	; get individual segment offset
	lfs	bx,fs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; fs:bx -> master segdef entry
	add	eax,fs:[bx+MasterSegDefRecStruc.mssSegOffset]	; add in master segment offset for absolute segment offset

	mov	edx,TargetProgOffset	; get absolute program offset
	sub	edx,eax			; subtract off individual segment offset (make relative to segment)

	sub	edx,LogicalDataRecOff	; make relative to logical data record

	mov	ax,DataRecordOffset
	movzx	eax,ax		; eax==data record offset value
	sub	edx,eax			; make target offset relative to location

COMMENT !
	mov	eax,TargetSegment
	shl	eax,4			; target segment to absolute segment offset in bytes
	sub	edx,eax
END COMMENT !

	mov	al,BYTE PTR Locat
	and	al,LOCFIELDOFLOCAT	; get loc field

	cmp	al,LOWORDERBYTELOC	; see if low-order byte loc field
	jne	p2fself3		; no
	dec	edx				; adjust for one-byte length
	jmp	p2fres

p2fself3:
	cmp	al,OFFSETLOC	; see if offset byte loc field
	je	p2flocoff
	cmp	al,OFFSETLOC5	; check alternate offset value
	jne	poor12			; not an allowable self-relative fixup loc field value

p2flocoff:
	sub	edx,2			; adjust for two-byte field
	jmp	p2fres

; invalid loc field for self-relative fixup (2,3,4,6,7-15)
poor12:
	mov	cl,12
	call	BadOBJModuleExit	; no return

; done with this round of fixups
p2fdone:

IFDEF SYMBOLPACK
	cmp	CompressThisModule,OFF	; see if compressing Clipper code
	je	p2fchkrel		; no
	mov	ax,LastLEDATASegIndex
	cmp	ax,ClipperSymSegIndex
	jae	p2fchkrel		; not Clipper code
	inc	ClipperFixupCount
	cmp	ClipperFixupCount,2	; see if at base fixup (2nd fixup)
	jne	p2fchkrel		; no

	push	fs			; save critical registers
	push	si

	lfs	si,LDATADataRecPtr	; get pointer to record data
	call	ReadDword	; scan past first three bytes of LEDATA
	cmp	ClipperSymbolSegBase,0	; see if first Clipper procedure fixed up
	jne	p2fnotfirst

; first clipper procedure fixed up
; save fixup value at offset 4 for Clipper 5
	call	ReadWord	; get initial segment base value for Clipper 5
	mov	ClipperSymbolSegBase,ax	; save the initial symbol segment base value
	jmp	p2fclipdone

; subsequent clipper procedure
; place saved fixup symbol segment base value at offset 4
p2fnotfirst:
	mov	ax,ClipperSymbolSegBase	; get the initial segment base value
	cmp	si,SIZEIOBUFFBLK	; see if past buffer
	jb	p2fnot2			; no
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain

p2fnot2:
	mov	fs:[si],al		; store initial segment base value low byte
	inc	si				; move to high byte
	cmp	si,SIZEIOBUFFBLK	; see if past buffer
	jb	p2fnot3			; no
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain

p2fnot3:
	mov	fs:[si],ah		; store initial segment base value low byte

p2fclipdone:
	pop	si				; restore critical registers
	pop	fs

p2fchkrel:
ENDIF

	pop	ax				; get fs pointer off of stack, fs unchanged, pop to nonseg register
	cmp	DataFixupCount,0	; see if any relocation items to process
	je	p2fret			; no relocation items

; process relocation items
	lgs	bx,LDATAIndSegPtr	; gs:bx -> individual segdef entry

; converted to 32-bit register 01/26/94
	mov	edx,gs:[bx+IndSegDefRecStruc.isdrSegOffset]	; dx==individual segment offset
	add	edx,LogicalDataRecOff	; add enumerated data record offset

	lgs	bx,gs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; gs:bx -> master segdef entry
	mov	eax,gs:[bx+MasterSegDefRecStruc.mssSegOffset]	; get in master segment offset
	mov	ebx,eax			; save seg offset

; converted to 32-bit 01/26/94
	and	eax,0fh			; get offset remainder of seg address
	add	edx,eax			; add to offset adjustment

	shr	ebx,4			; convert seg offset to paras
	mov	si,OFFSET DGROUP:RelocationTable
	mov	cx,DataFixupCount	; count of relocation entries

	cmp	LastDataFlag,LIDATA
	je	p2frellid

	movzx	ecx,cx		; extend to 32 bits for following addition
	add	RelocEntryCount,ecx	; bump count of permanent relocation entries
	push	es			; save critical register

p2frellast:
	mov	ax,LastRelocEntryBlkPtr	; ax -> last allocated block
	or	ax,ax			; zero if no previous allocations
	je	p2frelnew			; no previous allocations, block must be allocated

; check if allocated relocation entry block is full
	mov	es,ax			; es -> relocation entry block

p2frel3:
	mov	di,es:[RelocEntryBlkStruc.rebCount]

; convert to two dwords 01/26/94
	shl	di,3			; two dwords per entry

	add	di,RELOCENTRYSYSVARSIZE	; adjust for sysvars at beginning of block

p2frelloop:
	cmp	es:[RelocEntryBlkStruc.rebCount],MAXCOUNTRELOCENTRYBLK	; see if any free entries in block
	jae	p2frelnew		; no free entry exists
	inc	es:[RelocEntryBlkStruc.rebCount]	; bump count of entry
	lodsw				; get 16-bit offset from temporary fixup storage
	movzx	eax,ax		; zero high word
	add	eax,edx			; add in segment+record offset

IFDEF SYMBOLPACK
	sub	eax,RelocationAdjustment	; adjust for compressed out symbols
ENDIF

	stosd				; save offset
	mov	eax,ebx
	stosd				; save segment value (from master segment of L?DATA record)

	loop	p2frelloop	; loop through all entries
	pop	es				; restore critical register

p2fret:
	ret

; make a new block
p2frelnew:
	call	MakeRelocEntryBlk	; allocate relocation entry block
	jmp	p2frel3

p2frellid:
	push	es			; save critical register

p2flidrlast:
	mov	ax,LastRelocEntryBlkPtr	; ax -> last allocated block
	or	ax,ax			; zero if no previous allocations
	jne	p2flidr2
	call	MakeRelocEntryBlk	; allocate relocation entry block
	jmp	p2flidrloop

p2flidr2:
	mov	es,ax			; es -> relocation entry block

p2flidrloop:
	lodsw				; get 16-bit offset from temporary fixup storage

	mov	LIDATARelocOffset,ax	; save lidata record relocation offset
	push	cx			; save critical registers
	push	si
;	push	edx
	lfs	si,LDATAFullRecPtr	; fs:si -> LIDATA record
	call	ReadByte	; scan past type
	call	ReadWordCX	; cx holds record length

	call	ReadIndexDecCX	; scan past segment index
	call	ReadWordDecCX	; scan past iterated data offset
	mov	DataOffset32,0	; init data offset, don't use record's
	dec	cx				; adjust for checksum byte

p2flidrrecloop:
	jcxz	p2flidrdone		; no more bytes to write
	mov	CurrentRelocOffset,0	; init relocation entry offset in lidata record
	call	RelocRecurseLIDATA	; use recursive routine to extract data out of lidata record
	jmp	p2flidrrecloop

p2flidrdone:
;	pop	edx				; restore critical registers
	pop	si
	pop	cx
	loop	p2flidrloop
	pop	es				; restore critical register
	ret

; unresolved external, UnresSymPtr -> symbol entry
; warn, ignore fixup
p2funres:
	call	UnresExternalWarn
	jmp	p2ffixloop

Pass2FIXUPPProc	ENDP

;*****************************
;* RELOCRECURSELIDATA        *
;*****************************

; use recursion to get all data out of LIDATA record
; upon entry fs:si -> current data byte, cx == size of data block,
;  edx == segment+record offset
; destroys eax

RelocRecurseLIDATA  PROC
	call	ReadWordDecCx	; get repeat count
	mov	RepeatCount,ax	; save it
	call	ReadWordDecCx	; get block count
	mov	BlockCount,ax	; save it
	add	CurrentRelocOffset,4	; update lidata record relocation offset
	push	CurrentRelocOffset
	push	si			; save buffer position
	push	cx			; save record iterated data block length

rrlidreploop:
	pop	cx				; restore record length
	pop	si				; restore buffer position
	pop	CurrentRelocOffset
	push	CurrentRelocOffset
	push	si			; put values back on stack
	push	cx

	cmp	BlockCount,0	; see if nested iterated data blocks
	je	rrliddata		; no

; nested iterated data blocks
	mov	ax,BlockCount	; get number of blocks to loop through

rrlidblkloop:
	push	ax			; save current number of blocks left to loop through
	push	RepeatCount	; save repeat value
	push	BlockCount	; save block value
	call	RelocRecurseLIDATA	; nest down one level
	pop	BlockCount		; save block value
	pop	RepeatCount		; restore repeat value
	pop	ax				; restore number of blocks left to loop through
	dec	ax				; one iteration complete
	jne	rrlidblkloop		; loop not complete

; block loop is complete, do next iteration of repeat loop
rrlidnextrep:
	dec	RepeatCount
	jne	rrlidreploop		; more repeat iterations to do

	add	sp,6			; trash old record length and record position values on stack
	ret					; ; repeat loop complete, return to next highest level or calling procedure

; block size is zero, data follows
rrliddata:
	call	ReadByteDecCx	; get length of data to write
	inc	CurrentRelocOffset

; write the data bytes
	push	cx			; save record's total data length
	mov	cl,al
	xor	ch,ch			; data bytes to write in cx
	mov	ax,LIDATARelocOffset

; 05/09/96
	test	ah,80h		; see if 32-bit nonsegment fixup
	je	rrlidsegadj		; no
	and	ah,7fh			; remove 32-bit nonsegment fixup flag for comparison
	jmp	rrlidcmp

rrlidsegadj:
	add	ax,2			; put into segment territory if pointer fixup
	sub	ax,cx			; adjust down to start to data byte offset

rrlidcmp:
	cmp	ax,CurrentRelocOffset
	jne	rrlidscan

rrlidrelblk:
	mov	di,es:[RelocEntryBlkStruc.rebCount]
	shl	di,3			; two dwords per entry
	add	di,RELOCENTRYSYSVARSIZE	; adjust for sysvars at beginning of block
	cmp	es:[RelocEntryBlkStruc.rebCount],MAXCOUNTRELOCENTRYBLK	; see if any free entries in block
	jb	rrlid2			; free entry exists
	call	MakeRelocEntryBlk
;	mov	es,ax			; es -> new block
	jmp	rrlidrelblk

rrlid2:
	inc	es:[RelocEntryBlkStruc.rebCount]	; bump count of entry
	inc	RelocEntryCount	; bump count of permanent relocation entries

	mov	eax,DataOffset32	; data offset in ax

	movzx	ecx,cx
	add	eax,ecx			; point to just past final byte write

	test	BYTE PTR LIDATARelocOffset+1,80h	; see if 32-bit nonsegment
	je	rrlidsegadj2
	sub	eax,4			; adjust down to start of 32-bit territory
	or	eax,FLATSEGMENTFIXUPFLAG	; flag flat segment fixup
	jmp	rrlidstore

rrlidsegadj2:
	sub	eax,2			; adjust down to segment territory

rrlidstore:
	add	eax,edx			; add in segment+record offset
	stosd
	mov	eax,ebx
	stosd

rrlidscan:
	movzx	eax,cx		; get data byte count
	add	DataOffset32,eax	; update data offset with byte count
	call	ScanAhead	; move source buffer ahead byte count
	mov	ax,cx			; byte count to ax
	add	CurrentRelocOffset,ax	; update record offset
	pop	cx				; restore record's total data length
	sub	cx,ax			; subtract off data byte count
	jmp	rrlidnextrep

RelocRecurseLIDATA  ENDP

;*****************************
;* MAKERELOCENTRYBLK         *
;*****************************

; make a relocation entry block
; returns es -> block, updates LastRelocEntryBlkPtr, FirstRelocEntryBlkPtr variables
; destroys eax

MakeRelocEntryBlk	PROC
	push	dx			; save critical register
	mov	dx,SIZERELOCENTRYBLK	; get number of bytes to allocate for relocation entry block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastRelocEntryBlkPtr	; keep -> previously last allocated block, if any
	mov	LastRelocEntryBlkPtr,ax	; update last allocated block pointer
	mov	es,ax			; es -> block
	xor	eax,eax
	mov	es:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstRelocEntryBlkPtr,ax	; see if first block allocated yet
	je	msptfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,es
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	es,ax			; es -> previous block
	mov	es:[RelocEntryBlkStruc.rebNextPtr],dx
	mov	es,dx			; es -> current block

msptret:
	pop	dx				; restore critical register
	ret

msptfirst:
	mov	ax,LastRelocEntryBlkPtr
	mov	FirstRelocEntryBlkPtr,ax	; update first allocated block pointer
	jmp	msptret

MakeRelocEntryBlk	ENDP

;*****************************
;* GETTARGETADDRESS          *
;*****************************

; compute target program offset for fixupp address
; uses TargetMethod, TargetDatum, TargetDisplacment memory variables
; can set IsUnresSymbol memory variable flag
; upon entry gs -> base i/o buffer
; return target program offset in memory variable TargetProgOffset
; destroys eax,bx,edx,fs

GetTargetAddress	PROC
	mov	ax,TargetDatum
	dec	ax				; make relative zero
	mov	bl,TargetMethod
	xor	bh,bh
	add	bx,bx			; convert to word offset
	jmp	[bx+OFFSET DGROUP:TargetJumpTable]	; transfer to proper handling code for method

; invalid target method (3,7)
poor10:
	mov	cl,10
	call	BadOBJModuleExit

; target specified by segment index, method 0,4
; ax holds target datum relative zero
gtasegment:
	lfs	bx,gs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; fs:bx -> individual segdef pointers
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to fixup segdef entry
	add	bx,ax			; fs:bx -> individual segdef entry, not normalized
	cmp	bx,SIZEINDSEGDEFBLK	; see if overflow to next block
	jae	gtanormseg		; yes

; fs:bx -> individual segdef entry
gtaseg2:
	test	fs:[bx+IndSegDefRecStruc.isdrFlags],ABSOLUTESEGMENTFLAG
	jne	gtaabsseg		; absolute segment
	mov	eax,TargetDisplacement	; get target displacement
	add	eax,fs:[bx+IndSegDefRecStruc.isdrSegOffset]	; add in individual segment offset

	lfs	bx,fs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; fs:bx -> master segdef entry
	mov	Is32BitSeg,OFF	; init 32-bit segment flag
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG
	je	gtaseg3			; is not 32-bit target segment
	mov	Is32BitSeg,ON	; flag 32-bit segment in case of pointer fixup

gtaseg3:
	mov	dx,fs:[bx+MasterSegDefRecStruc.mssSegmentID]	; get segment ID
	mov	FixupSegmentID,dx	; save segment ID
	mov	edx,fs:[bx+MasterSegDefRecStruc.mssSegOffset]	; get master segment offset
	mov	CanonicFrame,edx	; save as canonic frame in case of LOC type fixup
	add	eax,edx
	mov	TargetProgOffset,eax	; save target program offset
	ret

; segment entry has wrapped to next buffer
gtanormseg:
	sub	bx,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	fs,fs:[IndSegDefBlkStruc.isdbNextPtr]	; fs -> next block
	jmp	gtaseg2

; absolute segment
gtaabsseg:
	mov	al,ON
	mov	IsNotRelocatable,al	; flag nonrelocatable segment
	mov	IsAbsoluteSeg,al	; flag absolute segment
	lfs	bx,fs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; fs:bx -> master segdef entry
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegOffset]	; get frame number
	mov	TargetSegment,eax	; save as target segment
	shl	eax,4			; convert to bytes from segment address (x16)
	add	eax,TargetDisplacement	; add in original target displacement
	mov	TargetProgOffset,eax	; update target program offset
	ret

; target specified by group index, method 1,5
; ax holds target datum relative zero
gtagroup:
	lfs	bx,gs:[IOBuffHeaderStruc.ibhsPtrToGrpPtrs]	; fs:bx -> group pointer table
	shl	ax,2			; dword per entry
	add	bx,ax			; fs:bx -> pointer group entry, not normalized
	cmp	bx,SIZEGRPPTRTABLEBLK	; see if overflow to next block
	jae	gtanormgrp		; yes

; fs:bs -> group entry
gtagrp2:
	lfs	bx,fs:[bx]		; fs:bx -> group entry
	mov	eax,fs:[bx+GrpDefRecStruc.gdrGrpOffset]	; get group offset
	mov	CanonicFrame,eax	; save canonic frame
	add	eax,TargetDisplacement	; add in original target displacement
	mov	TargetProgOffset,eax	; update target program offset
	lfs	bx,fs:[bx+GrpDefRecStruc.gdrFirstSegPtr]	; fs:bx -> first segment of group
	mov	ax,fs:[bx+MasterSegDefRecStruc.mssSegmentID]	; get segment ID
	mov	FixupSegmentID,ax	; save segment ID
	ret

; group entry has wrapped to next buffer
gtanormgrp:
	sub	bx,(SIZEGRPPTRTABLEBLK-GRPPTRTABLESYSVARSIZE)
	mov	fs,fs:[GrpPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block
	jmp	gtagrp2

; target specified by external index, method 2,6
; ax holds target datum relative zero
gtaexternal:
	lfs	bx,gs:[IOBuffHeaderStruc.ibhsPtrToSymPtrs]	; fs:bx -> symbol pointer table
	shl	ax,2			; dword per entry
	add	bx,ax			; fs:bx -> pointer symbol entry, not normalized

gtaext2:
	cmp	bx,SIZESYMPTRTABLEBLK	; see if overflow to next block
	jae	gtanormsym		; yes

; fs:bx -> pointer to symbol entry
	lfs	bx,fs:[bx]		; fs:bx -> symbol entry

gtaext3:
	test	fs:[bx+PubSymRecStruc.pssFlags],(WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; see if weak/lazy extern
	jne	gtaweak			; yes
	test	fs:[bx+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	gtaunres		; unresolved external
	mov	eax,fs:[bx+PubSymRecStruc.pssOffset]	; get public symbol offset
	test	fs:[bx+PubSymRecStruc.pssFlags],ABSOLUTESYMBOLFLAG
	jne	gtaabssym		; absolute symbol
	add	TargetDisplacement,eax	; add public offset to target displacement
	lfs	bx,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; fs:bx -> owning segdef
	jmp	gtaseg2	; jump to code shared with segment index

gtaweak:
	lfs	bx,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; fs:bx -> default resolution symbol
	jmp	gtaext3

; symbol entry has wrapped to next buffer
gtanormsym:
	sub	bx,(SIZESYMPTRTABLEBLK-SYMPTRTABLESYSVARSIZE)
	mov	fs,fs:[SymPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block
	jmp	gtaext2

gtaabssym:
	mov	TargetProgOffset,eax	; update target program offset
	mov	al,ON
	mov	IsNotRelocatable,al	; flag not relocatable
	mov	IsAbsoluteSym,al	; flag absolute symbol
	mov	eax,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; get frame number
	mov	TargetSegment,eax	; save as target segment
	ret

; unresolved external referenced for fixup
gtaunres:

IFDEF DLLSUPPORT
	test	fs:[bx+PubSymRecStruc.pssFlags],IMPORTSYMBOLFLAG
	je	gtaunres2			; not an import symbol
	mov	IsIMPDEFSymbol,ON	; flag an import symbol
	mov	WORD PTR IMPDEFTargetPtr,bx	; save pointer to frame symbol
	mov	WORD PTR IMPDEFTargetPtr+2,fs
	push	fs			; save critical register
	push	bx
	lfs	bx,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; fs:bx -> IMPDEF entry
	mov	al,fs:[bx+IMPDEFRecStruc.idsOrdinalFlag]
	mov	IsOrdinalFlag,al
	mov	eax,fs:[bx+IMPDEFRecStruc.idsModuleNumber]
	mov	IMPDEFModuleNumber,eax
	mov	eax,fs:[bx+IMPDEFRecStruc.idsFunctionNumber]
	mov	IMPDEFFunctionNumber,eax
	pop	bx				; restore critical register
	pop	fs

gtaunres2:
ENDIF

	mov	IsUnresSymbol,ON	; flag unresolved
	mov	WORD PTR UnresSymPtr,bx	; keep pointer to unresolved symbol for feedback
	mov	WORD PTR UnresSymPtr+2,fs
	ret					; no further processing necessary
GetTargetAddress	ENDP

;*****************************
;* GETFRAMEADDRESS           *
;*****************************

; compute frame for fixupp address
; uses FrameMethod, FrameDatum or TargetMethod,TargetDatum  memory variables
; can set IsUnresSymbol memory variable flag
; upon entry gs -> base i/o buffer
; return frame in TargetSegment
; destroys eax,bx,edx,fs

GetFrameAddress	PROC
	mov	bl,FrameMethod
	mov	ax,FrameDatum

gfa2:
	dec	ax
	xor	bh,bh
	add	bx,bx			; convert to frame method word offset
	jmp	[bx+OFFSET DGROUP:FrameJumpTable]	; transfer to proper handling code for method

; invalid frame method (3,6,7)
poor11:
	mov	cl,11
	call	BadOBJModuleExit

; frame method 5, use target's datum and index
gfatarg:
	mov	bl,TargetMethod
	mov	ax,TargetDatum
	jmp	gfa2		; return to frame code with target variable values

; frame method 4, use canonic frame
gfacanon:
	mov	eax,CanonicFrame	; get canonic frame as bytes
	shr	eax,4			; convert to paras
	mov	TargetSegment,eax	; save as target segment
	ret

; frame specified by segment index, method 0
; ax holds frame datum relative zero
gfasegment:
	lfs	bx,gs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; fs:bx -> individual segdef pointers
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to fixup segdef entry
	add	bx,ax			; fs:bx -> individual segdef entry, not normalized
	cmp	bx,SIZEINDSEGDEFBLK	; see if overflow to next block
	jae	gfanormseg		; yes

; fs:bx -> individual segdef entry
gfaseg2:
	test	fs:[bx+IndSegDefRecStruc.isdrFlags],ABSOLUTESEGMENTFLAG
	jne	gfaabsseg		; absolute segment
	lfs	bx,fs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; fs:bx -> master segdef entry

IFDEF CLIPPER
	cmp	IsSpecialFixupOption,OFF
	je	gfaseg3
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],GROUPMEMBERFLAG
	je	gfaseg3			; not a member of a group
	lfs	bx,fs:[bx+MasterSegDefRecStruc.mssGroupPtr]	; fs:bx -> owning group
	jmp	gfagrp3

gfaseg3:
ENDIF

	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegOffset]	; get segment offset
	shr	eax,4			; convert to paras
	mov	TargetSegment,eax	; save as target segment
	mov	ax,fs:[bx+MasterSegDefRecStruc.mssSegmentID]	; get segment ID
	mov	FixupSegmentID,ax	; save segment ID
	ret

; segment entry has wrapped to next buffer
gfanormseg:
	sub	bx,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	fs,fs:[IndSegDefBlkStruc.isdbNextPtr]	; fs -> next block
	jmp	gfaseg2

gfaabsseg:
	mov	al,ON
	mov	IsNotRelocatable,al	; flag nonrelocatable segment
	mov	IsAbsoluteSeg,al	; flag absolute segment
	lfs	bx,fs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; fs:bx -> master segdef entry
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegOffset]	; get frame number
	mov	TargetSegment,eax	; save as target segment
	ret

; frame specified by group index, method 1
; ax holds frame datum relative zero
gfagroup:
	lfs	bx,gs:[IOBuffHeaderStruc.ibhsPtrToGrpPtrs]	; fs:bx -> group pointer table
	shl	ax,2			; dword per entry
	add	bx,ax			; fs:bx -> pointer group entry, not normalized
	cmp	bx,SIZEGRPPTRTABLEBLK	; see if overflow to next block
	jae	gfanormgrp		; yes

gfagrp2:
	lfs	bx,fs:[bx]		; fs:bx -> group entry

gfagrp3:
	mov	eax,fs:[bx+GrpDefRecStruc.gdrGrpOffset]	; get group offset
	shr	eax,4			; convert to paras
	mov	TargetSegment,eax	; save as target segment
	lfs	bx,fs:[bx+GrpDefRecStruc.gdrFirstSegPtr]	; fs:bx -> first segment of group
	mov	ax,fs:[bx+MasterSegDefRecStruc.mssSegmentID]	; get segment ID
	mov	FixupSegmentID,ax	; save segment ID
	ret

; group entry has wrapped to next buffer
gfanormgrp:
	sub	bx,(SIZEGRPPTRTABLEBLK-GRPPTRTABLESYSVARSIZE)
	mov	fs,fs:[GrpPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block
	jmp	gfagrp2

; frame specified by external index, method 2
; ax holds frame datum relative zero
gfaexternal:
	lfs	bx,gs:[IOBuffHeaderStruc.ibhsPtrToSymPtrs]	; fs:bx -> symbol pointer table
	shl	ax,2			; dword per entry
	add	bx,ax			; fs:bx -> pointer symbol entry, not normalized
	cmp	bx,SIZESYMPTRTABLEBLK	; see if overflow to next block
	jae	gfanormsym		; yes

; fs:bs -> pointer to symbol entry
gfaext2:
	lfs	bx,fs:[bx]		; fs:bx -> symbol entry

gfaext4:
	test	fs:[bx+PubSymRecStruc.pssFlags],(WEAKEXTERNFLAG OR LAZYEXTERNFLAG)	; see if weak/lazy extern
	jne	gfaweak			; yes

	test	fs:[bx+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	gfaunres		; unresolved external
	test	fs:[bx+PubSymRecStruc.pssFlags],ABSOLUTESYMBOLFLAG
	jne	gfaabssym		; absolute symbol
	test	fs:[bx+PubSymRecStruc.pssFlags],GROUPRELSYMBOLFLAG
	je	gfaext3			; not a member of a group
	lfs	bx,fs:[bx+PubSymRecStruc.pssGrpDefPtr]	; fs:bx -> owning group
	jmp	gfagrp3

gfaext3:
	lfs	bx,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; fs:bx -> owning segdef
	jmp	gfaseg2

gfaweak:
	lfs	bx,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; fs:bx -> default resolution symbol
	jmp	gfaext4

; symbol entry has wrapped to next buffer
gfanormsym:
	sub	bx,(SIZESYMPTRTABLEBLK-SYMPTRTABLESYSVARSIZE)
	mov	fs,fs:[SymPtrTableBlkStruc.isdbNextPtr]	; get pointer to next block
	jmp	gfaext2

gfaabssym:
	mov	al,ON
	mov	IsNotRelocatable,al	; flag not relocatable
	mov	IsAbsoluteSym,al	; flag absolute symbol
	mov	eax,fs:[bx+PubSymRecStruc.pssIndSegDefPtr]	; get frame number
	mov	TargetSegment,eax	; save as target segment
	ret

gfaunres:
IFDEF DLLSUPPORT
	test	fs:[bx+PubSymRecStruc.pssFlags],IMPORTSYMBOLFLAG
	je	gfaunres2			; not an import symbol
	mov	IsFrameIMPDEF,ON	; flag frame is import
	mov	WORD PTR IMPDEFFramePtr,bx	; save pointer to frame symbol
	mov	WORD PTR IMPDEFFramePtr+2,fs

gfaunres2:
ENDIF

	mov	IsUnresSymbol,ON	; flag unresolved
	mov	WORD PTR UnresSymPtr,bx	; keep pointer to unresolved symbol for feedback
	mov	WORD PTR UnresSymPtr+2,fs
	ret					; no further processing necessary
GetFrameAddress	ENDP

;*****************************
;* PASS2FIXUPP32PROC         *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass2FIXUPP32Proc	PROC
	cmp	IsDebugSeg,OFF	; see if debug segment
	je	p2f3start		; no
	ret					; ignore fixup

p2f3start:
	cmp	LastDataFlag,COMDAT
	je	p2f3comdat

p2f3chkadj:
	dec	cx				; adjust for checksum byte
	mov	gs,CurrentBaseOBJBuff	; gs -> i/o base buffer for fixup calculations
	mov	DataFixupCount,0	; init count of fixup items in array
	push	fs			; save fs -> record

p2f3fixloop:
	or	cx,cx			; see if at end of record
	je	p2f3done		; yes
	pop	fs				; restore fs -> record
	call	ReadByteDecCX	; read thread data field/low locat field byte
	test	al,FIXUPSUBRECORD	; see if fixup or thread subrecord
	jne	p2f3fixup		; fixup subrecord

; thread subrecord
	mov	bl,al
	and	bl,THREADNUMOFTHREADDATA	; get thread number in bx
	xor	bh,bh
	test	al,DBITOFTHREADDATA
	jne	p2f3frame		; frame thread
	and	al,TARGMETHOFTHREADDATA	; get target method (2 bits only)
	shr	al,2			; convert to relative zero
	mov	[bx+OFFSET DGROUP:ThreadTARGETMethod],al
	call	ReadIndexDecCX	; get thread index
	add	bx,bx			; convert to word offset
	mov	[bx+OFFSET DGROUP:ThreadTARGETDatum],ax
	push	fs			; save fs -> record to stack
	jmp	p2f3fixloop

p2f3comdat:
	lgs	di,LastCOMDATPtr	; gs:di -> last COMDAT pointer
	test	gs:[di+PubSymRecStruc.pssFlags],COMDATCOMPLETEFLAG
	jne	p2f3ret			; processing on this comdat is complete
;	or	gs:[di+PubSymRecStruc.pssFlags],COMDATFIXUPFLAG
	jmp	p2f3chkadj	; keep processing

p2f3frame:
	and	al,METHODOFTHREADDATA	; get method
	shr	al,2			; convert to relative zero
	mov	[bx+OFFSET DGROUP:ThreadFRAMEMethod],al
	call	ReadIndexDecCX	; get thread datum index
	add	bx,bx			; convert to word offset
	mov	[bx+OFFSET DGROUP:ThreadFRAMEDatum],ax
	push	fs			; save fs -> record to stack
	jmp	p2f3fixloop

p2f3fixup:
	mov	BYTE PTR Locat,al	; save low byte of locat word
	call	ReadByteDecCX
	mov	BYTE PTR Locat+1,al
	mov	ah,BYTE PTR Locat
	and	ah,3			; get two MSB of data record offset
	mov	al,BYTE PTR Locat+1	; get low byte of data record offset
	mov	DataRecordOffset,ax	; save data record offset from Locat subrecord
	mov	TargetDisplacement,0	; init target displacement

	call	ReadByteDecCX	; read fix data byte
	mov	FixData,al

; from the FixData subrecord:
;  frame datum index present iff F (80h) bit is reset and Frame (70h) field is 0-2 (0, 10h,20h)
;  target datum index present iff T (8) bit is reset
;  target displacement word present iff P (4) bit is reset
	cmp	al,2fh			; check if F bit reset and Frame field <=20h
	ja	p2f3tdat			; no
	call	ReadIndexDecCX	; get frame datum
	mov	FrameDatum,ax

p2f3tdat:
	test	FixData,TBITOFFIXDATA	; see if T bit set
	jne	p2f3tdisp		; yes, no target datum
	call	ReadIndexDecCX	; get target datum
	mov	TargetDatum,ax

p2f3tdisp:
	test	FixData,PBITOFFIXDATA	; see if P bit set
	jne	p2f32			; yes, no target displacement
	call	ReadDwordDecCX	; get target displacement (32-bit value)
	mov	TargetDisplacement,eax

p2f32:
	mov	al,FixData
	test	al,FBITOFFIXDATA	; see if thread field for frame (F bit)
	je	p2f3nothrfr		; no thread field for frame

	and	al,30h			; mask off F bit and high bit of 3-bit frame field for thread value
	shr	al,4			; convert to relative 0
	mov	bl,al
	xor	bh,bh
	mov	al,[bx+OFFSET DGROUP:ThreadFRAMEMethod]
	mov	FrameMethod,al	; save frame method as current
	add	bx,bx			; convert to word offset
	mov	ax,[bx+OFFSET DGROUP:ThreadFRAMEDatum]
	mov	FrameDatum,ax	; save frame datum index as current
	jmp	p2f33

p2f3nothrfr:
	and	al,FRAMEFIELDOFFIXDATA	; get frame field
	shr	al,4			; convert to relative zero
	mov	FrameMethod,al	; save frame method

p2f33:
	mov	al,FixData
	test	al,TBITOFFIXDATA	; see if thread field for target (T bit)
	je	p2f3nothrtar		; no thread field for target

	and	al,TARGTFIELDOFFIXDATA	; get targt field value in al
	mov	bl,al
	xor	bh,bh
	mov	al,[bx+OFFSET DGROUP:ThreadTARGETMethod]
	mov	TargetMethod,al	; save target method as current
	add	bx,bx			; convert to word offset
	mov	ax,[bx+OFFSET DGROUP:ThreadTARGETDatum]
	mov	TargetDatum,ax	; save target datum index as current
	jmp	p2f34

p2f3nothrtar:
	and	al,TARGTFIELDOFFIXDATA	; get targt field value in al
	mov	TargetMethod,al	; save target method

p2f34:
	push	fs			; save fs -> record to stack

	cmp	IsDebugSeg,OFF	; see if debug segment
	jne	p2f3fixloop		; yes, only trolling for thread fixups

	xor	al,al
	mov	IsUnresSymbol,al	; init unresolved symbol flag
	mov	IsNotRelocatable,al	; init not relocatable segment flag
	mov	IsAbsoluteSeg,al
	mov	IsAbsoluteSym,al

IFDEF	DLLSUPPORT
	mov	IsIMPDEFSymbol,al	; init impdef symbol flag
	mov	IsFrameIMPDEF,al
ENDIF

	call	GetTargetAddress	; get the fixup target address

	mov	al,IsAbsoluteSeg
	or	al,IsAbsoluteSym	; no frame computation on absolute segment or symbol
	jne	p2f3segrel			; absolute

	call	GetFrameAddress	; the fixup frame address

IFDEF DLLSUPPORT
	cmp	IsIMPDEFSymbol,OFF	; see if fixup on impdef symbol
	je	p2f3notimp			; no
	call	PerformIMPDEFFixup32
	jmp	p2f3fixloop

p2f3notimp:
ENDIF

;IFNDEF CLARION
;IFNDEF CLIPPER
;	cmp	IsFlatOption,0	; see if flat option turned on
;	je	p2f3cont		; no
;	mov	TargetSegment,0	; zero out the target segment

;p2f3cont:
;ENDIF
;ENDIF

	cmp	IsUnresSymbol,OFF	; see if unresolved symbol
	jne	p2f3unres		; yes

; convert target segment and target program offset to seg:off format
	test	Locat,MBITOFLOCAT	; check M bit for self-relative fixup
	je	p2f3selfrel		; M bit reset, self-relative fixup

IFDEF WATCOM_ASM
	cmp	IsFlatOption,0	; see if flat option turned on
	je	p2f3cont		; no
	mov	TargetSegment,0	; zero out the target segment

p2f3cont:
ENDIF

; fixup segment relative
p2f3segrel:
	mov	eax,TargetSegment
	shl	eax,4			; eax holds byte value of target segment

	mov	edx,TargetProgOffset	; edx holds target program offset
	sub	edx,eax			; edx==difference between frame and absolute program offset

; resolve the fixup, value in edx
p2f3res:

; check if fixup overflow, edx should be <=64K and >=-64K unless 32-bit offset
;@@@ code goes here

	lfs	bx,LDATADataRecPtr	; fs:bx -> L?DATA record data being fixed up
	add	bx,DataRecordOffset	; fs:bx -> location to be fixed up, not normalized
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jae	p2f3normbx1		; yes, normalize it

; fs:bx -> fixup location
p2f3res2:
	mov	al,BYTE PTR Locat
	and	al,LOCFIELDOFLOCAT	; get loc field
	cmp	al,OFFSETLOC	; see offset fixup
	jne	p2f3chkloc5		; no

; offset fixup
p2f3loc:
	cmp	Is32BitSeg,OFF	; see if fixing up 32-bit segment (32-bit offset)
	jne	p2f3loc32fix	; yes
	add	fs:[bx],dl		; adjust low byte of word
	adc	dh,0			; carry to high byte
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jae	p2f3normbx2		; yes, normalize it

p2f3loc2:
	add	fs:[bx],dh		; adjust high byte of word

IFDEF WATCOM_ASM
	cmp	IsFlatOption,0	; see if flat option turned on
	je	p2f3cont3		; no
	test	Locat,MBITOFLOCAT	; check M bit for self-relative fixup
	je	p2f3cont3		; M bit reset, self-relative fixup

	jmp	p2f3rel32off	; do fixups on 32-bit offsets, not self-relative

p2f3cont3:
ENDIF

	jmp	p2f3fixloop

p2f3normbx2:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2f3loc2

p2f3chkloc5:
	cmp	al,OFFSETLOC5	; see alternate method 5 offset fixup
;	je	p2f3loc
	je	p2f3chkphar		; see if phar lap format (32-bit offset)

	cmp	al,SEGMENTLOC	; see segment fixup
	jne	p2f3chkptr		; no

; segment fixup
p2f3seg:
	xor	dx,dx			; adjustment for fixup location to segment portion

; code shared with pointer fixup
p2f3segshared:
	mov	al,IsNotRelocatable
	or	al,IsCreateEXEOption
	je	p2f3seg32		; P3 format and not absolute segment, used segment ID for fixup
	mov	ax,WORD PTR TargetSegment
	add	fs:[bx],al		; adjust low byte of word
	adc	ah,0			; carry to high byte
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jae	p2f3normbx3		; yes, normalize it

p2f3seg2:
	add	fs:[bx],ah		; adjust high byte of word
	cmp	IsNotRelocatable,OFF	; see if relocatable segment
	jne	p2f3fixloop		; no

IFDEF WATCOM_ASM
	jmp	p2f3relproc

; create relocation entry for 32-bit offset
p2f3rel32off:
	cmp	IsNotRelocatable,OFF	; see if relocatable segment
	jne	p2f3fixloop		; no
;	xor	dx,dx
	mov	dx,8000h		; flags 32-bit offset relocation entry
ENDIF

; relocation entry store for later processing
p2f3relproc:
	add	dx,DataRecordOffset	; dx -> relocation fixup location relative L?DATA record
	mov	bx,DataFixupCount
	add	bx,bx			; word per entry
	mov	ds:[bx+WORD PTR RelocationTable],dx	; save relocation entry offset relative L?DATA record
	inc	DataFixupCount	; bump count of data fixups
	jmp	p2f3fixloop

; P3 format and not absolute segment, use segment ID for fixup
p2f3seg32:
	mov	ax,FixupSegmentID
	mov	fs:[bx],al		; adjust low byte of word
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2f3seg32b		; no
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables

p2f3seg32b:
	mov	fs:[bx],ah		; adjust high byte of word
	jmp	p2f3relproc

p2f3chkphar:
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	je	p2f3loc			; no, process LOC5 as normal LOC
	jmp	p2f3loc32fix	; phar lap module, process loc 5 as 32-bit offset

p2f3normbx3:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2f3seg2

p2f3chkptr:
	cmp	al,POINTERLOC	; see if pointer fixup
	jne	p2f3loc32		; no
	cmp	Is32BitSeg,OFF	; see if fixing up 32-bit segment (48-bit pointer)
	jne	p2f348bit		; yes

; pointer fixup
	add	fs:[bx],dl		; adjust low byte of word
	adc	dh,0			; carry to high byte
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jae	p2f3normbx4		; yes, normalize it

p2f3ptr2:
	add	fs:[bx],dh		; adjust high byte of word
	mov	dx,2			; adjustment for fixup location to segment portion
	inc	bx				; move to segment portion of fixup
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2f3segshared	; no
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2f3segshared

p2f3normbx4:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2f3ptr2

p2f3loc32:
	cmp	al,OFFSET32LOC	; see 32-bit offset fixup
	jne	p2f3chkloc3213	; no

; 32-bit offset fixup
p2f3loc32fix:
	mov	ax,dx			; get low word of offset
	shr	edx,16			; convert high word to 16-bit dx register

; 32-bit offset in dx:ax
	add	fs:[bx],al		; adjust low byte of word
	adc	ah,0			; carry to high byte
	adc	dx,0			; carry to high word
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2f3loc32a
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables

p2f3loc32a:
	add	fs:[bx],ah		; adjust high byte of word
	adc	dx,0			; carry to high word
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2f3loc32b
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables

; dx holds high word
p2f3loc32b:
	add	fs:[bx],dl		; adjust low byte of high word
	adc	dh,0			; carry to high byte of high word
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2f3loc32c
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables

p2f3loc32c:
	add	fs:[bx],dh		; adjust high byte of high word

IFDEF WATCOM_ASM
	cmp	IsFlatOption,0	; see if flat option turned on
	je	p2f3cont4		; no
	test	Locat,MBITOFLOCAT	; check M bit for self-relative fixup
	je	p2f3cont4		; M bit reset, self-relative fixup
	jmp	p2f3rel32off	; do fixups on 32-bit offsets

p2f3cont4:
ENDIF

	jmp	p2f3fixloop

p2f3chkloc3213:
	cmp	al,OFFSET32LOC13	; see alternate method 13 offset fixup
	je	p2f3loc32fix

p2f3chkptr48:
	cmp	al,POINTER48LOC	; see 48-bit pointer fixup
	jne	p2f3chklow		; no

; 48-bit pointer fixup (32-bit offset, 16-bit segment)
p2f348bit:
	mov	ax,dx			; get low word of offset
	shr	edx,16			; convert high word to 16-bit dx register

; 32-bit offset in dx:ax
	add	fs:[bx],al		; adjust low byte of word
	adc	ah,0			; carry to high byte
	adc	dx,0			; carry to high word
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2f3ptr48a
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables

p2f3ptr48a:
	add	fs:[bx],ah		; adjust high byte of word
	adc	dx,0			; carry to high word
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2f3ptr48b
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables

; dx holds high word
p2f3ptr48b:
	add	fs:[bx],dl		; adjust low byte of high word
	adc	dh,0			; carry to high byte of high word
	inc	bx				; move to next location to fix up
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point
	jb	p2f3ptr48c
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables

p2f3ptr48c:
	add	fs:[bx],dh		; adjust high byte of high word
	mov	dx,4			; adjustment for fixup location to segment portion
	inc	bx				; move to segment portion of fixup
	cmp	bx,SIZEIOBUFFBLK	; see if bx is past wrap point

IFNDEF WATCOM_ASM
	jb	p2f3segshared	; not past wrap point
ELSE
	jb	p2f3chk48rel
ENDIF

	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables

IFNDEF WATCOM_ASM
	jmp	p2f3segshared	; do segment adjustment
ENDIF

IFDEF WATCOM_ASM
p2f3chk48rel:
	cmp	IsFlatOption,0	; see if flat option turned on
	je	p2f3segshared	; no
	mov	dx,8000h		; flags 32-bit offset relocation entry
	add	dx,DataRecordOffset	; dx -> relocation fixup location relative L?DATA record
	push	bx			; save critical register
	mov	bx,DataFixupCount
	add	bx,bx			; word per entry
	mov	ds:[bx+WORD PTR RelocationTable],dx	; save relocation entry offset relative L?DATA record
	inc	DataFixupCount	; bump count of data fixups
	pop	bx				; restore critical register
	mov	dx,4			; adjustment for fixup location to segment portion
	jmp	p2f3segshared	; do segment adjustment
ENDIF

p2f3chklow:
	cmp	al,LOWORDERBYTELOC	; see if lower order byte fixup
	jne	poor15			; no bad loc value

; low-order byte fixup
	add	fs:[bx],dl

IFDEF WATCOM_ASM
	cmp	IsFlatOption,0	; see if flat option turned on
	je	p2f3cont5		; no
	test	Locat,MBITOFLOCAT	; check M bit for self-relative fixup
	je	p2f3cont5		; M bit reset, self-relative fixup
	jmp	p2f3rel32off	; do fixups on 32-bit offsets

p2f3cont5:
ENDIF

	jmp	p2f3fixloop

p2f3normbx1:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	sub	bx,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust past system variables
	jmp	p2f3res2

; invalid loc field (4,6,7,8,10,11,12,14,15)
poor15:
	mov	cl,15
	call	BadOBJModuleExit

; self-relative fixup
p2f3selfrel:
	lfs	bx,LDATAIndSegPtr	; fs:bx -> individual segdef entry

COMMENT !
	mov	ax,LastLEDATASegIndex	; get last segment index
	sub	ax,1			; make relative zero
	jc	poor25

	lfs	bx,gs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; fs:bx -> individual segdef pointers
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to fixup segdef entry
	add	bx,ax			; fs:bx -> individual segdef entry, not normalized
	cmp	bx,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	p2f3self2		; no
	sub	bx,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	fs,fs:[IndSegDefBlkStruc.isdbNextPtr]	; fs -> next block

; fs:bx -> individual segdef entry
p2f3self2:
END COMMENT !

	mov	eax,fs:[bx+IndSegDefRecStruc.isdrSegOffset]	; get individual segment offset
	lfs	bx,fs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; fs:bx -> master segdef entry
	add	eax,fs:[bx+MasterSegDefRecStruc.mssSegOffset]	; add in master segment offset for absolute segment offset

	mov	edx,TargetProgOffset	; get absolute program offset
	sub	edx,eax			; subtract off individual segment offset (make relative to segment)

	sub	edx,LogicalDataRecOff	; make relative to logical data record

	mov	ax,DataRecordOffset
	movzx	eax,ax		; eax==data record offset value
	sub	edx,eax			; make target offset relative to location

	mov	al,BYTE PTR Locat
	and	al,LOCFIELDOFLOCAT	; get loc field

	cmp	al,LOWORDERBYTELOC	; see if low-order byte loc field
	jne	p2f3self3		; no
	dec	edx				; adjust for one-byte length
	jmp	p2f3res

p2f3self3:
	cmp	al,OFFSETLOC	; see if offset byte loc field
	je	p2f3locoff
	cmp	al,OFFSETLOC5	; check alternate offset value
	jne	p2f4self4		; not an allowable self-relative fixup loc field value

p2f3locoff:
	cmp	Is32BitSeg,OFF	; see if fixing up 32-bit segment (32-bit offset)
	jne	p2f3loc32off	; yes
	sub	edx,2			; adjust for two-byte field
	jmp	p2f3res

p2f4self4:
	cmp	al,OFFSET32LOC	; see if 32-bit offset byte loc field
	je	p2f3loc32off
	cmp	al,OFFSET32LOC13	; check alternate 32-bit offset value
	jne	poor14			; not an allowable self-relative fixup loc field value

p2f3loc32off:
	sub	edx,4			; adjust for four-byte field
	jmp	p2f3res

; invalid loc field for self-relative fixup (2,3,4,6,7,8,10,12,14,15)
poor14:
	mov	cl,14
	call	BadOBJModuleExit	; no return

; bad segment index (comdat fixup hosed)
poor25:
	mov	cl,25
	call	BadOBJModuleExit

; done with this round of fixups
p2f3done:
	pop	ax				; get fs pointer off of stack, fs unchanged, pop to nonseg register
	cmp	DataFixupCount,0	; see if any relocation items to process
	je	p2f3ret			; no relocation items

; process relocation items
	lgs	bx,LDATAIndSegPtr	; gs:bx -> individual segdef entry

COMMENT !
; process relocation items
	mov	ax,LastLEDATASegIndex	; get last segment index
	sub	ax,1			; make relative zero
	jc	poor25
	lgs	bx,gs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; gs:bx -> individual segdef pointers
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to fixup segdef entry
	add	bx,ax			; fs:bx -> individual segdef entry, not normalized
	cmp	bx,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	p2f3rel2			; no
	sub	bx,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	gs,gs:[IndSegDefBlkStruc.isdbNextPtr]	; gs -> next block

; gs:bx -> individual segdef entry
p2f3rel2:
END COMMENT !

	mov	edx,gs:[bx+IndSegDefRecStruc.isdrSegOffset]	; edx==individual segment offset
	add	edx,LogicalDataRecOff	; add enumerated data record offset

	lgs	bx,gs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; gs:bx -> master segdef entry
	mov	eax,gs:[bx+MasterSegDefRecStruc.mssSegOffset]	; get in master segment offset
	mov	ebx,eax			; save seg offset
	and	eax,0fh			; get offset remainder of seg address
	add	edx,eax			; add to offset adjustment

	shr	ebx,4			; convert seg offset to paras
	mov	si,OFFSET DGROUP:RelocationTable
	mov	cx,DataFixupCount	; count of relocation entries

	cmp	LastDataFlag,LIDATA32
	je	p2f3rellid

	cmp	LastDataFlag,LIDATA
	je	p2f3rellid

	movzx	ecx,cx		; extend to 32 bits for following addition
	add	RelocEntryCount,ecx	; bump count of permanent relocation entries
	push	es			; save critical register

p2f3rellast:
	mov	ax,LastRelocEntryBlkPtr	; ax -> last allocated block
	or	ax,ax			; zero if no previous allocations
	je	p2f3relnew			; no previous allocations, block must be allocated

; check if allocated relocation entry block is full
	mov	es,ax			; es -> relocation entry block

p2f3rel3:
	mov	di,es:[RelocEntryBlkStruc.rebCount]
	shl	di,3			; two dwords per entry
	add	di,RELOCENTRYSYSVARSIZE	; adjust for sysvars at beginning of block

p2f3relloop:
	cmp	es:[RelocEntryBlkStruc.rebCount],MAXCOUNTRELOCENTRYBLK	; see if any free entries in block
	jae	p2f3relnew		; no free entry exists
	inc	es:[RelocEntryBlkStruc.rebCount]	; bump count of entry
	lodsw				; get 16-bit offset from temporary fixup storage
	movzx	eax,ax		; zero high word

IFDEF WATCOM_ASM
	test	ax,8000h	; see if 32-bit offset fixup
	je	p2f3cont2		; no
	and	ax,7fffh		; turn off bit flag
	or	eax,FLATSEGMENTFIXUPFLAG	; and flag segment fixup
	
p2f3cont2:
ENDIF

	add	eax,edx			; add in segment+record offset
	stosd				; save offset

;;**** TEMPORARY
;	cmp	eax,(0c7991h or FLATSEGMENTFIXUPFLAG)
;	jne	bugger1
;heyman:
;	nop

;bugger1:
	mov	eax,ebx
	stosd				; save segment value (from master segment of L?DATA record)

	loop	p2f3relloop	; loop through all entries
	pop	es				; restore critical register

p2f3ret:
	ret

; make a new block
p2f3relnew:
	call	MakeRelocEntryBlk	; allocate relocation entry block
	jmp	p2f3rel3

p2f3rellid:
	push	es			; save critical register

p2f3lidrlast:
	mov	ax,LastRelocEntryBlkPtr	; ax -> last allocated block
	or	ax,ax			; zero if no previous allocations
	jne	p2f3lidr2
	call	MakeRelocEntryBlk	; allocate relocation entry block
	jmp	p2f3lidrloop

p2f3lidr2:
	mov	es,ax			; es -> relocation entry block

p2f3lidrloop:
	lodsw				; get 16-bit offset from temporary fixup storage

	mov	LIDATARelocOffset,ax	; save lidata record relocation offset
	push	cx			; save critical registers
	push	si
;	push	edx
	lfs	si,LDATAFullRecPtr	; fs:si -> LIDATA record
	call	ReadByte	; scan past type
	call	ReadWordCX	; cx holds record length

	call	ReadIndexDecCX	; scan past segment index

; since both LIDATA and LIDATA32 records could have a 32-bit fixupp
;  a distinction must be made
	cmp	LastDataFlag,LIDATA32
	je	p2f3lid32

; fixup is on a LIDATA record
	call	ReadWordDecCX	; scan past 16-bit iterated data offset
	mov	DataOffset32,0	; init data offset, don't use record's
	dec	cx				; adjust for checksum byte

p2f3lid16loop:
	jcxz	p2f3lidrdone		; no more bytes to write
	mov	CurrentRelocOffset,0	; init relocation entry offset in lidata record
	call	RelocRecurseLIDATA	; use recursive routine to extract data out of lidata record
	jmp	p2f3lid16loop

; fixup is on a LIDATA32 record
p2f3lid32:
	call	ReadDwordDecCX	; scan past iterated data offset
	mov	DataOffset32,0	; init data offset, don't use record's
	dec	cx				; adjust for checksum byte

p2f3lidrrecloop:
	jcxz	p2f3lidrdone		; no more bytes to write
	mov	CurrentRelocOffset,0	; init relocation entry offset in lidata record
	call	RelocRecurseLIDATA32	; use recursive routine to extract data out of lidata record
	jmp	p2f3lidrrecloop

p2f3lidrdone:
;	pop	edx				; restore critical registers
	pop	si
	pop	cx
	loop	p2f3lidrloop
	pop	es				; restore critical register
	ret

; unresolved external, UnresSymPtr -> symbol entry
; warn, ignore fixup
p2f3unres:
	call	UnresExternalWarn
	jmp	p2f3fixloop

Pass2FIXUPP32Proc	ENDP

;*****************************
;* RELOCRECURSELIDATA32      *
;*****************************

; use recursion to get all data out of LIDATA32 record
; upon entry fs:si -> current data byte, cx == size of data block,
;  edx == segment+record offset
; destroys eax

RelocRecurseLIDATA32  PROC
	cmp	OS2ModuleFlag,OFF	; see if processing os/2 flagged module
	jne	rrlid3rep		; yes, don't do special phar lap exception
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	jne	rrlid3phar		; yes, repeat count is only 16-bit

rrlid3rep:
	call	ReadDwordDecCx	; get repeat count

rrlid3scount:
	mov	RepeatCount32,eax	; save it
	call	ReadWordDecCx	; get block count
	mov	BlockCount,ax	; save it
	add	CurrentRelocOffset,6	; update lidata record relocation offset
	push	CurrentRelocOffset
	push	si			; save buffer position
	push	cx			; save record iterated data block length

rrlid3reploop:
	pop	cx				; restore record length
	pop	si				; restore buffer position
	pop	CurrentRelocOffset
	push	CurrentRelocOffset
	push	si			; put values back on stack
	push	cx

	cmp	BlockCount,0	; see if nested iterated data blocks
	je	rrlid3data		; no

; nested iterated data blocks
	mov	ax,BlockCount	; get number of blocks to loop through

rrlid3blkloop:
	push	ax			; save current number of blocks left to loop through
	push	RepeatCount32	; save repeat value
	push	BlockCount	; save block value
	call	RelocRecurseLIDATA32	; nest down one level
	pop	BlockCount		; save block value
	pop	RepeatCount32	; restore repeat value
	pop	ax				; restore number of blocks left to loop through
	dec	ax				; one iteration complete
	jne	rrlid3blkloop		; loop not complete

; block loop is complete, do next iteration of repeat loop
rrlid3nextrep:
	dec	RepeatCount32
	jne	rrlid3reploop		; more repeat iterations to do

	add	sp,6			; trash old record length and record position values on stack
	ret					; ; repeat loop complete, return to next highest level or calling procedure

; block size is zero, data follows
rrlid3data:
	call	ReadByteDecCx	; get length of data to write
	inc	CurrentRelocOffset

; write the data bytes
	push	cx			; save record's total data length
	mov	cl,al
	xor	ch,ch			; data bytes to write in cx
	push	cx			; save data bytes written

	mov	ax,LIDATARelocOffset

; 05/09/96
	test	ah,80h		; see if 32-bit nonsegment fixup
	je	rrlid3segadj	; no
	and	ah,7fh			; remove 32-bit nonsegment fixup flag for comparison
	jmp	rrlid3cmp

rrlid3segadj:
	add	ax,4			; put into segment territory if pointer fixup
	sub	ax,cx			; adjust down to start to data byte offset

rrlid3cmp:
	cmp	ax,CurrentRelocOffset
	jne	rrlid3scan

rrlid3relblk:
	mov	di,es:[RelocEntryBlkStruc.rebCount]
	shl	di,3			; two dwords per entry
	add	di,RELOCENTRYSYSVARSIZE	; adjust for sysvars at beginning of block
	cmp	es:[RelocEntryBlkStruc.rebCount],MAXCOUNTRELOCENTRYBLK	; see if any free entries in block
	jb	rrlid32			; free entry exists
	call	MakeRelocEntryBlk
;	mov	es,ax			; es -> new block
	jmp	rrlid3relblk

rrlid32:
	inc	es:[RelocEntryBlkStruc.rebCount]	; bump count of entry
	inc	RelocEntryCount	; bump count of permanent relocation entries

	mov	eax,DataOffset32	; data offset in ax

	movzx	ecx,cx
	add	eax,ecx			; point to just past final byte write

	test	BYTE PTR LIDATARelocOffset+1,80h	; see if 32-bit nonsegment
	je	rrlid3segadj2
	sub	eax,4			; adjust down to start of 32-bit territory
	or	eax,FLATSEGMENTFIXUPFLAG	; flag flat segment fixup
	jmp	rrlid3store

rrlid3segadj2:
	sub	eax,2			; adjust down to segment territory

rrlid3store:
	add	eax,edx			; add in segment+record offset
	stosd
	mov	eax,ebx
	stosd

rrlid3scan:
	movzx	eax,cx		; get data byte count
	add	DataOffset32,eax	; update data offset with bytes written
	call	ScanAhead	; move source buffer ahead bytes written
	mov	ax,cx			; bytes written count to ax
	add	CurrentRelocOffset,ax	; update record offset
	pop	cx				; restore record's total data length
	sub	cx,ax			; subtract off data bytes written
	jmp	rrlid3nextrep

; phar lap module, 16-bit repeat count
rrlid3phar:
	call	ReadWordDecCx	; get repeat count
	movzx	eax,ax		; extend to 32-bit for normal processing
	jmp	rrlid3scount

RelocRecurseLIDATA32  ENDP

;*****************************
;* PASS2EXTDEFPROC           *
;*****************************

Pass2EXTDEFProc	PROC
	ret
Pass2EXTDEFProc	ENDP

;*****************************
;* PASS2PUBDEFPROC           *
;*****************************

Pass2PUBDEFProc	PROC
	ret
Pass2PUBDEFProc	ENDP

;*****************************
;* PASS2PUBDEF32PROC         *
;*****************************

Pass2PUBDEF32Proc	PROC
	ret
Pass2PUBDEF32Proc	ENDP

;*****************************
;* PASS2LEXTDEFPROC          *
;*****************************

Pass2LEXTDEFProc	PROC
	ret
Pass2LEXTDEFProc	ENDP

;*****************************
;* PASS2LPUBDEFPROC          *
;*****************************

Pass2LPUBDEFProc	PROC
	ret
Pass2LPUBDEFProc	ENDP

;*****************************
;* PASS2LPUBDEF32PROC        *
;*****************************

Pass2LPUBDEF32Proc	PROC
	ret
Pass2LPUBDEF32Proc	ENDP

;*****************************
;* PASS2SEGDEFPROC           *
;*****************************

; upon entry fs:si -> read buffer, cx holds record length

Pass2SEGDEFProc	PROC
	ret
Pass2SEGDEFProc	ENDP

;*****************************
;* PASS2SEGDEF32PROC         *
;*****************************

Pass2SEGDEF32Proc	PROC
	ret
Pass2SEGDEF32Proc	ENDP

;*****************************
;* PASS2COMDEFPROC           *
;*****************************

Pass2COMDEFProc	PROC
	ret
Pass2COMDEFProc	ENDP

;*****************************
;* PASS2LCOMDEFPROC          *
;*****************************

Pass2LCOMDEFProc	PROC
	ret
Pass2LCOMDEFProc	ENDP

;*****************************
;* PASS2LIDATAPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass2LIDATAProc	PROC
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	jne	Pass2LIDATA32Proc	; yes, LIDATA's are processed as LIDATA32's, sort of

	mov	eax,OBJRecPtr
	mov	LDATAFullRecPtr,eax	; save for LIDATA relocation fixups
	mov	LastDataFlag,LIDATA
	call	ReadIndex	; get segment index in ax
	call	ReadWordCX	; get iterated data offset into cx
	movzx	ecx,cx			; make 32-bit
	mov	LogicalDataRecOff,ecx	; save logical data record offset
	mov	WORD PTR LDATADataRecPtr,si	; save pointer to record data for later fixups
	mov	WORD PTR LDATADataRecPtr+2,fs
	cmp	ax,LastLEDATASegIndex	; see if equal to last segment index (no flagging needed)
	mov	LastLEDATASegIndex,ax	; update last LEDATA segment index, KEEP FLAG STATUS
	je	pli2			; matches, no need to update
	call	SetSegDefDataFlag

pli2:
	ret
Pass2LIDATAProc	ENDP

;*****************************
;* PASS2LIDATA32PROC         *
;*****************************

Pass2LIDATA32Proc	PROC
	mov	eax,OBJRecPtr
	mov	LDATAFullRecPtr,eax	; save for LIDATA relocation fixups
	mov	LastDataFlag,LIDATA32
	call	ReadIndex	; get segment index in ax
	call	ReadDwordECX	; get iterated data offset into cx
	mov	LogicalDataRecOff,ecx	; save logical data record offset
	mov	WORD PTR LDATADataRecPtr,si	; save pointer to record data for later fixups
	mov	WORD PTR LDATADataRecPtr+2,fs
	cmp	ax,LastLEDATASegIndex	; see if equal to last segment index (no flagging needed)
	mov	LastLEDATASegIndex,ax	; update last LEDATA segment index, KEEP FLAG STATUS
	je	pli32			; matches, no need to update
	call	SetSegDefDataFlag

pli32:
	ret
Pass2LIDATA32Proc	ENDP

;*****************************
;* PASS2LNAMESPROC           *
;*****************************

; upon entry fs:si -> read buffer, cx holds record length

Pass2LNAMESProc	PROC

plnloop:
	cmp	cx,1			; see if at end of record
	jbe	plnret			; yes
	inc	CurrentLNAMESIndex
	mov	ax,CurrentLNAMESIndex
	mov	di,ax			; save index value
	shl	ax,2			; total size of current index pointer table, dword per entry

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
	jmp	plnloop	; get pointer to next LNAMES name, if any

plnret:
	ret
Pass2LNAMESProc	ENDP

;*****************************
;* PASS2COMENTPROC           *
;*****************************

Pass2COMENTProc	PROC
	ret
Pass2COMENTProc	ENDP

;*****************************
;* PASS2CEXTDEFPROC          *
;*****************************

Pass2CEXTDEFProc	PROC
	ret
Pass2CEXTDEFProc	ENDP

;*****************************
;* PASS2COMDATPROC           *
;*****************************

Pass2COMDATProc	PROC
	push	es
	mov	LastDataFlag,COMDAT
	mov	LastLEDATASegIndex,0	; re-init segment index of L?DATA record

	call	ReadByteDecCX	; get comdat flags
	mov	COMDATFlags,al
	call	ReadByteDecCX	; get comdat attributes
	call	ReadByteDecCX	; get align
	call	ReadWordDecCX	; get offset
	movzx	eax,ax
	mov	LogicalDataRecOff,eax	; save offset
	call	ReadIndexDecCX	; scan past type index
	call	ReadIndexDecCX	; get public name index
	mov	WORD PTR LDATADataRecPtr,si	; save pointer to record data for later fixups
	mov	WORD PTR LDATADataRecPtr+2,fs

	mov	fs,LNAMESIndexSel	; fs -> LNAMES table of pointers block
	mov	si,ax			; get logical name index
	dec	si				; make relative zero
	shl	si,2			; convert to dword offset
	lfs	si,fs:[si]		; fs:si -> logical name, from dword entries table

; fs:si -> symbol name with length byte prepended
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	mov	SearchExistSymFlag,ON	; flag only search for existence
	call	GetPubSymEntry	; search for public symbol, should always be present
	jc	p2cderr			; internal error
	mov	WORD PTR LastCOMDATPtr,di	; save -> last comdat for fixup
	mov	WORD PTR LastCOMDATPtr+2,gs

	les	bx,gs:[di+PubSymRecStruc.pssIndSegDefPtr]	; es:bx -> segdef entry
	mov	WORD PTR LDATAIndSegPtr,bx	; save -> owning individual segdef entry
	mov	WORD PTR LDATAIndSegPtr+2,es

	mov	IsDebugSeg,OFF	; init debug segment flag

; see if data flag previously set
	test	es:[bx+IndSegDefRecStruc.isdrFlags],ASSOCIATEDDATAFLAG
	jne	p2cchkdone		; flag previously set, ignore

; set flag to show data associated with segdef in module, keep pointer to first data block
; fs:si -> data block
	or	es:[bx+IndSegDefRecStruc.isdrFlags],ASSOCIATEDDATAFLAG
	mov	eax,OBJRecPtr
	mov	es:[bx+IndSegDefRecStruc.isdrDataPtr],eax
	les	bx,es:[bx+IndSegDefRecStruc.isdrMasterPtr]	; es:bx -> master segdef

	test	es:[bx+MasterSegDefRecStruc.mssFlags],(DEBUGTYPESSEGMENTFLAG OR DEBUGSYMBOLSSEGMENTFLAG)
	je	p2cchk32		; not a debug segment

; debug segment, so flag, and reset associated data so debug keeps getting flagged
	les	bx,LDATAIndSegPtr	; es:bx -> individual segdef
	and	es:[bx+IndSegDefRecStruc.isdrFlags],NOT ASSOCIATEDDATAFLAG
	mov	IsDebugSeg,ON
	jmp	p2cchkdone		; don't worry about 32-bit segments if they are debug

p2cchk32:
	or	es:[bx+MasterSegDefRecStruc.mssFlags],ASSOCIATEDDATAFLAG
	mov	Is32BitSeg,OFF	; init 32-bit segment flag
	test	es:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG	; flag if 32-bit segment
	je	p2cchkdone		; not 32-bit segment
	mov	Is32BitSeg,ON	; flag 32-bit segment

; see if comdat has completed all process (first module processing done)
p2cchkdone:
	test	gs:[di+PubSymRecStruc.pssFlags],COMDATCOMPLETEFLAG
	jne	p2cret			; comdat complete

	test	gs:[di+PubSymRecStruc.pssFlags],PASS2COMDATFLAG
	je	p2cret			; no previous comdat

	test	COMDATFlags,COMDATCONTINUEFLAG
	jne	p2cret			; current continuation

; process not flagged complete, previous comdat occurred, no current continuation
; turn on process complete flag
	or	gs:[di+PubSymRecStruc.pssFlags],COMDATCOMPLETEFLAG

p2cret:
	or	gs:[di+PubSymRecStruc.pssFlags],PASS2COMDATFLAG	; flag previous comdat
	pop	es
	ret

p2cderr:
	mov	cl,22
	call	InternalErrorExit	; no return

Pass2COMDATProc	ENDP

;*****************************
;* PASS2COMDAT32PROC         *
;*****************************

Pass2COMDAT32Proc	PROC
	push	es
	mov	LastDataFlag,COMDAT
	mov	LastLEDATASegIndex,0	; re-init segment index of L?DATA record

	call	ReadByteDecCX	; get comdat flags
	mov	COMDATFlags,al
	call	ReadByteDecCX	; get comdat attributes
	push	ax				; save attributes
	call	ReadByteDecCX	; get align
	call	ReadDwordDecCX	; get 32-bit offset
	mov	LogicalDataRecOff,eax	; save offset
	call	ReadIndexDecCX	; scan past type index
	pop	ax
	test	al,COMDATALLOCTYPEFIELD	; mask attributes to allocation type
	jne	p2c32pubind			; allocation not explicit (nonzero)

; public base fields present
	call	ReadIndexDecCX	; scan past group index
	call	ReadIndexDecCX	; get segment index
	test	ax,ax			; see if segment index is zero
	jne	p2c32pubind			; no frame number
	call	ReadWordDecCX	; scan past frame number

p2c32pubind:
	call	ReadIndexDecCX	; get public name index
	mov	WORD PTR LDATADataRecPtr,si	; save pointer to record data for later fixups
	mov	WORD PTR LDATADataRecPtr+2,fs

	mov	fs,LNAMESIndexSel	; fs -> LNAMES table of pointers block
	mov	si,ax			; get logical name index
	dec	si				; make relative zero
	shl	si,2			; convert to dword offset
	lfs	si,fs:[si]		; fs:si -> logical name, from dword entries table

; fs:si -> symbol name with length byte prepended
	mov	IsLocalSymbol,OFF	; flag not a local symbol
	mov	SearchExistSymFlag,ON	; flag only search for existence
	call	GetPubSymEntry	; search for public symbol, should always be present
	jc	p2cd32err			; internal error
	mov	WORD PTR LastCOMDATPtr,di	; save -> last comdat for fixup
	mov	WORD PTR LastCOMDATPtr+2,gs

	les	bx,gs:[di+PubSymRecStruc.pssIndSegDefPtr]	; es:bx -> segdef entry
	mov	WORD PTR LDATAIndSegPtr,bx	; save -> owning individual segdef entry
	mov	WORD PTR LDATAIndSegPtr+2,es

	mov	IsDebugSeg,OFF	; init debug segment flag

; see if data flag previously set
	test	es:[bx+IndSegDefRecStruc.isdrFlags],ASSOCIATEDDATAFLAG
	jne	p2c32chkdone	; flag previously set, ignore

; set flag to show data associated with segdef in module, keep pointer to first data block
; fs:si -> data block
	or	es:[bx+IndSegDefRecStruc.isdrFlags],ASSOCIATEDDATAFLAG
	mov	eax,OBJRecPtr
	mov	es:[bx+IndSegDefRecStruc.isdrDataPtr],eax
	les	bx,es:[bx+IndSegDefRecStruc.isdrMasterPtr]	; es:bx -> master segdef

	test	es:[bx+MasterSegDefRecStruc.mssFlags],(DEBUGTYPESSEGMENTFLAG OR DEBUGSYMBOLSSEGMENTFLAG)
	je	p2c32chk32		; not a debug segment

; debug segment, so flag, and reset associated data so debug keeps getting flagged
	les	bx,LDATAIndSegPtr	; es:bx -> individual segdef
	and	es:[bx+IndSegDefRecStruc.isdrFlags],NOT ASSOCIATEDDATAFLAG
	mov	IsDebugSeg,ON
	jmp	p2c32chkdone	; don't worry about 32-bit segments if they are debug

p2c32chk32:
	or	es:[bx+MasterSegDefRecStruc.mssFlags],ASSOCIATEDDATAFLAG
	mov	Is32BitSeg,OFF	; init 32-bit segment flag
	test	es:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG	; flag if 32-bit segment
	je	p2c32chkdone	; not 32-bit segment
	mov	Is32BitSeg,ON	; flag 32-bit segment

; see if comdat has completed all process (first module processing done)
p2c32chkdone:
	test	gs:[di+PubSymRecStruc.pssFlags],COMDATCOMPLETEFLAG
	jne	p2c32ret			; comdat complete

	test	gs:[di+PubSymRecStruc.pssFlags],PASS2COMDATFLAG
	je	p2c32ret			; no previous comdat

	test	COMDATFlags,COMDATCONTINUEFLAG
	jne	p2c32ret			; current continuation

; process not flagged complete, previous comdat occurred, no current continuation
; turn on process complete flag
	or	gs:[di+PubSymRecStruc.pssFlags],COMDATCOMPLETEFLAG

p2c32ret:
	or	gs:[di+PubSymRecStruc.pssFlags],PASS2COMDATFLAG	; flag previous comdat
	pop	es
	ret

p2cd32err:
	mov	cl,22
	call	InternalErrorExit	; no return
	ret
Pass2COMDAT32Proc	ENDP

;*****************************
;* PASS2BAKPATPROC           *
;*****************************

Pass2BAKPATProc	PROC
	ret
Pass2BAKPATProc	ENDP

;*****************************
;* PASS2BAKPAT32PROC         *
;*****************************

Pass2BAKPAT32Proc	PROC
	ret
Pass2BAKPAT32Proc	ENDP

;*****************************
;* PASS2NBKPATPROC           *
;*****************************

Pass2NBKPATProc	PROC
	ret
Pass2NBKPATProc	ENDP

;*****************************
;* PASS2NBKPAT32PROC         *
;*****************************

Pass2NBKPAT32Proc	PROC
	ret
Pass2NBKPAT32Proc	ENDP

;*****************************
;* PASS2GRPDEFPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass2GRPDEFProc	PROC
	ret
Pass2GRPDEFProc	ENDP

;*****************************
;* PASS2THEADRPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass2THEADRProc	PROC
	ret
Pass2THEADRProc	ENDP

;*****************************
;* PASS2LHEADRPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass2LHEADRProc	PROC
	ret
Pass2LHEADRProc	ENDP

;*****************************
;* PASS2MODENDPROC           *
;*****************************

; upon entry si -> read buffer, cx holds record length

Pass2MODENDProc	PROC
	mov	EndOfOBJFlag,ON	; flag at end of module
	mov	gs,CurrentBaseOBJBuff	; gs -> i/o base buffer for fixup calculations
	cmp	IsEntryPoint,OFF	; see if previous entry point
	jne	p2mret			; yes, ignore further processing
	call	ReadByteDecCX	; get module type byte
	test	al,STARTBITOFMODTYPE	; see if start address bit set
	je	p2mret			; no, no further processing needed
	call	ReadByteDecCX	; read end data byte
	mov	EndData,al

; from the EndData subrecord:
;  frame datum index present iff F (80h) bit is reset and Frame (70h) field is 0-2 (0, 10h,20h)
;  target datum index present iff T (8) bit is reset
;  target displacement word always present -- THIS IS DIFFERENT FROM NORMAL FIXUPP RECORDS
	cmp	al,2fh			; check if F bit reset and Frame field <=20h
	ja	p2mtdat			; no
	call	ReadIndexDecCX	; get frame datum
	mov	FrameDatum,ax

p2mtdat:
	test	EndData,TBITOFFIXDATA	; see if T bit set
	jne	p2mtdisp		; yes, no target datum
	call	ReadIndexDecCX	; get target datum
	mov	TargetDatum,ax

p2mtdisp:
	call	ReadWordDecCX	; get target displacement
	movzx	eax,ax
	mov	TargetDisplacement,eax

p2m2:
	mov	al,EndData
	test	al,FBITOFFIXDATA	; see if thread field for frame (F bit)
	je	p2mnothrfr		; no thread field for frame

	and	al,30h			; mask off F bit and high bit of 3-bit frame field for thread value
	shr	al,4			; convert to relative 0
	mov	bl,al
	xor	bh,bh
	mov	al,[bx+OFFSET DGROUP:ThreadFRAMEMethod]
	mov	FrameMethod,al	; save frame method as current
	add	bx,bx			; convert to word offset
	mov	ax,[bx+OFFSET DGROUP:ThreadFRAMEDatum]
	mov	FrameDatum,ax	; save frame datum index as current
	jmp	p2m3

p2mnothrfr:
	and	al,FRAMEFIELDOFFIXDATA	; get frame field
	shr	al,4			; convert to relative zero
	mov	FrameMethod,al	; save frame method

p2m3:
	mov	al,EndData
	test	al,TBITOFFIXDATA	; see if thread field for target (T bit)
	je	p2mnothrtar		; no thread field for target

	and	al,TARGTFIELDOFFIXDATA	; get targt field value in al
	mov	bl,al
	xor	bh,bh
	mov	al,[bx+OFFSET DGROUP:ThreadTARGETMethod]
	mov	TargetMethod,al	; save target method as current
	add	bx,bx			; convert to word offset
	mov	ax,[bx+OFFSET DGROUP:ThreadTARGETDatum]
	mov	TargetDatum,ax	; save target datum index as current
	jmp	p2m4

p2mnothrtar:
	and	al,TARGTFIELDOFFIXDATA	; get targt field value in al
	mov	TargetMethod,al	; save target method

p2m4:
	xor	al,al
	mov	IsUnresSymbol,al	; init unresolved symbol flag
	mov	IsNotRelocatable,al	; init not relocatable segment flag
	mov	IsAbsoluteSeg,al
	mov	IsAbsoluteSym,al
	call	GetTargetAddress	; get the fixup target address

	mov	al,IsAbsoluteSeg
	or	al,IsAbsoluteSym	; no frame computation on absolute segment or symbol
	jne	p2msegrel			; absolute

	call	GetFrameAddress	; the fixup frame address
	cmp	IsUnresSymbol,OFF	; see if unresolved symbol
	jne	p2munres		; yes

; fixup segment relative
p2msegrel:
	mov	eax,TargetSegment
	mov	EntrySegmentValue,eax	; save program's entry segment value

; convert program's entry segment and offset value to seg:off format
	shl	eax,4			; convert entry segment to byte offset
	mov	edx,TargetProgOffset
	sub	edx,eax
	mov	EntryOffsetValue,edx	; save entry offset value
	mov	ax,FixupSegmentID
	mov	EntrySegmentID,ax	; save program's entry segment ID
	mov	IsEntryPoint,ON	; flag entry point exists

p2mret:
	ret

; unresolved external, UnresSymPtr -> symbol entry
; warn, ignore fixup
p2munres:
	call	UnresExternalWarn
	jmp	p2mret

Pass2MODENDProc	ENDP

;*****************************
;* PASS2MODEND32PROC         *
;*****************************

Pass2MODEND32Proc	PROC
	mov	EndOfOBJFlag,ON	; flag at end of module
	mov	gs,CurrentBaseOBJBuff	; gs -> i/o base buffer for fixup calculations
	cmp	IsEntryPoint,OFF	; see if previous entry point
	jne	p2m3ret			; yes, ignore further processing
	call	ReadByteDecCX	; get module type byte
	test	al,STARTBITOFMODTYPE	; see if start address bit set
	je	p2m3ret			; no, no further processing needed
	call	ReadByteDecCX	; read end data byte
	mov	EndData,al

; from the EndData subrecord:
;  frame datum index present iff F (80h) bit is reset and Frame (70h) field is 0-2 (0, 10h,20h)
;  target datum index present iff T (8) bit is reset
;  target displacement word always present -- THIS IS DIFFERENT FROM NORMAL FIXUPP RECORDS
	cmp	al,2fh			; check if F bit reset and Frame field <=20h
	ja	p2m3tdat			; no
	call	ReadIndexDecCX	; get frame datum
	mov	FrameDatum,ax

p2m3tdat:
	test	EndData,TBITOFFIXDATA	; see if T bit set
	jne	p2m3tdisp		; yes, no target datum
	call	ReadIndexDecCX	; get target datum
	mov	TargetDatum,ax

p2m3tdisp:
	call	ReadDwordDecCX	; get target displacement (dword value)
	mov	TargetDisplacement,eax

p2m32:
	mov	al,EndData
	test	al,FBITOFFIXDATA	; see if thread field for frame (F bit)
	je	p2m3nothrfr		; no thread field for frame

	and	al,30h			; mask off F bit and high bit of 3-bit frame field for thread value
	shr	al,4			; convert to relative 0
	mov	bl,al
	xor	bh,bh
	mov	al,[bx+OFFSET DGROUP:ThreadFRAMEMethod]
	mov	FrameMethod,al	; save frame method as current
	add	bx,bx			; convert to word offset
	mov	ax,[bx+OFFSET DGROUP:ThreadFRAMEDatum]
	mov	FrameDatum,ax	; save frame datum index as current
	jmp	p2m33

p2m3nothrfr:
	and	al,FRAMEFIELDOFFIXDATA	; get frame field
	shr	al,4			; convert to relative zero
	mov	FrameMethod,al	; save frame method

p2m33:
	mov	al,EndData
	test	al,TBITOFFIXDATA	; see if thread field for target (T bit)
	je	p2m3nothrtar		; no thread field for target

	and	al,TARGTFIELDOFFIXDATA	; get targt field value in al
	mov	bl,al
	xor	bh,bh
	mov	al,[bx+OFFSET DGROUP:ThreadTARGETMethod]
	mov	TargetMethod,al	; save target method as current
	add	bx,bx			; convert to word offset
	mov	ax,[bx+OFFSET DGROUP:ThreadTARGETDatum]
	mov	TargetDatum,ax	; save target datum index as current
	jmp	p2m34

p2m3nothrtar:
	and	al,TARGTFIELDOFFIXDATA	; get targt field value in al
	mov	TargetMethod,al	; save target method

p2m34:
	xor	al,al
	mov	IsUnresSymbol,al	; init unresolved symbol flag
	mov	IsNotRelocatable,al	; init not relocatable segment flag
	mov	IsAbsoluteSeg,al
	mov	IsAbsoluteSym,al
	call	GetTargetAddress	; get the fixup target address

	mov	al,IsAbsoluteSeg
	or	al,IsAbsoluteSym	; no frame computation on absolute segment or symbol
	jne	p2m3segrel			; absolute

	call	GetFrameAddress	; the fixup frame address

IFDEF WATCOM_ASM
	cmp	IsFlatOption,0	; see if flat option turned on
	je	p2m3cont		; no
	mov	TargetSegment,0	; zero out the target segment

p2m3cont:
ENDIF

	cmp	IsUnresSymbol,OFF	; see if unresolved symbol
	jne	p2m3unres		; yes

; fixup segment relative
p2m3segrel:
	mov	eax,TargetSegment
	mov	EntrySegmentValue,eax	; save program's entry segment value

; convert program's entry segment and offset value to seg:off format
	shl	eax,4			; convert entry segment to byte offset
	mov	edx,TargetProgOffset
	sub	edx,eax
	mov	EntryOffsetValue,edx	; save entry offset value
	mov	ax,FixupSegmentID
	mov	EntrySegmentID,ax	; save program's entry segment ID
	mov	IsEntryPoint,ON	; flag entry point exists

p2m3ret:
	ret

; unresolved external, UnresSymPtr -> symbol entry
; warn, ignore fixup
p2m3unres:
	call	UnresExternalWarn
	jmp	p2m3ret

Pass2MODEND32Proc	ENDP

;*****************************
;* PASS2MSLIBRPROC           *
;*****************************

Pass2MSLIBRProc	PROC
	ret
Pass2MSLIBRProc	ENDP

;*****************************
;* PASS2LINNUMPROC           *
;*****************************

Pass2LINNUMProc	PROC
	cmp	IsSYMOption,OFF	; see if /sy option set
	je	pnmret			; no

	push	es			; save critical register
	call	ReadIndexDecCX	; get base group (ignore)
	call	ReadIndexDecCX	; get base segment

; lookup individual segdef pointer
	mov	gs,CurrentBaseOBJBuff	; gs -> i/o buffer base
	lgs	bx,gs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; gs:bx -> first segdef entry
	dec	ax				; make index relative zero
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to symbol's segdef entry
	add	bx,ax			; gs:bx -> symbol's individual segment entry, not normalized
	cmp	bx,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	pnmgetoff		; no

; segment entry has wrapped to next buffer
	sub	bx,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	gs,gs:[IndSegDefBlkStruc.isdbNextPtr]	; get pointer to next block

; gs:bx -> individual segdef entry
pnmgetoff:
	mov	edx,gs:[bx+IndSegDefRecStruc.isdrSegOffset]	; get offset from base segment address
	les	di,gs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; es:di => master segdef
	mov	ax,es:[di+MasterSegDefRecStruc.mssSegmentID]	; get segment ID
	mov	LineSegmentID,ax	; save line number segment
	mov	ebx,edx			; segment offset to ebx register

	mov	gs,CurrentBaseOBJBuff
	cmp	gs:[IOBuffHeaderStruc.ibhsSourceLinePtr],0
	jne	pnmoldblk		; previous source block

; allocate a new source line block
pnmalloc:
	call	MakeSourceLineBlk	; return pointer to new block in es
	jmp	pnm2

pnmoldblk:
	mov	es,gs:[IOBuffHeaderStruc.ibhsSourceLinePtr]	; es -> source block
	mov	ax,LineSegmentID

pnmchkmatch:
	cmp	es:[SourceLineBlkStruc.ssbSegmentID],ax	; see if matching segment ID
	je	pnmmatch		; yes
	cmp	es:[SourceLineBlkStruc.ssbNextSegPtr],0	; see if another segment to check
	je	pnmalloc		; no
	mov	es,es:[SourceLineBlkStruc.ssbNextSegPtr]	; es -> next source line segment ID
	jmp	pnmchkmatch

pnmmatch:
	cmp	es:[SourceLineBlkStruc.ssbNextContPtr],0	; see if continuation
	je	pnm2			; no
	mov	es,es:[SourceLineBlkStruc.ssbNextContPtr]
	jmp	pnmmatch	; es -> continuation block

pnm2:
	mov	di,WORD PTR es:[SourceLineBlkStruc.ssbCount]	; get count (known word value)
	shl	di,3			; convert to byte offset (x8) for two dwords
	add	di,SOURCELINESYSVARSIZE	; adjust for system variables

; es:di -> current source line entry
pnmlineloop:
	cmp	cx,1			; see if at end of record
	jbe	pnmdone			; yes
	cmp	es:[SourceLineBlkStruc.ssbCount],MAXCOUNTSOURCELINEBLK	; see if any free entries in block
	jb	pnmstore		; free entry exists

; make a new block
	call	MakeSourceLineBlk	; allocate source line block
	mov	di,SOURCELINESYSVARSIZE	; adjust di past system variables to first entry

; store the line number
; ebx holds segment offset
; es:di -> source line entry, es -> block
pnmstore:
	inc	es:[SourceLineBlkStruc.ssbCount]	; bump count of entries
	call	ReadWordDecCX
	movzx	eax,ax		; zero extend to dword
	stosd				; store the line number
	call	ReadWordDecCX
	movzx	eax,ax		; zero extend to dword
	add	eax,ebx			; add in segment offset
	stosd				; store the line offset
	jmp	pnmlineloop	; scan through all line entries

pnmdone:
	pop	es				; restore critical register

pnmret:
	ret
Pass2LINNUMProc	ENDP

;*****************************
;* PASS2LINNUM32PROC         *
;*****************************

Pass2LINNUM32Proc	PROC
	cmp	IsSYMOption,OFF	; see if /sy option set
	je	pnm3ret			; no

	push	es			; save critical register
	call	ReadIndexDecCX	; get base group (ignore)
	call	ReadIndexDecCX	; get base segment

; lookup individual segdef pointer
	mov	gs,CurrentBaseOBJBuff	; gs -> i/o buffer base
	lgs	bx,gs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; gs:bx -> first segdef entry
	dec	ax				; make index relative zero
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to symbol's segdef entry
	add	bx,ax			; gs:bx -> symbol's individual segment entry, not normalized
	cmp	bx,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	pnm3getoff		; no

; segment entry has wrapped to next buffer
	sub	bx,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	gs,gs:[IndSegDefBlkStruc.isdbNextPtr]	; get pointer to next block

; gs:bx -> individual segdef entry
pnm3getoff:
	mov	edx,gs:[bx+IndSegDefRecStruc.isdrSegOffset]	; get offset from base segment address
	les	di,gs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; es:di => master segdef
	mov	ax,es:[di+MasterSegDefRecStruc.mssSegmentID]	; get segment ID
	mov	LineSegmentID,ax	; save line number segment
	mov	ebx,edx			; segment offset to ebx register

	mov	gs,CurrentBaseOBJBuff
	cmp	gs:[IOBuffHeaderStruc.ibhsSourceLinePtr],0
	jne	pnm3oldblk		; previous source block

; allocate a new source line block
pnm3alloc:
	call	MakeSourceLineBlk	; return pointer to new block in es
	jmp	pnm32

pnm3oldblk:
	mov	es,gs:[IOBuffHeaderStruc.ibhsSourceLinePtr]	; es -> source block
	mov	ax,LineSegmentID

pnm3chkmatch:
	cmp	es:[SourceLineBlkStruc.ssbSegmentID],ax	; see if matching segment ID
	je	pnm3match		; yes
	cmp	es:[SourceLineBlkStruc.ssbNextSegPtr],0	; see if another segment to check
	je	pnm3alloc		; no
	mov	es,es:[SourceLineBlkStruc.ssbNextSegPtr]	; es -> next source line segment ID
	jmp	pnm3chkmatch

pnm3match:
	cmp	es:[SourceLineBlkStruc.ssbNextContPtr],0	; see if continuation
	je	pnm32			; no
	mov	es,es:[SourceLineBlkStruc.ssbNextContPtr]
	jmp	pnm3match	; es -> continuation block

pnm32:
	mov	di,WORD PTR es:[SourceLineBlkStruc.ssbCount]	; get count (known word value)
	shl	di,3			; convert to byte offset (x8) for two dwords
	add	di,SOURCELINESYSVARSIZE	; adjust for system variables

; es:di -> current source line entry
pnm3lineloop:
	cmp	cx,1			; see if at end of record
	jbe	pnm3done			; yes
	cmp	es:[SourceLineBlkStruc.ssbCount],MAXCOUNTSOURCELINEBLK	; see if any free entries in block
	jb	pnm3store		; free entry exists

; make a new block
	call	MakeSourceLineBlk	; allocate source line block
	mov	di,SOURCELINESYSVARSIZE	; adjust di past system variables to first entry

; store the line number
; ebx holds segment offset
; es:di -> source line entry, es -> block
pnm3store:
	inc	es:[SourceLineBlkStruc.ssbCount]	; bump count of entries
	call	ReadWordDecCX
	movzx	eax,ax		; zero extend to dword
	stosd				; store the line number
	call	ReadDWordDecCX
	add	eax,ebx			; add in segment offset
	stosd				; store the line offset
	jmp	pnm3lineloop	; scan through all line entries

pnm3done:
	pop	es				; restore critical register

pnm3ret:
	ret
Pass2LINNUM32Proc	ENDP

;*****************************
;* PASS2LINSYMPROC           *
;*****************************

Pass2LINSYMProc	PROC
	ret
Pass2LINSYMProc	ENDP

;*****************************
;* PASS2LINSYM32PROC         *
;*****************************

Pass2LINSYM32Proc	PROC
	ret
Pass2LINSYM32Proc	ENDP

;*****************************
;* PASS2ALIASPROC            *
;*****************************

Pass2AliasProc	PROC
	ret
Pass2AliasProc	ENDP

IFDEF DLLSUPPORT

;*****************************
;* GETIMPDEFFIXUPTYPEA       *
;*****************************

; process fixup type, put in IMPDEFFixupType variable
; destroys al

GetIMPDEFFixupType	PROC
	mov	al,BYTE PTR Locat
	and	al,LOCFIELDOFLOCAT	; get loc field
	cmp	al,OFFSETLOC
	jne	gifloc2

gifoffchk:
	cmp	Is32BitSeg,OFF	; see if fixing up 32-bit segment (32-bit offset)
	jne	gifoff32			; yes
	mov	al,IMPDEFFIXUP16OFF
	jmp	gifchkself

gifloc2:
	cmp	al,OFFSETLOC5
	je	gifoffchk

	cmp	al,SEGMENTLOC
	jne	gifloc3
	mov	al,IMPDEFFIXUP16SEGONLY
	jmp	gifchkself

gifloc3:
	cmp	al,POINTERLOC
	jne	gifloc4
	cmp	Is32BitSeg,OFF	; see if fixing up 32-bit segment (32-bit offset)
	jne	gifseg32			; yes
	mov	al,IMPDEFFIXUP16SEGOFF
	jmp	gifchkself

gifloc4:
	cmp	al,OFFSET32LOC
	jne	gifloc5

gifoff32:
	mov	al,IMPDEFFIXUP32OFF
	jmp	gifchkself

gifloc5:
	cmp	al,OFFSET32LOC13
	je	gifoff32

	cmp	al,POINTER48LOC
	jne	poor23			; disallowed fixup type for impdef

gifseg32:
	mov	al,IMPDEFFIXUP32SEGOFF

gifchkself:
	test	Locat,MBITOFLOCAT	; check M bit for self-relative fixup
	jne	gifchkord			; M bit set, not self-relative fixup
	or	al,IMPDEFSELFRELATIVEFLAG

gifchkord:
	cmp	IsOrdinalFlag,OFF	; see if ordinal flag tripped
	je	gifsavetype			; no
	or	al,IMPDEFORDINALFLAG

gifsavetype:
	mov	IMPDEFFixupType,al	; save type of impdef fixup
	ret

; invalid fixup type for IMPDEF
poor23:
	mov	cl,23
	call	BadOBJModuleExit

GetIMPDEFFixupType	ENDP

;*****************************
;* PERFORMIMPDEFFIXUP        *
;*****************************

; process IMPDEF fixup
; conserve cx,si,fs,gs

PerformIMPDEFFixup	PROC
	push	fs			; save critical registers
	push	gs
	push	cx
	push	si
	lgs	bx,LDATAIndSegPtr	; gs:bx -> individual segdef
	mov	edx,gs:[bx+IndSegDefRecStruc.isdrSegOffset]	; edx==individual segment offset
	add	edx,LogicalDataRecOff	; add enumerated data record offset

	lgs	bx,gs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; gs:bx -> master segdef entry
	mov	eax,gs:[bx+MasterSegDefRecStruc.mssSegOffset]	; get in master segment offset
;	and	eax,0fh			; get offset remainder of seg address
	add	edx,eax			; add to offset adjustment

	cmp	LastDataFlag,LIDATA
	je	pifrellid

	movzx	eax,DataRecordOffset	; eax -> relocation fixup location relative L?DATA record
	add	eax,edx
	mov	IMPDEFFixupOffset,eax	; save impdef fixup offset
	call	SaveIMPDEFFixup
	jmp	pifret

pifrellid:
	mov	ax,DataRecordOffset	; ax -> relocation fixup location relative L?DATA record

	mov	LIDATARelocOffset,ax	; save lidata record relocation offset
	lfs	si,LDATAFullRecPtr	; fs:si -> LIDATA record
	call	ReadByte	; scan past type
	call	ReadWordCX	; cx holds record length

	call	ReadIndexDecCX	; scan past segment index
	call	ReadWordDecCX	; scan past iterated data offset
	mov	DataOffset32,0	; init data offset, don't use record's
	dec	cx				; adjust for checksum byte

piflidrrecloop:
	jcxz	pifret		; no more bytes to write
	mov	CurrentRelocOffset,0	; init relocation entry offset in lidata record
	call	RelocRecurseLIDATA	; use recursive routine to extract data out of lidata record
	jmp	piflidrrecloop

pifret:
	pop	si				; restore critical registers
	pop	cx
	pop	gs
	pop	fs
	ret

PerformIMPDEFFixup	ENDP

;*****************************
;* IMPDEFRECURSELIDATA       *
;*****************************

; use recursion to get all data out of LIDATA record
; upon entry fs:si -> current data byte, cx == size of data block,
; destroys eax,dx

IMPDEFRecurseLIDATA  PROC
	call	ReadWordDecCx	; get repeat count
	mov	RepeatCount,ax	; save it
	call	ReadWordDecCx	; get block count
	mov	BlockCount,ax	; save it
	add	CurrentRelocOffset,4	; update lidata record relocation offset
	push	CurrentRelocOffset
	push	si			; save buffer position
	push	cx			; save record iterated data block length

irlidreploop:
	pop	cx				; restore record length
	pop	si				; restore buffer position
	pop	CurrentRelocOffset
	push	CurrentRelocOffset
	push	si			; put values back on stack
	push	cx

	cmp	BlockCount,0	; see if nested iterated data blocks
	je	irliddata		; no

; nested iterated data blocks
	mov	ax,BlockCount	; get number of blocks to loop through

irlidblkloop:
	push	ax			; save current number of blocks left to loop through
	push	RepeatCount	; save repeat value
	push	BlockCount	; save block value
	call	RelocRecurseLIDATA	; nest down one level
	pop	BlockCount		; save block value
	pop	RepeatCount		; restore repeat value
	pop	ax				; restore number of blocks left to loop through
	dec	ax				; one iteration complete
	jne	irlidblkloop		; loop not complete

; block loop is complete, do next iteration of repeat loop
irlidnextrep:
	dec	RepeatCount
	jne	irlidreploop		; more repeat iterations to do

	add	sp,6			; trash old record length and record position values on stack
	ret					; repeat loop complete, return to next highest level or calling procedure

; block size is zero, data follows
irliddata:
	call	ReadByteDecCx	; get length of data to write
	inc	CurrentRelocOffset

; write the data bytes
	push	cx			; save record's total data length
	mov	cl,al
	xor	ch,ch			; data bytes to write in cx
	mov	ax,LIDATARelocOffset

;	add	ax,2			; put into segment territory if pointer fixup
	sub	ax,cx			; adjust down to start to data byte offset

	cmp	ax,CurrentRelocOffset
	jne	irlidscan

irlidrelblk:
	mov	di,es:[RelocEntryBlkStruc.rebCount]
	shl	di,3			; two dwords per entry
	add	di,RELOCENTRYSYSVARSIZE	; adjust for sysvars at beginning of block
	cmp	es:[RelocEntryBlkStruc.rebCount],MAXCOUNTRELOCENTRYBLK	; see if any free entries in block
	jb	irlid2			; free entry exists
	call	MakeRelocEntryBlk
;	mov	es,ax			; es -> new block
	jmp	irlidrelblk

irlid2:
	inc	es:[RelocEntryBlkStruc.rebCount]	; bump count of entry
	inc	RelocEntryCount	; bump count of permanent relocation entries

	mov	eax,DataOffset32	; data offset in ax

	movzx	ecx,cx
	add	eax,ecx			; point to just past final byte write
	sub	eax,4			; adjust down to offset territory

	add	eax,edx			; add in segment+record offset
	mov	IMPDEFFixupOffset,eax	; save impdef fixup offset
	call	SaveIMPDEFFixup

irlidscan:
	movzx	eax,cx		; get data byte count
	add	DataOffset32,eax	; update data offset with byte count
	call	ScanAhead	; move source buffer ahead byte count
	mov	ax,cx			; byte count to ax
	add	CurrentRelocOffset,ax	; update record offset
	pop	cx				; restore record's total data length
	sub	cx,ax			; subtract off data byte count
	jmp	irlidnextrep

IMPDEFRecurseLIDATA  ENDP

;*****************************
;* PERFORMIMPDEFFIXUP32      *
;*****************************

; process IMPDEF 32-bit fixup
; conserve cx,si,fs,gs

PerformIMPDEFFixup32	PROC
	push	fs			; save critical registers
	push	gs
	push	cx
	push	si
	lgs	bx,LDATAIndSegPtr	; gs:bx -> individual segdef
	mov	edx,gs:[bx+IndSegDefRecStruc.isdrSegOffset]	; edx==individual segment offset
	add	edx,LogicalDataRecOff	; add enumerated data record offset

	lgs	bx,gs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; gs:bx -> master segdef entry
	mov	eax,gs:[bx+MasterSegDefRecStruc.mssSegOffset]	; get in master segment offset
;	and	eax,0fh			; get offset remainder of seg address
	add	edx,eax			; add to offset adjustment

	cmp	LastDataFlag,LIDATA32
	je	pif3rellid

	movzx	eax,DataRecordOffset	; eax -> relocation fixup location relative L?DATA record
	add	eax,edx
	call	SaveIMPDEFFixup
	jmp	pif3ret

pif3rellid:
	mov	ax,DataRecordOffset	; ax -> relocation fixup location relative L?DATA record

	mov	LIDATARelocOffset,ax	; save lidata record relocation offset
	lfs	si,LDATAFullRecPtr	; fs:si -> LIDATA record
	call	ReadByte	; scan past type
	call	ReadWordCX	; cx holds record length

	call	ReadIndexDecCX	; scan past segment index
	call	ReadDwordDecCX	; scan past iterated data offset
	mov	DataOffset32,0	; init data offset, don't use record's
	dec	cx				; adjust for checksum byte

pif3lidrrecloop:
	jcxz	pif3ret		; no more bytes to write
	mov	CurrentRelocOffset,0	; init relocation entry offset in lidata record
	call	RelocRecurseLIDATA32	; use recursive routine to extract data out of lidata record
	jmp	pif3lidrrecloop

pif3ret:
	pop	si				; restore critical registers
	pop	cx
	pop	gs
	pop	fs
	ret
PerformIMPDEFFixup32	ENDP

;*****************************
;* IMPDEFRECURSELIDATA32     *
;*****************************

; use recursion to get all data out of LIDATA32 record
; upon entry fs:si -> current data byte, cx == size of data block,
; destroys eax,dx

IMPDEFRecurseLIDATA32  PROC
	cmp	OS2ModuleFlag,OFF	; see if processing os/2 flagged module
	jne	irlid3rep		; yes, don't do special phar lap exception
	cmp	PharLapModuleFlag,OFF	; see if processing phar lap module
	jne	irlid3phar		; yes, repeat count is only 16-bit

irlid3rep:
	call	ReadDwordDecCx	; get repeat count

irlid3scount:
	mov	RepeatCount32,eax	; save it
	call	ReadWordDecCx	; get block count
	mov	BlockCount,ax	; save it
	add	CurrentRelocOffset,6	; update lidata record relocation offset
	push	CurrentRelocOffset
	push	si			; save buffer position
	push	cx			; save record iterated data block length

irlid3reploop:
	pop	cx				; restore record length
	pop	si				; restore buffer position
	pop	CurrentRelocOffset
	push	CurrentRelocOffset
	push	si			; put values back on stack
	push	cx

	cmp	BlockCount,0	; see if nested iterated data blocks
	je	irlid3data		; no

; nested iterated data blocks
	mov	ax,BlockCount	; get number of blocks to loop through

irlid3blkloop:
	push	ax			; save current number of blocks left to loop through
	push	RepeatCount32	; save repeat value
	push	BlockCount	; save block value
	call	RelocRecurseLIDATA32	; nest down one level
	pop	BlockCount		; save block value
	pop	RepeatCount32	; restore repeat value
	pop	ax				; restore number of blocks left to loop through
	dec	ax				; one iteration complete
	jne	irlid3blkloop		; loop not complete

; block loop is complete, do next iteration of repeat loop
irlid3nextrep:
	dec	RepeatCount32
	jne	irlid3reploop		; more repeat iterations to do

	add	sp,6			; trash old record length and record position values on stack
	ret					; ; repeat loop complete, return to next highest level or calling procedure

; block size is zero, data follows
irlid3data:
	call	ReadByteDecCx	; get length of data to write
	inc	CurrentRelocOffset

; write the data bytes
	push	cx			; save record's total data length
	mov	cl,al
	xor	ch,ch			; data bytes to write in cx
	push	cx			; save data bytes written

	mov	ax,LIDATARelocOffset
	sub	ax,cx			; adjust down to start to data byte offset

	cmp	ax,CurrentRelocOffset
	jne	irlid3scan

irlid3relblk:
	mov	di,es:[RelocEntryBlkStruc.rebCount]
	shl	di,3			; two dwords per entry
	add	di,RELOCENTRYSYSVARSIZE	; adjust for sysvars at beginning of block
	cmp	es:[RelocEntryBlkStruc.rebCount],MAXCOUNTRELOCENTRYBLK	; see if any free entries in block
	jb	irlid32			; free entry exists
	call	MakeRelocEntryBlk
;	mov	es,ax			; es -> new block
	jmp	irlid3relblk

irlid32:
	inc	es:[RelocEntryBlkStruc.rebCount]	; bump count of entry
	inc	RelocEntryCount	; bump count of permanent relocation entries

	mov	eax,DataOffset32	; data offset in ax

	movzx	ecx,cx
	add	eax,ecx			; point to just past final byte write
	sub	eax,6			; adjust down to offset territory

	add	eax,edx			; add in segment+record offset
	mov	IMPDEFFixupOffset,eax	; save impdef fixup offset
	call	SaveIMPDEFFixup

irlid3scan:
	movzx	eax,cx		; get data byte count
	add	DataOffset32,eax	; update data offset with bytes written
	call	ScanAhead	; move source buffer ahead bytes written
	mov	ax,cx			; bytes written count to ax
	add	CurrentRelocOffset,ax	; update record offset
	pop	cx				; restore record's total data length
	sub	cx,ax			; subtract off data bytes written
	jmp	irlid3nextrep

; phar lap module, 16-bit repeat count
irlid3phar:
	call	ReadWordDecCx	; get repeat count
	movzx	eax,ax		; extend to 32-bit for normal processing
	JMP	Irlid3scount

IMPDEFRecurseLIDATA32  ENDP

;*****************************
;* SAVEIMPDEFFIXUP           *
;*****************************

; save IMPDEF fixup information
; data variables hold pertinent information to save
; conserve cx,si,fs

SaveIMPDEFFixup	PROC
	push	fs			; save critical registers
	push	esi
	push	ecx

; check that frame pointer and target pointer point to same symbol
	cmp	IsFrameIMPDEF,OFF	; see if frame not set
	je	poor24			; bad frame for impdef
	mov	eax,IMPDEFFramePtr
	cmp	eax,IMPDEFTargetPtr
	jne	poor24

	call	GetIMPDEFFixupType

	mov	ax,IMPDEFFixupSel	; ax -> IMPDEF fixup table
	test	ax,ax		; see if exists
	jne	sif2			; yes

; allocate table for IMPDEF fixups, 4K to start
	mov	edx,4096
	mov	MaxIMPDEFFixupAlloc,edx
	call	AllocateMemory
	mov	IMPDEFFixupSel,ax

sif2:
	mov	edi,TotalIMPDEFFixupSize	; edi offsets into fixup table
	mov	ebx,edi			; check that allocation is large enough for new fixup
	add	ebx,14
	cmp	ebx,MaxIMPDEFFixupAlloc
	jb	sif3
	mov	eax,MaxIMPDEFFixupAlloc	; need to expand allocation
	add	eax,4096
	mov	MaxIMPDEFFixupAlloc,eax
	mov	edx,eax
	mov	ax,IMPDEFFixupSel
	call	ResizeMemory32	; resize allocation an addditional 4K bytes

sif3:
	mov	gs,ax			; gs -> fixup table
	mov	ecx,7			; initial size of fixup entry
	mov	esi,1			; init offset of function name number from module name number
	mov	al,IMPDEFFixupType
	mov	gs:[edi],al
	inc	edi
	xor	eax,eax			; al==module number length relative zero
	cmp	BYTE PTR IMPDEFModuleNumber+1,0
	je	sif4
	inc	al				; length at least 2 bytes
	inc	ecx				; bump size of fixup entry
	inc	esi				; bump offset of function number
sif4:
	cmp	BYTE PTR IMPDEFModuleNumber+2,0
	je	sif5
	inc	al				; three bytes
	inc	ecx				; bump size of fixup entry
	inc	esi				; bump offset of function number
sif5:
	cmp	BYTE PTR IMPDEFModuleNumber+3,0
	je	sif6
	inc	al				; four bytes
	inc	ecx				; bump size of fixup entry
	inc	esi				; bump offset of function number

; al==0,1,2, or 3 depending on module number size
sif6:
	inc	al				; make relative 1
	shl	eax,30			; put module number flag bytes in proper area
	xor	ebx,ebx
	cmp	BYTE PTR IMPDEFFunctionNumber+1,0
	je	sif7
	inc	bl				; length at least 2 bytes
	inc	ecx				; bump size of fixup entry
sif7:
	cmp	BYTE PTR IMPDEFFunctionNumber+2,0
	je	sif8
	inc	bl				; three bytes
	inc	ecx				; bump size of fixup entry
sif8:
	cmp	BYTE PTR IMPDEFFunctionNumber+3,0
	je	sif9
	inc	bl				; four bytes
	inc	ecx				; bump size of fixup entry

; bl==0,1,2, or 3 depending on function number size
sif9:
	inc	bl				; make relative 1
	shl	ebx,28			; put function number flag bytes in proper area
	or	eax,ebx			; merge into module number
	or	eax,IMPDEFFixupOffset
	mov	gs:[edi],eax
	add	edi,4			; position to module name number field
	mov	eax,IMPDEFModuleNumber
	mov	gs:[edi],eax	; put in 4 byte value, though up to 3 may be overwritten
	mov	eax,IMPDEFFunctionNumber
	mov	gs:[edi+esi],eax	; place funciton number number value, 4 byte, though up to 3 unwritten

	add	TotalIMPDEFFixupSize,ecx	; bump total fixup size by amount to write
	inc	TotalIMPDEFFixupCount	; bump count of impdef fixups
	pop	ecx				; restore critical registers
	pop	esi
	pop	fs
	ret

; bad or mismatching frame for impdef
poor24:
	mov	cl,24
	call	BadOBJModuleExit

SaveIMPDEFFixup	ENDP

ENDIF

ENDS

END
