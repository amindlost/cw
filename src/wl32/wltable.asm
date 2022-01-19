;*********************************************************************
;*   WLTABLE.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          07/31/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   internal linker table routines                                  *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLTABLE
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLDATA.INC
INCLUDE WLERRCOD.INC

;*****************************
;* Public declarations       *
;*****************************

; procedures
PUBLIC	AssignSegToGroup
PUBLIC	ConvertToUpperCase
PUBLIC	GetComDatEntry
PUBLIC	GetGrpDefEntry
PUBLIC	GetMasterSegDefEntry
PUBLIC	GetPubSymEntry
PUBLIC	IsAny32BitSegFlag
PUBLIC	MakeIndSegDefEntry
PUBLIC	MakeSourceLineBlk
PUBLIC	NormalDSSISource
PUBLIC	NormalESDIDest
PUBLIC	NormalGSBXSource
PUBLIC	SetGrpPtrTableEntry
PUBLIC	SetSymPtrTableEntry

IFDEF	DLLSUPPORT
PUBLIC	GetEXPDEFEntry
PUBLIC	GetIMPDEFEntry
ENDIF

; variables
PUBLIC	FirstComDatBlkPtr
PUBLIC	FirstGrpDefBlkPtr
PUBLIC	FirstMasterSegDefBlkPtr
PUBLIC	FirstPubSymBlkPtr
PUBLIC	GrpDefNamePtr
PUBLIC	LastGrpDefBlkPtr
PUBLIC	RelocEntryCount
PUBLIC	TotalGetPubCount
PUBLIC	TotalRelSegCount

IFDEF DLLSUPPORT
PUBLIC	FirstEXPDEFBlkPtr,FirstIMPDEFBlkPtr
ENDIF

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

PubSymChainFlag	DB	?	; nonzero if public symbol entries need chaining, ==1 if new>parent, ==2 if parent>new
SegDefChainFlag	DB	?	; nonzero if segdef entries need chaining, ==1 if new>parent, ==2 if parent>new

GrpDefNamePtr	DD	?	; pointer to current grpdef name
PrevLastIndSegPtr	DD	?	; previously last individual segdef pointer for master segdef
PubSymNamePtr	DD	?	; pointer to current public symbol name
SegDefClassNamePtr	DD	?	; pointer to current segdef class name
SegDefSegNamePtr	DD	?	; pointer to current segdef segment name

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

;DEBSYMText	DB	6,'DEBSYM'
;DEBTYPText	DB	6,'DEBTYP'
;SYMBOLSText	DB	9,'$$SYMBOLS'
;TYPESText	DB	7,'$$TYPES'

IFDEF CLARION
GDESCText	DB	5,'GDESC'
GDATAText	DB	5,'GDATA'
ENDIF

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

IsAny32BitSegFlag	DB	0	; nonzero if any 32-bit segments

TotalAbsSegCount	DW	0	; total count of absolute segments in program
TotalRelSegCount	DW	0	; total count of relative (nonabsolute) segments
TotalSegCount	DW	0	; total count of segments in program

FirstComDatBlkPtr	DW	0	; first allocated comdat block pointer
FirstGrpDefBlkPtr	DW	0	; first allocated grpdef block pointer
FirstGrpPtrTableBlkPtr	DW	0	; first allocated group pointer table block pointer
FirstIndSegDefBlkPtr	DW	0	; first allocated individual segdef block pointer
FirstMasterSegDefBlkPtr	DW	0	; first allocated master segdef block pointer
FirstPubSymBlkPtr	DW	0	; first allocated public symbol block pointer
FirstSymPtrTableBlkPtr	DW	0	; first allocated symbol pointer table block pointer
LastComDatBlkPtr	DW	0	; last allocated comdat block pointer
LastGrpDefBlkPtr	DW	0	; last allocated grpdef block pointer
LastGrpPtrTableBlkPtr	DW	0	; last allocated group pointer table block pointerr
LastIndSegDefBlkPtr		DW	0	; last allocated individual segdef block pointer
LastMasterSegDefBlkPtr	DW	0	; last allocated master segdef block pointer
LastPubSymBlkPtr	DW	0	; last allocated public symbol block pointer
LastSymPtrTableBlkPtr	DW	0	; last allocated symbol pointer table block pointer

AbsSegDefPtr	DD	0	; pointer to absolute master segdef entries
RelocEntryCount	DD	0	; count of relocation entries

SegDefHashTable	DD	256 DUP (0)	; hash table of pointers to master segdef entries based on segment name
SymbolHashTable	DD	256 DUP (0)	; hash table of pointers to symbol entries based on symbol name

TotalGetPubCount	DD	0

IFDEF	DLLSUPPORT
FirstEXPDEFBlkPtr	DW	0	; first allocated expdef block pointer
FirstIMPDEFBlkPtr	DW	0	; first allocated impdef block pointer
LastEXPDEFBlkPtr	DW	0	; last allocated expdef block pointer
LastIMPDEFBlkPtr	DW	0	; last allocated impdef block pointer
ENDIF

IFDEF CLARION
GDESCFlag	DB	0		; nonzero if GDESC class segment encountered
GDATAFlag	DB	0		; nonzero if GDATA class segment encountered
ENDIF

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	ACBPByte:BYTE,Is32BitSeg:BYTE,IsAbsoluteSeg:BYTE
EXTRN	CompBuffSource:BYTE,CompBuffDest:BYTE
EXTRN	CurrentBaseOBJBuff:WORD
EXTRN	CurrentSegDefCount:WORD,CurrentGrpDefCount:WORD,CurrentFixSymCount:WORD
EXTRN	FlatModuleFlag:BYTE
EXTRN	GroupNameIndex:WORD,GrpDefSegmentIndex:WORD
EXTRN	IsLocalSymbol:BYTE
EXTRN	LineSegmentID:WORD
EXTRN	LNAMESIndexSel:WORD
EXTRN	ModuleCount:DWORD
EXTRN	SearchExistSymFlag:BYTE
EXTRN	SegFrameNumber:WORD
EXTRN	SegmentNameIndex:WORD,ClassNameIndex:WORD
EXTRN	SegmentLength:DWORD

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
EXTRN	LinkerErrorExit:PROC
EXTRN	NormalizeErrorExit:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* GETMASTERSEGDEFENTRY      *
;*****************************

; look up master segment definition entry, create if none exist
; upon entry segdef information variables are set
;  (use SegmentNameIndex and ClassNameIndex)
; return -> master segdef entry in gs:di
; maintains ds,es,fs, destroy all other registers at will

GetMasterSegDefEntry	PROC
	push	fs			; save critical registers
	push	es

	mov	SegDefChainFlag,OFF	; init chaining flag
	cmp	IsAbsoluteSeg,OFF	; see if absolute segment
	jne	gmabs			; yes

	mov	gs,LNAMESIndexSel	; gs -> LNAMES table of pointers block
	mov	si,ClassNameIndex
	dec	si				; make relative zero
	shl	si,2			; convert to dword offset
	mov	eax,gs:[si]		; eax -> current class name
	mov	SegDefClassNamePtr,eax	; save it

	mov	di,SegmentNameIndex
	dec	di				; make relative zero
	shl	di,2			; convert to dword offset
	lfs	bx,gs:[di]		; fs:bx -> name to get hashcode of, from dword entries table
	mov	WORD PTR SegDefSegNamePtr,bx	; save pointer to name
	mov	WORD PTR SegDefSegNamePtr+2,fs
	call	GetHashCode	; hash code returned in bx
	shl	bx,2			; convert to dword offset
	mov	ax,ds:[bx+WORD PTR SegDefHashTable+2]	; get hash code selector
	or	ax,ax			; see if used
	je	gmnouse			; no

; hash code used, check for collision or duplicate master segdef entry
	lfs	bp,ds:[bx+SegDefHashTable]	; fs:bp -> master segdef entry of first segdef with same hash code
	lgs	bx,gs:[di]		; gs:bx -> current segment name, not normalized
	cmp	bx,SIZEIOBUFFBLK-MAXOBJRECNAME	; check for possible overflow, normalize name if so
	jae	gmgsbx			; possible normalization needed

; fs:bp -> master segdef entry on record
; gs:bx -> current segment name
gmmainloop:
	les	di,fs:[bp+MasterSegDefRecStruc.mssNamePtr]	; es:di -> segment name on record, not normalized
	cmp	di,SIZEIOBUFFBLK-MAXOBJRECNAME	; check for possible name buffer overflow
	jae	gmesdi			; possible, normalize name

gmdssi:
	push	gs
	pop	ds
	mov	si,bx			; ds:si -> current segment name
	cmpsb				; see if length byte matches
	je	gmlenok			; yes

; segment names did not match, try another
	jc	gmoldgr			; old name greater than new name

; new name greater than old name
gmnewgr:
	xor	dl,dl			; zero dl to flag new > old
	cmp	WORD PTR fs:[bp+MasterSegDefRecStruc.mssHigherNamePtr+2],0	; see if next link
	je	gmnewseg		; no more names, this is a new segment
	lfs	bp,fs:[bp+MasterSegDefRecStruc.mssHigherNamePtr]	; fs:bp -> master segdef entry of higher record name
	jmp	gmmainloop	; try next

; normalize gs:bx source name for compare
gmgsbx:
	call	NormalGSBXSource	; normalize gs:bx to name source buffer
	jmp	gmmainloop

; normalize es:di destination name for compare
gmesdi:
	call	NormalESDIDest	; normalize es:di to destination name buffer
	jmp	gmdssi

; old name greater than new name
gmoldgr:
	mov	dl,1			; set dl==1 to flag old>new
	cmp	WORD PTR fs:[bp+MasterSegDefRecStruc.mssLowerNamePtr+2],0	; see if next link
	je	gmnewseg		; no more names, this is a new segment
	lfs	bp,fs:[bp+MasterSegDefRecStruc.mssLowerNamePtr]	; fs:bp -> master segdef entry of lower record name
	jmp	gmmainloop	; try next

gmlenok:
	mov	cl,ds:[si-1]
	xor	ch,ch			; get # of bytes to check
	mov	ax,cx			; save count of bytes
	and	cx,1			; get odd byte
	jcxz	gm2			; no odd byte
	cmpsb
	jc	gmoldgr			; old name greater than new name
	jne	gmnewgr			; new name greater than old name
gm2:
	mov	cx,ax			; restore bytes to check
	shr	cx,1			; get words to check, odd byte accounted for
	mov	ax,cx			; save words to check
	and	cx,1			; get odd word
	jcxz	gm3			; no odd word
	cmpsw
	jc	gmoldgr			; old name greater than new name
	jne	gmnewgr			; new name greater than old name
gm3:
	mov	cx,ax			; restore words to check
	shr	cx,1			; get dwords to check, odd word accounted for
	jcxz	gmsmatch
;@@@	movzx	ecx,cx		; convert count to 32-bit for cmpsd
	repe	cmpsd
	jc	gmoldgr			; old name greater than new name
	jne	gmnewgr			; new name greater than old name

; segment names match, check class names
; fs:bp -> master segdef entry on record
gmsmatch:
	push	DGROUP
	pop	ds				; ds -> wl32 data
	lds	si,SegDefClassNamePtr	; ds:si -> current class name, not normalized
	cmp	si,SIZEIOBUFFBLK-MAXOBJRECNAME	; see if normalization might be needed
	jae	gmnorms			; yes

gmclrec:
	les	di,fs:[bp+MasterSegDefRecStruc.mssClassPtr]	; es:di -> class name on record, not normalized
	cmp	di,SIZEIOBUFFBLK-MAXOBJRECNAME	; see if normalization might be needed
	jae	gmnormd			; yes

; ds:si -> current class name, es:di -> class name on record
gmlencmp:
	cmpsb				; see if length byte matches
	jne	gmcno			; no

	mov	cl,ds:[si-1]
	xor	ch,ch			; get # of bytes to check
	mov	ax,cx			; save count of bytes
	and	cx,1			; get odd byte
	jcxz	gmc2		; no odd byte
	cmpsb
	jne	gmcno			; no match
gmc2:
	mov	cx,ax			; restore bytes to check
	shr	cx,1			; get words to check, odd byte accounted for
	mov	ax,cx			; save words to check
	and	cx,1			; get odd word
	jcxz	gmc3		; no odd word
	cmpsw
	jne	gmcno			; no match
gmc3:
	mov	cx,ax			; restore words to check
	shr	cx,1			; get dwords to check, odd word accounted for
	jcxz	gmcmatch
;@@@	movzx	ecx,cx		; convert count to 32-bit for cmpsd
	repe	cmpsd
	jne	gmcno			; no match

; class names match, segment names match, not an absolute segment, return pointer to entry
; fs:bp -> segdef entry on record
gmcmatch:
	push	DGROUP
	pop	ds				; ds -> wl32 data

; see if one or the other segments is private, if so fail match
	mov	al,ACBPByte
	and	al,CFIELDOFACBP	; get combine field of current segment
	je	gmcno			; private, cannot match
	mov	al,fs:[bp+MasterSegDefRecStruc.mssACBPByte]
	and	al,CFIELDOFACBP	; get combine field of segment on record
	je	gmcno			; private, cannot match

	mov	di,bp
	push	fs
	pop	gs				; gs:di -> matching entry
	jmp	gmret	; done

; normalize source name in ds:si
gmnorms:
	call	NormalDSSISource	; normalize ds:si to name source buffer
	jmp	gmclrec

; normalize destination name in es:di
gmnormd:
	call	NormalESDIDest	; normalize es:di to destination name buffer
	jmp	gmlencmp

; no match on class names or private, check next segdef entry name, use higher pointer
gmcno:
	push	DGROUP
	pop	ds				; ds -> wl32 data
	mov	gs,LNAMESIndexSel	; gs -> LNAMES table of pointers block
	mov	di,SegmentNameIndex
	dec	di				; make relative zero
	shl	di,2			; convert to dword offset
	lgs	bx,gs:[di]		; gs:bx -> segment name, not normalized
	cmp	bx,SIZEIOBUFFBLK-MAXOBJRECNAME	; check normalization
	jae	gmnorms2

gmhimast:
	xor	dl,dl			; zero dl to flag new > old
	cmp	WORD PTR fs:[bp+MasterSegDefRecStruc.mssHigherNamePtr+2],0	; see if next link
	je	gmnewseg		; no more names, this is a new segment
	lfs	bp,fs:[bp+MasterSegDefRecStruc.mssHigherNamePtr]	; fs:bp -> master segdef entry of higher record name, not normalized
	cmp	bp,SIZEIOBUFFBLK-MAXOBJRECNAME		; check normalization
	jb	gmmainloop		; not needed

; normalize destination name buffer for fs:bp
	call	NormalFSBPDest	; normalize fs:bp to name destination buffer
	jmp	gmmainloop

; normalize source name in gs:bx
gmnorms2:
	call	NormalGSBXSource	; normalize gs:bx to name source buffer
	jmp	gmhimast

; new segment, fs:bp -> parent; dl==0 if new name > parent, ==1 if parent > new
gmnewseg:
	push	DGROUP
	pop	ds				; ds -> wl32 data
	mov	bx,fs
	mov	si,bp			; save old fs:bp pointer in bx:si
	inc	dx				; dl==1 if new>parent, ==2 if parent>new
	mov	SegDefChainFlag,dl	; save flag to indicate chaining needed

; check if any segdef allocations made
gmnouse:
	cmp	LastMasterSegDefBlkPtr,0	; zero if no previous allocations
	je	gmalloc			; no previous allocations, block must be allocated

; check if allocated master segdef block is full
	mov	gs,LastMasterSegDefBlkPtr	; gs -> last allocated block
	cmp	gs:[MasterSegDefBlkStruc.msdbCount],MAXCOUNTMASTERSEGDEFBLK	; see if any free entries in block
	jb	gminit			; free entry exists

; make a new block
gmalloc:
	call	MakeMasterSegDefBlk	; allocate master segdef block

; initialize master segdef entry in block
gminit:
	mov	ax,gs:[MasterSegDefBlkStruc.msdbCount]	; ax holds current count
	inc	gs:[MasterSegDefBlkStruc.msdbCount]	; bump count in block
	inc	TotalSegCount	; bump total count of discrete segments
	mov	dx,SIZE MasterSegDefRecStruc	; bytes/structure
	mul	dx				; ax hold offset to entry, not counting sysvars
	add	ax,MASTERSEGDEFSYSVARSIZE	; adjust for sysvars, gs:ax -> entry slot
	mov	di,ax			; gs:di -> master segdef entry

; perform initializations
	mov	gs:[di+MasterSegDefRecStruc.mssFlags],0
	mov	al,ACBPByte
	mov	gs:[di+MasterSegDefRecStruc.mssACBPByte],al
	cmp	Is32BitSeg,OFF	; see if 32-bit segment
	jne	gm32			; yes

gmchkstk:
	and	al,CFIELDOFACBP	; get combine field
	cmp	al,STACKSEGMENT	; see if stack combine
	jne	gmnotstack		; no
	or	gs:[di+MasterSegDefRecStruc.mssFlags],STACKSEGMENTFLAG
	jmp	gmzero

; flag a 32-bit segment
gm32:
	mov	IsAny32BitSegFlag,ON	; flag 32-bit segment exists
	or	gs:[di+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG
	cmp	IsCreateEXEOption,OFF	; see if creating DOS EXE file
	je	gmchkstk			; no

; can't have a 32-bit segment with DOS EXE file
	lgs	bx,SegDefSegNamePtr	; gs:bx -> nonnormalized text string
	mov	al,SEG32BITEXEERRORCODE	; flag 32-bit segment with EXE file
	call	NormalizeErrorExit	; normalize text string, do linker error exit
	jmp	gmchkstk

gmnotstack:
	cmp	al,COMMONSEGMENT	; see if common combine
	jne	gmzero			; no
	or	gs:[di+MasterSegDefRecStruc.mssFlags],COMMONSEGMENTFLAG

gmzero:	
	cmp	IsAbsoluteSeg,OFF	; see if absolute segment
	jne	gmsegname			; yes

;@@@	mov	ax,TotalRelSegCount	; get count of nonabsolute segments, relative 0
;@@@	mov	gs:[di+MasterSegDefRecStruc.SegmentID],ax	; init segment id
	inc	TotalRelSegCount	; increment total relative (nonabsolute) segment count

gmsegname:
	mov	eax,SegDefSegNamePtr	; eax -> segdef name
	mov	gs:[di+MasterSegDefRecStruc.mssNamePtr],eax	; init segment name
	mov	eax,SegDefClassNamePtr	; eax -> class name
	mov	gs:[di+MasterSegDefRecStruc.mssClassPtr],eax	; init class name

	xor	eax,eax
	mov	gs:[di+MasterSegDefRecStruc.mssSegLength],eax	; init segment length
	mov	gs:[di+MasterSegDefRecStruc.mssGroupPtr],eax	; must init all of this pointer
	mov	gs:[di+WORD PTR MasterSegDefRecStruc.mssNextSegPtr+2],ax	; init pointer selectors
	mov	gs:[di+WORD PTR MasterSegDefRecStruc.mssFirstIndSegPtr+2],ax
	mov	gs:[di+WORD PTR MasterSegDefRecStruc.mssLastIndSegPtr+2],ax
	mov	gs:[di+WORD PTR MasterSegDefRecStruc.mssHigherNamePtr+2],ax	; init name pointer
	mov	gs:[di+WORD PTR MasterSegDefRecStruc.mssLowerNamePtr+2],ax

	cmp	SegDefChainFlag,OFF	; see if chaining segdef entries from previous
	je	gmabschk		; no

; must chain segdef entry from previous/parent/old entry
; parent pointer in bx:si, SegDefChainFlag value determines hi/low selection
; gs:di -> new master segdef entry
	mov	fs,bx			; fs:si -> old entry
	cmp	SegDefChainFlag,1	; see if new name is greater than parent
	jne	gmlower			; no
	mov	fs:[si+WORD PTR MasterSegDefRecStruc.mssHigherNamePtr+2],gs
	mov	fs:[si+WORD PTR MasterSegDefRecStruc.mssHigherNamePtr],di
	jmp	gmret

; parent (old) name is greater than new name
gmlower:
	mov	fs:[si+WORD PTR MasterSegDefRecStruc.mssLowerNamePtr+2],gs
	mov	fs:[si+WORD PTR MasterSegDefRecStruc.mssLowerNamePtr],di

gmret:
	pop	es				; restore critical registers
	pop	fs
	ret

gmabschk:
	cmp	IsAbsoluteSeg,OFF	; see if absolute segment
	jne	gmabs2			; yes

; not an absolute segment, new hash code, save address
	mov	ds:[bx+WORD PTR SegDefHashTable],di	; save hash code address
	mov	ds:[bx+WORD PTR SegDefHashTable+2],gs
	jmp	gmret		; one

; absolute segment, check for duplicate
gmabs:
	mov	ax,WORD PTR AbsSegDefPtr+2
	or	ax,ax			; see if any previous absolute segments
	je	gmnouse			; no, new entry by default

	mov	gs,ax
	mov	di,WORD PTR AbsSegDefPtr	; gs:di -> first absolute master segdef
	mov	ax,SegFrameNumber	; get new absolute segment's frame number

gmabsloop:
	cmp	gs:[di+WORD PTR MasterSegDefRecStruc.mssSegOffset],ax	; see if frame numbers match
	je	gmret			; yes, gs:di -> master segdef entry

; use higher name pointer to chain to next absolute segment entry
	mov	bx,gs
	mov	si,di			; save old gs:di pointer in bx:si
	cmp	gs:[di+WORD PTR MasterSegDefRecStruc.mssHigherNamePtr+2],0	; see if next link in chain exists
	je	gmnouse			; no further links, unique absolute segment, bx:si -> last link
	lgs	di,gs:[di+MasterSegDefRecStruc.mssHigherNamePtr]	; gs:di -> next link in chain
	jmp	gmabsloop

; end processing of absolute segment, ax known zero
gmabs2:
	inc	TotalAbsSegCount	; bump total count of discrete absolute segments
	or	gs:[di+MasterSegDefRecStruc.mssFlags],ABSOLUTESEGMENTFLAG
	mov	ax,SegFrameNumber	; get new absolute segment's frame number
	movzx	eax,ax
	mov	gs:[di+MasterSegDefRecStruc.mssSegOffset],eax	; set frame number
	cmp	WORD PTR AbsSegDefPtr+2,0	; see if first absolute segment pointer
	je	gmfirstabs			; yes

; not the first absolute segment, add chain link in previous entry (bx:si)
	mov	fs,bx			; fs:si -> old entry
	mov	fs:[si+WORD PTR MasterSegDefRecStruc.mssHigherNamePtr],di
	mov	fs:[si+WORD PTR MasterSegDefRecStruc.mssHigherNamePtr+2],gs
	jmp	gmret

gmfirstabs:
	mov	WORD PTR AbsSegDefPtr,di	; save -> first pointer
	mov	WORD PTR AbsSegDefPtr+2,gs
	jmp	gmret

GetMasterSegDefEntry	ENDP

;*****************************
;* MAKEMASTERSEGDEFBLK       *
;*****************************

; make a master segdef block
; returns gs -> block,
;   updates LastMasterSegDefBlkPtr, FirstMasterSegDefBlkPtr variables
; destroys eax,dx

MakeMasterSegDefBlk	PROC
	mov	dx,SIZEMASTERSEGDEFBLK	; get number of bytes to allocate for master segdef block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastMasterSegDefBlkPtr	; keep -> previously last allocate block, if any
	mov	LastMasterSegDefBlkPtr,ax	; update last allocated block pointer
	mov	gs,ax			; gs -> block
	xor	eax,eax
	mov	gs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstMasterSegDefBlkPtr,ax	; see if first block allocated yet
	je	mmsfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,gs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	gs,ax			; gs -> previous block
	mov	gs:[MasterSegDefBlkStruc.msdbNextPtr],dx
	mov	gs,dx			; gs -> current block
	ret

mmsfirst:
	mov	ax,LastMasterSegDefBlkPtr
	mov	FirstMasterSegDefBlkPtr,ax	; update first allocated block pointer
	ret
MakeMasterSegDefBlk	ENDP

;*****************************
;* MAKEINDSEGDEFENTRY        *
;*****************************

; make individual segdef record entry
; upon entry gs:di -> master segdef entry

MakeIndSegDefEntry	PROC
	push	fs			; save critical registers
	push	gs
	push	di

	cmp	LastIndSegDefBlkPtr,0	; zero if no previous allocations
	je	mialloc			; no previous allocations, block must be allocated

; check if allocated individual segdef block is full
	mov	gs,LastIndSegDefBlkPtr	; gs -> last allocated block
	cmp	gs:[IndSegDefBlkStruc.msdbCount],MAXCOUNTINDSEGDEFBLK	; see if any free entries in block
	jb	miinit			; free entry exists

; make a new block
mialloc:
	call	MakeIndSegDefBlk	; allocate individual segdef block

; initialize individual segdef entry in block
miinit:
	mov	ax,gs:[IndSegDefBlkStruc.isdbCount]	; ax holds current count
	inc	gs:[IndSegDefBlkStruc.isdbCount]	; bump count in block
	mov	dx,SIZE IndSegDefRecStruc	; bytes/structure
	mul	dx				; ax hold offset to entry, not counting sysvars
	add	ax,INDSEGDEFSYSVARSIZE	; adjust for sysvars, gs:ax -> entry slot
	mov	di,ax			; gs:di -> individual segdef entry

; check if first individual segdef allocation for object module
; if so, keep a pointer to it in the first (base) i/o buffer
	cmp	CurrentSegDefCount,0	; see if current segdef is first
	jne	mi2				; no
	cmp	ModuleCount,-1	; see if not a module (created in resolution phase)
	je	mi2				; yes, bypass pointer in i/o buffer setup

	mov	fs,CurrentBaseOBJBuff	; fs -> first OBJ buffer
	mov	fs:[WORD PTR IOBuffHeaderStruc.ibhsSegDefPtr],di
	mov	fs:[WORD PTR IOBuffHeaderStruc.ibhsSegDefPtr+2],gs

mi2:
	pop	bx
	pop	fs				; fs:bx -> master segdef entry
	push	fs			; restore to stack
	push	bx
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssLastIndSegPtr]
	mov	PrevLastIndSegPtr,eax	; keep pointer to previously last individual segdef, if any, for chain update
	mov	fs:[bx+WORD PTR MasterSegDefRecStruc.mssLastIndSegPtr],di	; update master last individual segdef pointer
	mov	fs:[bx+WORD PTR MasterSegDefRecStruc.mssLastIndSegPtr+2],gs
	mov	ah,ACBPByte
	and	ah,AFIELDOFACBP	; get alignment field of individual segdef
	mov	al,fs:[bx+MasterSegDefRecStruc.mssACBPByte]	; get master ACBP byte
	and	al,AFIELDOFACBP	; get alignment field of master segdef

; ah holds align field of individual segdef
; al holds align field of master segdef

; KILL 3P forced para alignment, MED 02/23/96
;; if creating 3P file and ah<PARAALIGN or ah==DWORDALIGN then
;;   change ah to PARAALIGN
	cmp	IsCreateEXEOption,OFF	; see if creating EXE file
	jne	mialignchk		; yes

; if Clarion, see if GDESC or GDATA class segment
; if so, set Clarion global data flag
;   if not first, then don't modify alignment
IFDEF CLARION
	push	ax
	push	bx
	push	fs

	xor	ecx,ecx
	lfs	bx,fs:[bx+MasterSegDefRecStruc.mssClassPtr]	; fs:bx -> class name

michkgdesc:
	cmp	bx,SIZEIOBUFFBLK	; see if bx is at wrap point
	jb	mi4				; no
	mov	bx,IOBUFFSYSVARSIZE
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; fs -> next block in chain

mi4:
	mov	al,ds:[GDESCText+ecx]
	cmp	al,fs:[bx]		; see if name matches
	jne	migdatasetup	; no
	inc	bx
	inc	ecx
	cmp	ecx,6			; see if checked all chars in name
	jne	michkgdesc		; not yet

; is a GDESC class segment
	pop	fs
	pop	bx				; fs:bx -> master segment
	push	bx			; restore to stack
	push	fs
	or	fs:[bx+MasterSegDefRecStruc.mssFlags],CLARIONGLOBALDATAFLAG
	cmp	GDESCFlag,1		; see if already segment of this class
	mov	GDESCFLag,1		; set flag, KEEP FLAG REGISTER STATUS
	jmp	michkdone	; Z flag indicates whether to do alignment adjustment

migdatasetup:
	pop	fs
	pop	bx				; fs:bx -> master segment
	push	bx			; restore to stack
	push	fs
	xor	ecx,ecx
	lfs	bx,fs:[bx+MasterSegDefRecStruc.mssClassPtr]	; fs:bx -> class name

michkgdata:
	cmp	bx,SIZEIOBUFFBLK	; see if bx is at wrap point
	jb	mi5				; no
	mov	bx,IOBUFFSYSVARSIZE
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; fs -> next block in chain

mi5:
	mov	al,ds:[GDATAText+ecx]
	cmp	al,fs:[bx]		; see if name matches
	jne	michkdone		; no
	inc	bx
	inc	ecx
	cmp	ecx,6			; see if checked all chars in name
	jne	michkgdata		; not yet

; is a GDATA class segment
	pop	fs
	pop	bx				; fs:bx -> master segment
	push	bx			; restore to stack
	push	fs
	or	fs:[bx+MasterSegDefRecStruc.mssFlags],CLARIONGLOBALDATAFLAG
	cmp	GDATAFlag,1		; see if already segment of this class
	mov	GDATAFlag,1		; set flag, KEEP FLAG REGISTER STATUS

michkdone:
	pop	fs
	pop	bx
	pop	ax
	je	mialignchk		; matching GDESC or GDATA alignment, not first, bypass alignment adjustment
ENDIF

IFNDEF WATCOM_ASM
; re-force Clarion programs back to paragraph alignment
; MED 07/19/96
; Clipper too, 07/31/96
michkpara:
	cmp	ah,PARAALIGNSEGMENT	; see if individual segdef is less than para align
	jb	miparaalign		; yes, change to para align
	cmp	ah,DWORDALIGNSEGMENT	; see if individual segdef < para align
	jne	mialignchk		; no

miparaalign:
	mov	ah,PARAALIGNSEGMENT
ENDIF

; if ah>al and ah<=PAGEALIGN then change ACBP
mialignchk:
	cmp	ah,al			; see if individual > master
	jbe	mialign2		; no
	cmp	ah,PAGEALIGNSEGMENT	; see if individual <= para align
	jbe	minewalign		; yes

;  else if ah==DWORDALIGN and al<=WORDALIGN then change ACBP
mialign2:
	cmp	ah,DWORDALIGNSEGMENT	; see if individual==dword align
	jne	mialign3		; no
	cmp	al,WORDALIGNSEGMENT	; see if master <= word align
	jbe	minewalign		; yes

;  else if al==DWORDALIGN and (ah==PARAALIGN or ah==PAGEALIGN) then change ACBP
mialign3:
	cmp	al,DWORDALIGNSEGMENT	; see if master== dword align
	jne	miinit2			; no
	cmp	ah,PARAALIGNSEGMENT	; see if individual== para align
	je	minewalign		; yes
	cmp	ah,PAGEALIGNSEGMENT	; see if individual==page align
	jne	miinit2			; no

; update master segdef with new alignment
minewalign:
	mov	al,fs:[bx+MasterSegDefRecStruc.mssACBPByte]	; get master ACBP byte
	and	al,NOT AFIELDOFACBP	; mask off alignment field of master segdef
	or	al,ah			; merge in new alignment field
	mov	fs:[bx+MasterSegDefRecStruc.mssACBPByte],al	; save to master segdef
	
; do various initializations
miinit2:
	mov	gs:[di+WORD PTR IndSegDefRecStruc.isdrMasterPtr],bx	; update master back pointer
	mov	gs:[di+WORD PTR IndSegDefRecStruc.isdrMasterPtr+2],fs
	mov	ax,CurrentBaseOBJBuff
	mov	WORD PTR gs:[di+IndSegDefRecStruc.isdrModulePtr],ax
	mov	al,ACBPByte
	mov	gs:[di+IndSegDefRecStruc.isdrACBPByte],al
	mov	eax,SegmentLength
	mov	gs:[di+IndSegDefRecStruc.isdrSegLength],eax
	xor	eax,eax
	mov	gs:[di+IndSegDefRecStruc.isdrNextIndSegPtr],eax
	mov	gs:[di+IndSegDefRecStruc.isdrFlags],ax

	cmp	IsAbsoluteSeg,OFF	; see if absolute segment
	jne	miabs			; yes

; test if stack or common segment, set flags if so
; precedence of combine is stack > public > common when conflicting combines
; fs:bx -> master segdef entry
	mov	ax,fs:[bx+MasterSegDefRecStruc.mssFlags]
	test	ax,STACKSEGMENTFLAG
	jne	mistack
	mov	dl,ACBPByte
	mov	dh,dl
	and	dl,CFIELDOFACBP	; combine field
	cmp	dl,STACKSEGMENT
	jne	mipubchk		; neither segment is stack align, check if public

; one of combine types is stack, make both stack, clearing common segment flag if existent
mistack:
	mov	ax,NOT COMMONSEGMENTFLAG
	and	gs:[di+IndSegDefRecStruc.isdrFlags],ax
	and	fs:[bx+MasterSegDefRecStruc.mssFlags],ax
	mov	ax,STACKSEGMENTFLAG
	or	gs:[di+IndSegDefRecStruc.isdrFlags],ax
	or	fs:[bx+MasterSegDefRecStruc.mssFlags],ax
	xor	ax,ax			; no segment length adjustment
	jmp	mioff

; check if public type (check not common)
; ax holds flags of master, dl holds combine field of individual segment
mipubchk:
	test	ax,COMMONSEGMENTFLAG
	je	mipub			; not a common combine
	cmp	dl,COMMONSEGMENT
	jne	mipub			; not common combine

; both segments are common combine
	mov	ax,COMMONSEGMENTFLAG
	or	gs:[di+IndSegDefRecStruc.isdrFlags],ax
	or	fs:[bx+MasterSegDefRecStruc.mssFlags],ax

; update offset of segment (always zero for common segment)
	mov	gs:[di+IndSegDefRecStruc.isdrSegOffset],0

; update length of common segment if individual length > master length
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegLength]	; get master segment length
	cmp	eax,SegmentLength
	jae	mi3				; no update needed
	mov	eax,SegmentLength
	mov	fs:[bx+MasterSegDefRecStruc.mssSegLength],eax	; update master segment length
	jmp	mi3

; poorly formed object record
poor5:
	mov	cl,5
	call	BadOBJModuleExit

; segments are public combine, shut off common flags
; compute segment length adjustment
; fs:bx -> master segdef entry
; dh holds ACBP byte of individual segment
mipub:
	mov	ax,NOT COMMONSEGMENTFLAG
	and	gs:[di+IndSegDefRecStruc.isdrFlags],ax
	and	fs:[bx+MasterSegDefRecStruc.mssFlags],ax

	xor	ax,ax			; init segment length adjustment
	and	dh,AFIELDOFACBP
	cmp	dh,BYTEALIGNSEGMENT	; see if byte aligned segment
	je	mioff			; yes, no adjustment needed
	mov	ax,WORD PTR fs:[bx+MasterSegDefRecStruc.mssSegLength]	; get segment length low word
	xor	ah,ah
	sub	ah,al			; compute segment adjustment byte to 256 roundup
	mov	al,ah
	xor	ah,ah			; ax holds segment adjust value

miword:
	cmp	dh,WORDALIGNSEGMENT	; see if word aligned segment
	jne	mipara
	and	al,1			; align to word
	jmp	mioff

mipara:
	cmp	dh,PARAALIGNSEGMENT	; see if paragraph aligned segment
	jne	midword			; no
	and	al,15			; align to para
	jmp	mioff

midword:
	cmp	dh,DWORDALIGNSEGMENT	; see if dword aligned segment
	jne	mipage			; no
	and	al,3			; align to dword
	jmp	mioff

mipage:
	cmp	dh,PAGEALIGNSEGMENT	; see if page alignment
	jne	poor5			; no, poorly formed object module

; set individual segment offset from start of base
; ax holds adjustment amount for total segment length (based on align type)
mioff:
	mov	dx,ax
	movzx	edx,dx
	mov	eax,fs:[bx+MasterSegDefRecStruc.mssSegLength]	; get segment length
	add	eax,edx			; compute new segment length after adjustment
	mov	gs:[di+IndSegDefRecStruc.isdrSegOffset],eax	; save as offset of individual segment
	add	eax,SegmentLength	; compute total segment length
	cmp	eax,65536		; see if >64K segment
	jbe	milenup			; no
	test	fs:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG	; see if 32-bit segment
	jne	milenup			; yes

	cmp	FlatModuleFlag,OFF	; see if flat module
	je	mi16seg			; no
	
; flat module, segment is 32-bit by definition
	or	fs:[bx+MasterSegDefRecStruc.mssFlags],SEGMENT32BITFLAG	; flag 32-bit segment
	jmp	milenup

; segment is 16-bit and >64K, fatal error
; fs:bx -> master segdef record
mi16seg:
	lgs	bx,fs:[bx+MasterSegDefRecStruc.mssNamePtr]	; gs:bx -> nonnormalized text string
	mov	al,SEGLEN64KERRORCODE	; flag segment length >64K
	call	NormalizeErrorExit	; normalize text string, do linker error exit

milenup:
	mov	fs:[bx+MasterSegDefRecStruc.mssSegLength],eax	; update total segment length

mi3:
	cmp	fs:[bx+WORD PTR MasterSegDefRecStruc.mssFirstIndSegPtr+2],0	; see if first individual segdef
	je	mifirst			; yes

; update previously last individual segdef to point to this new one
	push	fs			; save -> master segdef entry
	mov	dx,bx
	lfs	bx,PrevLastIndSegPtr	; fs:bx -> previously last
	mov	fs:[bx+WORD PTR IndSegDefRecStruc.isdrNextIndSegPtr],di	; update next segdef pointer
	mov	fs:[bx+WORD PTR IndSegDefRecStruc.isdrNextIndSegPtr+2],gs
	pop	fs
	mov	bx,dx			; fs:bx -> master segdef entry

miret:
	pop	di				; restore critical registers
	pop	gs
	pop	fs
	ret

; absolute segment, so flag
miabs:
	or	gs:[di+IndSegDefRecStruc.isdrFlags],1
	mov	ax,SegFrameNumber
	movzx	eax,ax
	mov	gs:[di+IndSegDefRecStruc.isdrSegOffset],eax	; save frame number as segment offset
	jmp	mi3

; first individual segment of segdef
mifirst:
	mov	fs:[bx+WORD PTR MasterSegDefRecStruc.mssFirstIndSegPtr],di	; update master first individual segdef pointer
	mov	fs:[bx+WORD PTR MasterSegDefRecStruc.mssFirstIndSegPtr+2],gs
	jmp	miret

MakeIndSegDefEntry	ENDP

;*****************************
;* MAKEINDSEGDEFBLK          *
;*****************************

; make a individual segdef block
; returns gs -> block,
;   updates LastIndSegDefBlkPtr, FirstIndSegDefBlkPtr variables
; destroys eax,dx

MakeIndSegDefBlk	PROC
	mov	dx,SIZEINDSEGDEFBLK	; get number of bytes to allocate for individual segdef block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastIndSegDefBlkPtr	; keep -> previously last allocated block, if any
	mov	LastIndSegDefBlkPtr,ax	; update last allocated block pointer
	mov	gs,ax			; gs -> block
	xor	eax,eax
	mov	gs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstIndSegDefBlkPtr,ax	; see if first block allocated yet
	je	misfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,gs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	gs,ax			; gs -> previous block
	mov	gs:[IndSegDefBlkStruc.isdbNextPtr],dx
	mov	gs,dx			; gs -> current block
	ret

misfirst:
	mov	ax,LastIndSegDefBlkPtr
	mov	FirstIndSegDefBlkPtr,ax	; update first allocated block pointer
	ret
MakeIndSegDefBlk	ENDP

;*****************************
;* GETHASHCODE               *
;*****************************

; get 8-bit hash code (0-255)
; upon entry fs:bx -> name with one-byte length prefix in module buffer
; returns hash code in bx
; destroys ax,cx

GetHashCode	PROC
	push	si			; save critical registers
	push	ds
	push	fs
	pop	ds
	mov	si,bx			; ds:si -> name for faster lods'ing
	cmp	si,SIZEIOBUFFBLK-MAXOBJRECNAME	; see if normalization might be needed
	jae	hashnorm		; yes

hashlen:
	lodsb
	mov	bl,al			; bl holds hash code (length byte+char values, ignore overflow)
	mov	cl,al			; name length to cl
	xor	ax,ax
	xor	bh,bh			; zero high byte of hash code, remains zero through routine
	mov	ch,bh			; zero high byte of name length
	jcxz	hashret		; name length is zero
	shr	cl,1			; convert to words for faster throughput
	jnc	hashloop		; no odd byte
	lodsb				; get odd byte value
	add	bl,al			; add to hash code
	jcxz	hashret		; if only that one char, then exit

hashloop:
	lodsw				; get next two chars in name
	add	bl,al			; add in char values, ignore overflow
	add	bl,ah
	loop	hashloop	; loop through all chars in name, two at a time

hashret:
	pop	ds				; restore critical registers
	pop	si
	ret

; normalize name buffer in ds:si
hashnorm:
	call	NormalDSSISource	; normalize ds:si to name source buffer
	jmp	hashlen

GetHashCode	ENDP

;*****************************
;* NORMALGSBXSOURCE          *
;*****************************

; normalize name (ensure name not split across two buffers)
; upon entry gs:bx -> name length byte, move name into buffer if necessary,
;  if so update gs:bx -> source buffer
; destroy no registers other than gs:bx

NormalGSBXSource	PROC
	push	ax			; save critical registers
	push	cx

	xor	ch,ch
	cmp	bx,SIZEIOBUFFBLK	; bx need normalization on entry
	jb	ngschklen

	mov	bx,IOBUFFSYSVARSIZE	; wrap bx past sysvars
	mov	gs,gs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; gs-> next block in chain
	mov	cl,gs:[bx]		; get length byte
	jmp	ngssplit

ngschklen:
	mov	cl,gs:[bx]		; get length byte
	jcxz	ngsbret		; null length
	mov	ax,bx
	add	ax,cx			; get position of last byte of name
	cmp	ax,SIZEIOBUFFBLK
	jb	ngsbret			; no need to normalize name

; name split across two buffers, move to source buffer
ngssplit:
	push	ds			; save critical registers
	push	es
	push	si
	push	di

	inc	cx				; adjust for length byte in transfer

	mov	si,bx
	push	gs
	pop	ds				; ds:si -> split name source
	mov	ax,DGROUP
	mov	es,ax
	mov	gs,ax
	mov	di,OFFSET DGROUP:CompBuffSource	; es:di -> name storage
	mov	bx,di			; gs:bx -> name source buffer

ngsloop:
	cmp	si,SIZEIOBUFFBLK	; see if si is at wrap point
	jb	ngs2			; nope

	mov	si,IOBUFFSYSVARSIZE	; wrap si past sysvars
	mov	ds,ds:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; ds-> next block in chain

ngs2:
	movsb				; transfer a name char
	loop	ngsloop		; do all chars in name

	pop	di				; restore critical registers
	pop	si
	pop	es
	pop	ds

ngsbret:
	pop	cx				; restore critical registers
	pop	ax
	ret
NormalGSBXSource	ENDP

;*****************************
;* NORMALDSSISOURCE          *
;*****************************

; normalize name (ensure name not split across two buffers)
; upon entry ds:si -> name length byte, move name into buffer if necessary,
;  if so update ds:si -> source buffer
; destroy no registers other than ds:si

NormalDSSISource	PROC
	push	ax			; save critical registers
	push	cx

	xor	ch,ch
	cmp	si,SIZEIOBUFFBLK	; si need normalization on entry
	jb	ndschklen

	mov	si,IOBUFFSYSVARSIZE	; wrap si past sysvars
	mov	ds,ds:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; ds-> next block in chain
	mov	cl,ds:[si]		; get length byte
	jmp	ndssplit

ndschklen:
	mov	cl,ds:[si]		; get length byte
	jcxz	ndsbret		; null length
	mov	ax,si
	add	ax,cx			; get position of last byte of name
	cmp	ax,SIZEIOBUFFBLK
	jb	ndsbret			; no need to normalize name

; name split across two buffers, move to source buffer
ndssplit:
	push	es			; save critical registers
	push	di

	inc	cx				; adjust for length byte in transfer

	push	DGROUP
	pop	es
	mov	di,OFFSET DGROUP:CompBuffSource	; es:di -> name storage

ndsloop:
	cmp	si,SIZEIOBUFFBLK	; see if si is at wrap point
	jb	nds2			; nope

	mov	si,IOBUFFSYSVARSIZE	; wrap si past sysvars
	mov	ds,ds:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; ds-> next block in chain

nds2:
	movsb				; transfer a name char
	loop	ndsloop		; do all chars in name

	push	DGROUP
	pop	ds
	mov	si,OFFSET DGROUP:CompBuffSource	; ds:si -> name storage

	pop	di				; restore critical registers
	pop	es

ndsbret:
	pop	cx				; restore critical registers
	pop	ax
	ret
NormalDSSISource	ENDP

;*****************************
;* NORMALESDIDEST            *
;*****************************

; normalize name (ensure name not split across two buffers)
; upon entry es:di -> name length byte, move name into buffer if necessary,
;  if so update es:di -> destination buffer
; destroy no registers other than es:di

NormalESDIDest	PROC
	push	ax			; save critical registers
	push	cx

	xor	ch,ch
	cmp	di,SIZEIOBUFFBLK	; di need normalization on entry
	jb	neschklen

	mov	di,IOBUFFSYSVARSIZE	; wrap di past sysvars
	mov	es,es:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; ds-> next block in chain
	mov	cl,es:[di]		; get length byte
	jmp	nessplit

neschklen:
	mov	cl,es:[di]		; get length byte
	jcxz	nesbret		; null length
	mov	ax,di
	add	ax,cx			; get position of last byte of name
	cmp	ax,SIZEIOBUFFBLK
	jb	nesbret			; no need to normalize name

; name split across two buffers, move to destination buffer
nessplit:
	push	ds			; save critical registers
	push	si

	inc	cx				; adjust for length byte in transfer

	push	es
	pop	ds
	mov	si,di			; ds:si -> source of name
	push	DGROUP
	pop	es
;@@@	mov	di,OFFSET DGROUP:CompBuffSource	; es:di -> name storage
	mov	di,OFFSET DGROUP:CompBuffDest	; es:di -> name storage

nesloop:
	cmp	si,SIZEIOBUFFBLK	; see if si is at wrap point
	jb	nes2			; nope

	mov	si,IOBUFFSYSVARSIZE	; wrap si past sysvars
	mov	ds,ds:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; ds-> next block in chain

nes2:
	movsb				; transfer a name char
	loop	nesloop		; do all chars in name

	push	DGROUP
	pop	es
;@@@	mov	di,OFFSET DGROUP:CompBuffSource	; es:di -> name storage
	mov	di,OFFSET DGROUP:CompBuffDest	; es:di -> name storage

	pop	si				; restore critical registers
	pop	ds

nesbret:
	pop	cx				; restore critical registers
	pop	ax
	ret
NormalESDIDest	ENDP

;*****************************
;* NORMALFSBPDEST            *
;*****************************

; normalize name (ensure name not split across two buffers)
; upon entry fs:bp -> name length byte, move name into buffer if necessary,
;  if so update fs:bp -> destination buffer
; destroy no registers other than fs:bp

NormalFSBPDest	PROC
	push	ax			; save critical registers
	push	cx

	xor	ch,ch
	cmp	bp,SIZEIOBUFFBLK	; bp need normalization on entry
	jb	nfschklen

	mov	bp,IOBUFFSYSVARSIZE	; wrap bp past sysvars
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; ds-> next block in chain
	mov	cl,fs:[bp]		; get length byte
	jmp	nfssplit

nfschklen:
	mov	cl,fs:[bp]		; get length byte
	jcxz	nfsbret		; null length
	mov	ax,bp
	add	ax,cx			; get position of last byte of name
	cmp	ax,SIZEIOBUFFBLK
	jb	nfsbret			; no need to normalize name

; name split across two buffers, move to source buffer
nfssplit:
	push	ds			; save critical registers
	push	es
	push	si
	push	di

	inc	cx				; adjust for length byte in transfer

	mov	si,bp
	push	fs
	pop	ds				; ds:si -> split name source
	mov	ax,DGROUP
	mov	es,ax
	mov	fs,ax
;@@@	mov	di,OFFSET DGROUP:CompBuffSource	; es:di -> name storage
	mov	di,OFFSET DGROUP:CompBuffDest	; es:di -> name storage
	mov	bp,di			; fs:bp -> name source buffer

nfsloop:
	cmp	si,SIZEIOBUFFBLK	; see if si is at wrap point
	jb	nfs2			; nope

	mov	si,IOBUFFSYSVARSIZE	; wrap si past sysvars
	mov	ds,ds:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; ds-> next block in chain

nfs2:
	movsb				; transfer a name char
	loop	nfsloop		; do all chars in name

	pop	di				; restore critical registers
	pop	si
	pop	es
	pop	ds

nfsbret:
	pop	cx				; restore critical registers
	pop	ax
	ret
NormalFSBPDest	ENDP

;*****************************
;* GETGRPDEFENTRY            *
;*****************************

; get grpdef entry, create if doesn't exist
; upon entry GroupNameIndex holds group name index
; returns grpdef entry in gs:di
; maintain cx,si,fs
; destroys ax,bx,dx,di

GetGrpDefEntry	PROC
	push	fs			; save critical registers
	push	es
	push	si
	push	cx

	mov	fs,LNAMESIndexSel	; fs -> LNAMES table of pointers block
	mov	bx,GroupNameIndex
	dec	bx				; make relative zero
	shl	bx,2			; convert to dword offset
	mov	eax,fs:[bx]		; eax -> group name
	mov	GrpDefNamePtr,eax	; save it

; convert group name to upper case
	lfs	si,GrpDefNamePtr
	call	ConvertToUpperCase
	cmp	LastGrpDefBlkPtr,0	; zero if no previous allocations
	je	ggalloc			; no previous allocations, block must be allocated

; check if group entry already exists, compare based on name
	mov	ax,FirstGrpDefBlkPtr	; ax -> first grpdef block
	push	ds
	pop	fs				; fs -> wl32 data

ggmainloop:
	mov	gs,ax			; gs -> grpdef block
	mov	dx,gs:[GrpDefBlkStruc.gdbCount]	; dx holds entry count in block
	mov	bx,GRPDEFSYSVARSIZE	; gs:bx -> first entry

ggentloop:
	les	di,gs:[bx+GrpDefRecStruc.gdrGrpNamePtr]
	lds	si,fs:GrpDefNamePtr

; ds:si -> current grpdef name
; es:di -> stored grpdef name
gglencmp:
	cmpsb				; see if length byte matches
	jne	ggnextent		; no

	mov	cl,ds:[si-1]
	xor	ch,ch			; get # of bytes to check
	mov	al,cl			; save count of bytes
	and	cl,3			; get odd byte+word
	jcxz	gg2			; no odd byte
	repe	cmpsb
	jne	ggnextent		; no match

gg2:
	mov	cl,al			; restore bytes to check
	shr	cl,2			; get dwords to check, odd byte+word accounted for
	jcxz	ggmatch
	repe	cmpsd
	je	ggmatch			; match

ggnextent:
	add	bx,SIZE GrpDefRecStruc	; point to next entry
	dec	dx				; drop count of entries to check in block
	jne	ggentloop		; more entries remain

ggnextblk:
	mov	ax,gs:[GrpDefBlkStruc.gdbNextPtr]	; ax -> next block, if any
	or	ax,ax
	jne	ggmainloop		; next block exists

; new entry needed
	push	fs
	pop	ds				; restore ds -> wl32 data

; check if allocated individual segdef block is full
	mov	gs,LastGrpDefBlkPtr	; gs -> last allocated block
	cmp	gs:[GrpDefBlkStruc.gdbCount],MAXCOUNTGRPDEFBLK	; see if any free entries in block
	jb	gginit			; free entry exists

; make a new block
ggalloc:
	call	MakeGrpDefBlk	; allocate group definitions block

; initialize new grpdef entry in block
; gs -> grpdef block
gginit:
	mov	ax,gs:[GrpDefBlkStruc.gdbCount]	; ax holds current count
	inc	gs:[GrpDefBlkStruc.gdbCount]	; bump count in block
	mov	dx,SIZE GrpDefRecStruc	; bytes/structure
	mul	dx				; ax hold offset to entry, not counting sysvars
	add	ax,GRPDEFSYSVARSIZE	; adjust for sysvars, gs:ax -> entry slot
	mov	di,ax			; gs:di -> grpdef entry

; zero init entry
	xor	eax,eax
	mov	gs:[di+GrpDefRecStruc.gdrGrpOffset],eax	; zero init group offset
	mov	gs:[di+GrpDefRecStruc.gdrGrpLen],eax
	mov	gs:[di+GrpDefRecStruc.gdrFirstSegPtr],eax
	mov	gs:[di+GrpDefRecStruc.gdrGrpFlags],ax	; init flags

; save pointer to group name
	mov	eax,GrpDefNamePtr
	mov	gs:[di+GrpDefRecStruc.gdrGrpNamePtr],eax
	jmp	ggret

; found matching grpdef entry, gs:bx -> matched entry
ggmatch:
	push	fs
	pop	ds				; restore ds -> wl32 data
	mov	di,bx			; gs:di -> grpdef entry

ggret:
	pop	cx				; restore critical register
	pop	si
	pop	es
	pop	fs
	ret
GetGrpDefEntry	ENDP

;*****************************
;* MAKEGRPDEFBLK             *
;*****************************

; make a grpdef block
; returns gs -> block, updates LastGrpDefBlkPtr, FirstGrpDefBlkPtr variables
; destroys eax,dx

MakeGrpDefBlk	PROC
	mov	dx,SIZEGRPDEFBLK	; get number of bytes to allocate for grpdef block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastGrpDefBlkPtr	; keep -> previously last allocated block, if any
	mov	LastGrpDefBlkPtr,ax	; update last allocated block pointer
	mov	gs,ax			; gs -> block
	xor	eax,eax
	mov	gs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstGrpDefBlkPtr,ax	; see if first block allocated yet
	je	mgdfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,gs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	gs,ax			; gs -> previous block
	mov	gs:[GrpDefBlkStruc.gdbNextPtr],dx
	mov	gs,dx			; gs -> current block
	ret

mgdfirst:
	mov	ax,LastGrpDefBlkPtr
	mov	FirstGrpDefBlkPtr,ax	; update first allocated block pointer
	ret
MakeGrpDefBlk	ENDP

;*****************************
;* SETGRPPTRTABLEENTRY       *
;*****************************

; set the group pointer table block entry to point to
;  grpdef entry for current group
; set IOBuffHeaderStruc.ibhsPtrToGrpPtrs if first group for module
; upon entry gs:di -> grpdef entry to save pointer to in table,
;  CurrentGrpDefCount holds the current grpdef count for table index
; maintain cx,si,fs,gs
; destroys ax,bx,dx

SetGrpPtrTableEntry	PROC
	push	fs			; save critical registers

	cmp	LastGrpPtrTableBlkPtr,0	; zero if no previous allocations
	je	sgpalloc			; no previous allocations, block must be allocated

; check if allocated group pointer block is full
	mov	fs,LastGrpPtrTableBlkPtr	; fs -> last allocated block
	cmp	fs:[GrpPtrTableBlkStruc.gptbCount],MAXCOUNTGRPPTRTABLEBLK	; see if any free entries in block
	jb	sgpinit			; free entry exists

; make a new block
sgpalloc:
	call	MakeGrpPtrTableBlk	; allocate group pointer table block

; init proper entry in the block
; fs -> block
sgpinit:
	mov	bx,fs:[GrpPtrTableBlkStruc.gptbCount]
	inc	fs:[GrpPtrTableBlkStruc.gptbCount]	; bump count in block
	shl	bx,2			; dword per entry
	add	bx,GRPPTRTABLESYSVARSIZE	; fs:bx -> next group pointer table entry
	cmp	CurrentGrpDefCount,0	; see if first grpdef of module
	jne	sgp2			; no
	cmp	ModuleCount,-1	; see if not a module (created in resolution phase)
	je	sgp2			; yes, bypass pointer in i/o buffer setup

; first grpdef of module, keep pointer to start of table pointer in i/o buffer head
	mov	ax,fs			; ax:bx -> first group pointer entry
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	mov	fs:[WORD PTR IOBuffHeaderStruc.ibhsPtrToGrpPtrs],bx
	mov	fs:[WORD PTR IOBuffHeaderStruc.ibhsPtrToGrpPtrs+2],ax
	mov	fs,ax			; fs:bx -> group pointer entry

; fs:bx -> group pointer table entry
; gs:di -> group entry
sgp2:
	mov	fs:[bx],di		; save pointer to group entry in table
	mov	fs:[bx+2],gs

	pop	fs				; restore critical registers
	ret
SetGrpPtrTableEntry	ENDP

;*****************************
;* MAKEGRPPTRTABLEBLK        *
;*****************************

; make a grpdef block
; returns fs -> block, updates LastGrpPtrTableBlkPtr, FirstGrpPtrTableBlkPtr variables
; destroys eax,dx

MakeGrpPtrTableBlk	PROC
	mov	dx,SIZEGRPPTRTABLEBLK	; get number of bytes to allocate for group pointer table block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastGrpPtrTableBlkPtr	; keep -> previously last allocated block, if any
	mov	LastGrpPtrTableBlkPtr,ax	; update last allocated block pointer
	mov	fs,ax			; fs -> block
	xor	eax,eax
	mov	fs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstGrpPtrTableBlkPtr,ax	; see if first block allocated yet
	je	mgptfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,fs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	fs,ax			; fs -> previous block
	mov	fs:[GrpPtrTableBlkStruc.gptbNextPtr],dx
	mov	fs,dx			; fs -> current block
	ret

mgptfirst:
	mov	ax,LastGrpPtrTableBlkPtr
	mov	FirstGrpPtrTableBlkPtr,ax	; update first allocated block pointer
	ret
MakeGrpPtrTableBlk	ENDP

;*****************************
;* ASSIGNSEGTOGROUP          *
;*****************************

; assign segment to group
; upon entry gs:di -> group entry, GrpDefSegmentIndex holds segment index
; maintain cx,si,di,fs,gs

AssignSegToGroup	PROC
	push	gs			; save gs:di -> group entry
	push	di
	mov	gs,CurrentBaseOBJBuff	; gs -> i/o buffer base
	lgs	bx,gs:[IOBuffHeaderStruc.ibhsSegDefPtr]	; gs:bx -> first segdef entry
	mov	ax,GrpDefSegmentIndex
	dec	ax				; make relative zero
	mov	dx,SIZE IndSegDefRecStruc
	mul	dx				; ax holds offset from first segdef entry to grouped segdef entry
	add	bx,ax			; gs:bx -> grouped individual segment entry, not normalized
	cmp	bx,SIZEINDSEGDEFBLK	; see if overflow to next block
	jb	astgetmast		; no

; segment entry has wrapped to next buffer
	sub	bx,(SIZEINDSEGDEFBLK-INDSEGDEFSYSVARSIZE)
	mov	dx,gs:[IndSegDefBlkStruc.isdbNextPtr]	; get pointer to next block
	or	dx,dx			; make sure nonzero
	je	poor7			; poorly formed object module, bad index value
	mov	gs,dx			; gs:bx -> grouped individual segment entry, normalized

astgetmast:
	lgs	bx,gs:[bx+IndSegDefRecStruc.isdrMasterPtr]	; gs:bx -> master segdef entry of grouped segment
	pop	eax				; eax -> group entry
	push	eax			; save back to stack
	cmp	WORD PTR gs:[bx+MasterSegDefRecStruc.mssGroupPtr+2],0	; see if pre-existing group
	je	ast2			; no

	cmp	gs:[bx+MasterSegDefRecStruc.mssGroupPtr],eax	; see if pre-existing group differs from current
	jne	astdiff			; already member of different group than one specified

astret:
	pop	di				; restore gs:di -> group entry
	pop	gs
	ret

ast2:
	mov	gs:[bx+MasterSegDefRecStruc.mssGroupPtr],eax	; update group pointer
	or	gs:[bx+MasterSegDefRecStruc.mssFlags],GROUPMEMBERFLAG	; flag member of group
	jmp	astret

; segment is already member of different group
;@@@ warn segment in a different group already???
astdiff:
	jmp	astret			; ignore new group assignment for now

; poorly formed object record
poor7:
	mov	cl,7
	call	BadOBJModuleExit

AssignSegToGroup	ENDP

;*****************************
;* GETPUBSYMENTRY            *
;*****************************

; look up public symbol entry, create if none exist
; upon entry fs:si -> public symbol name
; return -> public entry in gs:di, carry flag set if new entry
; maintains cx,si,bp,ds,es,fs

GetPubSymEntry	PROC

;@@@	inc	TotalGetPubCount	; @@@ temporary

	push	es			; save critical registers
	push	fs
	push	bp
	push	cx
	push	si

	mov	PubSymChainFlag,OFF	; init chaining flag
	mov	bx,si			; fs:bx -> name
	mov	WORD PTR PubSymNamePtr,bx	; save pointer to name
	mov	WORD PTR PubSymNamePtr+2,fs

IFDEF	WATCOM_ASM
	cmp	IsCaseSensitiveOption,OFF
	jne	gpshash			; no uppercase conversion
ENDIF
	
	call	ConvertToUpperCase	; convert symbol name to uppercase if necessary

gpshash:
	call	GetHashCode	; hash code returned in bx
	shl	bx,2			; convert to dword offset
	mov	ax,ds:[bx+WORD PTR SymbolHashTable+2]	; get hash code selector
	or	ax,ax			; see if used
	je	gpsnouse			; no

; hash code used, check for collision or duplicate public symbol entry
	push	fs
	pop	gs				; gs:si -> current symbol name
	lfs	bp,ds:[bx+SymbolHashTable]	; fs:bp -> entry of first symbol with same hash code
	mov	bx,si			; gs:bx -> current symbol name
	cmp	bx,SIZEIOBUFFBLK-MAXOBJRECNAME	; check for possible overflow, normalize name if so
	jae	gpsgsbx			; possible normalization needed

; fs:bp -> symbol entry on record
; gs:bx -> current symbol name
gpsmainloop:
	les	di,fs:[bp+PubSymRecStruc.pssNamePtr]	; es:di -> symbol name on record, not normalized
	cmp	di,SIZEIOBUFFBLK-MAXOBJRECNAME	; check for possible name buffer overflow
	jae	gpsesdi			; possible, normalize name

gpsdssi:
	push	gs
	pop	ds
	mov	si,bx			; ds:si -> current symbol name
	cmpsb				; see if length byte matches
	je	gpslenok		; yes

; symbol names did not match, try another
	jc	gpsoldgr		; old name greater than new name

; new name greater than old name
gpsnewgr:
	xor	dl,dl			; zero dl to flag new > old
	cmp	WORD PTR fs:[bp+PubSymRecStruc.pssHigherNamePtr+2],0	; see if next link
	je	gpsnewsym		; no more names, this is a new symbol
	lfs	bp,fs:[bp+PubSymRecStruc.pssHigherNamePtr]	; fs:bp -> symbol entry of higher record name
	jmp	gpsmainloop	; try next

; normalize gs:bx source name for compare
gpsgsbx:
	call	NormalGSBXSource	; normalize gs:bx to name source buffer
	jmp	gpsmainloop

; normalize es:di destination name for compare
gpsesdi:
	call	NormalESDIDest	; normalize es:di to destination name buffer
	jmp	gpsdssi

; old name greater than new name
gpsoldgr:
	mov	dl,1			; set dl==1 to flag old>new
	cmp	WORD PTR fs:[bp+PubSymRecStruc.pssLowerNamePtr+2],0	; see if next link
	je	gpsnewsym		; no more names, this is a new symbol
	lfs	bp,fs:[bp+PubSymRecStruc.pssLowerNamePtr]	; fs:bp -> symbol entry of lower record name
	jmp	gpsmainloop	; try next

gpslenok:
	mov	cl,ds:[si-1]	; get # of bytes to check
	xor	ch,ch			; high byte always zero
	mov	al,cl			; save count of bytes
	and	cl,1			; get odd byte
	je	gps2			; no odd byte
	cmpsb
	jc	gpsoldgr		; old name greater than new name
	jne	gpsnewgr		; new name greater than old name
gps2:
	mov	cl,al			; restore bytes to check
	shr	cl,1			; get words to check, odd byte accounted for
	mov	al,cl			; save words to check
	and	cl,1			; get odd word
	je	gps3			; no odd word
	cmpsw
	jc	gpsoldgr		; old name greater than new name
	jne	gpsnewgr		; new name greater than old name
gps3:
	mov	cl,al			; restore words to check
	shr	cl,1			; get dwords to check, odd word accounted for
	je	gpsmatch
	repe	cmpsd
	jc	gpsoldgr		; old name greater than new name
	jne	gpsnewgr		; new name greater than old name

; symbol names match
; fs:bp -> symbol entry on record
gpsmatch:
	push	DGROUP
	pop	ds				; ds -> wl32 data
	mov	di,bp			; fs:di -> matching entry

; check local status, one or other local both must match
	cmp	IsLocalSymbol,0	; see if current symbol is local
	setne	al			; set al to nonzero if local
	test	fs:[di+PubSymRecStruc.pssFlags],LOCALSYMBOLFLAG
	setne	ah			; set ah to nonzero if local
	mov	cl,al			; test local status
	or	cl,ah
	je	gpsdupe			; neither symbol is local, both are duplicates

; one or both are local, al holds local status of new symbol, ah holds of symbol on record
gpslocal:
	cmp	al,ah			; see if both are local
	jne	gpsnewgr		; no, automatic no match, use higher name link for next compare

; both symbols are local, ModuleCount must match
	mov	eax,ModuleCount
	cmp	fs:[di+PubSymRecStruc.pssModuleCount],eax
	jne	gpsnewgr		; no match, use higher name link for next compare

; duplicate entry, return pointer to it
gpsdupe:
	push	fs
	pop	gs				; gs:di -> matching entry
	clc					; flag not a new public
	jmp	gpsret	; done

; new symbol, fs:bp -> parent; dl==0 if new name > parent, ==1 if parent > new
gpsnewsym:
	push	DGROUP
	pop	ds				; ds -> wl32 data
	mov	bx,fs
	mov	si,bp			; save old fs:bp pointer in bx:si
	inc	dx				; dl==1 if new>parent, ==2 if parent>new
	mov	PubSymChainFlag,dl	; save flag to indicate chaining needed

; check if any public symbol allocations made
gpsnouse:
	cmp	SearchExistSymFlag,OFF	; see if search for existing symbol only flag set
	jne	gpsnewflag		; set, don't create new symbol, but flag as new
	cmp	LastPubSymBlkPtr,0	; zero if no previous allocations
	je	gpsalloc		; no previous allocations, block must be allocated

; check if allocated public symbol block is full
	mov	gs,LastPubSymBlkPtr	; gs -> last allocated block
	cmp	gs:[PubSymBlkStruc.psbCount],MAXCOUNTPUBSYMBLK	; see if any free entries in block
	jb	gpsinit			; free entry exists

; make a new block
gpsalloc:
	call	MakePubSymBlk	; allocate public symbol block

; update public symbol block
gpsinit:
	mov	ax,gs:[PubSymBlkStruc.psbCount]	; ax holds current count
	inc	gs:[PubSymBlkStruc.psbCount]	; bump count in block
	mov	dx,SIZE PubSymRecStruc	; bytes/structure
	mul	dx				; ax hold offset to entry, not counting sysvars
	add	ax,PUBSYMSYSVARSIZE	; adjust for sysvars, gs:ax -> entry slot
	mov	di,ax			; gs:di -> public symbol entry

; init entry in block
	mov	eax,PubSymNamePtr	; eax -> public symbol name
	mov	gs:[di+PubSymRecStruc.pssNamePtr],eax	; init ssymbol name

	xor	eax,eax
	mov	gs:[di+WORD PTR PubSymRecStruc.pssHigherNamePtr+2],ax	; init name pointer
	mov	gs:[di+WORD PTR PubSymRecStruc.pssLowerNamePtr+2],ax
	mov	gs:[di+PubSymRecStruc.pssModuleCount],eax
	mov	gs:[di+PubSymRecStruc.pssFlags],ax

	cmp	PubSymChainFlag,OFF	; see if chaining symbol entries from previous
	jne	gpschain		; yes

	mov	ds:[bx+WORD PTR SymbolHashTable],di	; save hash code address
	mov	ds:[bx+WORD PTR SymbolHashTable+2],gs
	jmp	gpsnewflag

; must chain symbol entry from previous/parent/old entry
; parent pointer in bx:si, PubSymChainFlag value determines hi/low selection
; gs:di -> new public symbol entry
gpschain:
	mov	fs,bx			; fs:si -> old entry
	cmp	PubSymChainFlag,1	; see if new name is greater than parent
	jne	gpslower			; no
	mov	fs:[si+WORD PTR PubSymRecStruc.pssHigherNamePtr+2],gs
	mov	fs:[si+WORD PTR PubSymRecStruc.pssHigherNamePtr],di
	jmp	gpsnewflag

; parent (old) name is greater than new name
gpslower:
	mov	fs:[si+WORD PTR PubSymRecStruc.pssLowerNamePtr+2],gs
	mov	fs:[si+WORD PTR PubSymRecStruc.pssLowerNamePtr],di

gpsnewflag:
	stc					; flag a new public

gpsret:
	pop	si				; restore critical registers
	pop	cx
	pop	bp
	pop	fs
	pop	es
	ret
GetPubSymEntry	ENDP

;*****************************
;* CONVERTTOUPPERCASE        *
;*****************************

; convert symbol name to uppercase, if necessary
; upon entry fs:si -> symbol, length byte first
; destroys ax,cx

ConvertToUpperCase	PROC
	push	fs			; save critical registers
	push	si

	cmp	si,SIZEIOBUFFBLK	; see if overflow/wrap to next i/o buffer
							; this overflow value is assumed not to occur except with i/o buffer wrap
							; not through library or DOSSEG routines
	jae	ctunorm2			; yes

ctugetlen:
	lods	BYTE PTR fs:[si]	; get length byte
	mov	cl,al
	xor	ch,ch			; cx==bytes in symbol name

ctuadj:
	dec	si				; entry adjust for si increment

ctuloop:
	jcxz	cturet		; no more chars in name
	inc	si				; move to next char in name
	cmp	si,SIZEIOBUFFBLK	; see if overflow/wrap to next i/o buffer
							; this overflow value is assumed not to occur except with i/o buffer wrap
							; not through library or DOSSEG routines
	jae	ctunorm			; yes
	dec	cx				; drop count of chars to check
	mov	al,fs:[si]		; get symbol name char
	cmp	al,'a'			; check lower bounds
	jb	ctuloop			; not a lowercase letter
	cmp	al,'z'			; check upper bounds
	ja	ctuloop			; not lowercase
	sub	al,20h			; force lowercase to uppercase
	mov	fs:[si],al		; store back to name
	jmp	ctuloop	; loop through all chars in name

cturet:
	pop	si				; restore critical registers
	pop	fs
	ret

ctunorm:
	mov	si,IOBUFFSYSVARSIZE	; wrap si past sysvars
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; fs -> next block in chain
	jmp	ctuadj	; return to check loop with si adjustment

ctunorm2:
	mov	si,IOBUFFSYSVARSIZE	; wrap si past sysvars
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; fs -> next block in chain
	jmp	ctugetlen	; return to get length with si adjustment

ConvertToUpperCase	ENDP

;*****************************
;* MAKEPUBSYMBLK             *
;*****************************

; make public symbol block
; returns gs -> block,
;   updates LastPubSymBlkPtr, FirstPubSymBlkPtr variables
; destroys eax,dx

MakePubSymBlk	PROC
	mov	dx,SIZEPUBSYMBLK	; get number of bytes to allocate for public symbol block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastPubSymBlkPtr	; keep -> previously last allocate block, if any
	mov	LastPubSymBlkPtr,ax	; update last allocated block pointer
	mov	gs,ax			; gs -> block
	xor	eax,eax
	mov	gs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstPubSymBlkPtr,ax	; see if first block allocated yet
	je	mpsfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,gs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	gs,ax			; gs -> previous block
	mov	gs:[PubSymBlkStruc.psbNextPtr],dx
	mov	gs,dx			; gs -> current block
	ret

mpsfirst:
	mov	ax,LastPubSymBlkPtr
	mov	FirstPubSymBlkPtr,ax	; update first allocated block pointer
	ret
MakePubSymBlk	ENDP

;*****************************
;* SETSYMPTRTABLEENTRY       *
;*****************************

; set the symbol pointer table block entry to point to
;  symbol entry for current symbol (fixupp-referenceable)
; set IOBuffHeaderStruc.ibhsPtrToSymPtrs if first symbol for module
; upon entry gs:di -> symbol entry to save pointer to in table,
;  CurrentFixSymCount holds the current fixupp-referenceable symbol count for table index
; maintain cx,si,fs,gs
; destroys ax,bx,dx

SetSymPtrTableEntry	PROC
	push	fs			; save critical registers

	cmp	LastSymPtrTableBlkPtr,0	; zero if no previous allocations
	je	sspalloc			; no previous allocations, block must be allocated

; check if allocated symbol pointer block is full
	mov	fs,LastSymPtrTableBlkPtr	; fs -> last allocated block
	cmp	fs:[SymPtrTableBlkStruc.sptbCount],MAXCOUNTSYMPTRTABLEBLK	; see if any free entries in block
	jb	sspinit			; free entry exists

; make a new block
sspalloc:
	call	MakeSymPtrTableBlk	; allocate symbol pointer table block

; init proper entry in the block
; fs -> block
sspinit:
	mov	bx,fs:[SymPtrTableBlkStruc.sptbCount]
	inc	fs:[SymPtrTableBlkStruc.sptbCount]	; bump count in block
	shl	bx,2			; dword per entry
	add	bx,SYMPTRTABLESYSVARSIZE	; fs:bx -> next symbol pointer table entry
	cmp	CurrentFixSymCount,0	; see if first symbol of module
	jne	ssp2			; no
	cmp	ModuleCount,-1	; see if not a module (created in resolution phase)
	je	ssp2			; yes, bypass pointer in i/o buffer setup

; first symbol of module, keep pointer to start of table pointer in i/o buffer head
	mov	ax,fs			; ax:bx -> first symbol pointer entry
	mov	fs,CurrentBaseOBJBuff	; fs -> i/o buffer base
	mov	fs:[WORD PTR IOBuffHeaderStruc.ibhsPtrToSymPtrs],bx
	mov	fs:[WORD PTR IOBuffHeaderStruc.ibhsPtrToSymPtrs+2],ax
	mov	fs,ax			; fs:bx -> symbol pointer entry

; fs:bx -> symbol pointer table entry
; gs:di -> symbol entry
ssp2:
	mov	fs:[bx],di		; save pointer to symbol entry in table
	mov	fs:[bx+2],gs

	pop	fs				; restore critical registers
	ret
SetSymPtrTableEntry	ENDP

;*****************************
;* MAKESYMPTRTABLEBLK        *
;*****************************

; make a symbol pointer block
; returns fs -> block, updates LastSymPtrTableBlkPtr, FirstSymPtrTableBlkPtr variables
; destroys eax,dx

MakeSymPtrTableBlk	PROC
	mov	dx,SIZESYMPTRTABLEBLK	; get number of bytes to allocate for symbol pointer table block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastSymPtrTableBlkPtr	; keep -> previously last allocated block, if any
	mov	LastSymPtrTableBlkPtr,ax	; update last allocated block pointer
	mov	fs,ax			; fs -> block
	xor	eax,eax
	mov	fs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstSymPtrTableBlkPtr,ax	; see if first block allocated yet
	je	msptfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,fs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	fs,ax			; fs -> previous block
	mov	fs:[SymPtrTableBlkStruc.sptbNextPtr],dx
	mov	fs,dx			; fs -> current block
	ret

msptfirst:
	mov	ax,LastSymPtrTableBlkPtr
	mov	FirstSymPtrTableBlkPtr,ax	; update first allocated block pointer
	ret
MakeSymPtrTableBlk	ENDP

;*****************************
;* GETCOMDATENTRY            *
;*****************************

; create comdat entry
; returns comdat entry in gs:di
; maintain cx,si,fs
; destroys ax,bx,dx,di

GetComDatEntry	PROC
	cmp	LastComDatBlkPtr,0	; zero if no previous allocations
	je	gcdalloc			; no previous allocations, block must be allocated

; check if allocated individual comdat block is full
	mov	gs,LastComDatBlkPtr	; gs -> last allocated block
	cmp	gs:[ComDatBlkStruc.cdbCount],MAXCOUNTCOMDATBLK	; see if any free entries in block
	jb	gcdinit			; free entry exists

; make a new block
gcdalloc:
	call	MakeComDatBlk	; allocate comdat definitions block

; initialize new comdat entry in block
; gs -> comdat block
gcdinit:
	mov	ax,gs:[ComDatBlkStruc.cdbCount]	; ax holds current count
	inc	gs:[ComDatBlkStruc.cdbCount]	; bump count in block
	mov	dx,SIZE ComDatRecStruc	; bytes/structure
	mul	dx				; ax hold offset to entry, not counting sysvars
	add	ax,COMDATSYSVARSIZE	; adjust for sysvars, gs:ax -> entry slot
	mov	di,ax			; gs:di -> comdat entry

; zero init entry
	xor	eax,eax
	mov	gs:[di+ComDatRecStruc.cdsLength],eax
	mov	gs:[di+ComDatRecStruc.cdsIndSegDefPtr],eax
	mov	gs:[di+ComDatRecStruc.cdsGrpDefPtr],eax
	mov	gs:[di+ComDatRecStruc.cdsFlags],ax	; init flags
	ret
GetComDatEntry	ENDP

;*****************************
;* MAKECOMDATBLK             *
;*****************************

; make a comdat block
; returns gs -> block, updates LastComDatBlkPtr, FirstComDatBlkPtr variables
; destroys eax,dx

MakeComDatBlk	PROC
	mov	dx,SIZECOMDATBLK	; get number of bytes to allocate for comdat block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastComDatBlkPtr	; keep -> previously last allocated block, if any
	mov	LastComDatBlkPtr,ax	; update last allocated block pointer
	mov	gs,ax			; gs -> block
	xor	eax,eax
	mov	gs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstComDatBlkPtr,ax	; see if first block allocated yet
	je	mcdfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,gs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	gs,ax			; gs -> previous block
	mov	gs:[ComDatBlkStruc.cdbNextPtr],dx
	mov	gs,dx			; gs -> current block
	ret

mcdfirst:
	mov	ax,LastComDatBlkPtr
	mov	FirstComDatBlkPtr,ax	; update first allocated block pointer
	ret
MakeComDatBlk	ENDP

IFDEF	DLLSUPPORT

;*****************************
;* GETEXPDEFENTRY            *
;*****************************

; create expdef entry
; returns expdef entry in gs:di
; maintain cx,si,fs
; destroys ax,bx,dx,di

GetEXPDEFEntry	PROC
	cmp	LastEXPDEFBlkPtr,0	; zero if no previous allocations
	je	gedalloc			; no previous allocations, block must be allocated

; check if allocated expdef block is full
	mov	gs,LastEXPDEFBlkPtr	; gs -> last allocated block
	cmp	gs:[EXPDEFBlkStruc.edbCount],MAXCOUNTEXPDEFBLK	; see if any free entries in block
	jb	gedinit			; free entry exists

; make a new block
gedalloc:
	call	MakeEXPDEFBlk	; allocate expdef block

; initialize new expdef entry in block
; gs -> expdef block
gedinit:
	mov	ax,gs:[EXPDEFBlkStruc.edbCount]	; ax holds current count
	inc	gs:[EXPDEFBlkStruc.edbCount]	; bump count in block
	mov	dx,SIZE EXPDEFRecStruc	; bytes/structure
	mul	dx				; ax hold offset to entry, not counting sysvars
	add	ax,EXPDEFSYSVARSIZE	; adjust for sysvars, gs:ax -> entry slot
	mov	di,ax			; gs:di -> expdef entry

; zero init entry
	xor	eax,eax
	mov	gs:[di+EXPDEFRecStruc.edsExportedFlag],al
	mov	gs:[di+EXPDEFRecStruc.edsExportedNamePtr],eax
	mov	gs:[di+EXPDEFRecStruc.edsInternalNamePtr],eax
	mov	gs:[di+EXPDEFRecStruc.edsExportOrdinal],ax
	ret
GetEXPDEFEntry	ENDP

;*****************************
;* MAKEEXPDEFBLK             *
;*****************************

; make a expdef block
; returns gs -> block, updates LastEXPDEFBlkPtr, FirstEXPDEFBlkPtr variables
; destroys eax,dx

MakeEXPDEFBlk	PROC
	mov	dx,SIZEEXPDEFBLK	; get number of bytes to allocate for expdef block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastEXPDEFBlkPtr	; keep -> previously last allocated block, if any
	mov	LastEXPDEFBlkPtr,ax	; update last allocated block pointer
	mov	gs,ax			; gs -> block
	xor	eax,eax
	mov	gs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstEXPDEFBlkPtr,ax	; see if first block allocated yet
	je	medfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,gs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	gs,ax			; gs -> previous block
	mov	gs:[EXPDEFBlkStruc.edbNextPtr],dx
	mov	gs,dx			; gs -> current block
	ret

medfirst:
	mov	ax,LastEXPDEFBlkPtr
	mov	FirstEXPDEFBlkPtr,ax	; update first allocated block pointer
	ret
MakeEXPDEFBlk	ENDP

;*****************************
;* GETIMPDEFENTRY            *
;*****************************

; create impdef entry
; returns impdef entry in gs:di
; maintain cx,si,fs
; destroys ax,bx,dx,di

GetIMPDEFEntry	PROC
	cmp	LastIMPDEFBlkPtr,0	; zero if no previous allocations
	je	gidalloc			; no previous allocations, block must be allocated

; check if allocated impdef block is full
	mov	gs,LastIMPDEFBlkPtr	; gs -> last allocated block
	cmp	gs:[IMPDEFBlkStruc.idbCount],MAXCOUNTIMPDEFBLK	; see if any free entries in block
	jb	gidinit			; free entry exists

; make a new block
gidalloc:
	call	MakeIMPDEFBlk	; allocate impdef block

; initialize new impdef entry in block
; gs -> impdef block
gidinit:
	mov	ax,gs:[IMPDEFBlkStruc.idbCount]	; ax holds current count
	inc	gs:[IMPDEFBlkStruc.idbCount]	; bump count in block
	mov	dx,SIZE IMPDEFRecStruc	; bytes/structure
	mul	dx				; ax hold offset to entry, not counting sysvars
	add	ax,IMPDEFSYSVARSIZE	; adjust for sysvars, gs:ax -> entry slot
	mov	di,ax			; gs:di -> impdef entry

; zero init entry
	xor	eax,eax
	mov	gs:[di+IMPDEFRecStruc.idsOrdinalFlag],al
	mov	gs:[di+IMPDEFRecStruc.idsInternalNamePtr],eax
	mov	gs:[di+IMPDEFRecStruc.idsModuleNamePtr],eax
	mov	gs:[di+IMPDEFRecStruc.idsEntryIdentPtr],eax
	mov	gs:[di+IMPDEFRecStruc.idsGeneralFlags],al
	ret
GetIMPDEFEntry	ENDP

;*****************************
;* MAKEIMPDEFBLK             *
;*****************************

; make a impdef block
; returns gs -> block, updates LastIMPDEFBlkPtr, FirstIMPDEFBlkPtr variables
; destroys eax,dx

MakeIMPDEFBlk	PROC
	mov	dx,SIZEIMPDEFBLK	; get number of bytes to allocate for impdef block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastIMPDEFBlkPtr	; keep -> previously last allocated block, if any
	mov	LastIMPDEFBlkPtr,ax	; update last allocated block pointer
	mov	gs,ax			; gs -> block
	xor	eax,eax
	mov	gs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstIMPDEFBlkPtr,ax	; see if first block allocated yet
	je	midfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,gs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	gs,ax			; gs -> previous block
	mov	gs:[IMPDEFBlkStruc.idbNextPtr],dx
	mov	gs,dx			; gs -> current block
	ret

midfirst:
	mov	ax,LastIMPDEFBlkPtr
	mov	FirstIMPDEFBlkPtr,ax	; update first allocated block pointer
	ret
MakeIMPDEFBlk	ENDP

ENDIF

;*****************************
;* MAKESOURCELINEBLK         *
;*****************************

; create source line block
; upon entry LineSegmentID holds line number segment
; returns pointer to block in es
; maintain ebx,cx,dx,si,fs
; destroys ax,es,gs

MakeSourceLineBlk	PROC
	mov	dx,SIZESOURCELINEBLK	; get number of bytes to allocate for source line block
	call	AllocateMemory	; allocate memory for it
	mov	es,ax			; es -> block

	xor	eax,eax
	mov	DWORD PTR es:[SourceLineBlkStruc.ssbNextContPtr],eax	; zero next continuation, next segment pointers
	mov	es:[SourceLineBlkStruc.ssbCount],eax	; zero count of entries in block
	mov	ax,LineSegmentID
	mov	es:[SourceLineBlkStruc.ssbSegmentID],ax	; save Segment ID
	mov	gs,CurrentBaseOBJBuff
	cmp	gs:[IOBuffHeaderStruc.ibhsSourceLinePtr],0	; see if first block
	jne	mslb2			; no
	mov	gs:[IOBuffHeaderStruc.ibhsSourceLinePtr],es

mslbret:
	ret

; update next block pointers of previous block
; ax==LineSegmentID
mslb2:
	mov	gs,gs:[IOBuffHeaderStruc.ibhsSourceLinePtr]	; gs -> first block

mslbsegloop:
	cmp	gs:[SourceLineBlkStruc.ssbSegmentID],ax	; see if same ID
	jne	mslbnext		; no match, try next segment

mslbcontloop:
	cmp	gs:[SourceLineBlkStruc.ssbNextContPtr],0	; see if continuation
	je	mslbcontupd		; no continuation, update continuation pointer
	mov	gs,gs:[SourceLineBlkStruc.ssbNextContPtr]	; gs -> next block
	jmp	mslbcontloop

; update continuation pointer
mslbcontupd:
	mov	gs:[SourceLineBlkStruc.ssbNextContPtr],es
	jmp	mslbret

mslbnext:
	cmp	gs:[SourceLineBlkStruc.ssbNextSegPtr],0	; see if next segment
	je	mslbsegupd		; no next segment, update next segment pointer
	mov	gs,gs:[SourceLineBlkStruc.ssbNextSegPtr]	; gs -> next block
	jmp	mslbsegloop

; update next segment pointer
mslbsegupd:
	mov	gs:[SourceLineBlkStruc.ssbNextSegPtr],es
	jmp	mslbret

MakeSourceLineBlk	ENDP

ENDS

END
