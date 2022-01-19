;*********************************************************************
;*   WLCLIP.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          03/07/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Clipper specific routines                                       *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLCLIP
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module

IFDEF SYMBOLPACK
DGROUP	GROUP CONST,_BSS,_DATA

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
PUBLIC	CheckCompressedSymbol
PUBLIC	CompressedSymbolFixup
PUBLIC	CreateClipperModEntry
PUBLIC	FixupClipperTokens
PUBLIC	ParseClipperSymbols
PUBLIC	Pass2ClipperCheck
PUBLIC	ProcessCompressedSymbols
PUBLIC	ProcessSymbolTable

; variables
PUBLIC	CompressThisModule
PUBLIC	CurrentModSymCount
PUBLIC	NewLEDATAOffset
PUBLIC	RelocationAdjustment
PUBLIC	StartSymbolCount
PUBLIC	UniqueSymbolCount

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

CompressThisModule	DB	?	; nonzero if need to compress this module's symbols

CurrentClipperModPtr	DW	?	; current clipper module entry pointer, selector
CurrentModSymCount	DW	?	; current module symbol count
NewLEDATAOffset	DW	?	; data offset of LEDATA after compression, running total
ParseSymbolOffset	DW	?	; offset of parsed symbol from Clipper code fixup routine
PreviousModSymCount	DW	?	; previous module symbol count
StartSymbolCount	DW	?	; starting symbol count prior to this module

RelocationAdjustment	DD	?	; relocation adjustment due to Clipper symbol compression

SymbolBuffer	DB	16	; storage of symbol entry

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

Clipper5TokenList	DB	6h,7h,8h,9h,0ah,0bh,0ch,0dh,0eh,0fh,10h,11h,12h,13h,14h,15h,16h

; merge Clipper symbol tables into CONST data
INCLUDE WLSYMTOK.INC

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

LeftToParse	DB	0		; length of symbol token left to parse
OldParseProc	DB	0	; nonzero if procedure already being compression processed
ParseLength	DB	0		; nonzero if parsing asciiz token length (length of string)

FirstClipperModPtr	DW	0	; first clipper module entry pointer
FirstClipSymBlkPtr	DW	0	; first clipper symbol block pointer
LastClipSymBlkPtr	DW	0	; last clipper symbol block pointer
UniqueSymbolCount	DW	0	; unique symbol count

FirstSymEntPtr	DD	0	; first stored symbol entry pointer

ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	ClipperSymSegIndex:WORD
EXTRN	CompBuffSource:BYTE
EXTRN	CompressionInForce:BYTE
EXTRN	CurrentBaseOBJBuff:WORD
EXTRN	CurrentFileName:BYTE
EXTRN	CurrentSymbolSegPtr:DWORD
EXTRN	DataRecordOffset:WORD
EXTRN	IsCompressedSegment:BYTE
EXTRN	LogicalDataRecOff:DWORD
EXTRN	ModuleCount:DWORD
EXTRN	NeedCompSymProcess:BYTE
EXTRN	SymbolTableOverflow:BYTE

;*****************************
;* External declarations     *
;*****************************

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

EXTRN	AllocateMemory:PROC
EXTRN	LinkerErrorExit:PROC
EXTRN	ReadByte:PROC
EXTRN	ReadByteDecCX:PROC
EXTRN	ReadDwordDecCX:PROC
EXTRN	ReadWordDecCX:PROC
EXTRN	ScanAhead:PROC

;*****************************
;* CREATECLIPPERMODENTRY     *
;*****************************

; create clipper module entry for symbol table compression use
; format as follows (12 system bytes):
;   module ID, 4 bytes
;   next entry pointer/selector, 2 bytes
;   symbol count, 2 bytes
;   prior unique symbol count 2 bytes
;   SYMBOLS segdef index, relative 1, 2 bytes
;   symbol entry pointer, 4 bytes per symbol, after processing changed to:
;      symbol name counts (offsets with compressed table)
;   symbol flags, 1 byte per symbol
; destroys ax,bx,cx,dx,si,di,es

CreateClipperModEntry	PROC
	les	di,CurrentSymbolSegPtr
	mov	ax,WORD PTR es:[di+MasterSegDefRecStruc.mssSegLength]	; get length (symbol count*16)
	shr	ax,2			; 4 bytes/symbol
	mov	cx,ax
	shr	cx,2			; cx==symbol count
	add	ax,cx			; 5 bytes/symbol

	push	ax			; save total symbol byte count
	add	ax,SIZE ClipperModEntStruc	; clipper module system bytes
	mov	dx,ax
	call	AllocateMemory	; allocate memory for Clipper module entry
	cmp	FirstClipperModPtr,0	; see if pre-existing clipper module entry
	je	ccefirst		; no
	mov	es,CurrentClipperModPtr
	mov	es:[ClipperModEntStruc.cmsNextPtr],ax	; save -> to next (now current) entry
	jmp	SHORT cce2

ccefirst:
	mov	FirstClipperModPtr,ax	; save -> first clipper module entry

cce2:
	mov	CurrentClipperModPtr,ax	; save -> current module
	mov	es,CurrentBaseOBJBuff	; es -> obj base buffer
	or	es:[IOBuffHeaderStruc.ibhsFlags],ISCLIPPERMODFLAG	; flag clipper module
	mov	es:[IOBuffHeaderStruc.ibhsClipModPtr],ax	; save pointer to clipper module entry

	mov	es,ax			; es -> current clipper module entry
	mov	es:[ClipperModEntStruc.cmsSymbolCount],cx		; save symbol count

	mov	ax,ClipperSymSegIndex
	mov	es:[ClipperModEntStruc.cmsSegdefIndex],ax	; save symbols segment index
	mov	es:[ClipperModEntStruc.cmsNextPtr],0	; zero next entry pointer
	mov	eax,ModuleCount
	mov	es:[ClipperModEntStruc.cmsModuleID],eax		; save module ID

	pop	cx					; get symbol entry byte count
	mov	di,SIZE ClipperModEntStruc	; point past system info bytes
	xor	eax,eax
	mov	dx,cx
	shr	cx,2
	rep	stosd				; zero entries
	mov	cx,dx
	and	cx,3
	rep	stosb				; pick up leftover bytes
	ret
CreateClipperModEntry	ENDP

;*****************************
;* PARSECLIPPERSYMBOLS       *
;*****************************

; parse through clipper SYMBOLS data, compress segment,
; set up counts in clipper module entry symbol entries
; upon entry cx=record length, fs:si -> enumerated data offset
; destroys ax,bx,dx,di,bp,gs

ParseClipperSymbols	PROC
	push	es			; save critical registers
	push	si
	push	cx
	mov	IsCompressedSegment,ON	; flag compression on fixups for this LEDATA
	mov	ax,CurrentModSymCount
	mov	PreviousModSymCount,ax
	call	ReadWordDecCX	; get enumerated data record offset

; fs:si -> symbol table entries, cx holds total length+1
	movzx	eax,ax
	mov	LogicalDataRecOff,eax	; save symbols data record offset
	mov	dx,cx
	shr	dx,4			;  dx holds counts of symbol table entries in LEDATA

	mov	ax,CurrentModSymCount
	mov	bx,ax
	shl	bx,2			; convert to dword offset
	add	bx,ax			; convert to 5-byte offset
	add	bx,SIZE ClipperModEntStruc	; adjust past system bytes, bx -> current symbol entry

; parse through each symbol table entry
pcssymloop:
	push	dx			; save count of symbol table entries
	mov	di,OFFSET DGROUP:SymbolBuffer	; es:di -> destination

	cmp	si,SIZEIOBUFFBLK-15	; see if at changeover point to next block
	jb	pcsfasttrans	; no

; have to read the slow way
	mov	bp,4			; 16 bytes, 4 dwords

pcsslowloop:
	call	ReadDwordDecCX
	mov	ds:[di],eax		; save symbol name
	add	di,4
	dec	bp				; drop count of symbol words to save
	jne	pcsslowloop
	jmp	SHORT pcschkstatic

; do fast transfer of symbol table entry
; es:di -> symbol buffer
pcsfasttrans:
	push	cx
	push	fs
	pop	ds				; ds:si -> obj image
	mov	cx,4			; 16 bytes, 4 dwords
	rep	movsd
	push	es
	pop	ds
	pop	cx
	sub	cx,16			; update chars in obj record

pcschkstatic:
	pop	dx				; restore count of symbol entries
	push	es			; save critical register
	cmp	WORD PTR SymbolBuffer+10,0	; see if static symbol
	jne	pcsnextsym			; yes, leave a zero pointer as indicator

; not a static/special symbol
	call	GetClipSymEntry	; get pointer to symbol entry in gs:di
	mov	es,CurrentClipperModPtr	; KEEP FLAGS
	mov	es:[bx],di			; save pointer for module's symbol count/offset entry, KEEP FLAGS
	mov	es:[bx+2],gs
	jnc	pcsnextsym			; not a new entry
	or	BYTE PTR es:[bx+4],10h	; flag a new entry created

pcsnextsym:
	pop	es				; restore critical register
	inc	CurrentModSymCount	; bump current symbol
	add	bx,5			; point to next entry
	dec	dx				; drop count of symbols to check
	jne	pcssymloop		; more symbols

	mov	ax,CurrentModSymCount
	add	ax,UniqueSymbolCount
	cmp	ax,4096				; check if overflow occurred on symbols in program
	jbe	pcschklast

; overflow occurred
	mov	gs,CurrentBaseOBJBuff	; gs -> obj base buffer
	and	gs:[IOBuffHeaderStruc.ibhsFlags],(NOT ISCLIPPERMODFLAG)	; turn off clipper module flag
	mov	eax,-1
	mov	gs,CurrentClipperModPtr
	mov	gs:[ClipperModEntStruc.cmsModuleID],eax	; -1 module ID flags end of clipper symbol table entries
	inc	ax					; ax==0
	mov	IsCompressedSegment,al	; flag no compression on any fixups for this L?DATA
	inc	ax					; ax==1
	mov	SymbolTableOverflow,al	; flag overflow
	mov	ax,StartSymbolCount
	mov	UniqueSymbolCount,ax	; restore unique symbol count to previous
	jmp	SHORT pcsdone

pcschklast:
	lgs	di,CurrentSymbolSegPtr
	mov	ax,CurrentModSymCount
	shl	ax,4				; *16
	movzx	eax,ax
	cmp	eax,gs:[di+MasterSegDefRecStruc.mssSegLength]
	jb	pcsdone				; not last LEDATA

	mov	NeedCompSymProcess,ON	; flag further compression parsing needed for MODEND check

pcsdone:
	pop	cx					; restore critical registers
	pop	si
	pop	es
	ret
ParseClipperSymbols	ENDP

;*****************************
;* GETCLIPSYMENTRY           *
;*****************************

; get clipper symbol entry, create if doesn't exist
; returns clipper symbol entry in gs:di
; maintain bx,cx,dx,si,es
; destroys ax,cx,di

GetClipSymEntry	PROC
	push	es			; save critical registers
	push	bx
	push	dx
	push	si

	xor	dl,dl			; init high/low flag
	cmp	LastClipSymBlkPtr,0	; zero if no previous allocations
	je	gcsalloc		; no previous allocations, block must be allocated

; check if symbol entry already exists, compare based on name
	mov	ax,FirstClipSymBlkPtr	; ax -> first clipper symbol block
	mov	di,CLIPSYMSYSVARSIZE	; ax:di -> first entry

; ds:si -> current clipper symbol name
; ax:di -> stored clipper symbol name
gcscomploop:
	or	ax,ax			; see if any names left, null selector if not
	je	gcsnomatch		; no
	mov	si,OFFSET DGROUP:SymbolBuffer
	mov	bx,di			; ax:bx -> stored name
	mov	es,ax			; es:di -> stored name
	mov	cx,5
	repe	cmpsw		; must match to 10 chars
	je	gcsmatch		; match

; names didn't match, try next from high/low pointer
gcstrynext:
	jc	gcslow

gcshigh:
	mov	di,WORD PTR es:[bx+ClipSymRecStruc.cssHighPtr]	; get high pointer in ax:di
	mov	ax,WORD PTR es:[bx+ClipSymRecStruc.cssHighPtr+2]
	mov	dl,ClipSymRecStruc.cssHighPtr	; flag previous lower
	jmp	SHORT gcscomploop

gcslow:
	mov	di,WORD PTR es:[bx+ClipSymRecStruc.cssLowPtr]	; get low pointer in ax:di
	mov	ax,WORD PTR es:[bx+ClipSymRecStruc.cssLowPtr+2]
	mov	dl,ClipSymRecStruc.cssLowPtr	; flag previous higher
	jmp	SHORT gcscomploop

; didn't match any symbol names on record, create symbol table entry and update pointers
; check if allocated clipper symbol block is full
; es:di -> final nonmatching entry
gcsnomatch:
	mov	gs,LastClipSymBlkPtr	; gs -> last allocated block
	cmp	gs:[ClipSymBlkStruc.csbCount],MAXCOUNTCLIPSYMBLK	; see if any free entries in block
	jb	gcsinit			; free entry exists

; make a new block
gcsalloc:
	call	MakeClipSymBlk	; allocate symbol entry block

; initialize new clipper symbol entry in block
; gs -> clipper symbol block
; es:bx -> final nomatching entry
gcsinit:
	push	es			; save -> nonmatching entry needs pointer updated
	mov	ax,gs:[ClipSymBlkStruc.csbCount]	; ax holds current count
	inc	gs:[ClipSymBlkStruc.csbCount]	; bump count in block
	mov	cl,dl			; cl holds high/low offset of previous entry, zero if none
	mov	dx,SIZE ClipSymRecStruc	; bytes/structure
	mul	dx				; ax hold offset to entry, not counting sysvars
	add	ax,CLIPSYMSYSVARSIZE	; adjust for sysvars, gs:ax -> entry slot
	mov	di,ax			; gs:di -> clipper symbol entry

	push	gs
	pop	es				; es:di -> clipper symbol entry
	mov	si,OFFSET DGROUP:SymbolBuffer	; ds:si -> new symbol name
	movsd				; copy ten name chars
	movsd
	movsw
	pop	es				; es:bx -> nonmatching entry, if any
	or	cl,cl			; see if previous exists
	je	gcszero			; no, first ever symbol entry saved

; previous pointer exists
; gs:ax -> clipper symbol entry (update value)
; es:bx -> nonmatching entry (to update)
; cl holds high or low offset relative entry for es:bx
	xor	ch,ch			; zap high byte of offset
	add	bx,cx			; es:bx -> pointer to update
	mov	es:[bx],ax		; update previous pointer
	mov	es:[bx+2],gs

; zero out values in symbol entry
; gs:di -> clipper symbol entry past name
gcszero:
	push	gs
	pop	es				; es:di -> clipper symbol entry past name
	mov	bx,di
	sub	bx,10			; gs:bx -> clipper symbol entry
	xor	eax,eax
	mov	cx,(SIZE ClipSymRecStruc)-10
	mov	dx,cx			; zero out information following symbol name
	shr	cx,2
	rep	stosd
	mov	cx,dx
	and	cx,3
	rep	stosb
	stc					; show failure (new entry creation)
	jmp	SHORT gcsret

; found matching clipper symbol entry, es:bx -> entry
gcsmatch:
	push	es
	pop	gs				; gs:bx -> matched entry
	clc					; show success

; gs:bx -> new or matched entry
gcsret:
	mov	di,bx			; gs:di -> clipper symbol entry
	pop	si				; restore critical register
	pop	dx
	pop	bx
	pop	es
	ret
GetClipSymEntry	ENDP

;*****************************
;* MAKECLIPSYMBLK             *
;*****************************

; make a clipper symbol block
; returns gs -> block, updates LastClipSymBlkPtr, FirstClipSymBlkPtr variables
; destroys eax

MakeClipSymBlk	PROC
	push	dx
	mov	dx,SIZECLIPSYMBLK	; get number of bytes to allocate for clipper symbol block
	call	AllocateMemory	; allocate memory for it
	mov	dx,LastClipSymBlkPtr	; keep -> previously last allocated block, if any
	mov	LastClipSymBlkPtr,ax	; update last allocated block pointer
	mov	gs,ax			; gs -> block
	xor	eax,eax
	mov	gs:[eax],eax	; zero first four bytes of block, count and next pointer sysvars
	cmp	FirstClipSymBlkPtr,ax	; see if first block allocated yet
	je	mcsfirst		; no

; update previously last allocated block to point to this new block
	mov	ax,gs
	xchg	ax,dx		; ax -> previous block, dx -> current block
	mov	gs,ax			; gs -> previous block
	mov	gs:[ClipSymBlkStruc.csbNextPtr],dx
	mov	gs,dx			; gs -> current block

mcsret:
	pop	dx
	ret

mcsfirst:
	mov	ax,LastClipSymBlkPtr
	mov	FirstClipSymBlkPtr,ax	; update first allocated block pointer
	jmp	SHORT mcsret

MakeClipSymBlk	ENDP

;*****************************
;* COMPRESSEDSYMBOLFIXUP     *
;*****************************

; process fixup on compressed symbol segment LEDATA
; destroys ax,bx,di

CompressedSymbolFixup	PROC
	cmp	IsCompressedSegment,0	; see if compressed segment
	jne	csfdofix			; yes
	ret

csfdofix:
	push	cx				; save critical registers
	push	si
	push	es

	mov	ax,DataRecordOffset	; offset of fixup within this LEDATA
	shr	ax,2				; convert to dword offset
	and	al,0fch				; mask off leftover bits
	mov	bx,ax
	shr	bx,2
	add	ax,bx				; 5-byte offset

	mov	bx,PreviousModSymCount
	shl	bx,2				; convert to dword offset (offset prior to this LEDATA)
	add	bx,PreviousModSymCount	; convert to 5-byte offset
	add	bx,ax
	add	bx,SIZE ClipperModEntStruc	; adjust past system bytes
	mov	es,CurrentClipperModPtr	; es:bx -> current entry for fixup
	cmp	DWORD PTR es:[bx],0	; see if static
	je	csfret				; yes, no new entry needed

	mov	al,80h
	or	BYTE PTR es:[bx+4],al	; flag that this entry was procedure
	mov	cx,es				; save -> current entry
	les	di,es:[bx]			; es:di -> symbol table name entry

	test	es:[di+ClipSymRecStruc.cssFlags],al	; see if already flagged as procedure
	jne	csfret				; yes
	or	es:[di+ClipSymRecStruc.cssFlags],al	; flag as procedure
	mov	es,cx				; es -> current entry
	or	BYTE PTR es:[bx+4],10h	; flag as new (not previously flagged as procedure)

csfret:
	pop	es					; restore critical registers
	pop	si
	pop	cx
	ret
CompressedSymbolFixup	ENDP

;*****************************
;* PROCESSCOMPRESSEDSYMBOLS  *
;*****************************

; final process of compressed symbols, update pointers to numbers and segment length
; destroys ax,bx,dx,di

ProcessCompressedSymbols	PROC
	cmp	CurrentModSymCount,0	; see if any symbols in module
	jne	cfcdofinal			; yes
	ret

cfcdofinal:
	push	cx				; save critical registers
	push	si
	push	es
	mov	cx,CurrentModSymCount
	mov	bx,SIZE ClipperModEntStruc	; adjust past system bytes

cfcchkloop:
	mov	es,CurrentClipperModPtr
	mov	dx,es				; save -> current clipper module pointer
	cmp	DWORD PTR es:[bx],0	; see if static
	je	cfcstatic			; yes, automatic new symbol
	mov	al,es:[bx+4]		; get flags on current entry

	les	di,es:[bx]			; es:di -> symbol table name entry
	test	al,80h			; see if procedure (always flagged in symbol table entry too)
	je	cfcnonproc			; no
	test	al,10h			; see if new procedure
	jne	cfcnewproc			; yes
	jmp	SHORT cfcoldproc	; flagged as procedure previously

; symbol was a variable
; symbol is new even if not marked new IFF symbol table entry nonprocedure
;  flag is not set
cfcnonproc:
	test	al,10h			; see if new variable
	jne	cfcnewvar			; yes
	test	BYTE PTR es:[di+ClipSymRecStruc.cssFlags],40h	; see if flagged as variable/nonprocedure
	jne	cfcoldvar			; yes

cfcnewvar:
	or	BYTE PTR es:[di+ClipSymRecStruc.cssFlags],40h	; flag used as variable, possibly redundant
	mov	ax,UniqueSymbolCount	; get unique symbol offset
	mov	es:[di+ClipSymRecStruc.cssNonProcValue],ax	; update variable unique symbol for this entry
	jmp	SHORT cfcbumpcount

cfcoldvar:
	mov	ax,es:[di+ClipSymRecStruc.cssNonProcValue]	; get symbol number of variable
	jmp	SHORT cfcflagcomp	; flag compression of old symbol

cfcoldproc:
	mov	ax,es:[di+ClipSymRecStruc.cssProcValue]	; get symbol number of procedure

cfcflagcomp:
	mov	es,dx				; es -> current clipper module
	or	BYTE PTR es:[bx+4],20h	; set compress this symbol flag
	jmp	SHORT cfcchange

; es -> symbol table name entry
cfcnewproc:
	mov	ax,UniqueSymbolCount	; get unique symbol offset
	mov	es:[di+ClipSymRecStruc.cssProcValue],ax	; update procedure unique symbol for this entry
; fall through to redundant load, faster than jumping around

cfcstatic:
	mov	ax,UniqueSymbolCount	; get unique symbol offset

cfcbumpcount:
	inc	UniqueSymbolCount	; bump count of uniques
	mov	es,CurrentClipperModPtr

cfcchange:
	movzx	eax,ax
	mov	es:[bx],eax			; change pointer to offset

; move to next entry, if any
cfcnext:
	add	bx,5
	loop	cfcchkloop		; loop thru all entries

; update segment length, clipper module unique symbol count
	mov	dx,StartSymbolCount
	mov	ax,UniqueSymbolCount
	sub	ax,dx				; compute nonduplicate symbol count for module
	shl	ax,4
	movzx	eax,ax
	les	di,CurrentSymbolSegPtr
	mov	es:[di+MasterSegDefRecStruc.mssSegLength],eax
	les	di,es:[di+MasterSegDefRecStruc.mssFirstIndSegPtr]	; es:di -> individual segdef
	mov	es:[di+IndSegDefRecStruc.isdrSegLength],eax
	mov	es,CurrentClipperModPtr
	mov	es:[ClipperModEntStruc.cmsPriorUnique],dx	; save unique symbols previous to this module

cfcret:
	pop	es					; restore critical registers
	pop	si
	pop	cx
	ret
ProcessCompressedSymbols	ENDP

;*****************************
;* PASS2CLIPPERCHECK         *
;*****************************

; see if Clipper module that is compressed, set appropriate flags if so
; destroys ax,gs

Pass2ClipperCheck	PROC
	xor	ax,ax
	mov	CompressThisModule,al	; init compress this module flag
	cmp	CompressionInForce,al	; see if Clipper symbol table compression in force
	je	p2cret			; no

	mov	gs,CurrentBaseOBJBuff	; gs -> obj base buffer
	test	gs:[IOBuffHeaderStruc.ibhsFlags],ISCLIPPERMODFLAG	; see if clipper module
	je	p2cret			; no
	mov	gs,gs:[IOBuffHeaderStruc.ibhsClipModPtr]

	mov	CompressThisModule,ON	; flag compression on this module
	mov	CurrentClipperModPtr,gs	; update current clipper module pointer
	mov	ax,gs:[ClipperModEntStruc.cmsSegDefIndex]	; get SYMBOLS segment index, relative 0
	mov	ClipperSymSegIndex,ax	; save symbols segment index for LEDATA lookup

p2cret:
	ret
Pass2ClipperCheck	ENDP

;*****************************
;* FIXUPCLIPPERTOKENS        *
;*****************************

; fixup tokens in Clipper code
; upon entry cx == count of data bytes, fs:[si] -> first data byte
; es:di -> individual segdef entry.
; destroys dx

FixupClipperTokens	PROC
	push	eax			; save critical registers
	push	bx
	push	cx
	push	si
	push	fs
	push	gs
	push	di

	mov	gs,WORD PTR es:[di+IndSegDefRecStruc.isdrModulePtr]	; gs -> current buffer base
	mov	ax,gs:[IOBuffHeaderStruc.ibhsClipModPtr]
	mov	CurrentClipperModPtr,ax	; update current clipper module pointer

	mov	bx,OFFSET DGROUP:Clipper5SymbolTable
	xor	al,al
	cmp	OldParseProc,al	; see if parsing new procedure
	jne	fctscanloop		; no
	inc	al
	mov	OldParseProc,al	; set flag to bypass header next time

	mov	dx,cx
	mov	cx,16h
	call	ScanAhead	; adjust for header at start of p-code
	mov	cx,dx
	sub	cx,16h

; main p-code scanning loop
fctscanloop:
	call	ReadByte	; force valid si value by reading buffer then backing up one
	dec	si
	mov	al,fs:[si]		; get data byte

; LeftToParse can hold the following values
; 	0==parsing a new token value
;	<10h==parsing an old token value, number of bytes left to parse
;	10h==parsing a length byte
;	21h==parsing symbol offset high byte
;	22h==parsing symbol offset low byte

	cmp	LeftToParse,0	; see if parsing new token
	jne	fctnotnew		; no

; parsing new token value
	cmp	al,60h			; see if end of code token
	je	fctended		; yes
	mov	ah,al			; save token value
	xlat				; look up length byte
	or	al,al			; zero if illegal token
	jne	fctvalidtoken	; valid token

; illegal token encountered
	xor	ch,ch
	mov	cl,ah			; bad value in cx
	pop	di				; es:di -> segdef entry
	mov	gs,WORD PTR es:[di+IndSegDefRecStruc.isdrModulePtr]	; gs -> current buffer base
	lgs	si,gs:[IOBuffHeaderStruc.ibhsFileNamePtr]	; gs:si -> file name
	mov	dx,OFFSET DGROUP:CompBuffSource	; string to printer after transfer
	push	ds
	pop	es
	mov	di,dx			; es:di -> string destination

errloop:
	lods	BYTE PTR gs:[si]
	stosb
	or	al,al			; see if null terminator transferred yet
	jne	errloop			; no
	mov	al,BADSYMBOLTOKENERRORCODE
	call	LinkerErrorExit	; no return

fctended:
	xor	al,al
	mov	LeftToParse,al	; reset parsing flag for next module
	mov	ParseLength,al	; zero parse length byte
	mov	OldParseProc,al	; reset parsing old procedure flag
	jmp	NEAR PTR fctdone

fctvalidtoken:
	cmp	ah,1			; see if ASCIIZ string opcode (ignore length lookup)
	jne	fctnotasciiz	; no
	mov	LeftToParse,11h	; flag parsing a length byte next, adjust 1 for autodecrement
	jmp	SHORT fctnextbyte

fctnotasciiz:
	mov	LeftToParse,al	; save length of token to parse

; check if token has symbol that requires fixing up
	push	cx			; save critical register
	mov	cx,17			; seventeen possibilities for Clipper 5
	mov	di,OFFSET DGROUP:Clipper5TokenList	; di -> list of tokens that require fixup

fctcomploop:
	cmp	ah,ds:[di]		; see if a match
	je	fcttokmatch		; yes
	inc	di				; try next
	dec	cx
	jne	fctcomploop
	pop	cx				; restore critical register
	jmp	SHORT fctnextbyte

fcttokmatch:
	mov	LeftToParse,23h	; flag parsing a symbol offset next, adjust 1 for autodecrement
	pop	cx				; restore critical register
	jmp	SHORT fctnextbyte

; check if parsing asciiz string content (ParseLength!=0, ParseLength && LeftToParse==content length)
; if so, decrement ParseLength
fctnotnew:
	cmp	ParseLength,0
	je	fctchklen
	dec	ParseLength
	jmp	SHORT fctnextbyte

; check if parsing asciiz string length byte (ParseLength==0, LeftToParse==10h)
; if so, set ParseLength, LeftToParse bytes
fctchklen:
	cmp	LeftToParse,10h
	jne	fctchkofflow
	inc	ax				; adjust for null terminator
	mov	ParseLength,al
	inc	ax				; adjust for length byte
	mov	LeftToParse,al
	jmp	SHORT fctnextbyte

; check if parsing symbol offset low byte (ParseLength==0, LeftToParse==22h)
; if so, save value
fctchkofflow:
	cmp	LeftToParse,22h
	jne	fctchkoffhigh
	mov	BYTE PTR ParseSymbolOffset,al
	jmp	SHORT fctnextbyte

; check if parsing symbol offset high byte (ParseLength==0, LeftToParse==21h)
; if so, save value, else scanning past token arguments, ignore
fctchkoffhigh:
	cmp	LeftToParse,21h
	jne	fctnextbyte
	mov	BYTE PTR ParseSymbolOffset+1,al

; get next byte in token scanning
fctnextbyte:
	call	ReadByteDecCX	; read byte already processed, position to next
	dec	LeftToParse		; drop count of bytes left to parse
	cmp	ParseLength,0	; see if parsing asciiz
	jne	fctendchk		; yes
	mov	al,LeftToParse
	test	al,1fh		; get value with symbol offset flag masked
	jne	fctendchk		; more bytes associated with token, see if at end of data record

; end of bytes associate with token
	mov	ParseLength,0	; zero parse length byte
	mov	LeftToParse,0	; zero out LeftToParse
	and	al,20h			; see if scanned out symbol offset
	jne	fctsymfix		; yes

fctendchk:
	jcxz	fctdone		; no more bytes in data record
	jmp	NEAR PTR fctscanloop

; need to fix up this symbol
fctsymfix:
	mov	gs,CurrentClipperModPtr
	mov	di,ParseSymbolOffset	; get symbol offst
	shl	di,2			; make dword offset
	add	di,ParseSymbolOffset	; make 5-byte offset
	add	di,SIZE ClipperModEntStruc	; adjust past system bytes

; gs:[di]-> symbol table count for this symbol

fctsymupd:
	mov	ax,gs:[di]		; get new symbol number

	cmp	si,IOBUFFSYSVARSIZE+2	; see if i/o buffer wrap
	jae	fctnowrap		; no

	mov	gs,fs:[IOBuffHeaderStruc.ibhsParentPtr]	; gs -> previous i/o block
	mov	di,SIZEIOBUFFBLK
	dec	di				; di -> last valid byte in previous i/o block

	cmp	si,IOBUFFSYSVARSIZE+1	; see if buffer straddle
	je	fctstraddle		; yes

; update at old buffer end
	dec	di				; di -> second to last valid byte
	mov	gs:[di],ax
	jmp	SHORT fctendchk

; straddle buffer
fctstraddle:
	mov	gs:[di],al
	mov	fs:[si-1],ah
	jmp	SHORT fctendchk

fctnowrap:
	mov	fs:[si-2],ax	; overwrite old symbol number
	jmp	SHORT fctendchk

fctdone:
	pop	di				; restore critical registers
	pop	gs
	pop	fs
	pop	si
	pop	cx
	pop	bx
	pop	eax
	ret
FixupClipperTokens	ENDP

;*****************************
;* PROCESSSYMBOLTABLE        *
;*****************************

; process the symbol table data
; upon entry cx == count of data bytes, fs:[si] -> first data byte
; es:di -> individual segdef entry.
; (e)ax == LEDATA data offset
; modifies cx to == new count of data bytes after compression
; modifies (e)ax == new data offset to reflect compression
; destroys dx

ProcessSymbolTable	PROC
	push	bx			; save critical registers
	push	si
	push	di
	push	fs
	push	gs

	mov	bx,ax			; get LEDATA data offset
	shr	bx,2			; bx holds symbol number within module (dword offset)
	mov	dx,bx
	shr	dx,2
	add	bx,dx			; make 5-byte offset
	add	bx,SIZE ClipperModEntStruc	; adjust past system bytes
	mov	dx,cx			; dx holds count of bytes after compression removal

	mov	gs,WORD PTR es:[di+IndSegDefRecStruc.isdrModulePtr]	; gs -> current buffer base
	mov	ax,gs:[IOBuffHeaderStruc.ibhsClipModPtr]
	mov	CurrentClipperModPtr,ax	; update current clipper module pointer

pstcomploop:
	mov	gs,CurrentClipperModPtr	; gs:[bx] -> symbol table entry
	test	BYTE PTR gs:[bx+4],20h	; see if compressed symbol
	je	pstnocomp		; no

; this symbol was compressed, remove its entry from LEDATA data
	push	si			; save critical registers
	push	cx
	push	es
	push	ds

	push	fs
	push	fs
	pop	ds				; ds, es -> i/o buffer
	pop	es

	mov	di,si			; di -> destination (overwrite this symbol in LEDATA)
	add	si,16			; si -> source
	jc	pstof2			; overflow
	cmp	si,SIZEIOBUFFBLK	; see if past buffer
	jb	psttransfer		; no

; buffer wrap/overflow
pstof2:
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)
	mov	ds,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	jmp	SHORT psttransfer

pstdioverf:
	mov	di,IOBUFFSYSVARSIZE
	mov	es,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	jmp	SHORT psttrans1

pstsioverf:
	mov	si,IOBUFFSYSVARSIZE
	mov	ds,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	jmp	SHORT psttrans2

psttransfer:
	sub	cx,16			; move all remaining bytes between symbol entry and end
	je	psttransdone	; no bytes to transfer
	mov	ax,cx			; check for buffer overflow (slow transfer code)
	add	ax,di
	jc	pstbuffover		; buffer overflow will occur
	cmp	ax,SIZEIOBUFFBLK
	jae	pstbuffover		; buffer overflow will occur
	mov	ax,cx
	add	ax,si
	jc	pstbuffover		; buffer overflow will occur
	cmp	ax,SIZEIOBUFFBLK
	jae	pstbuffover		; buffer overflow will occur

; buffer will not overflow, do quick transfer code
	shr	cx,2			; convert byte to dword count
	rep	movsd
	jmp	SHORT psttransdone

pstbuffover:
	mov	ax,SIZEIOBUFFBLK	; ax holds constant, buffer end

psttransloop:
	cmp	di,ax			; see if destination overflow
	jae	pstdioverf		; yes

psttrans1:
	cmp	si,ax			; see if source overflow
	jae	pstsioverf		; yes

psttrans2:
	movsb				; transfer a char
	loop	psttransloop	; loop through all chars to transfer

psttransdone:
	pop	ds				; restore critical registers
	pop	es
	pop	cx
	pop	si
	mov	ax,16			; use as constant
	sub	dx,ax			; drop count of bytes in LEDATA data
	sub	si,ax			; adjust si for transfer

	cmp	si,IOBUFFSYSVARSIZE	; check for buffer wrap
	jae	pstnocomp		; no wrap
	add	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; adjust for buffer wrap
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsParentPtr]	; previous block in chain

pstnocomp:
	add	bx,5			; move to next symbol table entry, if any
	mov	ax,16			; use as constant
	sub	cx,ax			; drop count of data bytes
	add	si,ax			; point to next entry
	jc	pstof1			; overflow
	cmp	si,SIZEIOBUFFBLK	; see if past buffer
	jb	pstdonechk		; no

; buffer wrap/overflow
pstof1:
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain

pstdonechk:
	or	cx,cx
	jne	pstcomploop		; more symbols to check

	mov	ax,NewLEDATAOffset	; get new LEDATA offset after previous compression
	add	NewLEDATAOffset,dx	; save new next LEDATA offset after compression
	mov	cx,dx			; get count of data bytes after compression

	pop	gs				; restore critical registers
	pop	fs
	pop	di
	pop	si
	pop	bx
	ret
ProcessSymbolTable	ENDP

;*****************************
;* CHECKCOMPRESSEDSYMBOL     *
;*****************************

; performing fixup of compressed symbols, see if should be allowed, ignore if not
; otherwise return adjustment to relocation entry for previous compressed symbols
; upon entry DataRecordOffset holds position of fixup within LEDATA image,
;  LogicalDataRecOff holds offset of LEDATA data record
; returns carry flag set if discard fixup, reset otherwise
; destroys ax,bx

CheckCompressedSymbol	PROC
	push	cx			; save critical register
	push	dx
	push	es
	mov	bx,DataRecordOffset
	add	bx,WORD PTR LogicalDataRecOff	; known <64K
	and	bl,0f0h			; mask off low bits
	shr	bx,2			; bx contains symbol number of fixup, dword offset
	mov	cx,bx
	shr	cx,2
	add	bx,cx			; make 5-byte offset
	add	bx,SIZE ClipperModEntStruc	; adjust past system bytes

	mov	es,CurrentClipperModPtr	; es:[bx] -> symbol table count/offset
	test	BYTE PTR es:[bx+4],20h	; see if compressed symbol
	je	cfnocomp		; no

; compressed symbol, trash fixup
cfcomp:
	stc					; set carry flag to indicate discard fixup
	jmp	SHORT cfret

; noncompressed symbol
; find relocation adjustment for this fixup by looking through all previous entries for compression
cfnocomp:
	mov	RelocationAdjustment,0
	mov	dx,bx			; save end (current) entry
	mov	bx,SIZE ClipperModEntStruc	; bx -> first entry

cfloop:
	cmp	bx,dx			; see if at end
	jae	cfend			; yes
	test	BYTE PTR es:[bx+4],20h	; see if compressed symbol
	je	cfnext			; no
	add	RelocationAdjustment,10h

cfnext:
	add	bx,5			; move to next entry
	jmp	SHORT cfloop

cfend:
	clc

cfret:
	pop	es				; restore critical register
	pop	dx
	pop	cx
	ret
CheckCompressedSymbol	ENDP

ENDS

ENDIF

END
