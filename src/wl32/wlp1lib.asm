;*********************************************************************
;*   WLP1LIB.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          04/18/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   pass 1 library processing routines                              *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLP1LIB
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
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
PUBLIC	LIBPass1

; public for debugger
PUBLIC	GetModulePages

; variables
PUBLIC	CurrentLIB
PUBLIC	FirstLIBModCount

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

FirstPassFlag	DB	?	; flags first pass of libraries in processing
LIBOpenFlag	DB	?		; nonzero if library open

CurrentHighLIB	DW	?	; count of current high water library
CurrentLIB	DW	?		; count of current library
LIBProcessingFlag	DW	?	; library processing flag
ModToProcCount	DW	?	; count of library modules to process on pass
PageCount	DW	?		; count of unique module pages in library

PreAllLibPubSymCount	DD	?	; count of public symbols prior to all library passes
PreAllLibResSymCount	DD	?	; count of resolved symbols prior to all library passes
PreThisLibPubSymCount	DD	?	; count of public symbols prior to current library passes
PreThisLibResSymCount	DD	?	; count of resolved symbols prior to current library passes

ENDS

;*****************************
;* Constant data             *
;*****************************

CONST	SEGMENT WORD PUBLIC USE16 'DATA'

ENDS

;*****************************
;* Initialized data          *
;*****************************

_DATA	SEGMENT WORD PUBLIC USE16 'DATA'

FirstLIBModCount	DD	-1	; count of first module used by library

ENDS

;*****************************
;* External data             *
;*****************************

LIBHeaderStruc	STRUC
	lhsType		DB	?	; should always be 0F0h or MSLHED
	lhsRecLen	DW	?	; record length
	lhsDictOff	DD	?	; dictionary offset in file
	lhsDictSize	DW	?	; dictionary size in 512-byte blocks
	lhsFlags	DB	?	; library flags
LIBHeaderStruc	ENDS

EXTRN	LIBHeader:LIBHeaderStruc
EXTRN	BaseLIBDictBuff:WORD
EXTRN	CurrentBAKPATCount:WORD
EXTRN	CurrentBaseOBJBuff:WORD
EXTRN	CurrentFileHandle:WORD
EXTRN	CurrentFileName:BYTE
EXTRN	CurrentLNAMESIndex:WORD
EXTRN	CurrentSegDefCount:WORD
EXTRN	CurrentGrpDefCount:WORD
EXTRN	CurrentFixSymCount:WORD
EXTRN	CurrentLEDATACount:DWORD
EXTRN	EndOfOBJFlag:BYTE
EXTRN	FlatModuleFlag:BYTE
EXTRN	IsLocalSymbol:BYTE
EXTRN	LIBDictTablePtr:WORD
EXTRN	LIBFlagSelector:WORD
EXTRN	LIBNameOffset:WORD
EXTRN	LIBNameSelector:WORD
EXTRN	ModuleCount:DWORD
EXTRN	PharLapModuleFlag:BYTE
EXTRN	SearchExistSymFlag:BYTE

IFDEF SYMBOLPACK
EXTRN	CurrentModSymCount:WORD
EXTRN	IsCompressedSegment:BYTE
EXTRN	KnownClipperMod:BYTE
EXTRN	MayBeClipperMod:BYTE
EXTRN	NeedCompSymProcess:BYTE
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

EXTRN	AllocateMemory:PROC,ResizeMemory:PROC
EXTRN	DisplayReadFileFeedback:PROC
EXTRN	GetPubSymEntry:PROC
EXTRN	LoadLIBModule:PROC
EXTRN	OpenCurrentLIB:PROC
EXTRN	OpenFile:PROC
EXTRN	Process1OBJRecord:PROC
EXTRN	ProcessBAKPATRecords:PROC

;*****************************
;* Code routines             *
;*****************************

;*****************************
;* LIBPASS1                  *
;*****************************

; pass 1 processing of library files

LIBPass1	PROC
	cmp	TotalLIBCount,0	; see if any library files to process
	je	lpret			; no

; see if any unresolved symbols to check for
	mov	eax,PublicSymbolCount
	cmp	eax,ResolvedSymbolCount	; see if unresolved symbols
	je	lpret			; no

	mov	eax,ModuleCount
	mov	FirstLIBModCount,eax	; save first module count used by library (for error messages)
	mov	CurrentLIB,1	; init current library count
	mov	CurrentHighLIB,0	; init current high water library count (for /ls algo)
	mov	ProcessingLIBFlag,ON	; set processing library flag
	mov	FirstPassFlag,ON	; turn on first pass flag
	mov	PreAllLibPubSymCount,eax	; save total symbol count prior to all library passes
	mov	eax,ResolvedSymbolCount	; get resolved symbols
	mov	PreAllLibResSymCount,eax	; save resolved symbol count prior to all library passes

lpinit:
	call	OpenCurrentLIB
	call	DisplayReadFileFeedback
	call	GetModulePages	; get pages of modules in dictionary
	call	SortModulePages	; sort pages of library
	call	CompModuleOffsets	; compute library module offsets
	call	CompModuleSizes	; compute library module sizes
	call	XlateSymPageEnt	; translate library module symbol page entries to lookup offset
	mov	LIBOpenFlag,ON	; flag library file open
	jmp	SHORT lplibloop	; don't need to reopen library

lpallloop:
	mov	eax,PublicSymbolCount
	mov	PreAllLibPubSymCount,eax	; save total symbol count prior to all library passes
	mov	eax,ResolvedSymbolCount	; get resolved symbols
	mov	PreAllLibResSymCount,eax	; save resolved symbol count prior to all library passes

lplibloop:
	mov	eax,PublicSymbolCount
	mov	PreThisLibPubSymCount,eax	; save total symbol count prior to this library passes
	mov	eax,ResolvedSymbolCount	; get resolved symbols
	mov	PreThisLibResSymCount,eax	; save resolved symbol count prior to this library passes

	mov	bx,CurrentLIB
	dec	bx				; make relative zero
	add	bx,bx			; convert to word offset
	mov	fs,LIBDictTablePtr	; fs:bx -> entry in table of memory block selectors
	mov	ax,fs:[bx]
	mov	BaseLIBDictBuff,ax	; update current library base dictionary buffer pointer

; get special library processing flags
	mov	fs,LIBFlagSelector
	mov	ax,fs:[bx]
	mov	LIBProcessingFlag,ax

	mov	IsLocalSymbol,OFF	; searching for nonlocal symbols
	call	ScanDictForMods	; scan library dictionary for modules containing symbols which resolve externals
	mov	cx,ModToProcCount	; modules to process in cx
	cmp	LIBOpenFlag,OFF	; see if library open or needs to be opened
	jne	lpchkmod		; already open

; setup current file name even if won't be opened and modules used
	mov	di,OFFSET DGROUP:CurrentFileName	; es:di -> file name storage
	mov	si,LIBNameOffset
	push	ds			; save ds -> wl32 data
	pop	es				; ensure es -> wl32 data
	push	ds			; back to stack
	mov	ds,LIBNameSelector	; ds:si -> saved library name

lpnameloop:
	movsb
	cmp	BYTE PTR ds:[si-1],0
	jne	lpnameloop		; transfer to null terminator

	pop	ds				; restore ds -> wl32 data
	mov	LIBNameOffset,si	; update library module name offset for next read

lpchkmod:
	or	cx,cx			; see if modules to proces this pass
	jne	lpismod			; yes

; no modules to process this pass
here1:
	cmp	IsLIBSearchOption,OFF
	je	lpnextlib		; /ls option off, try next library
	jmp	NEAR PTR lpsearch

lpismod:
	mov	PageCount,cx	; modules to process count in PageCount for sorting routine
	call	SortModulePages	; sort pages of library to process
	cmp	LIBOpenFlag,OFF	; see if library open or needs to be opened
	jne	lpproc			; open

	call	DisplayReadFileFeedback
	mov	dx,OFFSET DGROUP:CurrentFileName
	mov	al,40h			; read-only, deny none access
	call	OpenFile	; open library file, ax == handle
	mov	CurrentFileHandle,ax	; save handle of file being read
	mov	LIBOpenFlag,ON	; flag library file open

lpproc:
	mov	cx,ModToProcCount	; modules to process back in cx
	mov	fs,BaseLIBDictBuff	; fs -> library dictionary
	or	fs:[LIBDictHeaderStruc.ldhsFlags],MODULEUSEDFLAG	; flag modules used from library
	mov	fs,fs:[LIBDictHeaderStruc.ldhsPagePtr]	; fs -> page storage
	xor	si,si			; si offsets into page storage

lpmodloop:
	lods	WORD PTR fs:[si]	; get library module page
	push	cx			; save modules to processed
	push	si
	push	fs			; save fs:si -> module page
	call	LoadLIBModule	; load the library module in ax
	push	ds
	pop	es				; es -> wl32 data
	call	Process1LIBModule
	pop	fs
	pop	si
	pop	cx
	loop	lpmodloop	; loop through all modules to processed

; see if unresolved symbols exist
	mov	eax,PublicSymbolCount
	cmp	eax,ResolvedSymbolCount	; see if unresolved symbols
	je	lpcloseret	; no, close open library file and return

; see if using library search algorithm
	cmp	IsLIBSearchOption,OFF	; check if using alternate algo
	jne	lpsearch		; yes

; see if symbols resolved or added this pass
	mov	eax,PublicSymbolCount
	cmp	PreThisLibPubSymCount,eax	; check new public symbols count
	jne	lplibloop		; new symbols exist
	mov	eax,ResolvedSymbolCount	; get resolved symbols
	cmp	PreThisLibResSymCount,eax	; check for new resolved symbols
	jne	lplibloop		; new resolved symbols exist

lpnextlib:
	cmp	LIBOpenFlag,OFF	; see if library needs to be closed
	je	lpinclib		; no, already closed
	mov	bx,CurrentFileHandle	; close previous library
	mov	ah,3eh
	int	21h
	mov	LIBOpenFlag,OFF	; flag library file off

lpinclib:
	inc	CurrentLIB		; bump to next lib
	mov	ax,CurrentLIB
	cmp	ax,CurrentHighLIB	; see if above high water mark
	jbe	lpinc2			; no
	mov	CurrentHighLIB,ax

lpinc2:
	cmp	ax,TotalLIBCount	; see if next lib exists
	ja	lpchange		; no

; process next library, if first pass must do dictionary setup
	cmp	FirstPassFlag,OFF	; see if first pass
	je	lplibloop		; no
	jmp	NEAR PTR lpinit	; yes, first pass, do next library initializations

; all libraries processed at least once, see if any changes in symbols overall
; if so, do another pass on them, if more than one library
lpchange:
	mov	FirstPassFlag,OFF	; turn off first pass flag
	cmp	TotalLIBCount,1	; see if only one library
	je	lpret			; yes, don't do needless pass
	mov	LIBNameOffset,0	; reset name pointer
	mov	CurrentLIB,1	; reset current library

; see if symbols resolved or added this pass
	mov	eax,PublicSymbolCount
	cmp	PreAllLibPubSymCount,eax	; check new public symbols count
	jne	lpallloop		; new symbols exist
	mov	eax,ResolvedSymbolCount	; get resolved symbols
	cmp	PreAllLibResSymCount,eax	; check for new resolved symbols
	jne	lpallloop		; new resolved symbols exist

lpret:
	ret

; close open file and return
lpcloseret:
	mov	bx,CurrentFileHandle	; close previous library
	mov	ah,3eh
	int	21h
	ret

; using alternate library search algorithm
; see if symbols resolved or added this pass
lpsearch:
	cmp	LIBOpenFlag,OFF	; see if previous library needs to be closed
	je	lpsearch2		; no, already closed
	mov	bx,CurrentFileHandle	; close previous library
	mov	ah,3eh
	int	21h
	mov	LIBOpenFlag,OFF	; flag library file close

lpsearch2:
	mov	eax,PublicSymbolCount
	cmp	PreThisLibPubSymCount,eax	; check new public symbols count
	jne	lprepass		; new symbols exist
	mov	eax,ResolvedSymbolCount	; get resolved symbols
	cmp	PreThisLibResSymCount,eax	; check for new resolved symbols
	jne	lprepass		; new symbols

; no new symbols, when incrementing library see if need to open and do setup
	mov	FirstPassFlag,OFF	; pre-set flag to avoid open and setup
	mov	ax,CurrentLIB
	cmp	ax,CurrentHighLIB
	jb	lpinclib			; no need to re-open library
	cmp	ax,TotalLIBCount	; see if maximum library
	jae	lpinclib			; yes, no need to re-open library
	mov	FirstPassFlag,ON	; flag for open and setup
	jmp	NEAR PTR lpinclib

; this library added or resolved new symbols, restart library pass from beginning
lprepass:
	mov	LIBNameOffset,0	; reset name pointer
	cmp	CurrentLIB,1	; see if on first library
	je	lplibloop		; yes
	cmp	TotalLIBCount,1	; see if only one library
	je	lplibloop		; yes
	mov	CurrentLIB,1	; reset current library
	jmp	NEAR PTR lplibloop

LIBPass1	ENDP

;*****************************
;* ScanDictForMods           *
;*****************************

; scan library dictionary for modules containing symbols which resolve externals
; keep them in page storage, track modules to process count
; destroys all registers except ds

ScanDictForMods	PROC
	mov	SearchExistSymFlag,ON	; search for existing symbol only flag set
	mov	fs,BaseLIBDictBuff	; fs -> library dictionary
	mov	es,fs:[LIBDictHeaderStruc.ldhsPagePtr]	; es -> page storage for library
	xor	dx,dx			; dx holds processed page count
	mov	bx,dx			; bx will offset within page
	mov	ModToProcCount,dx	; init count of modules to process

sdfreset:
	mov	si,200h			; si offset into buckets

; fs:si -> 37-byte bucket table
sdfbuckloop:
	mov	ax,si			; get page/bucket, ignore page in ah
	cmp	al,37			; see if past final bucket of page
	jae	sdfnextpage		; yes
	lods	BYTE PTR fs:[si]	; get bucket value
	xor	ah,ah			; zap high byte
	add	ax,ax			; ax -> symbol entry on page
	je	sdfbuckloop		; not used
	mov	bx,ax			; bx -> symbol entry relative page
	mov	ax,si
	and	ax,0fe00h
	add	bx,ax			; bx -> symbol entry relative block

; fs:bx -> symbol
	xchg	bx,si		; bx -> bucket, si -> symbol
	mov	bp,si			; save bp -> symbol name
	lods	BYTE PTR fs:[si]	; get symbol length
	xor	ah,ah
	add	si,ax			; si -> module page following name
	lods	WORD PTR fs:[si]	; get symbol module page
	mov	si,bx			; restore si -> bucket

; ax holds module page, scan for it in previous entries
	xor	di,di			; es:di -> start of page storage
	mov	cx,ModToProcCount
	jcxz	sdfusechk	; no pages save, new page by default
	repne	scasw		; search for page

;@@@	je	sdfbuckloop		; found, don't save module
	je	sdfzerobuck		; found, zero out bucket as unnecessary

sdfusechk:
	test	LibProcessingFlag,USEALLMODULESFLAG	; see if using all modules
	jne	sdfres			; yes, automatically use module

; search for symbol if preexisting
; if search turns up new, then ignore
; if search turns up public or comdef, then zero out bucket, ignore
; if search turns up extdef, then keep page for future processing
sdfnewpage:
	push	si			; save critical registers
	push	ax
	push	bx
	push	dx
	mov	si,bp			; fs:si -> public symbol name
	call	GetPubSymEntry	; public symbol in gs:di, if exists
	pop	dx				; restore critical registers
	pop	bx
	pop	ax				; ax == library module
	pop	si
	jc	sdfbuckloop		; new symbol don't use
	test	gs:[di+PubSymRecStruc.pssFlags],(PUBLICSYMBOLFLAG OR NEARCOMSYMBOLFLAG OR FARCOMSYMBOLFLAG)
	je	sdfchkweak		; new resolution

; symbol already exists and is defined
sdfzerobuck:
	mov	BYTE PTR fs:[si-1],0	; zero bucket so not checked again
	jmp	SHORT sdfbuckloop

; extdef resolved by this module, save it if not weak extern
sdfchkweak:
	test	gs:[di+PubSymRecStruc.pssFlags],WEAKEXTERNFLAG
	jne	sdfbuckloop		; weak external, but might go to strong, don't use and don't zero bucket

; extdef resolved by this module, save module
sdfres:
	mov	di,ModToProcCount
	add	di,di			; es:di -> new page storage slot
	stosw				; save it
	inc	ModToProcCount		; bump count of pages
	mov	BYTE PTR fs:[si-1],0	; zero bucket so not checked again
	jmp	SHORT sdfbuckloop

sdfnextpage:
	and	si,0fe00h		; mask si down to start of dictionary page (512-byte block)
	add	si,200h			; si -> next page
	inc	dx				; bump count of dictionary pages checked
	cmp	dx,fs:[LIBDictHeaderStruc.ldhsDictSize]	; see if total # pages checked
	jae	sdfret			; yes

;	mov	al,dl
;	and	al,PAGECOUNTLIBDICTBUFFBLK
;	cmp	al,PAGECOUNTLIBDICTBUFFBLK	; see if need to move to next block
;	jne	sdfbuckloop		; no
	cmp	si,SIZELIBDICTBUFFBLK
	jb	sdfbuckloop

	mov	fs,fs:[LIBDictHeaderStruc.ldhsChildPtr]	; fs -> next dictionary block
	jmp	NEAR PTR sdfreset	; reset si bucket pointer

sdfret:
	ret
ScanDictForMods	ENDP

;*****************************
;* Process1LIBModule         *
;*****************************

; pass 1 processing library module

Process1LIBModule	PROC
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
	mov	fs,CurrentBaseOBJBuff
	mov	si,IOBUFFSYSVARSIZE	; fs:si -> module first memory block first byte (offset for system variables)

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

	call	ProcessBAKPATRecords

	inc	ModuleCount		; bump count of modules parsed
	ret
Process1LIBModule	ENDP

;*****************************
;* GETMODULEPAGES            *
;*****************************

; get pages of modules in dictionary
; destroys ax,bx,cx,dx,si,di,bp,es,fs

GetModulePages	PROC
	mov	dx,1024			; allocate for page storage 512 modules at a time
	call	AllocateMemory
	mov	fs,BaseLIBDictBuff	; fs -> library dictionary
	mov	fs:[LIBDictHeaderStruc.ldhsPagePtr],ax	; keep -> page storage for library
	mov	es,ax			; es -> page storage

; set upper limit of dictionary in bp
	mov	ax,WORD PTR LIBHeader.lhsDictOff
	mov	dx,WORD PTR LIBHeader.lhsDictOff+2
	mov	bp,LIBHeader.lhsReclen
	add	bp,3
	div	bp
	mov	bp,ax			; bp holds maxium module count

	xor	dx,dx			; dx holds processed page count
	mov	bx,dx			; bx will offset within page
	mov	PageCount,dx	; init page counter

gmpreset:
	mov	si,200h			; si offset into buckets

; fs:si -> 37-byte bucket table
gmpbuckloop:
	mov	ax,si			; get page/bucket, ignore page in ah
	cmp	al,37			; see if past final bucket of page
	jae	gmpnextpage		; yes
	lods	BYTE PTR fs:[si]	; get bucket value
	xor	ah,ah			; zap high byte
	add	ax,ax			; ax -> symbol entry on page
	je	gmpbuckloop		; not used
	mov	bx,ax			; bx -> symbol entry relative page
	mov	ax,si
	and	ax,0fe00h
	add	bx,ax			; bx -> symbol entry relative block

; fs:bx -> symbol
	xchg	bx,si		; bx -> bucket, si -> symbol
	lods	BYTE PTR fs:[si]	; get symbol length
	xor	ah,ah
	add	si,ax			; si -> module page following name
	lods	WORD PTR fs:[si]	; get symbol module page
	mov	si,bx			; restore si -> bucket

	or	ax,ax			; see if invalid page
	je	gmpflushmod		; yes
	cmp	ax,bp			; see if invalid page
	jae	gmpflushmod		; yes

; ax holds module page, scan for it in previous entries
	xor	di,di			; es:di -> start of page storage
	mov	cx,PageCount
	jcxz	gmpnewpage	; no pages save, new page by default
	repne	scasw		; search for page
	je	gmpbuckloop		; found, don't save module

gmpnewpage:
	mov	di,PageCount
	add	di,di			; es:di -> new page storage slot
	stosw				; save it
	inc	PageCount		; bump count of pages
	test	PageCount,511	; see if need to resize the memory for larger allocation
	jne	gmpbuckloop		; no

; resize allocation to larger block for more module pages
	push	dx			; save critical register
	mov	dx,PageCount
	add	dx,dx			; word per page
	add	dx,1024			; extend room for another 512 modules
	mov	ax,es			; ax==selector of block to resize
	call	ResizeMemory
	pop	dx				; restore critical register
	jmp	SHORT gmpbuckloop

; invalid module page
gmpflushmod:
	mov	BYTE PTR fs:[si-1],0
	jmp	SHORT gmpbuckloop

gmpnextpage:
	and	si,0fe00h		; mask si down to start of dictionary page (512-byte block)
	add	si,200h			; si -> next page
	inc	dx				; bump count of dictionary pages checked
	cmp	dx,LIBHeader.lhsDictSize	; see if total # pages checked
	jae	gmpret			; yes

;	mov	al,dl
;	and	al,PAGECOUNTLIBDICTBUFFBLK
;	cmp	al,PAGECOUNTLIBDICTBUFFBLK	; see if need to move to next block
;	jne	gmpbuckloop		; no
	cmp	si,SIZELIBDICTBUFFBLK
	jb	gmpbuckloop

	mov	fs,fs:[LIBDictHeaderStruc.ldhsChildPtr]	; fs -> next dictionary block
	jmp	NEAR PTR gmpreset	; reset si bucket pointer

gmpret:
	ret
GetModulePages	ENDP

;*****************************
;* SORTMODULEPAGES           *
;*****************************

; sort the library module pages in ascending order
; sleazy bubble sort fast enough for our purposes
; destroys ax,bx,cx,dx,si,fs

SortModulePages	PROC
	cmp	PageCount,1		; see if only one page to process
	jbe	smpret			; yes, sorted by definition

	mov	fs,BaseLIBDictBuff	; fs -> library dictionary
	mov	fs,fs:[LIBDictHeaderStruc.ldhsPagePtr]	; fs -> page storage for library
	mov	dh,1			; use as constant to turn on flag

smpouterloop:
	xor	si,si			; si offsets into page values
	mov	cx,PageCount
	dec	cx
	xor	dl,dl			; nonzero dl flags swap this pass

smpinnerloop:
	lods	WORD PTR fs:[si]	; get array element (page value)
	mov	bx,fs:[si]		; get next higher array element
	cmp	ax,bx			; compare lower element to higher
	ja	smpswap			; greater than, swap elements
	loop	smpinnerloop	; move to next array element

smpswaptest:
	or	dl,dl			; see if swap occurred this pass
	jne	smpouterloop	; yes

smpret:
	ret					; done

; swap the array elements
smpswap:
	mov	fs:[si],ax
	mov	fs:[si-2],bx
	mov	dl,dh			; flag swap occurred
	loop	smpinnerloop	; move to next array element
	jmp	SHORT smpswaptest	; do swap test, route accordingly

SortModulePages	ENDP

;*****************************
;* COMPMODULEOFFSETS         *
;*****************************

; compute library module offsets, save them

CompModuleOffsets	PROC
	mov	dx,PageCount
	mov	cx,dx			; keep count of pages
	shl	dx,2			; dword entry per module
	call	AllocateMemory	; allocate storage for module offsets
	mov	fs,BaseLIBDictBuff	; fs -> library dictionary
	mov	fs:[LIBDictHeaderStruc.ldhsPageOffPtr],ax	; keep -> page offset storage for library

	or	cx,cx
	je	cmoret			; no pages
	mov	es,ax
	xor	di,di			; es:di -> library module offsets
	mov	fs,fs:[LIBDictHeaderStruc.ldhsPagePtr]
	mov	si,di			; fs:si -> library module pages
	mov	bx,LIBHeader.lhsRecLen
	add	bx,3			; bx holds byte size of library pages, record length+3

cmoloop:
	lods	WORD PTR fs:[si]	; get page
	mul	bx				; multiply by page size, offset in dx:ax
	stosw				; save low word offset
	mov	ax,dx
	stosw				; save high word offset
	loop	cmoloop

cmoret:
	ret
CompModuleOffsets	ENDP

;*****************************
;* COMPMODULESIZES           *
;*****************************

; compute library module offsets, save them

CompModuleSizes	PROC
	mov	dx,PageCount
	mov	cx,dx			; keep count of pages
	dec	cx				; special case for last page
	shl	dx,2			; dword entry per module
	call	AllocateMemory	; allocate storage for module offsets
	mov	fs,BaseLIBDictBuff	; fs -> library dictionary
	mov	fs:[LIBDictHeaderStruc.ldhsModSizePtr],ax	; keep -> module size storage for library

	mov	es,ax
	xor	di,di			; es:di -> library module sizes
	mov	fs,fs:[LIBDictHeaderStruc.ldhsPageOffPtr]
	mov	si,di			; fs:si -> library module page offsets
	jcxz	cms2		; only one module in library

cmsloop:
	lods	DWORD PTR fs:[si]	; get page offset
	mov	ebx,fs:[si]		; get next higher page
	sub	ebx,eax			; ebx holds size of modules
	mov	eax,ebx
	stosd				; save size of module
	loop	cmsloop

; last module, size if dictionary offset minus page offset
cms2:
	lods	DWORD PTR fs:[si]	; get page offset
	mov	ebx,LIBHeader.lhsDictOff
	sub	ebx,eax			; ebx holds module size
	mov	es:[di],ebx		; save module size

	ret
CompModuleSizes	ENDP

;*****************************
;* XLATESYMPAGEENT           *
;*****************************

; translate library module symbol page entries to lookup offset

XlateSymPageEnt	PROC
	mov	fs,BaseLIBDictBuff	; fs -> library dictionary
	mov	es,fs:[LIBDictHeaderStruc.ldhsPagePtr]	; es -> page storage for library
	xor	dx,dx			; dx holds processed page count
	mov	bx,dx			; bx will offset within page

xspreset:
	mov	si,200h			; si offset into buckets

; fs:si -> 37-byte bucket table
xspbuckloop:
	mov	ax,si			; get page/bucket, ignore page in ah
	cmp	al,37			; see if past final bucket of page
	jae	xspnextpage		; yes
	lods	BYTE PTR fs:[si]	; get bucket value
	xor	ah,ah			; zap high byte
	add	ax,ax			; ax -> symbol entry on page
	je	xspbuckloop		; not used
	mov	bx,ax			; bx -> symbol entry relative page
	mov	ax,si
	and	ax,0fe00h
	add	bx,ax			; bx -> symbol entry relative block

; fs:bx -> symbol
	xchg	bx,si		; bx -> bucket, si -> symbol
	lods	BYTE PTR fs:[si]	; get symbol length
	xor	ah,ah
	add	si,ax			; si -> module page following name
	lods	WORD PTR fs:[si]	; get symbol module page
	xchg	si,bx		; restore si -> bucket, bx -> module page+2

; ax holds module page, scan for it in previous entries
	xor	di,di			; es:di -> start of page storage
	mov	cx,PageCount
	repne	scasw		; search for page, assume found

; PageCount-cx holds offset entry relative 1
	mov	ax,PageCount
	sub	ax,cx
	mov	fs:[bx-2],ax	; update symbol's page
;@@@	or	ax,ax
;@@@	je	crash
;@@@	cmp	ax,PageCount
;@@@	jae	crash
	jmp	SHORT xspbuckloop

;@@@crash:
;@@@	mov	ax,0ff01h
;@@@	mov	gs,ax
;@@@	jmp	SHORT xspbuckloop

xspnextpage:
	and	si,0fe00h		; mask si down to start of dictionary page (512-byte block)
	add	si,200h			; si -> next page
	inc	dx				; bump count of dictionary pages checked
	cmp	dx,LIBHeader.lhsDictSize	; see if total # pages checked
	jae	xspret			; yes

;	mov	al,dl
;	and	al,PAGECOUNTLIBDICTBUFFBLK
;	cmp	al,PAGECOUNTLIBDICTBUFFBLK	; see if need to move to next block
;	jne	xspbuckloop		; no
	cmp	si,SIZELIBDICTBUFFBLK
	jb	xspbuckloop

	mov	fs,fs:[LIBDictHeaderStruc.ldhsChildPtr]	; fs -> next dictionary block
	jmp	SHORT xspreset	; reset si bucket pointer

xspret:
	ret
XlateSymPageEnt	ENDP

ENDS

END
