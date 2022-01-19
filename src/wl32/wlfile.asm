;*********************************************************************
;*   WLFILE.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          11/06/95                                         *
;*   Model:         Small                                            *
;*   Version:       1.3d                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   linker file i/o routines                                        *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLPROG
PAGE    50,80

.MODEL  SMALL
.386C					; using 386-level code in this module
DGROUP	GROUP CONST,_BSS,_DATA

;*****************************
;* Equates                   *
;*****************************

WRITEBUFFERSIZE	EQU	10000h

;*****************************
;* Include files             *
;*****************************

INCLUDE WLEQUATE.INC
INCLUDE WLDATA.INC
INCLUDE WLERRCOD.INC

;*****************************
;* Public declarations       *
;*****************************

; routines
PUBLIC	LoadLIBModule
PUBLIC	OpenCurrentOBJ,OpenCurrentLIB
PUBLIC	OpenFile,CreateFile
PUBLIC	ReadFile,WriteFile
PUBLIC	SeekToEndOfFile
PUBLIC	WriteFileVarString
PUBLIC	WriteToIOBuffer,FlushIOBuffer

; public for debugger
PUBLIC	padexe,wtionewbase
PUBLIC	LowWaterMark,HighWaterMark
PUBLIC	AbsoluteWriteOff,CurrentWriteBase
PUBLIC	StartWriteAddress,EndWriteAddress

; variables
PUBLIC	BaseLIBDictBuff
PUBLIC	CurrentBaseOBJBuff
PUBLIC	CurrentFileHandle
PUBLIC	LIBDictTablePtr
PUBLIC	LIBHeader
PUBLIC	LIBNameOffset
EXTRN	OBJNameSelector:WORD,LIBNameSelector:WORD

PUBLIC	OBJBuffSelTablePtr

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'

AbsoluteWriteOff	DD	?	; absolute write offset
BaseLIBDictBuff	DW	?	; pointer to current library dictionary buffer base
CurrentBaseOBJBuff	DW	?	; pointer to current buffer base
CurrentFileHandle	DW	?	; current file handle of file being processed obj/lib
EndWriteAddress	DD	?	; last byte written+1 address relative buffer
FileAllocBlockParent	DW	?	; parent of allocated block for reading file
StartWriteAddress	DD	?	; write offset relative current write base

LIBHeaderStruc	STRUC
	lhsType		DB	?	; should always be 0F0h or MSLHED
	lhsRecLen	DW	?	; record length
	lhsDictOff	DD	?	; dictionary offset in file
	lhsDictSize	DW	?	; dictionary size in 512-byte blocks
	lhsFlags	DB	?	; library flags
LIBHeaderStruc	ENDS

LIBHeader	LIBHeaderStruc	<>

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

CurrentWriteBase	DD	0	; current write base
HighWaterMark	DD	0	; high water mark for i/o buffer
IOBuffByteCount	DW	0	; count of bytes in i/o buffer to write
LIBDictTablePtr	DW	0	; pointer to table of pointers to LIB file dictionary buffers
LIBNameOffset	DW	0	; library name offset in name block
LowWaterMark	DD	0	; low water mark for i/o buffer
OBJNameOffset	DW	0	; object name offset in name block
OBJBuffSelTablePtr	DW	0	; pointer to table of pointers to OBJ file i/o buffers

ENDs

;*****************************
;* External data             *
;*****************************

EXTRN	CurrentFileName:BYTE
EXTRN	CurrentOBJ:WORD,CurrentLIB:WORD
EXTRN	ExecutableFileHandle:WORD
EXTRN	ExecutableHeaderSize:DWORD
EXTRN	HighestOffsetWritten:DWORD
EXTRN	ModuleCount:DWORD
EXTRN	OBJNameSelector:WORD,LIBNameSelector:WORD
EXTRN	RelocEntryCount:DWORD

;*****************************
;* Code begins               *
;*****************************

_TEXT	SEGMENT WORD PUBLIC USE16 'CODE'

ASSUME	ds:_DATA,ds:_BSS,ds:CONST,cs:_TEXT,ds:DGROUP

;*****************************
;* External code routines    *
;*****************************

;*****************************
;* Code routines             *
;*****************************

EXTRN	AllocateMemory:PROC,ResizeMemory:PROC,ReleaseMemory:PROC
EXTRN	CheckExtension:PROC
EXTRN	DisplayReadFileFeedback:PROC
EXTRN	DosErrorExit:PROC,BadOBJModuleExit:PROC
EXTRN	LinkerErrorExit:PROC
EXTRN	Zero64KIOBlock:PROC

;*****************************
;* OPENFILE                  *
;*****************************

; open a file, upon entry dx -> file name, al holds open flags
; return ax == file handle

OpenFile	PROC
	mov	ah,3dh			; open file
	int	21h
	jnc	ofret

; error opening file, dx -> file name, al holds error code
	call	DOSErrorExit

ofret:
	ret
OpenFile	ENDP

;*****************************
;* CREATEFILE                *
;*****************************

; create file, truncate if exists
; upon entry dx -> file name
; return ax == file handle

CreateFile	PROC
	xor	cx,cx			; normal file attribute
	mov	ah,3ch			; create or truncate file
	int	21h
	jnc	cfret

; error creating file, dx -> file name, al holds error code
	call	DOSErrorExit

cfret:
	ret
CreateFile	ENDP

;*****************************
;* READFILE                  *
;*****************************

; read file, upon entry bx == file handle, cx==bytes to read,
;  ds:dx -> read buffer
; return ax == bytes read

ReadFile	PROC
	mov	ah,3fh			; read file
	int	21h
	jnc	rfret

; error read file, dx -> file name, al holds error code
	call	DOSErrorExit

rfret:
	ret
ReadFile	ENDP

;*****************************
;* WRITEFILE                 *
;*****************************

; write file, upon entry bx == file handle, cx==bytes to write,
;  ds:dx -> write buffer
; return ax == bytes written

WriteFile	PROC
	mov	ah,40h			; write file
	int	21h
	jnc	wfret

; error writing file, dx -> file name, al holds error code
	call	DOSErrorExit

wfret:
	ret
WriteFile	ENDP

;*****************************
;* SEEKTOENDOFFILE           *
;*****************************

; seek to end of file, upon entry bx == file handle
; return dx:ax == end of file position
; destroys ax,cx,dx

SeekToEndOfFile	PROC
	xor	cx,cx
	mov	dx,cx
	mov	ax,4202h		; move file pointer relative end of file
	int	21h
	ret
SeekToEndOfFile	ENDP

;*****************************
;* WRITEFILEVARSTRING        *
;*****************************

; write variable null-terminated string to file
; upon entry bx -> write buffer, dx==file handle,
; return ax == bytes written
; returns bx==file handle, dx -> write buffer, cx -> bytes to write, ax==bytes written

WriteFileVarString	PROC
	xor	cx,cx			; init count of chars in string
	mov	ax,bx			; save -> start of string

wfvloop:
	cmp	BYTE PTR [bx],0	; see if at end of string
	je	wfv2
	inc	bx				; bump string position
	inc	cx				; bump count of chars in string
	jmp	SHORT wfvloop

wfv2:
	mov	bx,dx			; bx==file handle
	mov	dx,ax			; dx -> write buffer
	call	WriteFile
	ret
WriteFileVarString	ENDP

;*****************************
;* OPENCURRENTOBJ            *
;*****************************

; open current object module, read into memory

OpenCurrentOBJ	PROC
	push	es			; critical register modified in routine
	mov	di,OFFSET DGROUP:CurrentFileName	; es:di -> file name storage
	mov	si,OBJNameOffset
	push	si			; save -> original obj name offset
	push	ds			; save ds -> wl32 data
	mov	ds,OBJNameSelector	; ds:si -> saved object name

ocoloop:
	movsb
	cmp	BYTE PTR ds:[si-1],0
	jne	ocoloop			; transfer to null terminator

	pop	ds				; restore ds -> wl32 data
	mov	OBJNameOffset,si	; update object module name offset for next read

	mov	dx,OFFSET DGROUP:CurrentFileName
	mov	al,40h			; read-only, deny none access
	call	OpenFile	; open object file, ax == handle
	mov	CurrentFileHandle,ax	; save handle of file being read
	call	DisplayReadFileFeedback

; see if need to allocate table to hold pointer to OBJ buffers
	cmp	OBJBuffSelTablePtr,0
	jne	oco2			; no need to allocate table
	mov	dx,TotalOBJCount
	add	dx,dx			; one word (selector) per object module
	call	AllocateMemory	; allocate memory for pointers
	mov	OBJBuffSelTablePtr,ax	; save selector value

; allocate SIZEIOBUFFBLK block for reading in file 
; first IOBUFFSYSVARVSIZE bytes are for system variables,
;  so only read SIZEIOBUFFBLK-IOBUFFSYSVARSIZE at offset IOBUFFSYSVARSIZE
; original obj name offset still on stack
oco2:
	mov	dx,SIZEIOBUFFBLK
	call	AllocateMemory
	mov	bx,CurrentOBJ
	dec	bx				; make relative zero
	add	bx,bx			; convert to word offset
	mov	es,OBJBuffSelTablePtr	; es:bx -> entry in table of memory block selectors
	mov	es:[bx],ax	; keep pointer to first SIZEIOBUFFBLK buffer for file
	mov	CurrentBaseOBJBuff,ax	; save pointer to current buffer base
	mov	FileAllocBlockParent,0	; init parent of first block to zero

; read SIZEIOBUFFBLK-IOBUFFSYSVARSIZE chunk of file,
;  ax holds selector of buffer
ocoread:
	mov	bx,CurrentFileHandle
	mov	cx,SIZEIOBUFFBLK-IOBUFFSYSVARSIZE
	mov	dx,IOBUFFSYSVARSIZE	; offset from system variables
	push	ds
	mov	ds,ax			; ds -> allocated memory buffer
	call	ReadFile	; actual number of bytes read returned in ax
	mov	ds:[IOBuffHeaderStruc.ibhsBlockSize],ax	; save non-sysvar size of block to sysvar space
	xor	si,si			; use si to flag <SIZEIOBUFFBLK-IOBUFFSYSVARSIZE buffer read
	cmp	ax,SIZEIOBUFFBLK-IOBUFFSYSVARSIZE	; see if filled up buffer
	je	oco3			; yes
	inc	si				; flag read to end of file (<SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)

; make sure that any bytes were read (otherwise release selector)
	or	ax,ax			; check byte count
	jne	ocosize			; nonzero

; zero bytes read
	mov	ax,ds
	call	ReleaseMemory
	pop	ds				; restore ds -> wl32 data
	mov	ax,FileAllocBlockParent	; ax -> parent (last) block
	or	ax,ax			; make sure not a null file
	je	poor4			; null file
	mov	es,FileAllocBlockParent	; es -> parent (last) block
	or	WORD PTR es:[IOBuffHeaderStruc.ibhsFlags],FINALBLOCKFLAG	; flag final block
	jmp	SHORT ocoeof	; after end of file processing

poor4:
	mov	cl,4
	call	BadOBJModuleExit

; read to file end before SIZEIOBUFFBLK, resize buffer back down to read in bytes+system
ocosize:
	mov	dx,ax			; file bytes to size to
	add	dx,IOBUFFSYSVARSIZE	; add in system bytes
	mov	ax,ds			; ax == selector
	call	ResizeMemory

oco3:
	mov	dx,ds			; save selector value
	pop	ds
	mov	es,dx
	xor	di,di			; es:di -> allocated block
	movzx	eax,di
	mov	cx,IOBUFFSYSVARSIZE/4
	rep	stosd			; zero init first bytes for variable storage
	cmp	FileAllocBlockParent,0	; see if need to chain i/o buffer back to parent
	je	oco4			; no
	mov	ax,FileAllocBlockParent
	mov	es:[IOBuffHeaderStruc.ibhsParentPtr],ax	; child (current) -> parent
	mov	es,ax
	mov	es:[IOBuffHeaderStruc.ibhsChildPtr],dx	; parent -> child (current)
	mov	ax,es:[IOBuffHeaderStruc.ibhsBlockID]	; get parent block ID
	inc	ax
	mov	es,dx			; es -> child block
	mov	es:[IOBuffHeaderStruc.ibhsBlockID],ax	; child block ID is parent's plus one

oco4:
	mov	FileAllocBlockParent,dx	; update parent pointer for next block
	or	si,si			; see if partial read
	jne	ocoendflag		; yes, end of file, set flag

; allocate another block and reloop for read
	mov	dx,SIZEIOBUFFBLK
	call	AllocateMemory
	jmp	NEAR PTR ocoread

ocoendflag:
	mov	es,FileAllocBlockParent	; es -> parent (last) block
	or	WORD PTR es:[IOBuffHeaderStruc.ibhsFlags],FINALBLOCKFLAG	; flag final block

; done reading in file, close it now
ocoeof:
	mov	bx,CurrentFileHandle
	mov	ah,3eh
	int	21h

; save pointer to file name in first buffer
; original obj name offset still on stack
	mov	es,CurrentBaseOBJBuff	; es -> first buffer block of module
	pop	bx				; bx -> original obj name offset
	mov	es:[WORD PTR IOBuffHeaderStruc.ibhsFileNamePtr],bx
	mov	bx,OBJNameSelector
	mov	es:[WORD PTR IOBuffHeaderStruc.ibhsFileNamePtr+2],bx

	pop	es				; restore critical register
	ret
OpenCurrentOBJ	ENDP

;*****************************
;* LOADLIBMODULE             *
;*****************************

; load library module
; upon entry ax == lookup offset for library module

LoadLIBModule	PROC
	mov	si,ax			; si holds library offset
	dec	si				; make relative 0
	shl	si,2			; dword per entry

	mov	fs,BaseLIBDictBuff	; fs -> library dictionary
	mov	di,fs
	mov	fs,fs:[LIBDictHeaderStruc.ldhsPageOffPtr]	; fs -> page offset storage

; seek to library module
llmseek:
	mov	dx,fs:[si]
	mov	cx,fs:[si+2]
	mov	bx,CurrentFileHandle
	mov	ax,4200h		; move file pointer relative start of file
	int	21h

	mov	fs,di			; fs -> library dictionary
	mov	fs,fs:[LIBDictHeaderStruc.ldhsModSizePtr]	; fs -> module size storage
	mov	esi,fs:[si]		; esi==file size

; see if need to allocate table to hold pointers to library modules
	mov	fs,BaseLIBDictBuff	; fs -> library dictionary
	cmp	fs:[LIBDictHeaderStruc.ldhsModBuffPtr],0
	jne	llm2			; already allocated
	mov	dx,1024			; allocate in 1024-byte chunks (512 word entries)
	call	AllocateMemory	; allocate memory for pointers
	mov	fs:[LIBDictHeaderStruc.ldhsModBuffPtr],ax	; save selector value

llm2:
	inc	fs:[LIBDictHeaderStruc.ldhsModUsedCount]	; bump count of used modules
	mov	ax,fs:[LIBDictHeaderStruc.ldhsModUsedCount]	; get module used count
	test	ax,511		; see if need to resize allocation
	jne	llm3			; no
	mov	dx,ax
	mov	ax,fs:[LIBDictHeaderStruc.ldhsModBuffPtr]
	add	dx,dx			; word per entry
	add	dx,1024			; extend room for another 512 modules
	call	ResizeMemory

llm3:
	cmp	fs:[LIBDictHeaderStruc.ldhsModIDPtr],0
	jne	llmid			; already allocated
	mov	dx,2048			; allocate in 2048-byte chunks (512 dword entries)
	call	AllocateMemory	; allocate memory for pointers
	mov	fs:[LIBDictHeaderStruc.ldhsModIDPtr],ax	; save selector value

llmid:
	mov	ax,fs:[LIBDictHeaderStruc.ldhsModUsedCount]	; get module used count
	test	ax,511		; see if need to resize allocation for module IDs
	jne	llmfirst		; no
	mov	dx,ax
	mov	ax,fs:[LIBDictHeaderStruc.ldhsModIDPtr]
	shl	dx,2			; dword per entry
	add	dx,2048			; extend room for another 512 modules
	call	ResizeMemory

; allocate smaller of SIZEIOBUFFBLK or esi block for reading in file 
; first IOBUFFSYSVARVSIZE bytes are for system variables,
;  so only read SIZEIOBUFFBLK-IOBUFFSYSVARSIZE at offset IOBUFFSYSVARSIZE
llmfirst:
	mov	edx,esi			; get bytes to read to dictionary low word size
	add	edx,IOBUFFSYSVARSIZE
	cmp	edx,SIZEIOBUFFBLK
	jbe	llmalloc		; dictionary < default buffer block size+sysvars
	mov	dx,SIZEIOBUFFBLK

llmalloc:
	mov	di,dx			; save bytes allocated this pass including sysvars
	call	AllocateMemory

	mov	bx,fs:[LIBDictHeaderStruc.ldhsModUsedCount]	; get used modules count
	dec	bx				; make relative 0
	add	bx,bx			; convert to word offset

	mov	es,fs:[LIBDictHeaderStruc.ldhsModBuffPtr]
	mov	es:[bx],ax		; keep pointer to first SIZEIOBUFFBLK buffer for file
	mov	CurrentBaseOBJBuff,ax	; save pointer to current buffer base
	add	bx,bx			; make used modules a dword pointer
	mov	es,fs:[LIBDictHeaderStruc.ldhsModIDPtr]	; es -> module id counts
	mov	edx,ModuleCount
	mov	es:[bx],edx		; save module id
	mov	FileAllocBlockParent,0	; init parent of first block to zero

; read chunk of file, di holds byte count+sysvar bytes
;  ax holds selector of buffer
llmread:
	sub	di,IOBUFFSYSVARSIZE	; adjust for system variable bytes
	mov	bx,CurrentFileHandle
	mov	cx,di
	mov	dx,IOBUFFSYSVARSIZE	; offset from system variables
	push	ds
	mov	ds,ax			; ds -> allocated memory buffer
	call	ReadFile	; actual number of bytes read returned in ax
	mov	dx,ds			; save selector value
	pop	ds				; restore ds -> wl32 data
	mov	es,dx			; es -> allocated block
	mov	es:[IOBuffHeaderStruc.ibhsBlockSize],ax	; save size of block, not counting sysvars
	movzx	eax,ax
	sub	esi,eax			; update bytes to allocate

	mov	es,dx
	xor	di,di			; es:di -> allocated block
	mov	ax,di
	mov	cx,IOBUFFSYSVARSIZE/4
	rep	stosd			; zero init first bytes for variable storage

	mov	eax,fs:[LIBDictHeaderStruc.ldhsFileNamePtr]
	mov	es:[IOBuffHeaderStruc.ibhsFileNamePtr],eax	; keep pointer to file name
	cmp	FileAllocBlockParent,0	; see if need to chain i/o buffer back to parent
	je	llm4			; no

	mov	ax,FileAllocBlockParent
	mov	es:[IOBuffHeaderStruc.ibhsParentPtr],ax	; child (current) -> parent
	mov	es,ax
	mov	es:[IOBuffHeaderStruc.ibhsChildPtr],dx	; parent -> child (current)
	mov	ax,es:[IOBuffHeaderStruc.ibhsBlockID]	; get parent block ID
	inc	ax
	mov	es,dx			; es -> child block
	mov	es:[IOBuffHeaderStruc.ibhsBlockID],ax	; child block ID is parent's plus one

llm4:
	mov	FileAllocBlockParent,dx	; update parent pointer for next block
	or	esi,esi			; see if end of file
	je	llmret			; yes

; allocate another block and reloop for read
	mov	edx,esi			; get bytes to read to dictionary low word size
	add	edx,IOBUFFSYSVARSIZE
	cmp	edx,SIZEIOBUFFBLK
	jbe	llmall2			; dictionary < default buffer block size+sysvars
	mov	dx,SIZEIOBUFFBLK

llmall2:
	mov	di,dx			; save bytes allocated this pass including sysvars
	call	AllocateMemory
	jmp	NEAR PTR llmread

llmret:
	ret
LoadLIBModule	ENDP

;*****************************
;* OPENCURRENTLIB            *
;*****************************

; open current library file, read dictionary
; destroys ax,bx,cx,dx,di,si,es

OpenCurrentLIB	PROC
	mov	di,OFFSET DGROUP:CurrentFileName	; es:di -> file name storage
	mov	si,LIBNameOffset
	push	si			; save -> original lib name offset
	push	ds			; save ds -> wl32 data
	pop	es				; ensure es -> wl32 data
	push	ds			; save -> wl32 data back on stack
	mov	ds,LIBNameSelector	; ds:si -> saved library name

ololoop:
	movsb
	cmp	BYTE PTR ds:[si-1],0
	jne	ololoop			; transfer to null terminator

	pop	ds				; restore ds -> wl32 data
	mov	LIBNameOffset,si	; update library module name offset for next read

	mov	dx,OFFSET DGROUP:CurrentFileName
	mov	al,40h			; read-only, deny none access
	call	OpenFile	; open object file, ax == handle
	mov	CurrentFileHandle,ax	; save handle of file being read

; see if need to allocate table to hold pointer to library dictionary
	cmp	LIBDictTablePtr,0
	jne	olohead			; already allocated
	mov	dx,TotalLibCount
	add	dx,64			; total kludge, allow up to 64 new default libraries specified within library
	add	dx,dx			; one word selector pointer per library
	call	AllocateMemory	; allocate memory for pointers
	mov	LIBDictTablePtr,ax	; save selector value

	mov	es,ax
	mov	cx,TotalLibCount
	add	cx,64
	xor	ax,ax
	mov	di,ax
	rep	stosw			; zero init pointers for checking against null pointer later on

; read the library header first nine bytes, check that is valid file
; library system variables are contained in header
olohead:
	mov	dx,OFFSET DGROUP:LIBHeader
	mov	cx,9
	mov	bx,CurrentFileHandle
	call	ReadFile

	cmp	LIBHeader.lhsType,MSLHED
	je	oloseek			; valid

; bad library format
olobadlib:
	mov	al,BADLIBERRORCODE
	mov	dx,OFFSET DGROUP:CurrentFileName
	call	LinkerErrorExit	; no return

; seek to library dictionary
oloseek:
	mov	dx,WORD PTR LIBHeader.lhsDictOff
	mov	cx,WORD PTR LIBHeader.lhsDictOff+2
	mov	ax,4200h		; move file pointer relative start of file
	int	21h
	mov	si,LIBHeader.lhsDictSize	; get dictionary size in 512-byte blocks
	movzx	esi,si
	shl	esi,9			; convert to bytes in esi

; allocate smaller of SIZELIBDICTBUFFBLK or esi block for reading in file 
; first LIBDICTBUFFSYSVARVSIZE bytes are for system variables,
;  so only read SIZELIBDICTBUFFBLK-LIBDICTBUFFSYSVARSIZE at offset LIBDICTBUFFSYSVARSIZE
; original lib name offset still on stack
	mov	edx,esi			; get bytes to read to dictionary low word size
	add	edx,LIBDICTBUFFSYSVARSIZE
	cmp	edx,SIZELIBDICTBUFFBLK
	jbe	oloalloc		; dictionary < default buffer block size+sysvars
	mov	dx,SIZELIBDICTBUFFBLK

oloalloc:
	mov	di,dx			; save bytes allocated this pass including sysvars
	call	AllocateMemory
	mov	bx,CurrentLIB
	dec	bx				; make relative zero
	add	bx,bx			; convert to word offset
	mov	es,LIBDictTablePtr	; es:bx -> entry in table of memory block selectors
	mov	es:[bx],ax	; keep pointer to first SIZELIBDICTBUFFBLK buffer for file
	mov	BaseLIBDictBuff,ax	; save pointer to current buffer base
	mov	FileAllocBlockParent,0	; init parent of first block to zero
	mov	es,ax
;@@@	mov	es:[LIBDictHeaderStruc.ldhsModBuffPtr],0	; zero out module pointer
;@@@	mov	es:[LIBDictHeaderStruc.ldhsModUsedCount],0	; zero out module used count

; read chunk of file, di holds byte count+sysvars
;  ax holds selector of buffer
oloread:
	sub	di,LIBDICTBUFFSYSVARSIZE	; adjust for system variable bytes
	mov	bx,CurrentFileHandle
	mov	cx,di
	mov	dx,LIBDICTBUFFSYSVARSIZE	; offset from system variables
	push	ds
	mov	ds,ax			; ds -> allocated memory buffer
	call	ReadFile	; actual number of bytes read returned in ax
	mov	dx,ds			; save selector value
	pop	ds				; restore ds -> wl32 data
	mov	es,dx			; es -> allocated block
	mov	es:[LIBDictHeaderStruc.ldhsBlockSize],ax	; save size of block, not counting sysvars
	movzx	eax,ax
	sub	esi,eax			; update bytes to allocate

	xor	di,di			; es:di -> allocated block
	mov	ax,di
	mov	cx,LIBDICTBUFFSYSVARSIZE/4
	rep	stosd			; zero init first bytes for variable storage
	mov	ax,LIBHeader.lhsDictSize	; save dictionary size in all buffers
	mov	es:[LIBDictHeaderStruc.ldhsDictSize],ax
	cmp	FileAllocBlockParent,0	; see if need to chain i/o buffer back to parent
	je	olo4			; no
	mov	ax,FileAllocBlockParent
	mov	es:[LIBDictHeaderStruc.ldhsParentPtr],ax	; child (current) -> parent
	mov	es,ax
	mov	es:[LIBDictHeaderStruc.ldhsChildPtr],dx	; parent -> child (current)
	mov	ax,es:[LIBDictHeaderStruc.ldhsBlockID]	; get parent block ID
	inc	ax
	mov	es,dx			; es -> child block
	mov	es:[LIBDictHeaderStruc.ldhsBlockID],ax	; child block ID is parent's plus one

olo4:
	mov	FileAllocBlockParent,dx	; update parent pointer for next block
	or	esi,esi			; see if end of file
	je	oloeof			; yes

; allocate another block and reloop for read
	mov	edx,esi			; get bytes to read to dictionary low word size
	add	edx,LIBDICTBUFFSYSVARSIZE
	cmp	edx,SIZELIBDICTBUFFBLK
	jbe	oloall2			; dictionary < default buffer block size+sysvars
	mov	dx,SIZELIBDICTBUFFBLK

oloall2:
	mov	di,dx			; save bytes allocated this pass including sysvars
	call	AllocateMemory
	jmp	NEAR PTR oloread

; done reading in file, keep it open
; save pointer to file name in first dictionary buffer
;  original lib name offset still on stack
oloeof:
	mov	es,BaseLIBDictBuff	; es -> first buffer block of library dictionary
	pop	bx				; bx -> original lib name offset
	mov	es:[WORD PTR LIBDictHeaderStruc.ldhsFileNamePtr],bx
	mov	ax,LIBNameSelector
	mov	es:[WORD PTR LIBDictHeaderStruc.ldhsFileNamePtr+2],ax

	ret
OpenCurrentLIB	ENDP

;*****************************
;* WRITETOIOBUFFER           *
;*****************************

; write the program bytes to i/o Buffer
; upon entry fs:si -> bytes to write, cx holds count to write,
;  eax holds offset within segment, es:di -> individual segdef entry
; destroys dx

WriteToIOBuffer	PROC
	push	cx			; save critical register
	push	si
	push	di
	push	es
	push	fs
	push	gs
	push	bx
	push	ax

	movzx	ecx,cx		; convert bytes to write to 32-bits
	lgs	bx,es:[di+IndSegDefRecStruc.isdrMasterPtr]	; gs:bx -> master segdef pointer
	add	eax,es:[di+IndSegDefRecStruc.isdrSegOffset]	; add in individual segment offset
	add	eax,gs:[bx+MasterSegDefRecStruc.mssSegOffset]	; add in master segment offset

; eax holds absolute write offset, ecx holds bytes to write
wtioabs:
	mov	AbsoluteWriteOff,eax
	cmp	eax,CurrentWriteBase	; see if absolute byte offset is less than current base
	jae	wtiogetstart	; no

; need new write base
wtionewbase:
	call	FlushIOBuffer	; flush any pending bytes
	mov	CurrentWriteBase,eax	; update current write base
	push	ecx			; save bytes to write
	push	eax				; save current write base

; read in bytes already written, up to end of buffer or end of maximum write offset
; seek to place in executable to read bytes
	add	eax,ExecutableHeaderSize	; eax == file position to seek to
	mov	dx,ax
	shr	eax,16
	mov	cx,ax			; cx:dx == file position
	mov	bx,ExecutableFileHandle
	mov	ax,4200h		; move file pointer, relative start of file
	int	21h

; compute bytes to read
	pop	edi				; edi==CurrentWriteBase
	push	edi			; restore to stack
	mov	eax,HighestOffsetWritten
	sub	eax,edi	; compute bytes to read
	cmp	eax,WRITEBUFFERSIZE	; see if >i/o buffer size
	jbe	wtioreadsetup	; no
	mov	eax,WRITEBUFFERSIZE	; only read i/o # of bytes

; read bytes
wtioreadsetup:
	mov	edi,eax			; save bytes to read
	xor	dx,dx			; zero offset
	push	ds
	mov	ds,IOBlockSeg	; ds:dx -> i/o buffer block

wtioreadloop:
	mov	ecx,edi
	cmp	ecx,WRITEBUFFERSIZE-10h	; see if past max bytes to write
	jbe	wtioread		; no
	mov	cx,WRITEBUFFERSIZE-10h	; set bytes to write to max

wtioread:
	call	ReadFile
	add	dx,ax			; update source offset
	movzx	eax,ax
	sub	edi,eax			; update bytes to read
	jne	wtioreadloop	; more bytes to read
	pop	ds				; restore ds -> wl32 data

	pop	eax				; eax == CurrentWriteBase
	pop	ecx				; restore bytes to write

wtiogetstart:
	sub	eax,CurrentWriteBase
	mov	StartWriteAddress,eax
	add	eax,ecx			; get last byte written+1 address relative buffer
	mov	EndWriteAddress,eax
	cmp	eax,WRITEBUFFERSIZE	; see if wrap past buffer
	jbe	wtiochklow		; no

; write will wrap past buffer
; flush buffer and update current write base
	call	FlushIOBuffer	; flush any pending bytes
	mov	eax,AbsoluteWriteOff
	mov	CurrentWriteBase,eax	; update current write base
	jmp	SHORT wtiogetstart

; check low water mark
wtiochklow:
	mov	eax,StartWriteAddress
	cmp	eax,LowWaterMark	; see if new low water mark
	jae	wtiochkhigh		; no
	mov	LowWaterMark,eax

; check high water mark
wtiochkhigh:
	mov	eax,EndWriteAddress
	cmp	eax,HighWaterMark	; see if new high water mark
	jbe	wtiochkread		; no
	mov	HighWaterMark,eax

wtiochkread:
	movzx	eax,si
	add	eax,ecx			; get last byte read+1 from source buffer
	cmp	eax,SIZEIOBUFFBLK	; see if buffer overflow
	jbe	wtiotrans		; no

; overflow reading bytes from source
; transfer SIZEIOBUFFBLK-si bytes to wrap point,
; update StartWriteAddress past bytes written
; update fs:si source pointer
; transfer eax-SIZEIOBUFFBLK bytes
wtioreadof:
	sub	eax,SIZEIOBUFFBLK	; compute leftover bytes to transfer
	push	ax			; bytes in ax, high word zero
	mov	cx,SIZEIOBUFFBLK
	sub	cx,si			; get bytes to read prior to wrap
	mov	di,WORD PTR StartWriteAddress	; write start known <64K
	movzx	ecx,cx
	add	StartWriteAddress,ecx	; adjust StartWriteAddress past bytes written
	mov	es,IOBlockSeg	; es:di -> destination
	push	ds			; save -> wl32 data
	push	fs
	pop	ds				; ds:si -> source
	mov	al,cl
	and	al,3			; save odd byte and word count
	shr	cx,2			; dwords to transfer
	rep	movsd			; transfer dwords for maximum efficiency
	mov	cl,al			; pick up odd byte and word count
;@@@	shr	cx,1			; pick up odd word count
;@@@	rep	movsw
;@@@	rcl	cx,1			; pick up odd byte count
	rep	movsb
	pop	ds				; restore ds -> wl32 data
	pop	cx				; get remaining bytes to write
	mov	fs,fs:[IOBuffHeaderStruc.ibhsChildPtr]
	mov	si,IOBUFFSYSVARSIZE	; update fs:si -> start of next block

; transfer from fs:si to buffer:StartWriteAddress
wtiotrans:
	mov	di,WORD PTR StartWriteAddress	; write start known <64K
	mov	es,IOBlockSeg	; es:di -> destination
	push	ds			; save -> wl32 data
	push	fs
	pop	ds				; ds:si -> source
	mov	al,cl
	and	al,3			; save odd byte and word count
	shr	cx,2			; dwords to transfer
	rep	movsd			; transfer dwords for maximum efficiency
	mov	cl,al			; pick up odd byte and word count
;@@@	shr	cx,1			; pick up odd word count
;@@@	rep	movsw
;@@@	rcl	cx,1			; pick up odd byte count
	rep	movsb
	pop	ds				; restore ds -> wl32 data

wtioret:
	pop	ax				; restore critical register
	pop	bx
	pop	gs
	pop	fs
	pop	es
	pop	di
	pop	si
	pop	cx
	ret
WriteToIOBuffer	ENDP

;*****************************
;* FLUSHIOBUFFER             *
;*****************************

; flush IO buffer of impending writes
; destroys bx,dx,es,di

FlushIOBuffer	PROC
	push	eax			; save critical registers
	push	ecx
	push	esi

; check if current write base+low water mark is larger than highest file write
; if so, write zeros from highest file write to current write base+low water mark
	mov	eax,LowWaterMark
	cmp	eax,HighWaterMark	; see if any bytes to write
	jae	fioret			; no
	add	eax,CurrentWriteBase
	cmp	eax,HighestOffsetWritten
	jbe	flushseek		; no padding needed

; must pad EXE file from HighestOffsetWritten to CurrentWriteBase+LowWaterMark
padexe:
	mov	esi,eax
	sub	esi,HighestOffsetWritten	; esi holds bytes to write

; seek to end of file
	call	SeekToEndOfFile

	mov	bx,ExecutableFileHandle
	xor	dx,dx			; zero offset
	push	ds
	mov	ds,ZeroBlockSeg	; ds:dx -> zero value block

zeroloop:
	mov	ecx,esi
	cmp	ecx,4000h		; see if past max bytes to write
	jbe	zerowrite		; no
	mov	cx,4000h		; set bytes to write to max

; cx holds bytes to write
zerowrite:
	call	WriteFile
	movzx	eax,ax
	sub	esi,eax			; update bytes to write
	jne	zeroloop		; more bytes to write
	pop	ds				; restore ds -> wl32 data

; seek to current write base+low water mark, relative executable file header
flushseek:
	mov	eax,ExecutableHeaderSize
	add	eax,CurrentWriteBase
	add	eax,LowWaterMark	; eax == file position to seek to
	mov	dx,ax
	shr	eax,16
	mov	cx,ax			; cx:dx == file position
	mov	bx,ExecutableFileHandle
	mov	ax,4200h		; move file pointer, relative start of file
	int	21h

; write (high water mark - low water mark) bytes
; source is IOBlockSeg:LowWaterMark
	mov	esi,HighWaterMark
	sub	esi,LowWaterMark	; esi holds bytes to write
	xor	dx,dx			; zero offset
	push	ds
	mov	ds,IOBlockSeg	; ds:dx -> i/o buffer block

flushwloop:
	mov	ecx,esi
	cmp	ecx,WRITEBUFFERSIZE-10h	; see if past max bytes to write
	jbe	flushwrite		; no
	mov	cx,WRITEBUFFERSIZE-10h	; set bytes to write to max

flushwrite:
	call	WriteFile
	add	dx,ax			; update source offset
	movzx	eax,ax
	sub	esi,eax			; update bytes to write
	jne	flushwloop		; more bytes to write
	pop	ds				; restore ds -> wl32 data

; update highest offset written to current write base+high water mark if lower
	mov	eax,HighWaterMark
	add	eax,CurrentWriteBase
	cmp	eax,HighestOffsetWritten
	jbe	flushinit		; no highest offset update needed
	mov	HighestOffsetWritten,eax

; zero init bytes in block
flushinit:
	call	Zero64KIOBlock

; init low/high water marks
	mov	HighWaterMark,0
	mov	LowWaterMark,WRITEBUFFERSIZE

fioret:
	pop	esi				; restore critical registers
	pop	ecx
	pop	eax
	ret
FlushIOBuffer	ENDP

ENDS

END
