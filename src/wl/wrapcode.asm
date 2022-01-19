;*********************************************************************
;*   WRAPCODE.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          06/16/93                                         *
;*   Model:         Tiny                                             *
;*   Version:       1.0                                              *
;*   Assembler:     TASM 2.5                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   WARPWRAP environment variable burn-in runtime code              *
;*                                                                   *
;*********************************************************************

TITLE   WRAPCODE
PAGE    50,80

.MODEL SMALL
.CODE
	ASSUME	ds:_TEXT
start:
	nop					; avoid first word for zero'ed relocation entries
	nop					; WARPWRAP will set CS:IP to <program_segment>:2
	jmp	SHORT pastdata	; jump past data so WARPWRAP will always know where data is for placement

; code segment relative data

; signature bytes
	DB	"WARPWRAP1"		; signature used to avoid processing already processed file

; modified by WARPWRAP
WrapperSize		DW	?	; size of wrappered code and data (for replacement)
RelocationEntryCount	DW	?	; count of relocation entries (word sized)
OriginalCS	DW	?		; original CS value
OriginalIP	DW	?		; original IP value
NewStringCount	DW	?	; count of new strings
NewStringLocation	DW	?	; offset to new strings
ReplaceStringCount	DW	?	; count of replace strings
ReplaceStringLocation	DW	?	; offset to replace strings
DeleteStringCount	DW	?	; count of delete strings
DeleteStringLocation	DW	?	; offset to delete strings
MemoryFillFlag	DB	0	; nonzero if memory fill required
MemoryFillValue	DW	?	; memory fill value
EXEImageSizePara	DW	?	; EXE image (physically loaded from file) size in paras
OverwrittenByteCount	DW	?	; count of overwritten bytes

; internal, not used by WARPWRAP
NewBlockFlag	DB	0	; nonzero if new environment block required
RelocationEntrySegment	DW	?	; allocated memory segment holding relocation entries
TransferCodeSegment	DW	?	; allocated memory segment holding transfer code

; used to compute size of transfer code for placement in allocated memory block
TransferCodeStart	LABEL	WORD

; this code is relocated
; upon entry to this code, original AX, new environment block segment,
; and original CS:IP are stored on stack,
; stored on stack -> relocation entries segment, si==0 (reloc entries offset)
; stored on stack == relocation entry count
; bx == file handle of EXE file, cx == bytes to read
;  es -> PSP, ds:dx -> read location of EXE file bytes

	mov	ah,3fh			; read EXE file bytes back to original position
	int	21h

; file pointer now positioned to relocation entries
	pop	cx				; get relocation entry count
	pop	ds				; ds -> relocation entries
	jcxz	jumpin		; no relocation entries to fixup

	push	es			; save es -> PSP
	mov	ax,es
	add	ax,10h			; ax -> program load segment
	mov	es,ax
	mov	bp,ax			; save program load segment in dx for fixup purposes
	xor	dx,dx			; read offset
	add	cx,cx			; two bytes/relocation entry

nextread:
	push	cx			; save total bytes to read
	cmp	cx,40h			; see if more than storage
	jbe	fix2			; no
	mov	cx,40h			; read maximum allowed

fix2:
	mov	ah,3fh			; read EXE file bytes back to original position
	int	21h
	mov	si,dx			; zero init offset in relocation entry storage
	shr	cx,1			; convert to word count

fixloop:
	lodsw				; get relocation entry
	mov	di,ax
	add	es:[di],bp
	loop	fixloop
	pop	cx				; get total relocation entry bytes
	sub	cx,40h
	ja	nextread
	pop	es				; restore es -> PSP

jumpin:
	mov	ah,3eh			; close EXE file
	int	21h

	push	es
	pop	ds				; ds -> PSP
	pop	ax				; ax -> new environment block
	mov	ds:[2ch],ax		; update program PSP environment block pointer
	pop	ax				; restore DOS ax flagging
	retf				; jump to regular EXE though CS:IP stored on stack
TransferCodeEnd		LABEL	WORD

pastdata:
	mov	bp,ax			; save ax value for DOS flagging purposes
	mov	si,es			; save -> PSP
	push	cs
	pop	ds				; ds -> code segment data
	mov	ax,es
	add	ax,10h			; ax == load segment
	add	OriginalCS,ax		; fixup original program CS

; read DOS version, must be >=3.0 so can read EXE name from argv[0]
	mov	ax,3000h		; get DOS version number
	int	21h
	cmp	al,3			; must be >=3
	jae	dosgood			; yes

; error exit
errorout:
	mov	ax,4c01h
	int	21h

; clear memory if so flagged
; this clears the stack too, so don't have anything stored there before this
;  code is executed
; es -> PSP
dosgood:
	cmp	MemoryFillFlag,0	; see if filling memory with value
	je	procev			; no
	mov	ax,es
	add	ax,10h			; ax == program load segment
	add	ax,EXEImageSizePara	; ax -> segment just above program final load byte
	mov	dx,es:[2]		; dx -> top of memory, constant

; fill the memory one paragraph at a time
filoop:
	cmp	ax,dx			; see if at top of memory
	jae	procev			; yes, fill complete
	mov	es,ax
	mov	ax,MemoryFillValue	; get memory fill value
	xor	di,di			; zero offset
	mov	cx,8			; fill eight words at a time
	rep	stosw			; zero fill paragraph
	mov	ax,es
	inc	ax
	jmp	SHORT filoop

; process environment variables
; process new strings
procev:
	push	OriginalCS
	push	OriginalIP	; save original program CS:IP to stack
	push	bp			; save original ax value
	push	si			; save -> PSP on stack

	mov	dx,100h			; dl==zero constant, dh=nonzero (1) constant
	mov	si,NewStringLocation	; ds:si -> new strings
	mov	cx,NewStringCount	; get count of new strings
	jcxz	repstring	; no new strings

newmain:
	mov	bp,si			; save bp -> current source (WARPWRAP) string start
	pop	es				; es -> PSP
	push	es			; restore -> PSP to stack
	mov	es,es:[2ch]		; es -> environment string block from PSP pointer
	xor	di,di			; init offset into block

newdest:
	cmp es:[di],dl		; see if at end of environment block
	jne	newloop			; no, keep looking for source string
	mov	NewBlockFlag,dh	; flag that new e-var block is needed
	jmp	SHORT n_nextstring	; move to next source string processing

; ds:si -> new string, es:di -> e-var block string
newloop:
	cmpsb				; see if strings match
	jne	no_nmatch		; no match
	cmp	BYTE PTR ds:[si-1],'='	; see if matched to equals sign
	jne	newloop			; no

; strings matched to equals sign, don't add string
	dec	NewStringCount	; drop count of new strings
	mov	BYTE PTR ds:[bp],dh	; flag not to use this source string

; move to next source string
n_nextstring:
	mov	si,bp			; ds:si -> start of source string

ntermloop:
	lodsb				; get char, move to next
	or	al,al			; see if null terminator
	jne	ntermloop		; no

; ds:si -> next string
	loop	newmain		; loop to check next string
	jmp	SHORT repstring	; process replace strings

; strings didn't match, move to next in environment block
no_nmatch:
	cmp es:[di-1],dl	; see if at end of destination string
	je	at_newdest		; yes, di -> next destination string

; move to start of next destination string
n_destloop:
	mov	al,es:[di]		; get char
	inc	di				; move to next char
	or	al,al			; see if null terminator
	jne	n_destloop		; no

; es:di -> destination, make ds:si -> source
at_newdest:
	mov	si,bp			; si -> start of source string
	jmp	SHORT newdest	; loop with es:di -> new destination string

; process replace strings
repstring:
	mov	si,ReplaceStringLocation	; ds:si -> replace strings
	mov	cx,ReplaceStringCount	; get count of replace strings
	jcxz	delstring	; no replace strings

repmain:
	mov	bp,si			; save bp -> current source (WARPWRAP) string start
	xor	bx,bx			; bx -> current string offset in PSP e-var block
	pop	es				; es -> PSP
	push	es			; restore -> PSP to stack
	mov	es,es:[2ch]		; es -> environment string block from PSP pointer
	mov	di,bx			; init offset into block

repdest:
	cmp es:[di],dl		; see if at end of environment block
	jne	reploop			; no, keep looking for source string
	mov	NewBlockFlag,dh	; flag that new e-var block is needed
	jmp	SHORT r_nextstring	; move to next source string processing

; ds:si -> replace string, es:di -> e-var block string
reploop:
	cmpsb				; see if strings match
	jne	no_rmatch		; no match
	cmp	BYTE PTR ds:[si-1],'='	; see if matched to equals sign
	jne	reploop			; no

; strings matched to equals sign, check if match to null terminator
; if match to null terminator then kill source string,
;  else kill destination string
; ds:si -> replace string parameter, es:di -> e-var block string parameter
reploop2:
	cmpsb				; see if strings match
	jne	no_rparm		; no match to parameter
	cmp	ds:[si-1],dl	; see if matched to null terminator
	jne	reploop2		; no

; match to null terminator, replace string is superfluous
	dec	ReplaceStringCount	; drop count of replace strings
	mov	BYTE PTR ds:[bp],dh	; flag not to use this source string
	jmp	SHORT r_nextstring	; move to next replace string, if any

; parameters don't match, use replace string over e-var block string
no_rparm:
	mov	BYTE PTR es:[bx],dh	; flag not to use this e-var block string

; move to start of next destination string
r_destloop:
	mov	al,es:[di]		; get char
	inc	di				; move to next char
	or	al,al			; see if null terminator
	jne	r_destloop		; no

	mov	NewBlockFlag,dh	; flag that new e-var block is needed

; move to next source string
r_nextstring:
	mov	si,bp			; ds:si -> start of source string

rtermloop:
	lodsb				; get char, move to next
	or	al,al			; see if null terminator
	jne	rtermloop		; no

; ds:si -> next replace string
	loop	repmain		; loop to check next replace string
	jmp	SHORT delstring	; process delete strings

; strings didn't match, move to next in environment block
no_rmatch:
	cmp	BYTE PTR es:[di-1],dl	; see if at end of destination string
	je	at_repdest		; yes, di -> next replace destination string

; move to start of next destination string
r_destloop2:
	mov	al,es:[di]		; get char
	inc	di				; move to next char
	or	al,al			; see if null terminator
	jne	r_destloop2		; no

; es:di -> destination, make ds:si -> source
at_repdest:
	mov	bx,di			; keep bx -> start of destination string
	mov	si,bp			; si -> start of source string
	jmp	SHORT repdest	; loop with es:di -> next replace destination string

; process delete strings
delstring:
	mov	si,DeleteStringLocation	; ds:si -> delete strings
	mov	cx,DeleteStringCount	; get count of delete strings
	jcxz	compsize	; no delete strings

delmain:
	mov	bp,si			; save bp -> current source (WARPWRAP) string start
	xor	bx,bx			; bx -> current string offset in PSP e-var block
	pop	es				; es -> PSP
	push	es			; restore -> PSP to stack
	mov	es,es:[2ch]		; es -> environment string block from PSP pointer
	mov	di,bx			; init offset into block

deldest:
	cmp es:[di],dl		; see if at end of environment block
	je	dnextsrc		; yes

; ds:si -> delete string, es:di -> e-var block string
delloop:
	cmpsb				; see if strings match
	jne	no_dmatch		; no match
	cmp	BYTE PTR ds:[si-1],'='	; see if matched to equals sign
	jne	delloop			; no

; strings matched to equals sign, delete string
	mov	BYTE PTR es:[bx],dh	; flag not to use this e-var block string
	mov	NewBlockFlag,dh	; flag that new e-var block is needed

dnextsrc:
	mov	si,bp			; ds:si -> start of source string

dtermloop:
	lodsb				; get char, move to next
	or	al,al			; see if null terminator
	jne	dtermloop		; no
	loop	delmain		; loop to check next string
	jmp	SHORT compsize	; compute allocated memory block size for new environment block

; strings didn't match, move to next in environment block
no_dmatch:
	cmp es:[di-1],dl	; see if at end of destination string
	je	at_deldest		; yes, di -> next destination string

; move to start of next destination string
d_destloop:
	mov	al,es:[di]		; get char
	inc	di				; move to next char
	or	al,al			; see if null terminator
	jne	d_destloop		; no

; es:di -> destination, make ds:si -> source
at_deldest:
	mov	bx,di			; keep bx -> start of destination string
	mov	si,bp			; si -> start of source string
	jmp	SHORT deldest	; loop with es:di -> next destination string

; determine size of new environment variable block by counting length of
; environment variable string, discarding environment block and WARPWRAP strings
; when they begin with a 1 (Ctrl-A)
; no new block if NewBlockFlag is zero
compsize:
	xor	ax,ax			; preset ax to no block allocation
	cmp	NewBlockFlag,al	; see if new block
	je	topara			; no
	xor di,di			; es:di -> start of environment block

evar_main:
	cmp	BYTE PTR es:[di],0	; see if at end of environment block
	je	evar_end		; yes
	cmp	BYTE PTR es:[di],1	; no, see if string to discard
	je	evar_discard	; yes

; this string goes into the new environment block
evar_loop:
	inc	ax				; bump count of bytes
	inc	di
	cmp	BYTE PTR es:[di-1],0	; see if at end of environment string
	jne	evar_loop		; no
	jmp	SHORT evar_main	; loop until all checked

; discard this string
evar_discard:
	inc	di
	cmp	BYTE PTR es:[di-1],0	; see if at end of environment string
	jne	evar_discard	; no
	jmp	SHORT evar_main	; loop until all checked

evar_end:
	add	di,3			; bump past null terminator and word count after environment strings

; get byte count of ASCIIZ EXE name (argv[0])
name_loop:
	inc	ax				; bump count of bytes
	inc	di
	cmp	BYTE PTR es:[di-1],0	; see if at end of EXE name
	jne	name_loop		; no

; ax holds byte count from old environment block, add in byte count
; from new and replace strings
	mov	cx,NewStringCount
	jcxz	replace_start	; no new strings
	mov	si,NewStringLocation	; ds:si -> starting string location

new_main:
	cmp	BYTE PTR [si],1	; see if flushed string
	jne	new_loop		; no, use it

; this new string isn't used, discard it
new_discard:
	inc	si
	cmp	BYTE PTR ds:[si-1],0	; see if at end of environment string
	jne	new_discard		; no
	jmp	SHORT new_main	; loop until all checked

; this string goes into the new environment block
new_loop:
	inc	ax				; bump count of bytes
	inc	si
	cmp	BYTE PTR ds:[si-1],0	; see if at end of environment string
	jne	new_loop		; no
	loop	new_main	; loop until all checked

; now the replace strings
replace_start:
	mov	cx,ReplaceStringCount
	jcxz	topara		; no replace strings
	mov	si,ReplaceStringLocation	; ds:si -> starting string location

replace_main:
	cmp	BYTE PTR [si],1	; see if flushed string
	jne	replace_loop	; no, use it

; this replace string isn't used, discard it
replace_discard:
	inc	si
	cmp	BYTE PTR ds:[si-1],0	; see if at end of environment string
	jne	replace_discard	; no
	jmp	SHORT replace_main	; loop until all checked

; this string goes into the new environment block
replace_loop:
	inc	ax				; bump count of bytes
	inc	si
	cmp	BYTE PTR ds:[si-1],0	; see if at end of environment string
	jne	replace_loop		; no
	loop	replace_main	; loop until all checked

; ax contains size of new environment block in bytes
; convert block size in ax to paras
topara:
	add	ax,16			; round up to next para
	mov	cl,4
	shr	ax,cl			; convert to paras
	mov	bx,ax			; save count of paras to allocate

; add transfer/fixup code and relocation entries allocation to e-var allocation
	mov	ax,OFFSET TransferCodeEnd
	sub	ax,OFFSET TransferCodeStart
	add	ax,16			; round up to next para
	shr	ax,cl			; convert to paras
	add	bx,ax
	add	bx,5			; four paragraphs (64 bytes) for relocation entry processing+1 round up
	mov	dx,bx			; save allocate value

; allocate memory required, adjust current memory if necessary
	mov	ah,48h			; allocate memory
	int	21h
	jnc	allocok
	cmp	ax,8			; see if out of memory
	je	oom				; yes

to_errorout:
	jmp	NEAR PTR errorout	; nonmemory error

oom:
	pop	ax				; ax -> PSP
	push	ax			; restore -> PSP to stack
	mov	es,ax			; es -> PSP
	mov	bx,es:[2]		; get top of memory
	sub	bx,ax			; compute total memory to top of memory
	sub	bx,dx			; subtract off new allocation value
	sub	bx,2			; allow two paragraphs of slop
	mov	cx,bx			; save new allocation amount
	mov	ah,4ah			; resize memory block
	int	21h
	jc	to_errorout		; error resizing memory

	mov	ax,es
	add	ax,cx			; compute new top of memory (PSP + allocated memory)
	mov	es:[2],ax		; update top of memory
	mov	bx,dx			; get memory allocation value
	mov	ah,48h			; allocate memory
	int	21h
	jc	to_errorout		; error allocating memory

allocok:
	mov	es,ax			; es -> new environment block
	pop	ds				; ds -> PSP
	cmp	cs:NewBlockFlag,0	; see if new block
	jne	nb1				; yes

; no new environment block, store old one back to stack
	push	ds:[2ch]
	jmp	SHORT nb2

nb1:
	push	ax			; save -> new environment block on stack

; transfer old/new/modified environment variables to block
nb2:
	push	ds			; restore -> PSP to stack
	mov	ax,ds:[2ch]
	mov	ds,ax			; ds -> old environment block
	xor	si,si
	mov	di,si
	cmp	cs:NewBlockFlag,0	; see if new block
	je	evardone		; no

; ds:si -> old environment block, es:di -> new environment block
maintrans:
	lodsb				; get first byte of e-var string
	cmp	al,1			; see if ignore string flag
	je	igntrans		; yes
	or	al,al			; see if end of e-var block
	je	donew			; yes

transloop1:
	stosb
	or	al,al			; transfer chars through null terminator
	je	maintrans		; transfer of string complete
	lodsb
	jmp	SHORT transloop1

; ignore this string
igntrans:
	lodsb				; discard until null terminator
	or	al,al
	jne	igntrans		; not at terminator yet
	jmp	SHORT maintrans	

; old strings transferred, do new/replace strings
donew:
	push	si			; save -> byte following end of e-var block
	push	cs
	pop	ds				; ds -> code segment data

	mov	cx,NewStringCount
	jcxz	dorep		; no new strings
	mov	si,NewStringLocation	; ds:si -> starting string location

newchk:
	cmp	BYTE PTR [si],1	; see if flushed string
	jne	newtrans		; no, use it

; this new string isn't used, move to next
newmove:
	lodsb
	or	al,al			; see if at end of environment string
	jne	newmove			; no
	jmp	SHORT newchk	; loop until all transferred

; this string goes into the new environment block
newtrans:
	lodsb
	stosb
	or	al,al			; see if at end of environment string
	jne	newtrans		; no
	loop	newchk		; loop until all transferred

; transfer replace strings
dorep:
	mov	cx,ReplaceStringCount
	jcxz	transdone	; no replace strings
	mov	si,ReplaceStringLocation	; ds:si -> starting string location

repchk:
	cmp	BYTE PTR [si],1	; see if flushed string
	jne	reptrans		; no, use it

; this replace string isn't used, move to next
repmove:
	lodsb
	or	al,al			; see if at end of environment string
	jne	repmove			; no
	jmp	SHORT repchk	; loop until all transferred

; this string goes into the new environment block
reptrans:
	lodsb
	stosb
	or	al,al			; see if at end of environment string
	jne	reptrans		; no
	loop	repchk		; loop until all transferred

; transfer of strings complete, transfer zero terminator, word count,
; EXE name string
transdone:
	xor	al,al
	stosb				; transfer final zero
	pop	si				; si -> word count after end of old e-var block
	pop	ds				; ds -> PSP
	push	ds			; restore -> PSP to stack
	mov	ax,ds:[2ch]
	mov	ds,ax			; ds -> old environment block
	lodsw				; get word count
	stosw				; transfer it

transexe:
	lodsb
	stosb
	or	al,al			; transfer EXE name through null terminator
	jne	transexe

; end of environment variable processing
evardone:
	push	cs
	pop	ds				; ds -> code segment data

; compute segment to put relocation entries, after environment information
	cmp	RelocationEntryCount,0
	je	ready_trans		; no relocation entries exist
	mov	cl,4
	add	di,16			; round up to next para
	shr	di,cl
	mov	ax,es
	add	ax,di			; ax -> segment to place relocation entries
	mov	RelocationEntrySegment,ax	; save it

	add	ax,3			; four paragraphs for relocation entries (relative zero)
	mov	es,ax
	xor	di,di			; es:di -> transfer area for relocation entries

; compute segment to put transfer code, after relocation entries allocation
; transfer transfer code up to allocated memory block
ready_trans:
	mov	cl,4
	add	di,16			; round up to next para
	shr	di,cl
	mov	ax,es
	add	ax,di			; ax -> segment to place transfer code
	mov	TransferCodeSegment,ax	; save it

; transfer transfer code up to allocated memory block
	mov	cx,OFFSET TransferCodeEnd
	mov	si,OFFSET TransferCodeStart
	sub	cx,si
	mov	es,ax
	xor	di,di			; es:di -> transfer area for transfer code
	shr	cx,1			; convert byte count to write to words
	rep	movsw			; move code bytes
	rcl	cx,1			; pick up carry
	rep	movsb			; transfer leftover byte, if any

; transfer to transfer code
;  read original code back in to proper location
;  fixup original code
;  jump to normal program entry
	pop	es				; es -> PSP, don't restore to stack

; open EXE file for reading
	mov	ax,es:[2ch]
	mov	ds,ax
	xor	si,si			; ds:si -> start of original e-var block

openloop:
	lodsb
	or	al,al			; see if end of e-var string
	jne	openloop
	lodsb
	or	al,al			; see if end of e-var block
	jne	openloop
	add	si,2			; ds:si -> EXE name
	mov	dx,si			; ds:dx -> EXE file name

	mov	ax,3d40h		; open, read access
	int	21h
	jnc	openok			; no errors

to_errorout2:
	jmp	NEAR PTR errorout	; error opening file

; seek to end of file for EXE data
openok:
	push	cs
	pop	ds				; ds -> code segment data

	mov	bx,ax			; file handle in bx
	mov	dx,RelocationEntryCount
	add	dx,dx			; two bytes per relocation entry
	add	dx,OverwrittenByteCount	; offset from end of file to read
	neg	dx
	mov	cx,-1			; back up WrapperSize bytes from end of file
	mov	ax,4202h		; move file pointer, offset from end of file
	int	21h
	jc	to_errorout2	; error seeking

	push	RelocationEntrySegment	; place relocation entry segment on stack for transfer code
	push	RelocationEntryCount	; relocation entry count as well
	push	TransferCodeSegment	; place transfer code segment on stack

	mov	cx,OverwrittenByteCount	; get bytes to read from EXE file
	xor	ax,ax			; transfer code start offset
	push	ax			; place on stack
	mov	si,ax			; zero si
	mov	dx,ax			; ds:dx -> area to load code
	retf				; transfer to transfer code

; WARPWRAP-placed data
; new strings go here
; replace strings go here
; delete strings go here

END	start
