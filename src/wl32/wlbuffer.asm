;*********************************************************************
;*   WLBUFFER.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          11/02/93                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   read buffer related routines                                    *
;*                                                                   *
;*********************************************************************

TITLE   WL32 WLBUFFER
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

PUBLIC	ReadByte,ReadByteDecCX,
PUBLIC	ReadWord,ReadWordCX,ReadWordDecCX
PUBLIC	ReadDword,ReadDwordDecCX,ReadDwordECX
PUBLIC	ReadIndex,ReadIndexDecCX
PUBLIC	ReadNameString
PUBLIC	ScanAhead
PUBLIC	SetOBJBuffer

;*****************************
;* Data begins               *
;*****************************

;*****************************
;* Uninitialized data        *
;*****************************

_BSS	SEGMENT WORD PUBLIC USE16 'BSS'
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
ENDS

;*****************************
;* External data             *
;*****************************

EXTRN	CurrentBaseOBJBuff:WORD
EXTRN	CurrentOBJ:WORD
EXTRN	OBJBuffSelTablePtr:WORD

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

;*****************************
;* SETOBJBUFFER              *
;*****************************

; setup for reading current OBJ module from buffer
; returns fs:si -> read buffer
; destroys ax,si

SetOBJBuffer	PROC
	mov	si,CurrentOBJ
	dec	si				; make relative zero
	add	si,si			; convert to word offset
	mov	fs,OBJBuffSelTablePtr	; fs:bx -> entry in table of memory block selectors
	mov	fs,fs:[si]
	mov	CurrentBaseOBJBuff,fs	; update current obj base buffer
	mov	si,IOBUFFSYSVARSIZE	; fs:si -> object module first memory block first byte (offset for system variables)
	ret
SetOBJBuffer	ENDP

;*****************************
;* READBYTE                  *
;*****************************

; read a byte from module being processed
; upon entry si -> read buffer
; return byte value in al
; updates si, fs if necessary
; destroys ax

ReadByte	PROC
	cmp	si,SIZEIOBUFFBLK	; see if at changeover point to next block
	jae	rb2				; yes

rbread:
	lods	BYTE PTR fs:[0]	; get byte value
	ret

rb2:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	mov	si,IOBUFFSYSVARSIZE	; adjust past system variables
	jmp	SHORT rbread	; read byte from new block

ReadByte	ENDP

;*****************************
;* READBYTEDECCX             *
;*****************************

; read a byte from module being processed, update record length to process
; upon entry si -> read buffer, cx holds bytes in record
; return byte value in al
; updates cx, si, fs if necessary
; destroys ax

ReadByteDecCX	PROC
	cmp	si,SIZEIOBUFFBLK	; see if at changeover point to next block
	jae	rbd2			; yes

rbdread:
	lods	BYTE PTR fs:[0]	; get byte value
	dec	cx
	ret

rbd2:
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	mov	si,IOBUFFSYSVARSIZE	; adjust past system variables
	jmp	SHORT rbdread	; read byte from new block

ReadByteDecCX	ENDP

;*****************************
;* READWORD                  *
;*****************************

; read a word from module being processed into ax
; upon entry fs:si -> read buffer
; return word value in ax
; updates si, fs if necessary
; destroys dl

ReadWord	PROC
	cmp	si,SIZEIOBUFFBLK-1	; see if at changeover point to next block
	jae	rw2				; yes
	lods	WORD PTR fs:[0]
	ret

; block changeover, read a byte at a time
rw2:
	call	ReadByte
	mov	dl,al			; low byte
	call	ReadByte
	mov	ah,al
	mov	al,dl			; high byte
	ret
ReadWord	ENDP

;*****************************
;* READWORDDECCX             *
;*****************************

; read a word from module being processed into ax, update record length to process
; upon entry fs:si -> read buffer, cx holds bytes in record
; return word value in ax
; updates cx,si, fs if necessary
; destroys dl

ReadWordDecCX	PROC
	cmp	si,SIZEIOBUFFBLK-1	; see if at changeover point to next block
	jae	rwd2			; yes
	lods	WORD PTR fs:[0]
	sub	cx,2
	ret

; block changeover, read a byte at a time
rwd2:
	call	ReadByteDecCX
	mov	dl,al			; low byte
	call	ReadByteDecCX
	mov	ah,al
	mov	al,dl			; high byte
	ret
ReadWordDecCX	ENDP

;*****************************
;* READDWORD                 *
;*****************************

; read a dword from module being processed into ax
; upon entry fs:si -> read buffer
; return dword value in ax
; updates cx,si, fs if necessary
; destroys dl

ReadDword	PROC
	cmp	si,SIZEIOBUFFBLK-3	; see if at changeover point to next block
	jae	rd2				; yes
	lods	DWORD PTR fs:[0]
	ret

; block changeover, read a byte at a time
rd2:
	call	ReadByte
	mov	dl,al			; low byte, low word
	call	ReadByte
	mov	ah,al
	mov	al,dl			; high byte, low word
	push	ax			; save low word
	call	ReadByte
	mov	dl,al			; low byte, high word
	call	ReadByte
	mov	ah,al
	mov	al,dl			; high byte, high word
	shl	eax,16			; convert high word in ax to high word portion of eax
	pop	ax				; low word portion in ax (of eax)
	ret
ReadDword	ENDP

;*****************************
;* READDWORDDECCX            *
;*****************************

; read a dword from module being processed into ax, update record length to process
; upon entry fs:si -> read buffer, cx holds bytes in record
; return dword value in ax
; updates cx,si, fs if necessary
; destroys dl

ReadDwordDecCX	PROC
	cmp	si,SIZEIOBUFFBLK-3	; see if at changeover point to next block
	jae	rdd2			; yes
	lods	DWORD PTR fs:[0]
	sub	cx,4
	ret

; block changeover, read a byte at a time
rdd2:
	call	ReadByteDecCX
	mov	dl,al			; low byte, low word
	call	ReadByteDecCX
	mov	ah,al
	mov	al,dl			; high byte, low word
	push	ax			; save low word
	call	ReadByteDecCX
	mov	dl,al			; low byte, high word
	call	ReadByteDecCX
	mov	ah,al
	mov	al,dl			; high byte, high word
	shl	eax,16			; convert high word in ax to high word portion of eax
	pop	ax				; low word portion in ax (of eax)
	ret
ReadDwordDecCX	ENDP

;*****************************
;* READINDEX                 *
;*****************************

; read an index value (byte or word) from module being processed into ax
; upon entry fs:si -> read buffer
; return word value in ax
; updates si, fs if necessary
; destroys dl

ReadIndex	PROC
	call	ReadByte	; get first byte of index
	xor	ah,ah			; zero high byte of word
	cmp	al,80h			; see if 2-byte index value
	jb	ri2				; no
	and	al,7fh			; mask off high (flag) bit
	mov	dl,al			; save as high byte
	call	ReadByte	; get low byte in al
	mov	ah,dl			; high byte to ah for ax word value

ri2:
	ret
ReadIndex	ENDP

;*****************************
;* READINDEXDECCX            *
;*****************************

; read an index value (byte or word) from module being processed into ax,
;  update record length to process
; upon entry fs:si -> read buffer, cx holds bytes in record
; return word value in ax
; updates cx, si, fs if necessary
; destroys dl

ReadIndexDecCX	PROC
	call	ReadByteDecCX	; get first byte of index
	xor	ah,ah			; zero high byte of word
	cmp	al,80h			; see if 2-byte index value
	jb	rid2			; no
	and	al,7fh			; mask off high (flag) bit
	mov	dl,al			; save as high byte
	call	ReadByteDecCX	; get low byte in al
	mov	ah,dl			; high byte to ah for ax word value

rid2:
	ret
ReadIndexDecCX	ENDP

;*****************************
;* READWORDCX                *
;*****************************

; read a word from module being processed into cx
; upon entry fs:si -> read buffer
; return word value in cx
; updates si, fs if necessary

ReadWordCX	PROC
	push	ax			; save critical register
	cmp	si,SIZEIOBUFFBLK-1	; see if at changeover point to next block
	jae	rwc2			; yes
	lods	WORD PTR fs:[0]
	mov	cx,ax			; word value in cx

rwcret:
	pop	ax				; restore critical register
	ret

; block changeover, read a byte at a time
rwc2:
	call	ReadByte
	mov	cl,al			; low byte
	call	ReadByte
	mov	ch,al			; high byte
	jmp	SHORT rwcret

ReadWordCX	ENDP

;*****************************
;* READDWORDECX              *
;*****************************

; read a dword from module being processed into ecx
; upon entry fs:si -> read buffer
; return word value in ecx
; updates si, fs if necessary

ReadDwordECX	PROC
	push	ax			; save critical register
	cmp	si,SIZEIOBUFFBLK-3	; see if at changeover point to next block
	jae	rde2			; yes
	lods	DWORD PTR fs:[0]
	mov	ecx,eax			; dword value in ecx

rderet:
	pop	ax				; restore critical register
	ret

; block changeover, read a byte at a time
rde2:
	call	ReadByte
	mov	cl,al			; low byte, low word
	call	ReadByte
	mov	ch,al			; high byte, low word
	push	cx			; save low word
	call	ReadByte
	mov	cl,al			; low byte, high word
	call	ReadByte
	mov	ch,al			; high byte, high word
	shl	ecx,16			; convert high word in cx to high word portion of ecx
	pop	cx				; low word portion in cx (of ecx)
	jmp	SHORT rderet

ReadDwordECX	ENDP

;*****************************
;* READNAMESTRING            *
;*****************************

; read a name string from module being processed
; upon entry fs:si -> read buffer, es:di -> destination,
;  cx == length of current record
; updates si -> byte after string, cx to record length after string, fs if necessary
; destroys ax,dx,di

ReadNameString	PROC
	call	ReadByte	; get 1-byte length of name
	mov	dx,cx			; save old record length
	dec	dx				; adjust for length byte
	mov	cl,al			; length to cx
	xor	ch,ch			; zap high byte of word length
	sub	dx,cx			; subtract off name length
	jcxz	rnsret		; zero byte length of name, no chars to transfer

rnsloop:
	call	ReadByte	; get name char
	stosb				; save it
	loop	rnsloop		; transfer all chars in name
	xor	al,al
	stosb				; null terminate name

rnsret:
	mov	cx,dx			; update record length after name transfer
	ret
ReadNameString	ENDP

;*****************************
;* SCANAHEAD                 *
;*****************************

; scan past buffer bytes to new buffer location
; upon entry cx == scan byte amount, fs:si -> read buffer
; updates si, fs if necessary
; destroys ax

ScanAhead	PROC
	add	si,cx			; move to new offset
	cmp	si,SIZEIOBUFFBLK	; see if overflow
	jae	sa2
	ret
sa2:
	sub	si,(SIZEIOBUFFBLK-IOBUFFSYSVARSIZE)	; wrap to next buffer position plus system variable setting
	mov	fs,fs:[OFFSET IOBuffHeaderStruc.ibhsChildPtr]	; get next block in chain
	ret
ScanAhead	ENDP

ENDS

END
