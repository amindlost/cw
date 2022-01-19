;*********************************************************************
;*   MLPASS2C.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          03/12/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker pass 2 routines part C                                   *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlpass2c
PAGE    50,80

.MODEL  SMALL

;*****************************
;* Include files             *
;*****************************

INCLUDE MLEQUATE.INC
INCLUDE MLDATA.INC
INCLUDE MLERRMES.INC
     
;*****************************
;* Public declarations       *
;*****************************

; procedures
PUBLIC  proc2_data,get_data_off_seg
PUBLIC  make_reloc_entry

; variables
PUBLIC  data_offset,data_segment,prev_data_ptr,rec_offset
PUBLIC  ovl_pub_seg_ptr,seg_fix_frame,data_seg_part
PUBLIC  seg_ovl_class_flag
PUBLIC	which_ledata,current_segind

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   is_absseg:BYTE,ovl_code_count:WORD,filename:BYTE
EXTRN   ovl_entry_id:WORD,clarclass_text:BYTE
EXTRN	compress_this:BYTE,clipper_segindex:WORD
EXTRN	ignore_fixupp_flag:BYTE

; initialized local variables

EVEN                        ; maximize speed on 8086 and better

.DATA?

; uninitialized local variables

; bytes values
EVEN
seg_ovl_class_flag  DB  ?   ; nonzero if L?DATA segment is overlay class
which_ledata	DB	?		; ==1 if Clipper code, ==2 if SYMBOLS data, zero other

; word values
EVEN
data_segment    DW  ?       ; L?DATA data segment
repeat_count    DW  ?       ; LIDATA iterated data block repeat count field
block_count     DW  ?       ; LIDATA iterated data block block count field
prev_data_ptr   DW  ?       ; pointer to start of data block/bytes in previous L?DATA
rec_offset      DW  ?       ; L?DATA record offset
current_segind	DW	?		; current segdef index, relative zero

; doubleword values
data_offset DD  ?           ; L?DATA data offset from start of program (first segment address)
seg_fix_frame   DW  ?       ; fixup frame for segment if overlaid
data_seg_part   DW  ?       ; segment partition entry owning current L?DATA segment
ovl_pub_seg_ptr DD  ?       ; pointer to overlaid public block entry of current segment partition

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR,alloc_memory:NEAR
EXTRN   write_bytes:NEAR,caseless_strcmp:NEAR
EXTRN   write_ovl_reloc:NEAR
EXTRN	fixup_clipper_tokens:NEAR,process_symbol_table:NEAR

;*****************************
;* PROC2_DATA                *
;*****************************

; process LIDATA and LEDATA records
; destroys ax,bx,dx,di

proc2_data  PROC
    push    cx              ; save critical register
    push    si
    push    es

	mov	ignore_fixupp_flag,0	; flag don't ignore fixupps
    call    get_data_off_seg    ; get data record offset and segment
    dec cx                  ; adjust for record checksum byte, cx holds count of bytes to write

; check if absolute segment, if so, ignore write
    cmp is_absseg,0         ; check flag
    jne pd_ret              ; set, bypass L?DATA processing

; check which type of data record to process
    cmp prev_flag,1         ; check if LEDATA record
    jne pd_6                ; no
    call    proc2_ledata    ; process LEDATA record
    jmp SHORT pd_ret        ; done

pd_6:
    cmp prev_flag,2         ; check if LIDATA record
    jne pd_ret              ; no (shouldn't happen, but don't process based on bogus prev_flag value)
    call    proc2_lidata    ; process LIDATA record

pd_ret:
    pop es                  ; restore critical register
    pop si
    pop cx
    ret
proc2_data  ENDP

;*****************************
;* GET_DATA_OFF_SEG          *
;*****************************

; get data record offset and segment, save to memory variables
; destroys all registers except ds and di

get_data_off_seg    PROC
    push    bp
    mov is_absseg,0         ; init absolute segment flag off
    mov si,prev_read_ptr    ; point to previous record position
    mov cx,prev_rec_len     ; get length of record
    mov ax,buffer_end
    mov bp,ax               ; check only for end of physical buffer overflow

    mov dl,es:[si]          ; get low byte of segment record index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  gds_2               ; okay
    xor si,si               ; wrap to beginning of buffer
gds_2:
    cmp dl,80h              ; check if two byte index value
    jb  gds_3               ; no, continue
    mov dl,es:[si]          ; get second byte, actual value
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  gds_3               ; okay
    xor si,si               ; wrap to beginning of buffer

gds_3:
    xor dh,dh               ; zap high byte of segment record index

    dec dl                  ; make segment index relative zero
    mov ax,dx               ; save segment index relative zero
	mov	current_segind,ax	; save to memory variable for Clipper compression use

    shl dx,1                ; convert dx to word offset into array
    mov bx,OFFSET DGROUP:seg_partent_indptr  ; set bx to base of segment partition address array
    add bx,dx               ; bx -> array element holding segment partition entry address
    push    es              ; save es -> file i/o buffer
    mov es,[bx]             ; es -> segment partition entry
    mov bx,ax               ; save segment index relative zero
    mov dx,es               ; save segment partition entry pointer
    mov ax,es:[4]           ; see if back pointer to master segdef entry
    or  ax,ax               ; should be zero only if absolute segment (partition pointer really segdef pointer)
    jne gds_not_abs

; es should -> absolute segment segdef entry, but check acbp byte
; position to further verify

    mov al,es:[26]          ; get acbp byte
    and al,0e0h             ; get align field
    jne gds_internal        ; acbp byte doesn agree that an absolute segment
    mov is_absseg,1         ; set absolute segment flag
    jmp NEAR PTR gds_3a     ; allow file i/o buffer position adjustment

; inconsistent absolute segment indications
gds_internal:
    mov ax,INTERNAL_ERR
    mov cx,10               ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

gds_not_abs:
    xchg    bx,ax           ; bx -> master segdef entry from segment partition entry back pointer
                            ; ax holds segment index relative zero

    cmp is_clarion,0        ; see if clarion flag set
    je  gds_3a              ; no
    cmp obj_ovl_flag,0      ; see if module in overlay
    je  gds_3a              ; no
    test BYTE PTR es:[15],80h   ; see if overlaid segment
    jne gds_3a              ; yes

    push    bx              ; save critical register
    push    dx
    push    di

    mov dx,ax               ; dx holds segment index relative zero

; clarion switch set, nonoverlaid segment, module in overlay
; see if _DT or _DAT private segment, class CLARION
    mov al,es:[11]          ; get ACBP byte
    and al,1ch              ; get combine value
    jne gds_clardone        ; not a private segment

    push    es              ; save es -> segment partition entry
    mov es,bx               ; es -> master segdef entry
    les bx,es:[12]          ; es:bx -> class name
    add bx,8                ; adjust bx past 2 doubleword pointers
    mov di,OFFSET DGROUP:clarclass_text ; ds:di -> overlay class name

; perform a caseless string compare on the names
    call    caseless_strcmp ; al return 0 if match, 1 if no match
    pop es                  ; restore es -> segment partition entry
    or  al,al               ; see if match
    jne gds_clardone        ; no

    mov ax,dx               ; segment index relative zero in ax (al holds value, ah==0)
    mov bl,3
    div bl                  ; get multiple clarion segment grouping (_dt, _dat, _code) count
    mov bl,al               ; save clarion segment grouping count
    xor bh,bh               ; zero high byte

    mov ax,ovl_entry_id     ; get entry count of overlaid segments
    inc ax                  ; adjust for first current entry
    add ax,bx               ; adjust for multiple clarion segment grouping count, if any
    shl ax,1                ; x2

    mov es,es:[4]           ; get master segdef entry
    les bx,es:[8]
    add bx,8                ; es:bx -> segment name
    cmp BYTE PTR es:[bx],'_'
    jne gds_clardone        ; no match against clarion data segment name
    cmp BYTE PTR es:[bx+1],'D'
    jne gds_clardone
    cmp BYTE PTR es:[bx+2],'T'  ; see if _DT segment
    je  gds_data_id         ; yes
    cmp BYTE PTR es:[bx+2],'A'  ; see if _DA[T] segment (assume 'T')
    jne gds_clardone        ; no, no match

; _DT segment
    inc ax                  ; offset by 1 for _DAT segment (after _DT segment)

gds_data_id:
    mov ovl_data_id,ax      ; save overlaid data identifier

gds_clardone:
    pop di                  ; restore critical register
    pop dx
    pop bx

gds_3a:
    pop es                  ; restore es -> file i/o buffer
    mov al,es:[si]          ; get low byte of data offset
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  gds_4               ; okay
    xor si,si               ; wrap to beginning of buffer
gds_4:
    mov ah,es:[si]          ; get high byte of data offset
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  gds_5               ; okay
    xor si,si               ; wrap to beginning of buffer

gds_5:
    cmp is_absseg,0         ; see if absolute segment
    je  gds_not_abs2        ; no
    xor ax,ax
    mov ovl_code_id,ax      ; reset overlay class variables
    mov seg_ovl_class_flag,al
    pop bp                  ; bypass data offset computation code
    ret

gds_not_abs2:
    mov rec_offset,ax       ; save data record offset
    mov WORD PTR data_offset,ax ; save data offset to memory variable
    mov WORD PTR data_offset+2,0    ; zero high word
    mov prev_data_ptr,si    ; keep pointer to start of data bytes/block

    push    es              ; save es -> file i/o buffer
    mov es,dx               ; es -> segment partition entry

    xor ax,ax
    mov ovl_code_id,ax      ; init overlay code identifier flag to zero (in case of overlay specified but none used)
    mov seg_ovl_class_flag,al   ; init segment is overlay class flag
    cmp ovl_count,ax        ; see if any overlays
    je  gds_6               ; no

    mov ax,es               ; get -> segment partition entry
    mov data_seg_part,ax    ; save pointer to data segment partition entry owning L?DATA record
    test BYTE PTR es:[15],80h   ; see if overlaid segment
    jne gds_5a              ; yes

    mov es,es:[4]           ; es -> master segdef entry
    mov dx,es               ; keep -> segdef entry in dx
    test    BYTE PTR es:[28],1  ; see if overlay class segment
    mov es,ax               ; restore es -> segment partition entry
    je  gds_6               ; not overlay class

; overlaid or overlay class segment
gds_5a:
    mov seg_ovl_class_flag,1   ; set the segment is overlay class flag

; this segment may not be in the table, making the overlaid public pointers
; set invalid, but won't be used if that is the case
    mov ax,es:[6]           ; get offset of overlaid public block -> segment partition entry
    mov WORD PTR ovl_pub_seg_ptr,ax ; save it for fixup to vector reference
    mov ax,es:[8]           ; get segment of overlaid public block -> segment partition entry
    mov WORD PTR ovl_pub_seg_ptr+2,ax   ; save it

    test    BYTE PTR es:[15],80h    ; see overlaid bit set
    je  gds_5b              ; no

; segment is overlaid, set the ovl_code_id flag to proper nonzero value
    xor ax,ax
;***    add WORD PTR data_offset,48 ; bump bytes to write offset to compensate for near vectors
;***    adc WORD PTR data_offset+2,ax   ; carry to high word
    mov seg_fix_frame,ax    ; fixup frame of this segment is zero
    mov ax,es:[4]           ; get overlay identifier code
    mov ovl_code_id,ax      ; put value in memory variable
    jmp SHORT gds_7         ; bypass the data_offset and data_segment calculation of values

; get segment frame for far fixups within segment (if overlays)
gds_5b:
    push    es              ; save es -> segment partition entry
    mov es,dx               ; es -> segdef entry
    mov ax,es:[2]           ; get low word of offset
    mov dx,es:[4]           ; get high word of offset
    shr dx,1                ; convert to segment (paragraph) value in ax
    rcr ax,1
    shr dx,1
    rcr ax,1
    shr dx,1
    rcr ax,1
    shr dx,1
    rcr ax,1                ; ax holds segment value
    mov seg_fix_frame,ax    ; keep fixup frame of this segment
    pop es                  ; restore es -> segment partition entry

gds_6:
    mov ax,es:[0]           ; get offset of segment partition entry
    add WORD PTR data_offset,ax ; update offset from start of program
    adc WORD PTR data_offset+2,0    ; carry to high word
    mov es,bx               ; es -> segdef entry
    mov ax,es:[2]           ; get low word of segment offset
    add WORD PTR data_offset,ax ; update low word of offset from start of program
    mov bx,es:[4]           ; get high word of segment offset
    adc WORD PTR data_offset+2,bx   ; update high word of offset from start of program

; convert offset in bx:ax to paragraphs
    shr bx,1
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1                ; bx should be zero by the final shift
    rcr ax,1                ; /16
    mov data_segment,ax     ; save segment of data

gds_7:
    pop es                  ; restore es -> file i/o buffer
    pop bp
    ret
get_data_off_seg    ENDP

;*****************************
;* PROC2_LEDATA              *
;*****************************

; process ledata record
; upon entry cx == count of data bytes, es:[si] -> first data byte
; destroys ax,bx,cx,dx,di

proc2_ledata    PROC
	xor	al,al
	mov	which_ledata,al		; init LEDATA flag
	cmp	compress_this,al	; see if compressing Clipper code
	je	pl_relloop			; no

; check if Clipper code (LEDATA segment index < clipper_segindex) or
; SYMBOLS data (LEDATA segment index == clipper_segindex)
	mov	ax,current_segind
	cmp	ax,clipper_segindex
	ja	pl_relloop			; not Clipper code or symbols data
	je	pl_symbols			; symbols data

; clipper code
	mov	which_ledata,1		; flag Clipper code to following fixups, if any
	call	fixup_clipper_tokens	; fixup tokens in Clipper code
	jmp	SHORT pl_relloop

; symbols data 
pl_symbols:
	mov	which_ledata,2		; flag SYMBOLS data to following fixups, if any
	call	process_symbol_table	; process the symbol table data

pl_relloop:
    cmp data_fixup_count,0  ; see if any relocation table entries
    je  pl_2                ; no

    dec data_fixup_count    ; drop count of entries
    mov bx,OFFSET DGROUP:data_fixup_flag    ; bx -> base of fixup flag array
    mov ax,data_fixup_count ; get count of fixups in data record
    shl ax,1                ; convert to word offset
    add bx,ax               ; bx -> last used array element where data record offset of fixup stored
    mov dx,[bx]             ; dx holds data record offset

    cmp ovl_code_id,0       ; see if relocation item write is for an overlay
    je  pl_1                ; no
    call    write_ovl_reloc ; write overlay file relocation entry
    jmp SHORT pl_1a         ; bypass regular relocation entry handler

pl_1:
    call    make_reloc_entry

pl_1a:
    jmp SHORT pl_relloop    ; loop back and check for next item

pl_2:
    call    write_bytes     ; write data to program's memory image
    ret
proc2_ledata    ENDP

;*****************************
;* PROC2_LIDATA              *
;*****************************

; process lidata record
; upon entry cx == count of data bytes, es:[si] -> first data byte
; destroys ax,bx,cx,dx,di

proc2_lidata    PROC
    xor bx,bx               ; zero initial position in data record
    cmp cx,IDATA_BLK_MAX    ; check if iterated data block is too large
    jbe pli_2               ; no

    mov ax,WORD PTR prev_pos_adj    ; set file position adjustment to previous record position adjustment
    mov WORD PTR file_pos_adj,ax
    mov ax,WORD PTR prev_pos_adj+2
    mov WORD PTR file_pos_adj+2,ax
    mov dx,OFFSET DGROUP:filename
    mov ax,IDATA_BLK_SIZE_ERR   ; iterated data block larger than 512 bytes
    jmp NEAR PTR link_error ; transfer control to error handler

pli_2:
    jcxz    pli_ret         ; no more bytes to write, all done
    call    recurse_lidata  ; use recursion routine to extract data out of lidata record
    jmp SHORT pli_2         ; loop until no more bytes

pli_ret:
    mov data_fixup_count,0  ; re-init count of relocation items in array
    ret
proc2_lidata    ENDP

;*****************************
;* RECURSE_LIDATA            *
;*****************************

; use recursion to get all data out of lidata record
; upon entry es:[si] -> current data byte, cx == size of data block,
; bx == position in record
; destroys all noncritical registers

recurse_lidata  PROC
    mov al,es:[si]          ; get low byte of repeat count
    inc si
    inc bx                  ; bump position in record
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  rl_2                ; okay
    xor si,si               ; wrap to beginning of buffer
rl_2:
    mov ah,es:[si]          ; get high byte of repeat count
    inc si
    inc bx                  ; bump position in record
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  rl_3                ; okay
    xor si,si               ; wrap to beginning of buffer
rl_3:
    mov repeat_count,ax     ; save to memory variable

    mov al,es:[si]          ; get low byte of block count
    inc si
    inc bx                  ; bump position in record
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  rl_4                ; okay
    xor si,si               ; wrap to beginning of buffer
rl_4:
    mov ah,es:[si]          ; get high byte of block count
    inc si
    inc bx                  ; bump position in record
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  rl_5                ; okay
    xor si,si               ; wrap to beginning of buffer
rl_5:
    mov block_count,ax      ; save to memory variable

    push    si              ; save buffer position
    push    bx              ; save record position
    push    cx              ; save record iterated data block length

rl_reploop:
    pop cx                  ; restore record length
    pop bx                  ; restore record position
    pop si                  ; restore buffer position
    push    si              ; restore values to stack
    push    bx
    push    cx

    cmp block_count,0       ; see if nested iterated data blocks
    je  rl_data             ; no

; nested iterated data blocks
    mov ax,block_count      ; get total number of blocks to loop through
rl_blkloop:
    push    ax              ; save current number of blocks left to loop thru
    push    repeat_count    ; save repeat value
    push    block_count     ; save block value
    call    recurse_lidata  ; nest down one level
    pop block_count         ; restore block value
    pop repeat_count        ; restore repeat value
    pop ax                  ; restore number of blocks left to loop thru
    dec ax                  ; one iteration completed
    jne rl_blkloop          ; loop is not complete, using Z flag from previous decrement
    jmp SHORT rl_end_reploop    ; loop is complete, end this iteration of the repeat loop

; block size is zero, data follows
rl_data:
    mov al,es:[si]          ; get length of data to write
    inc si
    inc bx                  ; bump position in record
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  rl_6                ; okay
    xor si,si               ; wrap to beginning of buffer
rl_6:
    cmp data_fixup_count,0  ; see if any relocation table entries
    je  rl_7                ; no

; possible relocation entries
    push    ax
    mov dx,bx
    xor ah,ah               ; zero high byte of data bytes
    add dx,ax               ; bx --> dx == range of bytes to check for relocation entries
    mov di,OFFSET DGROUP:data_fixup_flag    ; di -> base of fixup flag array
    mov ax,data_fixup_count ; get count of fixups in data record

    dec ax                  ; make count relative zero
    shl ax,1                ; convert to word offset
    add di,ax               ; di -> last used array element where data record offset of fixup stored

rl_reloc:
    cmp [di],bx             ; check low bounds
    jb  rl_6b               ; below lowest, done
    cmp [di],dx             ; check high bounds
    jae rl_6a               ; above highest bounds (equal to is out)

; make a relocation entry
    push    dx              ; save critical registers
    push    di

    mov dx,[di]             ; get record offset where relocation item goes
    sub dx,bx               ; adjust to relative offset from start of data

    cmp ovl_code_id,0       ; see if relocation item write is for an overlay
    je  rel_rl2             ; no
    call    write_ovl_reloc ; write overlay file relocation entry
    jmp SHORT rel_rl3       ; bypass regular relocation entry handler

rel_rl2:
    call    make_reloc_entry    ; make relocation item entry

rel_rl3:
    pop di                  ; restore critical registers
    pop dx

rl_6a:
    cmp di,OFFSET DGROUP:data_fixup_flag    ; if equal than no more entries
    je  rl_6b               ; no more entries
    sub di,2                ; back up one word array element
    jmp SHORT rl_reloc      ; loop back for next check

rl_6b:
    pop ax

; write the data bytes
rl_7:
    push    cx              ; save record's total data length (in case of repeat loop termination)
    mov cl,al               ; get data bytes to write in cl
    xor ch,ch               ; zap high byte
    push    cx              ; save data bytes written
    call    write_bytes     ; write the data bytes to program's memory image
    pop ax                  ; get data bytes written
    mov dx,ax               ; save in dx
    add ax,WORD PTR data_offset ; add to data offset
    mov WORD PTR data_offset,ax ; update data offset low word
    adc WORD PTR data_offset+2,0    ; carry to high word
    pop cx                  ; restore record's total data length (in case of repeat loop termination)
    sub cx,dx               ; subtract off data bytes written
    add bx,dx               ; bump record position

rl_end_reploop:
    dec repeat_count        ; an iteration of the repeat loop was completed
;***    cmp repeat_count,0      ; see if loop is complete
    je  rl_end              ; yes
    jmp NEAR PTR rl_reploop ; no, go back for next iteration

rl_end:
    add sp,6                ; trash old record length, record and buffer position values on stack
    ret                     ; repeat loop complete, return to next highest level or proc2_lidata
recurse_lidata  ENDP

;*****************************
;* MAKE_RELOC_ENTRY          *
;*****************************

; make relocation entry
; upon entry dx == record offset
; destroys ax,dx,di

make_reloc_entry    PROC
    push    bx              ; save critical register
    push    si
    push    es
    mov ax,alloc_relblk_ptr
    or  ax,ax               ; check if any relocation blocks were previously allocated
    jne mre_2               ; yes

; make initial relocation block allocation
    mov bx,REL_BLK_SIZE     ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_relblk_ptr,ax ; update last allocated block pointer
    mov first_relblk_ptr,ax ; update first allocated block pointer
    mov es,ax               ; es -> block
    mov WORD PTR es:[0],0   ; zero count of entries in block
    mov WORD PTR es:[2],0   ; zero pointer to next entry

mre_2:
    mov es,ax               ; es -> current block
    cmp WORD PTR es:[0],REL_ITEM_COUNT  ; see if any free items in block
    jb  mre_3               ; yes
    call    make_reloc_blk  ; no free entries in block, make a new block

mre_3:
    mov bx,es:[0]           ; get entry count
    inc bx                  ; adjust for system info entry at beginning of block
    shl bx,1
    shl bx,1                ; make bx a doubleword offset (4 bytes/item)
    mov ax,data_segment
    mov es:[bx+2],ax        ; save segment of relocation entry
    mov ax,WORD PTR data_offset ; get program offset of data low word
    mov di,WORD PTR data_offset+2   ; get program offset high word
    add ax,dx               ; add in offset in data record
    adc di,0                ; carry to high word, di:ax == program offset+record offset
    xor si,si
    mov dx,es:[bx+2]        ; dx == relocation segment
    shl dx,1
    rcl si,1                ; x2
    shl dx,1
    rcl si,1                ; x4
    shl dx,1
    rcl si,1                ; x8
    shl dx,1
    rcl si,1                ; x16, si:dx == absolute byte value of data segment
    sub ax,dx
    sbb di,si               ; di:ax == relocation entry offset, di should be zero
    mov es:[bx],ax          ; save relocation item offset

    inc WORD PTR es:[0]     ; bump count of entries in relocation block
    inc number_reloc        ; bump global count of relocation items
    cmp number_reloc,RELOC_MAX  ; check if too many relocation items
    ja  mre_bounds          ; too many items
    pop es                  ; restore critical register
    pop si
    pop bx
    ret                     ; done

mre_bounds:
    mov dx,OFFSET DGROUP:filename
    mov ax,RELOC_COUNT_ERR  ; more than 32768 .EXE file relocation table entries
    jmp NEAR PTR link_error ; transfer control to error handler
make_reloc_entry    ENDP

;*****************************
;* MAKE_RELOC_BLK            *
;*****************************

; make relocation entry block
; return segment of block in es
; destroys ax,bx,es

make_reloc_blk  PROC
    push    dx              ; save critical register
    mov dx,alloc_relblk_ptr ; keep previously last allocated block segment
    mov bx,REL_BLK_SIZE     ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_relblk_ptr,ax ; update last allocated block pointer

    mov es,dx               ; es -> old block
    mov es:[2],ax           ; update old block to point to new block

    mov es,ax               ; es -> new block
    mov WORD PTR es:[0],0   ; zero count of entries in block
    mov WORD PTR es:[2],0   ; zero pointer to next entry
    pop dx                  ; restore critical register
    ret
make_reloc_blk  ENDP

END
