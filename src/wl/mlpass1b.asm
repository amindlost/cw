;*********************************************************************
;*   MLPASS1B.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/21/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker pass 1 routines part B                                   *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlpass1a
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
PUBLIC  proc1_lnames,proc1_segdef,init_segdef_entry,make_segdef_blk
PUBLIC  make_segpart_blk

; variables
PUBLIC  is_absseg,dgrouptext,class_name_index,first_lnamesblk_ptr

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

EXTRN   parse_complete:BYTE,name_field:BYTE,filename:BYTE
EXTRN   mod_ovl_count:WORD,clip_libnum:WORD
EXTRN	maybe_clipper:BYTE,symbol_overflow:BYTE
EXTRN	clipper_segdef_ptr:WORD,clipper_symseg_ptr:WORD,clipper_segindex:WORD
EXTRN	is_local:BYTE

; initialized local variables

; byte values
EVEN                        ; maximize speed on 8086 and better
_textflag   DB  0           ; nonzero if DOSSEG and segment _TEXT has 16 bytes inserted
first_lnamesblk_ptr DW  0   ; segment of first allocated lnames logical names block

.DATA?

; uninitialized local variables

; byte values
EVEN
hash_used   DB  ?           ; nonzero if lnames hash code previously used
EVEN
acbp_byte   DB  ?           ; ACBP byte in segdef record
EVEN
is_absseg   DB  ?           ; nonzero if segment is absolute
EVEN
is_common   DB  ?           ; nonzero if segment is COMMON type
EVEN
ovl_seg_flag    DB  ?       ; nonzero if current segment is in an overlay

; word values
EVEN
segment_name_index  DW  ?   ; segment name index in segdef record
class_name_index    DW  ?   ; class name index in segdef record
frame_number    DW  ?       ; frame number in segdef record

; doubleword values
old_lnamesent_ptr   DD  ?   ; previous lnames entry pointer
segment_length  DD  ?       ; segment length in segdef record
no_align_length DD  ?       ; segment length without alignment adjustment, saved to partition entry

;*****************************
;* Constant data             *
;*****************************

.CONST

EVEN
_texttext   DB  '_TEXT',0   ; segment name to check against for DOSSEG use
dgrouptext  DB  'DGROUP',0  ; segment name to check to set is_dgroup flag
symbolstext	DB	'SYMBOLS',0	; segment/class to check for Clipper code

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR
EXTRN   get_name:NEAR,get_hash:NEAR,alloc_memory:NEAR
EXTRN   error_bx_pos:NEAR,error_read_buff_pos:NEAR
EXTRN   chk_seg_ovl_class:NEAR

;*****************************
;* PROC1_LNAMES              *
;*****************************

; process LNAMES record
; upon entry cx=record length,bp=buffer_end,al=record type
; es:si -> first byte of record past record length
; destroys ax,bx,dx,di
; updates si

proc1_lnames    PROC
pl_loop:
    mov ax,current_lnames   ; get count of lnames logical names in file
    cmp ax,LNAMES_MAX       ; see if at or beyond maximum
    jb  pl_2                ; no
    mov dx,OFFSET DGROUP:filename
    mov ax,LNAMES_COUNT_ERR ; too many LNAMES logical names in file
    jmp NEAR PTR error_read_buff_pos    ; transfer control to error handler

pl_2:
    inc ax                  ; bump count of lnames logical names
    mov current_lnames,ax   ; save back to global memory variable
    mov bx,ax               ; save value
    mov di,OFFSET DGROUP:name_field ; point di at field to place name
	mov	is_local,0			; flag not a local for get_name
    call    get_name        ; get lnames logical name from record
    mov di,OFFSET DGROUP:name_field ; point di at name
    cmp BYTE PTR [di],0     ; check if null name
    jne pl_save_name        ; no, non-null name
    mov ax,OFFSET DGROUP:zero_table ; point to known zero value
    mov dx,DGROUP
    jmp SHORT pl_array

pl_save_name:
    call    get_lnames_entry    ; find or create entry, return segment:offset pointer

pl_array:
    mov di,bx               ; get current lnames logical names count in di
    dec di                  ; make count relative zero
    shl di,1
    shl di,1                ; make lnames index value a doubleword offset into index pointer array
    mov bx,OFFSET DGROUP:lnames_ent_indptr  ; set bx to base of lnames index pointer array
    add bx,di               ; point to array element
    mov [bx],ax             ; save lnames logical name offset
    mov [bx+2],dx           ; save lnames logical name segment

pl_checksum:
    cmp cx,1                ; check if at checksum byte
    jbe pl_out              ; yes, gobble it and return
    jmp NEAR PTR pl_loop    ; loop back for next definition

pl_out:
    inc si                  ; bump past checksum byte
    cmp si,bp               ; check boundary conditions
    jb  pl_ret              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pl_ret:
    ret
proc1_lnames    ENDP

;*****************************
;* GET_LNAMES_ENTRY          *
;*****************************

; find or make entry for lnames logical name
; upon entry di -> warplink data location holding logical name
; returns segment:offset pointer to entry in dx:ax
; destroys ax,dx,di

get_lnames_entry    PROC
    push    bx              ; save critical registers
    push    cx
    push    si
    push    es
    xor al,al
    mov hash_used,al        ; init hash_used flag
    mov si,di               ; si -> name to get hash code of
    call    get_hash        ; get hash code of name in ax

    mov si,OFFSET DGROUP:lnames_hash    ; si -> base of hash pointers to lnames entries
    shl ax,1
    shl ax,1                ; convert ax to doubleword offset
    add si,ax               ; si points to proper name hash code entry
    push    si              ; save pointer to hash code entry
    cmp WORD PTR [si+2],0   ; is hash code used (nonzero segment value)
    jne gle_2
    mov ax,alloc_lnamesblk_ptr
    mov es,ax               ; es -> current block
    or  ax,ax               ; check if any lnames blocks were previously allocated
    jne gle_3               ; yes

; make initial lnames allocation
    mov bx,LNAMES_BLK_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov first_lnamesblk_ptr,ax  ; save first lnames block pointer
    mov alloc_lnamesblk_ptr,ax  ; update last allocated block pointer
    mov es,ax               ; es == current (new) block segment
    xor bx,bx
    mov es:[bx],LNAMES_LIST_SIZE    ; all lnames entry space is free
    mov WORD PTR es:[2],bx  ; zero pointer to next entry
    jmp SHORT gle_3         ; bypass duplicate name /hash collision code

; hash code used, check whether duplicate name or hash collision
gle_2:
    inc hash_used           ; nonzero value flags that hash code was previously used
    push    di              ; save di pointing to name
    mov ax,[si+2]           ; get segment pointer to lnames entry
    mov dx,[si]             ; get offset pointer to lnames entry

gle_testloop:
    mov es,ax               ; es == segment of lnames entry testing name against
    mov di,dx               ; di == offset of lnames entry testing name against
    mov bx,di               ; save entry offset in bx
    add di,8                ; scan past segment:offset high and low pointers
    pop si                  ; si -> name
    push    si

; ds:si -> name, es:di -> name to test against
gle_byteloop:
    mov al,[si]
    or  al,es:[di]          ; see if both values are zero (matched to null terminator)
    jne  gle_4              ; at least one char left in at least one string

; names match, return pointer to name
    pop di                  ; trash hash point on stack
    pop di                  ; restore di -> name
    mov ax,dx               ; ax holds name offset
    mov dx,es               ; dx holds name segment
    jmp NEAR PTR gle_out

gle_4:
    cmpsb                   ; compare a nonzero byte in the two names
    je  gle_byteloop        ; bytes match, loop for next byte test
    mov WORD PTR old_lnamesent_ptr,bx   ; save offset of nonmatching lnames entry
    mov WORD PTR old_lnamesent_ptr+2,es ; save segment of nonmatching lnames entry
    jc  gle_5               ; old name greater than new name

; new name greater than old name
    mov ax,es:[bx+6]        ; get segment of lnames entry having a greater name
    mov dx,es:[bx+4]        ; get offset of lnames entry having greater name
    xor cl,cl               ; flag new > old, update high pointer
    jmp SHORT gle_6         ; check for null pointer

; old name greater than new name
gle_5:
    mov ax,es:[bx+2]        ; get segment of lnames entry having lesser name
    mov dx,es:[bx]          ; get offset of lnames entry having lesser name
    mov cl,1                ; flag old > new update low pointer

gle_6:
    or  ax,ax               ; check if a null pointer (segment value in ax=0)
    jne gle_testloop        ; no, keep checking entries

    mov ax,alloc_lnamesblk_ptr  ; get last allocated block pointer
    mov es,ax               ; es -> lnames block
    pop di                  ; restore di -> name

gle_3:
; get length of name in dx INCLUDING zero terminator
    xor bx,bx               ; bx will offset into name
    mov dx,1                ; init length of name
gle_lenloop:
    cmp BYTE PTR [bx+di],0  ; check if at end of name
    je  gle_7               ; yes, at end
    inc dx                  ; bump count of chars in string
    inc bx                  ; point to next char
    jmp SHORT gle_lenloop   ; check next char

gle_7:
    add dx,8                ; add in space for segment:offset high and low pointers
    mov ax,es:[0]           ; get free lnames entry space

    push    dx              ; save length of name entry on stack
    cmp dx,ax               ; see if lnames entry will fit in block
    ja  gle_8               ; no, it won't

; name will fit into current pubdef names block
    mov dx,LNAMES_LIST_SIZE
    sub dx,ax               ; get beginning slot for lnames entry
    jmp SHORT gle_9         ; bypass block allocation code

; not enough room for name in this block, allocate another lnames block
gle_8:
    call    make_lnames_blk
    xor dx,dx               ; first name entry starts at offset zero

gle_9:
    pop si                  ; get length of entry
    mov ax,es:[0]           ; get free lnames entry space
    sub ax,si               ; subtract name length including terminator byte and hi/low pointer space
    mov es:[0],ax           ; update free lnames entry space

    add dx,12               ; adjust name entry offset past 2 system info words and hi/low pointers
    mov bl,cl               ; save hi/low flag from cl
    mov cx,si               ; cx == bytes to transfer (length of entry)
    sub cx,8                ; adjust for hi/low pointer doublewords
    mov si,di               ; si -> lnames logical name
    mov di,dx               ; di -> destination of name in lnames block
    shr cx,1                ; convert to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
    mov cl,bl               ; restore hi/low flag to proper state

    sub dx,8                ; back dx up to beginning of entry
    mov di,dx
    xor ax,ax
    mov es:[di],ax          ; zero segment:offset low and high pointers
    mov es:[di+2],ax
    mov es:[di+4],ax
    mov es:[di+6],ax

    pop si                  ; get pointer to hash code array entry
    mov dx,es               ; get segment of new lnames entry in dx
    mov ax,di               ; get offset of new lnames entry in ax

    cmp hash_used,0         ; check if hash code was used
    jne gle_10              ; yes it was

; hash code previously unused, update hash code pointer
    mov [si],ax             ; save offset
    mov [si+2],dx           ; save segment
    jmp SHORT gle_out       ; done, restore critical registers and return

gle_10:
    les di,old_lnamesent_ptr    ; get segment:offset of previous lnames entry
    or  cl,cl               ; see which pointer to update
    jne gle_11              ; nonzero, update low name pointer
    mov es:[di+4],ax        ; update offset of high name pointer
    mov es:[di+6],dx        ; update segment of high name pointer
    jmp SHORT gle_out       ; done, restore critical registers and return

gle_11:
    mov es:[di],ax          ; update offset of low name pointer
    mov es:[di+2],dx        ; update segment of low name pointer

gle_out:
    pop es                  ; restore critical registers
    pop si
    pop cx
    pop bx
    ret
get_lnames_entry    ENDP

;*****************************
;* MAKE_LNAMES_BLK           *
;*****************************

; make a new lnames block
; return segment of new block in es
; destroys ax,es

make_lnames_blk PROC
    push    dx              ; save critical register
    mov dx,alloc_lnamesblk_ptr  ; keep previously last allocated block segment
    mov bx,LNAMES_BLK_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_lnamesblk_ptr,ax  ; update last allocated block pointer

    mov es,dx               ; es -> old block
    mov es:[2],ax           ; update old block to point to new block

    mov es,ax               ; es -> new block
    mov es:[0],LNAMES_LIST_SIZE ; all name list space is free
    mov WORD PTR es:[2],0   ; zero pointer to next entry
    pop dx                  ; restore critical registers
    ret
make_lnames_blk ENDP

;*****************************
;* PROC1_SEGDEF              *
;*****************************

; process SEGDEF record
; upon entry cx=record length,bp=buffer_end,al=record type
; es:si -> first byte of record past record length
; updates si
; destroys ax,bx,dx,di,es

proc1_segdef    PROC
    xor ax,ax
    mov frame_number,ax     ; init frame number to zero
    mov al,es:[si]          ; get acbp byte
    mov acbp_byte,al        ; save to memory
    mov dl,al               ; save acbp byte in dl
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_2                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_2:
    mov is_absseg,0         ; init absolute segment flag
    mov is_common,0         ; init common segment flag
    test    dl,0e0h         ; check if A bit field is zero
    jne ps_get_seglen       ; nonzero, no frame number

; A bit field is zero (absolute segment), frame number and offset bytes exist
    mov is_absseg,1         ; set absolute segment flag
    mov dl,es:[si]          ; get low byte of frame number
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_3                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_3:
    mov dh,es:[si]          ; get high byte of frame number
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_4                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_4:
    mov frame_number,dx     ; save frame number
    inc si                  ; scan past offset byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_get_seglen       ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_get_seglen:
    mov dl,es:[si]          ; get low byte of segment length
    mov bx,si               ; keep pointer to first index byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_6                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_6:
    mov dh,es:[si]          ; get high byte of segment length
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_7                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_7:
    mov WORD PTR segment_length,dx  ; save segment length low word
    mov WORD PTR no_align_length,dx ; save to no alignment adjustment segment length
    xor ax,ax
    mov WORD PTR segment_length+2,ax    ; zero segment length high word
    mov WORD PTR no_align_length+2,ax

    xor ah,ah               ; zero high byte
    mov al,es:[si]          ; get low byte of segment name index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_8                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
    jmp SHORT ps_8          ; bypass error message code

ps_bad_segnam:
    mov dx,OFFSET DGROUP:filename
    mov ax,SEGNAM_IND_ERR   ; Invalid SEGDEF segment name index value
    jmp NEAR PTR error_bx_pos   ; transfer control to error handler

ps_bad_classnam:
    mov dx,OFFSET DGROUP:filename
    mov ax,CLASSNAM_IND_ERR ; Invalid SEGDEF class name index value
    jmp NEAR PTR error_bx_pos   ; transfer control to error handler

ps_8:
    or  al,al               ; make sure value is nonzero
    je  ps_bad_segnam       ; zero value is invalid
    cmp al,80h              ; check if more than one byte index value
    jb  ps_10               ; no, continue
    and al,7fh              ; mask off high bit
    mov ah,al               ; transfer to high word
    mov al,es:[si]          ; get second byte, actual value
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_10               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_10:
    mov segment_name_index,ax   ; save segment name index

    xor ah,ah               ; zero high byte of index
    mov al,es:[si]          ; get low byte of class name index
    mov bx,si               ; keep pointer to first index byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_12               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_12:
    or  al,al               ; make sure value is nonzero
    je  ps_bad_classnam     ; zero value is invalid
    cmp al,80h              ; check if more than one byte index value
    jb  ps_13               ; no, continue
    and al,7fh              ; mask off high bit
    mov ah,al               ; transfer to high word
    mov al,es:[si]          ; get second byte, actual value
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_13               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_13:
    mov class_name_index,ax ; save class name index

; gobble the overlay name index value
    mov al,es:[si]          ; get low byte of overlay name index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_13a              ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_13a:
    cmp al,80h              ; check if more than one byte index value
    jb  ps_get_segdef       ; no, continue
    inc si                  ; scan past second byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_get_segdef       ; okay
    call    load_file       ; load next portion of file into buffer, at end position

; find or create segdef entry, return segment in es
ps_get_segdef:
    push    es              ; save critical register
    call    get_segdef_entry

    mov di,current_segdef   ; get number current segdef record
    shl di,1                ; convert di to word offset
    mov bx,offset DGROUP:seg_defent_indptr  ; set bx to base of segment index pointer array
    mov [bx+di],es          ; set array element pointer to segdef entry segment

    mov al,es:[26]          ; get acbp byte
    or  al,al               ; zero if absolute or first entry
    je  ps_chk_common       ; bypass common check for this entry
    and al,1ch              ; get combine field
    cmp al,18h              ; see if common combine type
    jne ps_chk_abs          ; no, see if absolute segment

ps_chk_common:
    mov al,acbp_byte        ; get current acbp_byte
    and al,1ch              ; get combine field
    cmp al,18h              ; see if common combine type
    jne ps_chk_abs          ; no, see if absolute segment

; this segment and any previous matching segdef entry are common combine types
    mov is_common,1         ; flag that a common segment

ps_chk_abs:
    cmp is_absseg,0         ; see if absolute segment
    je  ps_13b              ; no

; absolute segment, update frame number, acbp byte, point segment and
; class names at zero_table, and return
    mov ax,frame_number
    mov es:[0],ax
    mov al,acbp_byte        ; get current acbp byte
    mov es:[26],al
    mov ax,OFFSET DGROUP:zero_table ; make segment and class names point to known null value
    mov es:[8],ax
    mov es:[12],ax
    mov ax,DGROUP
    mov es:[10],ax
    mov es:[14],ax
    jmp NEAR PTR ps_out

; get adjustment to current segment length before saving segment partition entry
; stack combine type forces paragraph alignment, no segment adjustment
ps_13b:
    inc segdef_count        ; bump count of segdefs encountered (don't count absolutes)
    mov dl,acbp_byte
    and dl,0e0h             ; dl holds align field value
    xor dh,dh               ; dh holds segment adjustment value
    mov al,es:[26]          ; get previous segdef entry acbp byte
    and al,1ch              ; get combine field value
    cmp al,14h              ; check if stack combine type
    je  ps_make_spart       ; yes, no segment adjustment

    mov al,acbp_byte        ; get current acbp byte
    and al,1ch              ; get combine field value
    cmp al,14h              ; check if stack combine type
    je  ps_make_spart       ; yes, no segment adjustment

    mov ax,es:[6]           ; get segment length in ax
    xor ah,ah               ; zap high byte (alternately 256, ignore carry)
    cmp dl,80h              ; check if page aligned
    jne ps_14               ; no
    sub ah,al               ; get segment adjustment value to round to page
    jmp SHORT ps_seg_adjust ; bypass remaining test/adjustment code

ps_14:
    cmp dl,60h              ; paragraph aligned
    jne ps_15               ; no
    and al,15               ; make segment length modulo 16
    mov ah,16
    sub ah,al
    and ah,15               ; get segment adjustment value to round segment length to paragraph
    jmp SHORT ps_seg_adjust ; bypass remaining test/adjustment code

ps_15:
    cmp dl,40h              ; word aligned
    jne ps_seg_adjust       ; no
    and al,1
    mov ah,al               ; get segment adjustment value to round segment length to word

ps_seg_adjust:
    cmp is_common,0         ; see if common segment
    jne ps_make_spart       ; yes, bypass segment length adjustment

;    xor al,al
;    mov dh,al               ; init segment adjustment values 
;    cmp WORD PTR segment_length,0   ; check if a zero length segment
;    je  ps_15a              ; zero length, don't adjust (0 adjustment)

    mov al,ah
    mov dh,ah               ; save segment adjustment value in dh

ps_15a:
    xor ah,ah               ; get segment adjustment value to add to segment length
    add WORD PTR segment_length,ax  ; update segment length low word
    adc WORD PTR segment_length+2,0 ; update segment length high word

; make a new segment partition entry for this segdef record, return segment in es
ps_make_spart:

; Check if DOSSEG switch set.  If it is, check if this is first
; declaration of segment _TEXT.  If it is, make a segment partition
; entry of 16 bytes first and adjust the segment length accordingly.
; ONLY do this if not use DDL's

    xor ax,ax
    cmp is_dosseg,al        ; check DOSSEG switch
    jne  pms_1              ; set

to_pms_3:
    jmp  NEAR PTR pms_3     ; not set

pms_1:
    cmp any_ddl,al          ; see if using DDLs
    jne to_pms_3            ; yes, ignore DOSSEG segment stuff
    cmp _textflag,al        ; see if segment _TEXT already adjusted
    jne pms_2               ; yes
    mov bx,OFFSET DGROUP:_texttext  ; text to check segment name against
    mov di,OFFSET DGROUP:name_field ; text of segment name

pm_textloop:
    mov al,[bx]
    or  al,[di]             ; if both zero then successful match
    je  pm_insert_bytes     ; matched, insert bytes in front of segment
    cmp al,[di]             ; see if this char matches
    jne pms_2               ; no, segment is not _TEXT
    inc bx
    inc di                  ; bump to next char to compare
    jmp SHORT pm_textloop   ; loop back to test next char

pm_insert_bytes:
    mov _textflag,1         ; set _textflag so insertion only occurs once
    push    es              ; save es -> segdef entry
    call    make_segpart_entry  ; create a partition entry for 16 zero bytes
    mov WORD PTR es:[10],255    ; make record index invalid amount (no pass 2 segdef lookup)
    mov al,acbp_byte
    mov es:[11],al          ; save segment partition entry acbp byte
    xor ax,ax
    mov es:[0],ax           ; zero partition offset
    mov WORD PTR es:[12],16 ; partition length is 16 bytes
    mov ax,es               ; save segment pointer to partition entry in ax
    pop es                  ; restore es -> segdef entry
    cmp WORD PTR es:[22],0  ; see if previous partition entry
    je  pm_insert2          ; no

; previous partition entry exists, insert the 16 zero byte partition entry in
; as the first partition entry, make it point to the original first partition
; entry and adjust the original first partition entry's offset by 16 bytes
    add WORD PTR es:[6],16  ; add 16 bytes to segment length
    mov bx,es:[22]          ; bx-> first segment partition entry
    push    es              ; save es -> segdef entry

    mov es,ax               ; es -> 16 zero byte partition entry
    mov es:[2],bx           ; make 16 zero byte partition entry point to (previously) first entry

pm_partloop:
    mov es,bx               ; es -> segment partition entry
    add WORD PTR es:[0],16  ; adjust offset by 16 bytes
    mov bx,es:[2]           ; get next segment partition entry, if any
    or  bx,bx               ; check if non-null (entry exists)
    jne pm_partloop         ; yes

    pop es                  ; restore es -> segdef entry
    jmp SHORT pm_insert3

pm_insert2:
    mov WORD PTR es:[6],16  ; insert 16 bytes into segment (increase segment length)
    mov WORD PTR es:[24],ax ; update last segment partition entry pointer

pm_insert3:
    mov WORD PTR es:[22],ax ; update first segment partition entry pointer
    jmp SHORT pms_3         ; bypass DGROUP code

; check if segment is DGROUP, set flag if so
pms_2:
    cmp is_dgroup,0         ; see if flag already set
    jne pms_3               ; yes, bypass check for setting flag
    mov bx,OFFSET DGROUP:dgrouptext  ; text to check segment name against
    mov di,OFFSET DGROUP:name_field ; text of segment name

pm_dgroup_loop:
    mov al,[bx]
    or  al,[di]             ; if both zero then successful match
    je  pm_isdgroup         ; matched, set dgroup flag
    cmp al,[di]             ; see if this char matches
    jne pms_3               ; no, segment is not DGROUP
    inc bx
    inc di                  ; bump to next char to compare
    jmp SHORT pm_dgroup_loop    ; loop back to test next char

pm_isdgroup:
    mov is_dgroup,1         ; set dgroup flag

pms_3:
    push    es              ; save es -> segdef entry
    call    make_segpart_entry
    mov al,acbp_byte
    mov es:[11],al          ; save segment partition entry acbp byte
    mov ax,WORD PTR no_align_length
    mov es:[12],ax          ; save segment partition length

; if overlaid segment partition, check length and bypass partition adjustment
    cmp ovl_seg_flag,0      ; see if this partition is an overlay
    jne pms_zero            ; yes, zero segment partition offset

pms_3a:
    mov al,dh               ; al holds segment adjustment value
    xor ah,ah               ; zap high byte
    add es:[0],ax           ; adjust segment offset for alignment

; check if common segment, if common then zero segment partition offset
    cmp is_common,0         ; see if common segment
    je  pms_4               ; yes

pms_zero:
    mov WORD PTR es:[0],0   ; zero segment partition offset

pms_4:
    mov dx,es               ; save segment pointer to partition entry in dx
    pop es                  ; restore es -> segdef entry
    mov ax,es:[24]          ; get segment of previously last partition entry, if any, in ax
    mov es:[24],dx          ; update segment pointer to last partition entry to current partition entry
    or  ax,ax               ; see if previous segment partition entries
    jne ps_16               ; at least one previous partition entry
    mov es:[22],dx          ; save pointer to current partition entry as first entry
    jmp SHORT ps_17         ; bypass partition entry update code

ps_16:
    mov bx,es               ; save segment pointer to segdef entry in bx
    mov es,ax               ; set es to partition entry segment for update
    mov es:[2],dx           ; update next pointer of partition entry to current partition entry segment
    mov es,bx               ; restore es -> segdef entry

; if segment is overlay class make at least paragraph alignment
ps_17:
    cmp any_ddl,0           ; see if using/creating DDL's
    jne ps_high_align       ; yes, don't force paragraph alignment (do it at runtime)

    mov al,es:[28]          ; get overlay flag
    or  al,al               ; see if overlay class
    je  ps_high_align       ; no
    mov al,acbp_byte        ; get this segment's acbp byte
    mov ah,al               ; save in ah
    and al,0e0h             ; get align field
    cmp al,060h             ; see if paragraph alignment or better
    jae ps_high_align       ; yes
    and ah,01fh             ; mask out previous align field
    or  ah,060h             ; force paragraph alignment
    mov acbp_byte,ah        ; save back to memory variable

; if preexisting ACBP byte, keep highest of two Align fields
; page (A==4) > para (A==3) > word (A==2) > byte (A==1) > absolute (A==0)
ps_high_align:
    mov al,es:[26]          ; get segdef entry acbp byte
    or  al,al               ; check if nonzero, previous entry existed
    jne ps_17a              ; yes it did
    mov al,acbp_byte        ; get current acbp byte
    mov es:[26],al          ; save as entry acbp byte

ps_17a:
    cmp al,acbp_byte        ; compare to current acbp byte
    jae ps_18               ; entry acbp byte is highest

    and al,1fh              ; mask align field off of entry acbp byte
    mov ah,al               ; keep nonaligned fields in ah
    mov al,acbp_byte        ; get current acbp byte
    and al,0e0h             ; mask off all but align field
    or  al,ah               ; merge in old nonalign fields
    mov es:[26],al          ; save new acbp byte value

; update the combine field, giving proper precedence
ps_18:
    mov dl,al
    and dl,0e3h             ; dl holds entry's noncombine fields
    mov dh,al
    and dh,1ch              ; dh holds entry's combine field
    mov al,acbp_byte
    and al,1ch              ; get current combine field
    je  ps_19               ; no previous acbp byte, each private (combine==0) has its own entry
    cmp al,0ch              ; check for unused combine type
    je  ps_bad_combine      ; bad combine value
    cmp al,4                ; check for unused combine type
    jne ps_20               ; okay

ps_bad_combine:
    mov dx,OFFSET DGROUP:filename
    mov ax,COMBINE_VAL_ERR  ; Invalid SEGDEF combine field value
    jmp NEAR PTR error_read_buff_pos    ; transfer control to error handler

ps_20:
    cmp al,14h              ; check if stack combine type
    jne ps_21               ; no
    or  dl,14h              ; merge stack combine type in noncombine fields
    jmp SHORT ps_23         ; bypass other update code

ps_21:
    cmp al,18h              ; check if common combine type
    jne ps_22               ; no
    cmp dh,14h              ; stack combine has precedence
    je  ps_19               ; don't update entry
    cmp dh,8                ; public combine type has precedence
    je  ps_19               ; don't update entry
    or  dl,18h              ; merge common combine type in noncombine fields
    jmp SHORT ps_23         ; bypass other update code

; public combine type, values 8,10,1ch
ps_22:
    cmp dh,14h              ; stack combine has precedence
    je  ps_19               ; don't update entry
    or  dl,8                ; merge public combine type in noncomine fields

ps_23:
    mov es:[26],dl          ; save updated entry acbp byte

ps_19:
    mov al,es:[26]          ; get entry acbp byte
    mov ah,al               ; save in ah
    and al,1ch              ; get combine type
    cmp al,14h              ; check if stack combine type
    jne ps_24               ; no

; stack combine type forces paragraph alignment
    and ah,1fh              ; clear previous align type bit
    or  ah,60h              ; set paragraph align type
    mov es:[26],ah          ; update entry's acbp byte

; get segment name from previous lnames record
ps_24:
    mov ax,segment_name_index   ; get segment name index
    dec ax                  ; make count relative zero
    shl ax,1
    shl ax,1                ; make segment name index a doubleword offset into index pointer array
    mov di,offset DGROUP:lnames_ent_indptr  ; set di to base of lnames index pointer array
    add di,ax               ; di -> proper array element
    mov ax,[di]             ; get offset of segment name from lnames entry
    mov es:[8],ax           ; save offset of segment name
    mov ax,[di+2]           ; get segment of segment name from lnames entry
    mov es:[10],ax          ; save segment of segment name

; get class name from previous lnames record
    mov ax,class_name_index ; get class name index
    dec ax                  ; make count relative zero
    shl ax,1
    shl ax,1                ; make class name index a doubleword offset into index pointer array
    mov di,offset DGROUP:lnames_ent_indptr  ; set di to base of lnames index pointer array
    add di,ax               ; di -> proper array element
    mov ax,[di]             ; get offset of class name from lnames entry
    mov es:[12],ax          ; save offset of class name
    mov ax,[di+2]           ; get segment of class name from lnames entry
    mov es:[14],ax          ; save segment of class name

; if is_sympac and !maybe_clipper and !symbol_overflow,
; see if name SYMBOLS, class SYMBOLS, if so then set maybe_clipper
; and keep segment index value (for LEDATA lookup)
; and pointer to segdef entry (in es)
	xor	al,al
	cmp	is_sympac,al		; see if symbol table compression on
	je	ps_ddlchk			; no
	cmp	maybe_clipper,al	; see if possible clipper module already
	jne	ps_ddlchk			; yes
	cmp	symbol_overflow,al	; see if symbol table compaction routine has overflowed
	jne	ps_ddlchk			; yes

	push	cx				; save critical register
	push	si

	push	es
	les	di,es:[8]			; es:di -> lnames segment name entry
	mov	cx,8				; eight chars in SYMBOLS including null terminator
	add	di,cx				; adjust past hi/low pointers
	mov	si,OFFSET DGROUP:symbolstext
	mov	dx,si				; save text offset
	repe	cmpsb			; compare entry name to SYMBOLS
	pop	es
	jne	ps_symout			; failed

	push	es
	les	di,es:[12]			; es:di -> lnames class name entry
	mov	cx,8				; eight chars in SYMBOLS including null terminator
	add	di,cx				; adjust past hi/low pointers
	mov	si,dx				; get SYMBOLS offset in si
	repe	cmpsb			; compare entry name to SYMBOLS
	pop	es
	jne	ps_symout			; failed

; success, segment is name SYMBOLS class SYMBOLS
	mov	maybe_clipper,1		; set maybe clipper flag
	mov	clipper_segdef_ptr,es	; save -> this segdef for length modification

	mov	ax,current_segdef	; get current segdef index, relative zero
	mov	clipper_segindex,ax	; save symbols segment index for LEDATA lookup
	cmp	clipper_symseg_ptr,0	; see if this is the first SYMBOLS segment
	jne	ps_symout			; no
	mov	clipper_symseg_ptr,es	; save -> first SYMBOLS segment for later fixups

ps_symout:
	pop	si					; restore critical register
	pop	cx

; if segment partition is an overlay and not using DDLs, bypass segment length update
ps_ddlchk:
    xor al,al
    cmp any_ddl,al          ; see if using/creating DDLs
    jne ps_24a              ; yes
    cmp ovl_seg_flag,al     ; see if this partition is an overlay
    jne ps_out              ; yes, bypass segment length update

ps_24a:
    mov dl,es:[26]          ; get entry acbp byte
    mov al,dl
    and dl,2                ; get status of entry Big bit in dl
    mov dh,acbp_byte        ; get current acbp byte
    and dh,2                ; get status of current Big bit in dh

    and al,1ch              ; get combine field of entry
    cmp al,18h              ; check if common combine type
    jne ps_25               ; nope
    or  dx,dx               ; is either Big bit set
    jne ps_out              ; yes
    cmp WORD PTR segment_length+2,0 ; check if segment is Big (64K long)
    jne ps_out              ; yes

; segment is less than 64K long, keep largest segment length in entry
    mov ax,WORD PTR segment_length  ; get segment length
    cmp ax,es:[6]           ; check if current length longer than segment entry's
    jbe ps_out              ; no, all done
    mov es:[6],ax           ; save current length as new entry segment length
    jmp SHORT ps_out        ; all done

; segment is exactly 64K long
ps_seg_equ_64k:
    or  BYTE PTR es:[26],2      ; set Big bit in entry
    mov WORD PTR es:[6],0       ; zero segment length (>64K not allowed)
    jmp SHORT ps_out            ; all done

; segdef entry is not common or private combine type
ps_25:
    mov ax,es:[6]
    add WORD PTR segment_length,ax  ; add current length for total the segment length
    adc WORD PTR segment_length+2,0 ; update high word
    mov ax,WORD PTR segment_length+2    ; get high word
    mov di,WORD PTR segment_length  ; get low word
    or  ax,ax               ; is high word nonzero
    je  ps_27               ; yes, no overflow yet
    or  di,di               ; is low word and high word nonzero
    je  ps_28               ; no, no overflow

ps_seglen_err:
    mov ax,SEGMENT_LEN_ERR  ; segment size exceeds 64K

ps_to_linkerr:
    mov dx,OFFSET DGROUP:filename
    jmp NEAR PTR link_error ; transfer control to error handler

; check for Big bit set with nonzero segment length
ps_27:
    or  dx,dx               ; check if either Big bit set
    je  ps_28               ; no
    or  di,di               ; check if low word of segment length nonzero
    jne ps_seglen_err       ; yes, segment size overflow

ps_28:
    or  ax,ax               ; is high word of segment nonzero (bit 0 set)
    jne ps_seg_equ_64k      ; yes, segment is exactly 64K long

; save segment length to segdef entry
    mov ax,WORD PTR segment_length
    mov es:[6],ax           ; save low word as total length

ps_out:
    inc current_segdef      ; bump count of current segdef record
    inc si                  ; bump past checksum byte
    cmp si,bp               ; check boundary conditions
    jb  ps_ret              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_ret:
    pop es                  ; restore critical register
    ret
proc1_segdef    ENDP

;*****************************
;* GET_SEGDEF_ENTRY          *
;*****************************

; find or create segdef entry
; return segment of entry in es
; destroys ax,es

get_segdef_entry    PROC
    push    bx              ; save critical register
    push    dx
    push    si
    push    di
    mov ax,segment_name_index   ; get index value for lnames array
    dec ax                  ; make count relative zero
    shl ax,1                ; convert to word index
    shl ax,1                ; convert to doubleword index
    mov si,OFFSET DGROUP:lnames_ent_indptr  ; si -> base of lnames array
    add si,ax               ; si -> array element with segment:offset of name in lnames block

    mov di,[si]             ; get offset of name in di
    mov ax,[si+2]           ; get segment of name in ax
    mov si,OFFSET DGROUP:name_field ; point to slot to place name
    mov dx,ds               ; save warplink's data segment
    mov ds,ax               ; ds -> name segment in lnames block
    mov es,dx               ; es -> warplink's data
    xchg    si,di           ; di -> name field, si -> name offset in lnames block
    add si,8                ; bump si past hi/low segment:offset pointers in lnames entry

; ds:si -> lnames name, es:di -> warplink data name field
gse_get_name:
    movsb                   ; transfer a char
    cmp BYTE PTR [si-1],0   ; zero char flags end of transfer
    jne gse_get_name        ; not zero, keep transferring

    mov ax,es               ; get warplink's data segment
    mov ds,ax               ; restores ds -> warplink data
    mov si,OFFSET DGROUP:name_field ; si -> name to get hash code of
    call    get_hash        ; get hash code of name
    mov si,OFFSET DGROUP:segdef_hash   ; si -> base of hash pointers to segdef entries
    shl ax,1                ; convert ax to word offset
    add si,ax               ; si points to proper name hash code entry
    cmp WORD PTR [si],0     ; is hash code used (nonzero value)
    jne gse_2               ; yes

gse_make_ent:
    mov ax,alloc_segdefblk_ptr
    or  ax,ax               ; check if any segdef blocks were previously allocated
    jne gse_3               ; yes

; make initial segdef block allocation
    mov bx,SEG_DEFBLK_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_segdefblk_ptr,ax  ; update last allocated block pointer
    mov first_segdefblk_ptr,ax  ; update first allocate block pointer
    mov es,ax               ; es == current (new) block segment
    xor ax,ax
    mov WORD PTR es:[0],ax  ; zero count of entries in block
    mov WORD PTR es:[2],ax  ; zero pointer to next entry
    jmp SHORT gse_4         ; bypass block full check code

; at least one prior segdef entry made, check if block is full
gse_3:
    mov es,ax               ; es -> current block
    cmp WORD PTR es:[0],SEG_DEFENT_COUNT    ; see if any free entries in block
    jb  gse_4                ; yes
    call    make_segdef_blk ; no free entries in block, make a new block

gse_4:
    call    init_segdef_entry   ; perform segdef entry initialization code

    cmp is_absseg,0         ; if absolute segment don't check overlay class
    jne gse_4a              ; absolute, bypass check
    call    chk_seg_ovl_class   ; check if segment is overlay class, if overlays

gse_4a:
    mov [si],di             ; update hash code segment pointer
    mov es,di               ; es -> new segdef entry for return
    jmp NEAR PTR gse_5      ; bypass dup/collision code

; hash code used, check whether duplicate name or hash collision
gse_2:
    mov bx,[si]             ; get segment pointer to segdef entry in bx (will be parent entry to new entry)
    mov al,acbp_byte        ; get current acbp byte in dl
    and al,1ch              ; combine field only
    jne gse_2a              ; if nonzero then segments combineable

; private segment, not combineable
    mov bx,[si]             ; get segment pointer to segdef entry in bx (will be parent entry to new entry)

gse_privloop:
    mov es,bx               ; es -> segdef entry
    mov ax,es:[20]          ; get pointer to next entry
    or  ax,ax               ; if null, then at end of parent/child chain, last child becomes new parent
    je  SHORT gse_new_name  ; make new segdef entry for private segment
    mov bx,ax               ; update parent entry
    jmp SHORT gse_privloop  ; loop until final link in parent/child chain found

gse_2a:
    mov ax,[si]             ; get segment pointer to segdef entry

gse_testloop:
    mov es,ax               ; es -> segment of segdef entry testing name against
    mov bx,ax               ; save entry segment in bx
    mov ax,es:[10]          ; get segment of segment name in lnames block
    mov di,es:[8]           ; get offset of segment name in lnames block
    add di,8                ; adjust for 2 doubleword pointers in front of name
    mov es,ax               ; extra segment holds lnames segment
    mov si,OFFSET DGROUP:name_field ; si -> name to get hash code of

; ds:si -> name, es:di -> name to test against
gse_byteloop:
    mov al,[si]
    or  al,es:[di]          ; see if both values are zero (matched to null terminator)
    je  gse_match           ; strings matched
    cmpsb                   ; compare a nonzero byte in the two names
    je  gse_byteloop        ; bytes match, loop for next byte test
    mov es,bx               ; get segdef entry segment value

gse_next_ptr:
    mov ax,es:[20]          ; get next entry
    or  ax,ax               ; check if a null pointer (ax=0)
    jne gse_testloop        ; no, keep checking entries

; new name, no matches for any segdef entry
gse_new_name:
    mov ax,alloc_segdefblk_ptr   ; get last allocated block
    mov es,ax               ; es -> block
    cmp WORD PTR es:[0],SEG_DEFENT_COUNT    ; see if any free entries in block
    jb  gse_9               ; yes
    call    make_segdef_blk    ; no free entries in block, make a new block

gse_9:
    call    init_segdef_entry   ; perform segdef entry initialization code
    call    chk_seg_ovl_class   ; check if segment is overlay class, if overlays

    mov es,bx               ; get entry that points to this new entry (parent entry)
    mov es:[20],di          ; update next entry pointer
    mov es,di               ; return es -> new segdef entry
    jmp SHORT gse_5         ; perform new entry code

; segment names match, check class names
gse_match:
    mov es,bx               ; get segdef entry segment value
    push    es              ; save entry segment in es, used
    push    ds              ; save data segment value, used
    mov ax,class_name_index ; get index value for lnames array
    dec ax                  ; make count relative zero
    shl ax,1                ; convert to word index
    shl ax,1                ; convert to doubleword index
    mov si,OFFSET DGROUP:lnames_ent_indptr  ; si -> base of lnames array
    add si,ax               ; si -> array element with segment:offset of name in lnames block
    mov ax,[si+2]           ; get segment of name from lnames entry in ax
    mov si,[si]             ; get offset of name from lnames entry in si
    add si,8                ; adjust past doubleword pointers
    mov ds,ax               ; ds:si -> class name in lnames block from index

    mov ax,es:[14]          ; get segment of entry's class name
    mov di,es:[12]          ; get offset of entry's class name
    add di,8                ; adjust past doubleword pointers
    mov es,ax               ; es:di -> test entry's class name in lnames block

gse_matchloop:
    cmpsb                   ; compare a char
    jne gse_nomatch         ; class names didn't match
    mov al,[si]
    or  al,es:[di]          ; see if both values are zero (matched to null terminator)
    jne gse_matchloop       ; not done yet

    pop ds                  ; restore data segment
    pop es                  ; restore pointer to segdef entry
    mov al,es:[26]          ; get test entry acbp byte
    and al,1ch              ; combine field only
    jne gse_ret             ; names match and ACBP combineable types
    jmp NEAR PTR gse_next_ptr   ; private combine, try next entry

; class names didn't match, try next entry
gse_nomatch:
    pop ds                  ; restore segment registers
    pop es                  ; restore pointer to segdef entry
    jmp NEAR PTR gse_next_ptr

gse_5:
    inc WORD PTR seg_count  ; update count of total discrete segments

gse_ret:
    pop di                  ; restore critical registersr
    pop si
    pop dx
    pop bx
    ret
get_segdef_entry    ENDP

;*****************************
;* INIT_SEGDEF_ENTRY         *
;*****************************

; initialize segdef entry
; upon entry es -> segdef block
; returns new segdef entry segment in di
; destroys ax,dx,di

init_segdef_entry   PROC
    mov di,es:[0]           ; get old entry count
    inc WORD PTR es:[0]     ; bump count of entries
    shl di,1                ; each entry takes up two paragraphs
    mov ax,es               ; get block segment address
    add di,ax               ; get first free entry segment value
    inc di                  ; adjust for block system info size of 1 paragraph

    mov dx,es               ; save critical register
    mov es,di               ; ds -> new segdef entry
    push    di
    xor ax,ax
    mov di,ax
    stosw                   ; zero init values in segdef entry
    stosw
    stosw
    stosw
    mov di,16
    stosw
    stosw
    stosw
    stosw
    stosw
    stosw
    stosw
    stosw
    pop di
    mov es,dx               ; restore critical register
    ret
init_segdef_entry   ENDP

;*****************************
;* MAKE_SEGDEF_BLK           *
;*****************************

; make a new segdef block
; return segment of new block in es
; destroys ax,es

make_segdef_blk PROC
    push    dx              ; save critical register
    push    bx
    mov dx,alloc_segdefblk_ptr  ; keep previously last allocated block segment
    mov bx,SEG_DEFBLK_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_segdefblk_ptr,ax  ; update last allocated block pointer

    mov es,dx               ; es -> old block
    mov es:[2],ax           ; update old block to point to new block

    mov es,ax               ; es -> new block
    mov WORD PTR es:[0],0   ; zero count of entries in block
    mov WORD PTR es:[2],0   ; zero pointer to next entry
    pop bx
    pop dx                  ; restore critical registers
    ret
make_segdef_blk ENDP

;*****************************
;* MAKE_SEGPART_ENTRY        *
;*****************************

; make segment partition entry
; upon entry es -> segdef entry
; return segment of entry in es
; destroys ax,bx,es

make_segpart_entry  PROC
    push    cx              ; save critical register
    push    es              ; save pointer to segdef entry

; check if segdef is overlay class, if so and overlay flag for this
; module is set, this partition entry forms an overlay
; re/set overlay segment flag accordingly
    xor al,al
    mov ovl_seg_flag,al     ; init overlay current segment flag
    cmp BYTE PTR es:[28],al ; check segment overlay class flag
    je  mse_1               ; not overlay class
    mov bx,WORD PTR segment_length
    or  bx,WORD PTR segment_length+2    ; don't overlay zero length segment
    je  mse_1               ; zero length segment, don't make an overlay
    mov seg_ovlclass,1      ; set overlay class segment in module flag

    cmp is_inlib,al         ; see if parsing library
    je  mse_chkovl          ; no
    cmp is_clip5,al         ; see if Clipper 5 switch set
    je  mse_chkovl          ; no

; check if CLIPPER.LIB, if so, only overlay non-_TEXT segments
    call    check_clp5_lib
    or  al,al               ; nonzero if CLIPPER.LIB or EXTEND.LIB nonoverlayable
    jne mse_1

mse_chkovl:
    cmp obj_ovl_flag,0      ; check if module is in an overlay
    je  mse_1               ; no

mse_ovl:
    mov ovl_seg_flag,1      ; set overlay segment flag
    inc ovl_code_id         ; bump overlay code id (used for Clarion data overlay stuff)

mse_1:
    mov ax,alloc_segpartblk_ptr ; get last allocated block segment
    or  ax,ax               ; was there a previous block
    jne mse_2               ; yes

; make initial pubdef declarations block allocation
    mov bx,SEG_PARTBLK_SIZE ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_segpartblk_ptr,ax ; update last allocated block pointer
    mov first_segpartblk_ptr,ax ; update first
    mov es,ax               ; es -> block
    mov WORD PTR es:[0],0   ; zero count of entries in block
    mov WORD PTR es:[2],0   ; zero pointer to next entry
    jmp SHORT mse_3         ; bypass block full check code

; at least one prior segment partition block, check if block is full
mse_2:
    mov es,ax               ; es -> current block
    cmp WORD PTR es:[0],SEG_PARTENT_COUNT   ; see if any free entries in block
    jb  mse_3               ; yes
    call    make_segpart_blk    ; no free entries in block, make a new block

mse_3:
    mov bx,es:[0]           ; get entry count
    mov ax,es               ; get block segment address
    add bx,ax               ; get first free entry segment value
    inc bx                  ; adjust for block system info size of 1 paragraph

    inc WORD PTR es:[0]     ; increment count of entries in segment partition block

    mov es,bx               ; es -> segment partition entry
    cmp is_inlib,0          ; see if processing library module
    je  mse_4               ; no
    mov ax,WORD PTR lib_id  ; get library id low word
    mov es:[6],ax           ; save to file_mod_id number low word
    mov ax,WORD PTR lib_id+2    ; get library id high word
    mov es:[8],ax           ; save to file_mod_id number high word
    jmp SHORT mse_5         ; bypass nonlibrary code

mse_4:
    mov ax,current_obj
    mov es:[6],ax           ; keep id number for pass 2 lookup
    xor ax,ax
    mov es:[8],ax           ; zero high word of id number

mse_5:
    mov ax,current_segdef
    mov es:[10],al          ; keep segment record index for pass 2 lookup
    xor ax,ax
    mov WORD PTR es:[2],ax  ; zero pointer to next block
    mov WORD PTR es:[14],ax ; zero flag word

    pop ax                  ; get segdef entry segment pointer
    mov es:[4],ax           ; save pointer to master segdef entry

    cmp ovl_seg_flag,0      ; see if this partition is an overlay
    je  mse_6               ; no, update partition offset
    inc mod_ovl_count       ; bump count of overlays in this module
    inc ovl_count           ; bump total count of overlays
    mov ax,ovl_count        ; get count of overlays
    cmp ax,OVERLAY_MAX_COUNT    ; make sure that count of overlays wasn't exceeded
    jbe mse_5a              ; not exceeded

; too many overlays, fatal error
    mov ax,OVERLAY_COUNT_ERR
    jmp NEAR PTR link_error ; transfer control to error handler

mse_5a:
    mov BYTE PTR es:[15],80h    ; set high bit of byte to indicate overlaid partition
    jmp SHORT mse_ret       ; yes, bypass partition offset update

mse_6:
    mov cx,es               ; save pointer to segment partition entry
    mov es,ax               ; point to segdef entry
    mov ax,es:[6]           ; get segment length of segdef entry
    mov es,cx               ; restore es -> segment partition entry
    mov es:[0],ax           ; this segment partition starts at end of last partition, if any

mse_ret:
    pop cx                  ; restore critical register
    ret
make_segpart_entry  ENDP

;*****************************
;* CHECK_CLP5_LIB            *
;*****************************

; returns al==0 if overlay name doesn't end in _TEXT or current file
; is not CLIPPER.LIB (segment potentially overlayable),
; otherwise return al != 0
; destroys ax,bx

check_clp5_lib  PROC
    push    si              ; save critical register
    mov ax,current_lib
    cmp ax,clip_libnum      ; see if current lib is CLIPPER.LIB
    jne ccl_ovl             ; no

    mov si,OFFSET DGROUP:name_field

ccl_searchloop:
    lodsb                   ; search for null terminator of segment name
    or  al,al               ; at null terminator?
    jne ccl_searchloop      ; no

    sub si,6                ; si -> where _TEXT should start
    cmp BYTE PTR [si],'_'   ; see if _TEXT
    jne ccl_ovl             ; no
    cmp BYTE PTR [si+1],'T' ; see if _TEXT
    jne ccl_ovl             ; no
    cmp BYTE PTR [si+2],'E' ; see if _TEXT
    jne ccl_ovl             ; no
    cmp BYTE PTR [si+3],'X' ; see if _TEXT
    jne ccl_ovl             ; no
    cmp BYTE PTR [si+4],'T' ; see if _TEXT
    jne ccl_ovl             ; no

ccl_nonovl:
    mov al,1                ; current segment is NOT overlayable
    jmp SHORT ccl_exit

ccl_ovl:
    xor al,al               ; current segment is overlayable

ccl_exit:
    pop si                  ; restore critical register
    ret
check_clp5_lib  ENDP

;*****************************
;* MAKE_SEGPART_BLK          *
;*****************************

; make a new segment partition block
; return segment of new block in es
; destroys ax,es

make_segpart_blk    PROC
    push    dx              ; save critical register
    mov dx,alloc_segpartblk_ptr ; keep previously last allocated block segment
    mov bx,SEG_PARTBLK_SIZE ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_segpartblk_ptr,ax ; update last allocated block pointer

    mov es,dx               ; es -> old block
    mov es:[2],ax           ; update old block to point to new block

    mov es,ax               ; es -> new block
    mov WORD PTR es:[0],0   ; zero count of entries in block
    mov WORD PTR es:[2],0   ; zero pointer to next entry
    pop dx                  ; restore critical registers
    ret
make_segpart_blk    ENDP

END
