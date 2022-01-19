;*********************************************************************
;*   MLPASS1C.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          07/11/91                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker pass 1 routines part C                                   *
;*                                                                   *
;*********************************************************************

TITLE   MACHLINK mlpass1a
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

PUBLIC  proc1_grpdef

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   name_field:BYTE,filename:BYTE

; initialized local variables

.DATA?

; uninitialized local variables

; byte values
EVEN                        ; maximize speed on 8086 and better

; word value
grp_ent_ptr DW  ?           ; segment pointer to group entry

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR,link_warning:NEAR
EXTRN   get_name:NEAR,get_hash:NEAR,alloc_memory:NEAR,error_bx_pos:NEAR

;*****************************
;* PROC1_GRPDEF              *
;*****************************

; process GRPDEF record
; upon entry cx=record length,bp=buffer_end,al=record type
; si -> first byte of record past record length
; updates si
; destroys ax,bx,dx,di

proc1_grpdef    PROC
    xor ah,ah               ; zero high byte
    mov al,es:[si]          ; get low byte of group name index
    mov bx,si               ; keep pointer to first index byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pg_2                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pg_2:
    or  al,al               ; make sure value is nonzero
    je  pg_bad_grpind       ; zero value is invalid
    cmp al,80h              ; check if more than one byte index value
    jb  pg_4                ; no, continue
    and al,7fh              ; mask off high bit
    mov ah,al               ; get high byte in ax
    mov al,es:[si]          ; get second byte of index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pg_4                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pg_4:
    call    get_group_entry ; make or find group entry with group index in ax, return in ax
    mov di,current_grpdef   ; get number of current grpdef record
    shl di,1                ; convert di to word offset
    mov bx,offset DGROUP:grp_ent_indptr ; bx -> base of group index pointer array
    add bx,di               ; bx -> proper array element
    mov [bx],ax             ; set array element pointer to group entry segment
    mov grp_ent_ptr,ax      ; save pointer to group entry to local memory variable
    jmp NEAR PTR pg_checksum    ; check if at checksum (no segment indices)

pg_bad_grpind:
    mov dx,OFFSET DGROUP:filename
    mov ax,GRPIND_VAL_ERR   ; Invalid GRPDEF group name index value
    jmp NEAR PTR error_bx_pos   ; transfer control to error handler

; pull segments and update their segdef entries to point to group entry
pg_segloop:
    inc si                  ; scan past type byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pg_4a               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pg_4a:
    mov al,es:[si]          ; get first byte of segment index
    mov bx,si               ; keep pointer to first index byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pg_5                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pg_5:
    cmp al,80h              ; check if high bit set
    jb  pg_7                ; no, only 1 byte index

; two byte index, dh contains high-order byte with high bit set
    je  pg_6a               ; if greater than 0x80 then segment index > 255, out of bounds

pg_bad_segind:
    mov dx,OFFSET DGROUP:filename
    mov ax,SEGDEF_VAL_ERR   ; segment index greater than 255 maximum, or higher than count of segdef records
    jmp NEAR PTR error_bx_pos   ; transfer control to error handler

pg_6a:
    mov al,es:[si]          ; get second byte of segment index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pg_7                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pg_7:
    or  al,al               ; make sure that dh is nonzero value
    je  pg_bad_segind       ; zero value is invalid

; al contains segment index value
    mov bx,offset DGROUP:seg_defent_indptr  ; set bx to base of segment index pointer array
    dec al                  ; make segment name index relative zero
    xor ah,ah               ; zap high byte
    shl ax,1                ; convert to word offset
    add bx,ax               ; bx -> segdef entry of segment in group
    mov dx,es               ; save extra segment
    mov es,[bx]             ; get segment of segdef entry
    mov ax,grp_ent_ptr      ; get segment pointer to group entry

    cmp udl_proc_pass,1     ; see if udl processing
    je  pg_8                ; yes, bypass multi-group segment check
    cmp WORD PTR es:[16],0  ; see if previous pointer exists
    je  pg_8                ; no
    cmp es:[16],ax          ; is if previous entry matches this entry
    je  pg_8                ; yes

; warning, segment declared in more than one group
    mov di,es:[8]           ; get offset of name in di
    add di,8                ; adjust past 2 doubleword pointers
    mov es,es:[10]          ; get segment of name in es
    push    dx              ; save critical register
    mov dx,OFFSET DGROUP:filename
    mov ax,SEG_GRP_DECL_WARN
    call    link_warning    ; give warning feedback
    pop dx                  ; restore critical register
    jmp SHORT pg_9          ; ignore new declaration

pg_8:
    cmp any_ddl,0           ; see if using DDL's
    je  pg_noddl            ; no
    mov ax,current_grpdef   ; use group index instead of pointer for DDL's
    inc ax                  ; make relative one

pg_noddl:
    mov es:[16],ax          ; save pointer to group entry in segdef entry

pg_9:
    mov es,dx               ; restore extra segment

pg_checksum:
    cmp cx,1                ; check if at checksum byte
    jbe pg_out              ; yes, gobble it and return
    jmp NEAR PTR pg_segloop ; loop back for next definition

pg_out:
    inc current_grpdef      ; bump count of current grpdef record
    inc si                  ; bump past checksum byte
    cmp si,bp               ; check boundary conditions
    jb  pg_ret              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pg_ret:
    ret
proc1_grpdef    ENDP

;*****************************
;* GET_GROUP_ENTRY           *
;*****************************

; find or create a group entry
; upon entry ax is the group name index
; return segment of entry in ax
; destroys ax

get_group_entry PROC
    push    bx              ; save critical register
    push    cx
    push    dx
    push    di
    push    es
    dec    ax               ; make group name index count relative zero
    shl ax,1                ; convert to word index
    shl ax,1                ; convert to doubleword index
    mov di,OFFSET DGROUP:lnames_ent_indptr  ; si -> base of lnames array
    add di,ax               ; si -> array element with segment:offset of name in lnames block

    mov cx,[di+2]           ; get segment of name entry in cx
    mov di,[di]             ; get offset of name entry in di

    mov ax,alloc_grpblk_ptr
    or  ax,ax               ; check if any group blocks were previously allocated
    jne gge_2               ; yes

; make initial group block allocation
    mov bx,GRP_BLK_SIZE     ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_grpblk_ptr,ax ; update last allocated block pointer
    mov first_grpblk_ptr,ax ; update first allocated block pointer
    mov es,ax               ; es -> block
    mov WORD PTR es:[0],0   ; zero count of entries in block
    mov WORD PTR es:[2],0   ; zero pointer to next entry

gge_2:
    mov es,ax               ; es -> current block
    xor dx,dx               ; dx hold group entry number

gge_grp_loop:
    cmp dx,es:[0]           ; check if any more entries in group block
    jb  gge_3               ; yes

; no more entries in group block, get next group block
    mov ax,es:[2]           ; get pointer to next group
    or  ax,ax               ; check if null, no more group blocks
    je  gge_4               ; no more blocks, make new group name
    mov es,ax               ; es -> new block
    xor dx,dx               ; re-init group entry number

gge_3:
    push    es              ; save es -> block
    mov ax,es               ; get block segment address
    add ax,dx               ; get first free entry segment value
    inc ax                  ; adjust for block system info size of 1 paragraph
    mov es,ax               ; es -> group entry
    inc dx                  ; bump entry number

    cmp di,es:[4]           ; see if offsets of lnames pointers match
    je  gge_3a              ; yes

gge_nomatch:
    pop es                  ; restore es -> block
    jmp SHORT gge_grp_loop  ; try next entry

gge_3a:
    cmp cx,es:[6]           ; see if segments of lnames pointer match
    jne gge_nomatch         ; no

; names match, entry already made for this group
    pop bx                  ; trash old es value on stack
    jmp SHORT gge_ret       ; leave without creating a new entry

; make new group entry
gge_4:
    mov ax,alloc_grpblk_ptr ; get current group block
    mov es,ax               ; es -> block
    cmp WORD PTR es:[0],GRP_ENT_COUNT   ; check if room for any more entries
    jb  gge_5               ; yes
    call    make_group_blk  ; make a new group block, return segment in es
    xor dx,dx               ; re-init group entry count

gge_5:
    inc WORD PTR es:[0]     ; bump count of entries in group block
    mov ax,es               ; get block segment address
    add ax,dx               ; get first free entry segment value
    inc ax                  ; adjust for block system info size of 1 paragraph
    mov es,ax               ; es -> free group entry

; set doubleword group offset to high value (force first segment compare to update)
    mov es:[0],0ffffh
    mov es:[2],07fffh

    mov es:[6],cx           ; save segment pointer to group name entry in lnames block
    mov es:[4],di           ; save offset pointer
    mov bx,current_grpdef   ; get count of current group
    inc bx                  ; make relative 1
    mov es:[8],bx           ; save it
    mov WORD PTR es:[8],0   ; zero first segdef entry pointer
    mov WORD PTR es:[14],0  ; zero flag word

gge_ret:
    pop es                  ; restore critical registers
    pop di
    pop dx
    pop cx
    pop bx
    ret
get_group_entry ENDP

;*****************************
;* MAKE_GROUP_BLK            *
;*****************************

; make a new group block
; return segment of block in es
; destroys ax,bx,dx,es

make_group_blk  PROC
    mov dx,alloc_grpblk_ptr ; keep previously last allocated block segment
    mov bx,GRP_BLK_SIZE     ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_grpblk_ptr,ax ; update last allocated block pointer

    mov es,dx               ; es -> old block
    mov es:[2],ax           ; update old block to point to new block

    mov es,ax               ; es -> new block
    xor bx,bx
    mov es:[bx],bx          ; zero count of entries in block
    mov es:[2],bx           ; zero pointer to next entry
    ret
make_group_blk  ENDP

END
