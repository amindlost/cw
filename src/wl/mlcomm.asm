;*********************************************************************
;*   MLCOMM.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/22/92                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker communal variable routines                               *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlcomm
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
PUBLIC  resolve_communals,scan_comfield
;***PUBLIC	save_local_comm,get_local_comm
PUBLIC  compute_comm_len

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   filename:BYTE,pass_number:BYTE
;***EXTRN	local_comm_count:WORD
EXTRN   com_val1:DWORD,com_val2:DWORD,name_field:BYTE
EXTRN   dgrouptext:BYTE

; initialized local variables

EVEN                        ; maximize speed on 8086 and better

; word values
near_comm_segptr    DW  0   ; pointer to segdef entry of segment used for near communals
far_comm_segptr     DW  0   ; pointer to segdef entry of segment currently used for far communals
huge_comm_len   DW  0       ; length of huge communal variable modulo 64K
nc_group_ptr    DW  0       ; pointer to near communal group entry

; double word values
far_comm_len    DD  0       ; running length of far communal variables

.DATA?

; uninitialized local variables

EVEN

; doubleword values
temp_com_val    DD  ?       ; temporary storage for computing communal variable length

;*****************************
;* Constant data             *
;*****************************

.CONST

EVEN
c_commontext    DB  'c_common',0    ; segment name to check against for near communal use
bsstext DB  'BSS',0         ; class name to check against for near communal use
far_bsstext DB  'FAR_BSS',0 ; segment/class name for far communal variables
huge_bsstext    DB  'HUGE_BSS',0    ; segment/class name for huge communal variables

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,load_file:NEAR,error_bx_pos:NEAR
EXTRN   make_pubdecl_blk:NEAR,alloc_memory:NEAR
EXTRN   make_segdef_blk:NEAR,init_segdef_entry:NEAR,get_hash:NEAR
EXTRN   make_segpart_blk:NEAR

;*****************************
;* SCAN_COMFIELD             *
;*****************************

; scan past communal length field variables
; upon entry dh == field value, si -> current buffer position,
; bx == original byte offset, di-> 4 byte memory location (pass 1 only)
; if pass 1 returns value of field in 4-byte memory location pointed by di
; destroys dx,di
; updates si

scan_comfield   PROC
    cmp dh,80h              ; check if one byte field
    jae sc_2                ; no
    mov dl,1                ; dl holds number of bytes to scan past
    jmp SHORT sc_start_scan ; scan them

sc_2:
    cmp dh,81h              ; check if two byte field
    jne sc_3                ; no
    mov dl,3                ; dl holds number of bytes to scan past, including variable size
    jmp SHORT sc_start_scan ; scan them

sc_3:
    cmp dh,84h              ; check if three byte field
    jne sc_4                ; no
    mov dl,4                ; dl holds number of bytes to scan past
    jmp SHORT sc_start_scan ; scan them

sc_4:
    cmp dh,88h              ; check if four byte field
    je  sc_5                ; yes

    mov cl,dh               ; get value in cl
    mov dx,OFFSET DGROUP:filename
    mov ax,INV_COMFIELD_VAL_ERR ; invalid communal length field value
    jmp NEAR PTR error_bx_pos   ; transfer control to error handler

sc_5:
    mov dl,5                ; dl holds number of bytes to scan past

; dl holds number of bytes to scan past
sc_start_scan:
    cmp dl,1                ; check if one byte value
    jne sc_7                ; no, first byte isn't part of value

sc_loop:
    cmp pass_number,2       ; if pass two, don't save length values (memory allocations already made)
    je  sc_7

; pass 1, save number of bytes in communal variable for allocating
    mov al,es:[si]          ; get byte value
    mov [di],al             ; save it

sc_6:
    inc di                  ; bump to next byte position of communal value

sc_7:
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  sc_8                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

sc_8:
    dec dl                  ; drop count of to-scan bytes
    jne sc_loop             ; not done, scan another byte
    ret                     ; done scanning, return
scan_comfield   ENDP

COMMENT # **********

;*****************************
;* SAVE_LOCAL_COMM           *
;*****************************

; pass 1 save local communals to nonlocal communal block, setting
; the local flag (bit 5, 20h) and putting the lib_id (library processing)
; or current_obj (object module processing) in place of the
; pub_declent_loptr and pub_declent_hiptr bytes.  Use a zero word for
; the high word of current_obj (pub_declent_hiptr).
; destroys ax,cx,dx,di,es

save_local_comm PROC
    mov ax,first_local_ptr  ; get pointer to first block

slc_blkloop:
    or  ax,ax               ; see if any more blocks
    je  slc_ret             ; no, return
    mov es,ax               ; es -> block
    mov cx,es:[0]           ; get count of entries
    jcxz    slc_ret         ; no more entries
    inc ax                  ; point to first entry

slc_entloop:
    push    es              ; save -> block
    mov es,ax               ; es -> entry
    test    BYTE PTR es:[15],40h    ; see if local communal
    je  slc_trynext         ; no

; local communal, transfer to nonlocal communal block
    call    local_transfer

slc_trynext:
    mov ax,es:[6]           ; get length of name
    add ax,1fh              ; add in size of local public entry and round up to nearest paragraph
    shr ax,1                ; convert to segment value, /2
    shr ax,1                ; /4
    shr ax,1                ; /8
    shr ax,1                ; /16
    pop es
    mov bx,es               ; bx holds block segment
    add ax,bx               ; ax holds segment of next entry
    loop    slc_entloop     ; check next entry, if any
    mov ax,es:[2]           ; no more entries in this block, get pointer to next block
    jmp SHORT slc_blkloop   ; try next block

slc_ret:
    ret
save_local_comm ENDP

;*****************************
;* LOCAL_TRANSFER            *
;*****************************

; transfer local communal to nonlocal communal block
; destroys ax

local_transfer  PROC
    push    es              ; save critical register
    mov ax,alloc_cdeclblk_ptr
    or  ax,ax               ; check if any comdef declarations blocks were previously allocated
    jne slc_2               ; yes

; make initial comdef declarations block allocation
    mov bx,PUB_DECLBLK_SIZE ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_cdeclblk_ptr,ax   ; update last allocated block pointer

    mov first_cdeclblk_ptr,ax   ; update first allocated block pointer

    mov es,ax               ; es == current (new) block segment
    xor ax,ax
    mov WORD PTR es:[0],ax  ; zero count of entries in block
    mov WORD PTR es:[2],ax  ; zero pointer to next block
    jmp SHORT slc_3         ; bypass block full check code

; at least one prior comdef declaration made, check if block is full
slc_2:
    mov es,ax               ; es -> current block
    cmp WORD PTR es:[0],PUB_DECLENT_COUNT  ; see if any free entries in block
    jb  slc_3               ; yes
    call    make_pubdecl_blk    ; no free entries in block, make a new block

slc_3:
    mov ax,es               ; get block segment address in ax
    add ax,es:[0]           ; compute first free entry segment value
    inc WORD PTR es:[0]     ; bump count of entries
    inc ax                  ; adjust for block system info size of 1 paragraph
    push    ds              ; save -> warplink data
    mov ds,ax               ; ds -> new entry

; transfer pertinent information from local communal block to nonlocal block
    mov ax,es:[2]           ; transfer communal length high word
    mov ds:[2],ax
    mov ax,es:[4]           ; transfer current extdef (LCOMDEF identifier)
    mov ds:[4],ax
    mov ax,es:[8]           ; transfer communal length low word
    mov ds:[8],ax
    mov BYTE PTR ds:[14],1  ; flag extdef/comdef
    mov ax,es:[15]          ; transfer NEAR communal bit
    mov ds:[15],ax
    or  BYTE PTR ds:[15],60h    ; set communal and local bits

    mov ax,ds
    mov es,ax               ; es -> new entry
    pop ds                  ; ds -> warplink data
    cmp is_inlib,0          ; see if processing library
    je  slc_4               ; no
    mov ax,WORD PTR lib_id+2    ; get high word of library id
    mov es:[12],ax          ; transfer it
    mov ax,WORD PTR lib_id  ; get low word of library id
    jmp SHORT slc_5         ; jump to shared code

slc_4:
    mov ax,current_obj

slc_5:
    mov es:[10],ax          ; transfer value used for identification
    pop es                  ; restore critical register
    ret
local_transfer  ENDP

;*****************************
;* GET_LOCAL_COMM            *
;*****************************

; pass 2 get local communals from nonlocal communal block
; returns segment of entry in ax, 0 if not found (LCOMDEF had corresponding LPUBDEF)
; destroys ax,bx,dx,es

get_local_comm  PROC
    mov ax,first_cdeclblk_ptr

glc_blkloop:
    or  ax,ax               ; check if any comdef declarations blocks left
    je  glc_ret             ; no, all done
    mov es,ax               ; es -> block
    mov bx,ax               ; save -> block
    mov dx,es:[0]           ; get count of entries
    or  dx,dx
    je  glc_ret             ; no more entries
    inc ax                  ; point to first entry

glc_entloop:
    mov es,ax               ; es -> entry
    test    BYTE PTR es:[15],20h    ; see if local communal bit set
    je  glc_next_entry      ; no

    cmp is_inlib,0          ; check if library processing
    je  glc_2               ; no

; processing library, check library id against entry's
    mov ax,WORD PTR lib_id+2    ; get high word
    cmp ax,es:[12]          ; see if matches
    jne glc_next_entry      ; no
    mov ax,WORD PTR lib_id  ; get low word
    jmp SHORT glc_shared    ; jump to shared code

glc_2:
    mov ax,current_obj      ; get current object module number for id purposes

glc_shared:
    cmp ax,es:[10]          ; see if ids match
    jne glc_next_entry

; file ids match, check if extdef numbers match
    mov ax,current_extdef   ; get current extdef number
    cmp ax,es:[4]           ; see if matches entry's
    mov ax,es               ; preload ax, flags not affected
    je  glc_ret             ; entry matches, return segment in ax

glc_next_entry:
    inc ax                  ; point to next entry (each entry 16 bytes)
    dec dx                  ; drop count of entries
    jne glc_entloop         ; more left, check them
    mov es,bx               ; es -> communal block
    mov ax,es:[2]           ; get pointer to next block
    jmp SHORT glc_blkloop   ; pull entries from next block, if any

glc_ret:
    ret
get_local_comm  ENDP

********** END COMMENT #

;*****************************
;* COMPUTE_COMM_LEN          *
;*****************************

; compute communal length from memory variables com_val1 and com_val2
; return product in com_val1
; destroys ax,dx

compute_comm_len    PROC
    push    cx              ; save critical register
    mov ax,WORD PTR com_val1    ; get low word of first value
    mul WORD PTR com_val2       ; multiply by low word of second value
    mov WORD PTR temp_com_val,ax
    mov WORD PTR temp_com_val+2,dx  ; save to temporary value
    mov ax,WORD PTR com_val1    ; get low word of first value
    mul WORD PTR com_val2+2     ; multiply by high word of second value
    or  dx,dx                   ; see if overflow occurred
    jne ccl_overflow            ; yes, fatal error
    add WORD PTR temp_com_val+2,ax
    jc  ccl_overflow            ; high word cannot overflow
    mov ax,WORD PTR com_val1+2  ; get high word of first value
    mul WORD PTR com_val2       ; multiply by low word of second value
    or  dx,dx                   ; see if overflow occurred
    jne ccl_overflow            ; yes, fatal error
    add WORD PTR temp_com_val+2,ax
    jc  ccl_overflow            ; high word cannot overflow
    mov ax,WORD PTR com_val1+2  ; get high word of first value
    mul WORD PTR com_val2+2     ; multiply by high word of second value
    or  dx,ax                   ; value should be zero
    jne ccl_overflow            ; not zero, overflow occurred
    mov ax,WORD PTR temp_com_val    ; stuff value back into com_val1
    mov WORD PTR com_val1,ax
    mov ax,WORD PTR temp_com_val+2
    mov WORD PTR com_val1+2,ax
    pop cx                  ; restore critical register
    ret

; communal variable overflow
ccl_overflow:
    mov dx,OFFSET DGROUP:filename
    mov ax,COMM_OVRFLW_ERR
    mov cx,OFFSET DGROUP:name_field ; cx -> symbol name for this error
    jmp NEAR PTR link_error ; transfer control to error handler

compute_comm_len    ENDP

;*****************************
;* RESOLVE_COMMUNALS         *
;*****************************

; resolve communal variables addresses that have no corresponding PUBDEF,
; adjusting or creating segments as necessary
; destroys ax,bx,cx,dx,di,si,es

resolve_communals   PROC
    cmp communal_count,0    ; see if any communals to resolve
    je  rc_ret              ; no

    mov ax,first_cdeclblk_ptr

rc_blkloop:
    or  ax,ax               ; check if any comdef declarations blocks left
    je  rc_ret              ; no, all done
    mov es,ax               ; es -> block
    mov bx,ax               ; save -> block
    mov cx,es:[0]           ; get count of entries
    jcxz    rc_ret          ; no more entries
    inc ax                  ; point to first entry

rc_entloop:
    mov es,ax               ; es -> entry
    test    BYTE PTR es:[15],40h    ; see if communal bit set
    je  rc_next_entry       ; no
    test    BYTE PTR es:[15],10h    ; see if near communal
    jne rc_near             ; yes
    call    res_far_communal    ; resolve far communal
    jmp SHORT rc_next_entry ; check next entry, if any

rc_near:
    call    res_near_communal   ; resolve near communal

rc_next_entry:
    mov ax,es
    inc ax                  ; point to next entry (each entry 16 bytes)
    loop    rc_entloop      ; loop thru all entries
    mov es,bx               ; es -> communal block
    mov ax,es:[2]           ; get pointer to next block
    jmp SHORT rc_blkloop    ; pull entries from next block, if any

rc_ret:
    ret
resolve_communals   ENDP

;*****************************
;* RES_NEAR_COMMUNAL         *
;*****************************

; resolve near communal variables
; upon entry es -> communal declaration entry
; destroys ax,dx,di,si

res_near_communal   PROC
    push    bx              ; save critical register
    push    cx
    push    es              ; save -> communal entry
    mov ax,near_comm_segptr ; get pointer to segdef entry of segment for near communal variables
    or  ax,ax               ; see if set yet
    je  rnc_create          ; no, create segment
    mov dx,nc_group_ptr     ; get pointer to group entry (if any) in dx
    mov es,ax               ; es -> segdef entry
    jmp NEAR PTR rnc_res_com    ; bypass segment search

; create segment c_common, class BSS, group DGROUP
rnc_create:
    call    create_comm_segment ; create segdef and segment partition entry for new segment
    mov near_comm_segptr,ax ; save pointer to near communal variables segdef entry
    mov ax,ds               ; update segment and class names, pointing them into warplink data
    mov es:[10],ax
    mov es:[14],ax          ; save segment pointer to segment and class names
    mov ax,OFFSET DGROUP:c_commontext-8 ; get offset to segment name, back off 8 bytes to match lnames entry format
    mov es:[8],ax           ; save offset pointer to segment name
    mov ax,OFFSET DGROUP:bsstext-8  ; get offset to segment class, back off 8 bytes to match lnames entry format
    mov es:[12],ax          ; save offset pointer to segment class
    mov BYTE PTR es:[26],01100000b  ; paragraph aligned, private combine
    xor ax,ax
    mov es:[27],ax          ; zero miscellaneous flag bytes

; point segment at group DGROUP if it exists
    push    es              ; save es -> segdef entry
    mov ax,first_grpblk_ptr ; get segment of first allocated group block

rnc_group_loop:
    or  ax,ax               ; check if null block
    je  rnc_nomatch         ; null, no more entries, no match occurred
    mov es,ax               ; es -> group block
    mov bx,es               ; save -> group block
    mov cx,es:[0]           ; get number of entries in group block
    jcxz    rnc_nomatch     ; no more entries, no match found
    inc ax                  ; ax -> first entry

rnc_group_entry:
    mov es,ax               ; es -> group entry
    mov dx,ax               ; save -> group entry
    mov si,OFFSET DGROUP:dgrouptext ; ds:si -> DGROUP name
    mov di,es:[4]           ; get offset of group name in lnames block
    mov es,es:[6]           ; get segment of group name in lnames block
    add di,8                ; adjust past 2 doubleword pointers in lnames entry

; es:di -> group entry name, ds:si -> DGROUP name
rnc_matchloop:
    mov al,[si]
    or  al,es:[di]          ; see if both values are zero (matched to null terminator)
    je  rnc_match           ; yes
    cmpsb                   ; compare a nonzero byte in the two names
    je  rnc_matchloop       ; bytes match, loop for next byte test

rnc_next_entry:
    mov ax,dx               ; ax -> last group entry
    inc ax                  ; point to next group entry
    loop    rnc_group_entry ; loop thru all entries in block

; no more entries in group block, get next group block
    mov es,bx               ; es -> group block
    mov ax,es:[2]           ; get pointer to next group
    jmp SHORT rnc_group_loop

; DGROUP group entry found, segment in dx
rnc_match:
    pop es                  ; restore es -> segdef entry
    mov es:[16],dx          ; save -> group entry in segdef entry
    pop ds                  ; ds -> communal entry
    push    ds
    mov ax,DGROUP
    mov ds,ax               ; ds -> WarpLink data
    mov nc_group_ptr,dx     ; save near communal group entry pointer
    jmp SHORT rnc_res_com   ; bypass no group match code restore from stack

rnc_nomatch:
    xor dx,dx               ; zero dx to show no group match
    pop es                  ; restore es -> segdef entry

; segment created, es -> segdef entry, now resolve communal
rnc_res_com:
    pop ds                  ; ds -> communal entry
    or  dx,dx               ; see if a group pointer to near communals
    je  rnc_5               ; no

    or  BYTE PTR ds:[15],80h    ; flag that group is associated with communal declaration
    mov ds:[2],dx           ; save pointer to group entry in communal declaration

rnc_5:
    mov ax,es:[22]          ; get pointer to first segment partition entry
    mov ds:[0],ax           ; save pointer in communal declaration entry
    mov BYTE PTR ds:[14],2  ; flag as resolved public
    mov ax,es:[6]           ; get old segment length
    cmp ax,0ffffh           ; see if segment overflow will occur
    je  rnc_segover_err     ; segment too large
    test    BYTE PTR es:[26],2  ; see if Big bit is set
    jne rnc_segover_err     ; yes

    mov cx,ds:[8]           ; get low word of communal length
    mov ds:[8],ax           ; save communal variable offset (previous end of segment)
    add ax,cx
    jnc rnc_6               ; no overflow
    or  ax,ax               ; overflow, check if ax is zero (segment exactly 64k)
    jne rnc_segover_err     ; no, true segment overflow
    or  BYTE PTR es:[26],2  ; set Big bit

rnc_6:
    mov es:[6],ax           ; save new segment length
    mov ax,ds
    mov es,ax               ; restore es -> communal entry
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    pop cx
    pop bx
    ret

rnc_segover_err:
    mov dx,OFFSET DGROUP:exe_name
    mov ax,DEF_SEG_OVRFLW_ERR   ; default data segment size exceeds 64K
    jmp NEAR PTR link_error     ; transfer control to error handler

res_near_communal   ENDP

;*****************************
;* RES_FAR_COMMUNAL          *
;*****************************

; resolve far communal variables
; upon entry es -> communal declaration entry
; destroys ax,dx,di,si

res_far_communal    PROC
    push    bx              ; save critical register
    push    cx
    push    es              ; save -> communal declaration entry
    mov ax,es:[8]           ; get low word of communal length in ax
    cmp WORD PTR es:[2],1   ; high word length more than 1 if huge
    jb  rfc_far             ; zero if far
    ja  rfc_huge            ; huge communal
    or  ax,ax               ; low word length nonzero if huge
    jne rfc_huge            ; nonzero with nonzero high word, huge communal

; far communal variable (<64k)
rfc_far:
    add ax,WORD PTR far_comm_len    ; add in previous communal lengths
    mov bx,es:[2]           ; get high word of communal length
    adc bx,WORD PTR far_comm_len+2  ; add with carry in high word of previous lengths

    cmp bx,1                ; see if >64K total length
    jb  rfc_nonew           ; no, add to total running communal variable length
    ja  rfc_new             ; yes, create a new segment for previous communals
    or  ax,ax               ; could be exactly 64K, check for nonzero low word
    jne rfc_new             ; nonzero, >64K length, create new segment

rfc_nonew:
    mov dx,far_comm_segptr  ; get pointer to segdef entry for far communals
    or  dx,dx               ; see if null (no far communal segment created yet)
    jne rfc_1               ; non-null, communal segment exists

rfc_new:
    mov si,OFFSET DGROUP:far_bsstext-8  ; get offset to segment name, back off 8 bytes to match lnames entry format
    call    update_comm_segment

    mov dx,es               ; save -> segdef entry in dx
    mov far_comm_segptr,dx  ; save pointer to segdef entry in memory variable
    pop es                  ; es -> communal variable entry
    push    es              ; restore -> communal variable to stack
    mov ax,es:[8]           ; get low word of communal length
    mov bx,es:[2]           ; get high word of communal length

rfc_1:
    mov es,dx               ; es -> segdef entry
    mov dx,es:[6]           ; get previous segment length for public offset
    mov es:[6],ax           ; save new segment length
    or  bx,bx               ; see if high word nonzero
    je  rfc_2               ; no
    or  BYTE PTR es:[26],2  ; nonzero (64K segment), set Big bit

rfc_2:
    mov WORD PTR far_comm_len,ax
    mov WORD PTR far_comm_len+2,bx  ; save new communal variable length
    mov ax,es:[22]          ; get pointer to segment partition entry
    pop es                  ; es -> communal declaration entry
    mov es:[0],ax           ; update communal entry segment partition entry pointer
    mov es:[8],dx           ; save offset of variable
    mov BYTE PTR es:[14],2  ; flag as resolved public
    jmp SHORT rfc_ret       ; return

; huge communal variable (>=64k)
rfc_huge:
    mov huge_comm_len,ax    ; save length of huge communal variable modulo 64K
    mov cx,es:[2]           ; get count of 64K huge segments to create
    xor dx,dx               ; nonzero dx flags not first time thru segment creation loop

rfc_loop:
    mov si,OFFSET DGROUP:huge_bsstext-8 ; get offset to segment name, back off 8 bytes to match lnames entry format
    call    update_comm_segment

    mov WORD PTR es:[6],0   ; zero segment length
    or  BYTE PTR es:[26],2  ; set Big bit
    or  dx,dx               ; see if first time thru loop
    jne rfc_3               ; no, bypass communal entry update

    mov dx,1                ; set flag so update only occurs once
    mov ax,es:[22]          ; get pointer to segment partition entry
    pop es                  ; es -> communal declaration entry
    push    es              ; restore -> communal entry to stack
    mov es:[0],ax           ; update communal entry segment partition entry pointer
    mov BYTE PTR es:[14],2  ; flag as resolved public
    mov WORD PTR es:[8],0   ; zero offset of variable

rfc_3:
    loop rfc_loop           ; loop until all 64K segments created

    mov si,OFFSET DGROUP:huge_bsstext-8 ; get offset to segment name, back off 8 bytes to match lnames entry format
    call    update_comm_segment

    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    mov ax,huge_comm_len    ; get low word of communal length
    mov es:[6],ax           ; save length of segment (communal length modulo 64K)
    pop es                  ; restore es -> communal entry

rfc_ret:
    pop cx                  ; restore critical register
    pop bx
    ret
res_far_communal    ENDP

;*****************************
;* UPDATE_COMM_SEGMENT       *
;*****************************

; update segment and class name and acbp byte of far or huge communal segment
; upon entry si -> offset of segment/class name (adjusted back 8 bytes to emulate lnames entries)
; destroys ax,bx,di,es

update_comm_segment PROC
    call    create_comm_segment
    mov ax,ds               ; update segment and class names, pointing them into warplink data
    mov es:[10],ax
    mov es:[14],ax          ; save segment pointer to segment and class names
    mov es:[8],si           ; save offset pointer to segment name
    mov es:[12],si          ; save offset pointer to segment class
    mov BYTE PTR es:[26],01100000b  ; para aligned, private combine
    xor ax,ax
    mov es:[27],ax          ; zero miscellaneous flag bytes
    ret
update_comm_segment ENDP

;*****************************
;* CREATE_COMM_SEGMENT       *
;*****************************

; create segment for communal variable
; returns segment of segdef entry in es,ax
; destroys ax,bx,di,es

create_comm_segment PROC
    push    dx              ; save critical register
    mov ax,alloc_segdefblk_ptr
    or  ax,ax               ; check if any segdef blocks were previously allocated
    jne ccs_2               ; yes

; make initial segdef block allocation
    mov bx,SEG_DEFBLK_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_segdefblk_ptr,ax  ; update last allocated block pointer
    mov first_segdefblk_ptr,ax  ; update first allocate block pointer
    mov es,ax               ; es == current (new) block segment
    xor ax,ax
    mov WORD PTR es:[0],ax  ; zero count of entries in block
    mov WORD PTR es:[2],ax  ; zero pointer to next entry
    jmp SHORT ccs_3         ; bypass block full check code

; at least one prior segdef entry made, check if block is full
ccs_2:
    mov es,ax               ; es -> current block
    cmp WORD PTR es:[0],SEG_DEFENT_COUNT    ; see if any free entries in block
    jb  ccs_3                ; yes
    call    make_segdef_blk ; no free entries in block, make a new block

ccs_3:
    call    init_segdef_entry   ; perform segdef entry initialization code
    mov es,di               ; es -> new segdef entry
    inc WORD PTR seg_count  ; update count of total discrete segments

; make a dummy segment partition entry for the new segment (so fixup routines work correctly)
    push    es              ; save es -> segdef entry
    mov ax,alloc_segpartblk_ptr ; get last allocated block segment
    or  ax,ax               ; was there a previous block
    jne ccs_4               ; yes

; make initial pubdef declarations block allocation
    mov bx,SEG_PARTBLK_SIZE ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_segpartblk_ptr,ax ; update last allocated block pointer
    mov first_segpartblk_ptr,ax ; update first
    mov es,ax               ; es -> block
    mov WORD PTR es:[0],0   ; zero count of entries in block
    mov WORD PTR es:[2],0   ; zero pointer to next entry
    jmp SHORT ccs_5         ; bypass block full check code

; at least one prior segment partition block, check if block is full
ccs_4:
    mov es,ax               ; es -> current block
    cmp WORD PTR es:[0],SEG_PARTENT_COUNT   ; see if any free entries in block
    jb  ccs_5               ; yes
    call    make_segpart_blk    ; no free entries in block, make a new block

ccs_5:
    mov bx,es:[0]           ; get entry count
    mov ax,es               ; get block segment address
    add bx,ax               ; get first free entry segment value
    inc bx                  ; adjust for block system info size of 1 paragraph
    inc WORD PTR es:[0]     ; increment count of entries in segment partition block

    mov es,bx               ; es -> segment partition entry
    xor ax,ax
    mov es:[2],ax           ; zero pointer to next block
    mov es:[15],al          ; zero flag byte

    pop ax                  ; get segdef entry segment pointer
    mov es:[4],ax           ; save pointer to master segdef entry
    mov WORD PTR es:[0],0   ; zero segment partition offset
    mov es,ax               ; restore es -> segdef entry
    mov es:[22],bx          ; save -> first segment partition entry
    pop dx                  ; restore critical register
    ret
create_comm_segment ENDP

END
