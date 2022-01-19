;*********************************************************************
;*   MLOVLRES.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/22/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker overlay file routines                                    *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlovlres
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
PUBLIC  resolve_ovl_info,ovl_entry_point,fixup_ovl_extdef
PUBLIC  fixup_ind_ref_extdef,fixup_seg_ref,make_master_segblk

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   ovlpubblk_pos:WORD,loc:BYTE,target_prog_off:DWORD
EXTRN   ovl_pub_seg_ptr:DWORD,data_seg_part:WORD
EXTRN   data_offset:DWORD,locat:WORD,ind_segment:WORD,segcall_segment:WORD
EXTRN   seg_ovl_class_flag:BYTE,is_ind_call:BYTE
EXTRN   target_segment:WORD,is_relocatable:BYTE
EXTRN   target_disp:DWORD,data_rec_offset:WORD
EXTRN   large_ovl_name:DWORD
EXTRN   frame_method:BYTE,filename:BYTE
EXTRN   no_fixbyte_flag:BYTE,gen_flags:BYTE
EXTRN   fixup_pos:WORD

; initialized local variables

; byte values
tbuff   DB  ' 00K '          ; buffer for minimum overlay allocation feedback
left_paren  DB  '('
right_paren DB  ')',CR,LF

.DATA?

EVEN

; uninitialized local variables
curseg_ptr  DW  ?           ; pointer to current segment in overlaid public block
curblk_ptr  DW  ?           ; pointer to block for which curseg_ptr is valid
mainovl_offval  DW  ?       ; main overlay vector entry address offset
mainovl_segval  DW  ?       ; main overlay vector entry address segment

;*****************************
;* Constant data             *
;*****************************

.CONST

maxtext_len     DB  maxtext_stop-max_text
max_text        DB  CR,LF,'Minimum overlay allocation exceeds maximum pool size, size adjusted.'
maxtext_stop    =   $

mintext_len     DB  mintext_stop-min_text
min_text        DB  CR,LF,'Minimum allowed overlay allocation: '
mintext_stop    =   $

rectext_len     DB  rectext_stop-rec_text
rec_text        DB  'Required minimum for /r usage: '
rectext_stop    =   $

clartext_len    DB  clartext_stop-clar_text
clar_text       DB  'Required minimum for Clarion usage: '
clartext_stop   =   $

entry_text      DB  '$$_OVL_INIT_ENTRY',0
main_text       DB  '$$_OVL_VECTOR',0

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   link_error:NEAR,dos_error:NEAR,alloc_memory:NEAR
EXTRN   setup_ovl_file:NEAR,link_warning:NEAR
EXTRN   make_segdef_blk:NEAR,init_segdef_entry:NEAR
EXTRN   find_pubdecl_entry:NEAR,get_symbol_segoff:NEAR
EXTRN   restore_ems_map:NEAR

;*****************************
;* RESOLVE_OVL_INFO          *
;*****************************

; resolve overlay information, set up for second pass
; destroys ax,bx,cx,dx,di,bp,si,es

resolve_ovl_info    PROC
    xor dx,dx               ; init overlaid public count after check for overlay class
    xor bp,bp               ; init far reference count after check
    mov ax,first_ovlpubblk_ptr  ; get first allocate overlaid public block
    mov bx,2                ; init block position
    mov es,ax               ; es -> public block
    jmp NEAR PTR res_blk_chk2   ; bypass previous segment entry update

; no more overlaid publics for this segment, move to next, if any
; update length of previous segment (dx*7 bytes),
; nonoverlaid segment partition updates segdef entry length
; overlay updates segment partition entry length
; set si to segment partition, re-init cx,dx counters
res_seg_done:
    mov di,curseg_ptr       ; get pointer to current segment value
    add di,2                ; bump to near references count
    push    es              ; save -> current overlaid public block on stack
    mov es,curblk_ptr       ; es -> block for curseg_ptr
    mov es:[di],dx          ; save final overlaid public count for current segment
    mov es:[di+2],bp        ; save far public references
    pop di                  ; di -> current overlaid public block from stack
    or  dx,dx               ; see if any overlaid publics left for update length
    jne res_seg2            ; yes
    jmp NEAR PTR res_blk_chk    ; no, bypass segment updates

; the segment is too long as a result of adding overlay overhead, fatal error
res_seglen_err:
    or  al,al               ; see if overlay or root segment
    je  res_rootseg_err     ; root

res_ovlseg_err:
    mov ax,OVLSEG_LEN_ERR   ; overlay segment too long
    jmp NEAR PTR link_error ; transfer control to error handler

res_rootseg_err:
    mov ax,ROOTSEG_LEN_ERR  ; root segment too long
    jmp NEAR PTR link_error ; transfer control to error handler

res_ovlmodpub_err:
    mov ax,OVL_MOD_PUB_MAX_ERR
    jmp NEAR PTR link_error ; transfer control to error handler

res_seg2:
    mov es,si               ; es -> segment partition entry of completed overlaid public sequence

    cmp dx,OVL_MOD_PUB_MAX  ; see if too many overlaid public references
    ja  res_ovlmodpub_err   ; yes

; adjust for far vector removal here
; subtract from dx the value at curblk_ptr:curseg_ptr+4 (far references)
    mov ax,es
    mov si,curseg_ptr
    mov es,curblk_ptr
    sub dx,es:[si+4]        ; back off far references count
    mov es,ax               ; restore es

    mov cx,dx               ; save vector count in cx

    mov ax,7
    mul dx                  ; get count of bytes required by vectors
    mov dx,ax               ; get value in dx

    test    BYTE PTR es:[15],80h    ; see if overlaid segment
    jne res_ovl_update      ; yes, update overlay segment

; update root segment
    mov es,es:[4]           ; get owner segdef entry
    mov si,es:[30]          ; get previous total vector bytes
    add es:[30],dx          ; update total vector bytes count
    test    BYTE PTR es:[28],2  ; see if length update bit was previously set
    jne res_old_update      ; yes

; first update for root segment, add 20h bytes for near and shared vector bytes
    or  BYTE PTR es:[28],2  ; set length updated bit
    add dx,20h              ; bump dx value to be added to segment length

res_old_update:
    add es:[6],dx           ; add vector bytes to segment length
    jnc res_prev_vectbyte   ; segment length hasn't overflowed, continue
    mov ax,es:[6]           ; get new length
    or  ax,ax               ; see if zero (segment okay if and only if new length is 64K)
    jne res_rootseg_err     ; segment too big
    or  BYTE PTR es:[26],2  ; set Big bit

; save the previous total vector byte count prior to this segment partition update
res_prev_vectbyte:
    mov ax,si               ; get vector byte count prior to current vector bytes
    mov es,curblk_ptr       ; es -> block for curseg_ptr
    mov si,curseg_ptr       ; get pointer to current segment value
    mov es:[si+6],ax        ; save vector byte count to this segment partition entries vector bytes

    jmp SHORT res_blk_chk   ; bypass overlay segment update

; jump to fatal error routine
to_res_ovlseg_err:
    jmp NEAR PTR res_ovlseg_err

res_ovl_update:
    add dx,20h              ; bump dx value to be added to segment length for near/shared vectors
    or  ch,10h              ; set overlaid segment length update bit
    mov es:[14],cl          ; save low word of vector count
    or  es:[15],ch          ; merge in high word of vector count with flag bits

    add es:[12],dx          ; add vector bytes to segment length
    jnc res_blk_chk         ; segment length hasn't overflowed, continue
    mov ax,es:[12]          ; get new length
    or  ax,ax               ; see if zero (segment okay if and only if new length is 64K)
    jne to_res_ovlseg_err   ; segment too big
    or  BYTE PTR es:[11],2  ; set Big bit

; check if in last block, if so, check if at last used position
res_blk_chk:
    mov es,di               ; restore es -> current overlaid public block
    mov ax,di

res_blk_chk2:
    cmp ax,alloc_ovlpubblk_ptr  ; see if on last block
    jne more_segs           ; no
    cmp bx,ovlpubblk_pos    ; see if at last position
    jb  more_segs           ; no
    jmp NEAR PTR chk_segdefs    ; yes, done

; internal error, this entry is not a segment partition entry
res_internal_err:
    mov ax,INTERNAL_ERR     ; put warplink error code in ax
    mov cx,11               ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

to_res_count_err:
    jmp NEAR PTR res_count_err

; check if room in block for reading in next segment partition info
more_segs:
    cmp bx,OVLPUB_BLK_BYSIZE-9  ; see if at end of block, no room for segment partition entry info
    jb  res_get_seg         ; no
    mov es,es:[0]           ; get pointer to next block in ax
    mov bx,2                ; init block position

res_get_seg:
    mov al,es:[bx]          ; get flag byte
    cmp al,2                ; check if equals non-overlay class segment partition value
    je  res_nonovl          ; yes
    cmp al,1                ; check if equals overlay class segment partition value
    jne res_internal_err    ; no, internal error
    jmp NEAR PTR res_seg_okay  ; yes

; Non-overlay class segment partition entry that references potentially
; overlaid publics.
; All that is necessary is to update the ovl_pubcount variable and
; the overlaid public number for the public declaration entry if overlaid.

res_nonovl:
    inc bx
    mov curseg_ptr,bx       ; get position in block holding entry segment
    mov curblk_ptr,es       ; save block holding position
    mov cx,es:[bx+4]        ; get far references (should only be far due to pointer filter)
    xor dx,dx               ; init overlaid public count after check for overlay class
    xor bp,bp               ; init far reference count too
    add bx,8                ; scan past info to first overlaid public
    mov di,ovl_pubcount     ; get count of overlaid publics

nonovl_loop:
    cmp bx,OVLPUB_BLK_BYSIZE-3  ; see if at end of block, no room for public declaration entry info
    jb  nonovl_1            ; no
    mov es,es:[0]           ; get pointer to next block in ax
    mov bx,2                ; init block position

nonovl_1:
    mov ax,es:[bx+1]        ; get overlaid public declaration entry
    push    ds              ; save data segment
    mov ds,ax               ; ds -> public declaration entry
    xor al,al               ; setup al to reset far flag
    test    BYTE PTR ds:[15],1  ; see if overlaid public
    je  nonovl_2            ; no

; overlaid public far referenced by non-overlay class segment
    mov ax,ds:[2]           ; get overlaid public number
    or  ax,ax               ; see if overlaid public number is zero
    je  newovl              ; zero, new overlaid public

; see if group flag is set, if so then number is a group pointer, reset and use
    xor al,al               ; setup al to reset far flag
    test    BYTE PTR ds:[15],80h    ; check group bit
    je  nonovl_2            ; not in group, nonzero overlaid public number, already used
    and BYTE PTR ds:[15],7fh    ; reset group flag, use this overlaid public

newovl:
    inc di                  ; bump overlaid public count
    inc dx                  ; bump count of overlaid publics for this segment partition
    inc bp                  ; bump count of far publics
    cmp di,OVERLAY_MAX_COUNT    ; check if maximum number of overlaid publics exceeded
    ja  to_res_count_err    ; yes, terminate with error
    mov ds:[2],di           ; set overlaid public number for entry
    mov al,80h              ; setup al to set far flag

nonovl_2:
    mov es:[bx],al          ; re/set far flag
    pop ds                  ; restore ds -> warplink data
    add bx,3                ; move to next overlaid public entry
    loop    nonovl_loop     ; loop through all entries
    mov ovl_pubcount,di     ; update overlaid public count

    mov ax,es
    mov di,curseg_ptr       ; get pointer to old block segment value
    mov es,curblk_ptr       ; es -> block for curseg_ptr
    mov es:[di+2],dx        ; save total overlaid public references for segment entry
    mov es:[di+4],bp        ; save far public references

    mov es,ax               ; restore es -> current overlaid public block
    jmp NEAR PTR res_blk_chk2   ; get next segment partition entry in overlaid block

res_seg_okay:
    inc bx
    mov curseg_ptr,bx       ; get position in block holding entry segment
    mov curblk_ptr,es       ; save block holding position
    mov si,es:[bx]          ; get segment partition entry segment in si
    mov cx,es:[bx+2]        ; get near references in cx
    add cx,es:[bx+4]        ; add in far references
    xor dx,dx               ; init overlaid public count after check for overlay class
    xor bp,bp               ; init far reference count too
    add bx,8                ; scan past info to first overlaid public

res_get_pubs:
    cmp bx,OVLPUB_BLK_BYSIZE-3  ; see if at end of block, no room for public declaration entry info
    jb  resolve_1           ; no
    mov es,es:[0]           ; get pointer to next block in ax
    mov bx,2                ; init block position

resolve_1:
    mov ax,es:[bx+1]        ; get overlaid public declaration entry
    mov di,ovl_pubcount
    push    ds              ; save data segment
    mov ds,ax               ; ds -> public declaration entry

    test BYTE PTR ds:[15],1 ; see if overlaid public
    je  resolve_2           ; no, check overlay class bit

; overlaid public, see if owning segment partition matches current segment partition
    cmp si,ds:[0]           ; see if segment partition entries match
    jne res_chk_pubnum      ; no, check public number to see if should update

; owning segment partition matches current segment partition
; ignore near calls
; only setup vectors for far calls internal to a routine
    mov al,es:[bx]          ; get flag bits

    test al,40h             ; see if near bit set
    je  res_chkfar          ; no, check far bit flag
    and al,0bfh             ; reset near bit
    mov es:[bx],al          ; save new status of bit flags
    dec cx                  ; drop count of overlaid publics left to check

res_chkfar:
    and al,80h              ; get far bit flag
    je  resolve_nextpub     ; not set, try next public
    jmp SHORT res_chk_pubnum    ; set, check public number to see if should update

resolve_2:
    test    BYTE PTR ds:[15],8  ; check if overlay class bit
    je  resolve_nextpub     ; not set, try next public
    mov ax,ds               ; save ds -> overlaid public entry
    mov ds,si               ; ds -> current segment partition entry from overlay publics block
    test    BYTE PTR ds:[15],80h    ; see if segment partition is overlaid
    mov ds,ax               ; restore ds -> overlaid public entry
    je  resolve_nextpub     ; segment partition is not overlaid

; this public declaration meets the requirements for vectoring,
; in an different overlay from where it is called or in root called from an overlay
; now see if it has already had a public number assigned to it
res_chk_pubnum:
    mov ax,ds:[2]           ; get overlaid public number
    or  ax,ax               ; see if overlaid public number is zero
    jne resolve_oldpub      ; no, already used

res_bumpcount:
    inc di                  ; bump overlaid publics count
    cmp di,OVERLAY_MAX_COUNT    ; check if maximum number of overlaid publics exceeded
    ja  res_count_err       ; yes, terminate with error

resolve_3:
    mov ds:[2],di           ; save overlaid public number
    pop ds                  ; restore data segment
    mov ovl_pubcount,di     ; update overlaid public count
    jmp SHORT res_bump_count    ; bypass old public stack adjustment

; too many references to overlay class symbols
res_count_err:
    mov ax,OVLPUB_COUNT_ERR
    jmp NEAR PTR link_error ; transfer control to error handler

resolve_oldpub:
    pop ds                  ; restore data segment

; bump the count of overlaid publics in dx, near and far are treated as discrete
res_bump_count:
    mov al,es:[bx]          ; get flag bits
    test    al,80h          ; see if far bit set
    je  resolve_4           ; no
    inc dx
    inc bp                  ; bump count of far references
    dec cx

resolve_4:
    and al,040h             ; see if near bit set
    je  res_update_pos      ; no
    inc dx
    dec cx
    jmp SHORT res_update_pos    ; check next entry

; unvectored overlaid public
resolve_nextpub:
    pop ds                  ; restore data segment

; decrement the value in cx for each near and far bit set
resolve_next2:
    mov al,es:[bx]
    mov BYTE PTR es:[bx],0  ; zero out flag bits in entry (no vectors needed)
    and al,0c0h             ; get near and far flag bits
    test    al,80h          ; see if far bit set
    je  res_next3           ; no
    dec cx

res_next3:
    and al,40h              ; see if near bit set
    je  res_update_pos      ; no
    dec cx

res_update_pos:
    add bx,3                ; move to next overlaid public entry
    or  cx,cx               ; see if any overlaid publics left for this segment
    je  to_res_seg_done     ; no
    jmp NEAR PTR res_get_pubs   ; yes, loop back for them

to_res_seg_done:
    jmp NEAR PTR res_seg_done   ;  move to next segment

; step thru each segdef entry
; if overlay class and length updated bit not set and length > 0
; then add 20h to length
chk_segdefs:
    mov ax,first_segdefblk_ptr  ; get first segdef block pointer

segdef_blkloop:
    or  ax,ax               ; see if another block
    je  create_publookup    ; no
    mov es,ax               ; es -> segdef block
    mov si,es               ; keep -> segdef block

    mov cx,es:[0]           ; get count of entries in block
    inc ax                  ; bump to first segdef entry in block

segdef_entloop:
    mov es,ax               ; es -> segdef entry
    mov al,es:[28]          ; get overlay flag
    and al,3                ; get overlay class and length updated bits
    je  next_segdef         ; not overlay class
    and al,2                ; get length updated bit
    jne next_segdef         ; length updated

; overlay class and length not updated
    cmp WORD PTR es:[6],0   ; see if length of segment was 0 (no calls to segment)
    je  next_segdef         ; yes
    add WORD PTR es:[6],20h ; add space for near vector (must include space for shared vector too)
    jnc next_segdef         ; no overflow
    mov ax,es:[6]
    or  ax,ax               ; see if zero after overflow (segment now 64K)
    je  seg_len_okay        ; yes
    jmp NEAR PTR res_rootseg_err    ; no, segment length overflow

seg_len_okay:
    or  BYTE PTR es:[28],2  ; set Big bit

next_segdef:
    mov ax,es               ; get old segdef entry
    add ax,2                ; move to next segdef entry in block
    loop    segdef_entloop  ; loop through all entries in block

    mov es,si               ; restore es -> segdef block
    mov ax,es:[2]           ; get pointer to next block, if any
    jmp SHORT segdef_blkloop    ; loop for next block

; update length of the public lookup segment
; allocation equals ovl_pubcount*4 bytes
create_publookup:
    mov ax,lookup_tbl_segdef    ; get segdef entry for overlaid public lookup table
    mov es,ax               ; es -> segdef entry for segment
    mov ax,ovl_pubcount     ; get count of overlaid publics
    shl ax,1
    shl ax,1
    shl ax,1                ; quadword entry for each public
    mov es:[6],ax           ; save segment length

; update length of the indirect call segment
; allocate equals ovl_pubcount*3 + 16 bytes
    mov ax,ind_tbl_segdef   ; get segdef entry for overlaid public indirect call table
    mov es,ax               ; es -> segdef entry for segment
    mov ax,ovl_pubcount     ; get count of overlaid publics
    mov bx,ax
    shl ax,1                ; x2
    add ax,bx               ; x3
    add ax,10h              ; x3+10h
    mov es:[6],ax           ; save segment length

; update length of the segment call via segment fixup segment
; allocate equals ovl_count*3 + 16 bytes
    mov ax,segcall_tbl_segdef   ; get segdef entry for overlaid public indirect call table
    mov es,ax               ; es -> segdef entry for segment
    mov ax,ovl_count        ; get count of overlays
    mov bx,ax
    shl ax,1                ; x2
    add ax,bx               ; x3
    add ax,10h              ; x3+10h
    mov es:[6],ax           ; save segment length

; create and write system info to OVL file, pad remaining with zeros
write_ovl_sysinfo:
    call    setup_ovl_file  ; create the overlay file and write information to it

; feedback minimum overlay allocation, and adjust max/give warning if necessary
    call    get_min_ovl_alloc

    ret
resolve_ovl_info    ENDP

;*****************************
;* GET_MIN_OVL_ALLOC         *
;*****************************

; check that minimum overlay allocation does exceed maximum, adjust if so and
; give associated warning
; feedback the minimum overlay allocation amount
; destroys ax,bx,cx,dx,di

get_min_ovl_alloc   PROC
    mov ax,largest_ovl      ; get size of largest overlay in bytes
    add ax,1023             ; round up to next 1K boundary
    jnc gm_2                ; no overflow
    mov ax,64               ; segment page size is 64K
    mov bx,256              ; bx holds page size in 256 byte blocks
    jmp SHORT gm_3          ; bypass segment page size calculation

gm_2:
    and ax,0fc00h           ; put on 1K boundary
    mov al,ah
    xor ah,ah               ; effective divide by 256
    mov bx,ax               ; save page size in 256 byte blocks
    shr ax,1                ; /512
    shr ax,1                ; /1024, ax holds 1K pages of overlay

gm_3:
    cmp is_min_pool,0       ; see if minimum pool size specified
    je  gm_3a               ; no
    mov WORD PTR ovl_pool+1,bx  ; set the pool size to required minimum

gm_3a:
    cmp ovl_mem_alloc,0     ; see if pool size if free memory or memory to allocate
    je  gm_minfeed          ; free memory

; check that largest overlay doesn't exceed specified pool size
    cmp WORD PTR ovl_pool+2,0   ; see if overlay pool MSW is nonzero (pool >=64K)
    jne gm_minfeed          ; yes, overlay pool size is okay
    cmp bx,WORD PTR ovl_pool+1  ; see largest overlay if exceeds overlay pool
    jbe gm_minfeed          ; no

; ajust the maximum overlay pool size, give warning feedback
    mov WORD PTR ovl_pool+1,bx
    mov bx,OFFSET DGROUP:max_text
    mov di,ax               ; save minimum allocation
    mov cl,[bx-1]           ; get length of string to print
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map
    mov ax,di               ; restore minimum allocation to ax

; give minimum overlay allocation feedback, K amount in ax
gm_minfeed:
    mov bx,OFFSET DGROUP:min_text
    call    give_min_feedback

; print largest overlay code segment name in parens after allocation
    mov cx,1
    mov dx,OFFSET DGROUP:left_paren ; print left parenthesis
    mov ah,40h
    int 21h
    call    restore_ems_map

    lds bx,large_ovl_name   ; ds:bx -> largest overlay code segment name
    mov dx,bx               ; save pointer to beginning of string
    xor cx,cx               ; init count of chars in string

name_loop:
    mov al,[bx]             ; get char
    or  al,al               ; see if at end
    je  print_name          ; yes
    inc cx                  ; bump count of chars in name
    inc bx                  ; bump to next char
    jmp SHORT name_loop

print_name:
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map

    mov ax,DGROUP           ; restore ds -> warplink data
    mov ds,ax

    mov cx,3
    mov dx,OFFSET DGROUP:right_paren    ; print right parenthesis and cr/lf
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map

    mov al,is_reload
    or  al,is_clarion       ; see if reload flag or Clarion flag tripped
    je  gm_ret              ; no

; give required overlay allocation (2 x largest - 1 + 2nd largest)
    mov bx,OFFSET DGROUP:rec_text

    cmp is_clarion,0        ; see if Clarion switch thrown
    je  gm_4                ; no
    mov bx,OFFSET DGROUP:clar_text  ; use clarion-specific feedback

gm_4:
    mov ax,di               ; get largest overlay size
    shl ax,1                ; x2
    dec ax                  ; x2-1
    mov cx,ax
    mov ax,second_ovl       ; add in second largest in paragraphs
    add ax,1023             ; round up to next 1K boundary
    and ax,0fc00h           ; put on 1K boundary
    mov al,ah
    xor ah,ah               ; effective divide by 256
    shr ax,1                ; /512
    shr ax,1                ; /1024, ax holds 1K pages of overlay
    add ax,cx               ; x2-1 + 2nd
    cmp is_min_pool,0       ; see if automatically use minimum pool size
    je  gm_5                ; no

    mov cx,ax               ; get pool size in 1K blocks
    shl cx,1
    shl cx,1                ; pool size in 256 byte pages
    mov WORD PTR ovl_pool+1,cx  ; set the pool size to required minimum

gm_5:
    call    give_min_feedback

; print trailing CR/LF
    mov dx,OFFSET DGROUP:min_text
    mov cx,2
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map

gm_ret:
    ret
get_min_ovl_alloc   ENDP

;*****************************
;* GIVE_MIN_FEEDBACK         *
;*****************************
 
; give minimum overlay allocation feedback
; upon entry bx -> text string to print, ax holds value
; return initial ax value in di, STDOUT in bx
; destroys ax,bx,cx,dx,di

give_min_feedback   PROC
    mov di,ax               ; save minimum allocation
    mov cl,[bx-1]           ; get length of string to print
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map
    mov ax,di               ; restore minimum allocation to ax

    mov bx,OFFSET DGROUP:tbuff  ; bx -> buffer to hold ax value
    cmp ax,100              ; see if minimum required >=100
    jb  gmf_1               ; no
    mov BYTE PTR [bx],'1'
    sub ax,100

gmf_1:
    mov cl,10
    div cl                  ; al holds decimal byte, ah holds ones
    or  ax,3030h            ; convert to ASCII values
    cmp al,30h              ; see if leading zero
    jne gmf_2               ; no
    mov al,' '              ; use blank instead

gmf_2:
    mov [bx+1],ax           ; store value in buffer
    mov dx,bx               ; ds:dx -> string to print
    mov cx,5                ; # bytes to print
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map
    ret
give_min_feedback   ENDP

;*****************************
;* MAKE_MASTER_SEGBLK        *
;*****************************

; allocate a block to hold the master segdef entry pointers for each overlaid
; segment partition entry, as they are overwritten
; destroys ax,bx

make_master_segblk  PROC
    mov bx,ovl_count        ; get count of overlays
    add bx,7                ; round up
    shr bx,1                ; 2 bytes per entry, convert to paragraphs for allocation
    shr bx,1                ; /4
    shr bx,1                ; /8, bx holds paragraphs to allocate
    call    alloc_memory    ; allocate block
    mov master_segblk,ax    ; save address of block
    ret
make_master_segblk  ENDP

;*****************************
;* OVL_ENTRY_POINT           *
;*****************************

; make new entry point that goes to overlay manager initialization code

ovl_entry_point PROC
    mov di,OFFSET DGROUP:entry_text
    call    find_pubdecl_entry  ; find the symbol
    or  ax,ax               ; make sure nonzero
    je  oep_mgrerr          ; not found, assume overlay manager wasn't linked in

    call    get_symbol_segoff
    mov entry_offval,ax     ; save entry offset value
    mov entry_segval,dx     ; save entry segment value
    ret

; overlay manager not linked in, fatal error
oep_mgrerr:
    mov ax,MISSING_OVLMGR_ERR
    jmp NEAR PTR link_error

ovl_entry_point ENDP

;*****************************
;* FIXUP_OVL_EXTDEF          *
;*****************************

; check if extdef in fixup should have offset changed to vector
; if so, change the offset
; upon entry es -> public declaration entry
; returns al nonzero if offset changed, zero if not and should continue with fixup process
; destroys ax,bx

fixup_ovl_extdef    PROC
    push    cx              ; save critical register
    push    dx
    push    di
    push    si
    push    es
    push    bp

    cmp frame_method,1      ; see if group fixup
    jne foe_notgroup        ; no
    test    BYTE PTR es:[15],1  ; see if overlaid public
    je  foe_notgroup        ; no

; group frame with overlaid extdef, set is_ind_call flag so that the
; group computation is bypassed and zero the target segment
    xor ax,ax
    mov target_segment,ax   ; zero target segment
    inc al
    mov is_ind_call,al      ; set indirect call flag so frame computation bypassed

foe_notgroup:
    mov al,loc              ; get loc field
    cmp al,1                ; see if offset
    je  foe_1               ; yes
    cmp al,5                ; see if alternate offset
    je  foe_1
    cmp al,3                ; see if pointer (seg:off)
    je  foe_1               ; yes
    cmp al,2                ; see if base fixup
    jne to_foe_retzero      ; no

; base fixup of overlay class public with current segment overlaid
; or base fixup of overlaid public

; remove use of far vectors within segment
    xor bh,bh               ; flag nonzero target disp warning if needed
    call    fixup_ind_ref_extdef
    jmp NEAR PTR foe_retone

to_foe_retzero:
    jmp NEAR PTR foe_retzero    ; no, don't process other types of loc fields

foe_1:
    mov di,es               ; save -> public declaration entry in di

; vector offset fixups if current segment
; matches target and far (matching base) and bracketed overlays, otherwise
; if current matches target, no vectoring

foe_2:

; give warning on nonzero target displacement
    mov al,1
    mov no_fixbyte_flag,al  ; set the flag to indicate fixup bytes at location should be zero
    mov ax,WORD PTR target_disp
    or  ax,WORD PTR target_disp+2
    je  foe_2a              ; zero target displacement

; nonzero target displacment, give warning
    push    es              ; save critical registers
    push    di
    les di,es:[4]           ; es:di -> symbol name
    mov dx,OFFSET DGROUP:filename   ; ds:dx -> filename with problem
    mov ax,NONOVL_SYM_WARN  ; nonoverlayable symbol warning
    call    link_warning
    pop di                  ; restore critical registers
    pop es

foe_2a:
    mov bx,WORD PTR ovl_pub_seg_ptr
    mov ax,WORD PTR ovl_pub_seg_ptr+2
    mov es,ax               ; es:bx -> total overlaid public references entry in overlaid public block
    or  ax,bx               ; see if zeroed (no table reference)
    je  to_foe_retzero      ; zero, no table entries, no vectoring necessary

; remember bx is offset by 2 from start of overlaid public entry
foe_2b:
    mov cx,es:[bx]          ; cx holds total overlaid public references

    sub cx,es:[bx+2]        ; subtract off far ref., cx holds NEAR public references

    mov si,es:[bx+4]        ; get previous total vector bytes count (if non-overlaid segment)
    xor dx,dx               ; dx holds current count
    xor bp,bp               ; bp flags whether entry found as zero
    add bx,6                ; bump to first overlaid public byte
    jmp SHORT foe_boundchk  ; see if at end of block

foe_seekloop:
    mov al,es:[bx]          ; get overlaid public flag byte
    test    al,3            ; see if segment info (overshot on this entry)
    jne foe_new_entry       ; segment info, multiple segment entries
    and al,0c0h             ; get far and near bits
    jne foe_4               ; nonzero

; check if entry matches zero'ed out entry, in case of bracketed intrasegment near offset
    cmp di,es:[bx+1]        ; see if public declaration entry segment matches current
    jne foe_next            ; no
    mov bp,1                ; yes, flag it

foe_next:
    add bx,3                ; bump to next public entry

foe_boundchk:
    cmp bx,OVLPUB_BLK_BYSIZE-3  ; see if at end of block
    jb  foe_seekloop        ; no

; new block
    mov ax,es:[0]           ; get pointer to next block in ax
    or  ax,ax               ; make sure non-null
    je  foe_internal        ; null, internal error
    mov es,ax               ; es -> next block
    mov bx,2                ; init block position
    jmp SHORT foe_seekloop  ; try new block

foe_internal:
    or  bp,bp               ; see if entry found as zero (not an error)
    jne to_foe_retzero      ; yes, don't vector

    mov ax,INTERNAL_ERR     ; put WarpLink error code in ax
    mov cx,14               ; internal error value

foe_toerr:
    mov si,fixup_pos        ; si -> offending fixupp record
    jmp NEAR PTR link_error ; transfer control to error handler

; this may be recoverable in the future, but for now a fatal error
foe_new_entry:
    or  bp,bp               ; see if entry found as zero (not an error)
    jne to_foe_retzero2     ; yes, don't vector

; multiple segment entries, noncontiguous overlay class segment code, fatal error
; because the vectors will be screwed up
    mov ax,NONCONT_OVLDATA_ERR  ; put WarpLink error code in ax
    mov dx,OFFSET DGROUP:filename
    jmp SHORT foe_toerr

foe_4:
    cmp di,es:[bx+1]        ; see if public declaration entry segment matches current
    je  foe_match           ; yes

foe_5:
    and al,40h              ; see if near bit set
    je  foe_next            ; no
    inc dx                  ; yes, bump current overlaid public count
    jmp SHORT foe_next      ; loop for next entry

; far reference, instead of vector within segment, make vector to indirect call table
foe_far:

; vector pointer fixups if current segment matches target
; and bracketed overlays, if parens then no vectoring
    cmp loc,3               ; see if pointer fixup
    jne foe_farvect         ; no
    cmp nonovl_rvect,0      ; see if nonvector root call flag is set
    je  foe_farvect         ; no, vector code

    mov es,es:[0]           ; es -> segment partition entry owning public
    cmp ovl_code_id,0       ; see if current segment is overlaid
    je  foe_farvect         ; no, must vector
    mov ax,es:[4]           ; get overlay identifier of owning segment
    cmp ax,ovl_code_id      ; see if matches current segment's
    jne foe_farvect         ; no, must vector

; current segment matches the target segment for this extdef
; and nonbracketed overlay, don't vector it
to_foe_retzero2:
    jmp NEAR PTR foe_retzero    ; yes, no vectoring needed

foe_farvect:
    mov es,di               ; restore es -> public declaration
    mov bh,1                ; flag no nonzero target disp warning (already given if needed)
    call    fixup_ind_ref_extdef
    jmp NEAR PTR foe_retone

; public declaration entry segment matches current public declaration in fixup
foe_match:

; Check if al holds far reference, loc==3 (pointer).  If not, (loc==1 or 5, offset),
; then check if extdef segment if overlaid.  If not, walk the extdef's segdef
; segment partition list and see if matches the L?DATA segment partition entry.
; If not (not in same segment), assume not a near call, and don't vector.
    mov es,di               ; restore es -> public declaration
    test    BYTE PTR locat,40h  ; see if M bit set
    je  foe_near            ; no, self-relative assume this is NEAR call, ignore far bit setting
    and al,80h              ; see if far bit set
    jne foe_far             ; yes

; near bit set, check if current segment is overlaid segment
; if so, then don't vector EVEN if bracketed
foe_near:
    mov es,es:[0]           ; es -> segment partition entry owning public
    cmp ovl_code_id,0       ; see if current segment is overlaid
    je  foe_nearvchk        ; no, continue vector check
    mov ax,es:[4]           ; get overlay identifier of owning segment
    cmp ax,ovl_code_id      ; see if matches current segment's
    je  to_foe_retzero2     ; yes, no vectoring required

; should never be a pointer with near bit set, but this makes sure
foe_nearvchk:
    cmp loc,3               ; see if pointer
    je  foe_6               ; yes

    mov es,di               ; es -> public declaration entry
    test    BYTE PTR es:[15],1  ; see if overlaid public
    jne foe_6               ; yes
    mov es,es:[0]           ; es -> segment partition entry
    mov es,es:[4]           ; es -> master segdef entry
    mov ax,data_seg_part    ; ax holds L?DATA segment partition entry to match
    mov es,es:[22]          ; es -> first segment partition entry
    or  gen_flags,1         ; initially set bitflag for no vector address change

foe_nearloop:
    mov bx,es
    or  bx,bx               ; see if any more entries to match
    je  foe_retzero         ; no, not found, assume NOT a near call to be vectored
    cmp ax,bx               ; see if found a match for segment partition entry
    je  foe_5a              ; yes, assume a near call, continue with vectoring
    mov es,es:[2]           ; es -> next segment partition entry, if any
    jmp SHORT foe_nearloop  ; keep looking

foe_5a:
    and gen_flags,0feh      ; reset no vector address change in frame bitflag

foe_6:
    mov es,data_seg_part    ; es -> L?DATA segment partition entry
    test    BYTE PTR es:[15],80h    ; see if segment is overlaid
    je  foe_7               ; no
    sub cx,dx               ; get overlaid public adjustment to make to length
    mov ax,7
    mul cx                  ; compute proper adjustment for vector in ax
    mov bx,es:[12]          ; get segment length in bx
    sub bx,ax               ; bx holds offset to proper vector

foe_update:
    mov WORD PTR target_prog_off,bx ; update target_prog_off variable with offset
    mov WORD PTR target_prog_off+2,0    ; zero high word of offset
    jmp SHORT foe_retone    ; bypass nonoverlaid segment code

; root segment calling overlay vector.
; SEGMENT RELATIVE (M bit set):
; get segment offset of symbol in target_prog_off
; SELF RELATIVE (M bit reset):
; add data_offset to segment offset of symbol to get its absolute address,
; BUT data_offset also has partition offset, so subtract that off
foe_7:
    mov cx,es:[0]           ; get partition offset value, for self relative fixup
    mov es,es:[4]           ; es -> master segdef entry
    mov bx,es:[6]           ; bx holds segment length
    sub bx,es:[30]          ; subtract off all end vector bytes
    add bx,si               ; adjust past previous vector bytes
    mov ax,7
    mul dx                  ; compute proper adjustment for vector in ax
    add bx,ax               ; bx holds offset to proper vector within segment

    test    BYTE PTR locat,40h  ; see if M bit set
    jne foe_update          ; yes, segment relative, bx hold proper value to put in target_prog_off

    sub bx,cx               ; subtract off partition offset
    add bx,WORD PTR data_offset ; compute low word of vector offset
    mov WORD PTR target_prog_off,bx ; save it
    mov WORD PTR target_prog_off+2,0    ; zero high word of vector offset
    mov ax,WORD PTR data_offset+2   ; get high word of offset
    adc WORD PTR target_prog_off+2,ax   ; compute high word of vector offset, with carry
    mov is_ind_call,1       ; set indirect call flag so frame computation bypassed

foe_retone:
    mov al,1                ; return al nonzero to indicate processing of offset done
    jmp SHORT foe_ret

foe_retzero:
    xor al,al               ; return al equal to zero to indicate further processing

foe_ret:
    pop bp
    pop es                  ; restore critical register
    pop si
    pop di
    pop dx
    pop cx
    ret
fixup_ovl_extdef    ENDP

;*****************************
;* FIXUP_IND_REF_EXTDEF      *
;*****************************

; overlaid public indirectly far referenced by non-overlay class segment
; change the fixup to a vector
; upon entry es -> public declaration entry, bh == nonzero if no nonzero
; target displacement warning given (coming from fixup_ovl_extdef routine)
; destroys ax,bx

fixup_ind_ref_extdef    PROC
    push    cx              ; save critical register
    push    dx
    push    es

; give warning on nonzero target displacement
    mov al,1
    mov no_fixbyte_flag,al  ; set the flag to indicate fixup bytes at location should be zero
    mov ax,WORD PTR target_disp
    or  ax,WORD PTR target_disp+2
    je  fir_zerodisp        ; zero target displacement
    or  bh,bh               ; see if should give warning
    jne fir_zerodisp        ; no

; nonzero target displacment, give warning
    push    es              ; save critical registers
    push    di
    les di,es:[4]           ; es:di -> symbol name
    mov dx,OFFSET DGROUP:filename   ; ds:dx -> filename with problem
    mov ax,NONOVL_SYM_WARN  ; nonoverlayable symbol warning
    call    link_warning
    pop di                  ; restore critical registers
    pop es

fir_zerodisp:
    mov al,1
    mov is_ind_call,al      ; set indirect call flag

    cmp loc,3               ; see if pointer fixup
    je  fir_reloc           ; yes
    cmp loc,2               ; see if base fixup
    jne fir_1               ; no, bypass setting relocatable flag

fir_reloc:
    mov is_relocatable,al   ; set is relocatable flag

fir_1:
    mov ax,ind_segment
    mov target_segment,ax   ; set target segment to indirect call table segment
    mov dx,es               ; save -> public declaration entry in dx
    mov ax,ind_tbl_segdef
    mov es,ax               ; es -> segdef entry for indirect call table
    mov bx,es:[2]           ; get low word of offset
    mov cx,es:[4]           ; get high word of offset
    mov es,dx               ; es -> public declaration entry
    mov ax,es:[2]           ; get overlaid public number
    dec ax                  ; make relative zero
    mov dx,ax
    shl ax,1                ; x2
    add ax,dx               ; x3
    add ax,10h              ; adjust for vector paragraph at start of indirect call segment

    add ax,bx               ; compute target_prog_off value
    adc cx,0                ; carry to high word

    mov WORD PTR target_prog_off,ax ; update low word
    mov WORD PTR target_prog_off+2,cx   ; update high word

    pop es                  ; restore critical register
    pop dx
    pop cx
    ret
fixup_ind_ref_extdef    ENDP

;*****************************
;* FIXUP_SEG_REF             *
;*****************************

; overlaid segment far referenced via segment fixup (assume target displacement of zero)
; OR segment fixup of overlaid segment that is current segment
; change the fixup to a vector
; upon entry es -> segment partition entry
; destroys ax,bx

fixup_seg_ref   PROC
    push    cx              ; save critical register
    push    dx
    push    es

    mov al,1
    mov is_ind_call,al      ; set overlaid segment fixup/indirect call flag
    mov is_relocatable,al   ; set is relocatable flag (always relocatable if pointer)
    mov ax,es               ; get segment partition entry of segment fixup
    cmp ax,data_seg_part    ; see if matches L?DATA segment partition entry
    jne fsr_2               ; no

; segment fixup within current segment, zero the segment, make the offset
; the target displacement
    xor ax,ax
    mov target_segment,ax   ; zero target segment

    mov ax,WORD PTR target_disp      ; target displacement goes in targ_prog_off
    mov cx,WORD PTR target_disp+2
    test    BYTE PTR locat,40h  ; see if fixup self or seg-relative
    jne to_fsr_ret          ; segment relative

; self-relative segment fixup within current segment
; subtract data_rec_offset and data_offset from target displacement, 
; subtract 1 for low-byte, 2 for offset

    mov is_relocatable,0    ; zero the relocatable flag (not a segment fixup)
    inc is_ind_call         ; bump flag to 2 to indicate self-relative modification
    sub ax,WORD PTR data_offset
    sbb cx,WORD PTR data_offset+2   ; borrow to high word
    sub ax,data_rec_offset  ; make target offset relative to location
    sbb cx,0                ; borrow to high word
    sub ax,1                ; adjustment alway at least 1 byte
    sbb cx,0
    cmp loc,0               ; see if low-byte loc
    je  fsr_ret             ; yes, proper adjustment made
    sub ax,1                ; subtract one more for offset loc
    sbb cx,0

to_fsr_ret:
    jmp SHORT fsr_ret

fsr_2:
    mov al,1
    mov no_fixbyte_flag,al  ; set the flag to indicate fixup bytes at location should be zero
    mov ax,WORD PTR target_disp
    or  ax,WORD PTR target_disp+2
    je  fsr_3               ; zero target displacement

; nonzero target displacment, give warning
; es -> segment partition entry
    push    es              ; save critical registers
    push    di

    mov di,es:[4]           ; get overlay identifer
    dec di                  ; make relative zero
    shl di,1                ; convert to work offset
    mov es,master_segblk
    mov es,es:[di]          ; es -> master segdef entry
    les di,es:[8]
    add di,8                ; es:di -> segment name

    mov dx,OFFSET DGROUP:filename   ; ds:dx -> filename with problem
    mov ax,NONOVL_SEG_WARN  ; nonoverlayable segment warning
    call    link_warning
    pop di                  ; restore critical registers
    pop es

fsr_3:
    mov ax,segcall_segment
    mov target_segment,ax   ; set target segment to segment call table segment

    mov dx,es               ; save -> public declaration entry in dx
    mov ax,segcall_tbl_segdef
    mov es,ax               ; es -> segdef entry for segment call table
    mov bx,es:[2]           ; get low word of offset
    mov cx,es:[4]           ; get high word of offset
    mov es,dx               ; es -> segment partition entry

    mov ax,es:[4]           ; get overlay identifier
    dec ax                  ; make relative zero
    mov dx,ax
    shl ax,1                ; x2
    add ax,dx               ; x3
    add ax,10h              ; adjust for vector paragraph at start of segment call segment
    add ax,bx               ; compute target_prog_off value
    adc cx,0                ; carry to high word

fsr_ret:
    mov WORD PTR target_prog_off,ax ; update low word
    mov WORD PTR target_prog_off+2,cx   ; update high word

    pop es                  ; restore critical register
    pop dx
    pop cx
    ret
fixup_seg_ref   ENDP

END
