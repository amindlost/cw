;*********************************************************************
;*   MLOVL1.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          03/12/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker overlay pass 1 routines                                  *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlovl1
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
PUBLIC  chk_seg_ovl_class,chk_pub_in_ovl
PUBLIC  proc1_datarec,proc1_fixupp
PUBLIC  flush_publics

; variables
PUBLIC  curseg_nearovlpub_count,curseg_farovlpub_count,ovlpub_array_seg
PUBLIC  ovlpubblk_pos,clarclass_text
PUBLIC	lxdata

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   name_field:BYTE,filename:BYTE
EXTRN   class_name_index:WORD
EXTRN   loc:BYTE,target_method:BYTE,target_index:WORD
EXTRN   ovl_entry_id:WORD,data_rec_offset:WORD,rec_offset:WORD
EXTRN	current_segind:WORD
EXTRN	ignore_fixupp_flag:BYTE

; initialized local variables

; word values
ovlpubblk_pos   DW  0       ; start position of unused area in overlay public block
off_extdef_number   DW  0   ; extdef index of last offset fixup save in overlaid public block
seg_extdef_number   DW  0   ; extdef index of last base fixup of overlaid public

.DATA?

; uninitialized local variables

; byte values
data_seg_overlaid   DB  ?   ; nonzero if overlay class segment partition is overlaid, zero otherwise
EVEN
off_to_far_flag DB  ?       ; nonzero if offset fixup is for far reference
EVEN
first_fixup_flag    DB  ?   ; nonzero if first fixup of L?DATA
lxdata		DB	?			; holds LEDATA or LIDATA value
EVEN

; word values
ovl_data_seg    DW  ?       ; segment partition segment of last L?DATA record if overlay class, zero otherwise
curseg_nearovlpub_count DW  ?   ; overlaid near public count of current segment
curseg_farovlpub_count  DW  ?   ; overlaid far public count of current segment
ovlpub_array_seg    DW  ?   ; segment partition entry of current overlaid publics in array

segpart_owner   DW  ?       ; segment partition owner of last L?DATA record
off_extdef_ptr  DW  ?       ; pointer in overlaid public block to last offset fixup (near) stored
off_extdef_segpart  DW  ?   ; segment partition entry of last offset fixup saved to overlaid public block
seg_extdef_segpart  DW  ?   ; segment partition entry of last base fixup of overlaid public

first_curr_offnum   DW  ?   ; extdef index of first offset fixup of overlaid public in current FIXUPP record
first_curr_offptr   DW  ?   ; pointer to first offset fixup of overlaid public in current FIXUPP record
first_curr_segnum   DW  ?   ; extdef index of first base fixup of overlaid public in current FIXUPP record
first_prev_offnum   DW  ?   ; extdef index of first offset fixup of overlaid public in previous FIXUPP record
first_prev_offptr   DW  ?   ; pointer to first offset fixup of overlaid public in previous FIXUPP record
first_prev_segnum   DW  ?   ; extdef index of first base fixup of overlaid public in previous FIXUPP record

;*****************************
;* Constant data             *
;*****************************

.CONST

EVEN
clarclass_text  DB  'CLARION',0

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR,link_warning:NEAR
EXTRN   alloc_memory:NEAR,error_bx_pos:NEAR
EXTRN   caseless_strcmp:NEAR,save_thread:NEAR
EXTRN	parse_clipper_symbols:NEAR,clip_sym_fixup:NEAR

IFNDEF DEMO
EXTRN   ddl_datarec:NEAR,ddl_fixupp:NEAR
ENDIF

;*****************************
;* CHK_SEG_OVL_CLASS         *
;*****************************

; check new segdef entry class to see if overlay class
; set or reset segdef entry's overlay class flag accordingly
; di -> segdef entry upon entry, name_field holds segment name
; destroys ax,es

chk_seg_ovl_class   PROC
    mov al,is_anyovls

IFNDEF DEMO
    or  al,any_ddl          ; see if any overlays specified, or using DLL's
ELSE
	or	al,al
ENDIF

    jne seg_1               ; yes
    ret                     ; no, just return

seg_1:
    push    bx              ; save critical registers
    push    di
    mov es,di               ; es -> segdef entry
    push    es              ; save -> segdef entry

; get class name from previous lnames record
    mov ax,class_name_index ; get class name index
    dec ax                  ; make count relative zero
    shl ax,1
    shl ax,1                ; make class name index a doubleword offset into index pointer array
    mov di,offset DGROUP:lnames_ent_indptr  ; set di to base of lnames index pointer array
    add di,ax               ; di -> proper array element
    mov bx,[di]             ; get offset of class name from lnames entry
    mov es,[di+2]           ; get segment of class name from lnames entry

    cmp is_clarion,0        ; see if clarion flag set
    je  seg_notclar         ; no
    call    cla_ovl_class_check ; check if clarion class and code segment
    jmp SHORT seg_2         ; bypass normal class name check

seg_notclar:
    call    ovl_class_check ; compare class name against overlay class

seg_2:
    pop es                  ; restore es -> segdef entry
    mov es:[28],al          ; re/set the overlay class flag
;***    or  al,al               ; see if overlay class
;***    je  seg_ret             ; no

seg_ret:
    pop di                  ; restore critical registers
    pop bx
    ret
chk_seg_ovl_class   ENDP

;*****************************
;* CHK_PUB_IN_OVL            *
;*****************************

; check public declaration's segment partition entry to see if in overlay
; set public declaration entry's in overlay flag accordingly
; upon entry es -> pubdef entry
; destroys ax

chk_pub_in_ovl  PROC
    cmp is_anyovls,0        ; see if any overlays specified
    jne pub_1               ; yes
    ret                     ; no, just return

pub_1:
    push    es              ; save es -> pubdef entry
    mov es,es:[0]           ; es -> segment partition entry
    mov al,es:[15]          ; get overlay flag of segment partition entry
    and al,80h              ; save only overlay flag bit
    rol al,1                ; get high bit value in low bit

; check if overlay flag set, if so bypass checks in case of pass 2 usage, so
; invalid master segdef entry value pointer doesn't screw up overlay class bit
    jne pub_ovl             ; overlay flag bit set, guaranteed overlay class

    mov es,es:[4]           ; es -> master segdef entry
    mov ah,es:[28]          ; get overlay class flag
    or  ah,ah               ; see if overlay class
    je  pub_2               ; no

pub_ovl:
    mov ah,8                ; overlay class, get proper bit value in ah

pub_2:
    pop es                  ; restore es -> pubdef entry
    or  es:[15],ah          ; re/set the overlay class bit
    or  es:[15],al          ; re/set the overlaid flag bit

pub_ret:
    ret
chk_pub_in_ovl  ENDP

;*****************************
;* OVL_CLASS_CHECK           *
;*****************************

; check the class name -> by es:bx (prior to doubleword adjustment)
; against the overlay class specified in the linker command line
; destroys ax,bx,di
; returns al==1 if overlay class, 0 if not overlay class

ovl_class_check PROC
    add bx,8                ; adjust bx past 2 doubleword pointers
    mov di,OFFSET DGROUP:ovl_class  ; ds:di -> overlay class name
    xor al,al
    cmp exact_ovl_class,al  ; see if exact match flag set
    jne chk_compare         ; yes, bypass suffix setup

; point to check string null terminator
chk_loop1:
    cmp es:[bx],al
    je  chk_2               ; at null terminator
    inc bx
    jmp SHORT chk_loop1     ; no at null terminator yet, keep looking

; step the check string back one char for every char in target string
; this will allow successful matches for a target string suffix
chk_2:
    cmp [di],al             ; see if at null terminator
    je  chk_3               ; yes, check string now pointing at possible suffix
    inc di                  ; bump to next char
    dec bx                  ; back up one char of check string
    jmp SHORT chk_2         ; loop back

chk_3:
    mov di,OFFSET DGROUP:ovl_class  ; restore di -> overlay class name

; perform a caseless string compare on the names
chk_compare:
    call    caseless_strcmp ; al return 0 if match, 1 if no match
    xor al,1                ; flip the match bit
    ret
ovl_class_check ENDP

;*****************************
;* CLA_OVL_CLASS_CHECK       *
;*****************************

; check if Clarion overlay class, class 'CLARION', segment prefix is '_CODE'
; upon entry es:bx -> class name (prior to doubleword adjustment)
; destroys ax,bx,di
; returns al==1 if overlay class, 0 if not overlay class

cla_ovl_class_check PROC
	xor	al,al
    cmp	any_ddl,al			; see if using DDL's
	je	coc_2				; no
    cmp obj_ovl_flag,al		; see if in overlay
	je	coc_2				; no, do regular check
	mov	al,1
	ret						; return al==1 as overlay class

; cheesy, but easy and quick, check for '_CODE' prefix on segment name
coc_2:
    mov di,OFFSET DGROUP:name_field ; ds:di -> segment name
    cmp BYTE PTR [di],'_'
    jne cla_nomatch
    cmp BYTE PTR [di+1],'C'
    jne cla_nomatch
    cmp BYTE PTR [di+2],'O'
    jne cla_nomatch
    cmp BYTE PTR [di+3],'D'
    jne cla_nomatch
    cmp BYTE PTR [di+4],'E'
    jne cla_nomatch

    add bx,8                ; adjust bx past 2 doubleword pointers
    mov di,OFFSET DGROUP:clarclass_text ; ds:di -> overlay class name

; perform a caseless string compare on the names
    call    caseless_strcmp ; al return 0 if match, 1 if no match
    xor al,1                ; flip the match bit
    ret

cla_nomatch:
    xor al,al               ; flag no match
    ret

cla_ovl_class_check ENDP

;*****************************
;* PROC1_DATAREC             *
;*****************************

; pass 1 L?DATA record processing for overlays
; upon entry cx=record length,bp=buffer_end
; si -> first byte of record past record length word
; set ovl_data_seg to partition segment if overlay class, reset otherwise
; if overlay class, set data_seg_overlaid if segment is overlaid, reset otherwise
; destroys ax,bx,dx,di,es
; updates si,cx

proc1_datarec   PROC
	mov	ignore_fixupp_flag,0	; flag not to ignore following fixup
IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDL's
    je  data_noddl          ; no
    jmp NEAR PTR ddl_datarec    ; transfer to DDL processing code
ENDIF

data_noddl:
	mov	lxdata,al			; save LEDATA or LIDATA type
    mov bx,si               ; keep offset pointer to index in case of error
    lods    BYTE PTR es:0   ; get first byte of segment index
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  data_1              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

data_1:
    cmp al,80h              ; check if high bit set
    jb  data_3              ; no, only 1 byte index

; two byte index, dh contains high-order byte with high bit set
    je  data_2              ; if greater than 0x80 then segment index > 255, out of bounds

data_bad_segind:
    mov cl,al               ; get value in cl
    mov dx,OFFSET DGROUP:filename
    mov ax,SEGDEF_VAL_ERR   ; segment index greater than 255 maximum, or higher than count of segdef records
    jmp NEAR PTR error_bx_pos   ; transfer control to error handler

data_2:
    lods    BYTE PTR es:0   ; get second byte of segment index
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  data_3              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

; al contains segment index value
data_3:
    dec al                  ; make segment index value relative zero
    xor ah,ah               ; zap high byte
	mov	current_segind,ax	; save to memory variable for Clipper compression use

	call	parse_clipper_symbols	; parse for clipper symbol table compression, if indicated

    mov dx,ax               ; save segment index in dx (dl)
    mov bx,offset DGROUP:seg_defent_indptr  ; set bx to base of segment index pointer array
    shl ax,1                ; make segment index value a word offset into index pointer array
    add bx,ax               ; bx -> proper segment index array element
    mov es,[bx]             ; es -> segdef entry

    cmp is_clarion,0        ; see if clarion switch set
    je  data_chkovl         ; no
    cmp obj_ovl_flag,0      ; see if in overlay
    je  data_chkovl         ; no

    call    proc1_clar_data ; process possible overlaid Clarion LEDATA

data_chkovl:
    mov al,es:[28]          ; get overlay flag value of segment
    or  al,al               ; see if set
    jne data_4              ; yes, this is an overlay class segment

; not overlay class
data_notovl:
    mov ax,es:[24]
    mov segpart_owner,ax    ; save segment partition entry owner
    xor ax,ax
    mov dl,al               ; reset overlaid data segment flag
    jmp SHORT data_setseg   ; zero the overlay data segment variable

; get last segment partition in segdef, this will be the owner of the overlaid code
data_4:
    mov ax,es:[24]          ; get last segment partition entry
    mov segpart_owner,ax    ; save segment partition entry owner
    mov es,ax               ; es -> segment partition entry
    mov dl,es:[15]
    and dl,80h              ; get overlaid status flag

data_setseg:
    mov data_seg_overlaid,dl    ; re/set the overlaid data segment flag
    mov ovl_data_seg,ax     ; set the overlay data segment to segment partition entry, zero if not overlay class

; This crap is necessary because Clipper 5.0 writes fixups in descending
; instead of ascending order and then will split up a base:offset reference
; to a single EXTDEF across multiple FIXUPPs.  Keeping track of the first
; reference in the previous record and then matching on it against offsets
; will catch this condition.
    mov ax,first_curr_offnum    ; transfer old first current to first previous variables
    mov first_prev_offnum,ax
    mov ax,first_curr_segnum
    mov first_prev_segnum,ax
    mov ax,first_curr_offptr
    mov first_prev_offptr,ax

    xor ax,ax               ; re-init fixup flags and variables
    mov first_curr_offnum,ax
    mov first_curr_segnum,ax
    mov first_curr_offptr,ax

; 10/20/92
;    mov seg_extdef_number,ax
;    mov off_extdef_number,ax

    inc al
    mov first_fixup_flag,al
    ret
proc1_datarec   ENDP

;*****************************
;* PROC1_CLAR_DATA           *
;*****************************

; process possible Clarion overlaid data segment
; upon entry cx=record length,bp=buffer_end
; es-> segdef entry owner of L?DATA record
; si -> enumerated offset of L?DATA record, within i/o buffer
; dl holds segment index, relative zero
; destroys ax,bx,dx,di
; updates si,cx

proc1_clar_data PROC
    push    es              ; save critical register
    mov es,buffer_base      ; es:si -> enumerated offset of L?DATA record

; get the enumerated data offset, if not zero, ignore
    lods    BYTE PTR es:0   ; get low byte of data offset
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  data_high           ; okay
    call    load_file       ; load next portion of file into buffer, at end position
data_high:
    mov ah,es:[si]          ; get high byte of data offset
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  data_nowrap         ; okay
    call    load_file       ; load next portion of file into buffer, at end position
data_nowrap:
    pop es                  ; restore es -> segdef entry
    or  ax,ax               ; see if nonzero data offset (not first LEDATA)
    jne pcd_ret             ; nonzero, ignore

; in overlay and clarion switch set, determine if Clarion _DT or _DAT segment
; (class 'CLARION', private segment, name prefixed with '_DT' or '_DAT')
; if so, track the segment size, zero the length entry
    mov al,es:[26]          ; get ACBP byte
    and al,1ch              ; get combine value
    jne pcd_ret             ; not a private segment

    push    es              ; save es -> segdef entry
    les bx,es:[12]          ; es:bx -> class name
    add bx,8                ; adjust bx past 2 doubleword pointers
    mov di,OFFSET DGROUP:clarclass_text ; ds:di -> overlay class name

; perform a caseless string compare on the names
    call    caseless_strcmp ; al return 0 if match, 1 if no match
    pop es                  ; restore es -> segdef entry
    or  al,al               ; see if match
    je  pcd_isclar          ; yes

pcd_ret:
    ret

pcd_isclar:

    mov ax,dx               ; segment index relative zero in ax (al holds value, ah==0)
    mov dl,3
    div dl                  ; get multiple clarion segment grouping (_dt, _dat, _code) count
    mov dl,al               ; save clarion segment grouping count

    mov ax,ovl_entry_id     ; get entry overlaid code id
    add ax,dx               ; adjust for multiple clarion segment grouping count, if any

; since no adjustment for current entry from ovl_entry_id, relative zero
; adjustment has effectively been made
    shl ax,1                ; x2
    shl ax,1                ; x4
    mov ovl_data_id,ax      ; save as current overlaid data identifier
    mov ax,es:[6]           ; get segment size
    mov di,ds               ; save critical register
    lds bx,es:[8]
    add bx,8                ; ds:bx -> segment name
    cmp BYTE PTR [bx],'_'
    jne pcd_nomatch         ; no match against clarion data segment name
    cmp BYTE PTR [bx+1],'D'
    jne pcd_nomatch
    cmp BYTE PTR [bx+2],'T' ; see if _DT segment
    je  pcd_dt_seg          ; yes
    cmp BYTE PTR [bx+2],'A' ; see if _DA[T] segment (assume 'T')
    jne pcd_nomatch         ; no, no match

    mov ds,di               ; restore ds -> WarpLink data
    mov _dat_seg_size,ax    ; save _DAT segment size
    add ovl_data_id,2       ; bump to next entry (two entries/module data)
    jmp SHORT pcd_2

pcd_nomatch:
    mov ds,di               ; restore ds -> WarpLink data
    ret

pcd_dt_seg:
    mov ds,di               ; restore ds -> WarpLink data
    mov _dt_seg_size,ax     ; save _DT segment size

; save segment length in overlay data block
pcd_2:
    push    es              ; save -> segdef entry
    push    cx              ; save critical registers
    push    dx
    mov dx,ovl_data_id      ; get data overlay identifier
    mov ax,data_ovlblk_ptr  ; get initial data overlay block
    mov es,ax               ; es -> block, if exists
    or  ax,ax               ; check if any blocks were previously allocated
    jne pcd_3               ; yes

; make initial data overlay block allocation
    mov bx,DATA_OVLBLK_SIZE ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov data_ovlblk_ptr,ax

pcd_zeroblk:
    mov es,ax               ; es -> data block
    xor ax,ax
    mov cx,DATA_OVLBLK_BYSIZE   ; size of block to zero in bytes
    shr cx,1                ; convert to words
    mov di,ax
    rep stosw               ; zero block

pcd_3:
    cmp dx,252              ; see entry is in this block (126 word entries)
    jb  pcd_inblk           ; yes
    sub dx,252              ; adjust past this block
    mov ax,es:[0]           ; get pointer to next block
    mov bx,es               ; save -> to old block in case no new
    mov es,ax
    or  ax,ax               ; see if block exists
    jne pcd_3               ; yes

; block doesn't exist
    mov es,bx               ; restore es -> old block
    mov bx,DATA_OVLBLK_SIZE ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov es:[0],ax           ; save pointer to new block in old block
    jmp SHORT pcd_zeroblk   ; zero it out and continue

pcd_inblk:
    test    ovl_data_id,2   ; see if _DT or _DAT segment
    je  pcd_4               ; _DT segment
    mov ax,_dat_seg_size    ; get _DAT segment size
    jmp SHORT pcd_5

pcd_4:
    mov ax,_dt_seg_size     ; get _DT segment size

pcd_5:
    mov bx,dx               ; es:bx -> overlaid data block entry to place segment size
    mov es:[bx+2],ax        ; save segment size, adjusting for system info word

    pop dx                  ; restore critical register
    pop cx
    pop es                  ; restore es -> segdef entry

; zero the segment length of the entry
    xor ax,ax
    mov es:[6],ax

    ret
proc1_clar_data ENDP

;*****************************
;* PROC1_FIXUPP              *
;*****************************

; pass 1 FIXUPP record processing for overlays
; upon entry cx=record length,bp=buffer_end
; si -> first byte of record past record length word
; destroys ax,bx,dx,di
; updates si,cx

proc1_fixupp    PROC
IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDL's
    je  fix_2               ; no
    jmp NEAR PTR ddl_fixupp ; transfer to DDL processing code
ENDIF

; track fixups of extdefs which are overlaid or unresolved
; if segment is overlaid, track count of segment fixups as well
fix_2:
    mov al,es:[si]          ; get first byte of either thread or fixup field
    and al,80h              ; check fixup field, high bit set
    jne fix_fixup           ; yes, fixup field

; thread field in fixupp record
    call    save_thread     ; save the thread field in case it is needed
    cmp cx,1                ; check if at checksum byte
    ja  fix_2               ; no, keep parsing fixupp record

pf_ret:
    ret                     ; yes, done

fix_fixup:
	cmp	ignore_fixupp_flag,0	; see if should ignore fixupp record
	jne	pf_ret				; yes
    lods    BYTE PTR es:0   ; get low byte of locat field
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  fix_3               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
fix_3:
    mov ah,es:[si]          ; get high byte of locat field
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  fix_4               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
fix_4:
    mov dl,al
    and al,1ch              ; get loc field
    shr al,1
    shr al,1                ; make loc value relative zero
    mov loc,al              ; save loc value

    mov BYTE PTR data_rec_offset,ah ; save bit 7-0 of data record offset in locat high byte
    and dl,3                ; get bit 9-8 of data record offset in locat low byte
    mov BYTE PTR data_rec_offset+1,dl   ; and save it

; check if need to do clipper symbol table fixup code
	call	clip_sym_fixup

    mov dl,es:[si]          ; get fix dat field
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  fix_5               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
fix_5:
    mov al,dl               ; get fix dat field
    test    al,80h          ; check if thread field for frame (fbit)
    jne fix_7               ; yes

; no thread field for frame
    and al,70h              ; get frame field in al
    cmp al,20h              ; see if index specified for this frame
    ja  fix_7               ; no

; index specified for this field
    lods    BYTE PTR es:0   ; get frame datum first byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  fix_6               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
fix_6:
    cmp al,80h              ; see if two byte field
    jb  fix_7               ; no
    inc si                  ; scan past frame datum second byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  fix_7               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

fix_7:
    mov al,dl               ; get fix dat field
    test    al,8            ; check if thread field for target (tbit)
    jne fix_thrdtarg        ; yes

; no thread field for target
    xor ah,ah               ; zap high byte
    lods    BYTE PTR es:0   ; get target datum first byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  fix_8               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
fix_8:
    cmp al,80h              ; see if two byte field
    jb  fix_9               ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte

    lods    BYTE PTR es:0   ; get target datum second byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  fix_9               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

fix_9:
    mov target_index,ax     ; target index, if any
    mov al,dl               ; get fix dat field
    and al,3                ; break out targt field
    mov target_method,al    ; target method is the targt field
    jmp SHORT fix_10        ; bypass target thread field code

; thread field for target
fix_thrdtarg:
    and al,3                ; get targt field value in al
    xor ah,ah               ; zap high byte
    mov bx,OFFSET DGROUP:target_thrd_meth   ; bx -> target thread method array base
    add bx,ax               ; bx -> proper byte array element
    mov bl,[bx]             ; get target method
    mov target_method,bl    ; save to memory variable

    shl ax,1                ; convert to word offset
    mov bx,OFFSET DGROUP:target_thrd_index  ; bx -> target thread index array base
    add bx,ax               ; bx -> proper word array element
    mov ax,[bx]             ; get target index
    mov target_index,ax     ; save to memory variable

fix_10:
    mov al,dl               ; get fix dat field
    and al,4                ; get P bit field
    jne fix_ovlchk          ; P bit set, no target displacement field

; P bit is zero, get target displacement field
    inc si                  ; scan past low byte of target displacement
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  fix_11              ; okay
    call    load_file       ; load next portion of file into buffer, at end position
fix_11:
    inc si                  ; scan past high byte of target displacement
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  fix_ovlchk			; okay
    call    load_file       ; load next portion of file into buffer, at end position

; check if the owner segment is overlaid
; if so and segment is overlaid, bump relocation count for partition entry
fix_ovlchk:
    cmp data_seg_overlaid,0
    je  fix_chk_targ        ; segment not overlaid, don't track segment fixups
    mov bx,es               ; save -> i/o buffer
    mov ax,ovl_data_seg
    mov es,ax               ; es -> segment partition entry
    cmp loc,2               ; see if segment (base) fixup
    je  fix_13              ; yes, increment count of relocation items
    cmp loc,3               ; see if pointer (segment:offset) fixup
    jne fix_14              ; no, not a segment fixup

fix_13:
    inc WORD PTR es:[0]     ; increment count of relocation items

; check for offset or seg:off fixups using an EXTDEF that is in an overlay
fix_14:
    mov es,bx               ; restore es -> i/o buffer

fix_chk_targ:
    cmp target_method,2     ; see if extdef target (target_method doesn't have bit 2)
    je  fix_14a             ; yes

to_next_fix3:
    jmp NEAR PTR next_fix   ; no, continue checking fixups

; fixup has EXTDEF target
fix_14a:
    mov al,loc              ; get fixup type
    or  al,al
    je  to_next_fix3        ; lobyte type, ignore

    xor al,al
    mov off_to_far_flag,al  ; initialize offset is far flag

; check if EXTDEF is either overlaid class or unknown
    mov ax,target_index     ; get target index (extdef index)
    dec ax                  ; make ax relative zero offset
    shl ax,1                ; convert ax to word offset
    mov di,ax               ; save in di
    mov dx,es               ; save -> i/o buffer
    mov bx,OFFSET DGROUP:ext_defent_indptr  ; set bx to base of pubdef declarations entry address array
    add bx,ax               ; bx -> array element holding pubdef declarations entry address
    mov bx,[bx]             ; bx -> pubdef declarations entry of referenced EXTDEF in FIXUPP record
;***    or  bx,bx               ; check if zero (local)
;***    je  nosave_ref          ; yes, don't vector locals
    mov es,bx               ; es -> pubdef declarations entry

    mov al,es:[15]
    mov ah,al
;***    and ah,4                ; see if local public
;***    jne nosave_ref          ; yes, don't vector locals

    or  BYTE PTR es:[14],40h    ; set used in fixup flag

    cmp first_fixup_flag,0  ; see if first fixup
    je  fix_15              ; no
    mov first_fixup_flag,0  ; reset first fixup flag
    mov bl,loc
    cmp bl,1                ; see if offset
    je  fix_locoff          ; yes
    cmp bl,5                ; see if alternate offset
    je  fix_locoff          ; yes

; first fixup is base or pointer
    cmp bl,3                ; see if pointer
    je  fix_15              ; ignore pointers, can't be split across fixupp records

; first fixup is base
    mov bx,target_index
    mov first_curr_segnum,bx    ; save index info
    jmp SHORT fix_15        ; bypass offset code

; first fixup is offset
fix_locoff:
    mov bx,target_index
    mov first_curr_offnum,bx    ; save index info

fix_15:
    mov bl,es:[14]          ; get definition flag
    and bl,3                ; mask off extraneous bits

; Check if non-overlay class segment and overlaid public or unknown public.
    cmp ovl_data_seg,0      ; see if nonoverlay class segment
    jne fix_seg_ovl_class   ; no
    and al,1                ; see if overlaid public
    jne non_ovl_save        ; yes, save this reference
    cmp bl,2                ; check if public or absolute
    jb  non_ovl_save        ; no, save unknown reference

; reference to known nonoverlaid public
nosave_ref:
    mov es,dx               ; restore es -> i/o buffer
    jmp NEAR PTR next_fix   ; check next fixup

; save this reference to an overlaid or unknown public in a non-overlay class segment
non_ovl_save:
    mov es,dx               ; restore es -> i/o buffer
    cmp loc,3               ; see if pointer fixup
    jne non_ovl_chkref      ; no
    jmp NEAR PTR fix_overlaid   ; yes, save reference

; reference is either base or offset (loc == 2, 1, or 5)
non_ovl_chkref:
    mov bx,segpart_owner    ; get info to save
    mov ax,target_index
    cmp loc,2               ; see if base fixup
    je  non_ovl_base        ; yes

; offset fixup of indirect reference
    mov off_extdef_segpart,bx   ; save segment partition info
    mov off_extdef_number,ax    ; save index info
    cmp bx,seg_extdef_segpart   ; check segment partition entries
    jne to_next_fix         ; no match, ignore

    cmp ax,first_prev_segnum    ; see if offset matches first previous base fixup index
    je  to_set_off_flag     ; yes

    cmp ax,seg_extdef_number    ; see if offset matches previous base fixup index
    je  to_set_off_flag     ; yes

to_next_fix:
    jmp NEAR PTR next_fix   ; no, ignore

to_set_off_flag:
    jmp NEAR PTR set_off_flag   ; set offset is far flag, save reference

; base fixup of indirect reference
non_ovl_base:
    mov seg_extdef_segpart,bx   ; save segment partition info
    mov seg_extdef_number,ax    ; save index info
    cmp bx,off_extdef_segpart   ; check segment partition entries
    jne to_next_fix2        ; no match, ignore

    cmp ax,first_prev_offnum    ; see if base matches first previous offset fixup index
    je  fix_chk_loc         ; yes

    cmp ax,off_extdef_number    ; see if base matches previous offset fixup index
    jne to_next_fix2        ; no, ignore

fix_chk_loc:
    mov loc,1               ; change base to offset so reference saved as far offset
    jmp NEAR PTR set_off_flag   ; set offset is far flag, save reference

to_next_fix2:
    jmp NEAR PTR next_fix

fix_seg_ovl_class:
    mov es,dx               ; es -> i/o buffer
    mov ah,al               ; save bit flags
    and al,1                ; get overlaid bit
    jne is_ovl_save         ; overlaid public, process

; check if public is overlay class and current segment is overlaid
; (have to track overlays calling the root)
    cmp data_seg_overlaid,0 ; see if segment is overlaid
    je  fix_16              ; no, check publics to see if resolved or not

; check if nonvector root calls flag is set, if so don't track overlays
; calling the root
    cmp nonovl_rvect,0      ; see if nonvectored root call
    jne fix_16              ; yes, bypass check of overlay class bit

    and ah,8                ; get overlay class bit
    jne is_ovl_save         ; overlay class public called from overlay, save it

; bl holds definition flag value
fix_16:
    cmp bl,2                ; check if resolved to public or absolute
    jae to_next_fix2        ; resolved public, not overlaid

; unresolved public, could be overlaid, save it and check it after pass 1

is_ovl_save:
    cmp loc,3               ; see if pointer fixup
    je  fix_overlaid        ; yes
    mov bx,segpart_owner    ; get info to save
    mov ax,target_index
    cmp loc,2               ; see if base fixup
    jne is_ovl_off          ; no, offset

; base fixup of direct reference
    mov seg_extdef_segpart,bx   ; save segment partition info
    mov seg_extdef_number,ax    ; save index info
    cmp bx,off_extdef_segpart   ; check segment partition entries
    jne to_next_fix2        ; no match, ignore

    mov bx,first_prev_offptr    ; init bx -> first previous offset entry
    cmp ax,first_prev_offnum    ; see if base matches first previous offset fixup index
    je  fix_near_far        ; yes

    cmp ax,off_extdef_number    ; see if base matches previous offset fixup index
    jne to_next_fix2        ; no, ignore

    mov bx,off_extdef_ptr   ; bx -> previous offset entry

; base fixup matches previous offset fixup, update near offset to far offset
fix_near_far:
    mov al,[bx+1]           ; get high byte holds flag bits
    test    al,40h          ; see if near bit set
    je  to_next_fix2        ; no, change nothing
    and al,0bfh             ; reset near bit

    test    al,80h          ; see if high bit already set
    jne is_ovl_2            ; yes
    or  al,80h              ; set far bit
    inc curseg_farovlpub_count  ; bump count of far references

is_ovl_2:
    mov [bx+1],al           ; update high byte with new flag bits
    dec curseg_nearovlpub_count ; drop count of near references
    jmp NEAR PTR next_fix   ; continue

; offset fixup of direct reference
is_ovl_off:
    mov off_extdef_segpart,bx   ; save segment partition info
    mov off_extdef_number,ax    ; save index info
    cmp bx,seg_extdef_segpart   ; check segment partition entries
    jne fix_overlaid        ; no, assume near call

    cmp ax,first_prev_segnum    ; see if offset matches previous base fixup index
    je  set_off_flag        ; yes

    cmp ax,seg_extdef_number    ; see if offset matches previous base fixup index
    jne fix_overlaid        ; no, assume near call

; set the offset is far flag so it is treated as far reference
set_off_flag:
    mov off_to_far_flag,1

; put symbol in overlaid publics array
; first check if need to flush pre-existing segment's publics to table
fix_overlaid:
    mov ax,ovlpub_array_seg ; get the array segment
    cmp ax,ovl_data_seg     ; see if matches the overlaid segment partition entry
    je  fix_markpub         ; yes
    cmp ax,-1               ; see if array segment has been set (previous overlaid publics)
    je  fix_17              ; no bypass flush of old publics to table

fix_flush:
    call    flush_publics   ; flush previous segment partition publics to table

fix_17:
    call    init_pubarray   ; reset public array to zeros, reset variables

; di still holds offset into array (extdef index, relative 0)
fix_markpub:
    add di,OFFSET DGROUP:ovlpub_array   ; di -> proper element in array of overlaid publics
    mov bx,[di]             ; get pre-existing element
    and bh,0c0h             ; only keep near and far flag bits
    mov ax,target_index     ; get index of public declaration
    cmp off_to_far_flag,0   ; see if offset is far flag set
    jne fix_faroff          ; yes, all references are far, offset or pointer
    cmp loc,3               ; see if near or far use of extdef
    jne fix_near            ; near
    jmp SHORT fix_far

; offset promoted to far call due to base match
fix_faroff:
    mov off_extdef_ptr,di   ; save pointer to near value in case of later base match

fix_far:
    test    bh,80h          ; get previous far bit value
    jne next_fix            ; already existent far setting, don't update array
    or  ah,80h              ; set high bit to indicate far call
    or  ah,bh               ; merge in near bit if previously set
    inc curseg_farovlpub_count  ; bump count of far overlaid publics
    jmp SHORT fix_18        ; bypass near bit set

; near use of extdef
fix_near:
    mov off_extdef_ptr,di   ; save pointer to near value in case later promotion to far

    mov dx,first_curr_offptr    ; get first current pointer to near value
    or  dx,dx               ; see if previous pointer
    jne fix_near2           ; yes
    mov first_curr_offptr,di    ; save new pointer value

fix_near2:
    test    bh,40h          ; get previous near bit value
    jne next_fix            ; already existent near setting, don't update array
    or  ah,40h              ; set bit 14 to indicate near call
    or  ah,bh               ; merge in far bit if previously set
    inc curseg_nearovlpub_count ; bump count of near overlaid publics

fix_18:
    mov [di],ax             ; put overlaid public in array

next_fix:
    cmp cx,1                ; check if at checksum byte
    jbe fix_ret             ; yes, done
    jmp NEAR PTR fix_2      ; no, check next fixup

fix_ret:
    ret
proc1_fixupp    ENDP

;*****************************
;* FLUSH_PUBLICS             *
;*****************************

; flush segment partition entry's overlaid publics to table
; destroys ax,bx,dx

flush_publics   PROC
    push    es              ; save critical register
    push    si
    push    cx
    mov ax,alloc_ovlpubblk_ptr  ; get pointer to last allocated block
    or  ax,ax
    jne flush_2             ; at least one block allocated

    mov bx,OVLPUB_BLK_SIZE  ; get size of overlaid public declarations block to allocate
    call    alloc_memory    ; get memory for block allocation
    mov es,ax               ; es -> block
    mov WORD PTR es:[0],0   ; zero next block pointer
    mov ovlpubblk_pos,2     ; init block pointer position
    mov alloc_ovlpubblk_ptr,ax  ; update last allocated block pointer
    mov first_ovlpubblk_ptr,ax  ; update first allocate block pointer

flush_2:
    mov es,ax               ; es -> block
    mov bx,ovlpubblk_pos    ; bx offsets into block

; first write segment partition flag and entry value
    cmp bx,OVLPUB_BLK_BYSIZE-9  ; see if at end of block (no room for segment partition entry info)
    jb  flush_3             ; no

    call    new_ovlpub_blk  ; make new overlaid public block

flush_3:
    mov al,1
    cmp ovlpub_array_seg,0  ; see if nonoverlay class segment
    jne flush_3a            ; no
    inc al                  ; yes, set flag to value of 2

flush_3a:
    mov BYTE PTR es:[bx],al  ; set segment partition entry flag
    inc bx                   ; bump offset

; now write segment partition entry value
flush_4:
    mov ax,ovlpub_array_seg
    mov WORD PTR es:[bx],ax
    mov ax,curseg_nearovlpub_count  ; get near overlaid publics count
    mov WORD PTR es:[bx+2],ax   ; save it
    mov cx,ax
    mov ax,curseg_farovlpub_count   ; get far overlaid publics count
    mov WORD PTR es:[bx+4],ax   ; save it
    add cx,ax               ; cx holds count of near and far overlaid publics
    xor ax,ax
    mov WORD PTR es:[bx+6],ax   ; init total used references count
    add bx,8                ; adjust offset past info bytes

; write public entries
    mov si,OFFSET DGROUP:ovlpub_array   ; si offsets into array

flush_loop:
    mov ax,[si]             ; get element
    or  ax,ax               ; see if nonzero
    je  flush_next          ; no, get next element
    mov dh,ah
    and ax,03fffh           ; mask off near and far flag bits
    and dh,0c0h             ; only keep near and far flag bits
    test    dh,80h          ; see if far bit set
    je  flush_near          ; no
    dec cx                  ; yes, drop count of publics to save

flush_near:
    test    dh,40h          ; see if near bit set
    je  flush_5             ; no
    dec cx                  ; yes, drop count of publics to save

flush_5:
    cmp bx,OVLPUB_BLK_BYSIZE-3  ; see if at end of block, no room for public info
    jb  flush_6             ; no

    push    ax              ; save index in ax
    call    new_ovlpub_blk  ; make new overlaid public block
    pop ax                  ; restore index in ax

flush_6:
    mov es:[bx],dh          ; set flag bits byte

; look up public declaration segment entry from index in ax
    dec ax                  ; make ax relative zero offset
    shl ax,1                ; convert ax to word offset
    mov dx,bx               ; save bx value
    mov bx,OFFSET DGROUP:ext_defent_indptr  ; set bx to base of pubdef declarations entry array
    add bx,ax               ; bx -> array element holding pubdef declarations entry address
    mov ax,[bx]             ; ax -> pubdef declarations entry of referenced EXTDEF in FIXUPP record
    mov bx,dx               ; restore bx value
    mov es:[bx+1],ax        ; keep public declaration entry segment
    add bx,3                ; bump to next free slot in table

flush_next:
    jcxz    flush_ret       ; no more overlaid public declaration entries to save 
    add si,2                ; bump to next position in array
    jmp SHORT flush_loop    ; loop back for more

flush_ret:
    mov ovlpubblk_pos,bx    ; update offset into block
    pop cx                  ; restore critical register
    pop si
    pop es
    ret
flush_publics   ENDP

;*****************************
;* NEW_OVLPUB_BLK            *
;*****************************

; create new overlaid public block
; inits bx,ovlpubblk_pos variable
; returns segment of new block in es
; destroys ax,bx,es

new_ovlpub_blk  PROC
    mov ax,alloc_ovlpubblk_ptr  ; get old last allocated block
    push    ax              ; save on stack
    mov bx,OVLPUB_BLK_SIZE  ; get size of overlaid public declarations block to allocate
    call    alloc_memory    ; get memory for block allocation
    pop es                  ; es -> old block
    mov es:[0],ax           ; save -> new block in old block
    mov alloc_ovlpubblk_ptr,ax  ; update last allocated block variable
    mov es,ax               ; es -> new block
    xor ax,ax
    mov WORD PTR es:[0],ax  ; zero next block pointer
    mov ax,2
    mov ovlpubblk_pos,ax
    mov bx,ax
    ret
new_ovlpub_blk  ENDP

;*****************************
;* INIT_PUBARRAY             *
;*****************************

; zero all entries in the overlaid public array
; initialize other array variables
; destroys ax,bx

init_pubarray   PROC
    push    es              ; save critical register
    push    cx
    push    di

; zero the array
    mov ax,ds
    mov es,ax
    mov di,OFFSET DGROUP:ovlpub_array   ; es:di -> array to zero
    mov cx,OVLPUB_BLK_BYSIZE/2  ; get size of block in words
    xor ax,ax
    rep stosw               ; zero it out

    mov curseg_nearovlpub_count,ax  ; init the count of near and far overlaid publics
    mov curseg_farovlpub_count,ax
    mov ax,ovl_data_seg     ; get this segment partition entry
    mov ovlpub_array_seg,ax ; set it as owning segment of publics to put in overlaid array

    pop di                  ; restore critical register
    pop cx
    pop es
    ret
init_pubarray   ENDP

END
