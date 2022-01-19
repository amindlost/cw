;*********************************************************************
;*   MLPASS2D.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/22/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker pass 2 routines part D                                   *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlpass2d
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
PUBLIC  proc2_fixupp,save_thread
PUBLIC  check_index,get_target_addr,get_frame_addr
PUBLIC  fixupp_offset_err

; variables
PUBLIC  frame_method,target_method,target_disp
PUBLIC  frame_ent_ptr,targ_ent_ptr,frext_ingroup
PUBLIC  frame_index,target_index,loc,is_resolved
PUBLIC  fixup_pos,target_prog_off,target_segment
PUBLIC  locat
PUBLIC  is_ind_call,is_relocatable,data_rec_offset,no_fixbyte_flag
PUBLIC  gen_flags

;*****************************
;* Data begins               *
;*****************************

.DATA

; variables
EXTRN   data_offset:DWORD,prev_data_ptr:WORD,filename:BYTE
EXTRN   is_absseg:BYTE
EXTRN   seg_fix_frame:WORD,data_seg_part:WORD
EXTRN   seg_ovl_class_flag:BYTE
EXTRN	is_clipper5:BYTE,compress_this:BYTE,which_ledata:BYTE
EXTRN	current_segind:WORD,clipper_segindex:WORD
EXTRN	ignore_fixupp_flag:BYTE

;*****************************
;* External declarations     *
;*****************************

; initialized local variables

EVEN                        ; maximize speed on 8086 and better

; byte values
EVEN
is_entry_point  DB  0       ; nonzero if have an entry point from modend record
clipper_fixflag	DB	0		; nonzero if previous clipper procedure fixed up
bounds_check    DW  SEGDEF_MAX,GRPDEF_MAX,EXTDEF_MAX

; jump table words
targ_jmp_table  DW  OFFSET _TEXT:gta_seg,OFFSET _TEXT:gta_grp
                DW  OFFSET _TEXT:gta_ext,OFFSET _TEXT:link_error
                DW  OFFSET _TEXT:gta_seg,OFFSET _TEXT:gta_grp
                DW  OFFSET _TEXT:gta_ext,OFFSET _TEXT:link_error

.DATA?

; uninitialized local variables

; byte values
EVEN                        ; maximize speed on 8086 and better
loc     DB  ?               ; loc field
EVEN
frame_method    DB  ?       ; frame method for fixup
EVEN
target_method   DB  ?       ; target method for fixup
EVEN
thread_method   DB  ?       ; thread method for fixup
EVEN
is_relocatable  DB  ?       ; nonzero if target segment is relocatable
EVEN
is_resolved DB  ?           ; nonzero if external index is unresolved
EVEN
is_abspub   DB  ?           ; nonzero if absolute public declaration
EVEN
frext_ingroup   DB  ?       ; nonzero if FRAME external index is in group
EVEN
no_fixbyte_flag DB  ?       ; nonzero if fixup location shouldn't have nonzero bytes
EVEN
is_ind_call DB  ?           ; nonzero if indirect call to overlaid public, or far call via segment fixup
EVEN
gen_flags   DB  ?           ; general flags used for miscellaneous purposes
                            ; bit 0 no vector address changes in frame computation

; word values
EVEN
locat   DW  ?               ; locat field
data_rec_offset DW  ?       ; data record offset field
frame_index DW  ?           ; frame index
target_index    DW  ?       ; target index
thread_index    DW  ?       ; thread index
target_segment  DW  ?       ; target segment
fixup_pos   DW  ?           ; position of current fixup field in fixupp record
frame_ent_ptr   DW  ?       ; pointer to segment holding FRAME group or segment entry
targ_ent_ptr    DW  ?       ; pointer to segment holding TARGET group, segment, or symbol entry
symbol_segbase	DW	?		; initial symbol segment base value for Clipper compression

; doubleword values
target_prog_off DD  ?       ; target program offset
target_disp DD  ?           ; target displacement field
lseg_canon      DD  ?       ; use to compute canonical (normalized) segment value
                            ; of LSEG (logical segment), used for LOC type fixup

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR,link_warning:NEAR
EXTRN   get_data_off_seg:NEAR,proc2_data:NEAR,fixup_warning:NEAR
EXTRN   fixup_ovl_extdef:NEAR,fixup_ind_ref_extdef:NEAR
EXTRN   fixup_seg_ref:NEAR
EXTRN	compsym_fixup:NEAR

;*****************************
;* PROC2_FIXUPP              *
;*****************************

; pass 2 fixupp record processing
; upon entry cx=record length,bp=buffer_end,al=record type
; es:si -> first byte of record past record length
; destroys ax,bx,dx,di
; updates si

proc2_fixupp    PROC
	cmp	ignore_fixupp_flag,0	; see if ignoring this fixupp
	je	pf_0				; no
	jmp	NEAR PTR pf_out		; yes, ignore it

pf_0:
    cmp prev_flag,0         ; check if previous L?DATA record
    je  pf_1                ; no previous data record (should be thread fixups)

    push    es              ; save critical registers used by get_data_off_seg procedure
    push    si
    push    cx
    call    get_data_off_seg    ; get data record offset and segment
    pop cx                  ; restore critical registers
    pop si
    pop es

pf_1:
    mov data_fixup_count,0  ; init count of relocation items in array

pf_loop1:
; get byte value, but don't increment past it
    mov fixup_pos,si        ; keep -> fixup in case of error
    mov al,es:[si]          ; get first byte of either thread or fixup field
    and al,80h              ; check fixup field, high bit set
    je  pf_3                ; no, thread field

    cmp prev_flag,0         ; check if previous L?DATA record
    jne pf_2                ; yes

; fixup field given with no preceding L?DATA record
    mov ax,FIXUPP_DATA_ERR  ; FIXUPP contains a fixup without preceding data record
    mov dx,OFFSET DGROUP:filename
    jmp NEAR PTR link_error ; transfer control to error handler

; inner loop without previous flag check to speed things up
pf_loop2:
; get byte value, but don't increment past it
    mov fixup_pos,si        ; keep -> fixup in case of error
    mov al,es:[si]          ; get first byte of either thread or fixup field
    and al,80h              ; check fixup field, high bit set
    je  pf_3                ; no, thread field

pf_2:
    call    perform_fixup   ; do fixup field code
    cmp cx,1                ; check if at checksum byte
    ja  pf_loop2            ; no, loop back for next fixup/thread field
    jmp SHORT pf_eat_chksum ; yes, eat it and continue

pf_3:
    call    save_thread     ; do thread field code
    cmp cx,1                ; check if at checksum byte
    ja  pf_loop1            ; no, loop back for next fixup/thread field

pf_eat_chksum:
    inc si                  ; bump past checksum byte, to next record byte
    dec cx
    cmp si,bp               ; check boundary conditions
    jb  pf_next_rec         ; okay
    call    load_file       ; load next portion of file into buffer, at end position

; check the next record.  if another fixupp record, process it.
; this allows multiple FIXUPP records after a L?DATA record as needed
; by Clipper, Force, and possibly others.

pf_next_rec:
    mov al,es:[si]          ; get next record type
    cmp al,FIXUPP           ; see if fixupp record
    jne pf_proc_data        ; no
    mov read_buff_ptr,si    ; keep beginning of object record
    inc si                  ; bump si past record type
    cmp si,bp               ; check boundary conditions
    jb  pf_4                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pf_4:
    mov cl,es:[si]          ; get record length low byte
    inc si
    cmp si,bp               ; check boundary conditions
    jb  pf_5                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pf_5:
    mov ch,es:[si]          ; get record length high byte
    inc si                  ; bump si past length word
    cmp si,bp               ; check boundary conditions
    jb  pf_6                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pf_6:
; check if a null record
    cmp cx,1                ; see if null record
    jbe pf_eat_chksum       ; yes, ignore it

; check that record doesn't exceed file length
    mov al,eof_flag
    or  al,al               ; see if end of file encountered
    je  pf_loop1            ; nope, loop back and process next FIXUPP record
    mov dx,cx               ; get record length
    add dx,si               ; add in current position
    jnc pf_7                ; no overflow

pf_unexpected_eof:
    mov dx,OFFSET DGROUP:filename
    mov ax,UNEXPECT_EOF_ERR ; unexpected end of file
    jmp NEAR PTR link_error ; transfer control to error handler

pf_7:
    jmp NEAR PTR pf_loop1       ; loop back and process next FIXUPP record

; process the modified LEDATA or LIDATA record, if any
pf_proc_data:
    cmp prev_flag,0         ; check if previous L?DATA record
    je  pf_ret              ; no previous data record (thread fixup only)

	cmp	compress_this,0		; see if compressing Clipper code
	je	pf_proc2			; no
	mov	ax,current_segind
	cmp	ax,clipper_segindex
	jae	pf_proc2			; not Clipper code

    mov bx,prev_data_ptr    ; bx -> previous record position data bytes/block
	cmp	clipper_fixflag,0	; see if first Clipper procedure fixed up
	jne	pf_notfirst

; first clipper procedure fixed up
; save fixup value at offset 0bh for S'87 or offset 4 for Clipper 5
	mov	clipper_fixflag,1	; flag following Clipper procedures as not first
	cmp	is_clipper5,0		; see if clipper 5 code
	je	pf_s87save			; no
	mov	ax,es:[bx+4]		; get initial segment base value for Clipper 5

pf_basesave:
	mov	symbol_segbase,ax	; save the initial symbol segment base value
	jmp	SHORT pf_proc2

pf_s87save:
	mov	ax,es:[bx+0bh]		; get initial segment base value for Summer 87
	jmp	SHORT pf_basesave

; subsequent clipper procedure
; place saved fixup symbol segment base value at offset 0bh or offset 4
pf_notfirst:
	mov	ax,symbol_segbase	; get the initial segment base value
	cmp	is_clipper5,0		; see if clipper 5 code
	je	pf_s87store			; no
	mov	es:[bx+4],ax		; store initial segment base value for Clipper 5
	jmp	SHORT pf_proc2

pf_s87store:
	mov	es:[bx+0bh],ax		; store inital segment base value for Summer 87

pf_proc2:
    call    proc2_data

pf_out:
    mov prev_flag,0         ; reset previous record flag

pf_ret:
    ret
proc2_fixupp    ENDP

;*****************************
;* PERFORM_FIXUP             *
;*****************************

; parse the fixup field and perform the fixup
; destroys ax,bx,dx,di
; updates si record pointer, cx record length

perform_fixup   PROC
    xor al,al
    mov is_absseg,al        ; init absolute flags
    mov is_abspub,al
    mov al,es:[si]          ; get low byte of locat field
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pef_2               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pef_2:
    mov ah,es:[si]          ; get high byte of locat field
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pef_3               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pef_3:
    mov locat,ax            ; save locat field to memory variable

    mov dl,al
    and dl,1ch              ; get loc field
    shr dl,1
    shr dl,1                ; make loc value relative zero
    mov loc,dl              ; save loc value

    mov BYTE PTR data_rec_offset,ah ; save bit 7-0 of data record offset in locat high byte
    and al,3                ; get bit 9-8 of data record offset in locat low byte
    mov BYTE PTR data_rec_offset+1,al   ; and save it

    mov dl,es:[si]          ; get fix dat field
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pef_4               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pef_4:
    mov dh,dl               ; dh holds fix dat field value
    and dh,70h              ; get frame field in dh
    shr dh,1
    shr dh,1
    shr dh,1
    shr dh,1                ; make frame value relative zero

    mov al,dl               ; get fix dat field
    test    al,80h          ; check if thread field for frame (fbit)
    jne pef_thrdframe       ; yes

; no thread field for frame
    cmp dh,2                ; see if index specified for this frame
    ja  pef_5               ; no

; index specified for this field
    xor ah,ah               ; zap high byte
    mov al,es:[si]          ; get frame datum first byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pef_4a              ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pef_4a:
    cmp al,80h              ; see if two byte field
    jb  pef_4b              ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte

    mov al,es:[si]          ; get frame datum second byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pef_4b              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pef_4b:
    mov frame_index,ax      ; frame index, if any

pef_5:
    mov frame_method,dh     ; frame method is the frame field
    jmp SHORT pef_6         ; bypass frame thread field code

; frame thread field exceeds 3
pef_frthrd_err:
    mov cl,al
    mov ax,FRAME_THRD_ERR
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

; thread field for frame
pef_thrdframe:
    shr al,1                ; make frame field value relative zero
    shr al,1
    shr al,1
    shr al,1
    and al,7                ; mask off F bit
    cmp al,3                ; make sure frame thread field doesn't exceed 3
    ja  pef_frthrd_err      ; bad frame thread field

    xor ah,ah               ; zap high byte
    mov bx,OFFSET DGROUP:frame_thrd_meth    ; bx -> frame thread method array base
    add bx,ax               ; bx -> proper byte array element
    mov bl,[bx]             ; get frame method
    mov frame_method,bl     ; save to memory variable

    shl ax,1                ; convert to word offset
    mov bx,OFFSET DGROUP:frame_thrd_index   ; bx -> frame thread index array base
    add bx,ax               ; bx -> proper word array element
    mov ax,[bx]             ; get frame index
    mov frame_index,ax      ; save to memory variable

pef_6:
    mov al,dl               ; get fix dat field
    test    al,8            ; check if thread field for target (tbit)
    jne pef_thrdtarg        ; yes

; no thread field for target
    xor ah,ah               ; zap high byte
    mov al,es:[si]          ; get target datum first byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pef_6a              ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pef_6a:
    cmp al,80h              ; see if two byte field
    jb  pef_6b              ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte

    mov al,es:[si]          ; get target datum second byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pef_6b              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pef_6b:
    mov target_index,ax     ; target index, if any
    mov al,dl               ; get fix dat field
    and al,3                ; break out targt field
    mov target_method,al    ; target method is the targt field
    jmp SHORT pef_7         ; bypass target thread field code

; illegal frame method is 3, 6, or 7
pef_bad_frame:
    mov cl,bl
    mov ax,FRAME_METH_ERR
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

; bad target method of 3 (prior to P bit modifier)
pef_bad_target:
    mov cl,bl
    mov ax,TARGET_METH_ERR
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

; segment, group, or external index was out of bounds
pef_oob:
    jmp NEAR PTR ci_oob

; thread field for target
pef_thrdtarg:
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

; check index validity
pef_7:
    mov bl,frame_method
    cmp bl,5
    ja  pef_bad_frame       ; frame method above 5, invalid
    cmp bl,3
    je  pef_bad_frame       ; frame method is 3, invalid
    ja  pef_8               ; frame method okay and no index specified

    mov ax,frame_index      ; get frame index for check_index routine
    shl bl,1                ; make word offset
    xor bh,bh
    add bx,OFFSET DGROUP:bounds_check
    cmp ax,[bx]             ; see if out of bounds
    ja  pef_oob             ; yes

pef_8:
    mov ax,target_index     ; get target index for check_index
    mov bl,target_method
    cmp bl,3                ; check for bad target method
    je  pef_bad_target      ; bad target method

    shl bl,1                ; make word offset
    xor bh,bh
    add bx,OFFSET DGROUP:bounds_check
    cmp ax,[bx]             ; see if out of bounds
    ja  pef_oob             ; yes

    mov al,dl               ; get fix dat field
    and al,4                ; get P bit field
    or  target_method,al    ; P bit modifies target method, merge it in

    or  al,al               ; check if P bit set
    mov ax,0                ; assume it is, setup for zero'ing target displacment, don't change flags via xor
    jne pef_10              ; yes, P bit set, force target displacement value to zero

; get target displacement field if P bit is zero
    mov al,es:[si]          ; get low byte of target displacement
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pef_8b              ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pef_8b:
    mov ah,es:[si]          ; get high byte of target displacement
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pef_10              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pef_10:
    mov WORD PTR target_disp,ax ; save target displacement value
    cwd
    mov WORD PTR target_disp+2,dx   ; save sign extension of target displacement

; compute program offset of target address and its frame
    xor al,al
    mov is_ind_call,al      ; init indirect call to overlaid public flag
    mov no_fixbyte_flag,al  ; init flag for fixup bytes
    mov gen_flags,al        ; init general flags

    inc al
    mov is_resolved,al      ; init is_resolved flag to assume fixup okay
    call    get_target_addr

    cmp is_ind_call,0       ; see if indirect call flag set
    jne pef_resolved        ; yes, bypass the frame computation, not needed

; see if absolute segment or absolute public declaration, no frame computation
    mov al,is_absseg
    or  al,is_abspub
    jne pef_resolved        ; either absolute segment or absolute public declaration

    mov al,frame_method
    cmp al,4                ; check if frame determined by location segment
    jne pef_11              ; no

    mov ax,WORD PTR lseg_canon  ; get canonical segment low word offset containing lseg
    mov bx,WORD PTR lseg_canon+2    ; get high word

    shr bx,1
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1
    rcr ax,1                ; /16, paragraph value (largest possible frame number)
    mov target_segment,ax

    mov al,1
    mov is_relocatable,al   ; flag that this is a relocatable segment
    jmp SHORT pef_13        ; bypass other target segment computation code

pef_11:
    cmp al,5                ; check if frame determined by target's index
    jne pef_12              ; no
    mov al,1                ; flag to get_frame_addr procedure to use target data
    call    get_frame_addr
    jmp SHORT pef_13        ; bypass other target segment computation code

pef_ret:
    ret                     ; early return from routine

; frame determined by segment, group, or external index
pef_12:
    xor al,al               ; flag to get_frame_addr procedure to use frame data
    call    get_frame_addr

pef_13:
    mov al,is_resolved
    or  al,al               ; check if reference to unresolved external
    je  pef_ret             ; yes, don't perform a fixup

; convert target segment and target program offset to segment:offset format
pef_resolved:
    push    bp              ; save critical register
    mov ax,buffer_end
    mov bp,ax               ; check only for end of physical buffer overflow

; see if absolute segment or absolute public declaration, no address adjustment
    mov al,is_absseg
    or  al,is_abspub
    jne pef_seg_rel         ; either absolute segment or absolute public declaration

; check M bit for self-relative fixup
    mov al,BYTE PTR locat   ; get locat field low byte
    and al,40h              ; get M bit field value
    je  pef_self_rel        ; M bit reset, fixup self-relative

; fixup segment relative
pef_seg_rel:
    mov ax,target_segment
    xor bx,bx               ; bx:ax will hold byte value of target segment
    shl ax,1
    rcl bx,1                ; x2
    shl ax,1
    rcl bx,1                ; x4
    shl ax,1
    rcl bx,1                ; x8
    shl ax,1
    rcl bx,1                ; x16

; get target offset in bx:di
    mov di,WORD PTR target_prog_off ; get absolute offset low word
    sub di,ax               ; compute low word difference
    mov ax,bx
    mov bx,WORD PTR target_prog_off+2   ; get absolute offset high word
    sbb bx,ax               ; compute high word difference, with borrow
    jmp SHORT pef_14        ; bypass self-relative fixup code

; intersegment self-relative fixup, fatal error
fsr_inv:
    mov cl,loc
    mov ax,SELF_REL_FIX_ERR ; intersegment self-relative fixup error
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

; fixup is self-relative
pef_self_rel:

    mov al,is_ind_call
    cmp al,2                ; don't do self-relative fixup code if flag set for current segment fixup
    je  pef_seg_rel

    cmp loc,2               ; can't have self-relative segment/base fixups
    je  fsr_inv             ; invalid loc value
    cmp loc,3               ; can't have self-relative pointer (segment:offset) fixups
    je  fsr_inv             ; invalid loc value

    mov di,WORD PTR target_prog_off ; get absolute offset low word
    mov bx,WORD PTR target_prog_off+2   ; get absolute offset high word
    sub di,WORD PTR data_offset
    sbb bx,WORD PTR data_offset+2   ; borrow to high word
    sub di,data_rec_offset  ; make target offset relative to location
    sbb bx,0                ; borrow to high word

    mov al,loc
    or  al,al               ; check if low-order byte
    jne pef_13a             ; no
    sub di,1                ; adjust for one byte length
    sbb bx,0                ; borrow to high word
    jmp SHORT pef_14        ; bypass remaining adjustment code

pef_13a:
    cmp al,1                ; check if offset
    je  pef_13b             ; yes
    cmp al,5                ; check if loader-resolved (same as offset)
    jne pef_14              ; no, perform no adjustments

pef_13b:
    sub di,2                ; adjust for two byte length
    sbb bx,0                ; borrow to high word

;ready to fixup the address
pef_14:

; check if fixup overflow, bx should be 0 or 0ffffh (-64K >= offset <= 64K)
    inc bx
    cmp bx,1
    jbe pef_14a             ; no fixup overflow

; check if possible lack of sign extension in data_offset vs. target_prog_off
    cmp bx,0ffffh
    jne pef_fixwarn         ; bx not equal to -1

; lack of sign extension only if target_disp+2==0FFFFh
    mov ax,WORD PTR target_disp+2
    cmp ax,0ffffh
    je  pef_14a             ; target_disp+2 == 0FFFFh

pef_fixwarn:
    call    fixup_warning   ; yes, print warning message

pef_14a:
	cmp	compress_this,0		; see if compressing Clipper code
	je	pef_notsymcomp		; no
	mov	ax,current_segind
	cmp	ax,clipper_segindex	; see if fixing up a SYMBOLS table during compression
	jne	pef_notsymcomp		; not
	call	compsym_fixup	; fixup of compressed symbols, modify data_rec_offset if compress
	jc	pef_out				; return carry flag set if throw away fixup

pef_notsymcomp:
    mov ax,di               ; get 2-byte offset in ax
    mov bx,prev_data_ptr    ; point to previous record position data bytes/block
    mov di,data_rec_offset  ; di holds offset into data record bytes/block
    mov dx,bx
    add dx,di               ; see if data bytes read past buffer end

    jc  pef_overflow        ; overflow occurred, bypass buffer end check

pef_14b:
    cmp dx,bp               ; check if past buffer
    jb  pef_14c             ; no

pef_overflow:
    sub dx,bp               ; adjust data record offset

    xor bx,bx               ; zero bx base
    mov di,dx               ; and adjust di record offset, overflow amount

pef_14c:
    mov dl,loc
    or  dl,dl               ; check if low_order byte
    jne pef_15              ; no

; low order byte
    add es:[bx+di],al       ; save new offset value back to data record
    pop bp                  ; restore critical register
    ret                     ; done

pef_15:
    cmp dl,1                ; check if offset
    je  pef_15a             ; yes
    cmp dl,5                ; check if loader-resolved offset, treat as offset
    jne pef_16              ; no

; offset or loader-resolved offset
pef_15a:
    add bx,di               ; bx -> data record byte
    mov dh,es:[bx]          ; dh holds pre-existing fixup bytes
    add es:[bx],al          ; save new offset low byte value back to data record
    adc ah,0                ; carry to high byte
    inc bx                  ; point to next byte
    cmp bx,bp               ; check if past buffer end
    jb  pef_15b             ; no

    xor bx,bx               ; wrap to first byte in buffer by zero'ing bx

; offset, base, and pointer all vector here for return
pef_15b:
    or  dh,es:[bx]          ; dh holds pre-existing fixup bytes
    add es:[bx],ah          ; save new value back to data record

    or  dh,dh               ; see if any pre-existing fixup bytes
    jne pef_chkovl          ; yes, check if overlaid reference

pef_out:
    pop bp                  ; restore critical register
    ret                     ; done

; nonzero bytes in fixup location, check if overlaid reference
; if so, generate warning
pef_chkovl:
    mov al,no_fixbyte_flag  ; get fixup byte flag
    or  al,al               ; see if set
    je  pef_out             ; no

pef_ovl_warn:
    push    es              ; save critical register

    mov bx,targ_ent_ptr
    mov al,target_method
    and al,3
    jne pef_ovl_ext         ; target extdef

; target segment
    mov ax,NONOVL_SEG_WARN  ; nonoverlayable segment warning
    mov es,bx
    les di,es:[8]
    add di,8                ; es:di -> segment name
    jmp SHORT pef_printwarn

; target extdef
pef_ovl_ext:
    mov ax,NONOVL_SYM_WARN  ; nonoverlayable symbol warning
    mov es,bx
    les di,es:[4]           ; es:di -> symbol name

pef_printwarn:
    mov dx,OFFSET DGROUP:filename
    call    link_warning
    pop es

    pop bp                  ; restore critical register
    ret                     ; done

pef_16:
    cmp dl,2                ; check if segment
    jne pef_17              ; no

; segment
pef_segment:
    cmp is_relocatable,0    ; check if relocatable segment
    je  pef_seg2            ; no

    mov dx,bx               ; save record position
    mov bx,OFFSET DGROUP:data_fixup_flag    ; bx -> base of fixup flag array
    mov ax,data_fixup_count ; get count of fixups in data record
    inc data_fixup_count    ; bump count of fixups
    shl ax,1                ; convert to word offset
    add bx,ax               ; bx -> first free array element to store fixup
    mov ax,data_rec_offset  ; get fixup position in data record
    mov [bx],ax             ; store in array
    mov bx,dx               ; restore record position

pef_seg2:
    add bx,di               ; bx -> data record byte
    xor dh,dh

; code shared with pointer (segment:offset) fixup
pef_seg3:
    mov ax,target_segment
    or  dh,es:[bx]          ; dh holds pre-existing fixup bytes
    add es:[bx],al          ; save new segment low byte value back to data record
    adc ah,0                ; carry to high byte
    inc bx                  ; point to next byte
    cmp bx,bp               ; check if past buffer end
    jb  pef_15b             ; no

    xor bx,bx               ; wrap to first byte in buffer by zero'ing bx
    jmp SHORT pef_15b

pef_17:
    cmp dl,3                ; check if pointer (segment:offset)
    jne pef_bad_loc         ; no, bad loc field value

; pointer fixup
pef_pointer:
    cmp is_relocatable,0    ; check if relocatable segment
    je  pef_point2          ; no

    push    bx              ; save record position
    mov dx,ax               ; save offset in dx
    mov bx,OFFSET DGROUP:data_fixup_flag    ; bx -> base of fixup flag array
    mov ax,data_fixup_count ; get count of fixups in data record
    inc data_fixup_count    ; bump count of fixups
    shl ax,1                ; convert to word offset
    add bx,ax               ; bx -> first free array element to store fixup
    mov ax,data_rec_offset  ; get fixup position in data record
    add ax,2                ; position past two offset bytes
    mov [bx],ax             ; store in array
    mov ax,dx               ; restore offset to ax
    pop bx                  ; restore record position

pef_point2:
    add bx,di               ; bx -> data record byte
    mov dh,es:[bx]          ; dh holds pre-existing fixup bytes
    add es:[bx],al          ; save new offset low byte value back to data record
    adc ah,0                ; carry to high byte
    inc bx                  ; point to next byte
    cmp bx,bp               ; check if past buffer end
    jb  pef_18a             ; no

    xor bx,bx               ; wrap to first byte in buffer by zero'ing bx

pef_18a:
    or  dh,es:[bx]          ; dh holds pre-existing fixup bytes
    add es:[bx],ah          ; save new value back to data record
    inc bx                  ; point to next byte
    cmp bx,bp               ; check if past buffer end
    jb  pef_18b             ; no

    xor bx,bx               ; wrap to first byte in buffer by zero'ing bx

pef_18b:
    jmp NEAR PTR pef_seg3   ; do segment code as well

; illegal loc values 4, 6, or 7
pef_bad_loc:
    mov cl,dl
    mov ax,LOC_VAL_ERR      ; bad loc field value in FIXUPP record
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

perform_fixup   ENDP

;*****************************
;* FIXUPP_OFFSET_ERR         *
;*****************************

; passes proper file offset in fixupp record and file name to link error
; terminates WarpLink, all non-error reporting registers are trashable
; ax holds error code upon entry
; cx holds associated error value, if any

fixupp_offset_err   PROC
    cmp si,fixup_pos        ; check if si is less than fixup_pos (buffer wrapped)
    jae foe_2               ; no

; back buffer_end bytes off of file_pos_adj, error occurred before buffer wrap
    mov bx,buffer_end
    sub WORD PTR file_pos_adj,bx    ; update low word
    sbb WORD PTR file_pos_adj+2,0   ; borrow to high word

foe_2:
    mov si,fixup_pos        ; get proper fixup record offset
    mov dx,OFFSET DGROUP:filename
    jmp NEAR PTR link_error ; transfer control to error handler
fixupp_offset_err   ENDP

;*****************************
;* CHECK_INDEX               *
;*****************************

; check that a segdef, grpdef, or extdef index is not out of bounds
; upon entry ax holds index
; bl holds method, segment, group, or external
; destroys ax,bx

check_index PROC
    shl bl,1                ; make word offset
    xor bh,bh
    add bx,OFFSET DGROUP:bounds_check
    cmp ax,[bx]             ; see if out of bounds
    ja  ci_oob              ; yes
    ret                     ; no

ci_oob:
    or  bl,bl               ; check if segment index
    jne ci_grpchk           ; no

    mov cx,ax
    mov ax,SEGDEF_VAL_ERR   ; segment index too large
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

ci_grpchk:
    cmp bl,2                ; check if a group index
    jne ci_ext              ; no

    mov cx,ax
    mov ax,GRPDEF_VAL_ERR   ; group index too large
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

; assume external index
ci_ext:
    mov cx,ax
    mov ax,EXTDEF_VAL_ERR   ; external index too large
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler
check_index ENDP

;*****************************
;* GET_TARGET_ADDR           *
;*****************************

; compute target program offset for fixupp address
; uses target_method,target_index,target_disp memory variables
; can reset is_resolved memory variable flag
; return target program offset in memory variable target_prog_off
; destroys ax,bx

get_target_addr PROC
    push    es              ; save critical register
    mov ax,target_index     ; get target index
    dec ax                  ; make ax relative zero offset
    shl ax,1                ; convert ax to word offset in arrays
    mov bl,target_method

    xor bh,bh               ; zero high byte
    shl bx,1                ; make word offset
    add bx,OFFSET DGROUP:targ_jmp_table ; point to proper jump table address
    jmp WORD PTR [bx]

; segment index, method 0 or 4
gta_seg:
    mov bx,OFFSET DGROUP:seg_partent_indptr  ; set bx to base of segment partition address array
    add bx,ax               ; bx -> array element holding segment partition entry address
    mov es,[bx]             ; es -> segment partition entry

; check if absolute segment
    mov ax,es:[4]           ; zero if absolute segment (es -> absolute segdef entry)
    or  ax,ax
    je  gta_is_abs          ; zero, absolute segment

; not an absolute segment, see if overlaid
    test    BYTE PTR es:[15],80h    ; see if overlaid segment
    je  gta_not_overlaid    ; no

    mov ax,es               ; get segment partition entry of segment fixup
    cmp ax,data_seg_part    ; see if matches L?DATA segment partition entry
    je  gta_seg_fix         ; yes, overlaid segment matches current segment

    cmp loc,3               ; see if a pointer fixup (overlaid segment <> current)
    jne gta_not_point

gta_seg_fix:
    call    fixup_seg_ref   ; fixup the far reference to an overlaid segment
    jmp SHORT gta_ovl_seg   ; do housekeeping code and return

gta_not_point:
    mov ax,WORD PTR target_disp     ; use target displacement only as target program offset
    mov WORD PTR target_prog_off,ax ; update target program offset low word
    mov ax,WORD PTR target_disp+2   ; get sign extension
    mov WORD PTR target_prog_off+2,ax   ; update offset high word

; es -> segment partition entry
gta_ovl_seg:
    mov bx,es:[4]           ; get overlay identifier
    dec bx                  ; make relative zero
    shl bx,1                ; convert to word offset
    mov es,master_segblk    ; es:bx -> master segdef entry
    mov ax,es:[bx]
    mov targ_ent_ptr,ax     ; save segment owner

    pop es
    ret

gta_not_overlaid:
    mov ax,WORD PTR target_disp ; get original target displacement
    add ax,es:[0]           ; add in offset of segment partition entry
    mov WORD PTR target_prog_off,ax ; update target program offset low word
    mov ax,WORD PTR target_disp+2   ; get sign extension
    adc ax,0                ; carry to high word
    mov WORD PTR target_prog_off+2,ax   ; update target program offset high word
    mov ax,es:[4]           ; get master segdef entry from segment partition entry back pointer
    mov targ_ent_ptr,ax     ; save pointer to target entry

gta_segdef_entry:
    mov es,ax               ; es -> master segdef entry
    mov ax,es:[2]           ; get low word of segment offset
    mov WORD PTR lseg_canon,ax  ; save low word of offset for canonical computation
    add WORD PTR target_prog_off,ax ; add to target program offset low word
    mov ax,es:[4]           ; get high word of segment offset
    mov WORD PTR lseg_canon+2,ax    ; save high word of offset for canonical computation
    adc WORD PTR target_prog_off+2,ax   ; add to target program offset high word with carry
    pop es
    ret

; absolute segment
gta_is_abs:
    xor ax,ax
    mov is_relocatable,al   ; nonrelocatable segment
    mov targ_ent_ptr,ax     ; zero frame entry pointer
    inc al
    mov is_absseg,al        ; set absolute segment flag
    mov ax,es:[0]           ; get frame number, convert to bytes
    mov target_segment,ax   ; get frame number in target_segment in case of LOC frame fixup
    xor bx,bx               ; zero high word of target program offset
    shl ax,1
    rcl bx,1                ; x2
    shl ax,1
    rcl bx,1                ; x4
    shl ax,1
    rcl bx,1                ; x8
    shl ax,1
    rcl bx,1                ; x16
    add ax,WORD PTR target_disp ; add in original target displacement
    adc bx,WORD PTR target_disp+2   ; carry to high word
    mov WORD PTR target_prog_off,ax ; update low word
    mov WORD PTR target_prog_off+2,bx   ; update high word
    pop es
    ret

; group index, method 1 or 5
gta_grp:
    mov bx,OFFSET DGROUP:grp_ent_indptr ; set bx to base of group entry array address
    add bx,ax               ; bx -> array element holding group entry address 
    mov es,[bx]             ; es -> group entry
    mov ax,es
    mov targ_ent_ptr,ax     ; save pointer to target entry

    mov ax,es:[0]           ; get low word of group offset
    mov WORD PTR lseg_canon,ax  ; save low word of offset for canonical computation
    add ax,WORD PTR target_disp ; add in original target displacement

    mov WORD PTR target_prog_off,ax ; update target program offset low word
    mov ax,es:[2]
    mov WORD PTR lseg_canon+2,ax    ; save high word of offset for canonical computation
    adc ax,WORD PTR target_disp+2   ; add high word of group offset with carry
    mov WORD PTR target_prog_off+2,ax   ; update target program offset high word
    pop es                  ; restore critical register
    ret

; external index, method 2 or 6
gta_ext:
    mov bx,OFFSET DGROUP:ext_defent_indptr  ; set bx to base of pubdef declarations entry address array
    add bx,ax               ; bx -> array element holding pubdef declarations entry address
    mov es,[bx]             ; es -> pubdef declarations entry

    or  BYTE PTR es:[14],40h    ; flag that used in fixup

    mov ax,es
    mov targ_ent_ptr,ax     ; save pointer to target entry

;***    test    BYTE PTR es:[15],4  ; see if local public
;***    jne gta_2               ; yes, don't vector it

    cmp seg_ovl_class_flag,0    ; see if overlay class segment
    jne gta_chk_dir_vect    ; yes

    test    BYTE PTR es:[15],1  ; see if public is overlaid
    je  gta_2               ; no

    mov al,loc
    or  al,al               ; see if lobyte fixup
    je  gta_2               ; yes
    xor bh,bh               ; flag to give nonzero target displacement warning if necessary
    call    fixup_ind_ref_extdef    ; fixup the indirect far reference to overlaid public

gta_ret:
    pop es                  ; restore critical register
    ret

; check if need to vector a direct call to overlay class public
gta_chk_dir_vect:
    test    BYTE PTR es:[15],1  ; see if overlaid public
    jne gta_chk_ovl_ext     ; yes
    cmp ovl_code_id,0       ; see if current segment is overlaid
    je  gta_2               ; no

    cmp nonovl_rvect,0      ; see if nonvector root call flag is set
    jne gta_2               ; yes, bypass vector code
    test    BYTE PTR es:[15],8  ; see if public is overlay class
    je  gta_2               ; no

; special case to allow accessing overlay class nonoverlaid code address from
; an overlaid segment.  If target displacement is nonzero then assume
; that the program really wants the address and not a vectored address.
    mov ax,WORD PTR target_disp
    or  ax,WORD PTR target_disp+2   ; see if nonzero target displacement
    jne gta_2               ; yes

gta_chk_ovl_ext:
    call    fixup_ovl_extdef    ; check if should change public offset to vector
    or  al,al               ; al return value is 0 if should continue
    jne gta_ret             ; nonzero, done

gta_2:
    mov ax,WORD PTR target_disp ; get original target displacement
    add ax,es:[8]           ; add in offset of pubdef declarations entry
    mov WORD PTR target_prog_off,ax ; update target program offset low word
    mov ax,WORD PTR target_disp+2
    adc ax,0                ; carry to high word
    mov WORD PTR target_prog_off+2,ax   ; carry to target program offset high word

    test    BYTE PTR es:[15],1  ; see if overlaid public (referenced within same segment)
    jne gta_ret             ; yes, don't add in segment partition/sefdef offsets

    mov al,es:[14]          ; get definitions byte value
    and al,3                ; only keep pubdef/extdef/comdef field
    cmp al,2                ; check if unresolved external
    jb gta_3                ; unresolved external

; external resolved, fixup okay
    cmp al,3                ; see if absolute public declaration
    je  gta_4               ; yes

; pubdef, not absolute
    mov es,es:[0]           ; es -> segment partition entry
    test    BYTE PTR es:[15],80h    ; see if overlaid segment (local public with overlaid flag not set)
    jne gta_ret             ; yes, don't add in segment partition/sefdef offsets
    mov ax,es:[0]           ; get offset of segment partition entry
    add WORD PTR target_prog_off,ax ; update target program offset low word
    adc WORD PTR target_prog_off+2,0    ; carry to target program offset high word
    mov ax,es:[4]           ; get master segdef entry from segment partition entry back pointer
    jmp NEAR PTR gta_segdef_entry   ; do segdef entry offset addition

; unresolved external, no fixup
gta_3:
    mov is_resolved,0       ; flag no fixup
    jmp NEAR PTR gta_ret    ; exit without further processing

; absolute, no segment partition or segdef entry offsets
gta_4:
    xor ax,ax
    mov is_relocatable,al   ; nonrelocatable segment
    mov targ_ent_ptr,ax     ; zero frame entry pointer
    inc al
    mov is_abspub,al        ; flag absolute public declaration
    mov ax,es:[2]           ; get frame number in target_segment in case of LOC frame fixup
    mov target_segment,ax
    pop es                  ; restore critical register
    ret

get_target_addr ENDP

;*****************************
;* GET_FRAME_ADDR            *
;*****************************

; compute frame for fixupp address
; upon entry al==0 if use frame data, al==1 if use target data
; uses frame_method,frame_index or target_method,target_index memory variables
; can reset is_resolved memory variable flag
; return frame in memory variable target_segment
; destroys ax,bx

get_frame_addr  PROC
    push    es              ; save critical register
    or  al,al               ; check if using frame data
    jne gfa_2               ; no
    mov ax,frame_index      ; get frame index
    mov bl,frame_method
    jmp SHORT gfa_3         ; bypass target code

gfa_2:
    mov ax,target_index     ; get target index
    mov bl,target_method
    and bl,3                ; mask off high bit to convert to segment/group/external value

gfa_3:
    dec ax                  ; make ax relative zero offset
    shl ax,1                ; convert ax to word offset in arrays
    or  bl,bl               ; check if segment index
    je  gfa_segind          ; yes
    jmp NEAR PTR gfa_4      ; no

; segment index
gfa_segind:
    mov bx,OFFSET DGROUP:seg_partent_indptr  ; set bx to base of segment partition address array
    add bx,ax               ; bx -> array element holding segment partition entry address
    mov es,[bx]             ; es -> segment partition entry

; check if absolute segment
    mov ax,es:[4]           ; zero if absolute segment (es -> absolute segdef entry)
    or  ax,ax
    jne gfa_spart_entry     ; not an absolute segment

; absolute segment, ax is zero
    mov is_relocatable,al   ; nonrelocatable segment
    mov frame_ent_ptr,ax    ; zero frame entry pointer
    inc al
    mov is_absseg,al        ; set absolute segment flag
    mov ax,es:[0]           ; get frame number
    mov target_segment,ax   ; save to memory variable
    pop es
    ret

gfa_spart_entry:
    test    BYTE PTR es:[15],80h    ; see if overlaid segment
    jne gfa_overlaid        ; yes

    mov ax,es:[4]           ; get master segdef entry
    mov es,ax               ; es -> segdef entry

    test    BYTE PTR es:[28],1  ; see if segment if overlay class
    je  gfa_3a              ; no
    cmp ovl_code_id,0       ; see if current L?DATA is owned by overlaid segment
    je  gfa_3a              ; no
    cmp nonovl_rvect,0      ; see if nonvector root calls flag set
    jne gfa_3a              ; yes, bypass vectoring

; special case to allow accessing overlay class nonoverlaid code address from
; an overlaid segment.  If target displacement is nonzero then assume
; that the program really wants the address and not a vectored address.

    mov ax,WORD PTR target_disp
    or  ax,WORD PTR target_disp+2   ; see if nonzero target displacement
    jne gfa_3a              ; yes

; if gen_flags bit 0 is set, then do no vector address changes
; Near/offset reference to different segment with no matching base references
; in fixup_ovl_extdef

    mov al,gen_flags
    and al,1                ; see if no vector address change flag set
    jne gfa_3a              ; yes

; frame is overlay class within an overlaid segment
; or frame is an overlaid segment
; es -> segment partition entry
gfa_overlaid:
    mov bx,es:[4]           ; get overlay identifier
    dec bx                  ; make relative zero
    shl bx,1                ; convert to word offset
    mov es,master_segblk    ; es:bx -> master segdef entry
    mov ax,es:[bx]
    mov frame_ent_ptr,ax    ; save segment owner

    mov ax,seg_fix_frame    ; use current segment fixup frame
    push    ax              ; save target segment value

; add segment byte value to target_prog_off so calculates back down to target displacement
    xor bx,bx               ; zero high word
    shl ax,1
    rcl bx,1                ; x2
    shl ax,1
    rcl bx,1                ; x4
    shl ax,1
    rcl bx,1                ; x8
    shl ax,1
    rcl bx,1                ; x16
    add WORD PTR target_prog_off,ax ; update target_prog_off variable
    adc WORD PTR target_prog_off+2,bx
    pop ax                  ; restore target segment value
    jmp SHORT gfa_save_seg  ; bypass nonoverlaid calculations

gfa_3a:
    mov frame_ent_ptr,ax    ; save pointer to frame entry
    mov ax,es:[2]           ; get low word of segment offset
    mov bx,es:[4]           ; get high word of segment offset
    shr bx,1
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1
    rcr ax,1                ; /16, have paragraph value of offset (segment value)

gfa_save_seg:
    mov target_segment,ax   ; save to memory variable
    jmp SHORT gfa_reloc

gfa_4:
    cmp bl,1                ; check if group index
    jne gfa_5               ; no

; group index
    mov bx,OFFSET DGROUP:grp_ent_indptr ; set bx to base of group entry array address
    add bx,ax               ; bx -> array element holding group entry address 
    mov es,[bx]             ; es -> group entry

gfa_group_entry:
    mov ax,es
    mov frame_ent_ptr,ax    ; save pointer to frame entry
    mov ax,es:[0]           ; get low word of group offset
    mov bx,es:[2]           ; get high word of group offset
    shr bx,1
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1
    rcr ax,1                ; /16, have paragraph value of offset (segment value)
    mov target_segment,ax   ; save to memory variable

gfa_reloc:
    mov al,1
    mov is_relocatable,al   ; relocatable segment (assume group is always relocatable)
    pop es
    ret

; external index
gfa_5:
    mov bx,OFFSET DGROUP:ext_defent_indptr  ; set bx to base of pubdef declarations entry address array
    add bx,ax               ; bx -> array element holding pubdef declarations entry address
    mov es,[bx]             ; es -> pubdef declarations entry

    or  BYTE PTR es:[14],40h    ; flag that used in fixup

    mov al,es:[14]          ; get definitions byte value
    and al,3                ; only keep pubdef/extdef/comdef/absolute field
    cmp al,2                ; check if unresolved external
    jb  gfa_unres_ext       ; unresolved external
    ja  gfa_abs             ; absolute public declaration

; pubdef declaration (not absolute)

; check if pubdef has a group associated with it
    mov al,es:[15]
    test    al,80h          ; high bit set if group associated with public declaration
    je  gfa_6               ; no group

    and al,1                ; see if overlaid public
    jne gfa_6               ; yes, flush group association

    mov al,1
    mov frext_ingroup,al    ; flag external has associated group
    mov es,es:[2]           ; es -> group entry
    jmp SHORT gfa_group_entry   ; perform code in common with group index

gfa_6:
    xor al,al
    mov frext_ingroup,al    ; flag external has no associated group
    mov es,es:[0]           ; es -> segment partition entry
    jmp NEAR PTR gfa_spart_entry     ; external resolved, perform code in common with segment index

; absolute public declaration
gfa_abs:
    xor ax,ax
    mov is_relocatable,al   ; nonrelocatable segment
    mov frame_ent_ptr,ax    ; zero frame entry pointer
    inc al
    mov is_abspub,al        ; flag absolute public declaration
    mov ax,es:[2]           ; get frame number of public entry
    mov target_segment,ax   ; save to target segment
    pop es
    ret

; unresolved external, no fixup
gfa_unres_ext:
    mov is_resolved,0       ; flag no fixup
    pop es                  ; restore critical register
    ret

get_frame_addr  ENDP

;*****************************
;* SAVE_THREAD               *
;*****************************

; parse and save thread information
; updates si record pointer,cx record length
; destroys ax,bx,dx,di

save_thread PROC
    mov fixup_pos,si        ; keep -> fixup in case of error
    mov bl,es:[si]          ; get thread data byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  st_2                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
st_2:
    mov al,bl               ; get thread field
    and al,1ch              ; break out thread method
    shr al,1
    shr al,1                ; adjust method value to relative zero (right justify bit field)
    mov thread_method,al    ; save thread method to memory variable
    cmp al,3                ; check if explicit frame number type, unsupported
    jne st_3                ; no

; bad method field
st_bad_meth:
    mov cl,al
    mov ax,INV_THRD_METH_ERR    ; invalid thread method field value
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

st_3:
    mov bh,bl               ; get thread data byte in bh
    and bh,40h              ; get D field bit
    je  st_4                ; D bit reset

; D bit set
    cmp al,5                ; check if invalid method value
    jbe st_5                ; okay
    jmp SHORT st_bad_meth   ; bad value

; D bit reset
st_4:
    cmp al,2                ; check if invalid method value
    ja  st_bad_meth         ; bad value

st_5:
    xor ah,ah               ; zero high byte of thread index
    mov al,es:[si]          ; get thread index byte(s)
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  st_6                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
st_6:
    cmp al,80h              ; see if two byte field
    jb  st_7                ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte

    mov al,es:[si]          ; get thread index second byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  st_7                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

st_7:
    mov thread_index,ax     ; save thread index value
    push    bx              ; save thread data byte in bl
    mov ax,thread_index     ; get thread index for check_index
    mov bl,thread_method
    call    check_index     ; check index validity
    pop bx                  ; restore thread data byte in bl
    mov dl,bl
    and dl,3                ; get thread number in dl
    xor dh,dh               ; zap high byte
    and bl,40h              ; get D bit field value in bl
    je  st_8                ; D bit reset, target thread field

; frame thread field
    mov bx,OFFSET DGROUP:frame_thrd_meth    ; bx -> base of frame thread method array
    mov di,OFFSET DGROUP:frame_thrd_index   ; di -> base of frame thread index array

st_array_save:
    add bx,dx               ; bx -> proper array element (thread number'th)
    mov al,thread_method
    mov [bx],al             ; save method type in array
    shl dx,1                ; convert dx to word offset
    add di,dx               ; di -> proper array element (thread number'th)
    mov ax,thread_index
    mov [di],ax             ; save index type to array
    ret

; target thread field
st_8:
    mov bx,OFFSET DGROUP:target_thrd_meth   ; bx -> base of target thread method array
    mov di,OFFSET DGROUP:target_thrd_index  ; di -> base of target thread index array
    jmp SHORT st_array_save ; save thread method and index to arrays

save_thread ENDP 

END
