;*********************************************************************
;*   MLPASS1A.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          05/10/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.7                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker pass 1 routines part A                                   *
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
PUBLIC  pass1_obj_proc,get_pubdecl_entry,add_pubdef_name
PUBLIC  make_pubdecl_blk
PUBLIC	proc_cextdef

; variables
PUBLIC  parse_complete,name_field
;*** PUBLIC	local_comm_count
PUBLIC  segment_rec_index,group_rec_index,frame_number
PUBLIC  com_val1,com_val2,libobj_flag,first_pdnameblk_ptr
PUBLIC	def_lib_flag,mod_ovl_count,ovl_entry_id
PUBLIC	known_clipper,maybe_clipper,symbol_overflow
PUBLIC	clipper_segdef_ptr,clipper_symseg_ptr,clipper_segindex
PUBLIC	first_clipmod_ptr,current_clipmod_ptr
PUBLIC	unique_symbol_count,prev_symbol_count,module_symbol_count
PUBLIC	is_summer87,is_clipper5
PUBLIC	is_local,is_cextdef
PUBLIC	ignore_fixupp_flag

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   filename:BYTE
EXTRN   in_overlay:BYTE,ovlpub_array_seg:WORD
EXTRN   curseg_nearovlpub_count:WORD,curseg_farovlpub_count:WORD
EXTRN	writing_qlk_flag:BYTE
EXTRN	must_parse_flag:BYTE,clip_fix_compress:BYTE
EXTRN   mod_alloc_base:WORD

; initialized local variables

; byte values
EVEN                        ; maximize speed on 8086 and better
def_lib_flag    DB  0       ; nonzero if default library from COMENT record
maybe_clipper	DB	0		; nonzero if module may be Clipper compiled
known_clipper	DB	0		; nonzero if current module is known Clipper module
is_clipper5		DB	0		; nonzero if any known Clipper 5.x program
is_summer87		DB	0		; nonzero if any known Clipper S'87 program
symbol_overflow	DB	0		; nonzero if symbol table compaction overflow

; word values
EVEN
first_pdnameblk_ptr DW  0   ; segment of last allocated pubdef names block
clipper_segdef_ptr	DW	0	; current module clipper SYMBOLS segdef pointer
clipper_symseg_ptr	DW	0	; first module SYMBOLS segdef pointer
clipper_segindex	DW	0	; current module SYMBOLS segment index, relative 0
first_clipmod_ptr	DW	0	; pointer to first clipper module entry
current_clipmod_ptr	DW	0	; pointer to current clipper module entry
module_symbol_count	DW	0	; clipper module current symbol count (from SYMBOLS)
unique_symbol_count	DW	0	; total count of unique SYMBOLS symbols up to overflow
prev_symbol_count	DW	0	; previous to current module total count of unique SYMBOLS symbols

.DATA?

; uninitialized local variables

; byte values
EVEN                        ; maximize speed on 8086 and better
parse_complete  DB  ?       ; nonzero if record parsing is complete for object module
EVEN
;***theadr_count    DB  ?       ; count of THEADR records in object module
EVEN
which_def       DB  ?       ; 1 if current record is pubdef record, 2 if comdef, 0 otherwise
EVEN
name_field      DB  256 DUP (?) ; ASCIIZ temporary storage of public, external, etc. name from record
EVEN
segment_rec_index   DB  ?   ; segment record index in pubdef record
EVEN
group_rec_index DB  ?       ; group record index in pubdef record
EVEN
near_communal   DB  ?       ; near communal flag
EVEN
libobj_flag     DB  ?       ; nonzero if lib being linked as obj
EVEN
is_local    DB  ?           ; nonzero if local EXTDEF
is_cextdef	DB	?			; nonzero if COMDAT EXTDEF
ignore_fixupp_flag	DB	?	; nonzero if should ignore following fixupp (LEDATA32)

; word values

EVEN
frame_number    DW  ?       ; pubdef record frame_number
;***local_comm_count    DW  ?   ; count of true local communal variables for module
IFNDEF JUNIOR
mod_ovl_count   DW  ?       ; count of overlaid segments in module
ovl_entry_id    DW  ?       ; value of ovl_code_id upon module entry
ENDIF

; doubleword values
com_val1    DD  ?           ; communal field value holder
com_val2    DD  ?           ; communal field value holder

;*****************************
;* Constant data             *
;*****************************

.CONST

summer87text	DB	'SUMMER87',0	; EXTDEF name if Clipper S'87 module
;clipper5text	DB	'CLIPPER501',0	; EXTDEF name if Clipper 5.01 module
clipper5text	DB	'CLIPPER5'		; EXTDEF name if Clipper 5.01 module
clipper5text2	DB	'C50R100',0		; EXTDEF name if Clipper 5.0 module

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   link_error:NEAR,dos_error:NEAR,get_name:NEAR,load_file:NEAR
EXTRN   get_hash:NEAR,alloc_memory:NEAR,link_warning:NEAR
EXTRN   proc1_lnames:NEAR,proc1_segdef:NEAR,proc1_grpdef:NEAR
EXTRN   make_symbols:NEAR,check_extension:NEAR,save_lib_name:NEAR
EXTRN   error_bx_pos:NEAR,error_read_buff_pos:NEAR
EXTRN   scan_comfield:NEAR,compute_comm_len:NEAR
;*** EXTRN   proc_locals:NEAR,zero_local_blocks:NEAR,save_local_comm:NEAR
EXTRN   chk_pub_in_ovl:NEAR
EXTRN   proc1_datarec:NEAR,proc1_fixupp:NEAR
EXTRN   flush_publics:NEAR
;*** EXTRN	proc2_lextdef:NEAR
EXTRN   update_incinfo:NEAR,check_cliplib:NEAR
EXTRN	create_clipmod_entry:NEAR,clip_final_compress:NEAR
EXTRN	write_qlk_modsize:NEAR

IFNDEF DEMO
EXTRN   save_module_type:NEAR
EXTRN	write_ddl_module:NEAR
ENDIF

;*****************************
;* PASS1_OBJ_PROC            *
;*****************************

; parse and process records in object modules
; all registers used or destroyed

pass1_obj_proc  PROC
    xor si,si               ; si points into buffer for parsed chars
    mov ax,si
    mov libobj_flag,al
    mov ax,buffer_tail      ; get buffer_tail variable in register for quick compares
    mov bp,ax               ; bp holds variable value

; entry point to process next module if more than one in specified OBJ
p1_proc_next:
    inc module_count        ; bump count of total modules (for DDL use)

IFNDEF DEMO
    cmp any_ddl,0           ; see if creating or using DDL
    je  pop_init            ; no
    call    save_module_type    ; save the type of module (required/elective, root/overlaid)
ENDIF

pop_init:
    xor ax,ax
    mov parse_complete,al   ; init flag indicating record parsing is complete for object module
	mov	maybe_clipper,al	; init flag for Clipper symbol table compression
;***    mov local_comm_count,ax ; init count of local communal variables
    mov current_lnames,ax   ; init count of current record types
    mov current_extdef,ax
    mov current_segdef,ax
    mov current_grpdef,ax
    mov seg_ovlclass,al     ; init overlay class segment in module flag
	mov	known_clipper,al
	mov	must_parse_flag,al	; init symbol compression flags
	mov	clip_fix_compress,al
	mov	ignore_fixupp_flag,al
    mov _dt_seg_size,ax     ; init clarion data segments
    mov _dat_seg_size,ax
    mov mod_ovl_count,ax    ; init count of overlaid segments in module
    mov curseg_nearovlpub_count,ax  ; init overlay variables
    mov curseg_farovlpub_count,ax
	mov	module_symbol_count,ax	; init current module clipper symbol count
	mov	ax,unique_symbol_count	; save previously unique symbol count in case of restore on overflow
	mov	prev_symbol_count,ax
    mov ax,-1
    mov ovlpub_array_seg,ax
    mov ax,ovl_code_id
    mov ovl_entry_id,ax     ; save ovl_code_id entry value
;***    call    zero_local_blocks   ; zero any pre-existing local blocks (don't carry locals to next module)

    cmp udl_proc_pass,1     ; see if udl processing pass
    je  p1_proc_loop        ; yes, no base reset

    mov ax,allocation_base
    mov mod_alloc_base,ax   ; save base of allocations for this module

p1_proc_loop:
    mov read_buff_ptr,si    ; keep beginning of object record
    mov ax,buffer_base      ; get i/o buffer segment
    mov es,ax               ; es -> i/o buffer
    mov al,es:[si]          ; get record type

pop_2:
    inc si                  ; bump si past record type
    cmp si,bp               ; check boundary conditions
    jb  pop_2a              ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pop_2a:
    mov cl,es:[si]          ; get record length low byte
    inc si
    cmp si,bp               ; check boundary conditions
    jb  pop_2c              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pop_2c:
    mov ch,es:[si]          ; get record length high byte
    inc si                  ; bump si past length word
    cmp si,bp               ; check boundary conditions
    jb  pop_2e              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

; check that record doesn't exceed file length
pop_2e:
    cmp eof_flag,0          ; see if end of file encountered
    je  len_chk             ; nope
    mov dx,cx               ; get record length
    add dx,si               ; add in current position
    jc  unexpected_eof      ; overflow, record file length exceeded

len_chk:
    cmp cx,1                ; see if null record
    jbe to_update_pos       ; yes, ignore it

pop_pubdef:
    cmp al,PUBDEF           ; is it a PUBDEF record
    jne pop_extdef

pop_pubproc:
    call    proc1_pubdef    ; process the record
    jmp NEAR PTR parse_chk

unexpected_eof:
    mov dx,OFFSET DGROUP:filename
    mov ax,UNEXPECT_EOF_ERR ; unexpected end of file
    jmp NEAR PTR link_error ; transfer control to error handler

pop_extdef:
    cmp al,EXTDEF           ; is it an EXTDEF record
    jne pop_cextdef

pop_extproc:
    call    proc1_extdef    ; process the record
    jmp NEAR PTR update_pos

pop_cextdef:
	cmp	al,CEXTDEF			; see if COMDAT EXTDEF record
	je	pop_extproc			; yes

    cmp al,COMDEF           ; is it a COMDEF record
    jne pop_lnames
pop_comproc:
    call    proc1_comdef    ; process the record
    jmp NEAR PTR parse_chk

pop_lnames:
    cmp al,LNAMES           ; is it a LNAMES record
    jne pop_segdef
    call    proc1_lnames    ; process the record
    jmp NEAR PTR parse_chk

pop_segdef:
    cmp al,SEGDEF           ; is it a SEGDEF record
    jne pop_grpdef
    call    proc1_segdef    ; process the record
    jmp NEAR PTR parse_chk

pop_grpdef:
    cmp al,GRPDEF           ; is it a GRPDEF record
    jne pop_coment
    call    proc1_grpdef    ; process the record
    jmp NEAR PTR parse_chk

pop_coment:
    cmp al,COMENT           ; is it a COMENT record
    jne pop_theadr
    call    proc1_coment    ; process the record

to_update_pos:
    jmp NEAR PTR update_pos

pop_theadr:
    cmp al,THEADR           ; is record a THEADR record
    jne pop_lheadr
pop_headr:
    call    proc1_theadr    ; process the record
    jmp NEAR PTR update_pos
pop_lheadr:
    cmp al,LHEADR           ; is record a LHEADR record
    je  pop_headr           ; process the record (LHEADR processes same as THEADR)

pop_modend:
    cmp al,MODEND           ; is it a MODEND record
    jne pop_mslhed
    mov parse_complete,al   ; al is known nonzero value, put it in parse_complete flag

; check if need to compress out clipper symbols
	cmp	must_parse_flag,0
	je	pop_nocomp
	call	clip_final_compress	; do clipper symbol compression code

pop_nocomp:
    cmp is_clpinc,0         ; see if clipper incremental link in force
    je  pop_chkddl          ; no
    call    update_incinfo  ; update the incremental link info

pop_chkddl:
IFNDEF DEMO
    cmp any_ddl,0           ; see if DDL's used
    je  to_update_pos       ; no
    call    write_ddl_module    ; write DDL module info
ENDIF
    jmp NEAR PTR update_pos

pop_mslhed:
    cmp al,MSLHED           ; make library header records valid
    jne pop_11
    mov libobj_flag,al      ; set library as object flag
    jmp NEAR PTR update_pos

pop_11:
    cmp al,LCOMDEF          ; check if local COMDEF
    jne pop_11a             ; no
	jmp	SHORT pop_comproc

to_pop_pubproc:
    jmp NEAR PTR pop_pubproc

pop_11a:
    cmp al,LPUBDEF1         ; check if local PUBDEF records
	je	to_pop_pubproc
    cmp al,LPUBDEF2
	je	to_pop_pubproc

pop_ledata:
    cmp al,LEDATA           ; check if a LEDATA record
    jne pop_lidata          ; no
pop_datarec:
    call    proc1_datarec   ; process the data record
    jmp SHORT update_pos

pop_lidata:
    cmp al,LIDATA           ; check if a LIDATA record
    je  pop_datarec         ; yes, process as LEDATA record

    cmp al,LEXTDEF          ; check if a local EXTDEF record
    jne pop_fixupp          ; no
	jmp	NEAR PTR pop_extproc

pop_fixupp:
    cmp al,FIXUPP           ; check if a FIXUPP record
    jne pop_ledata32		; no
    call    proc1_fixupp    ; process the fixup
    jmp SHORT update_pos

pop_ledata32:
    cmp al,LEDATA32			; check if a LEDATA32 record
    jne pop_valid			; no
	mov	ignore_fixupp_flag,1	; flag ignore following fixupp
    jmp SHORT update_pos

pop_valid:
    cmp al,LINNUM
    je  update_pos
    cmp al,LOCSYM
    je  update_pos
    cmp al,BLKDEF
    je  update_pos
    cmp al,BLKEND
    je  update_pos
    cmp al,DEBSYM
    je  update_pos
    cmp al,SEGDEF32
    je  update_pos

    cmp al,TYPDEF
    je  update_pos

bad_rectype:
    mov dx,OFFSET DGROUP:filename
    mov cl,al               ; value in cx
    xor ch,ch               ; zap high byte
    mov ax,BAD_RECTYPE_ERR  ; bad record type
    jmp NEAR PTR error_read_buff_pos    ; transfer control to error handler

update_pos:                 ; update si to point past record
    mov dx,si               ; save current position prior to update
    add si,cx               ; scan past checksum
    jnc pop_12              ; no overflow

pop_13:
    mov si,dx               ; restore si to current position
    mov ax,bp               ; get overflow point
    sub ax,si               ; get difference from current position to overflow
    mov si,bp               ; put si at overflow point
    sub cx,ax               ; update cx to account for changed si position
    call    load_file       ; load next portion of file into buffer, at end position
    jmp SHORT update_pos    ; loop back for next attempt

pop_12:
    cmp si,bp               ; check boundary conditions
    jae pop_13              ; out of bounds

parse_chk:
    mov al,parse_complete
    or  al,al               ; see if parsing should continue
    jne pop_done            ; parsing complete, return
    jmp NEAR PTR p1_proc_loop   ; loop for next record

pop_done:
    xor ax,ax

IFNDEF DEMO
    cmp any_ddl,al          ; see if creating/using DDL's
    je  pop_chklocal        ; no
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne pop_chkmod          ; no, bypass local processing for DDLs
ENDIF

pop_chklocal:

COMMENT # **********
    cmp local_comm_count,ax ; see if any local communal variables
    je  pop_done2           ; no
    call    save_local_comm ; save local communals in comdef declarations for memory allocation
********** END COMMENT #

pop_done2:
    mov ax,curseg_nearovlpub_count
    or  ax,curseg_farovlpub_count   ; see if any overlaid publics not flushed to table
    je  pop_done3           ; no
    call    flush_publics

pop_done3:
    xor ax,ax
    cmp is_inlib,al         ; see if processing library
	je	pop_chkmod			; no

	cmp	writing_qlk_flag,0	; see if writing to QLK file
	je	pop_ret				; no
	call	write_qlk_modsize	; write size of library module to QLK file
	ret

pop_chkmod:
    cmp libobj_flag,al      ; see if library treated as object module
    je  pop_ret             ; no

    mov al,es:[si]          ; get next record type
    cmp al,THEADR           ; see if start of new object module
    je  pop_more            ; yes
    cmp al,LHEADR           ; see if start of new object module
    je  pop_more            ; yes

    or  al,al               ; see if zero (padding)
    jne pop_ret             ; no
    mov cx,1                ; setup to eat one byte
    jmp SHORT update_pos    ; loop back

pop_ret:
    ret

pop_more:
    jmp NEAR PTR p1_proc_next   ; get next module in file

pass1_obj_proc  ENDP

;*****************************
;* PROC1_THEADR              *
;*****************************

; process THEADR record
; upon entry cx=record length,bp=buffer_end,al=record type
; si -> first byte of record past record length
; destroys ax
; updates si

proc1_theadr    PROC
	mov	is_local,0			; reset local flag for get_name procedure

    mov di,OFFSET DGROUP:tmod_name  ; di -> field to place name
    call    get_name        ; get and save T-module name

; kill check for multiple THEADRs, MSC 6.0 can generate them

pt_ret:
    ret                     ; all done
proc1_theadr    ENDP

;*****************************
;* PROC1_PUBDEF              *
;*****************************

; process PUBDEF record
; upon entry cx=record length,bp=buffer_end,al=record type
; si -> first byte of record past record length
; destroys ax,bx,dx,di,es
; updates si

proc1_pubdef    PROC
    xor ah,ah
    mov is_local,ah         ; init local flag
    cmp al,LPUBDEF1         ; see if local
    je  pp_local            ; yes
    cmp al,LPUBDEF2         ; check if other local type
    jne pp_1                ; no

pp_local:
    mov is_local,4          ; set local flag

pp_1:
    mov dl,es:[si]          ; get low byte of group_index
    cmp dl,GRPDEF_MAX       ; exceeded maximum
    jbe pp_2                ; no

; group index too large error
pp_bad_grpind:
    mov cl,dl               ; get value in cl
    mov dx,OFFSET DGROUP:filename
    mov ax,GRPDEF_VAL_ERR   ; group index value too high
    jmp NEAR PTR link_error ; transfer control to error handler

pp_2:
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pp_3                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pp_3:
    mov ax,current_grpdef   ; get count of current grpdef records in object module
    cmp dl,al               ; compare to given group index value
    ja  pp_bad_grpind       ; out of bounds

pp_4:
    mov dh,es:[si]          ; get first byte of segment index
    mov bx,si               ; keep offset pointer to index in case of error
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pp_5                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pp_5:
    cmp dh,80h              ; check if high bit set
    jb  pp_7                ; no, only 1 byte index

; two byte index, dh contains high-order byte with high bit set
    je  pp_6a               ; if greater than 0x80 then segment index > 255, out of bounds

pp_bad_segind:
    mov cl,dh               ; get value in cl
    mov dx,OFFSET DGROUP:filename
    mov ax,SEGDEF_VAL_ERR   ; segment index greater than 255 maximum, or higher than count of segdef records
    jmp NEAR PTR error_bx_pos   ; transfer control to error handler

pp_6a:
    mov dh,es:[si]          ; get second byte of segment index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pp_7                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

; dh contains segment index value, dl contains group index value
pp_7:
    mov ax,current_segdef   ; get count of current segdef records in object module
    cmp dh,al               ; compare to given segment index value
    ja  pp_bad_segind       ; out of bounds

    or  dh,dh               ; see if nonzero segment index
    jne pp_8                ; yes
    or  dl,dl               ; zero segment index, see if zero group index
    je  pp_8                ; yes

; group and segment index values conflict (segment index zero, group index nonzero)
    mov dx,OFFSET DGROUP:filename
    mov ax,SEG_GRP_CONF_ERR ; PUBDEF group and segment index values conflict
    jmp NEAR PTR error_bx_pos   ; transfer control to error handler

pp_8:
    xor bx,bx               ; zero bx, will contain frame number, if any
    mov al,dl
    or  al,dh               ; check if both segment and group index value are zero
    jne pp_12               ; nope

; record contains a frame number
    mov bl,es:[si]          ; get low byte of frame number
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pp_11               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pp_11:
    mov bh,es:[si]          ; get high byte of frame number
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pp_12               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

; bx contains frame number, zero if none
; dh holds segment index value, dl holds group index value
pp_12:
    mov segment_rec_index,dh    ; save values to memory variables
    mov group_rec_index,dl
    mov frame_number,bx
    cmp cx,1                ; see if empty PUBDEF
    ja  pp_loop             ; no
    jmp NEAR PTR pp_out     ; yes

pp_loop:
    mov di,OFFSET DGROUP:name_field ; point di at field to place name
    call    get_name        ; get public name from record

    mov bl,es:[si]          ; get low byte of public offset
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pp_14               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pp_14:
    mov bh,es:[si]          ; get high byte of public offset
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pp_15               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

; gobble the type index value
pp_15:
    mov al,es:[si]          ; get low byte of type index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_15a              ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_15a:
    cmp al,80h              ; check if more than one byte index value
    jb  pp_16               ; no, continue
    inc si                  ; scan past second byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pp_16               ; okay
    call    load_file

pp_16:
    mov di,OFFSET DGROUP:name_field ; point di at name field for get_pubdecl_entry procedure
    mov al,1                ; flag to get_pubdecl_entry procedure that current record is a pubdef record
    call    get_pubdecl_entry   ; find or create public declaration entry segment
    or  ax,ax               ; check if it was a duplicate definition (ax==0)
    jne pp_17               ; nope
    jmp NEAR PTR pp_checksum    ; check for end of record, loop for another

pp_17:
    push    ax              ; save public declaration entry segment

    mov di,OFFSET DGROUP:name_field ; point di at name field for add_pubdef_name procedure
    call    add_pubdef_name ; add name to pubdef names block

    mov di,es               ; save es
    pop es                  ; es -> public declaration entry
    and BYTE PTR es:[15],09fh   ; reset communal bit flags
    mov es:[4],ax           ; save offset pointer to name in pubdef names block
    mov al,is_local
    or  BYTE PTR es:[15],al ; set local flags (if DDL)
    mov es:[6],dx           ; save segment pointer to name in pubdef names block
    mov es:[8],bx           ; save public offset

    mov al,group_rec_index
    or  al,al               ; see if group index for this pubdef
    je  pp_17a              ; no

    or  BYTE PTR es:[15],80h    ; flag that group is associated with public declaration

;get pointer to group entry in ax
    xor ah,ah               ; zap high byte

IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDL's
    jne pp_17b              ; yes, use group index value
ENDIF

    mov bx,offset DGROUP:grp_ent_indptr ; bx -> base of group index pointer array
    dec al                  ; make segment index value relative zero
    shl ax,1                ; make group index value a word offset into index pointer array
    add bx,ax               ; bx -> proper group index array element
    mov ax,[bx]             ; get segment pointer to group entry
    jmp SHORT pp_17b        ; save pointer to group entry

pp_17a:
    mov ax,frame_number     ; get frame number

pp_17b:
    mov es:[2],ax           ; save frame number/pointer to group entry

pp_nofrptr:
    mov al,segment_rec_index
    or  al,group_rec_index  ; al is zero only if frame number (absolute public)
    jne pp_not_abs

; absolute public declaration
    mov BYTE PTR es:[14],3  ; flag that it was an absolute public entry
    jmp SHORT pp_17c

pp_not_abs:
	mov	al,es:[4]
	and	al,20h				; save clipper symbol bit
	add	al,2				; flag as pubdef entry
    mov BYTE PTR es:[14],al	; flag that it was a pubdef entry

pp_17c:
    mov dl,segment_rec_index
    or  dl,dl               ; was there a segment index
    je  pp_18               ; no, bypass segment partition entry code

    xor dh,dh               ; zap high byte
    mov bx,offset DGROUP:seg_defent_indptr  ; set bx to base of segment index pointer array
    dec dl                  ; make segment index value relative zero
    shl dx,1                ; make segment index value a word offset into index pointer array
    add bx,dx               ; bx -> proper segment index array element
    mov ax,[bx]             ; get segment pointer to segdef entry
    mov bx,es               ; save es -> pubdef entry
    mov es,ax               ; es -> segdef entry
    mov ax,es:[24]          ; get pointer to last segment partition entry
    mov es,bx               ; restore es -> pubdef entry
    mov es:[0],ax           ; save segment partition entry pointer

; see if public is in an overlay, zero group entry pointer if necessary
    call    chk_pub_in_ovl  ; set overlay flag if in overlay

IFNDEF DEMO
    cmp any_ddl,0           ; see if creating/using DDL
    je  pp_18               ; no
    mov al,segment_rec_index
    xor ah,ah
    mov es:[0],ax           ; using DDL's, save segment index instead of pointer
ENDIF

pp_18:
    mov es,di               ; restore es
    add WORD PTR pub_sym_count,1    ; update low word count
    adc WORD PTR pub_sym_count+2,0  ; add in carry from low word, if any

pp_checksum:
    cmp cx,1                ; check if at checksum byte
    jbe pp_out              ; yes, gobble it and return
    jmp NEAR PTR pp_loop    ; loop back for next definition

pp_out:
    inc si                  ; bump past checksum byte
    cmp si,bp               ; check boundary conditions
    jb  pp_ret              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pp_ret:
    ret
proc1_pubdef    ENDP

;*****************************
;* PROC1_EXTDEF              *
;*****************************

; process EXTDEF record
; upon entry cx=record length,bp=buffer_end,al=record type
; si -> first byte of record past record length
; updates si
; destroys ax,bx,dx,di

proc1_extdef    PROC
    xor ah,ah
    mov is_local,ah         ; init local flag
	mov	is_cextdef,ah		; init comdat extdef flag
    cmp al,LEXTDEF          ; see if local
    jne pe_1				; no
    mov is_local,4          ; set local flag

pe_1:
    cmp al,CEXTDEF          ; see if COMDAT extdef
    jne pe_loop				; no
	mov	is_cextdef,al		; yes, flag it

pe_loop:
	cmp	is_cextdef,0		; see if COMDAT extdef
	je	pe_notcext			; no
	call	proc_cextdef	; parse COMDAT extdef entry
	jmp	SHORT pe_2a			; process regular extdef parsing

pe_notcext:
    mov di,OFFSET DGROUP:name_field ; point di at field to place name
    call    get_name        ; get public name from record

; see if maybe_clipper set and !known_clipper and !overflowed symbol table,
; if so, check if name is SUMMER87 or CLIPPER5, 
; if so set known_clipper and is_summer87 or
; is_clipper5 variables appropriately
		xor	al,al
	cmp	maybe_clipper,al	; check if symbol table compression on and possible clipper module
	je	pe_gettype			; no
	cmp	known_clipper,al	; see if module already known clipper
	jne	pe_gettype			; yes
	cmp	symbol_overflow,al	; see if overflowed symbol table
	jne	pe_gettype			; yes

	push	es				; save critical registers
	push	si
	push	cx

	push	ds
	pop	es
	mov	al,1
	mov	di,OFFSET DGROUP:summer87text
	mov	si,OFFSET DGROUP:name_field
	mov	cx,9				; nine chars in name, including null terminator
	repe	cmpsb			; compare extdef name to summer87
	jne	pe_chk5				; didn't match

; clipper summer87 module
	mov	is_summer87,al		; flag as such
	jmp	SHORT pe_clipmod

pe_chk5:
	mov	di,OFFSET DGROUP:clipper5text
	mov	si,OFFSET DGROUP:name_field
;	mov	cx,11				; eleven chars in name
	mov	cx,8				; eight chars in name
	repe	cmpsb			; compare extdef name to clipper5
	je	pe_isclip5			; is clipper5

	mov	di,OFFSET DGROUP:clipper5text2
	mov	si,OFFSET DGROUP:name_field
	mov	cx,8				; eight chars in name
	repe	cmpsb			; compare extdef name to c50r100
	jne	pe_vardone			; didn't match

; clipper 5 module
pe_isclip5:
	mov	is_clipper5,al		; flag as such

pe_clipmod:
	mov	known_clipper,al	; flag known clipper module
	call	create_clipmod_entry	; create clipper module entry

pe_vardone:
	pop	cx					; restore critical registers
	pop	si
	pop	es

pe_gettype:
    mov al,es:[si]          ; get type index byte
    inc si                  ; scan past first type index byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pe_2                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pe_2:
    and al,80h              ; see if two-byte type index 
    je  pe_2a               ; no
    inc si                  ; scan past second type index byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pe_2a               ; okay
    call    load_file

pe_2a:
    mov di,OFFSET DGROUP:name_field ; point di at name field for get_pubdecl_entry procedure
    xor al,al               ; flag to get_pubdecl_entry procedure that current record is an extdef record
    call    get_pubdecl_entry   ; find or create public declaration entry segment

; keep public declaration segment in ext_defint_indptr array if overlays
    or  ax,ax               ; see if returned ax was null
    je  pe_2b               ; yes, use returned value in dx
    mov dx,ax               ; no, set dx to ax return value
pe_2b:
    mov bx,OFFSET DGROUP:ext_defent_indptr    ; bx -> base of array pointers to extdef'ed public entries
    mov di,current_extdef   ; get array element (current extdef number)
    shl di,1                ; make di a word offset
    add bx,di               ; bx -> proper array element
    mov [bx],dx             ; save pointer to public declaration entry

IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDL's
    je  pe_noddl            ; no

; using DDLs, set bit 1 (2) in general flag in case external was
; later declared in PUBDEF
    mov di,es
    mov es,dx               ; es -> public entry
    or  BYTE PTR es:[15],2h ; set external flag
    mov es,di
ENDIF

pe_noddl:
    or  ax,ax               ; check if it was a duplicate declaration (ax==0)
    je  pe_checksum         ; yes, already declared

pe_3:
    push    ax              ; save public declaration entry segment
    mov di,OFFSET DGROUP:name_field ; point di at name field for add_pubdef_name procedure
    call    add_pubdef_name ; add name to pubdef names block

    mov di,es               ; save es
    pop es                  ; es -> public declaration entry
    mov es:[4],ax           ; save offset pointer to name in pubdef names block
    mov es:[6],dx           ; save segment pointer to name in pubdef names block
    mov BYTE PTR es:[14],81h    ; flag as extdef entry, unknown strong or weak
    mov al,is_local
    or  BYTE PTR es:[15],al ; set local flag (if using DDLs)
    mov es,di               ; restore es

pe_checksum:
    inc current_extdef      ; update global memory variable
    cmp cx,1                ; check if at checksum byte
    jbe pe_ret              ; yes, return
    jmp NEAR PTR pe_loop    ; loop back for next definition

pe_ret:
    ret
proc1_extdef    ENDP

IFNDEF JUNIOR

;*****************************
;* PROC1_COMDEF              *
;*****************************

; process COMDEF record
; upon entry cx=record length,bp=buffer_end,al=record type
; si -> first byte of record past record length
; updates si
; destroys ax,bx,dx,di

proc1_comdef    PROC
    xor ah,ah
    mov is_local,ah         ; init local flag
    cmp al,LCOMDEF          ; see if local
    jne pcf_loop            ; no
    mov is_local,20h        ; set local flag

pcf_loop:
    xor ax,ax
    mov WORD PTR com_val1,ax    ; init communal value variables
    mov WORD PTR com_val1+2,ax
    mov WORD PTR com_val2,ax
    mov WORD PTR com_val2+2,ax
    mov di,OFFSET DGROUP:name_field ; point di at field to place name
    call    get_name        ; get public name from record

    mov al,es:[si]          ; get type index byte
    inc si                  ; scan past first type index byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pcf_2               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pcf_2:
    and al,80h              ; see if two-byte type index 
    je  pcf_2a              ; no
    inc si                  ; scan past second type index byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pcf_2a              ; okay
    call    load_file

pcf_2a:
    mov dl,es:[si]          ; get data segment type
    mov bx,si               ; save offset in case of error
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pcf_2b              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pcf_2b:
    mov dh,es:[si]          ; get low byte of variable size (NEAR)/number of elements (FAR)
    cmp dl,61h              ; check if FAR variable
    jne pcf_3
    mov bx,si               ; save offset in case of error
    mov di,OFFSET DGROUP:com_val1   ; di -> first communal value field
    call    scan_comfield   ; scan past communal length field number of elements
    mov dh,es:[si]          ; get low byte of element size
    mov bx,si               ; save offset in case of error
    mov di,OFFSET DGROUP:com_val2   ; di -> second communal value field
    call    scan_comfield   ; scan past communal length field element size
    call    compute_comm_len    ; compute communal length, return in com_val1
    mov near_communal,0h    ; reset near communal flag bit
    jmp SHORT pcf_5         ; bypass NEAR variable code

; invalid data segment type, neither 61h or 62h
pcf_invalid:
    mov cl,dl               ; get value in cl
    mov dx,OFFSET DGROUP:filename
    mov ax,INV_DSTYPE_ERR   ; invalid data segment type value
    jmp NEAR PTR error_bx_pos   ; transfer control to error handler

pcf_3:
    cmp dl,62h              ; check if NEAR variable
    jne pcf_invalid         ; no, invalid communal variable type

pcf_4:
    mov bx,si               ; save offset in case of error
    mov di,OFFSET DGROUP:com_val1   ; di -> first communal value field
    call    scan_comfield   ; scan past communal length field variable size
    mov near_communal,10h   ; set near communal flag bit

pcf_5:
    mov di,OFFSET DGROUP:name_field ; point di at name field for get_pubdecl_entry procedure
    mov al,2                ; flag to get_pubdecl_entry procedure that current record is an comdef record
    call    get_pubdecl_entry   ; find or create public declaration entry segment

    or  ax,ax               ; see if returned ax was null
    je  pcf_dup             ; yes, use returned value in dx
    mov dx,ax               ; no, set dx to ax return value
pcf_dup:
    mov bx,OFFSET DGROUP:ext_defent_indptr    ; bx -> base of array pointers to extdef'ed public entries
    mov di,current_extdef   ; get array element (current extdef number)
    shl di,1                ; make di a word offset
    add bx,di               ; bx -> proper array element
    mov [bx],dx             ; save pointer to public declaration entry

IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDL's
    je  pcf_noddl           ; no

; using DDLs, set bit 1 (2) in general flag in case comdef was
; later declared in PUBDEF
    mov di,es
    mov es,dx               ; es -> public entry
    or  BYTE PTR es:[15],2h ; set external flag
    mov es,di
ENDIF

pcf_noddl:
    or  ax,ax               ; check if it was a duplicate declaration (ax==0)
    je  pcf_checksum        ; yes, already declared

    inc communal_count      ; bump count of communal variables
    add WORD PTR pub_sym_count,1    ; update low word count
    adc WORD PTR pub_sym_count+2,0  ; add in carry from low word, if any

    push    ax              ; save public declaration entry segment

    cmp which_def,3         ; see if extdef changed to comdef
    je  pcf_5a              ; yes, bypass name addition

    mov di,OFFSET DGROUP:name_field ; point di at name field for add_pubdef_name procedure
    call    add_pubdef_name ; add name to pubdef names block

pcf_5a:
    mov di,es               ; save es
    pop es                  ; es -> public declaration entry

    cmp which_def,3         ; see if extdef changed to comdef
    je  pcf_5b              ; yes, bypass name update

    mov es:[6],dx           ; save segment pointer to name in pubdef names block
    mov es:[4],ax           ; save offset pointer to name in pubdef names block

pcf_5b:
    or  BYTE PTR es:[15],40h    ; flag as communal without public declaration
    mov al,is_local
    or  BYTE PTR es:[15],al ; set local flag (if using DDLs)
    mov al,near_communal
    or  BYTE PTR es:[15],al ; set or reset near communal bit as appropriate
    mov BYTE PTR es:[14],1  ; flag that it was an extdef/comdef entry

    mov ax,WORD PTR com_val1+2  ; get communal length high word
    cmp ax,es:[2]           ; make sure that it is greater or equal to pre-existing value
    jb  pcf_7               ; less than, skip update
    mov es:[2],ax           ; save in entry
    mov ax,WORD PTR com_val1    ; get communal length low word
    ja  pcf_6               ; high word is greater, always update low word
    cmp ax,es:[8]           ; see if low word is greater than or equal to pre-existing value
    jbe pcf_7               ; no, don't update low word

pcf_6:
    mov es:[8],ax           ; save in entry

pcf_7:
    mov es,di               ; restore es

pcf_checksum:
    inc current_extdef      ; update global memory variable
    cmp cx,1                ; check if at checksum byte
    jbe pcf_out             ; yes, gobble it and return
    jmp NEAR PTR pcf_loop   ; loop back for next definition

pcf_out:
    inc si                  ; bump past checksum byte
    cmp si,bp               ; check boundary conditions
    jb  pcf_ret             ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pcf_ret:
    ret
proc1_comdef    ENDP

ENDIF

;*****************************
;* PROC1_COMENT              *
;*****************************

; process COMENT record
; upon entry cx=record length,bp=buffer_end,al=record type
; si -> first byte of record past record length word
; destroys ax,bx,dx,di
; updates si,cx

proc1_coment    PROC
    inc si                  ; bump si past attrib byte to comment class
    dec cx                  ; adjust record length to account for scan past attrib byte
    cmp si,bp               ; check boundary conditions
    jb  pct_2               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pct_2:
    mov al,es:[si]          ; get comment class

pct_3:
IFNDEF JUNIOR
    cmp al,DOSSEG_MARK      ; see if DOSSEG ordering flagged
    jne pct_4               ; no
    cmp is_dosseg,0         ; see if DOSSEG ordering previously flagged
    jne pct_4               ; yes
    mov is_dosseg,1         ; set DOSSEG segment ordering flag
    call    make_symbols    ; generate _edata and _end symbol declarations
    ret

pct_4:
    cmp al,LIB_SEARCH_1     ; see if default library search type 1
    je  pct_lib             ; yes
    cmp al,LIB_SEARCH_2     ; see if default library search type 2
    jne pct_chkweak         ; no

; default library name in comment field
pct_lib:
    mov di,OFFSET DGROUP:name_field ; point to place to put library name

pct_loop:
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pct_l2              ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pct_l2:
    mov al,es:[si]          ; get file name char
    or  al,al               ; check if zero
    je  pct_l3              ; yes, transfer complete
    cmp cx,1                ; check if last char
    jbe pct_l3              ; yes, transfer complete
    mov [di],al             ; transfer char
    inc di                  ; point to next char to transfer
    jmp SHORT pct_loop      ; loop back for it

pct_l3:
    mov BYTE PTR [di],0     ; zero terminate filename, di -> zero terminator for check_extension
    call    check_cliplib

; check for default libraries allowed AFTER saving the name in case of
; incremental link where still need to set status of CLIPPER default lib
; encountered to flag Clipper module
pct_5:
    cmp is_nodeflib,0       ; see if default libraries allowed
    jne pct_ret             ; no

    push    es              ; save critical register
    push    cx
    mov dx,OFFSET DGROUP:name_field ; dx -> library name for check_extension
    call    check_extension ; set no_extension flag status for save_lib_name call
    mov di,OFFSET DGROUP:name_field ; di -> library name for save_lib_name
    mov in_overlay,0        ; default libraries are never in overlays
    mov def_lib_flag,1      ; set default library flag
    push    current_lib     ; save current library number so library module code doesn't screw up
    call    save_lib_name   ; save the library name
    pop current_lib         ; restore current library number
    mov def_lib_flag,0      ; reset default library flag
    pop cx                  ; restore critical register
    pop es
    ret

pct_chkweak:
    cmp al,WKEXT            ; see if weak extern type
    jne pct_ret             ; no
    call    get_weak_extern ; yes, get the weak extern type
ENDIF

pct_ret:
    ret
proc1_coment    ENDP

;*****************************
;* GET_WEAK_EXTERN           *
;*****************************

; get weak extern and the default resolution declaration
; upon entry si-> COMENT class a8h
; destroys ax,bx,dx,di
; updates cx,si

get_weak_extern PROC
    inc si                  ; bump si to weak extdef index
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  gwe_loop            ; okay
    call    load_file       ; load next portion of file into buffer, at end position

gwe_loop:
    cmp cx,1                ; see if at checksum
    ja  gwe_1               ; no
    ret                     ; yes, return

gwe_1:
    xor ah,ah               ; zero high byte of index
    mov al,es:[si]          ; get low byte of weak extdef index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  gwe_2               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
gwe_2:
    cmp al,80h              ; check if two byte index value
    jb  gwe_3               ; no, continue
    and al,7fh              ; mask off high bit
    mov ah,al               ; move value to high byte

    mov al,es:[si]          ; get second byte of index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  gwe_3               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

; ax holds weak extdef
gwe_3:
    xor bh,bh               ; zero high byte of index
    mov bl,es:[si]          ; get low byte of weak extdef index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  gwe_4               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
gwe_4:
    cmp bl,80h              ; check if two byte index value
    jb  gwe_5               ; no, continue
    and bl,7fh              ; mask off high bit
    mov bh,bl               ; move value to high byte

    mov bl,es:[si]          ; get second byte of index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  gwe_5               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

; ax holds weak extdef
; bx holds default extdef
gwe_5:
    dec ax                  ; make indices relative zero
    dec bx
    mov di,OFFSET DGROUP:ext_defent_indptr  ; di -> base of array pointers to extdef'ed public entries
    shl ax,1                ; make index a word offset
    add di,ax               ; di -> proper array element
    mov ax,[di]             ; get -> weak extdef
    mov dx,es               ; save -> i/o buffer
    mov es,ax
    mov ax,es:[14]          ; get declaration flag in al, general flags in ah

    and al,3
    cmp al,1                ; see if extdef or comdef
    ja  gwe_nextent         ; no, already resolved

    and ah,40h              ; see if comdef
    jne gwe_nextent         ; yes

    mov al,es:[14]          ; get declaration flag

; two possibilities
; al==81h if this extdef can be made weak
; al==1 if this extdef CANNOT be made weak because it was either declared as a
; comdef or previously extdef without WKEXT mechanism in module or previously
; weak and mismatch of default resolution extdefs occurred

    and al,80h              ; see if can be made weak
    je  gwe_nextent         ; no

    mov ax,bx               ; ax holds index of default extdef
    mov bx,OFFSET DGROUP:ext_defent_indptr  ; bx -> base of array pointers to extdef'ed public entries
    shl ax,1                ; make index a word offset
    add bx,ax               ; bx -> proper array element
    mov ax,[bx]             ; get -> default extdef

    mov bx,es:[0]           ; get previous default resolution extdef
    or  bx,bx               ; see if existed
    je  gwe_6               ; no
    cmp ax,bx               ; see if previous default resolution matches current
    je  gwe_6               ; yes

; previous default resolution does not match current resolution
; make this a strong extdef and ignore mismatched weaks
    mov BYTE PTR es:[14],1  ; strong extdef
    jmp SHORT gwe_nextent   ; bypass weak extdef code

gwe_6:
    mov BYTE PTR es:[14],80h    ; indicate weak extdef
    mov es:[0],ax           ; save -> default resolution extdef entry

gwe_nextent:
    mov es,dx               ; es -> i/o buffer
    jmp NEAR PTR gwe_loop   ; loop for next entry

gwe_ret:
    ret
get_weak_extern ENDP

;*****************************
;* GET_PUBDECL_ENTRY         *
;*****************************

; find or make public declaration entry for pubdef/extdef/comdef name
; upon entry ds:di -> warplink data location holding public name,
; al is 1 if current record is a pubdef record, 2 if comdef, 0 otherwise
; returns segment pointer to entry in ax, 0 if none or duplicate extdef/comdef
; return segment pointer to entry in dx if duplicate extdef/comdef
; destroys ax,dx,di

get_pubdecl_entry   PROC
    push    bx              ; save critical registers
    push    cx
    push    si
    push    es
    mov which_def,al        ; save current record type
    mov si,di               ; si -> name to get hash code of
    call    get_hash        ; get hash code of name in ax

    mov si,OFFSET DGROUP:pubdecl_hash   ; si -> base of hash pointers to public declaration entries
    shl ax,1                ; convert ax to word offset
    add si,ax               ; si points to proper name hash code entry
    cmp WORD PTR [si],0     ; is hash code used (nonzero value)
    je  gp_new_hash         ; no
    jmp NEAR PTR gp_2       ; yes

; hash code not used, new name
gp_new_hash:
    cmp which_def,2         ; see if comdef (promoted or first)
    jb  gp_1                ; no

IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDLs (save COMDEFs with PUBDEFs)
    je  gp_usecdec          ; no
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne gp_1                ; no, using DDLs
ENDIF

gp_usecdec:
    mov ax,alloc_cdeclblk_ptr   ; not using DDLs or 1st udl pass
    jmp SHORT gp_1a

gp_1:
    mov ax,alloc_pdeclblk_ptr

gp_1a:
    or  ax,ax               ; check if any pubdef declarations blocks were previously allocated
    jne gp_3                ; yes

; make initial pubdef declarations block allocation
gp_init_blk:
    mov bx,PUB_DECLBLK_SIZE ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation

    cmp which_def,2         ; see if comdef (2, or 3 if promoted from extdef)
    jb  gp_1b               ; no

IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDLs (save COMDEFs with PUBDEFs)
    je  gp_usec2            ; no
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne gp_1b               ; no, using DDLs
ENDIF

gp_usec2:
    mov alloc_cdeclblk_ptr,ax   ; update last allocated block pointer
    mov first_cdeclblk_ptr,ax   ; update first allocated block pointer
    jmp SHORT gp_1c

gp_1b:
    mov alloc_pdeclblk_ptr,ax   ; update last allocated block pointer
    mov first_pdeclblk_ptr,ax   ; update first allocated block pointer

gp_1c:
    mov es,ax               ; es == current (new) block segment
    xor ax,ax
    mov WORD PTR es:[0],ax  ; zero count of entries in block
    mov WORD PTR es:[2],ax  ; zero pointer to next block
    jmp SHORT gp_4          ; bypass block full check code

; at least one prior pubdef declaration made, check if block is full
gp_3:
    mov es,ax               ; es -> current block
    cmp WORD PTR es:[0],PUB_DECLENT_COUNT  ; see if any free entries in block
    jb  gp_4                ; yes
    call    make_pubdecl_blk    ; no free entries in block, make a new block

gp_4:
    mov bx,es:[0]           ; get entry count
    mov ax,es               ; get block segment address in ax
    add bx,ax               ; get first free entry segment value
    inc bx                  ; adjust for block system info size of 1 paragraph
    mov [si],bx             ; update hash code segment pointer
    mov ax,bx               ; get public declaration entry address in ax for return

    mov dx,es               ; save extra segment
    mov es,bx               ; es -> public declaration entry
    xor bx,bx
    mov es:[0],bx           ; zero init values in pubdef declaration entry
    mov es:[2],bx
    mov es:[8],bx
    mov es:[14],bx
    cmp which_def,3         ; see if comdef promotion
    jne gp_4a               ; no

; comdef promotion from extdef declaration entry, cx -> declaration entry
; update hi/low pointers and name pointers from cx -> to es ->
    call    promote_comdef
    jmp SHORT gp_4b         ; bypass low/high pointer zero'ing code

gp_4a:
    mov es:[10],bx
    mov es:[12],bx

gp_4b:
    mov es,dx               ; restore extra segment

    jmp NEAR PTR gp_update  ; bypass dup/collision code

; hash code used, check whether duplicate name or hash collision
gp_2:
    push    di              ; save di pointing to name
    mov ax,[si]             ; get segment pointer to pubdef declaration entry

gp_testloop:
    pop si                  ; si -> name
    push    si              ; put it back on stack
    mov es,ax               ; es -> segment of pubdef declaration entry testing name against
    mov bx,ax               ; save entry segment in bx
    mov ax,es:[6]           ; get segment of pubdef name in pubdef names block
    mov di,es:[4]           ; get offset of pubdef name in pubdef names block
    mov es,ax               ; extra segment holds pubdef name segment

; ds:si -> name, es:di -> name to test against
gp_byteloop:
    mov al,[si]
    or  al,es:[di]          ; see if both values are zero (matched to null terminator)
    je  to_gp_match         ; strings matched
    cmpsb                   ; compare a nonzero byte in the two names
    je  gp_byteloop         ; bytes match, loop for next byte test
    mov es,bx               ; get pubdef declaration entry segment value
    jc  gp_7                ; old name greater than new name

; new name greater than old name
    mov ax,es:[12]          ; get entry having a greater name
    xor dl,dl               ; flag new > old, update high pointer
    jmp SHORT gp_8          ; check for null pointer

; stepping stone to matched code
to_gp_match:
    jmp NEAR PTR gp_match

; old name greater than new name
gp_7:
    mov ax,es:[10]          ; get entry having a lesser name
    mov dl,1                ; flag old > new update low pointer

gp_8:
    mov cx,es               ; save -> parent entry in cx
    or  ax,ax               ; check if a null pointer (ax=0)
    jne gp_testloop         ; no, keep checking entries

; new name, no matches for any pubdef declaration entry
gp_newname:
    cmp which_def,2         ; see if comdef (promoted or first)
    jb  gp_8a               ; no

IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDLs (save COMDEFs with PUBDEFs)
    je  gp_usec3            ; no
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne gp_8a               ; no, using DDLs
ENDIF

gp_usec3:
    mov ax,alloc_cdeclblk_ptr   ; get last allocated block
    jmp SHORT gp_8b

gp_8a:
    mov ax,alloc_pdeclblk_ptr   ; get last allocated block

gp_8b:
    or  ax,ax               ; see if block previously existed
    jne gp_8d               ; yes
    mov di,bx               ; save bx
    mov bx,PUB_DECLBLK_SIZE ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov bx,di               ; restore bx

    cmp which_def,2         ; see if comdef
    jb  gp_pub_alloc        ; no

IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDLs (save COMDEFs with PUBDEFs)
    je  gp_usec4            ; no
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne gp_pub_alloc        ; no, using DDLs
ENDIF

gp_usec4:
    mov alloc_cdeclblk_ptr,ax   ; update last allocated block pointer
    mov first_cdeclblk_ptr,ax   ; update first allocated block pointer
    jmp SHORT gp_8c

gp_pub_alloc:
    mov alloc_pdeclblk_ptr,ax   ; update last allocated block pointer
    mov first_pdeclblk_ptr,ax   ; update first allocated block pointer

gp_8c:
    mov es,ax               ; es -> block
    xor ax,ax
    mov WORD PTR es:[0],ax  ; zero count of entries in block
    mov WORD PTR es:[2],ax  ; zero pointer to next block
    jmp SHORT gp_9          ; bypass check for block full (new block)

gp_8d:
    mov es,ax               ; es -> block
    cmp WORD PTR es:[0],PUB_DECLENT_COUNT   ; see if any free entries in block
    jb  gp_9                ; yes
    call    make_pubdecl_blk    ; no free entries in block, make a new block

gp_9:
    mov di,es:[0]           ; get entry count
    push    es              ; save segdef block address
    mov ax,es               ; get block segment address
    add di,ax               ; get first free entry segment value
    inc di                  ; adjust for block system info size of 1 paragraph

    mov si,es               ; save extra segment
    mov es,di               ; es -> new public declaration entry
    xor ax,ax
    mov es:[0],ax           ; zero init values in pubdef declaration entry
    mov es:[2],ax
    mov es:[8],ax
    mov es:[14],ax
    cmp which_def,3         ; see if comdef promotion
    jne gp_9a               ; no

; comdef promotion from extdef declaration entry, cx -> declaration entry
; bx -> parent entry to declaration entry
; update hi/low pointers and name pointers from cx -> to es ->
    call    promote_comdef
    jmp SHORT gp_9b         ; bypass low/high pointer zero'ing code

gp_9a:
    mov es:[10],ax
    mov es:[12],ax

gp_9b:
    mov es,si               ; restore extra segment
    mov ax,di               ; ax -> new public declaration entry for return

    mov es,bx               ; get entry that pointed to this new entry (parent entry)
    or  dl,dl               ; zero if high pointer update, nonzero if low pointer update
    jne gp_10               ; low pointer update
    mov es:[12],di          ; update high name pointer
    pop es                  ; restore es -> segdef block
    jmp SHORT gp_11         ; perform new entry code

gp_10:
    mov es:[10],di          ; update low name pointer
    pop es                  ; restore es -> segdef block
    jmp SHORT gp_11         ; perform new entry code

; names match
gp_match:
    cmp which_def,1         ; check if a pubdef record
    je  gp_12               ; yes it is

; duplicate extdef or comdef declaration, ignore without warning
; HOWEVER, first check if a comdef declaration that was first extdef
; declared.  If so, make a comdef entry and redirect the pointers to it.
gp_null:
    mov si,dx               ; save flag value of dl in si
    mov dx,bx               ; return pubdef declaration entry value in dx, if needed
    pop di                  ; restore di pointing to name

    mov es,bx               ; es -> pubdef declaration entry
    mov al,es:[14]
	and	al,83h				; mask off nonrelevant bits
    test    al,2            ; see if previously declared public
    jne null_ret            ; yes, ignore

    cmp which_def,2         ; see if current declaration is comdef
    je  gp_iscomdef         ; yes

; extdef has three possible values at this point
; 1 == strong extdef, not weak through WKEXT mechanism at least once, keep it strong
; 81 == unknown extdef in previous use, not modified by WKEXT mechanism, make strong
; 80 == known weak extdef, change to unknown status

    cmp al,1                ; see if extdef (not weak)
    je  null_ret            ; yes
    cmp al,81h              ; see if previously unknown extdef never 'weak'ened
    jne gp_weakext          ; no
    and al,1                ; yes, set as known strong extdef
    mov BYTE PTR es:[14],al ; save new value
    jmp SHORT null_ret

; assume al value is 80h (previously weakened extdef)
gp_weakext:
    mov al,81h              ; set as unknown extdef in previous use
    mov BYTE PTR es:[14],al ; save new value
    jmp SHORT null_ret

gp_iscomdef:
    mov al,1
    mov BYTE PTR es:[14],al ; force any weak extdefs to be ignored for comdef
    mov al,es:[15]
    and al,40h              ; see if previous declaration was comdef
    jne null_ret            ; yes
    jmp NEAR PTR ext_to_com ; no

null_ret:
    xor ax,ax
    jmp SHORT gp_ret

; current record is a pubdef record
gp_12:
    mov es,bx               ; get pubdef declaration entry segment value
    mov al,es:[14]          ; get previous entry definition flag
    and al,3                ; get pubdef/extdef/comdef field
    cmp al,2                ; check if previous entry was a pubdef/absolute
    jae gp_mult_warn        ; yes, symbol defined more than once

    test BYTE PTR es:[15],40h   ; check if previous definition was a communal
    je  gp_14               ; no
    dec communal_count      ; decrement count of communals, public declaration supersedes communal
    sub WORD PTR pub_sym_count,1    ; update low word count
    sbb WORD PTR pub_sym_count+2,0  ; borrow from low word, if any

; previous entry was comdef or extdef, return valid pointer value
gp_14:
    mov ax,bx               ; get segment value in ax
    pop di                  ; restore di pointing to name
    jmp SHORT gp_ret        ; all done, exit

gp_11:
    pop di                  ; restore di pointing to name

gp_update:
    inc WORD PTR es:[0]     ; bump count of entries
    add WORD PTR tot_sym_count,1    ; update low word count of total symbols
    adc WORD PTR tot_sym_count+2,0  ; update high word count with low word carry

gp_ret:
    pop es                  ; restore critical register
    pop si
    pop cx
    pop bx
    ret

; warning, symbol defined more than once
gp_mult_warn:
    mov di,es:[4]           ; get offset of name in di
    mov es,es:[6]           ; get segment of name in es
    mov dx,OFFSET DGROUP:filename
    mov ax,MULT_DEF_SYM_WARN    ; flag symbol defined multiple times
    call    link_warning    ; give warning feedback
    jmp NEAR PTR gp_null    ; ignore public definition

; comdef and previous declaration was extdef, redirect pointers and
; create as a new name
ext_to_com:
    mov dx,si               ; get dl flag value back from si
    mov which_def,3         ; flag extdef to comdef promotion
    sub WORD PTR tot_sym_count,1    ; back off total symbols by one (new entry will re-increment)
    sbb WORD PTR tot_sym_count+2,0
    mov si,di               ; si -> name to get hash code of
    call    get_hash        ; get hash code of name in ax
    mov si,OFFSET DGROUP:pubdecl_hash   ; si -> base of hash pointers to public declaration entries
    shl ax,1                ; convert ax to word offset
    add si,ax               ; si points to proper name hash code entry
    cmp WORD PTR [si],bx    ; is hash code -> this entry
    jne etc_2               ; no
    mov cx,bx               ; cx -> extdef declaration entry
    jmp NEAR PTR gp_new_hash    ; transfer to new hash code entry code

; not the first entry -> hash code, cx -> parent of extdef declaration entry
etc_2:
    push    di              ; save di -> name on stack
    xchg  bx,cx             ; bx -> parent of extdef declaration entry.
                            ; cx -> extdef declaration entry
    jmp NEAR PTR gp_newname ; transfer to new name code

get_pubdecl_entry   ENDP

;*****************************
;* PROMOTE_COMDEF            *
;*****************************

; code to promote extdef entry to comdef
; no registers destroyed

promote_comdef  PROC
    push    ds
    push    ax
    mov ds,cx               ; ds -> extdef declaration entry
    or  BYTE PTR ds:[15],20h    ; set local communal bit in abandoned extdef entry so map won't pick it up
    mov ax,ds:[4]
    mov es:[4],ax           ; transfer name pointer offset
    mov ax,ds:[6]
    mov es:[6],ax           ; transfer name pointer segment
    mov ax,ds:[10]
    mov es:[10],ax          ; transfer entry low pointer
    mov ax,ds:[12]
    mov es:[12],ax          ; transfer entry high pointer
    pop ax
    pop ds
    ret
promote_comdef  ENDP

;*****************************
;* MAKE_PUBDECL_BLK          *
;*****************************

; make a new pubdef declarations block
; return segment of new block in es
; destroys ax,es

make_pubdecl_blk    PROC
    push    bx              ; save critical registers
    push    dx

    cmp which_def,2         ; see if comdef (promoted ==3 or first ==2)
    jb  mpb_1               ; no

IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDLs (save COMDEFs with PUBDEFs)
    je  mpb_usec            ; no
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne mpb_1               ; no, using DDLs
ENDIF

mpb_usec:
    mov dx,alloc_cdeclblk_ptr   ; use comdef declaration block
    jmp SHORT mpb_2

mpb_1:
    mov dx,alloc_pdeclblk_ptr   ; keep previously last allocated block segment

mpb_2:
    mov bx,PUB_DECLBLK_SIZE ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation

    cmp which_def,2         ; see if comdef
    jb  mpb_3               ; no

IFNDEF DEMO
    cmp any_ddl,0           ; see if using DDLs (save COMDEFs with PUBDEFs)
    je  mpb_usec2           ; no
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne mpb_3               ; no, using DDLs
ENDIF

mpb_usec2:
    mov alloc_cdeclblk_ptr,ax   ; update last allocated comdef block pointer
    jmp SHORT mpb_4

mpb_3:
    mov alloc_pdeclblk_ptr,ax   ; update last allocated block pointer

mpb_4:
    mov es,dx               ; es -> old block
    mov es:[2],ax           ; update old block to point to new block

    mov es,ax               ; es -> new block
    xor ax,ax
    mov es:[0],ax           ; zero count of entries in block
    mov es:[2],ax           ; zero pointer to next block
    pop dx                  ; restore critical registers
    pop bx
    ret
make_pubdecl_blk    ENDP

;*****************************
;* ADD_PUBDEF_NAME           *
;*****************************

; add name to pubdef names block
; upon entry ds:di -> warplink data location holding public name
; returns segment:offset pointer to entry in dx:ax
; destroys ax,dx,di

add_pubdef_name PROC
    push    bx              ; save critical registers
    push    cx
    push    si
    push    es
    mov ax,alloc_pdnameblk_ptr
    or  ax,ax               ; check if any pubdef name blocks were previously allocated
    jne apn_2               ; yes

; make initial pubdef names block allocation
    mov bx,PUB_NAMBLK_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov first_pdnameblk_ptr,ax  ; save first allocated block pointer
    mov alloc_pdnameblk_ptr,ax  ; update last allocated block pointer
    mov es,ax               ; es == current (new) block segment
    xor bx,bx
    mov es:[bx],PUB_NAMELIST_SIZE   ; all name list space is free
    mov WORD PTR es:[2],bx  ; zero pointer to next block

apn_2:
    mov es,ax               ; es -> current block
    mov ax,es:[0]           ; get free space in block

; get length of name in dx INCLUDING zero terminator
    mov dx,1                ; init length of name
	cmp	any_ddl,0			; see if using DDL's
	je	apn_2a				; no
	inc	dx					; adjust for prepended length byte

apn_2a:
    mov bx,di               ; save start of name in bx
apn_lenloop:
    cmp BYTE PTR [di],0     ; check if at end of name
    je  apn_3               ; yes, at end
    inc dx                  ; bump count of chars in string
    inc di                  ; point to next char
    jmp SHORT apn_lenloop   ; check next char

apn_3:
    mov di,bx               ; restore start of name in di
    push    dx              ; save length of name string on stack
    cmp ax,dx               ; see if name will fit in block
    jb  apn_4               ; no, it won't

; name will fit into current pubdef names block
    mov dx,PUB_NAMELIST_SIZE
    sub dx,ax               ; get beginning slot for name
    jmp SHORT apn_5         ; bypass block allocation code

; not enough room for name in this block, allocate another pubdef names block
apn_4:
    call    make_pubdef_name_blk
    xor dx,dx               ; name begins at offset zero of namelist

apn_5:
    mov si,di               ; si -> public name
    mov di,dx               ; di -> space in pubdef names block where name goes
    add di,4                ; adjust for 2 words system info before namelist

    pop cx                  ; get length of name string including zero terminator
    sub es:[0],cx           ; update space free in block, subtracting off name length
    mov dx,es               ; dx:ax == segment:offset of pubdef name
    mov ax,di

	cmp	any_ddl,0			; see if using DDL's
	je	apn_6				; no
	mov	bx,ax				; save ax critical register value
	mov	ax,cx				; total length in ax (al)
	stosb					; store length byte as prepended to actual name
	dec	cx					; drop count of bytes to transfer
	mov	ax,bx				; restore ax critical register value

apn_6:
    shr cx,1                ; convert to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

    pop es                  ; restore critical registers
    pop si
    pop cx
    pop bx
    ret
add_pubdef_name ENDP

;*****************************
;* MAKE_PUBDEF_NAME_BLK      *
;*****************************

; make a new pubdef name block
; return segment of new block in es
; destroys ax,bx,es

make_pubdef_name_blk    PROC
    push    dx              ; save critical register
    mov dx,alloc_pdnameblk_ptr  ; keep previously last allocated block segment
    mov bx,PUB_NAMBLK_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov alloc_pdnameblk_ptr,ax  ; update last allocated block pointer

    mov es,dx               ; es -> old block
    mov es:[2],ax           ; update old block to point to new block

    mov es,ax               ; es -> new block
    xor bx,bx
    mov es:[bx],PUB_NAMELIST_SIZE   ; all name list space is free
    mov WORD PTR es:[2],bx  ; zero pointer to next block
    pop dx                  ; restore critical registers
    ret
make_pubdef_name_blk    ENDP

;*****************************
;* PROC_CEXTDEF              *
;*****************************

; parse CEXTDEF record, put name in name_field
; upon entry cx=record length,bp=buffer_end,al=record type
; si -> first byte of record past record length
; updates si
; destroys ax,bx,dx,di

proc_cextdef	PROC
	push	es				; save critical register

    xor ah,ah               ; zero high byte
	lods	BYTE PTR es:[0]	; get low byte of comdat external index
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ce_2                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ce_2:
    cmp al,80h              ; check if more than one byte index value
    jb  ce_3				; no, continue
    and al,7fh              ; mask off high bit
    mov ah,al               ; transfer to high word
	lods	BYTE PTR es:[0]	; get second byte, actual value
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ce_3				; okay
    call    load_file       ; load next portion of file into buffer, at end position

; ax holds index value
ce_3:
    dec ax                  ; make count relative zero
    shl ax,1                ; convert to word index
    shl ax,1                ; convert to doubleword index
	push	si				; save critical register
    mov si,OFFSET DGROUP:lnames_ent_indptr  ; si -> base of lnames array
    add si,ax               ; si -> array element with segment:offset of name in lnames block

    mov di,[si]             ; get offset of name in di
    mov ax,[si+2]           ; get segment of name in ax
    mov si,OFFSET DGROUP:name_field ; point to slot to place name
	push	ds				; save warplink's data segment
	pop	es					; es -> warplink's data
    mov ds,ax               ; ds -> name segment in lnames block
    xchg    si,di           ; di -> name field, si -> name offset in lnames block
    add si,8                ; bump si past hi/low segment:offset pointers in lnames entry

; ds:si -> lnames name, es:di -> warplink data name field
ce_get_name:
    movsb                   ; transfer a char
    cmp BYTE PTR [si-1],0   ; zero char flags end of transfer
    jne ce_get_name			; not zero, keep transferring

	push	es
	pop	ds					; restore ds -> warplink data
	pop	si					; restore critical registers
	pop	es
	ret
proc_cextdef	ENDP

END
