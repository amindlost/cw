;*********************************************************************
;*   MLPASS2A.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          05/10/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.7                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker pass 2 routines part A                                   *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlpass2a
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
     
PUBLIC  pass2_obj_proc

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   parse_complete:BYTE,filename:BYTE,libobj_flag:BYTE
EXTRN   ovl_entry_id:WORD,ovl_code_count:WORD
EXTRN	new_ledata_offset:WORD
EXTRN	is_local:BYTE
EXTRN	ignore_fixupp_flag:BYTE

; initialized local variables

.DATA?
; uninitialized local variables

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR,get_name:NEAR
EXTRN   proc2_lnames:NEAR,proc2_extdef:NEAR,proc2_segdef:NEAR
EXTRN   proc2_grpdef:NEAR,proc2_fixupp:NEAR,proc2_modend:NEAR
EXTRN   proc2_data:NEAR
;*** EXTRN   proc_locals:NEAR,zero_local_blocks:NEAR
EXTRN	pass2_clipcheck:NEAR

;*****************************
;* PASS2_OBJ_PROC            *
;*****************************

; parse and process records in object modules
; all registers used or destroyed

pass2_obj_proc  PROC
	call	pass2_clipcheck	; see if Clipper module that is compressed

    xor si,si               ; si points into buffer for parsed chars
    mov ax,si
    mov libobj_flag,al
    mov ax,buffer_tail      ; get buffer_tail variable in register for quick compares
    mov bp,ax               ; bp holds variable value

; entry point to process next module if more than one in specified OBJ
p2_proc_next:
    xor ax,ax
    mov parse_complete,al   ; init flag indicating record parsing is complete for object module
	mov	ignore_fixupp_flag,al
    mov current_lnames,ax   ; init count of current record types
    mov current_extdef,ax
    mov current_segdef,ax
    mov current_grpdef,ax
	mov	new_ledata_offset,ax	; init LEDATA data offset after compression
    mov seg_ovlclass,al     ; init overlay class segment in module flag
    mov ax,ovl_code_count
    mov ovl_entry_id,ax     ; save ovl_code_count entry value
;***    call    zero_local_blocks   ; zero any pre-existing local blocks (don't carry locals to next module)

p2_proc_loop:
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

pop_2e:
; check that record doesn't exceed file length
    cmp eof_flag,0          ; see if end of file encountered
    je  len_chk             ; nope
    mov dx,cx               ; get record length
    add dx,si               ; add in current position
    jc  unexpected_eof      ; overflow, fatal error

len_chk:
    cmp cx,1                ; see if null record
    jbe to_update_pos       ; yes, ignore it

pop_2f:
    cmp al,LNAMES           ; is record a LNAMES record
    jne pop_3
    call    data_process    ; process any previous LEDATA or LIDATA record
    call    proc2_lnames    ; process the record
    jmp NEAR PTR parse_chk

unexpected_eof:
    mov dx,OFFSET DGROUP:filename
    mov ax,UNEXPECT_EOF_ERR ; unexpected end of file
    jmp NEAR PTR link_error ; transfer control to error handler

pop_3:
    cmp al,EXTDEF           ; is it a EXTDEF record
    jne pop_cextdef
pop_extdef:
    push    ax              ; save al record type
    call    data_process    ; process any previous LEDATA or LIDATA record
    pop ax                  ; restore al record type
    call    proc2_extdef    ; process the record
    jmp NEAR PTR update_pos

pop_cextdef:
    cmp al,CEXTDEF			; is it a CEXTDEF record
    je pop_extdef			; yes

    cmp al,COMDEF           ; check if a COMDEF record
    je  pop_extdef          ; COMDEF behaves as an EXTDEF record on second pass

    cmp al,SEGDEF           ; is it an SEGDEF record
    jne pop_5
    call    data_process    ; process any previous LEDATA or LIDATA record
    call    proc2_segdef    ; process the record
    jmp NEAR PTR parse_chk
pop_5:
    cmp al,GRPDEF           ; is it a GRPDEF record
    jne pop_6
    call    data_process    ; process any previous LEDATA or LIDATA record
    call    proc2_grpdef    ; process the record

to_update_pos:
    jmp NEAR PTR update_pos
pop_6:
    cmp al,FIXUPP           ; is it a FIXUPP record
    jne pop_7
    call    proc2_fixupp    ; process the record
    jmp NEAR PTR update_pos

pop_7:
    cmp al,LEDATA           ; is it a LEDATA record
    jne pop_8
    cmp prev_flag,0         ; yes, see if prevous data record pending
    je  pop_7a              ; no
    call    data_process    ; process the previous data record
pop_7a:
    mov prev_flag,1         ; flag LEDATA record on hold
    jmp SHORT pop_data      ; jump to code shared with LIDATA record
pop_8:
    cmp al,LIDATA           ; is it a LIDATA record
    jne pop_9
    cmp prev_flag,0         ; yes, see if prevous data record pending
    je  pop_8a              ; no
    call    data_process    ; process the previous data record
pop_8a:
    mov prev_flag,2         ; flag LIDATA record on hold
pop_data:
    mov prev_read_ptr,si    ; save pointer to record in file buffer
    mov prev_rec_len,cx     ; save length of record
    mov ax,WORD PTR file_pos_adj    ; get file offset adjustment high word
    mov WORD PTR prev_pos_adj,ax    ; save it
    mov ax,WORD PTR file_pos_adj+2  ; get file offset adjustment low word
    mov WORD PTR prev_pos_adj+2,ax  ; save it
    jmp SHORT update_pos

pop_9:
    cmp al,MODEND           ; is it a MODEND record
    jne pop_10              ; no
    mov parse_complete,al   ; al is known nonzero value, put it in parse_complete flag
    call    data_process    ; process any previous LEDATA or LIDATA record
    call    proc2_modend    ; process the record
    jmp SHORT update_pos

pop_10:
    cmp al,THEADR           ; is it a THEADR record
    jne pop_mslhed          ; no
    call    proc2_theadr    ; process the record
    jmp SHORT update_pos

pop_mslhed:
    cmp al,MSLHED           ; make library header records valid
    jne pop_11
    mov libobj_flag,al      ; set library as object flag
    jmp SHORT update_pos

pop_11:
    cmp al,LHEADR           ; is it a LHEADR record
    jne pop_12              ; no
    call    proc2_theadr    ; LHEADR processes the sames as THEADR
    jmp SHORT update_pos

; local symbol processing
pop_12:
    cmp al,LEXTDEF          ; check if LEXTDEF record
    jne pop_12b
to_pop_extdef:
	jmp	NEAR PTR pop_extdef

pop_12b:
    cmp al,LCOMDEF          ; check if a LCOMDEF record
	je	to_pop_extdef		; yes, process as EXTDEF

    cmp al,LEDATA32			; check if a LEDATA32 record
	jne	pop_ignore			; no
    call    data_process    ; process any previous LEDATA or LIDATA record
	mov	ignore_fixupp_flag,1	; yes, flag ignore following FIXUPP
	jmp	SHORT update_pos

; remaining record types ignored, but check for previous L?DATA record to process
pop_ignore:
    call    data_process    ; process any previous LEDATA or LIDATA record

update_pos:                 ; update si to point past record
    mov dx,si               ; save current position prior to update
    add si,cx               ; scan past remaining bytes
    jnc pop_14              ; no overflow

pop_13:
    mov si,dx               ; restore si to current position
    mov ax,bp               ; get overflow point
    sub ax,si               ; get difference from current position to overflow
    mov si,bp               ; put si at overflow point
    sub cx,ax               ; update cx to account for changed si position
    call    load_file       ; load next portion of file into buffer, at end position
    jmp SHORT update_pos    ; loop back for next attempt

pop_14:
    cmp si,bp               ; check boundary conditions
    jae pop_13              ; out of bounds

parse_chk:
    mov al,parse_complete
    or  al,al               ; see if parsing should continue
    jne pop_done            ; parsing complete for this module
    jmp NEAR PTR p2_proc_loop   ; loop for next record

pop_done:
    cmp is_inlib,0          ; see if processing library
    jne pop_ret             ; yes

    cmp libobj_flag,0       ; see if library treated as object module
    je  pop_ret             ; no

    mov al,es:[si]          ; get next record type
    cmp al,THEADR           ; see if start of new object module
    je  pop_more            ; yes
    cmp al,LHEADR           ; see if start of new object module
    je  pop_more            ; yes

    or  al,al               ; see of zero (padding)
    jne pop_ret             ; no
    mov cx,1                ; setup to eat one byte
    jmp SHORT update_pos    ; loop back

pop_ret:
    ret

pop_more:
    jmp NEAR PTR p2_proc_next   ; get next module in file

pass2_obj_proc  ENDP

;*****************************
;* PROC2_THEADR              *
;*****************************

; process THEADR record
; upon entry cx=record length,bp=buffer_end,al=record type
; si -> first byte of record past record length
; destroys ax
; updates si

proc2_theadr    PROC
    mov di,OFFSET DGROUP:tmod_name  ; di -> field to place name
	mov	is_local,0			; flag not a local for get_name
    call    get_name        ; get and save T-module name
    ret                     ; all done
proc2_theadr    ENDP

;*****************************
;* DATA_PROCESS              *
;*****************************

; determine if previous records was LEDATA or LIDATA record, and process if so
; destroys ax,bx,dx,di

data_process    PROC
    cmp prev_flag,0         ; check if previous record was L?DATA
    je  dp_ret              ; no
    call    proc2_data      ; process previous L?DATA record
    mov prev_flag,0         ; reset previous record flag

dp_ret:
    ret
data_process    ENDP

END
