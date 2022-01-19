;*********************************************************************
;*   MLPASS2B.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          05/10/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.7                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker pass 2 routines part B                                   *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlpass2b
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
PUBLIC  proc2_lnames,proc2_extdef,proc2_segdef,proc2_grpdef
PUBLIC  find_pubdecl_entry,find_segdef_entry

; variables
PUBLIC  ovl_code_count

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   parse_complete:BYTE,name_field:BYTE,filename:BYTE
EXTRN   class_name_index:WORD
EXTRN   no_match_okay:BYTE,clarclass_text:BYTE
EXTRN	is_local:BYTE,is_cextdef:BYTE

; initialized local variables

EVEN                        ; maximize speed on 8086 and better

; word values
ovl_code_count  DW  0       ; running count of overlaid code segments, used by Clarion code

.DATA?

; uninitialized local variables

; byte values

EVEN                        ; maximize speed on 8086 and better
acbp_byte   DB  ?           ; ACBP byte in segdef record
EVEN
is_comdef   DB  ?           ; nonzero if record is a COMDEF record
EVEN
mustfind    DB  ?           ; nonzero if must find_segdef_entry must find entry
EVEN

; word values
EVEN
segment_name_index  DW  ?   ; segment name index in segdef record
frame_number    DW  ?       ; frame number in segdef record

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR,link_warning:NEAR
EXTRN   get_name:NEAR,get_hash:NEAR,write_bytes:NEAR,caseless_strcmp:NEAR
EXTRN   map_detail_seg:NEAR
EXTRN   scan_comfield:NEAR
EXTRN	proc_cextdef:NEAR

;*****************************
;* PROC2_LNAMES              *
;*****************************

; pass 2 lnames record processing
; upon entry cx=record length,bp=buffer_end,al=record type
; es:si -> first byte of record past record length
; destroys ax,bx,dx,di
; updates si

proc2_lnames    PROC
pl_loop:
    inc current_lnames      ; bump count of lnames in file
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
    call    find_lnames_entry   ; find lnames entry, return segment:offset pointer

pl_array:
    mov di,current_lnames   ; get current lnames logical names count in di
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
proc2_lnames    ENDP

;*****************************
;* FIND_LNAMES_ENTRY         *
;*****************************

; find entry for lnames name
; upon entry di -> warplink data location holding logical name
; returns segment:offset pointer to entry in dx:ax
; destroys ax,bx,dx,di

find_lnames_entry   PROC
    push    cx              ; save critical registers
    push    si
    push    es
    mov si,di               ; si must -> name for get_hash procedure
    call    get_hash        ; get hash code of name in ax
    mov si,OFFSET DGROUP:lnames_hash    ; si -> base of hash pointers to lnames entries
    shl ax,1
    shl ax,1                ; convert ax to doubleword offset
    add si,ax               ; si points to proper name hash code entry
    cmp WORD PTR [si+2],0   ; is hash code used (nonzero segment value)
    jne fle_2

; hash code for name unused, internal error
fle_internal:
    mov ax,INTERNAL_ERR     ; put warplink error code in ax
    mov cx,3                ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

fle_2:
    mov ax,[si+2]           ; get segment pointer to lnames entry
    mov dx,[si]             ; get offset pointer to lnames entry
    mov si,di               ; si -> name

fle_testloop:
    mov es,ax               ; es == segment of lnames entry testing name against
    mov di,dx               ; di == offset of lnames entry testing name against
    mov bx,di               ; save entry offset in bx
    add di,8                ; scan past segment:offset high and low pointers
    push    si              ; save si -> name

; ds:si -> name, es:di -> name to test against
fle_byteloop:
    mov al,[si]
    or  al,es:[di]          ; see if both values are zero (matched to null terminator)
    je  fle_match           ; yes
    cmpsb                   ; compare a nonzero byte in the two names
    je  fle_byteloop        ; bytes match, loop for next byte test
    jc  fle_5               ; old name greater than new name

; new name greater than old name
    mov ax,es:[bx+6]        ; get segment of lnames entry having a greater name
    mov dx,es:[bx+4]        ; get offset of lnames entry having greater name
    jmp SHORT fle_6         ; check for null pointer

; old name greater than new name
fle_5:
    mov ax,es:[bx+2]        ; get segment of lnames entry having lesser name
    mov dx,es:[bx]          ; get offset of lnames entry having lesser name

fle_6:
    pop si                  ; restore si -> name
    or  ax,ax               ; check if a null pointer (segment value in ax=0)
    jne fle_testloop        ; no, keep checking entries
    jmp SHORT fle_internal  ; no more entries, internal error

; names match, return pointer to name
fle_match:
    pop si                  ; pull old si value off of stack, garbage
    mov ax,dx               ; ax holds name offset
    mov dx,es               ; dx holds name segment

    pop es                  ; restore critical registers
    pop si
    pop cx
    ret
find_lnames_entry   ENDP

;*****************************
;* PROC2_EXTDEF              *
;*****************************

; process extdef records
; upon entry cx=record length,bp=buffer_end,al=record type
; si -> first byte of record past record length
; updates si
; destroys ax,dx,di

proc2_extdef    PROC
    xor ah,ah
	mov	is_cextdef,ah		; init comdat extdef flag
    cmp al,EXTDEF           ; see if a EXTDEF record
    je	pe_1                ; no
    cmp al,LEXTDEF			; see if a LEXTDEF record
    je	pe_1                ; no
    inc ah                  ; set flag for COMDEF

pe_1:
    mov is_comdef,ah        ; set appropriate flag value

    xor ah,ah				; init for is_local flag
	cmp	al,EXTDEF
	je	pe_saveloc
	cmp	al,COMDEF
	je	pe_saveloc
	inc	ah					; flag local (LCOMDEF or LEXTDEF)

pe_saveloc:
	mov	is_local,ah			; flag local or not
    cmp al,CEXTDEF          ; see if COMDAT extdef
    jne pe_loop				; no
	mov	is_cextdef,al		; yes, flag it

pe_loop:
	cmp	is_cextdef,0		; see if COMDAT extdef
	je	pe_notcext			; no
	call	proc_cextdef	; parse COMDAT extdef entry
	jmp	SHORT pe_3			; process regular extdef parsing

pe_notcext:
    mov di,OFFSET DGROUP:name_field ; point di at field to place name
    call    get_name        ; get public name from record

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
    cmp is_comdef,0         ; see if comdef record
    je  pe_3                ; no

; comdef record, scan past the data segment type, and communal length field
    mov dl,es:[si]          ; get data segment type
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pe_2b               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pe_2b:
    mov dh,es:[si]          ; get low byte of variable size (NEAR)/number of elements (FAR)
    cmp dl,61h              ; check if FAR variable
    jne pe_2c
    call    scan_comfield   ; scan past communal length field number of elements
    mov dh,es:[si]          ; get low byte of element size
    call    scan_comfield   ; scan past communal length field element size
    jmp SHORT pe_3          ; bypass NEAR variable code

; assume near variable
pe_2c:
    call    scan_comfield   ; scan past communal length field variable size

pe_3:
    mov di,OFFSET DGROUP:name_field ; point di at name field for find_pubdecl_entry procedure
    call    find_pubdecl_entry  ; find/return public declaration entry segment in ax

    mov bx,OFFSET DGROUP:ext_defent_indptr    ; bx -> base of array pointers to extdef'ed public entries
    mov di,current_extdef   ; get array element (current extdef number)
    shl di,1                ; make ax a word offset
    add bx,di               ; bx -> proper array element
    mov [bx],ax             ; save pointer to public declaration entry
    inc current_extdef      ; update global memory variable

pe_checksum:
    cmp cx,1                ; check if at checksum byte
    ja  pe_loop             ; no, loop back for next definition

pe_ret:
    ret
proc2_extdef    ENDP

;*****************************
;* FIND_PUBDECL_ENTRY        *
;*****************************

; find pubdef declaration entry
; upon entry ds:di -> pubdef name
; return segment of entry in ax
; fatal error if not found
; destroys ax,bx,dx

find_pubdecl_entry  PROC
    push    si              ; save critical register
    mov si,di               ; si -> name to get hash code of
    call    get_hash        ; get hash code of name in ax

    mov si,OFFSET DGROUP:pubdecl_hash   ; si -> base of hash pointers to public declaration entries
    shl ax,1                ; convert ax to word offset
    add si,ax               ; si points to proper name hash code entry
    cmp WORD PTR [si],0     ; is hash code used (nonzero value)
    jne fpe_2               ; yes

; hash code for name unused, internal error

fpe_internal:
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    cmp no_match_okay,0     ; check to see if no match okay flag is set
    je  fpe_int2            ; no
    pop si                  ; restore si
    xor ax,ax               ; return null value to show no match
    ret

fpe_int2:
    mov ax,INTERNAL_ERR     ; put warplink error code in ax
    mov cx,4                ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

; hash code used, check for hash collision
fpe_2:
    push    es              ; save critical register
    push    di              ; save di pointing to name
    mov ax,[si]             ; get segment pointer to pubdef declaration entry

fpe_testloop:
    pop si                  ; si -> name
    push    si              ; put it back on stack
    mov es,ax               ; es -> segment of pubdef declaration entry testing name against
    mov bx,ax               ; save entry segment in bx
    mov ax,es:[6]           ; get segment of pubdef name in pubdef names block
    mov di,es:[4]           ; get offset of pubdef name in pubdef names block
    mov es,ax               ; extra segment holds pubdef name segment

; ds:si -> name, es:di -> name to test against
fpe_byteloop:
    mov al,[si]
    or  al,es:[di]          ; see if both values are zero (matched to null terminator)
    je  fpe_match           ; strings matched
    cmpsb                   ; compare a nonzero byte in the two names
    je  fpe_byteloop        ; bytes match, loop for next byte test
    mov es,bx               ; get pubdef declaration entry segment value
    jc  fpe_7               ; old name greater than new name

; new name greater than old name
    mov ax,es:[12]          ; get entry having a greater name
    xor dl,dl               ; flag new > old, update high pointer
    jmp SHORT fpe_8         ; check for null pointer

; old name greater than new name
fpe_7:
    mov ax,es:[10]          ; get entry having a lesser name
    mov dl,1                ; flag old > new update low pointer

fpe_8:
    or  ax,ax               ; check if a null pointer (ax=0)
    jne fpe_testloop        ; no, loop back for next name check
    pop di                  ; restore di pointing to name
    pop es                  ; restore critical register
    jmp SHORT fpe_internal  ; name must be previously stored

; names match
fpe_match:
    mov es,bx               ; get pubdef declaration entry segment value
    mov al,es:[14]          ; get definitions flag
    and al,3                ; only keep pubdef/extdef/comdef/absolute field
    cmp al,2                ; check if previous entry was a pubdef/absolute
    jae fpe_14              ; yes
    test    BYTE PTR es:[15],40h    ; see if a communal
    jne fpe_14              ; yes

; check if weak extdef
    or  al,al               ; declaration flag must be zero for weak extdef
    jne fpe_unres           ; not a weak extdef
    mov bx,es:[0]           ; bx -> default resolution extdef
    jmp SHORT fpe_match     ; retry with new match entry

; unresolved external symbol
fpe_unres:
	push	ds
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    cmp no_match_okay,0     ; check to see if no match okay flag is set
	pop	ds
    jne fpe_14				; no match flag set, don't report warning

    mov di,es:[4]           ; get offset of name in di
    mov es,es:[6]           ; get segment of name in es
    mov dx,OFFSET DGROUP:filename
    mov ax,UNRES_EXT_WARN   ; flag unresolved external
    call    link_warning    ; give warning feedback

fpe_14:
    mov ax,bx               ; get segment value in ax

fpe_15:
    pop di                  ; restore di pointing to name

fpe_ret:
    pop es                  ; restore critical register
    pop si
    ret
find_pubdecl_entry  ENDP

;*****************************
;* PROC2_SEGDEF              *
;*****************************

; pass 2 segdef record processing
; upon entry cx=record length,bp=buffer_end,al=record type
; es:si -> first byte of record past record length
; destroys ax,bx,dx,di
; updates si

proc2_segdef    PROC
    mov al,es:[si]          ; get acbp byte
    mov acbp_byte,al        ; save to memory
    mov dl,al               ; save acbp byte in dl
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_2                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_2:
    test    dl,0e0h         ; check if A bit field is zero
    jne ps_scan_seglen      ; nonzero, no frame number

; A bit field is zero (absolute segment), frame number and offset bytes exist
    mov dl,es:[si]          ; get low byte of frame number
    inc si                  ; scan past low byte frame number
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_3                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_3:
    mov dh,es:[si]          ; get high byte of frame number
    inc si                  ; scan past high byte frame number
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_4                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_4:
    mov frame_number,dx     ; save frame number
    inc si                  ; scan past offset byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_scan_seglen      ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_scan_seglen:
    inc si                  ; scan past low byte of segment length
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_6                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_6:
    inc si                  ; scan past high byte of segment length
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_7                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_7:
    xor ah,ah               ; zero high byte of index
    mov al,es:[si]          ; get low byte of segment name index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_8                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_8:
    cmp al,80h              ; check if two byte index value
    jb  ps_10               ; no, continue
    and al,7fh              ; mask off high bit
    mov ah,al               ; move value to high byte

    mov al,es:[si]          ; get second byte of index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_10               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_10:
    mov segment_name_index,ax   ; save segment name index

    xor ah,ah               ; zero high byte of index
    mov al,es:[si]          ; get low byte of class name index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_11               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_11:
    cmp al,80h              ; check if two byte index value
    jb  ps_13               ; no, continue

    and al,7fh              ; mask off high bit
    mov ah,al               ; move value to high byte
    mov al,es:[si]          ; get second byte of index
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_13               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_13:
    mov class_name_index,ax ; save class name index

; scan the overlay name index value
    mov al,es:[si]          ; get first byte
    inc si                  ; scan past overlay name index
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_13a              ; okay
    call    load_file       ; load next portion of file into buffer, at end position
ps_13a:
    cmp al,80h              ; check if more than one byte index value
    jb  ps_find_segdef      ; no, continue
    inc si                  ; scan past second byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  ps_find_segdef      ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_find_segdef:
    push    es              ; save es critical register
    mov al,1                ; flag that must find entry
    call    find_segdef_entry   ; find segdef entry
    mov al,es:[26]          ; get acbp byte
    and al,0e0h             ; get align field
    jne ps_not_abs          ; not an absolute segment

; absolute segment, bypass segment partition stuff, use segdef entry as
; element in segment partition array
    mov dx,current_segdef
    jmp NEAR PTR ps_part_ptr

ps_not_abs:
    mov ax,es:[22]          ; get segment of first segment partition entry
    mov es,ax               ; es -> segment partition entry

; scan past nonmatching object or library file segment partition entries
ps_part_scan:
    cmp is_inlib,0          ; see if current processing library
    je  ps_use_obj          ; no

ps_idloop1:
    mov ax,es:[6]           ; get low word of file_mod_id
    cmp ax,WORD PTR lib_id  ; compare to low word of library id
    jne ps_13b              ; doesn't match
    mov ax,es:[8]           ; get high word of file_mod_id
    cmp ax,WORD PTR lib_id+2    ; compare to high word of library id
    je  ps_14               ; id's match

ps_13b:
    mov ax,es:[2]           ; get pointer to next entry
    or  ax,ax               ; check if null, no more entries
    mov es,ax
    jne ps_idloop1          ; no, non-null
    jmp SHORT ps_internal   ; no entry match found

ps_use_obj:
    mov dx,current_obj

ps_idloop2:
    cmp dx,es:[6]           ; see if id number for object module matches current object number
    je  ps_14               ; yes
    mov ax,es:[2]           ; get pointer to next entry
    or  ax,ax               ; check if null, no more entries
    mov es,ax
    jne ps_idloop2          ; no, non-null

; internal error, no matching partition entry found
ps_internal:
    mov ax,INTERNAL_ERR     ; put warplink error code in ax
    mov cx,5                ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

; scan past nonmatching index segment partition entries
ps_14:
    mov dx,current_segdef
    mov al,es:[10]          ; get record index
    xor ah,ah               ; zap high byte
    cmp ax,dx               ; see if record index for object module matches current segdef number
    je  ps_expmap_chk       ; yes
    mov ax,es:[2]           ; get pointer to next entry
    mov es,ax
    or  ax,ax               ; check if null, no more entries
    je  ps_internal         ; null, internal error
    jmp SHORT ps_part_scan  ; loop back and try again

; found the proper segment partition entry

ps_expmap_chk:
    cmp is_mapexpand,0      ; check if expanded map file
    je  ps_15               ; no

; write detailed segment map
    call    map_detail_seg

; check if segment partition is in overlaid public table by checking vectored written bit
ps_15:
    cmp is_clarion,0        ; see if clarion switch set
    je  ps_16               ; no
    mov al,es:[15]          ; get flag byte for segment partition entry
    and al,80h              ; see if in an overlay

    je  ps_16
    inc ovl_code_count      ; bump count of overlaid code segments

ps_16:
    xor ax,ax               ; zero out table entry pointer, overwriting unneeded file_mod_id
    mov es:[6],ax
    mov es:[8],ax

    mov al,es:[15]          ; get flag byte for segment partition entry
    and al,20h              ; see if end vectors written
    je  ps_part_ptr         ; no

; look up segment partition entry in overlaid public table
; save segment in old file/module identification word and set record index
; byte to 255 to eliminate possible match later
ps_save_lookup:
    call    find_seg_in_table   ; find segment partition entry in overlaid public table

; keep pointer to segment partition entry (segdef entry for absolute segment)
ps_part_ptr:
    shl dx,1                ; convert dx to word offset
    mov bx,offset DGROUP:seg_partent_indptr  ; set bx to base of array
    add bx,dx               ; bx -> proper array element
    mov [bx],es             ; set array element pointer to segment partition entry segment

    inc current_segdef      ; bump count of current segdef record
    inc si                  ; bump past checksum byte
    cmp si,bp               ; check boundary conditions
    jb  ps_ret              ; okay
    call    load_file       ; load next portion of file into buffer, at end position

ps_ret:
    pop es                  ; restore critical register
    ret
proc2_segdef    ENDP

;*****************************
;* FIND_SEG_IN_TABLE         *
;*****************************

; find segment partition entry in overlaid public table
; upon entry es -> segment partition entry
; BEWARE: uses ds to speed things up, don't make assumptions about
; automatic variable access when making changes
; destroys ax

find_seg_in_table   PROC
    push    bx              ; save critical register
    push    cx
    push    dx
    push    es

    mov dx,es               ; save compare value in dx
    mov ax,first_ovlpubblk_ptr  ; get first allocate overlaid public block
    mov bx,2                ; init block position
    mov ds,ax               ; ds -> public block

; no more overlaid publics for this segment, move to next, if any
fsi_next_seg:
    cmp bx,OVLPUB_BLK_BYSIZE-9  ; see if at end of block
    jb  fsi_1               ; no
    mov ds,ds:[0]           ; get pointer to next block in ax
    mov bx,2                ; init block position

fsi_1:
    mov al,[bx]             ; get flag byte
    cmp al,1                ; see if segment partition entry byte
    je  fsi_get_seg         ; yes
    cmp al,2                ; see if non-overlay class segment partition entry byte
    je  fsi_get_seg         ; yes
    add bx,3                ; move to next position in block
    jmp SHORT fsi_next_seg  ; keep looking for segment partition info

fsi_get_seg:
    inc bx
    mov ax,[bx]             ; get segment partition entry
    mov cx,[bx+2]           ; get total overlaid references in cx
    cmp ax,dx               ; see if current segment partition matches that in entry
    je  fsi_matched         ; yes, save pointer to entry in table
    add bx,8                ; scan past info to first overlaid public
    shr cx,1                ; divide by two (can be double references per entry due to near/far)
    mov ax,cx
    shl cx,1
    add cx,ax               ; get minimum number of bytes to scan past (cx*3)
    add bx,cx
    jmp SHORT fsi_next_seg  ; try for next segment partition entry

fsi_matched:
    add bx,2                ; adjust to proper block offset
    mov ax,ds               ; ax holds block segment
    pop es                  ; restore es -> segment partition entry
    mov es:[6],bx           ; save entry's block offset
    mov es:[8],ax           ; save entry's block segment
    mov BYTE PTR es:[10],255    ; force invalid record index so no mistaken matches

    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    pop dx                  ; restore critical registers
    pop cx
    pop bx
    ret
find_seg_in_table   ENDP

;*****************************
;* FIND_SEGDEF_ENTRY         *
;*****************************

; find segdef entry
; upon entry, al is nonzero if an entry must be found
; return segment of entry in es, null if none
; destroys ax,dx,di,es

find_segdef_entry   PROC
    mov mustfind,al         ; save mustfind flag
    push    si              ; save critical register
    mov ax,segment_name_index   ; get index value for lnames array
    dec    ax               ; make count relative zero
    shl ax,1                ; convert to word index
    shl ax,1                ; convert to doubleword index
    mov si,OFFSET DGROUP:lnames_ent_indptr  ; si -> base of lnames array
    add si,ax               ; si -> array element with segment:offset of name in lnames block
    mov di,[si]             ; get offset of name in di
    mov ax,[si+2]           ; get segment of name in ax
    mov si,OFFSET DGROUP:name_field ; point to slot to place name
    mov dx,ds
    mov ds,ax               ; ds -> name segment in lnames block
    mov es,dx               ; es -> warplink's data
    xchg    si,di           ; di -> name field, si -> name offset in lnames block
    add si,8                ; bump si past hi/low segment:offset pointers in lnames entry

; ds:si -> lnames name, es:di -> warplink data name field
fse_get_name:
    movsb                   ; transfer a char
    cmp BYTE PTR [si-1],0   ; zero char flags end of transfer
    jne fse_get_name        ; not zero, keep transferring

    mov ax,es               ; get warplink's data segment
    mov ds,ax               ; restores ds -> warplink data
    mov si,OFFSET DGROUP:name_field ; si -> name to get hash code of
    call    get_hash        ; get hash code of name
    mov si,OFFSET DGROUP:segdef_hash   ; si -> base of hash pointers to segdef entries
    shl ax,1                ; convert ax to word offset
    add si,ax               ; si points to proper name hash code entry
    cmp WORD PTR [si],0     ; is hash code used (nonzero value)
    jne fse_2               ; yes

; hash code for name unused, internal error
fse_internal:
    cmp mustfind,0          ; see if must find entry
    jne fse_int             ; yes
    xor ax,ax
    mov es,ax               ; null es
    jmp NEAR PTR fse_ret    ; and return

fse_int:
    mov ax,INTERNAL_ERR     ; put warplink error code in ax
    mov cx,6                ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

; hash code used, check for hash collision
fse_2:
    mov dx,[si]             ; get segment pointer to segdef entry

fse_testloop:
    mov es,dx               ; es -> segdef entry

; check if hash code is absolute segment, bypass segment/class name checks if so
    mov al,es:[26]          ; get acbp byte
    and al,0e0h             ; get align field
    je  fse_hash_abs        ; zero, segment is absolute

    mov ax,es:[10]          ; get segment of segment name in lnames block
    mov di,es:[8]           ; get offset of segment name in lnames block
    add di,8                ; adjust for 2 segment:offset pointers
    mov es,ax               ; extra segment holds lnames segment
    mov si,OFFSET DGROUP:name_field ; si -> name to get hash code of

; ds:si -> name, es:di -> name to test against
fse_byteloop:
    mov al,[si]
    or  al,es:[di]          ; see if both values are zero (matched to null terminator)
    je  fse_match           ; strings matched
    cmpsb                   ; compare a nonzero byte in the two names
    je  fse_byteloop        ; bytes match, loop for next byte test

fse_next_entry:
    mov es,dx               ; get segdef entry segment value
    mov dx,es:[20]          ; get next entry
    or  dx,dx               ; check if a null pointer (dx=0)
    jne fse_testloop        ; no, keep checking entries

; internal error, no names found
    jmp SHORT fse_internal

; segment names match, check class names
fse_match:
    mov es,dx               ; es -> segdef entry
    push    ds              ; save data segment value, used
    mov ax,class_name_index ; get index value for lnames array
    dec ax                  ; make count relative zero
    shl ax,1                ; convert to word index
    shl ax,1                ; convert to doubleword index
    mov si,OFFSET DGROUP:lnames_ent_indptr  ; si -> base of lnames array
    add si,ax               ; si -> array element with segment:offset of name in lnames block
    mov ax,[si+2]           ; get segment of name from lnames entry in ax
    mov si,[si]             ; get offset of name from lnames entry in si
    add si,8                ; adjust past 2 doubleword pointers
    mov ds,ax               ; ds:si -> class name in lnames block from index

    mov ax,es:[14]          ; get segment of entry's class name
    mov di,es:[12]          ; get offset of entry's class name
    add di,8                ; adjust past 2 doubleword pointers
    mov es,ax               ; es:di -> test entry's class name in lnames block

fse_matchloop:
    cmpsb                   ; compare a char
    je  fse_3               ; name char matched
    pop ds                  ; match failed, restore segment register
    jmp SHORT fse_next_entry    ; try next entry

fse_3:
    mov al,[si-1]           ; matching zero chars flag successful end of compare
    or  al,es:[di-1]
    jne fse_matchloop       ; not done yet

; class names match, check ACBP bytes
fse_class_match:
    pop ds                  ; restore segment register
    mov es,dx               ; es -> segdef entry

fse_hash_abs:
    mov al,acbp_byte
    mov ah,al
    and ah,0e0h             ; check if absolute segment
    jne fse_not_abs         ; no

; absolute segment, check frame numbers to see if they are the same
fse_is_abs:
    mov ax,frame_number     ; get frame number
    cmp ax,es:[0]           ; compare to entry's frame number
    jne fse_next_entry      ; not the same absolute segment, try next entry
    jmp SHORT fse_ret       ; same absolute segment

fse_not_abs:
    and al,1ch              ; mask out all but combine field of acbp byte
    je  fse_private         ; combine field is private (==0)
    mov al,es:[26]          ; get entry combine type
    and al,1ch
    jne fse_ret             ; both ACBP combineable types
    jmp SHORT fse_next_entry    ; not combineable, try next entry

; current ACBP combine field private, check if entry is private as well
fse_private:
    mov al,es:[26]          ; get entry combine type
    and al,1ch
    je  fse_4               ; both combine fields private types
    jmp SHORT fse_next_entry    ; not both private, try next segdef entry

; check file id and record index of first segment partition entry
fse_4:
    mov es,es:[22]          ; es -> first segment partition entry
    cmp is_inlib,0          ; check if processing library
    jne fse_5               ; yes

; processing object module, check against current object module number
    mov ax,current_obj
    cmp ax,es:[6]           ; check if file id matches
    je  fse_6               ; yes

fse_nomatch:
    jmp NEAR PTR fse_next_entry ; no, try next segdef entry

; processing library, check against library id
fse_5:
    mov ax,es:[6]           ; get low word of file_mod_id
    cmp ax,WORD PTR lib_id  ; compare to low word of library id
    jne fse_nomatch         ; doesn't match
    mov ax,es:[8]           ; get high word of file_mod_id
    cmp ax,WORD PTR lib_id+2    ; compare to high word of library id
    jne fse_nomatch         ; doesn't match

fse_6:
    mov ax,current_segdef
    cmp al,es:[10]          ; check if record index matches
    mov es,dx               ; es -> segdef entry
    je  fse_ret             ; yes
    jmp NEAR PTR fse_next_entry ; no, try next segdef entry

; names match, and ACBP combineable types OR first segment partition entry's file id and record index match
; OR absolute segment and frame numbers match
fse_ret:
    pop si
    ret
find_segdef_entry   ENDP

;*****************************
;* PROC2_GRPDEF              *
;*****************************

; pass 2 grpdef record processing
; upon entry cx=record length,bp=buffer_end,al=record type
; es:si -> first byte of record past record length
; destroys ax,bx,dx,di
; updates si

proc2_grpdef    PROC
    xor ah,ah               ; zero high byte of index
    mov al,es:[si]          ; get group name index byte
    inc si                  ; scan past group name index byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pg_2                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pg_2:
    cmp al,80h              ; see if two byte index
    jb  pg_3                ; no
    and al,7fh              ; mask off high bit
    mov ah,al               ; ah holds high word of index
    mov al,es:[si]          ; get second group name index byte
    inc si                  ; scan past group name index byte
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pg_3                ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pg_3:
    call    find_grpdef_entry  ; find/return group entry segment in ax

    mov bx,OFFSET DGROUP:grp_ent_indptr    ; bx -> base of array pointers to group entries
    mov di,current_grpdef   ; get array element (current grpdef number)
    shl di,1                ; make ax a word offset
    add bx,di               ; bx -> proper array element
    mov [bx],ax             ; save pointer to public declaration entry
    inc current_grpdef      ; bump count of current grpdef
    ret
proc2_grpdef    ENDP

;*****************************
;* FIND_GRPDEF_ENTRY         *
;*****************************

; find group entry for group name
; upon entry ax == group name index byte
; returns segment of entry in ax
; destroys ax,bx,dx,di

find_grpdef_entry   PROC
    push    es              ; save critical register
    dec ax                  ; make group index relative zero
    shl ax,1
    shl ax,1                ; convert ax to double word offset into array
    mov bx,OFFSET DGROUP:lnames_ent_indptr  ; set bx to base of lnames index pointer array
    add bx,ax               ; point to array element
    mov di,[bx]             ; di holds offset
    mov bx,[bx+2]           ; bx holds segment

    mov ax,first_grpblk_ptr ; get segment of first allocated group block
    mov es,ax               ; es -> group block
    xor dx,dx               ; dx hold group entry number

fge_grp_loop:
    cmp dx,es:[0]           ; check if any more entries in group block
    jb  fge_3               ; yes

; no more entries in group block, get next group block
    mov ax,es:[2]           ; get pointer to next group
    or  ax,ax               ; check if null, no more group blocks
    jne fge_2               ; nonnull, get next group block

; internal error, group entry not found
fge_internal:
    mov ax,INTERNAL_ERR     ; put warplink error code in ax
    mov cx,7                ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

fge_2:
    mov es,ax               ; es -> new block
    xor dx,dx               ; re-init group entry number

fge_3:
    push    es              ; save es -> block
    mov ax,es               ; get block segment address
    add ax,dx               ; get first free entry segment value
    inc ax                  ; adjust for block system info size of 1 paragraph
    mov es,ax               ; es -> group entry
    inc dx                  ; bump entry number

    cmp di,es:[4]           ; see if offsets of lnames pointers match
    je  fge_4               ; yes

fge_nomatch:
    pop es                  ; restore es -> block
    jmp SHORT fge_grp_loop  ; try next entry

fge_4:
    cmp bx,es:[6]           ; see if segments of lnames pointer match
    jne fge_nomatch         ; no

; names match, ax holds group entry segment
fge_ret:
    pop bx                  ; trash old es value on stack
    pop es                  ; restore critical register
    ret
find_grpdef_entry   ENDP

END
