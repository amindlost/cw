;*********************************************************************
;*   MLSHARED.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/21/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   routines shared by pass one and pass two                        *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlshared
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
PUBLIC  perform_pass,get_name,get_hash,load_file
PUBLIC  error_bx_pos,error_read_buff_pos,get_curr_obj,print_link_info

; variables
PUBLIC  filename,pass_number,obj_block_ptr
PUBLIC  ems_first_avail,ems_pages_flushed,obj_handle

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   lib_handle:WORD,lib_read_amt:WORD
EXTRN	is_local:BYTE

; initialized local variables

; byte values
use_ems_maps    DB  0       ; nonzero if object modules are, or can be, mapped to EMS pages
EVEN
ems_pages_flushed   DB  0   ; nonzero if EMS pages flushed to make room for ovl, temp file
EVEN

; word values
ems_first_avail DW  4       ; first available ems logical page -- updated as used

.DATA?

; uninitialized local variables

; byte values
EVEN                        ; maximize speed on 8086 and better
filename    DB  128 DUP (?) ; current object module file name
pass_number DB  ?           ; number of pass

; word values
EVEN
obj_handle  DW  ?           ; handle of currently open object file
obj_block_ptr   DW  ?       ; segment of current object module name block
pos_in_list DW  ?           ; position to read name in object name block namelist
ems_map_blk DW  ?           ; segment of block hold 4 word entries for each obj, with EMS page maps

;*****************************
;* Constant data             *
;*****************************

.CONST

proctext_len    DB  proctext_stop-proc_text
proc_text       DB  CR,LF,'*** Processing file: '
proctext_stop   =   $

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,alloc_memory:NEAR
EXTRN   pass1_obj_proc:NEAR,pass2_obj_proc:NEAR
EXTRN   load_buffer:NEAR,read_to_ems:NEAR
EXTRN   decrypt:NEAR,setup_incinfo:NEAR,map_ems_page:NEAR,restore_ems_map:NEAR

;*****************************
;* PERFORM_PASS              *
;*****************************

; perform generic pass 1 and pass 2 object module processing, route to appropriate pass code
; al has number of linker pass (1 or 2) upon entry
; destroys all registers

perform_pass    PROC    NEAR
    mov pass_number,al      ; save pass number to memory variable
    mov current_obj,0       ; init current object module number
    mov ax,first_objblk_ptr
    mov obj_block_ptr,ax    ; init current block pointer to first block

pp_loop:
    mov ax,obj_count        ; get total count of object modules
    cmp ax,current_obj      ; end loop when current equals total
    ja  pp_getobj           ; more object modules
    jmp NEAR PTR pp_ret     ; all done

pp_getobj:
    call    get_curr_obj    ; get current object module in filename

    xor ax,ax
    mov WORD PTR file_pos_adj,ax    ; init file position adjustment
    mov WORD PTR file_pos_adj+2,ax

    call    print_link_info

pp_2:
    cmp pass_number,1       ; see if pass one processing
    jne pp_pass2            ; no, assume pass two

    mov dx,OFFSET DGROUP:filename   ; DS:DX -> ASCIIZ file specification
    mov ax,3d00h            ; open file with read access
    int 21h
    call    restore_ems_map
    jnc pp_noerr            ; no errors
    jmp NEAR PTR dos_error  ; error opening file

pp_noerr:
    mov obj_handle,ax       ; save object module file handle
    mov bx,ax               ; save handle in bx
    call    load_buffer     ; load the file in the i/o buffer

    cmp is_clpinc,0         ; see if clipper incremental link specified
    je  pp_noinc            ; no
    call    setup_incinfo   ; setup incremental info for file

pp_noinc:
    call    pass1_obj_proc  ; perform pass 1 object module processing

; if using EMS, check if can map out page(s) used by i/o buffer
    call    ems_page_mapout
    jmp SHORT pp_closefile  ; skip over pass 2 code

pp_pass2:

; if using EMS, check if object modules have page(s) to map in
    call    ems_page_mapin
    or  al,al               ; nonzero return if page was successfully mapped in
    je  pp_loadfile         ; not mapped in, load file
    call    pass2_obj_proc  ; perform pass 2 object module processing
    jmp SHORT pp_bumpobj

pp_loadfile:
    mov dx,OFFSET DGROUP:filename   ; DS:DX -> ASCIIZ file specification
    mov ax,3d00h            ; open file with read access
    int 21h
    call    restore_ems_map
    jnc pp_noerr2           ; no errors
    jmp NEAR PTR dos_error  ; error opening file

pp_noerr2:
    mov obj_handle,ax       ; save object module file handle
    mov bx,ax               ; save handle in bx
    call    load_buffer     ; load the file in the i/o buffer
    call    pass2_obj_proc  ; perform pass 2 object module processing

pp_closefile:
    mov bx,obj_handle       ; get file handle of open object module file
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

pp_bumpobj:
    inc current_obj         ; bump number of current object module
    jmp NEAR PTR pp_loop    ; loop back for next object module

pp_ret:
    ret
perform_pass    ENDP

;*****************************
;* PRINT_LINK_INFO           *
;*****************************

; print linker files being processed, if option set
; upon entry filename holds the file name
; destroys ax,bx,cx,dx,si

print_link_info PROC
    cmp is_linkinfo,0       ; see if linker information to be printed
    je  pli_ret             ; no

    mov bx,OFFSET DGROUP:proc_text
    mov cl,[bx-1]           ; get length of string to print
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h

    mov si,OFFSET DGROUP:filename   ; print chars in filename one at a time

pli_filename:
    mov dx,si
    lodsb                   ; get char from filename
    or  al,al               ; see if null terminator
    je  pli_ret             ; done
    mov cx,1
    mov ah,40h              ; write to device
    int 21h
    jmp SHORT pli_filename

pli_ret:
    ret
print_link_info ENDP

;*****************************
;* EMS_PAGE_MAPOUT           *
;*****************************

; map pages out to EMS if possible
; if first object module, allocate block to hold object module mapped out pages
; destroys ax,bx,cx,dx,si,di,es

ems_page_mapout PROC
    xor ax,ax
    cmp is_no_ems,al        ; see if EMS used
    jne eout_ret            ; no

    cmp any_ddl,al          ; see if DDL's used
    jne eout_ret            ; yes

    cmp is_inlib,al         ; see if processing library
    je  eout_1              ; no

eout_ret:
    ret

eout_1:
    cmp current_obj,ax      ; see if first object module (== 0)
    jne eout_2              ; no

; first object module, each module will have 4 word entries to hold EMS pages
eout_first:
    mov bx,obj_count        ; get object module count
    add bx,1                ; round up for 8-byte to paragraph conversion
    shr bx,1                ; bx holds paragraphs to allocate
    call    alloc_memory
    mov ems_map_blk,ax      ; save segment of ems map block for object modules
    mov use_ems_maps,1      ; set ems maps used flag

eout_2:
    mov di,current_obj
    shl di,1
    shl di,1
    shl di,1                ; x8, di holds offset into ems map block for object module
    mov es,ems_map_blk      ; es:di -> proper obj ems map block entry
    mov ax,-1               ; init to all -1's to show unused
    stosw
    stosw
    stosw
    stosw                   ; all four entries init'ed
    sub di,8                ; point es:di back to beginning of entry

    mov ax,WORD PTR file_pos_adj
    or  ax,WORD PTR file_pos_adj+2  ; see if buffer wrapped
    jne eout_ret            ; yes, more than 4 pages used, cannot map out
    mov cx,1                ; init count of pages to map out

; si holds final byte used+1, used to compute number of pages to map out
eout_loop:
    sub si,16384            ; back off one page
    jbe eout_3              ; si ended on this page
    inc cx                  ; bump count of pages to map out
    jmp SHORT eout_loop     ; loop for next page check

eout_3:
    cmp ems_page_avail,4    ; must have over four free pages
    jbe eout_ret            ; not enough

    mov ax,ems_page_avail   ; get count of free pages
    sub ax,4                ; back off four free pages
    sub ax,cx               ; see if enough free pages to save this obj in ems
    jc  eout_ret            ; no

; map obj to ems pages, bring in free pages
    add ax,4                ; adjust back for four free pages
    mov ems_page_avail,ax   ; save new count of free pages

    mov ax,cx               ; save count of pages mapped out
    mov si,OFFSET DGROUP:ems_currmap    ; ds:si -> physical page entries
    rep movsw               ; move all used physical pages

    mov cx,ax               ; restore count of pages mapped out
    xor al,al

eout_loop2:
    push    ax              ; save physical page
    mov bx,ems_first_avail  ; map in first available free page
    inc ems_first_avail     ; bump first available page
    call    map_ems_page
    pop ax                  ; restore physical page
    inc al                  ; bump physical page
    loop    eout_loop2      ; loop until all free pages are mapped in

    ret
ems_page_mapout ENDP

;*****************************
;* EMS_PAGE_MAPIN            *
;*****************************

; returns al==0 if cannot map in from EMS, nonzero if page mapped from
; EMS and no file load required
; destroys ax,bx,cx,dx,si,es

ems_page_mapin  PROC
    cmp is_no_ems,0         ; see if EMS used
    jne ein_usefile         ; no

    cmp is_inlib,0          ; see if processing library
    jne ein_usefile         ; yes

    cmp use_ems_maps,0      ; see if EMS maps used
    je  ein_usefile         ; no

    cmp ems_pages_flushed,0 ; see if EMS pages flushed to store ovl, temporary file
    jne ein_usefile         ; yes

; it may be possible to map in page from EMS, check status
    mov si,current_obj
    shl si,1
    shl si,1
    shl si,1                ; x8, si holds offset into ems map block for object module
    mov es,ems_map_blk      ; es:si -> proper obj ems map block entry
    xor cl,cl               ; cl holds physical page

ein_loop:
    lods    WORD PTR es:0   ; get map page
    cmp ax,-1               ; see if not used (if not used, then no following ones are either)
    je  ein_chksuccess      ; not used
    mov bx,ax               ; get logical page in bx
    mov al,cl               ; get physical page in al
    call    map_ems_page    ; map in used pages
    inc cl                  ; bump physical page
    cmp cl,4                ; see if all physical pages mapped
    jb  ein_loop            ; not yet

ein_chksuccess:
    or  cl,cl               ; if cl is zero then no pages mapped, not successful
    je  ein_usefile         ; no pages mapped in

; at least one page mapped in, success
    mov al,1                ; return one to show success
    ret

ein_usefile:
    xor al,al               ; return zero to show must load buffer from file
    ret
ems_page_mapin  ENDP

;*****************************
;* GET_CURR_OBJ              *
;*****************************

; get current object module name in filename
; destroys ax,si,di,es

get_curr_obj    PROC
    cmp current_obj,0       ; see if first object file, requires position initialization
    jne gco_2               ; nope

gco_5:
    mov pos_in_list,4       ; init position in name list to first position in block

gco_2:
    mov si,pos_in_list      ; si will offset into block
    mov ax,obj_block_ptr    ; get current object name block
    mov es,ax               ; point extra segment at object name block
    mov ax,OBJ_NAMBLK_BYSIZE    ; size of block in bytes
    sub ax,es:[0]           ; minus free space, ax == end of used namelist
    cmp ax,si               ; check that position in list is below end
    ja  gco_3               ; not at end yet, pull name from this block's namelist

    mov ax,es:[2]           ; get pointer to next block
    mov obj_block_ptr,ax    ; save back to memory variable
    or  ax,ax               ; check that is not null
    jne gco_5               ; non-null, next block exists, loop back and try with it

; A WarpLink internal error has occurred, no more object names were
; available before the count of all object modules was complete
    mov ax,INTERNAL_ERR     ; put warplink error code in ax
    mov cx,2                ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

gco_3:
	lods	BYTE PTR es:[0]	; get flag byte
    and al,0c0h             ; only get overlay bits
    mov obj_ovl_flag,al     ; save overlay status for module
    and al,40h              ; get nonvector root call status
    mov nonovl_rvect,al     ; save it to global variable
    mov di,es               ; get object name block segment in di
    mov ax,ds
    mov es,ax               ; es -> warplink data area
    mov ds,di               ; ds -> object name block
    mov di,OFFSET DGROUP:filename   ; es:di -> destination of object module name

gco_6:
    movsb                   ; transfer a char from the block to filename
    cmp BYTE PTR [si-1],0   ; zero byte signals end of transfer
    jne gco_6               ; nonzero, keep looping

    mov ax,es
    mov ds,ax               ; ds -> warplink data

    mov pos_in_list,si      ; si points to next name or end, save back to memory
    ret
get_curr_obj    ENDP

;*****************************
;* GET_NAME                  *
;*****************************

; es:si -> string upon entry
; ds:di -> warplink data location to place string
; bp == buffer end (buffer_end)
; cx == length of current record
; is_local nonzero if local variable (mangle name to unique)
; destroys ax,dx,di
; updates si to point past string
; updates cx to record length after string

get_name        PROC
    mov dx,cx               ; save object record length in dx
    mov cl,es:[si]          ; get 1-byte length of name
    xor ch,ch               ; zap high byte
    push    cx              ; save number of bytes to transfer
    inc si                  ; point to first char of name
    dec dx                  ; decrement record length to parse
    jcxz    gn_out          ; zero length name, no name chars to transfer
    cmp si,bp               ; check boundary conditions
    jae gn_load1            ; out of bounds

gn_1:
    cmp is_casesense,0      ; check to see if string should go to all caps
    mov ax,es
    mov ds,ax
    mov ax,DGROUP
    mov es,ax               ; swap es and ds for string operations
    je  gn_5                ; not case sensitive, convert string to caps

IFNDEF DEMO
    mov ax,si
    add ax,cx               ; get final char position of string
    jc  gn_3                ; overflow, buffer end will be reached during transfer
    cmp ax,bp
    jae gn_3                ; buffer end will be reached during transfer

; names are case sensitive, no alteration (to all caps) of chars occurs
; buffer end will not be reached during transfer
; this allows a straight REP MOVS memory move
    shr cx,1                ; convert to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
    jmp SHORT gn_seg_restore    ; bypass char by char transfer code

; transfer to destination, case sensitive, buffer end reached sometime during transfer
gn_3:
    movsb                   ; transfer a byte
    cmp si,bp               ; check boundary conditions
    jae gn_load3            ; out of bounds

gn_4:
    loop    gn_3            ; loop back for next char transfer
    jmp SHORT gn_seg_restore
ENDIF

; case insensitive, convert lowercase to upper, check for boundary conditions
gn_5:
    lodsb                   ; get byte to transfer
    cmp al,'a'              ; check lowercase lower boundary
    jb  gn_6                ; not a lowercase letter
    cmp al,'z'              ; check lowercase upper boundary
    ja  gn_6                ; not a lowercase letter
    sub al,20h              ; convert to uppercase

gn_6:
    stosb                   ; transfer char
    cmp si,bp               ; check boundary conditions
    jae gn_load2            ; out of bounds

gn_7:
    loop    gn_5            ; loop back for next char transfer

gn_seg_restore:
    mov ax,es
    mov cx,ds
    mov ds,ax               ; restore ds -> warplink data
    mov es,cx               ; restore es

; 12/22/92
gn_out:
	cmp	is_local,0			; see if local variable
	jne	gn_local			; yes

gn_nullit:
    mov BYTE PTR [di],0     ; null terminate the destination string
    mov cx,dx               ; restore remaining record length to cx
    pop ax                  ; get bytes transferred
    sub cx,ax               ; update record length
    ret

gn_load1:
    call    load_file       ; load next portion of file into buffer, at end position
    jmp SHORT gn_1

gn_load2:
    call    load_file
    jmp SHORT gn_7

IFNDEF DEMO
gn_load3:
    call    load_file
    jmp SHORT gn_4
ENDIF

; 12/22/92
; local variable, mangle name by adding unique identifier at end
gn_local:
	cmp	is_inlib,0			; see if processing library
	jne	gn_loclib			; yes

; mangle based on current_obj word variable, set high word to 4001h
	mov	ax,current_obj		; (current_obj << 1) | 0x8001
	shl	ax,1
	or	ax,8001h
	mov	[di],ax
	add	di,2
	mov	[di],4001h
	add	di,2
	jmp	SHORT gn_nullit

; mangle based on lib_id dword variable
gn_loclib:
	push	dx
	mov	ax,WORD PTR lib_id
	mov	dx,WORD PTR lib_id+2
	shl	ax,1
	rcl	dx,1				; high word picks up rolled off high bit low word
	shl	dx,1
	or	ax,8001h
	mov	[di],ax
	add	di,2
	or	dx,08001h			; set high bit with libraries
	mov	[di],dx
	add	di,2
	pop	dx
	jmp	SHORT gn_nullit

get_name        ENDP

;*****************************
;* GET_HASH                  *
;*****************************

; returns 10-bit hashcode value in ax
; ds:si -> string upon entry
; destroys ax,si

get_hash    PROC    NEAR
    push    dx              ; save critical registers
    xor dx,dx               ; dx==hashcode, init to zero
    mov ah,dl               ; zero high byte of char value

gh_2:
    lodsb                   ; get char from string
    or  al,al               ; check for null terminator in string
    je  gh_out              ; hashcode computed
    add dx,ax               ; add in character value byte, ignore overflow
    jmp SHORT gh_2          ; loop until end of string

gh_out:
    mov ax,dx               ; ax = hashcode
    and ax,03ffh            ; make 10-bit value
    pop dx
    ret
get_hash    ENDP

;*****************************
;* LOAD_FILE                 *
;*****************************

; load next chunk of file containing object records into file buffer
; updates si to point to first read in char
; updates bp to point to new buffer_tail
; destroys no other registers

load_file   PROC
    push    ax              ; save critical registers
    push    bx
    push    cx
    push    dx
    push    ds
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    cmp si,buffer_end       ; see if buffer end overflow or buffer tail overflow
    jae lf_3                ; buffer end overflow

; current buffer position >= buffer_tail and <= buffer_end
    mov ax,buffer_tail
    cmp prev_flag,0         ; see if previous record needs to be preserved
    je  lf_2                ; no
    cmp ax,prev_read_ptr    ; see if tail is at previous record position
    je  lf_mem              ; yes, out of memory

lf_2:
    mov buffer_head,ax      ; update buffer head to old buffer tail
    mov ax,buffer_end       ; get buffer end as new tail position
    cmp prev_flag,0         ; see if need to save previous record
    je  lf_2a               ; no
    mov bx,prev_read_ptr
    cmp bx,buffer_head      ; check previous record position against buffer head
    jb  lf_2a               ; below buffer head, no overwrite problem
    cmp bx,ax               ; check previous record position against new tail position
    jae lf_2a               ; above or equal to buffer tail, no overwrite
    mov ax,prev_read_ptr    ; set tail so that previous record won't be overwritten

lf_2a:
    cmp is_inlib,0          ; see if processing library
    je  to_lf_shared        ; no
    cmp udl_proc_pass,1     ; see if udl processing
    je  lf_noddl            ; yes
    cmp any_ddl,0           ; see if creating/using DLL (all libraries treated as object modules)
    jne to_lf_shared		; yes

lf_noddl:
    mov bx,LIB_MAX_READ
    add lib_read_amt,bx     ; reread on same library module, bump amount read this time by LIB_MAX_READ bytes
	jc	lf_overflow			; can't read that much
	cmp	lib_read_amt,0ff00h	; see if past max buffer
	jbe	lf_2b				; no

lf_overflow:
	sub	lib_read_amt,bx		; adjust back

lf_2b:
    mov bx,buffer_tail      ; get old buffer tail
    add bx,lib_read_amt     ; add maximum buffer read for library
    jc  lf_shared           ; overflow, new tail position won't exceed lib_read_amt from old tail position
    cmp ax,bx               ; see if new tail position is more than lib_read_amt from old position
    jbe lf_shared           ; no
    mov ax,bx               ; use lib_read_amt from old tail position as new tail instead

to_lf_shared:
    jmp SHORT lf_shared     ; jump to shared code

; current buffer position >=buffer_end
lf_3:
    cmp prev_flag,0         ; see if previous record needs to be preserved
    je  lf_4                ; no
    cmp prev_read_ptr,0     ; see if previous record started at beginning of file buffer
    jne lf_4                ; no

; previous record must be preserved and takes up remaining file buffer, out of memory
lf_mem:
    mov ax,8                ; force DOS out of memory error
    jmp NEAR PTR dos_error

lf_4:
    mov ax,buffer_end       ; add buffer end to file position adjustment variable
    add WORD PTR file_pos_adj,ax    ; update low word
    adc WORD PTR file_pos_adj+2,0   ; carry to high word
    mov buffer_head,0       ; set buffer head to physical beginning of buffer
    mov ax,read_buff_ptr    ; get current beginning of record as new tail position

    cmp prev_flag,0         ; see if previous record used
    je  lf_5                ; no
    mov ax,prev_read_ptr    ; use previous record position as tail

lf_5:
    cmp is_inlib,0          ; see if processing library
    je  lf_shared           ; no
    cmp udl_proc_pass,1     ; see if udl processing
    je  lf_noddl2           ; yes
    cmp any_ddl,0           ; see if creating/using DLL (all libraries treated as object modules)
    jne lf_shared           ; yes

lf_noddl2:
    mov bx,LIB_MAX_READ
    add lib_read_amt,bx     ; reread on same library module, bump amount read this time by LIB_MAX_READ bytes
	jc	lf_overflow2		; can't read that much
	cmp	lib_read_amt,0ff00h	; see if past max buffer
	jbe	lf_5b				; no

lf_overflow2:
	sub	lib_read_amt,bx		; adjust back

lf_5b:
    mov bx,lib_read_amt     ; get maximum tail for library
    cmp ax,bx               ; see if new tail position is more than lib_read_amt from buffer start (at 0)
    jbe lf_shared           ; no
    mov ax,bx               ; use lib_read_amt from buffer start (at 0) as new tail

lf_shared:
    mov buffer_tail,ax      ; update buffer tail
    mov bp,ax               ; update buffer_tail bp register value
    sub ax,buffer_head      ; get difference between head and tail for bytes to load
    cmp is_inlib,0          ; see if processing library
    je  lf_7                ; no
    mov bx,lib_handle       ; use library file handle
    jmp SHORT lf_8

lf_7:
    mov bx,obj_handle       ; use object module handle

lf_8:
    mov cx,ax               ; get number of bytes to read
    mov dx,buffer_head
    mov si,dx               ; update si -> first read in char
    mov ax,buffer_base
    push    ds
    mov ds,ax               ; ds:dx -> buffer area to load into

    call    read_to_ems     ; read file, transfer to EMS if necessary

    pop ds

lf_ret:
    call    decrypt         ; decrypt if SmartMem library
    cmp cx,ax               ; see if all bytes were read
    je  lf_ret2             ; yes

    mov eof_flag,1          ; set end of file flag

lf_ret2:
    pop ds                  ; restore critical registers
    pop dx
    pop cx
    pop bx
    pop ax
    ret
load_file   ENDP

;*****************************
;* ERROR_BX_POS              *
;*****************************

; linker error, file position of error in bx
; terminates WarpLink, all non-error reporting registers are trashable
; ax holds error code upon entry
; cx holds associated error value, if any
; ds:dx -> file name

error_bx_pos    PROC
    cmp si,bx               ; check if si is less than bx (buffer wrapped)
    jae er_2                ; no

; back buffer_end bytes off of file_pos_adj, error occurred before buffer wrap
    mov di,buffer_end
    sub WORD PTR file_pos_adj,di    ; update low word
    sbb WORD PTR file_pos_adj+2,0   ; borrow to high word

er_2:
    mov si,bx               ; get proper record offset
    jmp NEAR PTR link_error ; transfer control to error handler
error_bx_pos    ENDP

;*****************************
;* ERROR_READ_BUFF_POS       *
;*****************************

; linker error, file position of error in read_buff_ptr
; terminates WarpLink, all non-error reporting registers are trashable
; ax holds error code upon entry
; cx holds associated error value, if any
; ds:dx -> file name

error_read_buff_pos PROC
    cmp si,read_buff_ptr    ; check if si is less than read_buff_ptr (buffer wrapped)
    jae erb_2               ; no

; back buffer_end bytes off of file_pos_adj, error occurred before buffer wrap
    mov di,buffer_end
    sub WORD PTR file_pos_adj,di    ; update low word
    sbb WORD PTR file_pos_adj+2,0   ; borrow to high word

erb_2:
    mov si,read_buff_ptr    ; get proper record offset
    jmp NEAR PTR link_error ; transfer control to error handler
error_read_buff_pos ENDP

END
