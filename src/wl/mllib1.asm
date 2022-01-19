;*********************************************************************
;*   MLLIB1.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          02/04/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   library processing part 1                                       *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mllib1
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
PUBLIC  proc1_libs,lib_router,get_lib_sys_info,remove_dup_pages
PUBLIC  make_libmod_entry
PUBLIC	sort_lib_pages

; variables
PUBLIC  lib_handle,use_libs,lib_page_size,lib_page_num,lib_block_ptr
PUBLIC  dir_pages,lib_pages_count,dir_offset

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

EXTRN   filename:BYTE,name_field:BYTE,module_flag:BYTE
EXTRN   ems_first_avail:WORD,obj_handle:WORD,lib_read_amt:WORD
EXTRN	writing_qlk_flag:BYTE,rsp_line:BYTE,pos_in_list:WORD
EXTRN	reading_qlk_flag:BYTE,no_qlk_modules:BYTE

; initialized local variables

; byte values
EVEN                        ; maximize speed on 8086 and better
use_libs    DB  0           ; nonzero if libs used (not merely present)
EVEN

.DATA?

; uninitialized local variables

; byte values
EVEN
all_resolved    DB  ?       ; nonzero if total symbols == public symbols
EVEN
new_lib DB  ?               ; nonzero if new library being processed
EVEN
proc_flag   DB  0           ; nonzero if all pass 1 libraries processed at least once (for module processing)
EVEN

; word values
lib_handle  DW  ?           ; handle of currently open library
lib_block_ptr   DW  ?       ; segment of current object module name block
lib_page_size   DW  ?       ; library page size
lib_page_num    DW  ?       ; library page number of public entry
dir_pages   DW  ?           ; number of directory pages
directory_size  DW  ?       ; size of file i/o buffer dedicated to library directory pages in paragraphs
dir_buffer_size DW  ?       ; paragraph size of buffer for directory pages in file i/o buffer
dir_buff_bysize DW  ?       ; byte size of buffer for directory pages in file i/o buffer
dir_buff_end    DW  ?       ; byte location of last used buffer address for directory pages
dir_paras_read  DW  ?       ; number of directory paragraphs read
lib_pages_count     DW  ?   ; number of library page pointers stored in lib_page_storage
lib_modlen_ptr  DW  ?       ; segment pointer to current library module file length

; doubleword values
prev_sym_count  DD  ?       ; count of public symbols prior to all passes of all libraries
lib_1pass_sym   DD  ?       ; count of total symbols prior to one pass of single library
lib_allpass_sym DD  ?       ; count of total symbols prior to one pass of all libraries
dir_offset  DD  ?           ; directory offset in library file

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR,get_hash:NEAR
EXTRN   load_buffer:NEAR,get_name:NEAR,alloc_memory:NEAR,pass1_obj_proc:NEAR
EXTRN   pass2_lib_proc:NEAR,get_curr_lib:NEAR
EXTRN   load_libmod:NEAR,print_link_info:NEAR
EXTRN   decrypt:NEAR,setup_incinfo:NEAR,read_to_ems:NEAR
EXTRN   restore_ems_map:NEAR,map_ems_page:NEAR
EXTRN	write_qlk_libname:NEAR,write_qlk_mod_info:NEAR,write_qlk_lib_info:NEAR
EXTRN	read_qlk_libname:NEAR,read_qlk_mod_info:NEAR,read_qlk_unres:NEAR

IFNDEF DEMO
EXTRN	ddl_lib_pass:NEAR
ENDIF

;*****************************
;* PROC1_LIBS                *
;*****************************

; main driver for pass 1 processing of libraries
; all registers destroyed except for ds

proc1_libs      PROC

IFNDEF DEMO
    cmp udl_proc_pass,2     ; see if pass 2 udl processing
    jne pl_2                ; no
    jmp NEAR PTR ddl_lib_pass   ; specially process udl library modules
ENDIF

pl_2:
    cmp lib_count,0         ; see if any libraries to process
    jne pl_trylibs          ; yes
    ret                     ; no libraries to process

pl_trylibs:
    mov all_resolved,0      ; zero init all symbols resolved flag
    les ax,pub_sym_count    ; get count of public symbols prior to library processing
    mov WORD PTR prev_sym_count,ax  ; save count for later comparison
    mov WORD PTR prev_sym_count+2,es

    mov al,1                ; give lib_router procedure pass 1 flag
    call    lib_router      ; initialize data and route to proper library code

    mov ax,WORD PTR prev_sym_count+2
    cmp ax,WORD PTR pub_sym_count+2 ; see if high word count of public symbols has changed
    ja  pl_uselibs          ; yes, libraries are used
    mov ax,WORD PTR prev_sym_count
    cmp ax,WORD PTR pub_sym_count  ; see if low word count of public symbols has changed
    jne pl_uselibs          ; yes
    ret                     ; no, leave use_libs flag zero

pl_uselibs:
    mov use_libs,1          ; flag to use libraries
    ret
proc1_libs      ENDP

;*****************************
;* LIB_ROUTER                *
;*****************************

; initialize and route to proper library code for the two passes
; upon entry al contains pass number (1 or 2)
; all registers destroyed except for ds

lib_router  PROC
    xor ah,ah
    mov tmod_name,ah        ; zero out library t-module name
    mov is_inlib,1          ; flag that processing libraries

IFNDEF DEMO
    cmp udl_proc_pass,1     ; see if udl processing
    je  lr_chkpass          ; yes
    cmp any_ddl,ah          ; see if creating/using DDL's
    jne lr_init             ; yes, bypass regular library stuff
ENDIF

lr_chkpass:
    cmp al,1                ; see if pass one processing
    je  lr_lib_pass         ; yes

    call    pass2_lib_proc  ; perform pass 2 library processing
    jmp NEAR PTR lr_ret     ; and return

lr_lib_pass:
    les ax,tot_sym_count    ; get count of total symbols prior to one pass on all libraries
    mov WORD PTR lib_allpass_sym,ax ; save count for later comparison
    mov WORD PTR lib_allpass_sym+2,es

lr_init:
    mov current_lib,0       ; init current library module number
    mov ax,first_libblk_ptr
    mov lib_block_ptr,ax    ; init current block pointer to first block

lr_new_lib:
    mov new_lib,1           ; set new library flag
	cmp	reading_qlk_flag,0	; see if reading library modules from QLK file
	je	lr_no_qlk			; no

; reading from QLK file
	call	read_qlk_libname	; get the library
	jc	lr_qlkexh			; done processing libraries
	cmp	no_qlk_modules,0	; see if any modules in QLK file for library
	je	lr_showname			; yes
	jmp	NEAR PTR lr_inclib	; no, try next library

; qlk libraries are exhausted, see if done processing
lr_qlkexh:
	call	read_qlk_unres
    xor ax,ax
	mov	WORD PTR lib_allpass_sym,ax	; zero in case of later passes needed
	mov	WORD PTR lib_allpass_sym+2,ax
	cmp	reading_qlk_flag,al	; see if still reading from QLK
	je	to_lr_inclib		; no, now writing to QLK file

; done processing, successful
	jmp	NEAR PTR lr_pass1_done

lr_no_qlk:
    call    get_curr_lib    ; get current library in filename, re/set library module flag

lr_showname:
    call    print_link_info ; print library name, if applicable

; check if library module, if so, don't process if not first processing pass
    xor al,al
    cmp module_flag,al      ; see if library module
    je  lr_openlib          ; no
    cmp proc_flag,al        ; see if first processing pass
	je	lr_openlib			; yes

to_lr_inclib:
	jmp	NEAR PTR lr_inclib	; no, don't reprocess library module

lr_openlib:
    mov dx,OFFSET DGROUP:filename   ; DS:DX -> ASCIIZ file specification
    mov ah,3dh              ; open file with read access
    int 21h
    call    restore_ems_map
    jnc lr_lib_open         ; no errors
    jmp NEAR PTR dos_error  ; error occurred

lr_lib_open:
    mov lib_handle,ax       ; save library file handle
	cmp	writing_qlk_flag,0	; see if creating qlk file
	je	lr_modchk			; no
	call	write_qlk_libname	; yes, write the just opened file name

lr_modchk:
	cmp	reading_qlk_flag,0	; see if reading library module info from QLK file
	jne	lr_ismod			; yes
    cmp module_flag,0       ; see if library module
    je  lr_same_lib         ; no

; library module processing
lr_ismod:
    call    lib_mod_proc    ; perform pass 1 library module processing
    jmp SHORT lr_closelib   ; bypass regular library code

lr_same_lib:
IFNDEF DEMO
    cmp any_ddl,0           ; see if creating/using DDL's
    je  lr_noddl            ; no
    cmp udl_proc_pass,1     ; see if udl processing
    je  lr_noddl            ; yes, process normally

; DDL processing
    mov ax,lib_handle       ; get file handle of open library
    mov obj_handle,ax       ; save into obj_handle for load_file proc
    mov bx,ax               ; get file handle into bx for load_buffer proc
    call    load_buffer     ; load library into i/o buffer
    call    pass1_obj_proc  ; pass 1 process loaded library module
    jmp SHORT lr_closelib
ENDIF

lr_noddl:
    les ax,tot_sym_count    ; get count of total symbols prior to pass on single library
    mov WORD PTR lib_1pass_sym+2,es
    mov WORD PTR lib_1pass_sym,ax   ; save count for later comparison

    call    pass1_lib_proc  ; perform pass 1 library processing
    xor ax,ax

    cmp all_resolved,al     ; see if all symbols resolved by library
    je  lr_notres           ; no
    cmp proc_flag,al        ; see if first processing pass
    jne lr_pass1_done       ; no, library modules are all processed

lr_notres:
    mov ax,WORD PTR lib_1pass_sym+2
    cmp ax,WORD PTR tot_sym_count+2 ; see if high word count of total symbols has changed
    ja  lr_same_lib         ; yes, libraries modules were used pass of one library
    mov ax,WORD PTR lib_1pass_sym
    cmp ax,WORD PTR tot_sym_count   ; see if low word count of total symbols has changed
    jne lr_same_lib         ; yes

    cmp lib_count,1         ; see if only one library
    je  lr_pass1_done       ; yes, skip useless closing and reopening it

lr_closelib:
    mov bx,lib_handle       ; get file handle of open library
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

lr_inclib:
    inc current_lib         ; bump number of current library
    mov ax,lib_count        ; get total count of libraries
    cmp ax,current_lib      ; end loop when current equals total
    jbe lr_proc_once        ; all libraries processed at least once
    jmp NEAR PTR lr_new_lib ; not all libraries processed, loop back for next library

; all library processed at least once
lr_proc_once:
    xor ax,ax

IFNDEF DEMO
    cmp udl_proc_pass,1     ; see if udl processing
    je  lr_udl              ; yes
    cmp any_ddl,al          ; see if creating/using DDL's
    jne lr_pass1_done       ; yes, only one time thru
ENDIF

lr_udl:
    mov is_msextlib,al      ; only use extended dictionary once to avoid collisions
    inc ax
    mov proc_flag,al        ; flag all libraries processed at least once
    mov ax,WORD PTR lib_allpass_sym+2
    cmp ax,WORD PTR tot_sym_count+2 ; see if high word count of total symbols has changed
    je  lr_3                ; no, check low word
    jmp NEAR PTR lr_lib_pass    ; yes, libraries modules were used during pass of all library

lr_3:
    mov ax,WORD PTR lib_allpass_sym
    cmp ax,WORD PTR tot_sym_count   ; see if low word count of total symbols has changed
    je  lr_pass1_done       ; no
    jmp NEAR PTR lr_lib_pass    ; yes

lr_pass1_done:
    mov bx,lib_handle       ; get file handle of last library
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

lr_ret:
    mov is_inlib,0          ; done, reset library processing flag
    ret
lib_router  ENDP

;*****************************
;* LIB_MOD_PROC              *
;*****************************

; pass 1 library module processing
; destroys all registers except ds

lib_mod_proc    PROC

; get match of library page from library module entries
lmp_mainloop:
    mov lib_read_amt,0      ; init the library read amount
	cmp	reading_qlk_flag,0	; see if reading info from QLK file
	je	lmp_notqlk			; no

; reading module info from QLK file
lmp_readqlk:
	call	read_qlk_mod_info
	jnc	lmp_modok			; read module info okay
	ret						; no more modules for this library

; create an entry for this QLK module
lmp_modok:
	call	make_libmod_entry
	jmp	SHORT lmp_match		; bypass search for proper entry

lmp_notqlk:
    mov es,first_libent_ptr ; es -> first library module entry
    mov ax,current_lib

lmp_searchloop:
    cmp ax,es:[0]           ; see if current library matches this entry
    je  lmp_match           ; yes
    mov es,es:[30]          ; es -> next entry
    jmp SHORT lmp_searchloop

lmp_match:
    mov ax,es:[4]           ; get library page size
    mov lib_page_size,ax
    mov ax,es:[2]           ; get library page number
    mov lib_page_num,ax
    mov lib_modlen_ptr,es   ; save current library module for length update

; call library load and go routine
    call    lib_load_and_go
	cmp	reading_qlk_flag,0	; see if reading info from QLK file
	jne	lmp_readqlk			; yes, bypass this other stuff

; copy filename to rsp_line (temporary storage)
	push	ds
	pop	es
	mov	di,OFFSET DGROUP:rsp_line
	mov	si,OFFSET DGROUP:filename

lmp_transloop:
	movsb
	cmp	BYTE PTR [si-1],0	; see if at null terminator
	jne	lmp_transloop		; not yet, more transferring

    inc current_lib			; bump number of current library
    mov ax,lib_count		; get total count of libraries
    cmp ax,current_lib		; end loop when current equals total
	jbe	lmp_out				; no more
	push	pos_in_list		; save original name lookup values in case next is not a match
	push	lib_block_ptr
    call    get_curr_lib    ; get current library in filename, re/set library module flag
    cmp module_flag,0		; see if library module
	jne	lmp_ismod			; yes
	pop	lib_block_ptr		; restore name lookup values
	pop	pos_in_list
	jmp	SHORT lmp_out		; and leave

; both are modules, see if filename is same as old
; NOTE: lib_block_ptr and pos_in_list still on stack
lmp_ismod:
	push	ds
	pop	es
	mov	di,OFFSET DGROUP:rsp_line
	mov	si,OFFSET DGROUP:filename

lmp_cmploop:
	cmpsb
	jne	lmp_nomatch			; file name match failed
	cmp	BYTE PTR [si-1],0	; see if at null terminator
	jne	lmp_cmploop			; not yet, more comparing
	add	sp,4				; throw away name lookup values saved on stack
	jmp	NEAR PTR lmp_mainloop	; match, process this module without closing and opening

lmp_nomatch:
	pop	lib_block_ptr		; restore name lookup values
	pop	pos_in_list

lmp_out:
	dec	current_lib			; adjust current library for main loop increment
    ret
lib_mod_proc    ENDP

;*****************************
;* PASS1_LIB_PROC            *
;*****************************

; main pass 1 library processing
; destroys ax,bx,cx,dx,di,si,es

pass1_lib_proc  PROC
    mov ax,buffer_base      ; get i/o buffer segment
    mov es,ax               ; es -> i/o buffer
    xor si,si               ; si offsets into i/o buffer

    cmp new_lib,0           ; see if new library being processed
    jne p1_lib_bytes        ; yes
    mov ax,dir_buffer_size  ; get the buffer allocated to directory pages
    cmp ax,directory_size   ; compare to actual directory size
    jb  p1_init_pos         ; reread in first part of directory

; no need to reread directory
p1_toscan:
    jmp NEAR PTR p1_scan_pubs

p1_lib_bytes:
    call    get_lib_sys_info    ; get library system information

    cmp is_no_ems,0         ; see if EMS used
    jne p1_noems            ; no

; EMS used, fit directory to 16K page
    cmp ax,0c00h            ; see if more than 3 EMS pages
    jbe p1_emsfulldir       ; no
    mov ax,0400h            ; make directory buffer 1 16K page
    jmp SHORT p1_dir2

; round directory up to next 16K page, keep it there
p1_emsfulldir:
    add ax,3ffh
    and ax,0fc00h           ; round to 16K page boundary
    jmp SHORT p1_dir2

p1_noems:
    add ax,100h             ; adjust for 4K minimum library file buffer size
    jc  p1_dir_nofit        ; all of directory won't fit in file buffer
    cmp ax,buffer_size      ; see if directory and 4K minimum lib buffer exceeds buffer size
    jbe p1_dir_position     ; no, directory will fit

; all of directory including 4K minimum lib buffer won't fit in file buffer
p1_dir_nofit:
    mov ax,buffer_size      ; get current buffer size
    sub ax,100h             ; back off library file buffer
    and ax,0ffe0h           ; round to lowest 512-byte boundary
    jmp SHORT p1_dir2       ; bypass re-adjustment

; position to directory entries in lib file
p1_dir_position:
    sub ax,100h             ; subtract off 4K adjustment

p1_dir2:
    mov dir_buffer_size,ax  ; save directory buffer size in paragraphs
    shl ax,1                ; convert paragraphs of directory buffer size to bytes
    shl ax,1
    shl ax,1
    shl ax,1
    mov dir_buff_bysize,ax

p1_init_pos:
    mov dir_paras_read,0    ; init paragraphs of directory read
    mov dx,WORD PTR dir_offset  ; dx has LSW of file offset
    mov cx,WORD PTR dir_offset+2    ; cx has MSW of file offset

p1_dir_loop:
    mov bx,lib_handle       ; get handle of library file in bx
    mov ax,4200h            ; move file pointer, from beginning of file
    int 21h
    call    restore_ems_map

    mov cx,dir_buff_bysize  ; get number of directory bytes to read

    mov ax,directory_size
    sub ax,dir_paras_read   ; get total number of bytes left to read
    cmp ax,dir_buffer_size  ; compare to directory buffer read size
    jae p1_read             ; greater than or equal, use directory buffer read size
    shl ax,1                ; convert paras to bytes, x2
    shl ax,1
    shl ax,1
    shl ax,1                ; x16
    mov cx,ax               ; use number of bytes left to read

p1_read:
    mov dir_buff_end,cx     ; save number of bytes until read end
    xor dx,dx
    push    ds              ; save ds -> warplink data
    mov ds,buffer_base      ; DS:DX -> segment:offset of buffer area

    call    read_to_ems

    pop ds                  ; restore ds -> warplink data

p1_checkdec:
    call    decrypt         ; decrypt if SmartMem library

p1_scan_pubs:
    call    scan_pub_entries    ; scan public entries for matches to unresolved externals
    cmp all_resolved,0      ; see if all symbols resolved by library
    je  p1_dir_chk          ; no
    jmp SHORT p1_ret        ; yes

; see if all directory pages processed in library
p1_dir_chk:
    mov ax,dir_buffer_size  ; get number of paragraphs already processed
    add ax,dir_paras_read   ; add in previous number processed
    jc  p1_ret              ; overflow, assume complete
    mov dir_paras_read,ax   ; update number processed
    cmp ax,directory_size   ; compare to total number of paragraphs to process
    jae p1_ret              ; all done

; more directory pages to process
    xor bx,bx
    shl ax,1                ; convert paragraphs to bytes in bx,ax
    rcl bx,1
    shl ax,1
    rcl bx,1
    shl ax,1
    rcl bx,1
    shl ax,1
    rcl bx,1
    mov dx,WORD PTR dir_offset  ; dx has LSW of file offset
    mov cx,WORD PTR dir_offset+2    ; cx has MSW of file offset
    add dx,ax               ; update low word of offset
    adc cx,bx               ; update high word including any carry
    jmp SHORT p1_dir_loop   ; loop until complete

p1_ret:
    ret
pass1_lib_proc  ENDP

;*****************************
;* GET_LIB_SYS_INFO          *
;*****************************

; get library system information, save to variables
; upon entry es:si -> start of i/o buffer
; destroys ax,bx,cx,dx,si

get_lib_sys_info    PROC

; load first nine bytes of lib file into i/o buffer
    mov bx,lib_handle       ; get handle of library file
    mov cx,9                ; read first nine bytes of lib file
    push    ds              ; save ds -> warplink data
    mov ds,buffer_base      ; DS:DX -> segment:offset of buffer area
    xor dx,dx

    call    read_to_ems

    pop ds                  ; restore ds -> warplink data

    call    decrypt         ; decrypt if SmartMem library
	lods	BYTE PTR es:[0] ; get first record type of lib file
    cmp al,MSLIBR           ; see if appropriate type
    je  p1_3                ; yes

; bad library format error
    mov dx,OFFSET DGROUP:filename
    mov ax,LIB_FORMAT_ERR   ; library format error
    jmp NEAR PTR link_error ; error occurred

p1_3:
	lods	WORD PTR es:[0] ; get record length
    add ax,3                ; library page size is record length plus 3
    mov lib_page_size,ax    ; save library page size to memory variable

	lods	WORD PTR es:[0] ; get directory offset low word
    mov WORD PTR dir_offset,ax  ; save to memory variable
	lods	WORD PTR es:[0] ; get directory offset high word
    mov WORD PTR dir_offset+2,ax    ; save to memory variable

    mov ax,es:[si]          ; get directory pages
    mov dir_pages,ax        ; save to memory variable

; convert 512-byte pages to paragraphs
    shl ax,1                ; x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    shl ax,1                ; x16
    shl ax,1                ; x32
    mov directory_size,ax   ; save to memory variable
    ret
get_lib_sys_info    ENDP

;*****************************
;* SCAN_PUB_ENTRIES          *
;*****************************

; scan public entries in library for matches to unresolved externals
; duplicates some separate procedures, but more tightly optimized
; if found, seek to proper page and pass 1 process object records
; destroys ax,bx,cx,dx,di,si

scan_pub_entries    PROC
    mov lib_pages_count,0   ; init count of stored library page pointers
    mov ax,es
    mov dx,ds
    mov ds,ax
    mov es,dx               ; swap es and ds for duration
    xor bx,bx               ; bx offsets into public pointer array

spe_loop:
    mov al,[bx]             ; get public pointer array element value
    or  al,al               ; check if nonzero entry
    jne spe_nonzero         ; nonzero

spe_next_entry:
    inc bx                  ; point to next element of array
    cmp bl,37               ; check entry count of page (lower byte of bx never exceeds 37)
    jb  spe_loop            ; not done yet, keep looking at public entries

spe_2:
    and bx,0fe00h           ; bx -> first entry of next page (zero lower 511 bytes)
    add bx,512              ; move to next directory page
    jc  to_spe_ret          ; overflowed, no more directory pages in buffer
    cmp bx,es:dir_buff_end  ; see if past end of directory

    jb  spe_loop            ; no

to_spe_ret:
    jmp NEAR PTR spe_ret    ; yes, done with bytes in buffer

spe_nonzero:
    xor ah,ah               ; zap high byte
    shl ax,1                ; convert to offset into directory page
    mov dx,bx
    and dx,0fe00h           ; get page value of bx
    or  ah,dh               ; merge into bx offset value for true offset
    mov si,ax               ; ds:si -> public name

    mov cl,[si]             ; get 1-byte length of name
    xor ch,ch               ; zap high byte
    jcxz    spe_next_entry  ; zero length name, ignore it
    push    cx              ; save count of chars
    inc si                  ; point to first char of name

spe_3:
    mov di,OFFSET DGROUP:name_field
    xor dx,dx               ; dx==hashcode, init to zero
    xor ah,ah               ; zero high byte of char value
    cmp es:is_casesense,0   ; check to see if string should go to all caps
    je spe_4                ; not case sensitive, convert string chars to caps

IFNDEF DEMO
; names are case sensitive, no alteration (to all caps) of chars occurs
spe_3a:
    lodsb                   ; get byte to check
    stosb                   ; store in name_field
    add dx,ax               ; add in character value byte, ignore overflow
    loop    spe_3a
    jmp SHORT spe_6         ; bypass case insensitive code
ENDIF

; names are NOT case sensitive, convert string chars to caps
spe_4:
    lodsb                   ; get byte to check
    cmp al,'a'              ; check lowercase lower boundary
    jb  spe_5               ; not a lowercase letter
    cmp al,'z'              ; check lowercase upper boundary
    ja  spe_5               ; not a lowercase letter
    sub al,20h              ; convert to uppercase

spe_5:
    stosb                   ; store in name_field
    add dx,ax               ; add in character value byte, ignore overflow
    loop    spe_4

; dx holds 16-bit hashcode value
spe_6:
    lodsw                   ; get library page number
    stosw                   ; store in name field
    and dx,03ffh            ; make hash code a 10-bit value
    pop cx                  ; get length of string

; see if hash code for name is used
    mov si,OFFSET DGROUP:pubdecl_hash   ; si -> base of hash pointers to public declaration entries
    shl dx,1                ; convert dx to word offset
    add si,dx               ; si points to proper name hash code entry
    cmp WORD PTR es:[si],0  ; is hash code used (nonzero value)
    je  spe_next_entry      ; no, name not found

; hash code used, check for hash collision
    push    ds              ; save ds -> directory buffer
    mov ax,es
    mov ds,ax               ; ds -> warplink data
    mov ax,[si]             ; get segment pointer to pubdef declaration entry
    mov di,OFFSET DGROUP:name_field
    push    di              ; save pointer to first char of converted string

spe_testloop:
    pop si                  ; si -> name
    push    si              ; put it back on stack
    mov es,ax               ; es -> segment of pubdef declaration entry testing name against
    mov dx,ax               ; save entry segment in dx
    mov ax,es:[6]           ; get segment of pubdef name in pubdef names block
    mov di,es:[4]           ; get offset of pubdef name in pubdef names block
    mov es,ax               ; extra segment holds pubdef name segment
    push    cx              ; save count of chars in string

; ds:si -> name, es:di -> name to test against
spe_byteloop:
    cmpsb                   ; compare a nonzero byte in the two names
    jne spe_nomatch         ; not equal, test for greater or less than
    loop    spe_byteloop    ; test thru all chars
    cmp BYTE PTR es:[di],0  ; name testing against should be zero terminated for match
    je spe_match            ; successful match

spe_nomatch:
    pop cx                  ; restore count of chars in string
    mov es,dx               ; get pubdef declaration entry segment value
    jc  spe_7               ; old name greater than new name

; new name greater than old name
    mov ax,es:[12]          ; get entry having a greater name
    jmp SHORT spe_null_chk  ; check for null pointer

; old name greater than new name
spe_7:
    mov ax,es:[10]          ; get entry having a lesser name

spe_null_chk:
    or  ax,ax               ; check if a null pointer (ax=0)
    jne spe_testloop        ; no, loop back for next name check
    pop si                  ; restore stack
    pop ds
    mov ax,DGROUP
    mov es,ax               ; restore es -> warplink data
    jmp NEAR PTR spe_next_entry ; try next public array entry

; names match
spe_match:
    pop cx                  ; pull count of string chars off of stack
    mov es,dx               ; get pubdef declaration entry segment value
    mov al,es:[14]          ; get definitions flag
    and al,3                ; only keep pubdef/extdef/comdef/absolute field

; only match if unresolved external, otherwise ignore
    cmp al,2                ; check if previous entry was a pubdef/absolute
    jb  spe_found           ; no

spe_ignore:
    pop si                  ; pull pointer to name off of stack
    pop ds
    mov BYTE PTR [bx],0     ; zero out pointer in the directory buffer for quick rescan
    mov ax,DGROUP
    mov es,ax               ; restore es -> warplink data
    jmp NEAR PTR spe_next_entry ; try next public array entry

; a matching entry in the library was found, use it if not a comdef and not weak extdef
spe_found:

    test    BYTE PTR es:[15],40h    ; see if comdef
    jne spe_ignore          ; yes, don't bring in pubdef from library then

    or  al,al               ; see if weak extdef
    je  spe_ignore          ; yes, don't bring in pubdef from library

    mov ax,[si]             ; get page number value of public entry
    mov lib_page_num,ax     ; save to memory variable
    pop si                  ; get public name pointer off of stack

; at this point the directory buffer pointer is still on the stack

; check if store another library page or process stored pages
    cmp lib_pages_count,1024    ; see if storage is full
    jae spe_destore         ; yes, process the stored pages

; store the library page
spe_store_page:
    mov ax,lib_pages_count
    shl ax,1                ; convert to word offset
    mov si,OFFSET DGROUP:lib_page_storage   ; si -> base of stored pages
    add si,ax               ; si -> proper array to store library page number
    mov ax,lib_page_num
    mov [si],ax             ; store page
    inc lib_pages_count     ; bump count of library pages stored
    mov ax,ds
    mov es,ax               ; es -> warplink's data
    pop ds                  ; ds -> directory buffer
    mov BYTE PTR [bx],0     ; zero out pointer in the directory buffer for quick rescan
    jmp NEAR PTR spe_next_entry ; loop until complete

spe_destore:
    call    remove_dup_pages    ; remove duplicate pages in storage

    cmp lib_pages_count,768 ; see if storage is at 75% levels
    jb  spe_store_page      ; no, continue storing pages

    mov is_msextlib,0       ; fail further use of extended library dictionary
    mov BYTE PTR [bx],0     ; zero out pointer in the directory buffer for quick rescan
    pop es                  ; pull directory pointer off of stack into es

    call    proc_stored_pages   ; process the stored library pages

; see if total symbols matches public symbols as result of library processing
    mov ax,WORD PTR es:pub_sym_count    ; get low word of public symbols count
    cmp ax,WORD PTR es:tot_sym_count    ; compare to low word of total symbols count
    je  spe_8               ; okay so far
    jmp NEAR PTR spe_next_entry ; not the same, library didn't resolve all externals

spe_8:
    mov ax,WORD PTR es:pub_sym_count+2  ; get high word of public symbols count
    cmp ax,WORD PTR es:tot_sym_count+2  ; compare to high word of total symbols count
    je  spe_all_res         ; all symbols resolved
    jmp NEAR PTR spe_next_entry ; not the same, library didn't resolve all externals

spe_all_res:
    mov es:all_resolved,1   ; flag that total symbols == public symbols

spe_ret:
    cmp es:lib_pages_count,0    ; see if any library pages left to process
    je  spe_ret2            ; no
    call    proc_stored_pages

spe_ret2:
    mov ax,es
    mov cx,ds
    mov ds,ax               ; restore ds -> warplink data
    mov es,cx               ; restore es

; see if total symbols matches public symbols as result of library processing
    mov ax,WORD PTR pub_sym_count   ; get low word of public symbols count
    cmp ax,WORD PTR tot_sym_count   ; compare to low word of total symbols count
    jne spe_exit            ; not the same, library didn't resolve all externals

    mov ax,WORD PTR pub_sym_count+2 ; get high word of public symbols count
    cmp ax,WORD PTR tot_sym_count+2 ; compare to high word of total symbols count
    jne spe_exit            ; not the same, library didn't resolve all externals

    mov all_resolved,1      ; flag that total symbols == public symbols

spe_exit:
    ret
scan_pub_entries    ENDP

;*****************************
;* REMOVE_DUP_PAGES          *
;*****************************

; remove duplicate pages in library page storage
; keeping nonduplicate entries in contiguous, sorted order
; destroys ax,cx,dx,si

remove_dup_pages    PROC
    push    di              ; save critical register
    cmp lib_pages_count,1   ; see if only one page to process
    jbe rdp_ret             ; yes, no duplicate pages to remove
    call    sort_lib_pages  ; sort library pages
    mov si,OFFSET DGROUP:lib_page_storage   ; si -> base of sorted stored pages
    mov di,si
    add di,2                ; di -> second entry
    mov cx,lib_pages_count  ; get number of page entries to compare
    dec cx                  ; start comparing with second entry

rdp_loop:
    mov ax,[di]             ; get entry
    cmp ax,[si]             ; see if matches previous entry
    je  rdp_match           ; yes
    add si,2                ; move to next entry slot
    mov [si],ax             ; save nonmatching value
    jmp SHORT rdp_next      ; try next entry

rdp_match:
    dec lib_pages_count     ; drop count of library pages stored (ignore duplicate)

rdp_next:
    add di,2                ; move to next slot to match
    loop    rdp_loop        ; loop until complete

rdp_ret:
    pop di                  ; restore critical register
    ret
remove_dup_pages    ENDP

;*****************************
;* PROC_STORED_PAGES         *
;*****************************

; process library pages stored in lib_page_storage array
; destroys ax,bx,cx,dx,di,si
; modifies es and ds to point to warplink data and directory buffer, respectively

proc_stored_pages   PROC
    mov ax,DGROUP
    mov ds,ax               ; temporarily make ds -> warplink data
    mov es,buffer_base      ; temporarily make es -> directory buffer

    mov ax,dir_buffer_size  ; get size of directory buffer in paragraphs
    add buffer_base,ax      ; temporarily adjust file i/o buffer base
    mov ax,dir_buff_bysize  ; get size of directory buffer in bytes
    sub buffer_end,ax       ; temporarily adjust file i/o buffer end

IFNDEF DEMO
    call    check_ext_dict  ; check use of and process extended library dictionary
ENDIF

; sort the pages in ascending order (array small enough for sleazy bubble sort)
    call    sort_lib_pages

    mov si,OFFSET DGROUP:lib_page_storage   ; init si -> base of sorted stored pages

psp_procloop:
    mov lib_read_amt,0      ; init the library read amount
    mov ax,[si]             ; get library page number
    cmp si,OFFSET DGROUP:lib_page_storage   ; check if first time thru loop
    je  psp_proc2           ; yes
    cmp ax,[si-2]           ; see if this entry matched previous entry
    je  psp_next_page       ; yes, don't process again

    sub ax,[si-2]           ; ax holds difference in page numbers
    mul lib_page_size       ; dx:ax has byte count of difference
    or  dx,dx               ; see if byte count >64K
    jne psp_defread         ; yes, use default library read amount
    cmp ax,LIB_MAX_READ     ; see if byte count difference >= default read amount
    jae psp_defread         ; yes, used default
    mov lib_read_amt,ax     ; save updated library read amount

psp_defread:
    mov ax,[si]             ; get library page number back in ax

psp_proc2:
    push    si              ; save si -> stored page offset
    mov lib_page_num,ax     ; store in memory variable
    call    make_libmod_entry   ; make library module entry

    call    lib_load_and_go
    pop si                  ; restore si -> stored page offset

psp_next_page:
    add si,2                ; point to next entry in array
    mov ax,si
    sub ax,OFFSET DGROUP:lib_page_storage   ; subtract off offset in memory
    shr ax,1                ; convert word offset back to byte count
    cmp ax,lib_pages_count  ; see if all stored pages used
    jb  psp_procloop        ; no, loop back and process next entry

psp_done_proc:
    mov ax,dir_buffer_size  ; get size of directory buffer in paragraphs
    sub buffer_base,ax      ; restore file i/o buffer base to original state
    mov ax,dir_buff_bysize  ; get size of directory buffer in bytes
    add buffer_end,ax       ; restore file i/o buffer end to original state

; see if total symbols matches public symbols as result of library processing
    mov ax,WORD PTR pub_sym_count   ; get low word of public symbols count
    cmp ax,WORD PTR tot_sym_count   ; compare to low word of total symbols count
    jne psp_ret             ; not the same, library didn't resolve all externals

    mov ax,WORD PTR pub_sym_count+2 ; get high word of public symbols count
    cmp ax,WORD PTR tot_sym_count+2 ; compare to high word of total symbols count
    jne psp_ret             ; not the same, library didn't resolve all externals

; all symbols resolved
    mov all_resolved,1      ; flag that total symbols == public symbols

psp_ret:
    mov lib_pages_count,0   ; re-init number of stored pages
    mov ax,DGROUP
    mov es,ax               ; make es -> warplink data
    mov ds,buffer_base      ; make ds -> directory buffer
    ret
proc_stored_pages   ENDP

;*****************************
;* LIB_LOAD_AND_GO           *
;*****************************

; load library module and process it
; destroys all registers except ds

lib_load_and_go PROC
    call    load_libmod     ; load library module
    mov es,buffer_base      ; es -> library file i/o buffer
    mov ax,current_lib      ; get current library value
    or  ax,8000h            ; set high bit
    mov WORD PTR lib_id+2,ax    ; save as high word of library id
    mov ax,lib_page_num     ; get page number (location in lib file)
    mov WORD PTR lib_id,ax  ; save as low word of library id

	cmp	writing_qlk_flag,0	; see if writing to qlk file
	je	lla_3				; no
	push	bx				; save bx==file handle
	cmp	new_lib,0			; see if new library
	je	lla_2				; no
	call	write_qlk_lib_info	; save library system info

lla_2:
	call	write_qlk_mod_info	; save library module info
	pop	bx					; restore bx==file handle

lla_3:
	xor	ax,ax
    mov new_lib,al			; reset new library flag
    cmp is_clpinc,al		; see if clipper incremental link specified
    je  psp_notinc          ; no
    call    setup_incinfo   ; setup incremental info for file

psp_notinc:
    call    pass1_obj_proc  ; pass 1 process loaded library module

    inc si                  ; bump past checksum so second pass won't load buffer
    mov ax,WORD PTR file_pos_adj
    mov bx,WORD PTR file_pos_adj+2
    sub ax,WORD PTR lib_pos_adj
    mov cx,ax
    sbb bx,WORD PTR lib_pos_adj+2   ; bx:ax holds read beyond buffer
    or  ax,bx               ; see if read beside buffer amount
    je  psp_chkems          ; no, use offset in si for module length
    add si,cx               ; add in previous amount low word
    jc  psp_use60k          ; overflow, use 60k for read amount
    or  bx,bx               ; see if file position besides buffer exceeds 64K
    jne psp_use60k          ; yes, use 60k for read amount
    cmp si,0f000h           ; see if more than 60K
    jbe psp_savelen         ; no, use it

psp_use60k:
    mov si,0f000h           ; make library read 60K
    jmp SHORT psp_savelen   ; bypass EMS stuff

; check if not library module and EMS available,
; if so and enough pages available, stash library pages in EMS
psp_chkems:
;***    cmp module_flag,0       ; see if library module
;***    jne psp_savelen         ; yes, don't stash in EMS
;***    call    lib_ems_mapout  ; map library module pages out to EMS, if possible

psp_savelen:
    mov es,lib_modlen_ptr   ; es -> entry for library module
    mov es:[6],si           ; save amount to read

    ret
lib_load_and_go ENDP

COMMENT #

;*****************************
;* LIB_EMS_MAPOUT            *
;*****************************

; map pages out to EMS if possible
; upon entry si holds byte count of buffer, es -> buffer base
; destroys ax,bx,cx,dx,di,es

lib_ems_mapout  PROC
    push    si              ; save critical register
    cmp is_no_ems,0         ; see if EMS used
    jne lem_ret             ; no

    cmp ems_page_avail,4    ; must have over four free pages
    jbe lem_ret             ; not enough

    mov dx,es               ; get buffer base
    sub dx,ems_base         ; get paragraphs beyond base
    mov ax,dx

    and dx,0c00h            ; dx holds extra page count in bits 10,11
    xchg dl,dh
    shr dl,1                ; dl holds extra page count x 2, word offset

    and ax,3ffh             ; get page leftover

    shl ax,1                ; x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    shl ax,1                ; x16
    mov cx,si               ; get byte count of library module
    add cx,ax               ; add in overflow from page
    mov cl,ch
    xor ch,ch               ; /256
    and cl,0c0h
    rol cl,1
    rol cl,1                ; /16384, cl==0 -> 3
    inc cx                  ; make cl page count relative 1

    mov ax,ems_page_avail   ; get count of free pages
    sub ax,4                ; back off four free pages
    sub ax,cx               ; see if enough free pages to save this obj in ems
    jc  lem_ret             ; no

; map obj to ems pages, bring in free pages
    add ax,4                ; adjust back for four free pages
    mov ems_page_avail,ax   ; save new count of free pages

    mov si,OFFSET DGROUP:ems_currmap    ; ds:si -> physical page entries
    add si,dx               ; adjust past only dictionary pages

    mov ax,es               ; get buffer base
    mov es,lib_modlen_ptr
    mov di,10               ; es:di -> library module first ems page entry
    mov es:[18],ax          ; save buffer base
    mov ax,cx               ; save count of pages mapped out
    rep movsw               ; save all used physical pages

    mov cx,ax               ; restore count of pages mapped out
    mov al,dl               ; get dictionary pages to scan past
    shr al,1                ; convert to byte value from word offset

lem_loop:
    push    ax              ; save physical page
    mov bx,ems_first_avail  ; map in first available free page
    inc ems_first_avail     ; bump first available page
    call    map_ems_page
    pop ax                  ; restore physical page
    inc al                  ; bump physical page
    loop    lem_loop        ; loop until all free pages are mapped in

lem_ret:
    pop si                  ; restore  critical register
    ret
lib_ems_mapout  ENDP

END COMMENT #

IFNDEF DEMO

;*****************************
;* CHECK_EXT_DICT            *
;*****************************

; check if should use extended dictionary, process if so
; destroys ax,cx,dx,di,si

; Microsoft-compatible library comes on the page immediately following the
; directory page and has the following format:
;
; BYTE, value 0F2h -- extended dictionary identifier
; WORD, number of bytes in dictionary following this word
; WORD, number of entries in dictionary
; start of 2-word dictionary entries, each entry has the following format:
;   WORD, library page that entry is on
;   WORD, offset to list of needed entries from start of entries
;   <repeated for as many entries as is in dictionary
; needed entry lists, with the following format:
;   WORD, number of needed entries, each entry having the following format:
;       WORD, dictionary entry needed, count relative 0
;       < repeated for number of needed entries>
;   <repeated for number of entry lists>

check_ext_dict  PROC
    push    es              ; save critical register
    push    bx
    cmp is_msextlib,0       ; see if extended dictionary option is tripped
    je  to_ced_ret          ; no, return
    mov ax,directory_size   ; get total size of directory
    cmp ax,dir_buffer_size  ; compare to directory size in buffer
    je  ced_read_five       ; they match, entire directory processed in one pass

to_ced_ret:
    jmp NEAR PTR ced_ret    ; the entire directory was not processed in one pass, don't use dictionary

; read five bytes after directory, file pointer should be properly positioned from directory read
ced_read_five:
    mov cx,5
    xor dx,dx
    mov bx,lib_handle       ; get handle of library file in bx
    push    ds              ; save ds -> warplink data
    mov ds,buffer_base      ; DS:DX -> segment:offset of buffer area

    call    read_to_ems

    pop ds                  ; restore ds -> warplink data

ced_2:
    call    decrypt         ; decrypt if SmartMem library
    mov es,buffer_base      ; es -> read in bytes
    cmp BYTE PTR es:[0],MSEXTDICT   ; see if extended dictionary identifier
    jne to_ced_ret          ; no, return
    mov cx,es:[1]           ; get number of bytes in dictionary
    sub cx,2                ; adjust for number of entries word
    mov di,es:[3]           ; get number of entries in dictionary
    cmp cx,buffer_end       ; see if enough room to read dictionary into file buffer
    jbe ced_read_dict       ; yes

; not enough room to read dictionary into file buffer, see if free memory is big enough
    mov ax,allocation_top
    sub ax,allocation_base  ; ax holds number of free paragraphs
    shl ax,1                ; convert to bytes, x2
    jc  ced_3               ; overflow, enough free memory for dictionary
    shl ax,1                ; x4
    jc  ced_3
    shl ax,1                ; x8
    jc  ced_3
    shl ax,1                ; x16
    jc  ced_3
    cmp ax,cx               ; compare free bytes to bytes needed
    jae ced_3               ; enough free bytes to load dictionary
    jmp NEAR PTR ced_ret    ; not enough room in free memory for dictionary either

; enough room in free memory to read in dictionary
ced_3:
    mov es,allocation_base  ; es -> buffer to read in dictionary

ced_read_dict:
    push    ds              ; save ds -> WarpLink data
    mov ax,es
    mov ds,ax               ; ds:dx -> buffer area

    call    read_to_ems

    pop ds                  ; restore ds -> WarpLink data

ced_4:
    call    remove_dup_pages    ; remove duplicate pages in storage

    mov cx,di               ; cx holds count of entries in dictionary
    shl cx,1
    shl cx,1                ; convert to byte count of entries (each entry 2 words)
    mov dx,lib_pages_count  ; dx holds count of stored pages
    shl dx,1                ; convert to byte count of pages (each page entry is 1 word)
    mov bx,OFFSET DGROUP:lib_page_storage   ; bx -> base of sorted stored pages
    add dx,bx               ; adjust stored pages byte count for start of base

ced_storeloop:
    xor di,di               ; di will offset into dictionary entries
    cmp bx,dx               ; see if checked to end of stored pages
    jae ced_ret             ; yes, done
    mov ax,[bx]             ; get stored page

ced_dictloop:
    cmp ax,es:[di]          ; see if matches dictionary entry
    je  ced_match           ; yes, check needed entry list
    jb  ced_next_page       ; not found, try next stored page
    add di,4                ; move to next dictionary entry
    cmp di,cx               ; see if any more dictionary entries
    jb  ced_dictloop        ; yes, try next entry

ced_next_page:
    add bx,2                ; bump to next stored page entry
    jmp SHORT ced_storeloop ; loop through all stored pages

; stored page entry matched dictionary entry
ced_match:
    cmp lib_pages_count,1024    ; see if any room to add new entry
    jae ced_ret             ; no
    push    cx              ; save critical register
    push    di
    mov WORD PTR es:[di],0  ; zap entry's page so won't be reused
    mov di,es:[di+2]        ; get offset to needed entry list
    mov cx,es:[di]          ; get number of needed entries
    jcxz    ced_5           ; no needed entries

ced_needloop:
    add di,2                ; position to needed entry in list
    mov si,es:[di]          ; get needed entry number
    shl si,1
    shl si,1                ; convert to byte offset
    mov ax,es:[si]          ; get library page
    or  ax,ax               ; check if already used
    je  ced_next_needed     ; yes, try next needed entry

; see if library page is already in storage
; since pages at end can be unsorted don't use binary search
; brute force compare from start of array will be fast enough
    mov si,OFFSET DGROUP:lib_page_storage   ; si -> base of sorted stored pages

ced_checkloop:
    cmp ax,[si]             ; see if this page entry matches one to add
    je  ced_next_needed     ; page already stored, try next
    add si,2                ; move to next entry
    cmp si,dx               ; see if any more entries
    jb  ced_checkloop       ; yes, check them  

; new entry, update lib_page_storage and continue
    mov si,dx               ; si -> entry following last entry
    mov [si],ax             ; save new page
    add dx,2                ; bump byte count in storage
    inc lib_pages_count     ; bump count of library pages in storage

ced_next_needed:
    cmp lib_pages_count,1024    ; see if any room to add new entries
    jae ced_ret             ; no, stop trying
    loop    ced_needloop    ; loop through all needed entries in list

ced_5:
    pop di                  ; restore critical register
    pop cx
    jmp SHORT ced_next_page ; try next page entry, if any

ced_ret:
    pop bx                  ; restore critical register
    pop es
    ret
check_ext_dict  ENDP

ENDIF

;*****************************
;* SORT_LIB_PAGES            *
;*****************************

; sort library pages in ascending order
; destroys ax,cx,dx,si

sort_lib_pages  PROC
    push    bx              ; save critical register
    cmp lib_pages_count,1   ; see if only one page to process
    je  slb_ret             ; yes, don't sort

psp_sort1:
    mov si,OFFSET DGROUP:lib_page_storage   ; init si -> base of stored pages
    mov cx,lib_pages_count
    dec cx                  ; loop n-1 times where n == number of elements
    xor dl,dl               ; nonzero dl flags swap this pass

psp_sort2:
    mov ax,[si]             ; get array element
    mov bx,[si+2]           ; get next high array element
    cmp ax,bx               ; compare lower element to higher
    jbe psp_sort3           ; less than or equal to, don't swap elements

; swap the array elements
    mov [si+2],ax
    mov [si],bx
    mov dl,1                ; flag that swap occurred

psp_sort3:
    add si,2                ; move to next array element
    loop    psp_sort2
    or  dl,dl               ; check if swap occurred this pass
    jne psp_sort1           ; yes

slb_ret:
    pop bx                  ; restore critical register
    ret
sort_lib_pages  ENDP

;*****************************
;* MAKE_LIBMOD_ENTRY         *
;*****************************

; make a library module entry
; returns es -> entry
; destroys ax,bx,dx,es

make_libmod_entry   PROC
    mov dx,first_libent_ptr ; save pointer to first library module entry
    mov bx,LIB_MODENT_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation

    or  dx,dx               ; see if previously allocated entry
    jne mle_2               ; yes
    mov first_libent_ptr,ax ; no, init first allocated entry pointer

mle_2:
    mov dx,alloc_libent_ptr ; save pointer to previous last allocated entry, if any
    mov alloc_libent_ptr,ax ; update last allocated entry pointer
    mov es,ax               ; es -> current entry
    mov WORD PTR es:[30],0  ; zero next entry pointer
    mov ax,current_lib      ; get current library
    mov es:[0],ax           ; update entry
    mov ax,lib_page_size    ; get library page size
    mov es:[4],ax           ; update entry
    mov ax,lib_page_num     ; get library page number
    mov es:[2],ax           ; update entry

    mov ax,es
    mov lib_modlen_ptr,ax

    or  dx,dx               ; see if previous entry to update
    je  mle_3               ; no

; update previous entry to point to this entry
    mov ax,es               ; ax holds segment of current entry
    mov es,dx               ; es -> previous entry
    mov es:[30],ax          ; previous entry -> current entry
    mov es,ax               ; restore es -> current entry

mle_3:
;***    mov ax,-1               ; init ems pages to -1 to show unused
;***    mov es:[10],ax
;***    mov es:[12],ax
;***    mov es:[14],ax
;***    mov es:[16],ax

mle_ret:
    ret
make_libmod_entry   ENDP

END
