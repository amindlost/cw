;*********************************************************************
;*   MLLIB2.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/31/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   library processing part 2                                       *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mllib2
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
PUBLIC  proc2_libs

; procedures declared public for profiler
PUBLIC  pass2_lib_proc,get_curr_lib,check_libobj_path,load_libmod

; variables
PUBLIC  lib_read_amt,libmod_obj_flag
PUBLIC	pos_in_list

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

EXTRN   filename:BYTE,lib_handle:WORD,use_libs:BYTE,lib_page_size:WORD
EXTRN   lib_page_num:WORD,lib_block_ptr:WORD,pass_number:BYTE
EXTRN   module_flag:BYTE,ems_pages_flushed:BYTE

; initialized local variables

.DATA?

; uninitialized local variables

; byte values
EVEN
libmod_obj_flag DB  ?       ; nonzero if library module listed in object module list

; word values
EVEN
lib_read_amt    DW  ?       ; amount of bytes to read in from a library (modified by file reread of same module)
pos_in_list DW  ?           ; position to read name in object name block namelist

; byte strings
pathed_file  DB  128 DUP (?)    ; filename with prepended path

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR
EXTRN   load_buffer:NEAR,alloc_memory:NEAR
EXTRN   pass2_obj_proc:NEAR,lib_router:NEAR
EXTRN   decrypt:NEAR,print_link_info:NEAR
EXTRN	file_not_found:NEAR

EXTRN   restore_ems_map:NEAR,map_ems_page:NEAR,read_to_ems:NEAR

;*****************************
;* PROC2_LIBS                *
;*****************************

; main driver for pass 2 processing of libraries
; all registers destroyed except for ds

proc2_libs  PROC
    cmp use_libs,0          ; see if libraries are used
    jne pl_2                ; yes
    ret                     ; no, return

pl_2:
    push    ds              ; save critical register
    mov cx,first_libent_ptr ; save cx -> first library module entry

; bubble sort the library entries by library number and page number in ascending order
pl_sort1:
    mov ax,cx               ; get pointer to first library module entry
    mov ds,ax               ; ds -> first library module entry
    mov bx,ds:[30]          ; bx == segment of second library module entry
    or  bx,bx               ; check if nonzero
    je  pl_sort_done        ; zero, no sort needed (only one entry)
    xor dl,dl               ; nonzero dl flags swap this pass

pl_sort2:
    mov ds,ax               ; ds -> lower position library module entry
    mov es,bx               ; es -> high position library module entry
    mov ax,ds:[0]           ; get library number of lower
    mov bx,es:[0]           ; get library number of higher
    cmp ax,bx               ; compare lower entry to higher
    jb  pl_sort3            ; less than, don't swap entries
    ja  pl_swap             ; greater than, swap entries

; library numbers are equal, check page numbers
    mov ax,ds:[2]           ; get page number of lower
    mov bx,es:[2]           ; get page number of higher
    cmp ax,bx               ; compare lower entry to higher
    jbe pl_sort3            ; less than or equal to, don't swap entries

; swap the library entries
pl_swap:
    xor si,si

; swap entire all bytes in entry EXCEPT last two bytes, segment pointer to next block
pl_swap2:
    mov ax,[si]
    mov bx,es:[si]
    mov es:[si],ax
    mov [si],bx
    add si,2
    cmp si,30               ; see if at pointer to next block yet
    jb  pl_swap2            ; no
    mov dl,1                ; flag that swap occurred

pl_sort3:
    mov ax,es
    mov bx,es:[30]          ; get segment of next block
    or  bx,bx               ; see if nonzero
    jne pl_sort2            ; yes
    or  dl,dl               ; check if swap occurred this pass
    jne pl_sort1            ; yes, try another pass

; sort complete, library module entries sorted by library number and page number
pl_sort_done:
    pop ds                  ; restore critical register

pl_proc:
    mov al,2                ; give lib_router procedure pass 2 flag
    call    lib_router      ; initialize data and route to proper library code
    ret
proc2_libs  ENDP

;*****************************
;* PASS2_LIB_PROC            *
;*****************************

; main pass 2 library processing
; destroys ax,bx,cx,dx,di,si,es

pass2_lib_proc  PROC
    mov current_lib,65535   ; force library to load on first compare
    mov ax,first_libent_ptr ; get pointer to first library module entry

p2_loop:
    or  ax,ax               ; check that segment is nonzero
    jne p2_1                ; yes
    jmp NEAR PTR p2_ret     ; segment is zero, all done processing library modules

p2_1:
    mov es,ax               ; es -> library module entry
    push    ax              ; save entry segment

    mov ax,es:[0]           ; get library
    cmp ax,current_lib      ; see if matches current library
    jne p2_newlib           ; no
    jmp NEAR PTR  p2_2      ; yes

p2_newlib:
    cmp current_lib,65535   ; see if first time through loop
    je  p2_1a               ; yes
    mov cx,ax               ; save library in cx
    mov bx,lib_handle       ; get file handle of previously opened library
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map
    mov ax,cx               ; get library back in ax

p2_1a:
    mov current_lib,ax      ; update current library to reflect library being processed

; get and open library indicated by library module entry
    push    es              ; save critical register
    xor cx,cx               ; cx == library entry counter
    mov ax,first_libblk_ptr ; get first object name block

p2_1b:
    mov si,4                ; si -> first name in block
    mov es,ax               ; point extra segment at object name block

p2_lib_loop:
    mov al,es:[si]          ; get flag byte
    and al,0c0h             ; only get overlay bits
    mov obj_ovl_flag,al     ; save overlay status for module
    and al,40h              ; get nonvector root call status
    mov nonovl_rvect,al     ; save it to global variable
    inc si                  ; adjust past prepended flag byte in entry
    mov ax,LIB_NAMBLK_BYSIZE    ; size of block in bytes
    sub ax,es:[0]           ; minus free space, ax == end of used namelist
    cmp ax,si               ; check that position in list is below end
    ja  p2_lib2             ; not at end yet, check namelist position

    mov ax,es:[2]           ; get pointer to next block
    or  ax,ax               ; check that is not null
    jne p2_1b               ; non-null, next block exists, loop and check it
    jmp NEAR PTR lib_internal   ; internal error

p2_lib2:
    cmp cx,current_lib      ; see if at proper position in library name block
    je  p2_lib4             ; yes

; not at proper position, gobble bytes until next library name
p2_lib_gobble:
    mov al,es:[si]          ; check if at end of library name in block
    or  al,al
    je  p2_lib3             ; yes
    inc si                  ; bump to next char
    jmp p2_lib_gobble

p2_lib3:
    inc si                  ; bump to first char of next name in block
    inc cx                  ; bump count of entry scanned
    jmp SHORT p2_lib_loop   ; and loop back to try next namelist position

p2_lib4:
    push    ds              ; save critical register
    mov ax,ds
    mov cx,es
    mov ds,cx               ; ds:si -> library name in block
    mov es,ax
    mov di,OFFSET DGROUP:filename   ; es:di -> destination of library name

p2_transfer:
    movsb                   ; transfer a char from the block to filename
    cmp BYTE PTR [si-1],0   ; zero byte signals end of transfer
    jne p2_transfer         ; nonzero, keep looping

    pop ds                  ; restore critical register
    pop es                  ; restore critical register
    call    print_link_info ; print library name, if applicable
    mov dx,OFFSET DGROUP:filename   ; DS:DX -> ASCIIZ file specification
    mov ax,3d00h            ; open file with read access
    int 21h
    call    restore_ems_map
    jc  p2_doserr           ; error occurred

    mov lib_handle,ax       ; save library file handle
    mov ax,current_lib      ; get current library value

p2_2:
    or  ah,80h              ; set high bit
    mov WORD PTR lib_id+2,ax    ; save as high word of library id
    mov ax,es:[4]           ; get library page size
    mov lib_page_size,ax    ; save to memory variable
    mov ax,es:[2]           ; get page number (location in lib file)
    mov lib_page_num,ax     ; save to memory variable
    mov WORD PTR lib_id,ax  ; save as low word of library id

    mov bx,es:[6]           ; get module length

    call    load_libmod     ; load the module
;***    mov es,buffer_base      ; es -> library file i/o buffer

    call    pass2_obj_proc  ; pass 2 process loaded library module

    pop es                  ; es -> library module entry
    mov ax,es:[30]          ; get pointer to next entry
    jmp NEAR PTR p2_loop    ; loop back to process it

p2_doserr:
    jmp NEAR PTR dos_error  ; error occurred

p2_ret:
    ret
pass2_lib_proc  ENDP

;*****************************
;* GET_CURR_LIB              *
;*****************************

; get current library in filename
; destroys ax,si,di,es

get_curr_lib    PROC
    cmp current_lib,0       ; see if first library, requires position initialization
    jne gcl_3               ; nope

gcl_2:
    mov pos_in_list,4       ; init position in name list to first position in block

gcl_3:
    mov si,pos_in_list      ; si will offset into block
    mov ax,lib_block_ptr    ; get current object name block
    mov es,ax               ; point extra segment at object name block
    mov ax,LIB_NAMBLK_BYSIZE    ; size of block in bytes
    sub ax,es:[0]           ; minus free space, ax == end of used namelist
    cmp ax,si               ; check that position in list is below end
    ja  gcl_4               ; not at end yet, pull name from this block's namelist

    mov ax,es:[2]           ; get pointer to next block
    mov lib_block_ptr,ax    ; save back to memory variable
    or  ax,ax               ; check that is not null
    jne gcl_2               ; non-null, next block exists, loop back and try with it

; A WarpLink internal error has occurred, no more libraries were
; available before the count of all libraries was complete
lib_internal:
    mov ax,INTERNAL_ERR     ; put warplink error code in ax
    mov cx,9                ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

gcl_4:
    mov al,es:[si]          ; get flag byte
    mov ah,al
    and ah,1                ; get library module flag
    mov module_flag,ah      ; save flag status
    mov ah,al
    and ah,2                ; get library module in object module list flag
    mov libmod_obj_flag,ah
    and al,0c0h             ; only get overlay bits
    mov obj_ovl_flag,al     ; save overlay status for module
    and al,40h              ; get nonvector root call status bit
    mov nonovl_rvect,al     ; save it to global variable
    inc si                  ; adjust past prepended flag byte in entry

	push	es
	push	ds
	pop	es					; es -> warplink data
	pop	ds					; ds -> name block
    mov di,OFFSET DGROUP:filename   ; es:di -> destination of library name

gcl_5:
    movsb                   ; transfer a char from the block to filename
    cmp BYTE PTR [si-1],0   ; zero byte signals end of transfer
    jne gcl_5               ; nonzero, keep looping

	push	es
	pop	ds					; ds -> warplink data

    mov pos_in_list,si      ; si points to next name or end, save back to memory
    ret
get_curr_lib    ENDP

;*****************************
;* CHECK_LIBOBJ_PATH         *
;*****************************

; search LIB or OBJ environment string path for library
; upon entry di -> filename, bx -> target string ('LIB=' or 'OBJ=')
; return di -> path+name or terminate with DOS error if not found
; destroys ax,bx,cx,di

check_libobj_path  PROC
    push    es              ; save critical register
    push    si
    push    dx
    push    di              ; save -> original filename
    cmp BYTE PTR [di],'.'   ; if file already has path, force error
    je  cp_not_found
    cmp BYTE PTR [di],'\'
    je  cp_not_found
    cmp BYTE PTR [di+1],':' ; check for drivespec
    je  cp_not_found

; check for a LIB= in the environment
    xor si,si               ; starting location for target string check
    mov es,psp              ; es -> WarpLink's PSP
    mov es,es:[2ch]         ; es -> environment segment from offset 2ch in PSP

cp_find_path:
    xor di,di               ; offset into target string

cp_loop2:
    mov al,es:[si]          ; get byte from environment string
    inc si                  ; point to next char in environment
    cmp al,[bx+di]          ; does environment char match LIB string char
    je  cp_byte_match       ; yes, try next location
    or  al,es:[si]          ; two zero values in a row mean the end of the environment
    jne cp_find_path        ; not the end of the environment

cp_not_found:
    mov ax,2                ; force file not found error
    pop dx                  ; ds:dx -> name of library not found
    jmp NEAR PTR dos_error  ; go to DOS error handling routine

; check that LIB is not part of another environment string
cp_byte_match:
    or  di,di               ; di is zero if first char is matched
    jne cp_2                ; not first char, test already done
    cmp si,1                ; si equals one if LIB is first string in environment block
    je  cp_2                ; no previous environment string
    cmp BYTE PTR es:[si-2],0    ; check if char before LIB was nonzero
    jne cp_find_path        ; yes, LIB is a subset of another string, keep looking

cp_2:
    inc di                  ; a match, move to next byte of target string
    cmp di,4                ; check if all bytes matched
    jb  cp_loop2            ; not yet, keep comparing

cp_path_found:
    mov bx,OFFSET DGROUP:pathed_file    ; bx+di offset into filename with path from LIB e-var
    mov dx,bx               ; make dx-> name for get file attribute DOS function

cp_3:
    xor di,di               ; offset into path prefix

cp_4:
    mov al,es:[si]          ; get path character
    cmp al,';'              ; check if path terminator character
    je  cp_prefix_complete  ; yes, check file's existence with the current path prefix
    cmp al,' '              ; anything less than a space is also a terminator character
    jb  cp_prefix_complete
    mov [bx+di],al          ; save path character
    inc di                  ; move to next name slot
    inc si                  ; move to next byte location
    jmp SHORT cp_4          ; loop for next character

cp_prefix_complete:
    pop ax                  ; ax -> original program name
    push    ax              ; restore to stack
    push    si              ; save si -> current environment position
    mov si,ax               ; append program name to path prefix
    cmp BYTE PTR [bx+di-1],'\'  ; check for backslash already in place
    je  cp_5
    mov BYTE PTR [bx+di],'\'    ;put a backslash between the path and program name
    inc di                  ; point di past backslash

cp_5:
    mov al,[si]             ; get program name character
    mov [bx+di],al          ; transfer program name
    or  al,al               ; stop transfer after first zero byte transfer
    je  cp_search           ; now see if file exists in this path
    inc si                  ; move to next name character slot
    inc di
    jmp SHORT cp_5          ; loop for next character

cp_search:
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    call    restore_ems_map
    jnc cp_prog_found       ; found the program
	call	file_not_found	; check for file not found errors

cp_6:
    pop si                  ; restore si -> environment block position
    cmp BYTE PTR es:[si],0  ; check last terminator
    je  cp_no_more_path     ; if zero then no more path prefixes to try
    cmp BYTE PTR es:[si+1],0    ; check character following last terminator
    je  cp_no_more_path     ; if zero then no more path prefixes to try
    jmp SHORT cp_search_again   ; not the final LIB path, keep trying

cp_no_more_path:
    mov ax,2                ; force program not found error
    jmp NEAR PTR cp_not_found

cp_search_again:
    inc si                  ; point to first character after last terminator
    jmp SHORT cp_3          ; try next path

cp_prog_found:
    mov di,OFFSET DGROUP:pathed_file    ; di -> filename with path from LIB e-var
    pop ax                  ; trash e-var position stored on stack
    pop ax                  ; trash old pointer to filename on stack
    pop dx
    pop si
    pop es                  ; restore critical register
    ret
check_libobj_path  ENDP

;*****************************
;* LOAD_LIBMOD               *
;*****************************

; seek to and load up to LIB_MAX_READ of library module for pass 1
; up to file length (64K max) for pass 2
; upon entry bx holds file length
; destroys ax,bx,cx,dx,di,si

load_libmod PROC
    xor ax,ax
    mov WORD PTR prev_pos_adj,ax    ; zero the previous position file adjustment
    mov WORD PTR prev_pos_adj+2,ax  ; zero high word as well
    mov buffer_head,ax      ; init buffer head to zero
    mov eof_flag,al         ; init end of file flag
    mov ax,buffer_end
    mov buffer_tail,ax      ; init buffer tail to buffer end
    cmp pass_number,1       ; see which pass number
    jne ll_pass2            ; pass two, use bx value for library read
    cmp udl_proc_pass,2     ; see if udl processing
    je  ll_pass2            ; yes, use bx value for library read

    mov ax,lib_read_amt     ; get library read amount
    or  ax,ax               ; see if preset to nonzero amount
    jne ll_1a               ; yes, use that
    mov ax,LIB_MAX_READ     ; no, use default read amount
    jmp SHORT ll_1          ; bypass pass 2 code

ll_pass2:
    mov ax,bx               ; library read amount in bx for pass 2

ll_1:
    mov lib_read_amt,ax     ; init library read amount (used if file reread on same module)

ll_1a:
    mov bx,lib_handle       ; get handle of library file in bx

; get file offset of module in file
    mov ax,lib_page_size
    mul lib_page_num        ; dx:ax has file offset of module
    mov cx,dx               ; cx == MSW of offset
    mov dx,ax               ; dx == LSW of offset
    mov WORD PTR file_pos_adj,dx    ; save offset for adjustment on file offset feedback
    mov WORD PTR file_pos_adj+2,cx  ; save high word
    mov WORD PTR lib_pos_adj,dx ; save offset for adjustment on library module feedback
    mov WORD PTR lib_pos_adj+2,cx   ; save high word

COMMENT #
; swap in library module if stashed in EMS and pass 2
    cmp pass_number,1       ; see which pass number
    je  ll_seek             ; pass one, no swap-in

    cmp ems_pages_flushed,0 ; see if pages in EMS were flushed for ovl, temp file
    jne ll_seek             ; yes

    mov si,10
    cmp WORD PTR es:[si],-1 ; see if module stashed in EMS (first page always set if so)
    je  ll_seek             ; no
    xor cl,cl               ; cl holds physical page

ll_emsloop:
    lods    WORD PTR es:0   ; get map page
    cmp ax,-1               ; see if not used (if not used, then no following ones are either)
    je  ll_emsmapped        ; not used, done mapping in
    mov bx,ax               ; get logical page in bx
    mov al,cl               ; get physical page in al
    call    map_ems_page    ; map in used pages
    inc cl                  ; bump physical page
    cmp cl,4                ; see if all physical pages mapped
    jb  ll_emsloop          ; not yet

; ems mapped in, now shift file bytes down to cover dictionary bytes from pass 1
ll_emsmapped:
    mov ax,es:[6]           ; get buffer tail (byte count) in ax
    mov bx,es:[18]          ; get file buffer start

    sub bx,ems_base         ; get offset from EMS base
    and bx,0f3ffh           ; place on nearest EMS page
    add bx,ems_base

    mov cx,bx               ; get EMS page, 4 high bits will roll off in byte computation
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1                ; cx holds bytes due to dictionary on this EMS page
    add cx,ax               ; add in byte count due to file

    mov es,ems_base
    xor di,di               ; es:di -> base of EMS
    push    ds              ; save critical register
    mov ds,bx
    xor si,si               ; ds:si -> file buffer start in EMS
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
    pop ds                  ; restore critical register
    jmp SHORT ll_ret2       ; bypass file i/o stuff and decryption (already decrypted if necessary)
END COMMENT #

ll_seek:
    mov ax,4200h            ; move file pointer, offset from beginning
    int 21h
    call    restore_ems_map
    jnc ll_2                ; no errors
    mov dx,OFFSET DGROUP:filename
    jmp NEAR PTR dos_error  ; error occurred

ll_2:
    mov cx,buffer_end       ; get number of bytes available in buffer to read
    cmp cx,lib_read_amt     ; don't read more than lib_read_amt of library module code
    jbe ll_3                ; lib_read_amt or less
    mov cx,lib_read_amt     ; force lib_read_amt read

ll_3:
    push    ds              ; save critical register
    xor dx,dx
    mov ds,buffer_base      ; DS:DX -> segment:offset of buffer area

    call    read_to_ems     ; read file, transfer to EMS is necessary

    pop ds                  ; restore critical register

ll_exit:
    cmp cx,ax               ; see if all bytes were read
    je  ll_ret              ; yes
    mov eof_flag,1          ; set end of file flag

ll_ret:
    call    decrypt         ; decrypt if SmartMem library

ll_ret2:
    mov buffer_tail,ax      ; set buffer tail at end of bytes read
    ret
load_libmod ENDP

END
