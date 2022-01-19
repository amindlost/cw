 ;*********************************************************************
;*   MLIMAGE2.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/31/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   memory image routines,part 2                                    *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlimage2
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
PUBLIC  zero_mem_image,create_temp_file,update_temp_file
PUBLIC  temp_file_write,dump_temp_file,make_temp_fbk
PUBLIC  ems_tempfile_create,ems_tempfile_write
PUBLIC	delete_temp_file

; variables
PUBLIC  temp_file_name,temp_file_pages,temp_page_ptr
PUBLIC  xms_tmp_handle

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   data_offset:DWORD,exe_handle:WORD
EXTRN   ems_first_avail:WORD,ems_trans_block:WORD
EXTRN   ems_pages_flushed:BYTE

; initialized local variables

; word values
EVEN
lru_counter DW  0   ; LRU counter value, used for figure which temp file page to swap to disk

; dword values
tmp_ems_pos     DD  0       ; current EMS write position (analogous to file position)

; byte strings

temp_file_name  DB  '.\VM.TMP',0,119 DUP (?)    ; default temporary file name

; structures
PAGE_CONTROL_STRUC  STRUC
    used    DB  ?   ; nonzero if page used
    counter DW  ?   ; LRU counter value of page
    range   DD  ?   ; beginning range of 4K page
PAGE_CONTROL_STRUC  ENDS

; array of 15 page control structures, init'ed to zero values
page_control    PAGE_CONTROL_STRUC  15 DUP (<0,0,0>)

.DATA?

; uninitialized local variables

; byte values
EVEN
temp_file_pages DB  ?       ; number of temporary file pages (1 to 15)
EVEN
unused_page DB  ?           ; pointer to unused page, 255 if none
EVEN

; word values
zero_block_size DW  ?       ; size of block of zero bytes written to temporary file
temp_page_ptr   DW  ?       ; segment pointer to 4K pages
data_remainder  DW  ?       ; data_offset modulo 0fffh
bytes_to_write  DW  ?       ; number of bytes to write to temp file
temp_buffer_size    DW  ?   ; size of temporary buffer
temp_buffer_base    DW  ?   ; segment pointer to start of temporary buffer
temp_path_end   DW  ?       ; end of temporary file path
ems_first_tmp   DW  ?       ; first EMS logical page used by temporary file
xms_tmp_handle  DW  ?       ; handle for temporary file in XMS

; doubleword values
data_page   DD  ?           ; data_offset 4K page value (updated by data writes crossing page boundary)

; structures
EMPB_STRUC  STRUC
    es_len  DD  ?           ; length of block in bytes
    es_src_handle   DW  ?   ; source EMB handle
    es_src_offset   DD  ?   ; source offset
    es_dest_handle  DW  ?   ; destination EMB handle
    es_dest_offset  DD  ?   ; destination offset
EMPB_STRUC  ENDS

tmp_empb    EMPB_STRUC  <>

;*****************************
;* Constant data             *
;*****************************

.CONST

EVEN
temptext_len    DB  temptext_stop-temp_file_text
temp_file_text  DB  CR,LF,'File image size exceeds available memory.'
                DB  CR,LF,'Temporary file created: '
temptext_stop   =   $

tmptext         DB  'TMP='      ; environment string to check for temporary file path
def_tempname    DB  'VM.TMP',0  ; default temporary file name

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   dos_error:NEAR,link_error:NEAR
EXTRN   map_ems_page:NEAR,restore_ems_map:NEAR,convert_to_page:NEAR
EXTRN   check_xms_error:NEAR,safe_xms_addr:NEAR
EXTRN	read_to_ems:NEAR

;*****************************
;* CREATE_TEMP_FILE          *
;*****************************

; create temporary file to hold executable image of 'image_size' size
; upon entry bx has number of free paragraphs (minimum of 500h, 20K's worth)
; destroys ax,bx,cx,dx,si,di,es

create_temp_file    PROC
    call    ems_tempfile_create ; create temporary file in EMS/XMS, if possible
    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    je  ctf_2               ; no, create file
    call    make_temp_pages ; get temp file memory pages for EMS pseudo-disk swapping
    ret

ctf_2:
    cmp bx,800h             ; check if more than 800h free paragraphs (32K)
    jbe ctf_3               ; no
    mov bx,800h             ; allocate only up to 32K for temporary disk write

ctf_3:
    mov ah,48h              ; allocate memory
    int 21h
    jnc ctf_4               ; should be successful, since using <= previous max value
    jmp NEAR PTR dos_error  ; any error is fatal

ctf_4:
    mov es,ax               ; es -> allocated block
    xor dx,dx               ; zap high word of block size in bytes
    shl bx,1                ; x2
    shl bx,1                ; x4
    shl bx,1                ; x8
    shl bx,1                ; x16, dx:bx contain block size in bytes (dx always zero, max 32K value)
    mov zero_block_size,bx  ; save the block size of zero bytes
    call    zero_mem_image  ; zero the bytes in the block

    call    make_temp_fbk   ; make temporary file and give feedback

; write image_size number of bytes to program in zero_block_size chunks
    mov bx,image_handle
    mov si,WORD PTR image_size  ; get low word
    mov di,WORD PTR image_size+2    ; get high word
    xor dx,dx               ; zero offset of write buffer

; di:si contain number of bytes to write, bx == file handle
ctf_write_file:
    or  di,di               ; see if byte count to write is 64K or more
    jne ctf_7               ; yes, write a zero_block_size chunk
    cmp si,zero_block_size  ; see if byte count is at least zero_block_size
    jb  ctf_9               ; no, exit zero_block_size writing loop

ctf_7:
    mov cx,zero_block_size  ; number of bytes to write
    push    ds              ; save ds, critical register
    mov ax,es
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore ds -> warplink data
    jnc ctf_8               ; no DOS errors

ctf_write_err:
    mov dx,OFFSET DGROUP:temp_file_name
    jmp NEAR PTR dos_error  ; error writing to file

ctf_8:
    or  ax,ax               ; see if out of disk space
    jne ctf_8a              ; no

; out of disk space for temporary file
ctf_diskspace:
    mov ax,TMP_DISK_FULL_ERR
    mov dx,OFFSET DGROUP:temp_file_name
    jmp NEAR PTR link_error ; transfer to error handler

ctf_8a:
    sub si,zero_block_size  ; back off number of bytes written from bytes to write
    sbb di,0                ; borrow to high word
    jmp SHORT ctf_write_file    ; loop for next write

; transfer leftover bytes (file size modulo zero_block_size)
ctf_9:
    mov cx,si               ; cx holds bytes to write
    jcxz    ctf_free        ; no bytes to write, close file
    push    ds              ; save ds, critical register
    mov ax,es
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore ds -> warplink data
    jc  ctf_write_err       ; error writing to file

    or  ax,ax               ; see out of disk space error
    je  ctf_diskspace       ; yes, abort with fatal error

; free allocated block
ctf_free:
    mov ah,49h              ; release memory
    int 21h
    jnc ctf_10              ; no errors
    jmp NEAR PTR dos_error  ; error releasing memory

ctf_10:
    call    make_temp_pages ; get temp file pages for disk swapping

ctf_ret:
    ret
create_temp_file    ENDP

;*****************************
;* MAKE_TEMP_PAGES           *
;*****************************

; allocate 4K pages to swap to disk
; destroys ax,bx

make_temp_pages PROC
    mov ah,48h              ; allocate memory
    mov bx,0ffffh           ; force request to fail, function will return largest available block
    int 21h
    cmp ax,8                ; insufficient memory error is expected
    je mtp_2
    jmp NEAR PTR dos_error  ; other errors are fatal

mtp_2:
    cmp bx,500h             ; must be at least 20K free memory
    jae mtp_3               ; okay
    jmp NEAR PTR dos_error  ; ax still has insufficient memory error value

mtp_3:
    sub bx,400h             ; subtract off 16K for use by other procedures

; bh holds count of 4K blocks available
    cmp bh,3                ; if 3 or less blocks available use all of them
    jbe mtp_4
    mov al,bh               ; get count of available 4K blocks
    mov bh,3                ; default blocks to 3
    shr al,1                ; /2
    shr al,1                ; /4, only allocate up to 25% of blocks available
    cmp al,3                ; allocate larger of 25% of blocks available or 3
    jbe mtp_4               ; 3 blocks or less
    mov bh,al
    cmp bh,15               ; don't allocate more than 15 4K blocks (60K, 0f00h paragraphs)
    jbe mtp_4               ; 15 or fewer 4K blocks available
    mov bh,15               ; set at maximum

mtp_4:
    mov temp_file_pages,bh  ; save count of 4K blocks
    xor bl,bl               ; zap low byte of paragraphs available (round down to nearest 4K block)
    mov ah,48h              ; allocate memory
    int 21h
    jnc mtp_5
    jmp NEAR PTR dos_error  ; error allocating memory

mtp_5:
    mov temp_page_ptr,ax    ; save pointer to block

mtp_ret:
    ret
make_temp_pages ENDP

;*****************************
;* MAKE_TEMP_FBK             *
;*****************************

; make the temporary file and give appropriate feedback
; destroys ax,bx,cx,dx

make_temp_fbk   PROC
    mov dx,OFFSET DGROUP:temp_file_name ; ds:dx -> ASCIIZ path spec
    xor cx,cx               ; create normal file
    cmp is_tempfile,0       ; see if temporary file specified
    jne mtf_2               ; yes, use it

    mov ax,dx
    add ax,2                ; point to first char past default path
    mov temp_path_end,ax    ; save pointer to memory variable
    call    check_tmp_var   ; check if TMP environment variable to use

    mov dx,OFFSET DGROUP:temp_file_name ; ds:dx -> ASCIIZ path spec
    xor cx,cx               ; create normal file
    cmp dos_version,2       ; see if we can use create temporary file dos function
    je  mtf_2               ; no, using DOS 2.x

; use DOS 3.x create temporary file function
    mov bx,temp_path_end
    mov BYTE PTR [bx],0     ; reduce name to path only
    mov ah,5ah              ; create temporary file
    int 21h
    call    restore_ems_map
    jnc mtf_3               ; no errors
    jmp NEAR PTR dos_error  ; error creating file

; use DOS 2.x create or truncate file (VM.TMP) function
mtf_2:
    mov ah,3ch              ; create or truncate file
    int 21h
    call    restore_ems_map
    jnc mtf_3
    jmp NEAR PTR dos_error  ; error creating file

mtf_3:
    mov image_handle,ax     ; save handle of temporary file

; print temporary file created feedback
    mov bx,OFFSET DGROUP:temp_file_text
    mov cl,[bx-1]           ; get length of string to print
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map

; print name of temporary file
    mov dx,OFFSET DGROUP:temp_file_name
    mov bx,dx
    xor cx,cx               ; cx will contain number of chars in file name

; mtf_loop computes number of chars in file name
mtf_loop:
    cmp BYTE PTR [bx],0     ; see if zero terminator in file name found
    je  mtf_print_name      ; yes
    inc cx                  ; bump count of chars in string
    inc bx                  ; move to next char slot to test
    jmp SHORT mtf_loop      ; loop back to test next char

mtf_print_name:
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map
    ret
make_temp_fbk   ENDP

;*****************************
;* CHECK_TMP_VAR             *
;*****************************

; check TMP environment variable when creating temporary file
; use it if it exists and is valid
; upon entry ds:dx -> default ASCIIZ file name, temp_path_end -> end of default name
; updates temp_path_end
; destroys ax,bx,cx,dx,si,di

check_tmp_var   PROC
    push    es              ; save critical register

; check for a TMP= in the environment
    mov bx,OFFSET DGROUP:tmptext    ; bx holds target string address for compares
    mov ax,psp
    mov es,ax               ; es -> warplink's PSP
    xor si,si               ; starting location for target string check
    mov ax,es:[2ch]         ; get environment segment from offset 2ch in PSP
    mov es,ax               ; es -> environment segment

ctv_find_path:
    xor di,di               ; offset into target string

ctv_loop:
    mov al,es:[si]          ; get byte from environment string
    inc si                  ; point to next char in environment
    cmp al,[bx+di]          ; does environment char match TMP string char
    je  ctv_byte_match      ; yes, try next location
    or  al,es:[si]          ; two zero values in a row mean the end of the environment
    je  ctv_ret             ; all done searching environment, no TMP variable
    jmp SHORT ctv_find_path ; not at end of environment, keep searching

; check that TMP is not part of another environment string
ctv_byte_match:
    or  di,di               ; di is zero if first char is matched
    jne ctv_2               ; not first char, test already done
    cmp si,1                ; si equals one if TMP is first string in environment block
    je  ctv_2               ; no previous environment string
    cmp BYTE PTR es:[si-2],0    ; check if char before TMP was nonzero
    jne ctv_find_path       ; yes, TMP is a subset of another string, keep looking

ctv_2:
    inc di                  ; a match, move to next byte of target string
    cmp di,4                ; check if all bytes matched
    jb  ctv_loop            ; not yet, keep comparing

; es:si -> temporary file path
ctv_path_found:
    mov bx,ds               ; save -> warplink data
    mov ax,es
    mov ds,ax
    mov dx,si               ; ds:dx -> temporary file path
    mov ax,4300h            ; get file attribute
    int 21h
    call    restore_ems_map
    jnc ctv_path_good       ; path was okay
    mov ds,bx               ; restore ds -> warplink data
    jmp SHORT ctv_ret       ; error accessing directory, return

ctv_path_good:
    mov si,dx               ; ds:si -> temporary file path
    mov es,bx
    mov di,OFFSET DGROUP:temp_file_name ; es:di -> fully pathed temporary file name

ctv_tranloop:
    lodsb                   ; get char of path
    or  al,al               ; check if zero terminator
    je  ctv_3               ; yes, end transfer
    stosb                   ; transfer char
    jmp SHORT ctv_tranloop  ; loop until complete

; done transferring path
ctv_3:
    mov ds,bx               ; ds -> warplink data
    mov temp_path_end,di    ; update pointer to end of path
    cmp BYTE PTR [di-1],'\' ; see if directory char is last char
    je  ctv_4               ; yes

    mov BYTE PTR [di],'\'   ; add directory char prior to adding file name
    inc di                  ; point past directory '\' char

ctv_4:
    mov si,OFFSET DGROUP:def_tempname   ; ds:si -> default file name
    mov cx,7                ; transfer 7 bytes
    rep movsb

ctv_ret:
    pop     es              ; restore critical register
    ret
check_tmp_var   ENDP

;*****************************
;* TEMP_FILE_WRITE           *
;*****************************

; write bytes to temporary file
; upon entry es:[si] -> buffer to write from, cx == number of bytes to write,
; data_offset == offset from start of program
; updates si
; destroys ax,bx,cx,dx,di

temp_file_write PROC
    mov ax,WORD PTR data_offset+2   ; get high word of offset
    mov WORD PTR data_page+2,ax     ; save to page high word
    mov ax,WORD PTR data_offset     ; get low word of offset
    mov bx,ax               ; save value
    and ax,0f000h           ; round to 4K page
    mov WORD PTR data_page,ax   ; save page low word value
    mov ax,bx               ; get offset low word value
    and ax,0fffh            ; convert to page remainder
    mov data_remainder,ax   ; save to memory variable
    mov bx,1000h
    sub bx,ax               ; compute bytes left to write on page
    cmp cx,bx               ; check if bytes to write exceed bytes left on page
    ja  tfw_2               ; yes

tfw_last_write:
    mov ax,cx               ; get number of bytes to write in ax for page_write procedure
    call    temp_page_write ; write bytes to page
    jmp SHORT tfw_ret       ; done

tfw_2:
    sub cx,bx               ; subtract bytes written (to be written) from total to write
    mov ax,bx               ; get bytes that can be written on page in ax
    call    temp_page_write ; write bytes to page

    mov data_remainder,0    ; zero data remainder (all bytes on subsequent pages can be written to)

tfw_write_loop:
    mov ax,1000h
    add WORD PTR data_page,ax   ; bump to next page
    adc WORD PTR data_page+2,0  ; carry to high word
    cmp cx,ax               ; see if more than one page to write
    jbe tfw_last_write      ; no
    sub cx,ax               ; subtract bytes written (to be written)
    call    temp_page_write ; write 4K bytes to page
    jmp SHORT tfw_write_loop    ; loop till no more bytes to write

tfw_ret:
    ret
temp_file_write ENDP

;*****************************
;* TEMP_PAGE_WRITE           *
;*****************************

; write bytes to temp file page
; upon entry es:[si] -> buffer to write from, ax == number of bytes to write
; destroys ax,bx,dx,di

temp_page_write PROC
    push    cx              ; save critical register
    mov bytes_to_write,ax   ; save number of bytes to write
    mov unused_page,255     ; init unused page pointer to not valid value
    xor ch,ch
    mov cl,temp_file_pages  ; get number of pages to loop through
    inc lru_counter         ; bump LRU counter value
    jnz tpw_2               ; didn't wrap

; reset values on all page control counter bytes
    mov dx,cx               ; save number of pages
    xor ax,ax
    mov bx,OFFSET DGROUP:page_control.counter   ; point to first structure's counter word

tpw_counter_loop:
    mov [bx],ax             ; zero it
    add bx,7                ; point to next counter byte
    loop    tpw_counter_loop    ; zero all structure's counter words
    mov cx,dx               ; restore number of pages to cx

tpw_2:
    mov di,65535            ; init counter value to check against
    mov dx,1                ; init page containing lowest counter value, relative 1
    mov ax,cx               ; get number of pages
    dec ax                  ; make relative zero
    mov bx,ax               ; save value
    shl ax,1                ; x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    sub ax,bx               ; x7
    mov bx,OFFSET DGROUP:page_control   ; point to first page control structure
    add bx,ax               ; bx -> last page control structure

tpw_page_loop:
    cmp BYTE PTR [bx],0     ; see if page used
    je  tpw_unused_page     ; no
    mov ax,WORD PTR data_page   ; get low word of page to write to
    cmp ax,[bx+3]           ; see if page matches
    jne tpw_nomatch         ; no
    mov ax,WORD PTR data_page+2 ; get high word of page to write to
    cmp ax,[bx+5]           ; see if page matches
    jne tpw_nomatch         ; no
    mov dx,cx               ; dx holds page number
    dec dx                  ; make relative zero
    mov dh,dl               ; register move does effective x256 value multiply
    xor dl,dl               ; zero low byte
    shl dh,1                ; x512
    shl dh,1                ; x1024
    shl dh,1                ; x2048
    shl dh,1                ; x4096, dx holds offset to proper page from page base
    jmp NEAR PTR tpw_write_bytes    ; yes, write the bytes to the page

; page to write to does not match page found
tpw_nomatch:
    cmp di,[bx+1]           ; see if new low counter value
    jbe tpw_toloop          ; no
    mov di,[bx+1]           ; update new low counter value
    mov dx,cx               ; update page containing lowest counter value
    jmp SHORT tpw_toloop

; page found is unused
tpw_unused_page:
    mov unused_page,cl      ; save pointer to unused page

tpw_toloop:
    sub bx,7                ; drop bx to next page control structure
    loop    tpw_page_loop   ; loop back for next page

; loop drops through, no matching pages were found
    mov cl,unused_page
    cmp cl,255              ; see if an unused page was found
    jne tpw_update          ; yes

; no unused page found, no pages match page to write to
; swap the least recently used page (in dx) to disk
    mov di,dx               ; save page swapped out in di

; seek to proper file position to write to
    dec dx                  ; make page number relative zero
    mov bx,dx               ; bx == number of page to write to

; convert page number in bx to 7-byte offset
    shl bx,1                ; x2
    shl bx,1                ; x4
    shl bx,1                ; x8
    sub bx,dx               ; x7
    add bx,OFFSET DGROUP:page_control   ; bx -> proper page control structure for page
    mov cx,[bx+5]           ; MSW of file offset
    mov dx,[bx+3]           ; LSW of file

    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    je  tpw_fileseek        ; no
    call    ems_tempfile_seek
    jmp SHORT tpw_file_write

tpw_fileseek:
    mov bx,image_handle     ; get handle of temporary file
    mov ax,4200h            ; move file pointer, offset from start
    int 21h
    call    restore_ems_map
    jnc tpw_file_write     ; no errors in seek

tpw_file_err:
    mov dx,OFFSET DGROUP:temp_file_name
    jmp NEAR PTR dos_error  ; DOS file error

; write old page to file
tpw_file_write:
    mov dx,di               ; get page swapped out
    dec dx                  ; make page number relative zero
    mov dh,dl               ; register move does effective x256 value multiply
    xor dl,dl               ; zero low byte
    shl dh,1                ; x512
    shl dh,1                ; x1024
    shl dh,1                ; x2048
    shl dh,1                ; x4096, dx holds offset to proper page from page base
    mov cx,4096             ; write 4K bytes
    mov ax,temp_page_ptr    ; get page base
    push    ds              ; save critical register

    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    je  tpw_filewrite       ; no
    mov ds,ax               ; ds:dx -> buffer area to write
    call    ems_tempfile_write
    pop ds                  ; restore critical register
    mov cx,di               ; cx == page to write to (page just swapped out)
    jmp SHORT tpw_update

tpw_filewrite:
    mov ds,ax               ; ds:dx -> buffer area to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore critical register
    mov cx,di               ; cx == page to write to (page just swapped out)
    jc  tpw_file_err        ; error writing to file

; update page control structure
tpw_update:
    dec cx                  ; make page number relative zero
    mov di,cx               ; save page number relative 0 in di
    mov bx,cx               ; bx == number of page to write to

; convert page number in bx to 7-byte offset
    shl bx,1                ; x2
    shl bx,1                ; x4
    shl bx,1                ; x8
    sub bx,cx               ; x7
    add bx,OFFSET DGROUP:page_control   ; bx -> proper page control structure for page
    mov BYTE PTR [bx],1     ; flag page is used
    mov ax,lru_counter
    mov [bx+1],ax           ; update counter in appropriate page control structure
    mov ax,WORD PTR data_page
    mov [bx+3],ax           ; update low word of range
    mov dx,ax               ; save as LSW of file offset
    mov ax,WORD PTR data_page+2
    mov [bx+5],ax           ; update high word of range
    mov cx,ax               ; save as MSW of file offset

; seek to new page in file
    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    je  tpw_fileseek2       ; no
    call    ems_tempfile_seek
    jmp SHORT tpw_readpage

tpw_fileseek2:
    mov bx,image_handle     ; get handle of temporary file
    mov ax,4200h            ; move file pointer, offset from start
    int 21h
    call    restore_ems_map

to_tpw_file_err:
    jc  tpw_file_err        ; error seeking to new position

; read page in from file
tpw_readpage:
    mov dx,di               ; get page number (relative zero) to read in from file
    mov dh,dl               ; register move does effective x256 value multiply
    xor dl,dl               ; zero low byte
    shl dh,1                ; x512
    shl dh,1                ; x1024
    shl dh,1                ; x2048
    shl dh,1                ; x4096, dx holds offset to proper page from page base
    mov cx,4096             ; read 4K bytes
    mov ax,temp_page_ptr    ; get page base
    push    ds              ; save critical register

    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    je  tpw_fileread        ; no
    mov ds,ax               ; ds:dx -> buffer area to read into
    call    ems_tempfile_read
    pop ds                  ; restore critical register
    jmp SHORT tpw_write_bytes

tpw_fileread:
    mov ds,ax               ; ds:dx -> buffer area to read into
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore critical register
    jc  to_tpw_file_err     ; error reading from file

; write bytes to memory page
tpw_write_bytes:
    mov bx,es
    mov di,data_remainder   ; get page position to start writing
    add di,dx               ; add in page offset from base
    mov ax,temp_page_ptr
    mov es,ax               ; es:di -> location in memory image to place data
    mov cx,bytes_to_write   ; get number of bytes to transfer in cx
    push    ds              ; save critical register
    mov ds,bx               ; ds:si -> source buffer

    mov ax,si               ; get offset of source
    add ax,cx               ; add in number of bytes to write
    jc  tpw_buff_wrap       ; overflow, buffer will wrap
    cmp ax,bp               ; see if past buffer end for total bytes written
    jbe tpw_no_buff_wrap    ; no

tpw_buff_wrap:
    mov dx,cx               ; save old byte count to write
    mov cx,bp               ; get buffer end
    sub cx,si               ; compute bytes to buffer end
    mov ax,cx               ; save byte count written this pass
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
    xor si,si               ; wrap source offset to start of i/o buffer
    sub dx,ax               ; update total count, subtracting off byte count written
    mov cx,dx               ; get new total in cx

tpw_no_buff_wrap:
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

tpw_ret:
    mov ax,ds
    mov es,ax               ; restore es -> file buffer
    pop ds                  ; restore critical register

    pop cx
    ret
temp_page_write ENDP

;*****************************
;* UPDATE_TEMP_FILE          *
;*****************************

; flush pages to disk
; destroys ax,bx

update_temp_file PROC
    call    flush_temp_pages    ; flush temp file pages to disk
    mov ax,temp_page_ptr    ; get page base segment pointer
    mov es,ax               ; es -> memory block to be released
    mov ah,49h              ; release memory
    int 21h
    jnc cltp_ret            ; no errors releasing memory
    jmp NEAR PTR dos_error

cltp_ret:
    ret
update_temp_file ENDP

;*****************************
;* FLUSH_TEMP_PAGES          *
;*****************************

; flush temporary file pages to disk
; destroys ax,bx,cx,dx,di

flush_temp_pages    PROC
    mov bx,OFFSET DGROUP:page_control   ; point to first page control structure
    xor di,di               ; init page number to zero
    xor ch,ch
    mov cl,temp_file_pages  ; get number of pages to loop through

ftp_loop:
    push    cx              ; save loop counter
    push    bx              ; save bx -> page control structures
    cmp BYTE PTR [bx],0     ; see if page used
    je  ftp_2               ; no

; seek to proper file position to write to
    mov bx,di               ; get page number, relative zero

; convert page number in bx to 7-byte offset
    shl bx,1                ; x2
    shl bx,1                ; x4
    shl bx,1                ; x8
    sub bx,di               ; x7
    add bx,OFFSET DGROUP:page_control   ; bx -> proper page control structure for page
    mov cx,[bx+5]           ; MSW of file offset
    mov dx,[bx+3]           ; LSW of file

    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    je  ftp_fileseek        ; no
    call    ems_tempfile_seek
    jmp SHORT ftp_file_write

ftp_fileseek:
    mov bx,image_handle     ; get handle of temporary file
    mov ax,4200h            ; move file pointer, offset from start
    int 21h
    call    restore_ems_map
    jnc ftp_file_write     ; no errors in seek

ftp_file_err:
    mov dx,OFFSET DGROUP:temp_file_name
    jmp NEAR PTR dos_error  ; DOS file error

; write to file
ftp_file_write:
    mov dx,di               ; get page number relative zero
    mov dh,dl               ; register move does effective x256 value multiply
    xor dl,dl               ; zero low byte
    shl dh,1                ; x512
    shl dh,1                ; x1024
    shl dh,1                ; x2048
    shl dh,1                ; x4096, dx holds offset to proper page from page base
    mov cx,4096             ; write 4K bytes
    mov bx,image_handle     ; get handle of temporary file
    mov ax,temp_page_ptr    ; get page base
    push    ds              ; save critical register

    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    je  ftp_filewrite       ; no
    mov ds,ax               ; ds:dx -> buffer area to write
    call    ems_tempfile_write
    pop ds                  ; restore critical register
    jmp SHORT ftp_2

ftp_filewrite:
    mov ds,ax               ; ds:dx -> buffer area to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore critical register
    jc  ftp_file_err        ; error writing to file

ftp_2:
    pop bx                  ; restore bx -> page control structures
    pop cx                  ; restore loop counter
    add bx,7                ; point to next page control structure
    inc di                  ; bump page number
    loop    ftp_loop        ; loop till complete
    ret
flush_temp_pages    ENDP

;*****************************
;* DUMP_TEMP_FILE            *
;*****************************

; dump temporary file bytes to executable file
; upon entry di:si contain number of bytes to write to executable file
; destroys ax,bx,cx,dx,di,si

dump_temp_file  PROC
    cmp tmp_in_xms,0        ; see if XMS used to hold temporary file
    jne to_dte              ; yes
    cmp is_no_ems,0         ; see if EMS used
    jne dtf_1               ; no
    cmp tmp_in_emsxms,0     ; see if temporary file in EMS (known not in XMS)
    je  dtf_use_ems         ; no, use EMS for buffering

to_dte:
    jmp NEAR PTR dump_tempfile_ems  ; dump temporary file in EMS

; tempory file not in EMS, but EMS is used,
; by definition it's okay to use physical page 0 for i/o buffer
dtf_use_ems:
    xor bx,bx
    mov al,bl
    call    map_ems_page    ; map page 0
COMMENT #
    inc bx
    mov al,bl
    call    map_ems_page    ; map page 1
    inc bx
    mov al,bl
    call    map_ems_page    ; map page 2
    inc bx
    mov al,bl
    call    map_ems_page    ; map page 3
    mov ax,0fff0h
END COMMENT #

	mov	ax,16384
    mov temp_buffer_size,ax ; keep size of temporary buffer
    mov ax,ems_base         ; use base of EMS memory as temporary base
    mov temp_buffer_base,ax
    jmp SHORT dtf_4a

dtf_1:
    mov ah,48h              ; allocate memory
    mov bx,0ffffh           ; force request to fail, function will return largest available block
    int 21h
    cmp ax,8                ; insufficient memory error is expected
    je dtf_2
    jmp NEAR PTR dos_error  ; other errors are fatal

dtf_2:
    cmp bx,0fffh            ; check if more than 0fffh free paragraphs (64K-16 bytes)
    jbe dtf_3               ; no
    mov bx,0fffh            ; allocate only up to 64K-16 for temporary disk write

dtf_3:
    mov ax,bx
    shl ax,1                ; convert paragraphs allocated to bytes, x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    shl ax,1                ; x16
    mov temp_buffer_size,ax ; keep size of temporary buffer
    mov ah,48h              ; allocate memory
    int 21h
    jnc dtf_4               ; should be successful, since using <= previous max value
    jmp NEAR PTR dos_error  ; any error is fatal

dtf_4:
    mov temp_buffer_base,ax ; save address of temporary buffer

dtf_4a:
    mov bx,image_handle
    xor cx,cx               ; move file pointer to start of file
    mov dx,cx
    mov ax,4200h            ; move file pointer, offset from file start
    int 21h
    call    restore_ems_map
    jnc dtf_5               ; no errors in seek

dtf_temp_err:
    mov dx,OFFSET DGROUP:temp_file_name
    jmp NEAR PTR dos_error  ; DOS file error

dtf_5:
    or  di,di               ; see if byte count to write is 64K or more
    jne dtf_6               ; yes, write a zero_block_size chunk
    cmp si,temp_buffer_size ; see if byte count is at least temp_buffer_size
    jb  dtf_8               ; no, exit temp_buffer_size writing loop

dtf_6:
    mov cx,temp_buffer_size ; number of bytes to read
    xor dx,dx               ; zero offset into buffer
    mov bx,image_handle
    mov ax,temp_buffer_base
    push    ds              ; save ds, critical register

	cmp	ax,ems_base			; see if reading to EMS
    mov ds,ax               ; ds -> segment to start write, DON'T MODIFY FLAGS
	jne	dtf_fileread1		; no
	call	read_to_ems		; call read to EMS code
	jmp	SHORT dtf_readdone1

dtf_fileread1:
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map

dtf_readdone1:
    pop ds                  ; restore ds -> warplink data
    jc  dtf_temp_err        ; error reading from file

dtf_6a:
    mov cx,temp_buffer_size ; number of bytes to write
    xor dx,dx               ; zero offset into buffer
    mov bx,exe_handle
    mov ax,temp_buffer_base
    push    ds              ; save ds, critical register
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore ds -> warplink data
    jc  dtf_exe_err         ; error occurred

dtf_7:
    or  ax,ax               ; see if out of disk space
    jne dtf_7a              ; no

; out of disk space for executable file
dtf_diskspace:
    mov ax,DISK_FULL_ERR

dtf_exe_err:
    mov dx,OFFSET DGROUP:exe_name
    jmp NEAR PTR link_error ; transfer to error handler

dtf_7a:
    sub si,temp_buffer_size ; back off number of bytes written from bytes to write
    sbb di,0                ; borrow to high word
    jmp SHORT dtf_5         ; loop for next write

; transfer leftover bytes (file size modulo temp_buffer_size)
dtf_8:
    mov cx,si               ; cx holds bytes to write
    jcxz    dtf_release     ; no bytes to write, release memory, close files
    xor dx,dx               ; zero offset into buffer
    mov bx,image_handle
    mov ax,temp_buffer_base
    push    ds              ; save ds, critical register

	cmp	ax,ems_base			; see if reading to EMS
    mov ds,ax               ; ds -> segment to start write, DON'T MODIFY FLAGS
	jne	dtf_fileread2		; no
	call	read_to_ems		; call read to EMS code
	jmp	SHORT dtf_readdone2

dtf_fileread2:
    mov ah,3fh              ; read from file
    int 21h
    call    restore_ems_map

dtf_readdone2:
    pop ds                  ; restore ds -> warplink data
    jnc dtf_9               ; no error
    mov dx,OFFSET DGROUP:temp_file_name
    jmp NEAR PTR dos_error  ; DOS file error

dtf_9:
    mov cx,si               ; cx holds bytes to write
    xor dx,dx               ; zero offset into buffer
    mov bx,exe_handle
    mov ax,temp_buffer_base
    push    ds              ; save ds, critical register
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore ds -> warplink data
    jc  dtf_exe_err         ; error writing to file

    or  ax,ax               ; see if out of disk space
    je  dtf_diskspace       ; yes

; release temporary buffer back to dos if not thru EMS
dtf_release:

    cmp is_no_ems,0         ; see if EMS used for i/o buffer
    je  dtf_close           ; yes, no memory to release

    mov es,temp_buffer_base
    mov ah,49h              ; release memory
    int 21h
    jc  dtf_exe_err         ; error occurred in memory release

; done read from temporary file, close and delete it if not in EMS
dtf_close:
	call	delete_temp_file
    ret
dump_temp_file  ENDP

;*****************************
;* DELETE_TEMP_FILE          *
;*****************************

; delete temporary file from disk, if it exists
; destroys ax,bx,dx

delete_temp_file	PROC
	xor	ax,ax
    cmp is_ondisk,al		; check if temporary file
    je  dlf_ret				; no

    cmp tmp_in_emsxms,al	; see if temporary file is in EMS/XMS
    jne dlf_ret				; yes

; close and delete temporary file, no longer needed
    mov bx,image_handle
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

    mov dx,OFFSET DGROUP:temp_file_name
    mov ah,41h              ; delete file
    int 21h
    call    restore_ems_map

dlf_ret:
	ret
delete_temp_file	ENDP

;*****************************
;* ZERO_MEM_IMAGE            *
;*****************************

; zero executable memory image
; upon entry ax contains segment of memory image
; dx:bx contain count of bytes to zero
; destroys ax,di

zero_mem_image  PROC
    push    es              ; save critical register
    push    bx
    push    dx
    mov es,ax               ; es -> memory image
    mov ax,bx
    or  ax,dx               ; make sure that memory image size is nonzero
    je  zmi_ret             ; zero memory image size

zmi_loop:
    xor di,di               ; zero offset into block
    mov ax,di               ; zero word to store
    or  dx,dx               ; see if high word set
    jne zmi_2               ; yes, a 32K can be zero'ed
    cmp bx,32768            ; see if 32K chunk to zero out in low word
    jb  zmi_3               ; no, zero out leftover amount

zmi_2:
    sub bx,32768            ; subtract bytes zero'd
    sbb dx,0                ; borrow to high word
    mov cx,16384            ; zero 16K words (32K bytes)
    rep stosw               ; do it
    mov cx,es
    add cx,800h             ; adjust past 32K block zero'd (800h paragraphs)
    mov es,cx               ; update segment pointer
    jmp SHORT zmi_loop      ; loop back for next chunk to zero

zmi_3:
    mov cx,bx               ; get byte count in cx
    shr cx,1                ; convert byte count to zero to words
    rep stosw               ; store zeros
    rcl cx,1                ; pick up carry
    rep stosw               ; zero leftover byte, if any

zmi_ret:
    pop dx                  ; restore critical register
    pop bx
    pop es
    ret
zero_mem_image  ENDP

;*****************************
;* DUMP_TEMPFILE_EMS         *
;*****************************

; dump temporary file bytes EMS/XMS to executable file
; upon entry di:si contain number of bytes to write to executable file
; destroys ax,bx,cx,dx,di,si,bp

dump_tempfile_ems   PROC

    cmp tmp_in_xms,0        ; see if temporary file in XMS
    je  dte_ems             ; no
    jmp NEAR PTR dump_tempfile_xms  ; dump temporary file to XMS

dte_ems:
    xor bp,bp
    add bp,ems_first_tmp    ; bp holds current EMS page of temp file

dte_writeloop:
    mov cx,16384            ; get bytes to write chunk
    or  di,di               ; see if byte count to write is 64K or more
    jne dte_mapin           ; yes, write a 16K chunk
    cmp si,cx               ; see if byte count left is at least 16K
    jae dte_mapin           ; yes, use 16K
    mov cx,si               ; no, use amount left to write

dte_mapin:
    jcxz    dte_ret         ; no more bytes to write
    mov bx,bp               ; get logical page
    xor al,al               ; map in EMS physical page 0
    call    map_ems_page

    xor dx,dx               ; zero offset into buffer
    mov bx,exe_handle
    mov ax,ems_base
    push    ds              ; save ds, critical register
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
    pop ds                  ; restore ds -> warplink data
    jc  dte_doserr          ; error occurred

    or  ax,ax               ; see if out of disk space
    jne dte_2               ; no

; out of disk space for executable file
    mov ax,DISK_FULL_ERR

dte_doserr:
    mov dx,OFFSET DGROUP:exe_name
    jmp NEAR PTR link_error ; transfer to error handler

dte_2:
    sub si,ax               ; back off number of bytes written from bytes to write
    sbb di,0                ; borrow to high word
    inc bp                  ; bump logical page
    jmp SHORT dte_writeloop ; loop for next write

dte_ret:
    ret
dump_tempfile_ems   ENDP

;*****************************
;* DUMP_TEMPFILE_XMS         *
;*****************************

; dump temporary file bytes EMS/XMS to executable file
; upon entry di:si contain number of bytes to write to executable file
; destroys ax,bx,cx,dx,di,si,bp

dump_tempfile_xms   PROC
    xor ax,ax
    mov WORD PTR tmp_empb.es_src_offset,ax  ; zero init the XMS offset in parameter block
    mov WORD PTR tmp_empb.es_src_offset+2,ax
    mov WORD PTR tmp_empb.es_len+2,ax   ; zero high word of block length
    mov tmp_empb.es_dest_handle,ax  ; zero handle (conventional memory destination)
    mov WORD PTR tmp_empb.es_dest_offset,ax ; zero offset

    mov ax,xms_tmp_handle
    mov tmp_empb.es_src_handle,ax   ; source is XMS
    mov ax,ems_trans_block
    mov WORD PTR tmp_empb.es_dest_offset+2,ax   ; destination segment
    mov bp,si               ; get low word of bytes to write in bp

dtx_xloop:
    or  di,di               ; see if high word of bytes to write is nonzero
    jne dtx_2               ; nonzero, write 16K block
    mov cx,bp               ; get low word of bytes to write
    jcxz    dtx_ret         ; no bytes to write, done
    cmp cx,16384            ; only write up to 16K
    jb  dtx_3               ; below 16K

dtx_2:
    mov cx,16384            ; write 16K block

dtx_3:
    mov ax,cx
    inc ax
    and al,0feh             ; round to word boundary for XMM acceptable length
    mov WORD PTR tmp_empb.es_len,ax ; length of block to transfer low word

    mov ah,0bh              ; move extend memory block
    mov si,OFFSET DGROUP:tmp_empb   ; ds:si -> parameter block
    call    DWORD PTR xms_addr
    call    check_xms_error ; see if error occurred

    mov bx,exe_handle
    xor dx,dx
    push    ds              ; save critical register
    mov ds,ems_trans_block  ; ds:dx -> write buffer
    mov ah,40h              ; write file
    int 21h
    pop ds                  ; restore ds -> warplink data
    call    restore_ems_map
    jc  dtx_doserr          ; error writing file

    add WORD PTR tmp_empb.es_src_offset,cx  ; bump dword offset in XMS
    adc WORD PTR tmp_empb.es_src_offset+2,0 ; carry to high word
    sub bp,cx               ; subtract off bytes written
    sbb di,0                ; borrow to high word
    mov ax,bp
    or  ax,di               ; see if written all bytes in temporary file
    jne dtx_xloop           ; not done yet

dtx_ret:
    ret

dtx_doserr:
    mov dx,OFFSET DGROUP:exe_name
    jmp NEAR PTR dos_error  ; error opening file

dump_tempfile_xms   ENDP

;*****************************
;* EMS_TEMPFILE_CREATE       *
;*****************************

; create temporary file in EMS if there is room, zero fill pages
; if temporary file successfully placed in EMS, set tmp_in_emsxms flag to nonzero
; destroys ax,bx,cx,dx,di

ems_tempfile_create PROC
    cmp is_xms,0            ; see if XMS used
    jne etc_getblock        ; yes
    cmp is_no_ems,0         ; see if EMS is available
    je  etc_getblock        ; yes
    ret

etc_getblock:
    push    es              ; save critical register
    push    di
    push    si

    mov ax,WORD PTR image_size  ; get size of temporary file in bytes
    mov dx,WORD PTR image_size+2
    add ax,16383            ; round up to next page
    adc dx,0                ; carry to high word
    call    convert_to_page ; convert bytes in dx:ax to 16K blocks in ax

; check if enough XMS to save temporary file, priority over EMS
    cmp is_xms,0            ; see if XMS used
    je  etc_chkems          ; no
    mov di,ax               ; save 16K block count in di
    shl ax,1                ; *2
    shl ax,1                ; *4
    shl ax,1                ; *8
    shl ax,1                ; *16, ax holds 1K block count
    mov cx,ax               ; save 1K block count
    mov ah,8                ; query free extended memory
    call    DWORD PTR xms_addr
    call    check_xms_error ; see if error occurred
    cmp ax,cx               ; see if largest free block meets 1K block count
    jae etc_usexms          ; yes, use XMS

; not enough XMS to stuff temporary file in it, try EMS
eos_noxms:
    cmp is_no_ems,0         ; see if EMS used
    jne etc_ret             ; no
    mov ax,di               ; get 16K block count back in ax

etc_chkems:
    cmp ax,ems_page_avail   ; see if enough pages are available to hold temporary file
	jbe	etc_success			; yes

; not enough available pages, see if overlays used, if not, see if flushing will work
    cmp ovl_count,0         ; see if any overlays
    jne etc_ret				; yes

; see if flushing will give enough room for temporary file stashing
	mov di,ems_pagecount	; get total ems pages
	sub di,4				; subtract amount used for file i/o buffer
	cmp ax,di				; see if enough total ems pages allocated to hold temporary file
	ja  etc_ret				; no, not enough room

; flush out stashing of modules
	mov ems_pages_flushed,1	; set pages flushed flag
	mov ems_first_avail,4	; reset first available page flag
	mov ems_page_avail,di	; adjust pages available back to pre-module stashed status

etc_success:
    sub ems_page_avail,ax   ; subtract off pages used by temporary file
    mov cx,ax               ; save temporary file page count
    mov ax,ems_first_avail  ; get first available page
    mov ems_first_tmp,ax    ; save as first used temporary file page
    add ems_first_avail,cx  ; bump up first available page past those used by temporary file
    mov tmp_in_emsxms,1     ; set temporary file in EMS/XMS flag

    push    WORD PTR ems_currmap    ; save current logical state of physical page 0
    mov bx,ems_first_tmp    ; bx holds first EMS logical page for temporary file
    mov es,ems_base

etc_loop:
    push    cx              ; save pages left to map in and zero
    xor al,al
    call    map_ems_page    ; map in proper page
    xor di,di               ; es:di -> EMS page frame physical page zero
    mov ax,di
    mov cx,8192
    rep stosw               ; zero 16K page
    inc bx                  ; bump EMS logical page
    pop cx                  ; restore pages left
    loop    etc_loop        ; loop until complete

; restore previous logical state of physical page 0
    xor al,al
    pop bx
    call    map_ems_page

etc_ret:
    pop si                  ; restore critical register
    pop di
    pop es
    ret

etc_usexms:
    mov dx,cx               ; get blocks to allocate in K
    mov ah,9                ; allocate extended memory block
    call    DWORD PTR xms_addr
    call    check_xms_error ; see if error occurred
    mov xms_tmp_handle,dx   ; save xms handle
    mov tmp_empb.es_dest_handle,dx  ; save in parameter block

    xor ax,ax
    mov WORD PTR tmp_empb.es_dest_offset,ax ; zero init the XMS offset in parameter block
    mov WORD PTR tmp_empb.es_dest_offset+2,ax
    mov WORD PTR tmp_empb.es_len+2,ax   ; zero high word of block length
    mov tmp_empb.es_src_handle,ax   ; zero handle (conventional memory source)
    mov WORD PTR tmp_empb.es_src_offset,ax  ; zero offset

    inc al
    mov tmp_in_emsxms,al    ; set temporary file in EMS/XMS flag
    mov tmp_in_xms,al       ; set temporary file in XMS only flag

    mov ax,16384
    mov WORD PTR tmp_empb.es_len,ax ; length of block to transfer low word
    mov ax,ems_trans_block
    mov WORD PTR tmp_empb.es_src_offset+2,ax    ; source segment

; zero the source 16K transfer block
    mov bx,di               ; save count of 16K pages in bx
    mov es,ax
    xor di,di
    mov cx,8192             ; 8K words
    xor ax,ax
    rep stosw

; transfer to XMS
etc_xmstrans:
    mov ah,0bh              ; move extend memory block
    mov si,OFFSET DGROUP:tmp_empb   ; ds:si -> parameter block
    call    safe_xms_addr
    call    check_xms_error ; see if error occurred

    add WORD PTR tmp_empb.es_dest_offset,16384  ; bump dword offset in XMS
    adc WORD PTR tmp_empb.es_dest_offset+2,0    ; carry to high word

    dec bx                  ; drop count of pages to read
    jne etc_xmstrans        ; not done yet
    jmp SHORT etc_ret

ems_tempfile_create ENDP

;*****************************
;* EMS_TEMPFILE_SEEK         *
;*****************************

; seek to temporary file page/position in EMS
; analogous to seek from start of file, function 4200h
; upon entry cx:dx -> absolute file position
; destroys ax,bx,cx,dx

ems_tempfile_seek   PROC
    mov WORD PTR tmp_ems_pos,dx
    mov WORD PTR tmp_ems_pos+2,cx   ; carry to high word
    ret
ems_tempfile_seek   ENDP

;*****************************
;* EMS_TEMPFILE_READ         *
;*****************************

; read bytes from temporary file in EMS/XMS at tmp_ems_pos
; ds:dx -> read buffer, cx holds bytes to read
; do NOT assume ds -> DGROUP upon entry
; destroys ax,bx,cx

ems_tempfile_read   PROC
    push    dx              ; save critical register
    push    si
    push    di
    push    es

    mov bx,ds               ; save -> write buffer
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data

    cmp tmp_in_xms,0        ; see if temporary file in XMS
    je  etr_ems             ; no, in EMS
    call    xms_tmp_read    ; read in XMS
    jmp SHORT etr_ret

etr_ems:
    push    WORD PTR ems_currmap    ; save current logical state of physical page 0
    push    bx              ; save -> write buffer segment on stack

    mov di,dx               ; di -> read buffer
    mov dx,WORD PTR tmp_ems_pos+2
    mov ax,WORD PTR tmp_ems_pos
    mov si,ax
    and si,16383            ; si offsets into EMS page
    call    convert_to_page ; ax holds 16K page of position

    add WORD PTR tmp_ems_pos,cx     ; adjust for bytes read
    adc WORD PTR tmp_ems_pos+2,0    ; carry to high word
    add ax,ems_first_tmp    ; adjust for first page used

    mov bx,ax               ; bx holds EMS logical page
    xor al,al
    call    map_ems_page    ; map in proper page
    pop es                  ; es:di -> read buffer

    mov ds,ems_base         ; ds:si -> source buffer in EMS

eor_readloop:
    mov ax,si               ; check if read from more than one page
    add ax,cx
    jc  eor_offpage         ; carry implies read off page
    cmp ax,16384            ; check page limit
    ja  eor_offpage         ; off of page
    shr cx,1                ; convert byte count to read to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
    jmp SHORT eor_2

eor_offpage:
    mov ax,16384
    sub ax,si               ; ax holds bytes to read on page
    xchg    ax,cx           ; cx holds bytes to read on page, ax holds total bytes
    sub ax,cx               ; ax holds bytes to read beside current page
    shr cx,1                ; convert byte count to read to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
    mov cx,ax               ; update total bytes to read
    xor si,si               ; position to start of next page
    inc bx
    xor al,al
    call    map_ems_page    ; map in next page
    jmp SHORT eor_readloop  ; loop to read more bytes

eor_2:
    mov ax,es               ; get read buffer segment
    mov ds,ax               ; restore ds to entry value

; restore previous logical state of physical page 0
    xor al,al
    pop bx                  ; get previous logical state from stack
    call    map_ems_page

etr_ret:
    pop es                  ; restore critical register
    pop di
    pop si
    pop dx
    ret
ems_tempfile_read   ENDP

;*****************************
;* XMS_TMP_READ              *
;*****************************

; read bytes from temporary file in XMS at tmp_ems_pos
; bx:dx -> read buffer, cx holds bytes to read
; safe to assume ds -> DGROUP upon entry
; destroys ax,bx,cx,dx,si

xms_tmp_read    PROC

xtr_loop:
    mov ax,cx
    and al,0feh             ; round down to word boundary for XMM acceptable length
    mov WORD PTR tmp_empb.es_len,ax ; length of block to transfer low word
    mov ax,xms_tmp_handle
    mov tmp_empb.es_src_handle,ax   ; source is XMS memory
    mov ax,WORD PTR tmp_ems_pos     ; compute source offset
    mov WORD PTR tmp_empb.es_src_offset,ax
    mov ax,WORD PTR tmp_ems_pos+2
    mov WORD PTR tmp_empb.es_src_offset+2,ax
    xor ax,ax
    mov WORD PTR tmp_empb.es_len+2,ax   ; zero high word of block length
    mov tmp_empb.es_dest_handle,ax  ; destination is conventional memory
    mov WORD PTR tmp_empb.es_dest_offset,dx
    mov WORD PTR tmp_empb.es_dest_offset+2,bx

    mov ah,0bh              ; move extend memory block
    mov si,OFFSET DGROUP:tmp_empb   ; ds:si -> parameter block
    call    safe_xms_addr
    call    check_xms_error ; see if error occurred

    add WORD PTR tmp_ems_pos,cx ; adjust for bytes read
    adc WORD PTR tmp_ems_pos+2,0    ; carry to high word
    add dx,cx               ; update destination offset
    and cx,1                ; see if extra byte to transfer
    je  xtr_ret             ; no
    sub WORD PTR tmp_ems_pos,2  ; back up one byte not written, back up one byte for rewrite
    sbb WORD PTR tmp_ems_pos+2,0    ; carry to high word
    sub dx,2                ; adjust back one byte, back one for byte not written
    inc cx                  ; two bytes to write
    jmp SHORT xtr_loop      ; write the bytes

xtr_ret:
    mov ds,bx               ; restore ds -> write buffer
    ret
xms_tmp_read    ENDP

;*****************************
;* EMS_TEMPFILE_WRITE        *
;*****************************

; write bytes to temporary file in EMS/XMS at tmp_ems_pos
; ds:dx -> write buffer, cx holds bytes to write
; do NOT assume ds -> DGROUP upon entry
; destroys ax,bx,cx,dx

ems_tempfile_write  PROC
    push    si              ; save critical register
    push    di
    push    es

    mov bx,ds               ; save -> write buffer
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data

    cmp tmp_in_xms,0        ; see if temporary file in XMS
    je  etw_ems             ; no, in EMS
    call    xms_tmp_write   ; write in XMS
    jmp SHORT etw_ret

etw_ems:
    push    WORD PTR ems_currmap    ; save current logical state of physical page 0
    push    bx              ; save -> write buffer on stack

    mov si,dx               ; si -> write buffer
    mov dx,WORD PTR tmp_ems_pos+2
    mov ax,WORD PTR tmp_ems_pos
    mov di,ax
    and di,16383            ; di offsets into EMS page
    call    convert_to_page ; ax holds 16K page of position

    add WORD PTR tmp_ems_pos,cx    ; adjust for bytes written
    adc WORD PTR tmp_ems_pos+2,0   ; carry to high word
    add ax,ems_first_tmp    ; adjust for first page used

    mov bx,ax               ; bx holds EMS logical page
    xor al,al
    call    map_ems_page    ; map in proper page

    mov es,ems_base         ; es:di -> destination EMS buffer
    pop ds                  ; ds:si -> write buffer

eow_writeloop:
    mov ax,di               ; check if write to more than one page
    add ax,cx
    jc  eow_offpage         ; carry implies write off page
    cmp ax,16384            ; check page limit
    ja  eow_offpage         ; off of page
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
    jmp SHORT eow_2

eow_offpage:
    mov ax,16384
    sub ax,di               ; ax holds bytes to write on page
    xchg    ax,cx           ; cx holds bytes to write on page, ax holds total bytes
    sub ax,cx               ; ax holds bytes to write beside current page
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
    mov cx,ax               ; update total bytes to write
    xor di,di               ; position to start of next page
    push    ds              ; save ds -> write buffer
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data for map_ems_page
    inc bx
    xor al,al
    call    map_ems_page    ; map in next page
    pop ds                  ; restore ds -> write buffer
    jmp SHORT eow_writeloop ; loop to read more bytes

eow_2:
    pop bx                  ; bx holds previous logical page of EMS page 0
    push    ds              ; save ds -> write buffer
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data

; restore previous logical state of EMS physical page 0
    xor al,al
    call    map_ems_page
    pop ds                  ; restore ds -> write buffer

etw_ret:
    pop es                  ; restore critical register
    pop di
    pop si
    ret
ems_tempfile_write  ENDP

;*****************************
;* XMS_TMP_WRITE             *
;*****************************

; write bytes to temporary file in XMS at tmp_ems_pos
; bx:dx -> write buffer, cx holds bytes to write
; ds -> DGROUP upon entry
; destroys ax,bx,cx,dx,si

xms_tmp_write   PROC
    cmp cx,1                ; see if bytes to write is one byte
    jne xtw_loop            ; no
    inc cx                  ; round to two byte minimum

xtw_loop:
    mov ax,cx
    and al,0feh             ; round down to word boundary for XMM acceptable length
    mov WORD PTR tmp_empb.es_len,ax ; length of block to transfer low word
    xor ax,ax
    mov WORD PTR tmp_empb.es_len+2,ax
    mov tmp_empb.es_src_handle,ax   ; source is conventional memory
    mov WORD PTR tmp_empb.es_src_offset,dx
    mov WORD PTR tmp_empb.es_src_offset+2,bx
    mov ax,xms_tmp_handle
    mov tmp_empb.es_dest_handle,ax  ; destination is XMS memory
    mov ax,WORD PTR tmp_ems_pos     ; compute destination offset
    mov WORD PTR tmp_empb.es_dest_offset,ax
    mov ax,WORD PTR tmp_ems_pos+2
    mov WORD PTR tmp_empb.es_dest_offset+2,ax

    mov ah,0bh              ; move extended memory block
    mov si,OFFSET DGROUP:tmp_empb   ; ds:si -> parameter block
    call    safe_xms_addr
    call    check_xms_error ; see if error occurred

    add WORD PTR tmp_ems_pos,cx ; adjust for bytes read
    adc WORD PTR tmp_ems_pos+2,0    ; carry to high word
    add dx,cx               ; update destination offset
    and cx,1                ; see if extra byte to transfer
    je  xtw_ret             ; no
    sub WORD PTR tmp_ems_pos,2  ; back up one byte not written, back up one byte for rewrite
    sbb WORD PTR tmp_ems_pos+2,0    ; carry to high word
    sub dx,2                ; adjust back one byte, back one for byte not written
    inc cx                  ; two bytes to write
    jmp SHORT xtw_loop      ; write the bytes

xtw_ret:
    mov ds,bx               ; restore ds -> write buffer
    ret
xms_tmp_write   ENDP

END
