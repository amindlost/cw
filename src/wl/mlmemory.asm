;*********************************************************************
;*   MLMEMORY.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          08/09/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   memory, file i/o buffer manipulation routines for linker        *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlmemory
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
     
PUBLIC  get_memory,alloc_memory,free_memory,load_buffer,shrink_buffer
PUBLIC  give_load_size
PUBLIC  map_ems_page,restore_ems_map,alloc_ems_trans,read_to_ems
PUBLIC  ems_trans_block
PUBLIC	mod_alloc_base

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   filename:BYTE,temp_file_name:BYTE
EXTRN   temp_file_pages:BYTE,temp_page_ptr:WORD

; initialized local variables

; byte values
EVEN
rollout_flag    DB  0       ; nonzero if memory image rolled out to disk during pass 2

.DATA?

; uninitialized local variables

; word values
EVEN
free_start      DW  ?       ; start of memory to free in free_memory routine if pass 2 roll-out occurred
ems_trans_block DW  ?       ; segment of EMS transfer block
mod_alloc_base  DW  ?       ; base of memory allocation made for module

;*****************************
;* Constant data             *
;*****************************

.CONST

tbuff   DB  '000K ',CR,LF   ; buffer for exe load image size feedback

image_tlen  DB  image_tstop-image_text
image_text  DB  CR,LF,'EXE load image size: '
image_tstop =   $

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,make_temp_fbk:NEAR

IFNDEF JUNIOR
EXTRN   ems_tempfile_create:NEAR,ems_tempfile_write:NEAR
ENDIF

;*****************************
;* GET_MEMORY                *
;*****************************

; allocate memory for linker for use with control blocks and file buffers
; destroys ax,bx,cx,dx

get_memory      PROC

gm_1:
    mov ah,48h              ; allocate memory
    mov bx,0ffffh           ; force request to fail, function will return largest available block
    int 21h
    cmp ax,8                ; insufficient memory error is expected
    je gm_2
    jmp NEAR PTR dos_error  ; other errors are fatal

; real memory allocation attempt with bx preset to largest available block size
gm_2:
    mov ah,48h              ; allocate memory
    int 21h
    jnc gm_3                ; no errors
    jmp NEAR PTR dos_error  ; any error is fatal

gm_3:
    mov memory_blk_size,bx  ; save size of memory block allocated in paras
    mov memory_blk_base,ax  ; save segment of allocated block
    mov allocation_base,ax  ; init block allocation block to initial allocation segment

IFNDEF JUNIOR
    xor ax,ax
    cmp is_no_ems,al        ; see if EMS is useable
    jne gm_chkblk           ; no
    cmp ems_handle,ax       ; see if EMS already allocated
    jne gm_chkblk           ; yes

; allocate EMS if possible
;*** COMMENT # temporary for 1.60f
    mov ah,42h              ; get number of pages
    int 67h
    or  ah,ah
    jne gm_no_ems           ; error occurred, don't use EMS
    cmp bx,4                ; see if at least 4 unused EMS pages
    jb  gm_no_ems           ; no

    cmp bx,9                ; see if at least nine pages
    jae gm_em2              ; yes, allocate them all
    mov bx,4                ; can only use 4 pages if 8 or less

gm_em2:
    mov ems_pagecount,bx    ; save unallocated pages
    mov ax,bx
    sub ax,4                ; 4 pages allocated for i/o buffer
    mov ems_page_avail,ax   ; save rest available for use by WarpLink for other purposes
    mov ah,43h              ; allocate handle and pages
    int 67h
    mov ems_handle,dx       ; save handle, if successful
    or  ah,ah
    jne gm_no_ems           ; error occurred

; map in pages to use logical page 0->0, 1->1, 2->2, 3->3
    xor bx,bx
    mov al,bl
    call    map_ems_page    ; map page 0
    inc bx
    mov al,bl
    call    map_ems_page    ; map page 1
    inc bx
    mov al,bl
    call    map_ems_page    ; map page 2
    inc bx
    mov al,bl
    call    map_ems_page    ; map page 3
    jmp SHORT gm_chkblk

; failed to get at least 4 EMS 16K pages successfully
gm_no_ems:
    mov is_no_ems,1         ; set no EMS page

;*** END COMMENT #

ENDIF

gm_chkblk:
    mov ax,memory_blk_base
    mov bx,memory_blk_size

    cmp is_no_ems,0         ; see if EMS i/o buffer allocated
    jne gm_chk16            ; no, must have 16K block
    cmp bx,1280             ; block size must be >=20K with EMS buffer (16K for transfer buffer)
    jae gm_3a               ; large enough
    jmp SHORT gm_tryroll    ; not large enough

gm_chk16:
    cmp bx,1024             ; block size must be >=16K (1024 paragraphs) if no EMS i/o buffer
    jae gm_3a               ; large enough

; not enough memory free, see if can roll memory image out to disk
gm_tryroll:
    cmp is_ondisk,0         ; see if memory image on disk (no roll-out possibilities)
    jne gm_oom              ; already on disk
    cmp is_inmem,0          ; see if memory image is in memory (if memory image exists)
    je  gm_oom              ; memory image not built yet

    call    free_memory     ; free too-small memory block back to DOS
    call    mem_roll_out    ; roll memory image to disk
    mov ax,image_mem_ptr    ; get segment of memory image
    mov memory_blk_base,ax  ; save as segment of memory block allocated
    call    free_memory     ; free up unused memory (memory image size less file block allocations)
    jmp NEAR PTR gm_1       ; retry DOS memory allocation

gm_oom:
    mov ax,8                ; force DOS out of memory error
    jmp NEAR PTR dos_error

; ax holds base of allocated memory block (memory_blk_base)
; bx holds size of memory block in paras (memory_blk_size)
gm_3a:
    cmp is_no_ems,0         ; see if EMS i/o buffer allocated
    jne gm_membuff          ; no

; set up for EMS i/o buffer
    add ax,memory_blk_size  ; compute end of memory block
    mov memory_blk_end,ax   ; and save it
    mov allocation_top,ax   ; save end of memory block as allocation ceiling
    mov ax,ems_base
    mov buffer_base,ax      ; buffer i/o base is EMS page frame
    mov ax,0fffh
    mov buffer_size,ax      ; EMS i/o buffer size is 64K-16 bytes, 4095 paragraphs
    jmp SHORT gm_buffend    ; bypass memory i/o buffer code

gm_membuff:
    cmp any_ddl,0           ; see if using DDL's
    je  gm_noddl            ; no
    cmp bx,0fffh            ; using DDL's, allocate 64K-16 for buffer
    jb  gm_oom              ; no enough memory
    jmp SHORT gm_0fffh      ; allocate memory for 64K-16 buffer

gm_noddl:
    cmp bx,2000h            ; if block size >=128K only use 64K-16 for file buffer
    jb  gm_4                ; available memory less than 128K

gm_0fffh:
    sub bx,0fffh            ; back off 64K-16 for file buffer, offset in memory block
    jmp SHORT gm_5          ; bypass buffer setup for <128K block code

gm_4:
    cmp bx,1536             ; see if block size smaller than 24K
    jae gm_4a               ; no, larger or equal to 24K

; allocate 12K for file buffer, remainder for available memory
    sub bx,768              ; back off 12K for file buffer
    jmp SHORT gm_5

; split file buffer and available memory 50-50
gm_4a:
    mov cx,bx               ; block size into cx
    shr cx,1                ; divide block size by 2
    adc cx,0                ; odd values add back
    sub bx,cx               ; back off memory for file buffer

gm_5:
    add bx,ax               ; compute base of buffer by adding memory block base to offset
    mov buffer_base,bx      ; and save it
    mov allocation_top,bx   ; save base of buffer as allocation ceiling
    add ax,memory_blk_size  ; compute end of memory block
    mov memory_blk_end,ax   ; and save it
    sub ax,bx               ; compute size of file buffer
    mov buffer_size,ax      ; and save it

; convert paragraphs in ax to bytes for buffer end, buffer tail, offset
gm_buffend:
    shl ax,1                ; x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    shl ax,1                ; x16, converted to bytes
    mov buffer_end,ax       ; offset from memory block base to end
    mov buffer_tail,ax      ; init buffer tail to end
    ret
get_memory      ENDP

;*****************************
;* ALLOC_MEMORY              *
;*****************************

; allocate memory for control block
; bx holds size of block in paragraphs upon entry
; destroys ax,bx
; returns ax -> allocated control block

alloc_memory    PROC

am_1:
    mov ax,allocation_base  ; get previous base to allocate from
    push    ax              ; save it
    add ax,bx               ; compute new base
    mov allocation_base,ax  ; save it
    cmp ax,allocation_top   ; must be less than allocation ceiling
    jbe am_ret              ; control block allocation fits into memory
    cmp is_ondisk,0         ; see if memory image on disk (no roll-out possibilities)
    jne am_2                ; already on disk
    cmp is_inmem,0          ; see if memory image is in memory (if memory image exists)
    je  am_2                ; memory image not built yet
    pop ax                  ; get previous base value
    mov free_start,ax       ; save as start of memory to free (new allocation base/top below old)
    mov rollout_flag,1      ; flag that memory was rolled out to disk during pass 2 (for free_memory proc)
    call    mem_roll_out    ; roll memory image to disk
    jmp SHORT am_1          ; retry memory allocation with new base and top from old memory image

am_2:
    call    shrink_buffer   ; doesn't fit, try shrinking file buffer

am_ret:
    pop ax                  ; restore previous base as allocate control block pointer
    ret
alloc_memory    ENDP

;*****************************
;* MEM_ROLL_OUT              *
;*****************************

; roll memory image out to disk
; destroys ax

mem_roll_out    PROC
    push    bx              ; save critical register
    push    cx
    push    dx
    push    si
    push    di
	push	es

    mov is_ondisk,1         ; flag that executable image is now on disk
    mov is_inmem,0          ; reset executable in memory flag

    call    ems_tempfile_create ; create temporary file in EMS/XMS, if possible
    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    jne mro_out             ; yes, bypass disk file creation

    call    make_temp_fbk   ; make temporary file and give feedback

; write image_size bytes starting at segment image_mem_ptr to disk or EMS
mro_out:
    mov si,WORD PTR image_size  ; get low word
    mov di,WORD PTR image_size+2    ; get high word
    mov bx,image_handle

; transfer program to disk in 64K-16 chunks
; di:si contain number of bytes to write, bx == file handle
    push    image_mem_ptr   ; save original memory pointer value

wp_mem_write:
    xor dx,dx               ; zero offset of write buffer
    or  di,di               ; see if byte count to write is 64K or more
    jne mro_2               ; yes, write a 64K-16 chunk
    cmp si,0fff0h           ; see if byte count is at least 64-16
    jb mro_4                ; no, exit 64K-16 writing loop

mro_2:
    mov cx,0fff0h           ; number of bytes to write (64K-16)
    mov ax,image_mem_ptr
    push    ds              ; save ds, critical register

    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    je  mro_noems           ; no
    mov ds,ax               ; ds -> segment to start write
    call    ems_tempfile_write
    pop ds                  ; restore ds -> warplink data
    jmp SHORT mro_3

mro_noems:
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    pop ds                  ; restore ds -> warplink data
    jnc mro_3               ; no errors
    mov dx,OFFSET DGROUP:temp_file_name
    jmp NEAR PTR dos_error  ; error writing to file

mro_3:
    add image_mem_ptr,0fffh ; adjust segment pointer past 64K-16 written (4K-1 paragraphs)
    sub si,0fff0h           ; back off number of bytes written from bytes to write
    sbb di,0                ; borrow to high word
    jmp SHORT wp_mem_write  ; loop for next write

; transfer leftover bytes (file size modulo 64K-16)
mro_4:
    mov cx,si               ; cx holds bytes to write
    jcxz    mro_rollout_done    ; no bytes to write, done
    mov ax,image_mem_ptr
    push    ds              ; save ds, critical register

    cmp tmp_in_emsxms,0     ; see if temporary file in EMS/XMS
    je  mro_noems2          ; no
    mov ds,ax               ; ds -> segment to start write
    call    ems_tempfile_write
    pop ds                  ; restore ds -> warplink data
    jmp SHORT mro_rollout_done

mro_noems2:
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF
    pop ds                  ; restore ds -> warplink data
    jnc mro_rollout_done    ; no errors, all done
    mov dx,OFFSET DGROUP:temp_file_name
    jmp NEAR PTR dos_error  ; error writing to file

mro_rollout_done:
    pop image_mem_ptr       ; restore original memory pointer value

    mov ax,WORD PTR image_size  ; get low word of image size
    add ax,15               ; round up for paragraph computation
    mov dx,WORD PTR image_size+2    ; get high word of image size
    adc dx,0                ; carry to high word

; convert image size in dx:ax to paragraphs
    shr dx,1
    rcr ax,1                ; /2
    shr dx,1
    rcr ax,1                ; /4
    shr dx,1
    rcr ax,1                ; /8
    shr dx,1                ; dx should be zero by the final shift
    rcr ax,1                ; /16
    mov bx,ax               ; bx holds paragraphs freed by roll-out

    mov ax,image_mem_ptr
    mov allocation_base,ax  ; old image pointer is new base, temporary before adding file blocks
    add ax,bx               ; compute new allocation ceiling
    mov allocation_top,ax   ; save to global memory variable

    cmp bx,500h             ; must be at least 20K free memory
    jae mro_5               ; okay

mro_mem_err:
    mov ax,8                ; force insufficient memory error
    jmp NEAR PTR dos_error

mro_5:
    sub bx,400h             ; subtract off 16K for use by other procedures

; bh holds count of 4K blocks available
    cmp bh,3                ; if 3 or less blocks available use all of them
    jbe mro_6
    mov al,bh               ; get count of available 4K blocks
    mov bh,3                ; default blocks to 3
    shr al,1                ; /2
    shr al,1                ; /4, only allocate up to 25% of blocks available
    cmp al,3                ; allocate larger of 25% of blocks available or 3
    jbe mro_6               ; 3 blocks or less
    mov bh,al               ; more than 3 blocks
    cmp bh,15               ; don't allocate more than 15 4K blocks (60K, 0f00h paragraphs)
    jbe mro_6               ; 15 or fewer 4K blocks available
    mov bh,15               ; set at maximum

mro_6:
    mov temp_file_pages,bh  ; save count of 4K blocks
    mov ax,allocation_base  ; get allocation base (old memory image pointer)
    mov temp_page_ptr,ax    ; save pointer to block
    xor bl,bl               ; zero low byte of paragraphs used
    add allocation_base,bx  ; update base past allocated 4K blocks
    mov ax,allocation_base  ; get new allocation base
    cmp ax,allocation_top   ; make sure not past top
    ja  mro_mem_err         ; out of memory
	mov	mod_alloc_base,ax	; save new base of allocations prior to any allocations

	pop	es					; restore critical register
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    ret
mem_roll_out    ENDP

;*****************************
;* LOAD_BUFFER               *
;*****************************

; load from file into i/o buffer
; bx holds file handle upon entry
; destroys ax,cx,dx

load_buffer     PROC
    mov eof_flag,0          ; init end of file flag
    push    ds              ; save critical register
    mov buffer_head,0       ; set buffer head equal to offset zero from buffer base
    mov ax,buffer_end       ; get physical end of buffer
    mov buffer_tail,ax      ; set tail to physical end of buffer
    mov cx,ax               ; get number of bytes to read
    xor dx,dx
    mov ds,buffer_base      ; DS:DX -> segment:offset of buffer area

    call    read_to_ems     ; read file, transfer to EMS if necessary

lb_exit:
    pop ds                  ; restore critical register
    cmp cx,ax               ; see if all bytes were read
    je  lb_ret              ; yes

    mov eof_flag,1          ; set end of file flag

;***    push    ds              ; save critical registers
;***    push    bx
;***    mov bx,ax
;***    mov ds,buffer_base      ; DS:BX -> first byte past last read
;***    mov BYTE PTR [bx],0     ; zero it out, so multi-module check will fail there
;***    pop bx                  ; restore critical registers
;***    pop ds

lb_ret:
    ret
load_buffer     ENDP

;*****************************
;* FREE_MEMORY               *
;*****************************

; free memory allocated for linker unused by control blocks
; destroys ax,bx,cx,es

free_memory     PROC
    cmp rollout_flag,0      ; see if memory rolled out to disk during pass 2
    je  fm_noroll           ; no
    mov bx,free_start       ; get start of memory to free (old base prior to rollout)
    jmp SHORT fm_1

fm_noroll:
    mov bx,allocation_base  ; get first free (nonallocated to control block) space in memory block

fm_1:
    mov ax,memory_blk_base  ; segment of memory block allocated
    sub bx,ax               ; compute difference == amount of memory used by control blocks
    mov es,ax               ; es holds segment of allocated memory block
    je  fm_2                ; if memory use is zero then free all of memory
    mov ah,4ah              ; modify memory allocation function

fm_intcall:
    int 21h
    jnc fm_ret              ; no errors
    jmp NEAR PTR dos_error  ; error occurred

fm_2:
    mov ah,49h              ; release memory function
    jmp SHORT fm_intcall    ; perform the function

fm_ret:
    ret
free_memory     ENDP

;*****************************
;* GIVE_LOAD_SIZE            *
;*****************************

; feedback EXE load image size if EXE file
; destroys ax,bx,cx,dx,si

give_load_size  PROC
    cmp is_comfile,0        ; see if COM file
    jne gls_ret             ; yes, no feedback

    mov bx,OFFSET DGROUP:image_text
    mov cl,[bx-1]           ; get length of string
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h

    mov ax,WORD PTR image_size  ; get low word of image size
    add ax,1023             ; round up to next 1K boundary
    mov al,0                ; zero low byte
    adc al,BYTE PTR image_size+2    ; get high word low byte with overflow (ignore highest byte, unused)
    xchg    al,ah           ; al holds low word high byte, ah holds high word low byte
    shr ax,1                ; /512
    shr ax,1                ; /1024, ax holds 1K pages of image size, rounded up

    mov bx,OFFSET DGROUP:tbuff+2    ; bx -> end of buffer to hold ax value
    mov si,10               ; divisor
    mov cx,3                ; 3 digits total

gls_divloop:
    xor dx,dx               ; extend dividend in ax to unsigned 32-bit dx:ax
    div si
    add dl,'0'              ; convert digit to ASCII
    mov [bx],dl             ; save it
    dec bx                  ; point to next most significant digit
    loop    gls_divloop     ; loop until complete

gls_2:
    mov dx,bx               ; ds:dx -> string to print
    mov cx,7                ; # bytes to print
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h

gls_ret:
    ret
give_load_size  ENDP

IFNDEF JUNIOR

;*****************************
;* MAP_EMS_PAGE              *
;*****************************

; map expanded memory page into memory
; upon entry al holds physical page, bx holds logical page
; destroys ax,dx

map_ems_page    PROC
    push    bx              ; save critical register
    push    ds
    push    ax              ; save physical page
    mov dx,DGROUP
    mov ds,dx               ; make sure ds -> warplink data
    mov dx,ems_handle
    mov ah,44h              ; map expanded memory page
    int 67h
    or  ah,ah               ; see if error occurred
    jne mep_error           ; yes
    mov ax,bx               ; get logical page
    pop bx
    xor bh,bh               ; bx holds physical page
    shl bx,1                ; bx is word offset
    add bx,OFFSET DGROUP:ems_currmap    ; bx -> physical page entry just mapped
    mov [bx],ax             ; save logical page at physical page
    pop ds                  ; restore critical registers
    pop bx
    ret

; EMS error occurred
mep_error:
    mov cl,ah               ; get 8 bit error code in cl
    mov ax,EMS_EMM_ERR
    jmp NEAR PTR link_error ; transfer control to error handler
map_ems_page    ENDP

;*****************************
;* RESTORE_EMS_MAP           *
;*****************************

; restore EMS mappings to stored values, in case of RAM disk or cache
; messing them up during a DOS function call
; destroys no registers or flags

restore_ems_map PROC
    pushf
    push    ax
    push    ds
    mov ax,DGROUP
    mov ds,ax               ; ensure ds -> warplink data
    cmp is_no_ems,0         ; see if EMS was used
    jne rem_ret             ; no

    push    dx
    push    cx
    push    bx
    mov dx,ems_handle
    mov cx,4

rem_maploop:
    mov bx,cx
    dec bx                  ; bx holds physical page
    mov al,bl               ; ax holds physical page
    shl bx,1                ; make word offset into mapped pages array
    add bx,OFFSET DGROUP:ems_currmap
    mov bx,[bx]             ; get logical page at physical page
    cmp bx,-1               ; see if illegal page (not set yet)
    je  rem_done            ; yes, don't map in pages
    mov ah,44h              ; map expanded memory page
    int 67h
    or  ah,ah               ; see if error occurred
    jne mep_error           ; yes
    loop    rem_maploop     ; map in all four pages

rem_done:
    pop bx
    pop cx
    pop dx

rem_ret:
    pop ds
    pop ax
    popf
    ret
restore_ems_map ENDP

;*****************************
;* READ_TO_EMS               *
;*****************************

; read from file to EMS transfer buffer, transfer to EMS, if EMS used
; upon entry ds:dx-> EMS area to transfer to, bx == file handle,
; cx holds byte count
; destroys NO registers other than typical function 3fh return values

read_to_ems PROC
    push    ds
    mov ax,DGROUP
    mov ds,ax    
    cmp is_no_ems,0         ; see if EMS used
    pop ds
    je  rte_ems             ; yes
    mov ah,3fh
    int 21h
    jc  rte_doserr          ; error reading file
    ret

rte_ems:
    push    si              ; save critical registers
    push    di
    push    es
    push    cx
    push    dx
    push    bp
    push    ds

    mov di,dx
    pop es                  ; es:di -> destination (EMS) block
    push    es              ; restore stack
    xor bp,bp               ; bp holds total bytes read

rte_readloop:
    push    cx              ; save bytes to read
    cmp cx,16384            ; write only 16K chunk max
    jbe rte_2
    mov cx,16384

rte_2:
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    mov ds,ems_trans_block  ; ds -> EMS transfer area
    xor dx,dx
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map
    jc  rte_doserr          ; error reading file

    add bp,ax               ; update total bytes read
    mov cx,ax               ; get bytes read in cx
    xor si,si               ; ds:si -> source (transfer) block

; transfer from transfer block to EMS block
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

    pop cx                  ; get bytes to read
    sub cx,16384            ; subtract maximum read
    jbe rte_ret             ; all read
    cmp ax,16384            ; see if actual bytes read matched requested
    je  rte_readloop        ; yes, not at end of file

rte_ret:
    mov ax,bp               ; get total bytes read in ax
    pop ds
    pop bp
    pop dx
    pop cx
    pop es
    pop di
    pop si
	clc						; clear carry for carry on file read is error checks
    ret

rte_doserr:
    mov dx,OFFSET DGROUP:filename
    jmp NEAR PTR dos_error  ; error writing to file

read_to_ems ENDP

;*****************************
;* ALLOC_EMS_TRANS           *
;*****************************

; allocate buffer for EMS transfer in 16K chunks for better compatibility
; or for XMS transfer
; destroys ax,bx

alloc_ems_trans PROC
    cmp is_xms,0            ; see if XMS  used
    jne aet_alloc           ; yes
    cmp is_no_ems,0         ; see if EMS used
    jne aet_ret             ; no

aet_alloc:
    mov bx,400h             ; 1K paragraphs
    call    alloc_memory
    mov ems_trans_block,ax  ; save segment of EMS transfer block

aet_ret:
    ret
alloc_ems_trans ENDP

ENDIF

;*****************************
;* SHRINK_BUFFER             *
;*****************************

; dynamically shrink memory allocated to file buffer

;*** this is a royal pain and may not be implemented for a while

shrink_buffer   PROC

;*** temporary
    mov ax,8                ; force DOS out of memory error
    jmp NEAR PTR dos_error

;*** code goes here

    ret
shrink_buffer   ENDP

END
