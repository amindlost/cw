;*********************************************************************
;*   MLIMAGE1.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/30/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   memory image routines, part 1                                   *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlimage1
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
PUBLIC  setup_exe_image,write_program,write_bytes,caseless_strcmp
PUBLIC	compute_seg_frames

; variables
PUBLIC  exe_handle,segment_start,segment_stop,true_seg_len

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

EXTRN   data_segment:WORD,data_offset:DWORD,filename:BYTE
EXTRN   is_emsxms_ovl:BYTE
EXTRN	filesize_text:BYTE
EXTRN	first_clipmod_ptr:WORD,is_summer87:BYTE,is_clipper5:BYTE

; initialized local variables

; byte values
EVEN                        ; maximize speed on 8086 and better
stack_found DB  0           ; nonzero if stack segment encountered
EVEN
phase   DB  0               ; DOSSEG segment ordering phase
EVEN
com_end_flag    DB  0       ;  1 if $$_COM_END variable allocated, 2 if not used
EVEN

res_incflag DB  0           ; 1 if Clipper incremental link and current class 'CODE'
                            ; 2 if class 'SYMBOLS'
EVEN

; doubleword values
segment_start   DD  0       ; start of segments

; structures
EXE_HEADER_STRUC    STRUC
    eh_sig1 DB  ?           ; EXE file signature bytes
    eh_sig2 DB  ?
    eh_flen DW  ?           ; length of file module 512 */
    eh_fsize    DW  ?       ; size of file in 512-byte pages, including header
    eh_numrel   DW  ?       ; number of relocation items
    eh_hsize    DW  ?       ; size of header in paragraphs
    eh_minalloc DW  ?       ; minimum number of paragraphs needed above program
    eh_maxalloc DW  ?       ; maximum number of paragraphs needed above program
    eh_ss   DW  ?           ; SS at entry
    eh_sp   DW  ?           ; SP at entry
    eh_chksum   DW  ?       ; word checksum (unused
    eh_ip   DW  ?           ; contents of IP register at entry
    eh_cs   DW  ?           ; CS at entry
    eh_roff DW  ?           ; offset of first relocation item in file
    eh_ovnum    DB  ?       ; overlay number
    eh_filler   DB  3 DUP (?)   ; not used
EXE_HEADER_STRUC    ENDS

exe_header  EXE_HEADER_STRUC    <4dh,5ah,,,,,,0ffffh,,,0,,,30,0>

.DATA?

; uninitialized local variables

; byte values
EVEN
res_occurred    DB  ?       ; nonzero flags a segment resolved in resolution pass
EVEN
using_class DB  ?           ; nonzero if resolving segments for a class, zero if no class selected yet

EVEN

; word values
unresolved_segs DW  ?       ; count of segments still have their addresses resolved
segdef_block_ptr    DW  ?   ; current pointer to segdef block
entry_number    DW  ?       ; current entry in segdef block
exe_handle  DW  ?           ; handle of executable file
com_end_pubptr  DW  ?       ; pointer to $$_COM_END public declaration entry
temp_buffer_size    DW  ?   ; size of temporary buffer
temp_buffer_base    DW  ?   ; segment pointer to start of temporary buffer
class_group DW  ?           ; group entry segment of class-setting segment, only used by DOSSEG

; doubleword values
true_seg_len    DD  ?       ; true segment length (can be 10000h from Big bit)
segment_stop    DD  ?       ; segment stop address

; byte strings
symname_buffer	LABEL	BYTE
class_name_field    DB  128 DUP (?) ; class name of segdef entry
group_name_field    DB  128 DUP (?) ; group name of segdef entry

;*****************************
;* Constant data             *
;*****************************

.CONST

; constant strings in DOSSEG segment class type computation
EVEN
codetext    DB  'CODE',0
EVEN
begdatatext DB  'BEGDATA',0
EVEN
bsstext     DB  'BSS',0
EVEN
stacktext   DB  'STACK',0
EVEN
com_end_text    DB  '$$_COM_END',0

maxalloc_len    DW  maxalloc_stop-maxalloc_warn
maxalloc_warn   DB  CR,LF,'Maximum program allocation space less than minimum required, maximum adjusted.'
maxalloc_stop   =   $

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,alloc_memory:NEAR
EXTRN   get_memory:NEAR,free_memory:NEAR,link_warning:NEAR
EXTRN   zero_mem_image:NEAR,update_temp_file:NEAR,create_temp_file:NEAR
EXTRN   temp_file_write:NEAR,dump_temp_file:NEAR
EXTRN   map_segments:NEAR,map_groups:NEAR,error_read_buff_pos:NEAR
EXTRN   get_pubdecl_entry:NEAR,add_pubdef_name:NEAR,find_pubdecl_entry:NEAR
EXTRN   resolve_ovl_info:NEAR,write_vectors:NEAR,flush_ovl_page:NEAR
EXTRN   ovl_file_write:NEAR,flush_reloc_array:NEAR
EXTRN   res_ilfseg:NEAR,ilf_rewind:NEAR,write_clar_data:NEAR
EXTRN   make_master_segblk:NEAR
EXTRN   map_ems_page:NEAR,ems_ovl_to_file:NEAR,restore_ems_map:NEAR
EXTRN	get_symbol_offset:NEAR,read_to_ems:NEAR

;*****************************
;* SETUP_EXE_IMAGE           *
;*****************************

; compute segment frame values and allocate disk or memory space for the
; executable image of the program
; destroys all registers except ds

setup_exe_image PROC
    xor ax,ax
    mov ovl_data_id,ax      ; zero overlaid data flag

    call    get_memory      ; allocate memory for warplink use

    cmp is_comfile,0        ; see if is a com file
    jne sei_iscom           ; yes

sei_no_com_end:
    mov com_end_flag,2      ; flag that $$_COM_END variable is not used
    jmp SHORT sei_1         ; bypass $$_COM_END variable check

; com file, check if $$_COM_END was previously declared
sei_iscom:
    mov di,OFFSET DGROUP:com_end_text   ; ds:di -> declaration name
    mov al,1                ; attempt public declaration
    call    get_pubdecl_entry   ; make or find previous public declaration entry
    or  ax,ax               ; see if segment is null
    je  sei_no_com_end      ; yes, previous $$_COM_END definition
    mov com_end_pubptr,ax   ; save -> entry
    mov es,ax               ; es -> $$_COM_END declaration entry
    cmp BYTE PTR es:[14],0  ; see if this was the first ever use of $$_COM_END
    je  sei_no_com_end      ; yes, not used as an external anywhere, don't bother creating it
    mov BYTE PTR es:[14],2  ; flag as public declaration
    mov di,OFFSET DGROUP:com_end_text   ; ds:di -> declaration name
    call    add_pubdef_name ; save public name
    mov es:[4],ax           ; save offset pointer to name in pubdef names block
    mov es:[6],dx           ; save segment pointer to name in pubdef names block

sei_1:
    cmp ovl_count,0         ; see if any overlays
    je  sei_1a              ; no
    call    make_master_segblk  ; create block holding entries for all master segdefs of overlaid segparts
    call    resolve_ovl_info    ; resolve all overlay information, create overlay file

sei_1a:
    call    compute_seg_frames

sei_1b:
    call    free_memory     ; de-allocate unused memory
    mov ax,WORD PTR segment_stop    ; get final segment stop low word
    mov bx,WORD PTR segment_stop+2  ; get final segment stop high word
    add ax,1                ; add one to value to get total program size
    adc bx,0                ; adjust high word
    mov WORD PTR image_size,ax  ; save low word of size
    mov WORD PTR image_size+2,bx    ; save high word of size

    mov ah,48h              ; allocate memory
    mov bx,0ffffh           ; force request to fail, function will return largest available block
    int 21h
    cmp ax,8                ; insufficient memory error is expected
    je sei_2

sei_todos_err:
    jmp NEAR PTR dos_error  ; other errors are fatal

sei_2:
    cmp bx,500h             ; block size must be >=20K (500h paragraphs)
    jbe sei_todos_err       ; not enough memory for building executable, ax holds insufficent memory error value

    mov ax,bx
    sub ax,400h             ; must leave at least 16K for i/o buffers and remaining tables
    xor dx,dx               ; dx:ax will hold bytes of of space available
    shl ax,1                ; convert paragraphs available in ax into bytes in dx:ax
    rcl dx,1                ; x2
    shl ax,1
    rcl dx,1                ; x4
    shl ax,1
    rcl dx,1                ; x8
    shl ax,1
    rcl dx,1                ; x16, paragraph converted to bytes
    cmp dx,WORD PTR image_size+2    ; compare high word of space available to image size
    ja  sei_inmemory        ; enough available space to place image in memory
    jb  sei_ondisk          ; too little space to build executable in memory
    cmp ax,WORD PTR image_size  ; compare low word of space available to image size
    jae sei_inmemory

sei_ondisk:
    mov BYTE PTR is_ondisk,1    ; flag that image is on disk
    call    create_temp_file    ; create temporary file for executable image
    jmp SHORT sei_4         ; bypass memory image code

sei_inmemory:
    mov BYTE PTR is_inmem,1     ; flag that image is in memory
    mov bx,WORD PTR image_size  ; get low word of image size
    add bx,15               ; round up for paragraph computation
    mov dx,WORD PTR image_size+2    ; get high word of image size
    adc dx,0                ; carry to high word

; convert image size in dx:bx to paragraphs
    shr dx,1
    rcr bx,1                ; /2
    shr dx,1
    rcr bx,1                ; /4
    shr dx,1
    rcr bx,1                ; /8
    shr dx,1                ; dx should be zero by the final shift
    rcr bx,1                ; /16

    mov ah,48h              ; allocate memory
    int 21h
    jnc sei_3               ; no errors
    jmp NEAR PTR dos_error  ; any error is fatal

sei_3:
    mov image_mem_ptr,ax    ; save pointer to memory image
    mov bx,WORD PTR image_size  ; get low word of image size to zero
    mov dx,WORD PTR image_size+2    ; get high word of image size to zero
    call    zero_mem_image  ; fill memory image with zeros

sei_4:
    cmp ovl_count,0         ; see if any overlays
    je  sei_5               ; no
    call    get_memory      ; allocate memory for warplink use
    call    write_vectors   ; write vector values to overlay class segments
    call    free_memory     ; de-allocate unused memory

;if $$_COM_END variable exists, put program size at its location
sei_5:
    cmp com_end_flag,1      ; see if $$_COM_END variable declared
    jne sei_ret             ; no

    mov ax,com_end_pubptr
    mov es,ax               ; es -> $$_COM_END public declaration entry
    mov ax,es:[8]           ; get public offset
    mov WORD PTR data_offset,ax ; store in data_offset variable for write_bytes procedure
    xor ax,ax
    mov WORD PTR data_offset+2,ax   ; zero high word of data_offset
    mov ax,es:[0]           ; get pointer to segment partition entry
    mov es,ax               ; es -> segment partition entry
    mov es,es:[4]           ; es -> master segdef entry of $$_COM_END variable
    mov ax,es:[2]           ; get segment offset
    add WORD PTR data_offset,ax ; add in segment offset to data_offset
    mov si,OFFSET DGROUP:image_size ; get image size
	push	ds
	pop	es					; es:si -> image_size byte values
    mov cx,2                ; write two bytes
    call    write_bytes     ; write 'em

sei_ret:
    ret
setup_exe_image ENDP

;*****************************
;* COMPUTE_SEG_FRAMES        *
;*****************************

; compute segment frame values
; destroys all registers except ds

compute_seg_frames  PROC
    mov ax,seg_count
    mov unresolved_segs,ax  ; init count of unresolved address segments

    cmp is_anyovls,0        ; see if any overlaid modules specified
	jne	csf_res3			; yes

csf_1:
    mov res_occurred,1      ; init flag on to avoid error message

csf_resloop:
    cmp unresolved_segs,0   ; check if all segments have been resolved
    jne csf_2               ; not yet
    jmp NEAR PTR csf_res_done   ; yes, all segment addresses resolved

csf_2:
    cmp res_occurred,0      ; see if segment resolved this pass
    jne csf_res3            ; yes
    cmp is_dosseg,0         ; see if dosseg switch set
    jne csf_res2            ; yes

; no segment resolved last pass and not using DOSSEG segment ordering, internal error
csf_internal:
    mov ax,INTERNAL_ERR     ; put warplink error code in ax
    mov cx,1                ; internal error value
    jmp NEAR PTR link_error ; transfer control to error handler

csf_res2:
    inc phase               ; bump phase of segment ordering
    cmp phase,6             ; see if phase out of bounds
    jae csf_internal        ; yes, out of bounds

csf_res3:
    xor ax,ax
    mov entry_number,ax     ; init entry number

csf_res4:
    mov ax,first_segdefblk_ptr
    mov segdef_block_ptr,ax ; init current pointer to segdef block
    xor al,al
    mov res_occurred,al     ; reset segment resolution flag
    mov using_class,al      ; init using class name flag

csf_segblk_loop:
    mov ax,segdef_block_ptr
    or  ax,ax               ; check if any more segdef entries
    je  csf_resloop         ; no, move to next segment
    mov es,ax               ; es -> segdef entry

    mov ax,entry_number     ; get current entry number
    cmp ax,SEG_DEFENT_COUNT ; check if any room for more entries in block
    jb  csf_3               ; yes
    mov ax,es:[2]           ; point to next block
    mov segdef_block_ptr,ax ; save back to memory variable
    xor ax,ax
    mov entry_number,ax     ; reinit entry number
    jmp SHORT csf_segblk_loop  ; loop back for next block entry

csf_3:
    cmp ax,es:[0]           ; see if any more entries in block
    jae csf_resloop         ; no, break out to main segment resolution loop

; get an entry from the current block
csf_4:
    mov bx,entry_number     ; get entry number
    shl bx,1                ; each entry takes up two paragraphs
    mov ax,es               ; get block segment address
    add bx,ax               ; get entry's segment value
    inc bx                  ; adjust for block system info size of 1 paragraph
    mov es,bx               ; es -> segdef entry

    cmp BYTE PTR es:[27],0  ; check segment resolution flag
    je  csf_5               ; segment not yet resolved
    inc entry_number        ; bump to next segdef entry
    jmp SHORT csf_segblk_loop     ; loop back to check next segdef entry

csf_5:
    mov di,OFFSET DGROUP:class_name_field
    mov si,es:[12]          ; get segdef entry class name offset into lnames block
    add si,8                ; adjust past two doubleword pointers at beginning of lnames entry
    mov bx,es:[14]          ; get segdef entry class name segment into lnames block

    mov dx,ds               ; save data segment
    mov ds,bx               ; ds:si -> segdef entry class name
    mov bx,es               ; save pointer to segdef entry
    mov es,dx               ; es:di -> WarpLink data class name field

    cmp es:using_class,0    ; check if class selected yet
    je  csf_noclass         ; no
    jmp NEAR PTR csf_6      ; yes

; no class selected yet
csf_noclass:
    cmp es:com_end_flag,0   ; see $$_COM_END variable used and not allocated yet
    jne csf_5a              ; no
    call    get_class_type  ; get class type, returned in al
    or  al,al               ; see if class CODE
    jne csf_5a              ; no

; allocate room for $$_COM_END variable at end of segment
    mov es:com_end_flag,1   ; yes, set com_end_flag
    push    ds              ; save critical register
    mov ds,bx               ; ds -> segdef entry
    mov cx,ds:[6]           ; get previous segment length

;*** check for segment length overflow after 2-byte addition?

    add WORD PTR ds:[6],2   ; adjust length by two bytes for $$_COM_END variable
    mov ax,ds:[22]          ; get pointer to first segment partition entry
    mov ds,es:com_end_pubptr    ; ds -> $$_COM_END declaration entry
    mov ds:[0],ax           ; update segment pointer to segdef partition entry
    mov ds:[8],cx           ; offset of variable is previous end of segment
    pop ds                  ; restore critical register

csf_5a:
    cmp es:is_dosseg,0      ; see if DOSSEG segment ordering in force
    je  csf_clname_loop     ; no

; generate proper DOSSEG segment ordering
    call    get_class_type  ; get class type, returned in al
    cmp al,es:phase         ; see if phase matches type
    jne to_csf_restore      ; no
    cmp al,4                ; see if class BSS
    jne csf_not_bss         ; no
    cmp es:_edata_segaddr,0 ; see if beginning BSS address set yet
    jne csf_clname_loop     ; yes
    mov es:_edata_segaddr,bx    ; keep segdef entry segment pointer to BSS
    jmp SHORT csf_clname_loop

to_csf_restore:
    jmp NEAR PTR csf_restore

csf_not_bss:
    cmp al,5                ; see if class STACK
    jne csf_clname_loop     ; no
    cmp es:_end_segaddr,0   ; see if beginning STACK address set yet
    jne csf_clname_loop     ; yes
    mov es:_end_segaddr,bx  ; keep segdef entry segment pointer to BSS

; select a class, use entry class name
csf_clname_loop:
    movsb                   ; transfer a char from entry class name to current class name
    cmp BYTE PTR [si-1],0   ; zero byte signals end of transfer
    jne csf_clname_loop     ; nonzero, keep looping
    mov es:using_class,1    ; flag that a class is in use

    cmp es:is_clpinc,0      ; see if Clipper incremental link in process
    je  csf_5b              ; no
    cmp BYTE PTR [si-5],'C'
    jne csf_chksym          ; not class 'CODE'
    cmp BYTE PTR [si-4],'O'
    jne csf_chksym          ; not class 'CODE'
    cmp BYTE PTR [si-3],'D'
    jne csf_chksym          ; not class 'CODE'
    cmp BYTE PTR [si-2],'E'
    jne csf_chksym          ; not class 'CODE'
    mov es:res_incflag,1    ; set incremental link segment resolution flag for CODE
    jmp SHORT csf_5c

; check if class 'SYMBOLS'
csf_chksym:
    cmp BYTE PTR [si-8],'S'
    jne csf_5b              ; not class 'SYMBOLS'
    cmp BYTE PTR [si-7],'Y'
    jne csf_5b
    cmp BYTE PTR [si-6],'M'
    jne csf_5b
    cmp BYTE PTR [si-5],'B'
    jne csf_5b
    cmp BYTE PTR [si-4],'O'
    jne csf_5b
    cmp BYTE PTR [si-3],'L'
    jne csf_5b
    cmp BYTE PTR [si-2],'S'
    jne csf_5b
    mov es:res_incflag,2    ; set incremental link segment resolution flag for SYMBOLS
    jmp SHORT csf_5c

; not proper class
csf_5b:
    mov es:res_incflag,0    ; zero incremental link segment resolution flag

csf_5c:
    mov dx,es               ; save es -> WarpLink data
    mov es,bx               ; es -> segdef entry
    mov ax,es:[16]          ; get group entry segment, if any
    mov es,dx               ; es -> WarpLink data
    mov es:class_group,ax   ; save group entry segment of class-setting segment, only used by DOSSEG
    jmp SHORT csf_7         ; bypass class selected code

; class has already been selected
csf_6:
    cmpsb                   ; compare a entry class name char to current class name char
    je  csf_6a              ; okay so far

csf_restore:
	push	es
	pop	ds					; restore ds -> WarpLink's data
    mov es,bx               ; restore es -> segdef entry
    inc entry_number        ; bump to next segdef entry
    jmp NEAR PTR csf_segblk_loop    ; loop back to check next segdef entry

csf_6a:
    mov al,es:[di-1]        ; two matching zero bytes signals end of compare
    or al,[si-1]
    jne csf_6               ; nonzero, keep looping

; now, if the DOSSEG switch is tripped, compare and see if the group
; entries match.  If not, then do not group them together.
    cmp es:is_dosseg,0      ; see if DOSSEG segment ordering in force
    je  csf_7               ; no

    mov dx,es               ; save es -> WarpLink data
    mov es,bx               ; es -> segdef entry
    mov ax,es:[16]          ; get group entry segment, if any
    mov es,dx               ; es -> WarpLink data
    cmp ax,es:class_group   ; see if group entry segment matches class-setting segment group
    jne csf_restore         ; no, don't use this segment

csf_7:
	push	es
	pop	ds					; restore ds -> WarpLink data
    mov es,bx               ; restore es -> segdef entry
    mov dl,es:[26]          ; dl holds acbp byte
    mov dh,dl
    and dh,0e0h             ; dh holds align field
    mov BYTE PTR es:[27],1  ; flag that this segment was resolved
    mov res_occurred,1      ; flag that a segment was resolved this pass
    dec unresolved_segs     ; decrement number of unresolved segments

    or  dh,dh               ; check if absolute frame address, align field==0
    jne csf_8               ; not absolute, resolve segment

; absolute segment, do not resolve segment
    jmp NEAR PTR csf_next_entry ; bump entry count and reloop

; not an absolute frame address resolve segment address
csf_8:
    mov ah,BYTE PTR segment_start   ; get low byte of segment start (modulo 256)
    xor al,al
    cmp dh,80h              ; check if page aligned
    jne csf_9               ; no
    sub al,ah               ; get page alignment adjustment in al
    jmp SHORT csf_adj_segstart  ; bypass remaining alignment adjustment code

csf_9:
    cmp dh,60h              ; check if paragraph aligned
    jne csf_10              ; no
    and ah,15
    sub al,ah
    and al,15               ; get paragraph alignment adjustment
    jmp SHORT csf_adj_segstart  ; bypass remaining alignment adjustment code

csf_10:
    cmp dh,40h              ; check if word aligned
    jne csf_10a             ; no, no adjustment to segment start
    and ah,1
    mov al,ah               ; get word alignment adjustment

; al holds amount to add to segment start
csf_adj_segstart:
    xor ah,ah               ; zap high byte
    add WORD PTR segment_start,ax   ; adjust low word
    adc WORD PTR segment_start+2,0  ; add in carry from low word

csf_10a:
    mov ax,WORD PTR segment_start
    mov es:[2],ax           ; save segment's start address low word
    mov ax,WORD PTR segment_start+2
    mov es:[4],ax           ; save segment's start address high word

    mov al,res_incflag
    or  al,al               ; see if should resolve incremental link segment info
    je  csf_10b             ; no
    and al,1                ; zero al passed parameter if SYMBOLS (value of 2)
    call    res_ilfseg      ; resolve segment information in ILF file

csf_10b:
    mov al,dl               ; get acbp byte
    and al,2                ; check big bit status
    shr al,1                ; make byte value from bit either 1 or 0
    xor ah,ah               ; zap high byte
    mov bx,es:[6]           ; get segment length from segdef entry
    mov WORD PTR true_seg_len,bx    ; save low word of segment length
    mov WORD PTR true_seg_len+2,ax  ; save high word of segment length
    mov dh,dl               ; get acbp byte
    and dh,1ch              ; get combine field
    cmp dh,14h              ; check if segment is a stack segment
    je  csf_11              ; yes, leave nonzero to flag stack segment
    xor dh,dh               ; zero dh to flag not at stack segment

csf_11:
    or  stack_found,dh      ; track whether a stack segment is in the program
    and bl,1                ; check if segment length is odd
    je  csf_12              ; no
    or  dh,dh               ; check if segment is stack combine type
    je  csf_12              ; no

; stack segment cannot have odd byte length
    inc WORD PTR es:[6]     ; bump segment length by one
    cmp WORD PTR es:[6],0   ; check if overflowed to zero
    jne csf_11a             ; no
    or  BYTE PTR es:[26],2  ; set big bit in acbp byte

csf_11a:
    add WORD PTR true_seg_len,1 ; bump true segment length by one byte adjustment
    adc WORD PTR true_seg_len,0 ; carry bit to high word

;*** check for segment length overflow?

csf_12:
    or  dh,dh               ; check if stack combine type
    je csf_13               ; no
    cmp is_comfile,0        ; check if .COM file type
    jne csf_13              ; yes

; set up EXE initial stack values
    mov ax,WORD PTR true_seg_len+2  ; check that stack segment is < 64K
    or  ax,ax
    je  csf_12a             ; not 64K

; stack exceeds 64K-2 bytes
csf_stack_err:
    mov dx,OFFSET DGROUP:exe_name
    mov ax,STACK_SIZE_ERR   ; stack segment too big
    jmp NEAR PTR link_error ; transfer control to error handler

csf_12a:
    cmp WORD PTR true_seg_len,65534 ; check that stack segment is < 64K-2
    ja  csf_stack_err       ; stack segment is too big

; check program's initial SS:SP value
    mov ax,WORD PTR segment_start   ; get low word of start in ax
    mov bx,WORD PTR segment_start+2 ; get high word of start in bx
    shr bx,1
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1
    rcr ax,1                ; /16

; ax == segment frame paragraph (segment start/16)
    mov stack_segval,ax     ; save initial SS
    mov ax,WORD PTR true_seg_len    ; get end of stack segment (high word is zero)
    mov stack_offval,ax     ; save initial SP

; compute segment stop address (segment_start+true_seg_len-1)
csf_13:
    mov ax,WORD PTR segment_start
    add ax,WORD PTR true_seg_len    ; ax contains low word of sum
    mov bx,WORD PTR segment_start+2
    adc bx,WORD PTR true_seg_len+2  ; bx contains high word of sum
    mov cx,ax
    or  cx,bx                       ; see if segment stop address is zero (zero length starting segment)
    je  csf_zerolen_seg             ; yes, zero the segment stop
    sub ax,1                        ; subtract one from value
    sbb bx,0                        ; borrow from high word

; bx:ax == segment stop value
    cmp bx,WORD PTR segment_start+2 ; check that segment stop>=segment length in high word
    ja  csf_newstop                 ; yes
    je  csf_14                      ; maybe, check low word

csf_zerolen_seg:
    mov bx,WORD PTR segment_start+2 ; make segment stop = segment length
    mov ax,WORD PTR segment_start
    jmp SHORT csf_newstop           ; bypass low word check

csf_14:
    cmp ax,WORD PTR segment_start   ; check that segment stop>=segment length in low word
    jb csf_zerolen_seg              ; no, adjust segment stop

csf_newstop:
    mov WORD PTR segment_stop,ax    ; update segment stop variable
    mov WORD PTR segment_stop+2,bx

    cmp is_mapfile,0                ; see if need to write segments to map file
    je  csf_15                      ; no
    call    map_segments            ; yes, do it

csf_15:
    cmp WORD PTR es:[16],0          ; see if segment has associated group
    je  csf_15a                     ; nope
    call    update_grp_off          ; update group entry's offset, if necessary

csf_15a:
    mov ax,WORD PTR true_seg_len
    add WORD PTR segment_start,ax   ; update low word of segment start (next segment)
    mov ax,WORD PTR true_seg_len+2
    adc WORD PTR segment_start+2,ax ; update high word of segment start (next segment)

csf_next_entry:
    inc entry_number        ; bump to next segdef entry
    jmp NEAR PTR csf_segblk_loop    ; loop back to check next segdef entry

csf_res_done:
    cmp stack_found,0       ; check if stack segment
    je  csf_16              ; no
    cmp is_comfile,0        ; check if .COM file
    je  csf_ret             ; no

; stack segment in .COM file, fatal error
    mov dx,OFFSET DGROUP:exe_name
    mov ax,COM_STACK_ERR    ; stack segment in .COM file error
    jmp NEAR PTR link_error ; transfer control to error handler

; no stack segment found
csf_16:
    cmp is_comfile,0        ; check if .COM file
    jne csf_ret             ; yes

; EXE file with no stack, issue warning
    mov dx,OFFSET DGROUP:exe_name
    mov ax,NO_EXE_STACK_WARN
    call    link_warning

csf_ret:
    cmp is_mapfile,0                ; see if need to write groups to map file
    je  csf_out                     ; no
    call    map_groups              ; map groups, if any

csf_out:
    ret
compute_seg_frames  ENDP

;*****************************
;* WRITE_PROGRAM             *
;*****************************

; write finished .COM or .EXE program based upon executable image
; destroys all registers except ds,es

write_program   PROC
    cmp is_comfile,0        ; see if COM file
    jne wp_com              ; yes

; set .EXE header variables
wp_1:
    mov ax,number_reloc
    mov exe_header.eh_numrel,ax
    mov ax,stack_segval
    mov exe_header.eh_ss,ax
    mov ax,stack_offval
    mov exe_header.eh_sp,ax
    mov ax,entry_segval
    mov exe_header.eh_cs,ax
    mov ax,entry_offval
    mov exe_header.eh_ip,ax
    jmp SHORT wp_3              ; bypass COM code

; check for proper .COM file format
wp_com:
    cmp number_reloc,0      ; check if any relocation items
    je  wp_2                ; no

; can't have relocation items for a .COM file
    mov dx,OFFSET DGROUP:exe_name
    mov ax,COM_FIXUP_ERR    ; program has segment-relative fixups, bad .COM format
    jmp NEAR PTR link_error ; transfer control to error handler

; check if COM file has good entry point
wp_2:
    cmp entry_segval,0      ; see if begins at 0:100h
    je  pm_2a               ; okay so far

; bad entry point address for com file
wp_com_entry:
    mov dx,OFFSET DGROUP:exe_name
    mov ax,COM_ENTRY_ERR
    jmp NEAR PTR link_error ; transfer control to error handler

pm_2a:
    cmp entry_offval,100h   ; check offset
    jne wp_com_entry        ; bad offset

; update temporary file from memory pages, if exists
wp_3:
    cmp is_ondisk,0         ; check if temporary file in use
    je  wp_3a               ; no
    call    update_temp_file    ; flush pages to disk

wp_3a:
    cmp ovl_count,0         ; see if any overlays
    je  wp_3b               ; no
    call    flush_ovl_page  ; flush overlay file memory page
    call    flush_reloc_array   ; flush relocation array to disk
    cmp is_emsxms_ovl,0     ; see if overlays in EMS/XMS
    je  wp_3b               ; no
    call    ems_ovl_to_file ; flush overlays from EMS/XMS to disk

wp_3b:
    mov dx,OFFSET DGROUP:exe_name   ; DS:DX -> ASCIIZ file specification
    mov ah,3ch              ; create/truncate file
    xor cx,cx               ; normal file attribute
    int 21h
    call    restore_ems_map
    jnc wp_4                ; no errors
    jmp NEAR PTR dos_error  ; error opening file

; write program image to highest write address (above is uninit'ed data)
; di:si will hold number of bytes to write
wp_4:
    mov exe_handle,ax       ; keep file handle of executable file
    mov si,WORD PTR highest_exe_write   ; get low word
    mov di,WORD PTR highest_exe_write+2 ; get high word
    add si,1                ; amount to write is one more than highest write amount
    adc di,0                ; carry to high word

    cmp  is_comfile,0       ; check if .COM file
    je  wp_5                ; no

    or  di,di               ; check if file is 64K or larger
    je  wp_com2             ; no
    cmp di,2                ; check that file doesn't exceed 64K in high word
    jae wp_com_size         ; it does, com file too large
    or  si,si               ; file is at least 64K, check that it isn't more
    je  wp_com2             ; file is exactly 64K

; .COM file is larger than 65536 bytes
wp_com_size:
    mov dx,OFFSET DGROUP:exe_name
    mov ax,COM_SIZE_ERR     ; program larger than 64K, bad .COM format
    jmp NEAR PTR link_error ; transfer control to error handler

; start writing offset 100h for .COM file
wp_com2:
    add WORD PTR image_mem_ptr,10h  ; adjust start of write by 10h paragraphs
    sub si,256              ; adjust bytes to write
    sbb di,0                ; borrow to high word
    mov bx,exe_handle       ; file handle
    jmp NEAR PTR wp_write_img   ; bypass EXE relocation table write code

wp_5:
; compute size of and allocate space for uninit'ed data
    mov ax,WORD PTR image_size
    mov bx,WORD PTR image_size+2    ; bx:ax == program image size
    sub ax,si               ; subtract low word of executable bytes
    sbb bx,di               ; subtract high word of executable bytes
    add ax,15
    adc bx,0                ; force paragraph computation to round up
    shr bx,1
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1
    rcr ax,1                ; /16, ax holds paragraphs of uninit'ed data
    mov exe_header.eh_minalloc,ax   ; save it as exe header value

wp_5a:
    mov ax,si               ; get low word of executable bytes
    and ax,511              ; make file length modulo 512
    mov exe_header.eh_flen,ax   ; save as exe header value

    xor bx,bx
    mov ax,number_reloc     ; get number of relocation items
    shl ax,1                ; convert items to bytes (4 bytes/item)
    rcl bx,1                ; x2
    shl ax,1
    rcl bx,1                ; x4, bx:ax holds relocation item bytes
    add ax,exe_header.eh_roff   ; get header size in bytes
    adc bx,0                ; carry to bx
    test    ax,511          ; see if on 512-byte boundary
    je  wp_6                ; yes
    mov cx,512
    mov dx,ax
    and dx,511              ; get odd page value in dx
    sub cx,dx               ; compute amount to add to bring to 512-byte boundary
    add ax,cx               ; add in amount
    adc bx,0                ; carry to bx

wp_6:
    push    bx              ; save header size in bytes high word
    push    ax              ; save header size in bytes low word
    shr bx,1                ; convert header size to paragraphs
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1                ; bx should be zero value by this shift
    rcr ax,1                ; /16, ax holds header size in paragraphs
    mov exe_header.eh_hsize,ax  ; save as exe header value

    xor cx,cx               ; adjustment value to bring to 512-byte boundary
    mov ax,si               ; get low word of executable bytes
    and ax,511              ; see if already on 512-byte boundary
    je  wp_7                ; yes
    mov cx,512
    sub cx,ax               ; compute amount to add to bring to 512-byte boundary

wp_7:
    mov bx,di
    mov ax,si
    add ax,cx               ; bring file size to 512-byte boundary
    adc bx,0                ; carry to bx
    pop cx                  ; get header size in bytes low word
    pop dx                  ; get header size in bytes high word
    add ax,cx
    adc bx,dx               ; get file size INCLUDING header in bytes

; compute file size in 512-byte pages
    mov al,ah
    mov ah,bl
    mov bl,bh
    xor bh,bh               ; register shifts do effective divide by 256
    shr bx,1
    rcr ax,1                ; /512
    mov exe_header.eh_fsize,ax  ; save file size in 512-byte pages in exe header

    cmp is_stackval,0       ; see if stack value specified
    je  wp_8                ; no
    mov ax,WORD PTR image_size
    mov bx,WORD PTR image_size+2    ; bx:ax == program image size
    mov cx,stack_segval
    xor dx,dx
    shl cx,1                ; convert paragraphs available in cx into bytes in dx:cx
    rcl dx,1                ; x2
    shl cx,1
    rcl dx,1                ; x4
    shl cx,1
    rcl dx,1                ; x8
    shl cx,1
    rcl dx,1                ; x16, paragraph converted to bytes
    sub ax,cx
    sbb bx,dx               ; bx:ax holds program bytes above stack allocation start
    or  bx,bx               ; see if >64K (always room for stack specified)
    jne wp_7a               ; yes
    mov cx,stack_value
    sub cx,ax               ; see if remainder is > specified stack
    jc  wp_7a               ; yes

; specified stack is greater than memory image, bump minimum value by difference (in paras)
    add cx,15               ; round up to next paragraph
    shr cx,1
    shr cx,1
    shr cx,1
    shr cx,1                ; bytes converted to paragraphs
    add exe_header.eh_minalloc,cx   ; add to previous minimum value

; set new stack value
wp_7a:
    mov ax,stack_value
    mov exe_header.eh_sp,ax

; see if maximum paragraph allocation space specified
; if less than minimum, give feeback and set to minimum
wp_8:
    cmp is_maxparval,0      ; see if allocation value specified
    je  wp_8a               ; no
    mov ax,maxpar_value     ; get allocation value
    mov exe_header.eh_maxalloc,ax   ; save it in header variable
    cmp ax,exe_header.eh_minalloc   ; make sure >= minimum allocation
    jae wp_8a               ; okay value

; specified maximum paragraph allocation is less than minimum
    mov dx,OFFSET DGROUP:maxalloc_warn  ; ds:dx -> write buffer area
    mov cx,maxalloc_len
    mov bx,STDOUT
    mov ah,40h              ; write to standard output device
    int 21h
    call    restore_ems_map

    mov ax,exe_header.eh_minalloc   ; get minimum allocation
    mov exe_header.eh_maxalloc,ax   ; save as maximum

; write .EXE file header
wp_8a:
    mov dx,OFFSET DGROUP:exe_header ; ds:dx -> write buffer area
    mov cx,30               ; size of EXE header control info
    mov bx,exe_handle       ; file handle
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jc  wp_to_dos_error     ; error writing to file

wp_9:
    or  ax,ax               ; see if out of disk space
    jne wp_9a               ; no

; out of disk space for executable file
wp_diskspace:
    mov ax,DISK_FULL_ERR
    mov dx,OFFSET DGROUP:exe_name
    jmp NEAR PTR link_error ; transfer to error handler

wp_9a:
    mov ax,first_relblk_ptr ; get first relocation table block

wp_rel_loop:
    or  ax,ax               ; check that is non-null
    je  wp_10               ; no more relocation entries
    push    ds              ; save ds -> warplink data segment, used
    mov ds,ax               ; ds -> block
    mov dx,4                ; ds:dx -> relocation items to write
    mov cx,ds:[0]           ; get number of entries
    shl cx,1
    shl cx,1                ; entries * 4 == byte count to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map

    or  ax,ax               ; see if out of disk space
    je  wp_diskspace        ; yes

    mov cx,ax               ; save error code in ax, if any
    mov ax,ds:[2]           ; get pointer to next block, if any
    pop ds                  ; restore ds -> warplink data
    jnc wp_rel_loop         ; no errors, loop for next block write
    mov ax,cx               ; get error code back in ax
    mov dx,OFFSET DGROUP:exe_name
    jmp NEAR PTR dos_error  ; error writing to file

wp_10:
    xor cx,cx
    mov ax,number_reloc
    shl ax,1
    rcl cx,1                ; x2
    shl ax,1
    rcl cx,1                ; x4, cx:ax hold bytes of relocation items
    add ax,exe_header.eh_roff   ; add in header offset, ignore carry
    and ax,511              ; get header size modulo 512
    mov cx,512
    sub cx,ax               ; cx holds number of zero byte values needed for header to be on 512-byte page
    jcxz    wp_write_img    ; no bytes needed

    mov dx,OFFSET DGROUP:zero_table ; ds:dx point to table of zeros to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc wp_write_img        ; no errors

wp_to_dos_error:
    mov dx,OFFSET DGROUP:exe_name
    jmp NEAR PTR dos_error  ; error writing to file

wp_write_img:
    or  ax,ax               ; see if out of disk space
    jne wp_10a              ; no
    jmp NEAR PTR wp_diskspace   ; yes

wp_10a:
    cmp is_ondisk,0         ; check if memory image is on disk
    je  wp_mem_write        ; no

    call    dump_temp_file  ; dump temporary file bytes into executable file
    jmp SHORT wp_ovlclose   ; close overlay file if exists

; transfer program to disk in 64K-16 chunks
; di:si contain number of bytes to write, bx == file handle
wp_mem_write:
    xor dx,dx               ; zero offset of write buffer
    or  di,di               ; see if byte count to write is 64K or more
    jne wp_11               ; yes, write a 64K-16 chunk
    cmp si,0fff0h           ; see if byte count is at least 64K-16
    jb wp_12                ; no, exit 64K-16 writing loop

wp_11:
    mov cx,0fff0h           ; number of bytes to write (64K-16)
    mov ax,image_mem_ptr
    push    ds              ; save ds, critical register
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore ds -> warplink data
    jnc wp_11a              ; no errors
    jmp SHORT wp_to_dos_error

wp_11a:
    or  ax,ax               ; see if out of disk space
    jne wp_11b              ; no
    jmp NEAR PTR wp_diskspace   ; yes

wp_11b:
    add image_mem_ptr,0fffh ; adjust segment pointer past 64K-16 written (4K-1 paragraphs)
    sub si,0fff0h           ; back off number of bytes written from bytes to write
    sbb di,0                ; borrow to high word
    jmp SHORT wp_mem_write  ; loop for next write

; transfer leftover bytes (file size modulo 0fff0h)
wp_12:
    mov cx,si               ; cx holds bytes to write
    jcxz    wp_ovlclose     ; no bytes to write
    mov ax,image_mem_ptr
    push    ds              ; save ds, critical register
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore ds -> WarpLink data
    jc  wp_to_dos_error     ; error occurred

    or  ax,ax               ; see if out of disk space
    jne wp_ovlclose         ; no
    jmp NEAR PTR wp_diskspace   ; yes

wp_ovlclose:
    mov bx,ovl_handle       ; get overlay file handle
    or  bx,bx               ; make sure nonzero
    je  wp_ret              ; zero, don't close (no file)

    cmp is_internal,0       ; check if temporary overlay file
    jne wp_tmpovl           ; yes
    mov ah,3eh              ; close overlay file
    int 21h
    call    restore_ems_map
    jmp SHORT wp_ret        ; bypass temporary overlay file code

wp_tmpovl:
    call    dump_ovl_file   ; dump temporary overlay file to end of executable file

wp_ret:
    mov bx,exe_handle
    mov ah,3eh              ; close executable file
    int 21h
    call    restore_ems_map
    ret
write_program   ENDP

;*****************************
;* DUMP_OVL_FILE             *
;*****************************

; copy temporary overlay file to end of EXE file
; then close it and delete it
; upon entry bx== overlay file handle
; destroys ax,bx,cx,dx,si,di

dump_ovl_file   PROC
    xor cx,cx               ; move file pointer to start of file
    mov dx,cx
    mov ax,4202h            ; move file pointer, offset from file end
    int 21h
    call    restore_ems_map
    jc  dof_ovl_err         ; error in seek

    mov di,dx               ; save overlay file size high word
    mov si,ax               ; save overlay file size low word

    cmp is_no_ems,0         ; see if EMS used
    jne dof_1               ; no

; EMS used, by definition it's okay to use physical page 0 for i/o buffer
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
    jmp SHORT dof_4a

dof_1:
    mov ah,48h              ; allocate memory
    mov bx,0ffffh           ; force request to fail, function will return largest available block
    int 21h
    cmp ax,8                ; insufficient memory error is expected
    je dof_2
    jmp NEAR PTR dos_error  ; other errors are fatal

dof_2:
    cmp bx,0fffh            ; check if more than 0fffh free paragraphs (64K-16 bytes)
    jbe dof_3               ; no
    mov bx,0fffh            ; allocate only up to 64K-16 for temporary disk write

dof_3:
    mov ax,bx
    shl ax,1                ; convert paragraphs allocated to bytes, x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    shl ax,1                ; x16
    mov temp_buffer_size,ax ; keep size of temporary buffer
    mov ah,48h              ; allocate memory
    int 21h
    jnc dof_4               ; should be successful, since using <= previous max value
    jmp NEAR PTR dos_error  ; any error is fatal

dof_4:
    mov temp_buffer_base,ax ; save address of temporary buffer

dof_4a:
    mov bx,ovl_handle
    xor cx,cx               ; move file pointer to start of file
    mov dx,cx
    mov ax,4200h            ; move file pointer, offset from file start
    int 21h
    call    restore_ems_map
    jnc dof_5               ; no errors in seek

dof_ovl_err:
    mov dx,OFFSET DGROUP:ovl_filename
    jmp NEAR PTR dos_error  ; DOS file error

dof_5:
    or  di,di               ; see if byte count to write is 64K or more
    jne dof_6               ; yes, write a zero_block_size chunk
    cmp si,temp_buffer_size ; see if byte count is at least temp_buffer_size
    jb  dof_8               ; no, exit temp_buffer_size writing loop

dof_6:
    mov cx,temp_buffer_size ; number of bytes to read
    xor dx,dx               ; zero offset into buffer
    mov bx,ovl_handle
    mov ax,temp_buffer_base
    push    ds              ; save ds, critical register

	cmp	ax,ems_base			; see if reading to EMS
    mov ds,ax               ; ds -> segment to start write, DON'T MODIFY FLAGS
	jne	dof_fileread1		; no
	call	read_to_ems		; call read to EMS code
	jmp	SHORT dof_readdone1

dof_fileread1:
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map

dof_readdone1:
    pop ds                  ; restore ds -> warplink data
to_dof_ovl_err:
    jc  dof_ovl_err         ; error reading from file

dof_6a:
    mov cx,temp_buffer_size ; number of bytes to write
    xor dx,dx               ; zero offset into buffer
    mov bx,exe_handle
    mov ax,temp_buffer_base
    push    ds              ; save ds, critical register
    mov ds,ax               ; ds -> segment to start write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore ds -> WarpLink data
    jnc dof_7               ; no errors

dof_exe_err:
    mov dx,OFFSET DGROUP:exe_name
    jmp NEAR PTR dos_error  ; error writing to file

dof_7:
    or  ax,ax               ; see if out of disk space
    jne dof_7a              ; no

; out of disk space for executable file
dof_diskspace:
    mov ax,DISK_FULL_ERR
    mov dx,OFFSET DGROUP:exe_name
    jmp NEAR PTR link_error ; transfer to error handler

dof_7a:
    sub si,temp_buffer_size ; back off number of bytes written from bytes to write
    sbb di,0                ; borrow to high word
    jmp SHORT dof_5         ; loop for next write

; transfer leftover bytes (file size modulo temp_buffer_size)
dof_8:
    mov cx,si               ; cx holds bytes to write
    jcxz    dof_release     ; no bytes to write
    xor dx,dx               ; zero offset into buffer
    mov bx,ovl_handle
    mov ax,temp_buffer_base
    push    ds              ; save ds, critical register

	cmp	ax,ems_base			; see if reading to EMS
    mov ds,ax               ; ds -> segment to start write, DON'T MODIFY FLAGS
	jne	dof_fileread2		; no
	call	read_to_ems		; call read to EMS code
	jmp	SHORT dof_readdone2

dof_fileread2:
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map

dof_readdone2:
    pop ds                  ; restore ds -> warplink data
    jc  to_dof_ovl_err		; error reading from file

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
    jc  dof_exe_err         ; error writing to file

    or  ax,ax               ; see if out of disk space
    je  dof_diskspace       ; yes

; release temporary buffer back to dos
dof_release:
    cmp is_no_ems,0         ; see if EMS used for i/o buffer
    je  dof_close           ; yes, no memory to release

    mov es,temp_buffer_base
    mov ah,49h              ; release memory
    int 21h
    jc  dof_exe_err         ; error occurred in memory release

; close temporary file
dof_close:
    mov bx,ovl_handle       ; get overlay file handle
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

; delete temporary overlay file
    mov dx,OFFSET DGROUP:ovl_filename
    mov ah,41h              ; delete file
    int 21h
    call    restore_ems_map
    ret
dump_ovl_file   ENDP

;*****************************
;* WRITE_BYTES               *
;*****************************

; write bytes to program's memory image
; upon entry es:[si] -> buffer to write from, cx == number of bytes to write,
; data_offset == offset from start of program
; updates si
; destroys ax,cx,di

write_bytes PROC
    push    ds              ; save critical register
    push    bp
    push    bx
    push    dx
    mov ax,buffer_base
    mov bx,es
    cmp bx,ax               ; see if write buffer is i/o buffer
    je  wb_is_io            ; yes
    mov ax,0ffffh           ; set to highest possible value so no overflow occurs
    jmp SHORT wb_set_bp

wb_is_io:
    mov ax,buffer_end

wb_set_bp:
    mov bp,ax               ; check only for end of physical buffer overflow

    cmp is_clarion,0        ; see if clarion switch thrown
    je  wb_notcdata         ; no
    cmp ovl_data_id,0       ; see if clarion overlay data
    je  wb_notcdata         ; no
    call    write_clar_data ; write to .OVL file
    jmp NEAR PTR wb_ret     ; and return

wb_notcdata:
    cmp ovl_code_id,0       ; see if write is to an overlay
    je  wb_1                ; no
    call    ovl_file_write  ; write to .OVL file
    jmp NEAR PTR wb_ret     ; and return

wb_1:
    mov ax,WORD PTR data_offset ; get offset from program start in di:ax
    mov bx,ax               ; bx holds low word data offset
    mov di,WORD PTR data_offset+2
    mov dx,di               ; dx holds high word data offset
    add ax,cx               ; add in number of bytes to write
    adc di,0                ; carry to high word
    sub ax,1                ; adjust for number of bytes relative zero
    sbb di,0                ; borrow to high word

    cmp di,WORD PTR highest_exe_write+2 ; compare to highest previous address write high word
    jb  wb_2                ; lower than previous address write
    ja  wb_new_high         ; higher than previous
    cmp ax,WORD PTR highest_exe_write   ; compare to highest previous address write low word
    jbe wb_2                ;  lower than or equal to previous

; update highest address write to new value
wb_new_high:
    mov WORD PTR highest_exe_write,ax
    mov WORD PTR highest_exe_write+2,di

    cmp di,WORD PTR image_size+2    ; check if attempt to write out of bounds
    jb  wb_2                ; no, in bounds
    ja  wb_bounds           ; yes, out of bounds
    cmp ax,WORD PTR image_size      ; check low word, high words are equal
    jb  wb_2                ; no, in bounds

; attempt to write out of bounds of program memory image
wb_bounds:
    mov dx,OFFSET DGROUP:filename
    mov ax,IMAGE_BOUNDS_ERR ; attempt to write data outside of program bounds
    jmp NEAR PTR error_read_buff_pos    ; transfer control to error handler

wb_2:
    cmp is_ondisk,0         ; check if memory image is on disk
    je  wb_memory           ; no
    call    temp_file_write ; write bytes to temporary file (memory pages)
    jmp SHORT wb_ret        ; and return

wb_memory:
    mov di,bx               ; di has low word offset of data
    and di,15               ; di has normalized offset
    and bx,0fff0h           ; clear out normalized offset bytes
    mov ax,dx               ; ax has high word of data bytes offset
    xchg    ax,bx           ; high word in bx, low word in ax (w/o normalized offset bytes)

; convert bx:ax byte count to segment value in ax
    shr bx,1
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1
    rcr ax,1                ; /16

; ax:di == normalized segment:offset of data offset

    add ax,image_mem_ptr    ; add in segment of memory image
    mov bx,es
    mov ds,bx               ; ds -> source buffer segment
    mov es,ax               ; es:di -> location in memory image to place data

    mov ax,si               ; get offset of source
    add ax,cx               ; add in number of bytes to write
    jc  wb_buff_wrap        ; overflow, buffer will wrap
    cmp ax,bp               ; see if past buffer end for total bytes written
    jbe wb_no_buff_wrap     ; no

wb_buff_wrap:
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

wb_no_buff_wrap:
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

wb_endloop:
    mov es,bx               ; restore es -> source buffer

wb_ret:
    pop dx                  ; restore critical register
    pop bx
    pop bp
    pop ds
    ret
write_bytes ENDP

;*****************************
;* UPDATE_GRP_OFF            *
;*****************************

; make group offset the lowest address of the segments in the group
; upon entry es -> segdef entry
; destroys ax

update_grp_off  PROC
    push    ds              ; save critical register
    mov ax,es:[16]          ; get pointer to group entry
    mov ds,ax               ; ds -> group entry
    mov ax,es:[4]           ; get segment offset high word
    cmp ax,ds:[2]           ; compare to group offset high word
    ja  ugo_ret             ; segment offset higher than group offset
    jb  ugo_2               ; segment offset lower, update group offset
    mov ax,es:[2]           ; get segment offset low word
    cmp ax,ds:[0]           ; compare to group offset low word
    jae ugo_ret             ; segment offset higher or equal to group offset

ugo_2:
    mov ax,es:[4]           ; get high word
    mov ds:[2],ax           ; update group offset high word
    mov ax,es:[2]           ; get low word
    mov ds:[0],ax           ; update group offset low word
    mov ax,es
    mov ds:[8],ax           ; save segdef entry pointer of lowest segment

ugo_ret:
    pop ds                  ; restore critical register
    ret
update_grp_off  ENDP

;*****************************
;* GET_CLASS_TYPE            *
;*****************************

; get type of segment class for DOSSEG segment ordering
; type == 0 for segment class CODE
;      == 1 for other segments outside of DGROUP
;      == 2 for DGROUP segment class BEGDATA
;      == 3 for DGROUP segment class not equal to BEGDATA, BSS, or STACK
;      == 4 for DGROUP segment class BSS
;      == 5 for DGROUP segment class STACK
; classes are not case sensitive for type (but are for concatenation)
; a suffix of the class name establishes type, e.g. BC_CODE is class CODE
; upon entry ds:si -> class name, es -> warplink data,
;  bx holds segment of segdef entry
; returns class type in al
; destroys ax,cx

get_class_type  PROC
    push    es              ; save critical register
    push    ds
    push    di
    push    bx

	push	es
	push	ds
	pop	es
	pop	ds					; ds -> warplink data, es:si -> class name
    xor cx,cx               ; cx will hold string char count
    mov bx,si               ; bx -> class name

gct_count_loop:
    cmp BYTE PTR es:[bx],0  ; see if at end of string
    je  gct_code_chk        ; yes
    inc cx                  ; bump count of chars
    inc bx                  ; bump char slot
    jmp SHORT gct_count_loop    ; loop to check next char

gct_code_chk:
    cmp cx,4                ; see if possible class CODE
    jb  gct_nodgroup_chk    ; no, name string too small
    mov di,cx               ; di will offset into end of string
    sub di,4                ; bx+di -> last four chars of string
    mov bx,si               ; bx -> class name
    add bx,di               ; bx -> last four chars of string
    mov di,OFFSET DGROUP:codetext   ; di -> target string
    call    caseless_strcmp ; see if strings match
    or  al,al               ; check return value
    jne gct_nodgroup_chk    ; nonzero return value, string didn't match
    jmp NEAR PTR gct_ret    ; return al == 0 for class CODE

; see if segment is NOT in group DGROUP
gct_nodgroup_chk:
    mov di,es               ; save es -> class name string
    pop es                  ; es -> segdef entry
    push    es              ; restore value to stack
    mov ax,es:[16]          ; get group entry segment, if any
    or  ax,ax               ; check if entry exists
    jne gct_2               ; yes

; no group exists for this segment, not in group DGROUP
gct_not_dgroup:
    mov al,1                ; return al ==1 for segment not in DGROUP
    jmp NEAR PTR gct_ret

gct_2:
    mov es,ax               ; es -> group entry
    mov bx,es:[4]           ; get group name offset in bx
    mov es,es:[6]           ; es:bx -> group name
    add bx,8                ; adjust past two doubleword pointers

; simple-minded check for group DGROUP, case sensitive
    cmp BYTE PTR es:[bx],'D'    ; check first char
    jne gct_not_dgroup      ; doesn't match
    cmp BYTE PTR es:[bx+1],'G'  ; check remaining chars
    jne gct_not_dgroup
    cmp BYTE PTR es:[bx+2],'R'
    jne gct_not_dgroup
    cmp BYTE PTR es:[bx+3],'O'
    jne gct_not_dgroup
    cmp BYTE PTR es:[bx+4],'U'
    jne gct_not_dgroup
    cmp BYTE PTR es:[bx+5],'P'
    jne gct_not_dgroup
    cmp BYTE PTR es:[bx+6],0  
    jne gct_not_dgroup

; segment is a member of group DGROUP
    mov es,di                   ; restore es -> class name

; see if segment is class BEGDATA
gct_begdata_chk:
    cmp cx,7                ; see if possible class BEGDATA
    jb  gct_bss_chk         ; no, name string too small
    mov di,cx               ; di will offset into end of string
    sub di,7                ; bx+di -> last seven chars of string
    mov bx,si               ; bx -> class name
    add bx,di               ; bx -> last seven chars of string
    mov di,OFFSET DGROUP:begdatatext    ; di -> target string
    call    caseless_strcmp ; see if strings match
    or  al,al               ; check return value
    jne gct_bss_chk         ; nonzero return value, string didn't match
    mov al,2                ; return al == 2 for class BEGDATA
    jmp SHORT gct_ret

; see if segment is class BSS
gct_bss_chk:
    cmp cx,3                ; see if possible class BSS
    jb  gct_stack_chk       ; no, name string too small
    mov di,cx               ; di will offset into end of string
    sub di,3                ; bx+di -> last three chars of string
    mov bx,si               ; bx -> class name
    add bx,di               ; bx -> last three chars of string
    mov di,OFFSET DGROUP:bsstext    ; di -> target string
    call    caseless_strcmp ; see if strings match
    or  al,al               ; check return value
    jne gct_stack_chk       ; nonzero return value, string didn't match
    mov al,4                ; return al == 4 for class BSS
    jmp SHORT gct_ret

; see if segment is class STACK
gct_stack_chk:
    cmp cx,5                ; see if possible class STACK
    jb  gct_other           ; no, name string too small
    mov di,cx               ; di will offset into end of string
    sub di,5                ; bx+di -> last five chars of string
    mov bx,si               ; bx -> class name
    add bx,di               ; bx -> last five chars of string
    mov di,OFFSET DGROUP:stacktext  ; di -> target string
    call    caseless_strcmp ; see if strings match
    or  al,al               ; check return value
    jne gct_other           ; nonzero return value, string didn't match
    mov al,5                ; return al == 5 for class STACK
    jmp SHORT gct_ret

; segment is in DGROUP but does NOT have class BEGDATA, BSS, or STACK
gct_other:
    mov al,3                ; resturn al == 3 for non- BEGDATA, BSS, and STACK DGROUP segment

gct_ret:
    pop bx                  ; restore critical register
    pop di
    pop ds
    pop es
    ret
get_class_type  ENDP

;*****************************
;* CASELESS_STRCMP           *
;*****************************

; caseless string compare, upper and lowercase chars match 
; checks string -> by es:bx against string -> by ds:di
; returns al == 0 is match, al == 1 is no match
; destroy ax,bx,di

caseless_strcmp PROC
cs_comp_loop:
    mov al,es:[bx]          ; get char
    or  al,al               ; if zero then at end of strings
    jne cs_2                ; not done with string compare
    or  al,[di]             ; merge in second string char, still zero (successful match) if both terminated
    jne cs_nomatch          ; not zero, flag unsuccessful match
    ret

cs_2:
    cmp al,'a'              ; check lowercase lower bound
    jb  cs_3                ; no
    cmp al,'z'              ; check lowercase upper bound
    ja  cs_3                ; no
    sub al,20h              ; convert to uppercase

cs_3:
    mov ah,[di]             ; get target string char
    cmp ah,'a'              ; check lowercase lower bound
    jb  cs_4                ; no
    cmp ah,'z'              ; check lowercase upper bound
    ja  cs_4                ; no
    sub ah,20h              ; convert to uppercase

cs_4:
    cmp al,ah               ; see if chars match
    jne cs_nomatch          ; no
    inc bx                  ; this char matches, try next char in string
    inc di
    jmp SHORT cs_comp_loop  ; loop till string complete

cs_nomatch:
    mov al,1                ; flag unsuccessful compare
    ret
caseless_strcmp ENDP

END
