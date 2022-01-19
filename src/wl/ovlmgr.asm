;*********************************************************************
;*   OVLMGR.ASM                                                      *
;*   By:            Michael Devore                                   *
;*   Date:          03/12/93                                         *
;*   Version:       2.50                                             *
;*   Assembler:     TASM 2.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*   Copyright 1990-91, Michael E. Devore                            *
;*                                                                   *
;*   overlay manager code                                            *
;*                                                                   *
;*********************************************************************

TITLE   OVLMGR.ASM
PAGE    50,80

;*****************************
;* EQUATES                   *
;*****************************

STDIN   EQU 0               ; standard input device
STDOUT  EQU 1               ; standard output device
STDERR  EQU 2               ; standard error device

BELL    EQU     7           ; beep the speaker
CR      EQU     13          ; carriage return
LF      EQU     10          ; line feed
FF      EQU     12          ; form feed

;*****************************
;* PUBLICS                   *
;*****************************

; set by /os linker option, used by overlay manager
PUBLIC  $$_ovl_stack

; set by /ol linker option, used by overlay manager
PUBLIC  $$_ovl_max_load

; set by /op linker option, used by overlay manager
PUBLIC  $$_ovl_pool
PUBLIC  $$_ovl_mem_alloc_flag

; set by /r linker option, used by overlay manager
PUBLIC  $$_ovl_reload_flag

; set by /cla linker option, used by overlay manager
PUBLIC  $$_ovl_clarion_flag

; set by /oi linker option, used by overlay manager
PUBLIC  $$_ovl_internal_flag

; set by /ox linker options, used by overlay manager
PUBLIC  $$_ovl_ems_pool_flag

; name of overlay file, including .OVL extension, computed by linker
PUBLIC  $$_ovl_filename
PUBLIC  _ovlmgr_overlay_filename

; /ox e-var setting
PUBLIC  $$_ovl_ox_evar

; size in paragraphs of largest overlay rounded to 1K page, computed by linker
PUBLIC  $$_ovl_min_alloc

; main entry point of program, computed by linker
PUBLIC  $$_main_entry

; segment of lookup table for overlay target addresses, computed by linker
PUBLIC  $$_ovl_lookup_table

; segment of indirect call table for overlay target addresses, computed by linker
PUBLIC  $$_ovl_ind_call_table

; address used by linker for initial entry
PUBLIC  $$_ovl_init_entry

; address used by linker to vector calls to overlaid publics
PUBLIC $$_ovl_vector

; address used by link to return vectored calls
PUBLIC $$_ovl_ret

; size in paragraphs of executable program
PUBLIC  $$_min_prog_size

; addresses used by link for indirect calls
PUBLIC  $$_ovl_ind_2ovl_ent

; address used by link for overlaid target segment fixups
PUBLIC  $$_ovl_seg_call_table
PUBLIC  $$_ovl_seg_call_entry

; routine to call to close overlay file
PUBLIC  _ovlmgr_close_ovl_file

; routine to call to open overlay file
PUBLIC  _ovlmgr_open_ovl_file

; routine to free overlay manager acquired EMS
PUBLIC  _ovlmgr_free_ems,_om_fr_ems

; /ort setting
PUBLIC  $$_ovl_ort

; /orp setting
PUBLIC  $$_ovl_orp

; /ohp setting
PUBLIC	$$_ovl_ohp,$$_ohp_size,$$_ohp_flag

; /oht setting
PUBLIC	$$_ovl_oht,$$_oht_size,$$_oht_flag

; /ou setting
PUBLIC	$$_ovl_umb

; /ohp3 EMS 3.0 compatibility settting
PUBLIC	$$_ems3_flag

; size of overlay file in kilobytes (used for file stashing)
PUBLIC	$$_ovl_file_size

; locations referenced by profiler
PUBLIC $$_ovl_jump_target, $$_ovl_destination_id
PUBLIC $$_ovl_call_target, $$_ovl_source_id
PUBLIC $$_ovl_load_start, $$_ovl_load_end
PUBLIC $$_ovl_load_count, $$_ovl_reload_count
PUBLIC $$_ovl_in_manager, $$_ovl_is_calling

; code addresses trapped by profiler
PUBLIC $$_ovl_prof_call, $$_ovl_prof_return
PUBLIC $$_ovl_load_overlay

;*****************************
;* STACK                     *
;*****************************

$$_ovl_mgr_stack    SEGMENT PARA PRIVATE '$$_OVL_MANAGER'
    DW  160 DUP (?)
$$_ovl_mgr_stack_end    =   $
$$_ovl_mgr_stack    ENDS

;*****************************
;* DATA                      *
;*****************************

$$_ovl_data SEGMENT PARA PRIVATE '$$_OVL_MANAGER'

; constant data
dos_err_text    DB  CR,LF,'FATAL DOS Error, Code '
ovl_err_text    DB  CR,LF,'FATAL Overlay Manager Error, Code '
code_text       DB  '00h',CR,LF
pathtext        DB  'PATH='

pointer_start	EQU	$
_ovlmgr_overlay_filename    EQU $
$$_ovl_filename DB  128 DUP (0) ; overlay file name
				DB 	13,5,4,'V','1'	; signature bytes

				DW	(OFFSET $$_ovl_pool)-pointer_start
				DW	(OFFSET $$_ovl_mem_alloc_flag)-pointer_start
				DW	(OFFSET $$_ovl_ohp)-pointer_start
				DW	(OFFSET $$_ohp_size)-pointer_start
				DW	(OFFSET $$_ohp_flag)-pointer_start
				DW	(OFFSET $$_ems3_flag)-pointer_start
				DW	(OFFSET $$_ovl_oht)-pointer_start
				DW	(OFFSET $$_oht_size)-pointer_start
				DW	(OFFSET $$_oht_flag)-pointer_start
				DW	(OFFSET $$_ovl_umb)-pointer_start
				DW	(OFFSET $$_ovl_stack)-pointer_start
				DW	(OFFSET $$_ovl_max_load)-pointer_start
				DW	(OFFSET $$_ovl_orp)-pointer_start
				DW	(OFFSET $$_ovl_ort)-pointer_start
				DW	(OFFSET $$_ovl_filename)-pointer_start
				DW	(OFFSET $$_ovl_ems_pool_flag)-pointer_start
				DW	(OFFSET $$_ovl_ox_evar)-pointer_start

EVEN

; modifiable data

; values set by linker
;***_ovlmgr_overlay_filename    EQU $
;***$$_ovl_filename DB  128 DUP (0) ; overlay file name
$$_min_prog_size    DW  ?   ; minimum program size in paragraphs
$$_ovl_ind_call_table DW  ? ; segment of indirect call table
$$_ovl_seg_call_table DW  ? ; segment of segment fixup jump table

$$_ovl_pool DD  147456      ; overlay pool or free memory size
$$_ovl_lru_counter  DD  1   ; least recent used counter for overlay swap priority

$$_ovl_max_load DW  96      ; maximum number of loaded overlays
$$_ovl_stack    DW  2048    ; overlay stack size
$$_ovl_min_alloc    DW  1024    ; size in paragraphs of largest overlay

$$_ovl_mem_alloc_flag   DB  0   ; nonzero if $$_ovl_pool holds overlay pool size, zero if free memory
EVEN                        ; re-align to word boundary

; computed by overlay initialization code
$$_ovl_array_start  DW  ?   ; start of overlay load array
$$_ovl_amt_alloc    DW  ?   ; amount of memory allocated to overlays in paragraphs
$$_ovl_amt_512      DW  ?   ; amount of memory allocated to overlays in 512 blocks
$$_ovl_filepos      DW  ?   ; segment of table of overlay file positions in .OVL file

; used and modified by overlay manager code
$$_ovl_tfile        DB  13 DUP (0)  ; temporary storage for file name if not found in current directory
EVEN
$$_ovl_sysinfo_off  DD  ?   ; offset in OVL file to system information
$$_ovl_file_offset  DD  ?   ; offset in OVL to current overlay file
$$_ovl_loaded_count DW  0   ; count of loaded overlays
$$_ovl_file_handle  DW  0   ; overlay file handle

$$_ovl_filepos_size DW  ?   ; size of file position table, # of entries, changed to # of bytes
$$_ovl_total_count  DW  ?   ; total number of overlays

$$_ovl_temp_buff    DW  3 DUP (?)   ; temporary buffer when reading info for internal overlays

$$_ovl_file_header  EQU $   ; first 5 words (10 bytes) of overlay file header go here
    ofh_ovl_number  DW  ?   ; number of overlay
    ofh_ovl_offset  DW  ?   ; Offset to overlay code
    ofh_ovl_size    DW  ?   ; Overlay code size in paragraphs
    ofh_near_vector DW  ?   ; Offset to near vector routine in segment
    ofh_reloc_count DW  ?   ; Relocation entry count

$$_ovl_rel_pos  DD  ?       ; relocation table file position
$$_ovl_load_seg DW  ?       ; current overlay load segment

$$_ovl_swap_count   DW  ?   ; number of overlays swapped out by proposed overlay load
;***$$_ovl_swap_active  DW  ?   ; number of active overlays swapped out by proposed overlay load
$$_ovl_best_load    DW  ?   ; the best of the proposed overlay loads, based on LRU and swap count
$$_ovl_best_count   DW  ?   ; best of proposed overlay load, number of overlays swapped out
;***$$_ovl_best_active  DW  ?   ; best of proposed overlay load, number of active overlays swapped

$$_ovl_highest_lru  DD  ?   ; highest LRU counter of overlays swapped out by proposed overlay load
$$_ovl_hi_best_lru  DD  ?   ; best of proposed overlay load, highest LRU counter
;***$$_ovl_hi_act_lru   DD  ?   ; highest LRU counter of active overlays swapped (for /r option)
;***$$_ovl_hi_best_act  DD  ?   ; best of proposed overlay load, highest LRU counter of actives swapped (/r option)

$$_ovl_activity     DW  ?   ; activity level of overlay (0 if not active)

swapped_lru_high    DW  ?   ; swapped out overlay LRU high word
swapped_lru_low     DW  ?   ; swapped out overlay LRU low word

ovl_act_size        DW  ?   ; proposed overlay load size
ovl_best_size       DW  ?   ; best of proposed overlay load sizes
check_array_size    DW  ?   ; size of check array in bytes
dos5_umb_flag	DB	0		; nonzero if DOS 5 UMB's used for overlay pool

ems_trans_block	LABEL	BYTE
; relocation table of overlay file (1K max each time, 512 entries)
$$_ovl_rel_table    EQU $
    DB  1024 DUP (?)
    DB  1024 DUP (?)

; modifiable
$$_ovl_ox_evar  DB  32 DUP (0)  ; e-var for /ox option
$$_ovl_ems_pool_flag    DB  0   ; nonzero if overlay pool uses EMS
$$_ovl_ort  DB  0           ; nonzero if /ort setting used
$$_ovl_orp  DB  0           ; nonzero if /orp setting used
$$_ovl_ohp	DB	0			; nonzero if /ohp setting
$$_ovl_oht	DB	0			; nonzero if /oht setting
$$_ohp_flag	DB	0			; nonzero if /ohp allocate to amount flag set
$$_oht_flag	DB	0			; nonzero if /oht allocate to amount flag set
$$_ovl_umb	DB	0			; nonzero if /ou setting used
$$_ems3_flag	DB	0		; nonzero if /ohp3 EMS 3.0 compatibility setting used

info_from_emsxms_flag	DB	?	; nonzero if current overlay stashed in EMS/XMS, 80h-XMS, 40h-EMS
emsxms_stash_flag	DB	?	; nonzero if stashing current overlay to EMS/XMS, 80h-XMs,40h-EMS
EVEN
$$_ohp_size	DW	0     		; /ohp size in K
$$_oht_size	DW	0			; /oht size in K
$$_ovl_file_size	DW	0	; overlay file size in K
$$_ovl_ox_handle	DW  ?   ; handle of allocated EMS for /ox
$$_ovl_ems_base     DW  ?   ; page frame base of EMS
emmname     DB  'EMMXXXX0',0
xe_table    DW  256 DUP (0) ; XMS/EMS active overlay stash table
orport_handle   DW  ?       ; handle for XMS/EMS active overlay stashing
ohpoht_handle   DW  ?       ; handle for XMS/EMS overlay file stashing
xms_addr    DD  ?           ; XMS entry point address
umb_avail	DW	?			; size of largest available UMB block in paras
emsxms_free	DD	?			; bytes of EMS/XMS free for file stashing
emsxms_offset	DD	0		; offset to current stash position in EMS/XMS

EMB_STRUC   STRUC
    es_len  DD  ?           ; length of block in bytes
    es_src_handle   DW  ?   ; source EMB handle
    es_src_offset   DD  ?   ; source offset
    es_dest_handle  DW  ?   ; destination EMB handle
    es_dest_offset  DD  ?   ; destination offset
EMB_STRUC   ENDS
emb EMB_STRUC   <>

OHTBUFF_STRUC	STRUC
    os_len  DD  ?           ; length of block in bytes
    os_src_handle   DW  ?   ; source EMB handle
    os_src_offset   DD  ?   ; source offset
    os_dest_handle  DW  ?   ; destination EMB handle
    os_dest_offset  DD  ?   ; destination offset
OHTBUFF_STRUC	ENDS
ohtbuff	OHTBUFF_STRUC	<>

XBUFF_STRUC STRUC
    xs_len  DD  ?           ; length of block in bytes
    xs_stype    DB  ?       ; source type (0==conventional, 1==expanded)
    xs_shandle  DW  ?       ; source handle
    xs_soffset  DW  ?       ; source offset
    xs_ssegpage DW  ?       ; source memory segment or logical page number
    xs_dtype    DB  ?       ; destination type (0==conventional, 1==expanded)
    xs_dhandle  DW  ?       ; destination handle
    xs_doffset  DW  ?       ; destination offset
    xs_dsegpage DW  ?       ; destination memory segment or logical page number
XBUFF_STRUC	ENDS
xbuff   XBUFF_STRUC <>

OHPBUFF_STRUC	STRUC
    os_len  DD  ?           ; length of block in bytes
    os_stype    DB  ?       ; source type (0==conventional, 1==expanded)
    os_shandle  DW  ?       ; source handle
    os_soffset  DW  ?       ; source offset
    os_ssegpage DW  ?       ; source memory segment or logical page number
    os_dtype    DB  ?       ; destination type (0==conventional, 1==expanded)
    os_dhandle  DW  ?       ; destination handle
    os_doffset  DW  ?       ; destination offset
    os_dsegpage DW  ?       ; destination memory segment or logical page number
OHPBUFF_STRUC	ENDS
ohpbuff	OHPBUFF_STRUC	<>

; profiler performance data
$$_ovl_load_count   DD  0   ; number of overlay loads
$$_ovl_reload_count DD  0   ; number of overlay reloads

check_array DW  ?           ; address of array to check for loaded overlays

$$_ovl_data ENDS

GROUP DGROUP
COMM    NEAR _ovlmgr_error_vector:DWORD ; installed error handler

;*****************************
;* CODE                      *
;*****************************

$$_ovl_code SEGMENT PARA PRIVATE '$$_OVL_MANAGER'

    ASSUME  cs:$$_ovl_code,ds:$$_ovl_data,ss:$$_ovl_mgr_stack

;*****************************
;* CODE SEGMENT DATA         *
;*****************************

; from procedure $$_ovl_init
prog_sp_store   DW  ?
prog_ss_store   DW  ?
$$_ovl_stack_ptr    DW  ?   ; current stack pointer for overlay manager
$$_ovl_stack_start  DW  ?   ; start of stack block
$$_ovl_mgr_stack_ptr    DW  ?   ; current stack pointer for internal overlay manager stack
$$_ovl_load_start   DW  ?   ; start of overlay load area
$$_ovl_load_end     DW  ?   ; end of overlay load area

; from procedure $$_ovl_mgr_entry
$$_ovl_source_id LABEL word
source_id   DW  ?
$$_ovl_destination_id LABEL word
destination_id  DW  ?
$$_ovl_call_target LABEL dword
call_offset DW  ?
call_segment    DW  ?
ax_store    DW  ?
bx_store    DW  ?
cx_store    DW  ?
dx_store    DW  ?
si_store    DW  ?
ds_store    DW  ?
flag_store  DW  ?
$$_ovl_jump_target LABEL dword
jump_offset DW  ?
jump_segment    DW  ?
nearv_offset    DW  ?

prog_psp    DW  ?           ; beginning PSP value

$$_main_entry   DD  ?       ; main entry point into program, needs to be fixed up
$$_ovl_lookup_table DW  ?   ; segment of lookup table, needs to be fixed up

$$_ovl_stackcount  DW   ?   ; count of active overlays swapped out to be reloaded upon return
$$_ovl_reload_flag  DB  ?   ; nonzero if must reload active overlays that are swapped out
EVEN
$$_ovl_is_calling   LABEL byte
is_calling_ovl      DB  ?   ; nonzero if calling overlay, zero if returning from overlay
EVEN
$$_ovl_clarion_flag DB  ?   ; nonzero if clarion overlay mode in effect
EVEN
$$_ovl_internal_flag    DB  ?   ; nonzero if internal overlays
EVEN
$$_ovl_in_manager   DB  0   ; nonzero if in overlay manager (for profiler)
EVEN
psp_terminate   DD  ?       ; termination vector from PSP
bp_store    DW  ?
last_access_slot    DW  0   ; array slot address of last accessed overlay

;*****************************
;* $$_OVL_INIT               *
;*****************************

; initial code performed when executable linked with overlays
; destroys ax,bx,cx,dx,di,si

$$_ovl_init PROC    FAR

$$_ovl_init_entry:
    mov ax,ss
    mov bx,sp               ; get stack entry values
    mov cs:prog_ss_store,ax
    mov cs:prog_sp_store,bx ; save them

    mov bx,OFFSET $$_ovl_mgr_stack_end
    mov ax,SEG $$_ovl_mgr_stack_end
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    push    ds              ; save data segment entry value
    mov ax,$$_ovl_data
    mov ds,ax               ; ds -> overlay manager data
    mov ax,es               ; get PSP value
    mov cs:prog_psp,ax      ; save to memory variable
    add ax,10h              ; bump to program load segment
    add WORD PTR cs:$$_main_entry+2,ax  ; fixup entry address
    add cs:$$_ovl_lookup_table,ax   ; fixup lookup table address

; check for existence of .OVL file, either in current directory or environment PATH
err_retry1:
    mov dx,OFFSET $$_ovl_filename   ; ds:dx -> .OVL file spec
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    jnc init_1              ; file found okay
    cmp ax,2                ; check for file not found error
    je  scan_path           ; yes, scan the path for .OVL file
    cmp ax,3                ; check if path not found or file doesn't exist error
    je  scan_path           ; yes
    cmp ax,5                ; check for access denied (not in current directory)
    je  scan_path

    call    NEAR PTR $$_ovl_dos_error   ; no, dos error
    jmp SHORT err_retry1    ; try again

; .OVL file not found in current directory, search PATH environment string
scan_path:
    call    $$_ovl_find_ovl_file

init_1:
; adjust the memory allocated to the program, using the top of memory
; pointer at PSP:2 and the PSP value
    mov ax,es:[2]           ; get top of allocated memory
    mov bx,es
    sub ax,bx               ; get difference between top of memory and PSP
    mov dx,ax               ; dx holds original memory allocation amount
    sub ax,10h              ; subtract off PSP to get all memory allocated to program
    sub ax,$$_min_prog_size ; get memory in excess of amount need for program in ax

    mov si,$$_ovl_stack     ; get size of stack to allocate
    shr si,1                ; convert to paragraphs
    shr si,1
    shr si,1
    shr si,1
    cmp ax,si               ; make sure excess memory is equal to or greater than stack allocation
    jae init_1a             ; yes

; out of memory error, not enough memory for overlay setup
init_memerr:
    mov ax,8                ; force out of memory error
    call    NEAR PTR $$_ovl_dos_error   ; no return

init_1a:
    sub ax,si               ; ax holds free memory paragraphs after stack reduction
    mov di,$$_ovl_max_load  ; get size of array to load (already in paragraphs)
    sub ax,di               ; ax holds free memory paragraphs after array reduction
    jc  init_memerr         ; out of memory

    call    get_ovl_total   ; open overlay file, get total number of overlays, compute memory allocation

; allow 10h paragraphs play for allocation adjustment overhead (at least 4 needed)
    cmp ax,10h

to_init_memerr:
    jb  init_memerr         ; not enough memory
    sub ax,10h              ; back off overhead adjustment

    mov cx,WORD PTR $$_ovl_pool+1 ; get 512 byte blocks*2
    add cx,15               ; round up
    shr cx,1
    shr cx,1
    shr cx,1
    shr cx,1                ; get paragraph count
    sub ax,cx               ; subtract off 2 bytes/512 byte page
    jc  init_memerr         ; out of memory

; check if UMB specified, if so, see if XMS available get largest UMB
    cmp $$_ovl_umb,0		; see if UMB specified
    je  init_chkems			; no
	call	umb_check_setup	; see if UMB is available
	jc	init_chkems			; no

; check if EMS specified, if so, see if available, if so, use it for overlay pool
init_chkems:
    cmp $$_ovl_ems_pool_flag,0  ; see if ems specified
    je  init_xms            ; no
    call    ems_check_setup
	call	ovl_ems_poolcheck
    cmp $$_ovl_ems_pool_flag,0  ; see if ems available
    je  init_xms            ; no

    mov ax,126
    mov $$_ovl_amt_512,ax   ; save 512 blocks allocated
    mov ax,0fc0h            ; 4032 paragraphs (63K)
    mov $$_ovl_amt_alloc,ax ; save amount of memory allocated
    mov ax,$$_ovl_ems_base  ; get EMS base

    mov cs:$$_ovl_load_start,ax ; save segment of overlay load area
    add ax,$$_ovl_amt_alloc ; calculate paragraph just above overlay load area
    mov cs:$$_ovl_load_end,ax   ; save to memory variable

init_xms:
    cmp $$_ovl_ort,0        ; see if XMS active overlay swapout specified
    je  init_lim40          ; no
    call    get_xms_for_ort ; try and grab some XMS for /ort swapping
    cmp $$_ovl_ort,0        ; see if XMS available
    jne init_1b             ; yes

init_lim40:
    cmp $$_ovl_orp,0        ; see if LIM 4.0 EMS active overlay swapout specified
    je  init_1b             ; no
    call    get_ems_for_orp ; try and grab some EMS for /orp swapping

init_1b:
    cmp $$_ovl_ems_pool_flag,0  ; see if ems page frame used for overlay pool
    je  init_notox          ; no

init_zeropool:
    xor cx,cx               ; zero overlay pool size to allocate
	mov	ax,$$_ovl_amt_512	; 512 byte chunks allocated in ax
    jmp NEAR PTR init_compalloc

; ax == free memory prior to overlay pool allocation after all other allocations
init_notox:
    mov cx,WORD PTR $$_ovl_pool ; get low word of pool size
    mov bx,WORD PTR $$_ovl_pool+2   ; get high word of pool size
    shr bx,1
    rcr cx,1                ; convert to paragraphs in cx
    shr bx,1
    rcr cx,1
    shr bx,1
    rcr cx,1
    shr bx,1
    rcr cx,1                ; cx holds overlay pool size in paragraphs

    cmp $$_ovl_mem_alloc_flag,0 ; see which way to allocate memory
    jne init_use_pool           ; allocate ovl_pool for overlay pool

; when using UMB overlay pool allocation for "minus" overlay pool (leave free)
; choose an UMB size of 3*largest overlay
	cmp	$$_ovl_umb,0		; check if using UMB for overlay pool
	je	init_freealloc		; no
	mov	bx,$$_ovl_min_alloc
	add	bx,bx
	add	bx,$$_ovl_min_alloc	; bx == 3*largest overlay
	cmp	umb_avail,bx		; see if enough UMB space for 3x pool
	jb	init_freealloc		; no
	mov	cx,bx				; get allocation size in cx for umb_pool_alloc proc
	call	umb_pool_alloc	; allocate UMB for overlay pool
	jmp	SHORT init_zeropool

; allocate all memory except amount in ovl_pool
init_freealloc:
	mov	$$_ovl_umb,0		; fail using UMB for overlay pool
    cmp ax,cx               ; see if enough memory for to leave ovl_pool amount free
    jae init_1c

to_init_memerr2:
    jmp NEAR PTR init_memerr    ; no, out of memory

init_1c:
    sub ax,cx               ; ax holds amount of paragraphs for overlay load area
    jmp SHORT init_allocate ; bypass ovl_pool memory calculation

; allocate up to amount of memory in ovl_pool
init_use_pool:
	cmp	$$_ovl_umb,0		; check if using UMB for overlay pool
	je	init_noumb			; no
	cmp	umb_avail,cx		; see if enough UMB space for overlay pool
	jb	init_noumb			; no
	call	umb_pool_alloc	; allocate UMB for overlay pool, using size in cx
	jmp	SHORT init_zeropool

init_noumb:
	mov	$$_ovl_umb,0		; fail using UMB for overlay pool
    cmp ax,cx               ; see if free memory is less than or equal to overlay pool size
    jbe init_allocate       ; yes, allocate all of it
    mov ax,cx               ; only allocate amount of memory up to maximum

; ax holds, in paragraphs, the amount of memory to allocate for the memory pool
; si holds the amount of memory to allocate for the stack
; di holds the amount of memory to allocate for the loaded overlay array
; dx holds original memory allocation amount
; $$_ovl_filepos_size holds the amount of memory to allocate for file position table
; also remember to subtract off 10h paragraphs for memory block allocation overhead
init_allocate:
    cmp ax,$$_ovl_min_alloc ; see if enough memory for largest overlay
    jb  to_init_memerr2     ; no, force out of memory error

init_alloc2:
    mov cx,ax               ; save memory pool paragraph allocation in cx
    dec ax                  ; make relative zero
    shr ax,1                ; convert to 512 blocks, /2
    shr ax,1
    shr ax,1                ; /8
    shr ax,1                ; /16
    shr ax,1                ; /32
    inc ax                  ; adjust for beginning block
    mov $$_ovl_amt_512,ax   ; save 512 blocks allocated 

init_compalloc:
    sub dx,cx				; subtract off memory pool
    sub dx,si				; stack allocation
    sub dx,di				; overlay loaded array
    sub dx,$$_ovl_filepos_size	; file position table
	add	ax,ax				; check array size is 512 bytes*2 (word per)

; 9/2/92
    mov check_array_size,ax ; keep it
	add	ax,15				; round up to next para
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1				; convert to paras

	sub	dx,ax				; check array allocation
    sub dx,10h              ; subtract off play, dx holds new allocation amount
    mov bx,dx               ; get allocation amount in bx
    mov ah,4ah              ; modify memory allocation
    int 21h                 ; es already -> PSP for memory block to be modified
    jnc init_get_free       ; no errors

init_to_doserr:
    call NEAR PTR $$_ovl_dos_error  ; no return

to_main_entry:
    jmp DWORD PTR cs:$$_main_entry  ; transfer control to program

; allocate from newly created free space for overlay setup
init_get_free:
    mov ax,es
    add ax,dx               ; ax holds new top of memory for program
    mov es:[2],ax           ; save back to PSP variable

	mov	al,$$_ovl_ems_pool_flag
	or	al,$$_ovl_umb		; see if EMS or UMB overlay pool used
    jne init_2              ; yes, bypass allocation and setup

    mov bx,cx               ; bx holds paragraphs to allocate for overlay load area

    mov $$_ovl_amt_alloc,bx ; save amount of memory allocated
    mov ah,48h              ; allocate memory
    int 21h
    jc  init_to_doserr      ; error occurred

    mov cs:$$_ovl_load_start,ax ; save segment of overlay load area
    add ax,$$_ovl_amt_alloc ; calculate paragraph just above overlay load area
    mov cs:$$_ovl_load_end,ax   ; save to memory variable

init_2:
    call    read_ovl_filepos    ; allocate space for and read table into memory

; compute count of 512*2 chunks of overlay pool
	mov	bx,$$_ovl_amt_512	; count of 512 chunks
	add	bx,bx				; *2

; 9/2/92
	add	bx,15				; round up to next para
	shr	bx,1
	shr	bx,1
	shr	bx,1
	shr	bx,1				; convert to paras

    mov ah,48h              ; allocate memory
    int 21h
    jc  init_to_doserr      ; error occurred
    mov check_array,ax      ; save check array address

    mov bx,si               ; bx holds paragraphs to allocate for stack
    mov ah,48h              ; allocate memory
    int 21h
    jc  init_to_doserr      ; error occurred

    mov cs:$$_ovl_stack_start,ax    ; save segment of stack
    mov ax,$$_ovl_stack     ; get stack size in bytes
    mov cs:$$_ovl_stack_ptr,ax  ; init stack pointer to top of stack

    mov bx,di               ; bx holds paragraphs to allocate for overlay loaded array
    mov ah,48h              ; allocate memory
    int 21h
    jc  init_to_doserr      ; error occurred

    mov $$_ovl_array_start,ax   ; save segment of overlay loaded array
    mov cx,di               ; cx holds number of array elements
	push	ds				; save -> overlay manager data
    mov ds,ax               ; ds -> overlay array start
    xor ax,ax
    mov bx,ax               ; bx offsets into array area

array_zloop:
    mov [bx],ax             ; zero the array element identifiers
    mov [bx+4],ax           ; zero activity level
    add bx,16               ; move to next element
    loop    array_zloop     ; loop until all ids are zeroed

; save old termination address in PSP, set to new
    mov ax,es:[10]
    mov WORD PTR cs:psp_terminate,ax
    mov ax,es:[12]
    mov WORD PTR cs:psp_terminate+2,ax
    mov ax,OFFSET new_terminate
    mov es:[10],ax
    mov ax,SEG new_terminate
    mov es:[12],ax

; allocate space for /oht or /ohp overlay file swapouts if requested
    pop ds					; restore ds -> overlay manager data
	cmp	$$_ovl_oht,0		; see if /oht switch set
	je	init_chkohp			; no
    call    get_xms_for_oht ; attempt XMS allocation for /oht swapping
    cmp $$_ovl_oht,0        ; see if XMS was allocated
    jne init_out            ; yes

init_chkohp:
	cmp	$$_ovl_ohp,0		; see if /ohp switch set
	je	init_out			; no
    call    get_ems_for_ohp ; attempt EMS allocation for /ohp swapping

init_out:
    pop ds                  ; restore data segment entry value
    mov ax,cs:prog_ss_store
    mov bx,cs:prog_sp_store ; get stack entry values
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on
	jmp	NEAR PTR to_main_entry
$$_ovl_init ENDP

;*****************************
;* GET_OVL_TOTAL             *
;*****************************

; open overlay file, read total number of overlays from first word
; upon entry ax holds number of free paragraphs
; destroys bx,cx
; modifies ax

get_ovl_total   PROC    NEAR
    push    dx              ; save critical register
    push    ax

err_retry2:
    mov dx,OFFSET $$_ovl_filename   ; ds:dx -> file spec
    mov ax,3d40h            ; open, read access
    int 21h 
    jnc ovl_file_open       ; no errors

    call NEAR PTR $$_ovl_dos_error
    jmp SHORT err_retry2

ovl_file_open:
    mov $$_ovl_file_handle,ax   ; save overlay file handle
    cmp cs:$$_ovl_internal_flag,0   ; see if internal overlays
    je  got_1               ; no

err_retry3:
    mov bx,$$_ovl_file_handle
    mov cx,6                ; read first 6 bytes of EXE file
    mov dx,OFFSET $$_ovl_temp_buff  ; into the temporary buffer
    mov ah,3fh              ; read file
    int 21h
    jnc got_read1           ; no error

    call NEAR PTR $$_ovl_dos_error
    jmp SHORT err_retry3

got_read1:
    mov cx,WORD PTR $$_ovl_temp_buff+4  ; get size of file in 512-byte pages
    mov ax,WORD PTR $$_ovl_temp_buff+2  ; get size of file modulo 512
    or  ax,ax               ; see if any remainder
    je  get_int_1           ; no
    dec cx                  ; drop count of pages

get_int_1:
    mov dh,cl
    mov cl,ch
    xor dl,dl
    mov ch,dl               ; effectively multiply page value by 256 in cx:dx
    shl dx,1
    rcl cx,1                ; get page value*512 in cx:dx
    add dx,ax               ; add in page remainder
    adc cx,0                ; carry to high word
    mov WORD PTR $$_ovl_temp_buff,dx    ; save low word of file end offset
    mov WORD PTR $$_ovl_temp_buff+2,cx  ; save high word of file end offset

err_retry9:
    mov bx,$$_ovl_file_handle
    mov cx,WORD PTR $$_ovl_temp_buff+2
    mov dx,WORD PTR $$_ovl_temp_buff
    mov ax,4200h            ; move file pointer, absolute byte offset
    int 21h

got_1:
    mov cx,2                ; read one word (total number of overlays)
    mov dx,OFFSET $$_ovl_total_count    ; read bytes into memory variable
    mov bx,$$_ovl_file_handle
    mov ah,3fh              ; read file
    int 21h
    jnc got_read2           ; no error

    call NEAR PTR $$_ovl_dos_error
    jmp SHORT err_retry9

got_read2:
    mov cx,$$_ovl_total_count   ; cx holds number of entries in table
    shl cx,1
    shl cx,1                ; cx holds total bytes (doubleword entries) of overlay file position table
    mov $$_ovl_total_count,cx   ; save total number of bytes
    add cx,15               ; round up to next paragraph
    shr cx,1                ; convert table size to paragraphs
    shr cx,1                ; /4
    shr cx,1                ; /8
    shr cx,1                ; /16, cx holds table size in paragraphs
    mov $$_ovl_filepos_size,cx  ; save size
    pop ax                  ; ax holds free paragraphs

    cmp ax,cx               ; make sure excess memory is equal to or greater than table allocation
    jae got_2               ; yes

    mov ax,8                ; no, force out of memory error
    call NEAR PTR $$_ovl_dos_error  ; no return

got_2:
    sub ax,cx               ; subtract off file position table allocation
    pop dx                  ; restore critical register
    ret
get_ovl_total   ENDP

;*****************************
;* READ_OVL_FILEPOS          *
;*****************************

; allocate memory for overlay file positions table, then read table into memory
; if internal overlays adjust table entries past EXE file
; destroys ax,bx,cx,dx

read_ovl_filepos    PROC    NEAR
    mov bx,$$_ovl_filepos_size  ; bx holds paragraphs to allocate for overlay file position table
    mov ah,48h              ; allocate memory
    int 21h
    jnc rof_2               ; no error
    call NEAR PTR $$_ovl_dos_error  ; no return

rof_2:
    mov $$_ovl_filepos,ax   ; save segment of overlay load area

err_retry4:
    mov cx,$$_ovl_total_count   ; read number of bytes in table
    mov bx,$$_ovl_file_handle
    xor dx,dx
    push    ds
    mov ds,$$_ovl_filepos   ; ds:dx -> load area for overlay file position table
    mov ah,3fh              ; read file
    int 21h
    pop ds                  ; restore ds
    jnc rof_open            ; no error

    call NEAR PTR $$_ovl_dos_error  ; no return 
    jmp SHORT err_retry4

rof_open:
    cmp cs:$$_ovl_internal_flag,0   ; see if internal overlays
    je  rof_ret             ; no

; make adjustments to overlay file position table for internal overlay
    mov cx,$$_ovl_total_count   ; get total number of bytes in table
    shr cx,1
    shr cx,1                ; convert back to number of overlays
    mov ax,WORD PTR $$_ovl_temp_buff    ; get low word of adjustment
    mov dx,WORD PTR $$_ovl_temp_buff+2  ; get high word of adjustment
    mov bx,$$_ovl_filepos   ; get segment of overlay load area
    push    ds              ; save ds-> overlay manager data
    mov ds,bx
    xor bx,bx               ; ds:bx -> overlay file position table

adjust_loop:
    add [bx],ax             ; add in low word of adjustment
    adc [bx+2],dx           ; ad in high word of adjustment plus carry
    add bx,4                ; move to next entry in overlay file position table
    loop    adjust_loop     ; loop until all adjustments done

    pop ds                  ; restore ds -> overlay manager data

rof_ret:
    ret
read_ovl_filepos    ENDP

;*****************************
;* $$_OVL_FIND_OVL_FILE      *
;*****************************

; find .OVL file using PATH environment string
; prior to using PATH, if using DOS 3.x+, check the EXE's path that is found
; after the local environment block (argv[0])
; es -> PSP upon entry
; dx -> file name upon entry
; destroys ax,bx,cx,dx,si,di

$$_ovl_find_ovl_file    PROC    NEAR
    push    es              ; save critical register

; get overlay file name in $$_ovl_tfile from $$_ovl_filename, so can append path to it
    mov di,OFFSET $$_ovl_tfile
    mov si,OFFSET $$_ovl_filename
    push    ds
    pop es                  ; es -> overlay manager data

ovl_findloop:
    movsb                   ; move a filename char
    cmp BYTE PTR [si-1],0   ; see if all chars transferred
    jne ovl_findloop        ; no

    pop es                  ; restore es -> PSP
    push    es              ; put PSP value back on stack
    mov ax,es:[2ch]         ; get environment segment from offset 2ch in PSP
    mov es,ax               ; es -> environment segment

    mov ah,30h              ; get MS-DOS version number
    int 21h
    cmp al,2                ; should be 3.x or above
    jbe dos_2x_entry        ; no, can't check path of EXE file after environment blockk

; search for end of environment block
    mov bx,ds
	push	es
	pop	ds
    mov es,bx               ; es -> warplink data
    xor si,si               ; ds:si -> start of environment

end_loop:
    lodsb                   ; get environment char
    or  al,[si]             ; merge in next char
    jne end_loop            ; not at end of environment block

    add si,3                ; bump si -> start of exe file path
    mov di,si               ; save start pointer

path_loop:
    lodsb                   ; get char of file path
    or  al,al               ; see if at end
    je  calc_path           ; yes
    cmp al,'\'              ; see if directory indicator
    jne path_loop           ; no
    mov bx,si               ; save -> to char past directory
    jmp SHORT path_loop     ; get next char

calc_path:
    mov si,di               ; ds:si -> start  of file path
    mov di,dx               ; es:di -> program name with path prefix slot

calc_loop:
    movsb                   ; transfer a char
    cmp si,bx               ; see if at end of path
    jne calc_loop           ; no

    mov bx,ds               ; save -> environment block
	push	es
	pop	ds					; ds -> machlink data
    mov si,OFFSET $$_ovl_tfile  ; append program name to end of path prefix

append_loop:
    movsb                   ; transfer a char
    cmp BYTE PTR [si-1],0   ; see if null terminator transferred
    jne append_loop         ; no

; $$_ovl_filename holds EXE file path and OVL file name
err_retry5:
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    jc  check_errors        ; program not found, make sure non-DOS error
    jmp NEAR PTR find_ovl_ret   ; program found, return with $$_ovl_filename properly set up

check_errors:
    cmp ax,2                ; file not found
    je  try_path
    cmp ax,3                ; path not found
    je  try_path
    cmp ax,5                ; access denied error
    je  try_path

find_retry:
    call    NEAR PTR $$_ovl_dos_error
    mov dx,OFFSET $$_ovl_filename   ; ds:dx -> filename for retry
    jmp SHORT err_retry5

; file wasn't found in EXE's path, try PATH environment variable
try_path:
    mov es,bx               ; es -> environment block

dos_2x_entry:
    mov bx,OFFSET pathtext  ; bx holds target string address for compares
    xor si,si               ; starting location for target string check

ovl_find_path:
    xor di,di               ; offset into target string

ovl_loop2:
    mov al,es:[si]          ; get byte from environment string
    inc si                  ; point to next char in environment
    cmp al,[bx+di]          ; does environment char match PATH string char
    je  ovl_byte_match      ; yes, try next location
    or  al,es:[si]          ; two zero values in a row mean the end of the environment
    jne ovl_find_path       ; not the end of the environment

ovl_not_found:
    mov ax,2                ; force file not found error
    jmp SHORT find_retry    ; transfer to error handler

; check that PATH is not part of another environment string
ovl_byte_match:
    or  di,di               ; di is zero if first char is matched
    jne ovl_2               ; not first char, test already done
    cmp si,1                ; si equals one if PATH is first string in environment block
    je  ovl_2               ; no previous environment string
    cmp BYTE PTR es:[si-2],0    ; check if char before PATH was nonzero
    jne ovl_find_path       ; yes, LIB is a subset of another string, keep looking

ovl_2:
    inc di                  ; a match, move to next byte of target string
    cmp di,5                ; check if all bytes matched
    jb  ovl_loop2           ; not yet, keep comparing

ovl_path_found:
    mov bx,dx               ; bx-> program name with path prefix slot

ovl_3:
    xor di,di               ; offset into path prefix

ovl_4:
    mov al,es:[si]          ; get path character
    cmp al,';'              ; check if path terminator character
    je  ovl_prefix_complete ; yes, check file's existence with the current path prefix
    cmp al,' '              ; anything less than a space is also a terminator character
    jb  ovl_prefix_complete
    mov [bx+di],al          ; save path character
    inc di                  ; move to next name slot
    inc si                  ; move to next byte location
    jmp SHORT ovl_4         ; loop for next character

ovl_prefix_complete:
    push    si              ; save si -> current environment position
    mov si,OFFSET $$_ovl_tfile  ; append program name to end of path prefix
    cmp BYTE PTR [bx+di-1],'\'  ; check for backslash already in place
    je  ovl_5
    mov BYTE PTR [bx+di],'\'    ;put a backslash between the path and program name
    inc di                  ; point di past backslash

ovl_5:
    mov al,[si]             ; get program name character
    mov [bx+di],al          ; transfer program name
    or  al,al               ; stop transfer after first zero byte transfer
    je  ovl_search          ; now see if file exists in this path
    inc si                  ; move to next name character slot
    inc di
    jmp SHORT ovl_5         ; loop for next character

ovl_search:
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    jnc ovl_prog_found      ; found the program
    cmp ax,2                ; file not found
    je  ovl_6
    cmp ax,3                ; path not found
    je  ovl_6
    cmp ax,5                ; access denied
    je  ovl_6
    jmp SHORT find_retry    ; transfer to error handler

ovl_6:
    pop si                  ; restore si -> environment block position
    cmp BYTE PTR es:[si],0  ; check last terminator
    je  ovl_not_found       ; if zero then no more path prefixes to try
    cmp BYTE PTR es:[si+1],0    ; check character following last terminator
    je  ovl_not_found       ; if zero then no more path prefixes to try
    jmp SHORT ovl_search_again  ; not the final PATH path, keep trying

ovl_search_again:
    inc si                  ; point to first character after last terminator
    jmp SHORT ovl_3         ; try next path

ovl_prog_found:
    pop di                  ; trash the value stored in the stack

find_ovl_ret:
    pop es                  ; restore critical register
    ret
$$_ovl_find_ovl_file    ENDP

;*****************************
;* $$_OVL_SEG_CALL_ENTRY     *
;*****************************

; Call to overlaid segment, through target segment fixup
; must assume that displacement is zero, valid assumption for Clipper
; otherwise, don't allow multiple code segments in an overlay module
; to avoid the possiblility of nonzero target displacments for overlaid segments
; Check if segment shows this is call from root or overlay
; if overlay walk the overlay loaded array checking the calling routine's
; segment against loaded overlays' segments to find the proper source id for bx

$$_ovl_seg_call_entry PROC	FAR
    mov cs:$$_ovl_in_manager, 1  ; flag used by profiler
    mov cs:ax_store,ax      ; save ax,bx,cx,dx value
    mov cs:bx_store,bx
    mov cs:cx_store,cx
    mov cs:dx_store,dx
    pushf                   ; save the flag register
    pop cs:flag_store
    pop ax                  ; get offset from near call to vector jump
    sub ax,13h              ; adjust for jump vector code at beginning of segment and for call offset
    xor dx,dx               ; zero high word of dividend
    mov bx,3
    div bx                  ; call every three bytes, this gets overlay id number in ax

; ax now holds the overlaid identifier after division, relative zero
    inc ax                  ; make overlay id number relative 1
    or  ah,80h              ; set flag for far call

    pop cx                  ; get indirect calling routine offset in cx
    pop dx                  ; get indirect calling routine segment in dx

    mov cs:call_offset,cx   ; save calling routine offset

    mov bx,0c000h           ; preset bx to show call from root via segment fixup
    cmp dx,cs:$$_ovl_load_start ; see if segment below overlay load area
    jb  ovl_seg_to_main     ; yes, this is an indirect call from the root
    cmp dx,cs:$$_ovl_load_end   ; see if segment above overlay load area
    jae ovl_seg_to_main     ; yes, indirect call from root

; call from overlay to overlay via segment fixup
    mov cs:ds_store,ds      ; save data segment, used for walking load overlay areas
    cmp cs:last_access_slot,0   ; see if last accessed overlay
    je  not_last            ; no
    mov ds,cs:last_access_slot  ; ds -> last access overlay slot
    cmp dx,ds:[6]           ; see if calling segment matches current segment
    je  ovl_seg_found       ; yes, segment found

not_last:
    mov bx,$$_ovl_data
    mov ds,bx               ; ds -> overlay manager data
    mov bx,$$_ovl_array_start   ; bx -> overlay loaded array element
    xor cx,cx

; bx -> overlay loaded array element
; ax holds overlay identification number with flags
; cx == 0
; dx holds calling segment of routine
ovl_segjmp_loop:
    mov ds,bx               ; ds -> overlay loaded array element
    cmp cx,ds:[0]           ; see if this array element was used
    je  ovl_segjmp_next     ; no, try next one
    cmp dx,ds:[6]           ; see if calling segment matches current segment
    je  ovl_seg_found       ; yes, segment found

ovl_segjmp_next:
    inc bx                  ; bump to next overlay loaded array element
    jmp SHORT ovl_segjmp_loop   ; see if matches

ovl_seg_found:
    mov bx,ds:[0]           ; get overlay identifier in bx
    or  bh,40h              ; set segment fixup reference bit
    mov ds,cs:ds_store      ; restore data segment

ovl_seg_to_main:
    mov cs:call_segment,dx  ; save segment of calling routine
    mov cs:source_id,bx     ; save id of calling routine
    mov cs:destination_id,ax    ; save id of routine to call

    mov dx,cs:dx_store      ; restore dx to original value
    push    cs:flag_store   ; restore original flag values
    popf
    jmp NEAR PTR vector_altent ; everything properly set up, transfer to main overlay driver
$$_ovl_seg_call_entry ENDP

;*****************************
;* $$_OVL_IND_2OVL_ENT       *
;*****************************

; indirect call vector (from root or overlay) to overlay
; the stack holds the offset of the near call to jump to the vector here,
; used to compute which overlaid public is being called,
; the segment:offset of the calling routine is also on the stack
; indirect calls are always assumed to be FAR
; check if segment shows this is call from root or overlay
; if overlay walk the overlay loaded array checking the calling routine's
; segment against loaded overlays' segments to find the proper source id for bx

$$_ovl_ind_2ovl_ent PROC    FAR
    mov cs:$$_ovl_in_manager, 1  ; flag used by profiler
    mov cs:ax_store,ax      ; save ax,bx,cx,dx value
    mov cs:bx_store,bx
    mov cs:cx_store,cx
    mov cs:dx_store,dx
    pushf                   ; save the flag register
    pop cs:flag_store
    pop ax                  ; get offset from near call to vector jump
    sub ax,13h              ; adjust for jump vector code at beginning of segment and for call offset
    xor dx,dx               ; zero high word of dividend
    mov bx,3
    div bx                  ; call every three bytes, this gets public overlay number in ax

; ax now holds the overlaid public number after division, relative zero
    inc ax                  ; make public number relative 1
    or  ah,80h              ; set flag for far call

    pop cx                  ; get indirect calling routine offset in cx
    pop dx                  ; get indirect calling routine segment in dx

ind_altent:
    mov cs:call_offset,cx   ; save calling routine offset

    mov bx,8000h            ; set bx to show call from root
    cmp dx,cs:$$_ovl_load_start ; see if segment below overlay load area
    jb  ovl_ind_to_main     ; yes, this is an indirect call from the root
    cmp dx,cs:$$_ovl_load_end   ; see if segment above overlay load area
    jae ovl_ind_to_main     ; yes, indirect call from root

; indirect call from overlay to overlay
    mov cs:ds_store,ds      ; save data segment, used for walking load overlay areas
    cmp cs:last_access_slot,0   ; see if last accessed overlay
    je  not_last2           ; no
    mov ds,cs:last_access_slot  ; ds -> last access overlay slot
    cmp dx,ds:[6]           ; see if calling segment matches current segment
    je  ovl_ind_found       ; yes

not_last2:
    mov bx,$$_ovl_data
    mov ds,bx               ; ds -> overlay manager data
    mov bx,$$_ovl_array_start   ; bx -> overlay loaded array element
    xor cx,cx

; bx -> overlay loaded array element
; ax holds overlay public number with flags
; cx == 0
; dx holds calling segment of routine
ovl_ind_loop:
    mov ds,bx               ; ds -> overlay loaded array element
    cmp cx,ds:[0]           ; see if this array element was used
    je  ovl_ind_next        ; no, try next one
    cmp dx,ds:[6]           ; see if calling segment matches current segment
    je  ovl_ind_found       ; yes

ovl_ind_next:
    inc bx                  ; bump to next overlay loaded array element
    jmp SHORT ovl_ind_loop  ; see if matches

ovl_ind_found:
    mov bx,ds:[0]           ; get overlay identifier in bx
    mov ds,cs:ds_store      ; restore data segment

ovl_ind_to_main:
    mov cs:call_segment,dx  ; save segment of calling routine
    mov cs:source_id,bx     ; save id of calling routine
    mov cs:destination_id,ax    ; save id of routine to call

    mov dx,cs:dx_store      ; restore dx to original value
    push    cs:flag_store   ; restore original flag values
    popf
    jmp SHORT vector_altent ; everything properly set up, transfer to main overlay driver
$$_ovl_ind_2ovl_ent ENDP

;*****************************
;* $$_OVL_MGR_ENTRY          *
;*****************************

; the main overlay manager driver
; this procedure is far jumped to, NOT called to
; upon entry ax contains public-in-overlay number (1-16383),
; or destination overlaid identifier number if via segment fixup (set bit 6 of bx)
; high bit (80h) of ah set if a far call to overlaid public
; high bit (80h) of ah reset if near call
; bit 6 (40h) of ah set if routine being called is not an overlay (root called from an overlay)
; bx contains source identifier (1-16383)
; high bit (80h) of bh set if calling routine is not an overlay (root calling an overlay)
; bit 6 (40h) of bh set if calling routine is via segment fixup (assume zero offset)
; cx contains segment of calling routine (needed for root calling overlay via NEAR call)
; near/far address+ax+bx+cx are on stack

$$_ovl_mgr_code PROC    FAR

; kludge to allow checking for source id <> call code segment id
; this is the case if an indirect call to overlay through a
; memory location was set up in overlay class code and then the call
; was made in a different segment than the setup segment
$$_ovl_vector:
    mov cs:$$_ovl_in_manager, 1  ; flag used by profiler
    pushf                   ; save the flag register
    pop cs:flag_store
    test    ah,80h          ; see if far call
    je  vect_1              ; no, near call, continue with normal code

check_seg_id:
    pop cs:cx_store         ; get cx,bx,ax original values in storage off of stack
    pop cs:bx_store
    pop cs:ax_store
    pop cx                  ; get call offset in cx
    mov cs:dx_store,dx      ; save dx
    pop dx                  ; get calling segment value in dx

; ax holds proper overlaid public value from initial entry
    jmp NEAR PTR ind_altent ; jump to code shared with indirect call routine

vect_1:
    mov cs:call_segment,cx  ; save segment of calling routine
    mov cs:source_id,bx     ; save id of calling routine
    mov cs:destination_id,ax    ; save id of routine to call
    pop cs:cx_store         ; save cx,bx,ax original values
    pop cs:bx_store
    pop cs:ax_store
    pop cs:call_offset

; alternate entry point for routine from $$_ovl_ind_2ovl_ent routine
vector_altent:

; save program's stack pointer and segment
vect_2:
    mov ax,ss
    mov bx,sp               ; get program stack values
    mov cs:prog_ss_store,ax
    mov cs:prog_sp_store,bx ; save them

; set stack to overlay manager internal stack
    mov bx,OFFSET $$_ovl_mgr_stack_end
    mov ax,SEG $$_ovl_mgr_stack_end
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    mov al,BYTE PTR cs:source_id+1
    and al,40h              ; see if calling via external fixup reference
    je  vect_2a             ; yes

; calling via segment fixup reference, use overlay identifier in destination_id
; and use an offset of zero
    xor ax,ax               ; zero offset
    mov cs:jump_offset,ax   ; save it
    mov ax,cs:destination_id    ; get overlay identifier number
    and ax,3fffh            ; maks off flag bits
    mov cs:jump_segment,ax  ; save segment
    jmp SHORT vect_3        ; calling an overlay

; get target address segment:offset from lookup table
; lookup table segment only used if called routine is not an overlay,
; otherwise segment value is really the overlay identifier
vect_2a:
    mov bx,cs:destination_id
    and bx,3fffh            ; mask off flag bits
    dec bx                  ; make relative zero
    shl bx,1
    shl bx,1
    shl bx,1                ; convert bx to quadword offset
    push    ds              ; save data segment
    mov ax,cs:$$_ovl_lookup_table   ; get lookup table segment
    mov ds,ax

    mov ax,[bx+4]           ; get offset to near vector routine of segment
    mov cs:nearv_offset,ax  ; save offset to near vector routine

; add extra check for call from overlay to root, since indirect
; overlay vector routine now used for that case with removal of far vectors
; within a segment will not set the flag properly
    mov al,[bx+6]           ; get root called flag
    or  BYTE PTR cs:destination_id+1,al ; merge into source identifier

    lds ax,[bx]             ; get seg:off of target address in ds:ax
    mov cs:jump_offset,ax   ; save offset
    mov ax,ds
    mov cs:jump_segment,ax  ; save segment

    pop ds                  ; restore data segment

    mov al,BYTE PTR cs:destination_id+1
    and al,40h              ; see if calling an overlay
    je  vect_3              ; yes

; calling non-overlaid code, fixup the segment address
    mov ax,cs:prog_psp      ; get PSP
    add ax,10h              ; add 10h paragraphs to get load address
    add cs:jump_segment,ax  ; add to segment paragraph value
    jmp SHORT vect_4        ; bypass overlaid code load check

vect_stackerr:
    mov ax,1                ; get error value in ax
    jmp NEAR PTR $$_ovl_op_error    ; stack overflow

vect_3:
    mov cs:is_calling_ovl,1 ; flag that overlay being called
    call    $$_ovl_ready_overlay    ; load overlay if necessary, modify jump_segment

; set stack to calling routine seg/id/offset stack
vect_4:
    mov bx,cs:$$_ovl_stack_ptr
    cmp bx,8                ; check for stack overflow
    jb  vect_stackerr       ; overflow

    mov ax,cs:$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    push    cs:destination_id   ; save destination routine, needed for reload option
    push    cs:call_segment ; save segment to stack
    push    cs:source_id    ; save source id code to stack
    push    cs:call_offset
    mov cs:$$_ovl_stack_ptr,sp  ; save new stack pointer value

; restore stack to program value
    mov ax,cs:prog_ss_store
    mov bx,cs:prog_sp_store ; get stack entry values
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

; restore ax,bx,cx to original values
    mov cx,cs:cx_store
    mov bx,cs:bx_store
    mov ax,cs:ax_store

; profiler traps here to monitor calls
$$_ovl_prof_call:
    nop

    test    BYTE PTR cs:destination_id+1,80h    ; see if a far call
    je  vect_near           ; no, near call

; far call from calling routine
    push    cs:flag_store   ; get original flag value
    popf                    ; restore to flag word
    mov cs:$$_ovl_in_manager, 0  ; flag used by profiler
    call    DWORD PTR cs:jump_offset    ; transfer control to overlay
    jmp SHORT $$_ovl_ret    ; upon return, transfer to overlay manager return handler

; near call from calling routine
vect_near:
    push    cs:flag_store   ; get original flag value
    popf                    ; restore to flag word
    push    cs:jump_offset  ; put offset on stack for overlay's near vector routine use
    push    cs:nearv_offset ; get offset to near vector routine for segment
    pop cs:jump_offset      ; replace jump address offset
    mov cs:$$_ovl_in_manager, 0  ; flag used by profiler
    jmp DWORD PTR cs:jump_offset    ; transfer control to overlay's near vector routine

$$_ovl_mgr_code ENDP

;*****************************
;* $$_OVL_MGR_RET            *
;*****************************

$$_ovl_mgr_ret  PROC    FAR

$$_ovl_ret:
    mov cs:$$_ovl_in_manager, 1  ; flag used by profiler
    mov cs:cx_store,cx      ; save original ax,bx,cx
    mov cs:bx_store,bx
    mov cs:ax_store,ax
    pushf                   ; save the flag register
    pop cs:flag_store

; save program's stack pointer and segment
    mov ax,ss
    mov bx,sp               ; get program stack values
    mov cs:prog_ss_store,ax
    mov cs:prog_sp_store,bx ; save them

; set stack to calling routine seg/id/offset stack
    mov bx,cs:$$_ovl_stack_ptr
    mov ax,cs:$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    pop cs:call_offset      ; get calling routine's offset
    pop cs:source_id        ; get calling routine's identification
    pop cs:call_segment     ; get calling routine's segment  (changed for return to overlay)
    pop cs:destination_id   ; get called routine's identification
    mov cs:$$_ovl_stack_ptr,sp  ; save new stack pointer value

    mov cs:is_calling_ovl, 0  ; only the profiler cares about this right now

    cmp cs:$$_ovl_reload_flag,0 ; see if reload overlay option in effect
    je  omr_2               ; no, bypass reload overlay stack adjustment

    mov al,BYTE PTR cs:destination_id+1
    and al,40h              ; see if root was called
    je  ovl_called          ; no
    jmp NEAR PTR omr_3      ; yes

; overlay called with reload option, extra info on stack
ovl_called:
    pop cs:$$_ovl_stackcount
    mov cs:$$_ovl_stack_ptr,sp  ; save new stack pointer value

; set stack to overlay manager internal stack
omr_2:
    mov bx,OFFSET $$_ovl_mgr_stack_end
    mov ax,SEG $$_ovl_mgr_stack_end
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    mov al,cs:$$_ovl_reload_flag
    or  al,al               ; see if reload overlay option in effect
    je  omr_3               ; no, bypass reload specific code

; drop activity level of overlay
    push    ds              ; save critical register
    mov cx,$$_ovl_data      ; save -> overlay manager data

    mov al,BYTE PTR cs:source_id+1
    and al,40h              ; see if called via external fixup reference
    je  deact_2             ; yes

; called via segment fixup reference,overlay identifier in destination_id
    mov ax,cs:destination_id    ; get overlay identifier number
    and ax,3fffh            ; mask off flag bits
    jmp SHORT deact_3       ; bypass overlay identifier computation

; get overlay identifier lookup table
deact_2:
    mov bx,cs:destination_id
    and bx,3fffh            ; mask off flag bits
    dec bx                  ; make relative zero
    shl bx,1
    shl bx,1
    shl bx,1                ; convert bx to quadword offset
    mov ds,cs:$$_ovl_lookup_table   ; get lookup table segment
    mov ax,[bx+2]           ; get overlay identifier

; overlay identifer in ax, search the overlays loaded array to find it
deact_3:
    cmp cs:last_access_slot,0
    je  deact_4
    mov ds,cs:last_access_slot  ; ds -> last access overlay slot
    cmp ax,ds:[0]           ; see if id matches last accessed id
    je  deact_found         ; yes, update the array element

deact_4:
    mov ds,cx               ; ds -> overlay manager data
    mov bx,$$_ovl_array_start
    mov ds,bx                   ; ds -> overlay array

deact_loop:
    cmp ax,ds:[0]           ; see if array element matches
    je  deact_found         ; yes, update the array element
    inc bx
    mov ds,bx
    jmp SHORT deact_loop    ; loop until overlay is found

deact_found:
    dec WORD PTR ds:[4]     ; drop activity level

    cmp cs:$$_ovl_stackcount,0  ; see if any overlays to restore
    je  omr_2a              ; no

; this overlay will be swapped out by the overlays that it swapped out
; zero the array entry and drop the overlays loaded count
    xor ax,ax
    mov ds:[0],ax
    mov ds:[4],ax           ; zero activity level

    mov ds,cx               ; ds -> overlay manager data
    dec $$_ovl_loaded_count ; drop count of loaded overlays to reflect terminated overlay swapout
    call    $$_ovl_swap_in  ; load the swapped out active overlays

omr_2a:
    pop ds                  ; restore critical register

omr_3:
    mov al,BYTE PTR cs:source_id+1
    and al,80h              ; see if calling routine was in root
    je  omr_4               ; no

; calling routine was in root
    push    ds
    push    si
    push    dx
    mov ax,$$_ovl_data
    mov ds,ax               ; ds -> overlay manager data
    mov cx,$$_ovl_loaded_count
    mov ds,$$_ovl_array_start   ; ds -> overlay loaded array
    xor ax,ax
    mov bx,ax
    mov dx,ax
    mov cs:last_access_slot,ax

last_access_loop:
    cmp ax,ds:[0]           ; see if unused array element
    je  next_access_check   ; yes
    dec cx                  ; drop count of active overlays checked
    cmp ax,ds:[4]           ; see if active overlay
    je  next_access_check   ; no
    cmp dx,ds:[12]          ; see if have higher LRU active
    ja  next_access_check   ; yes
    jb  new_last_access     ; no
    cmp bx,ds:[10]          ; equal, check higher LRU low word
    ja  next_access_check

new_last_access:
    mov cs:last_access_slot,ds  ; keep highest LRU active
	mov	dx,ds:[12]
	mov	bx,ds:[10]

next_access_check:
    mov si,ds
    inc si
    mov ds,si
    or  cx,cx
    jne last_access_loop

    pop dx
    pop si
    pop ds

    jmp NEAR PTR mgret_exit

; set stack to overlay manager internal stack in case of call to root bypassing first stack set
omr_4:
    mov bx,OFFSET $$_ovl_mgr_stack_end
    mov ax,SEG $$_ovl_mgr_stack_end
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    mov ax,cs:source_id     ; get source id
    and ax,3fffh            ; strip off flag bits
    mov cs:jump_segment,ax  ; place in jump_segment for get overlay ready routine

    mov cs:is_calling_ovl,0 ; flag that overlay being returned from
    call    $$_ovl_ready_overlay    ; load overlay if necessary, modify jump_segment

    mov ax,cs:jump_segment  ; get jump segment in call segment for return
    mov cs:call_segment,ax

; restore stack to program values, restore registers, and return
mgret_exit:
    mov ax,cs:prog_ss_store
    mov bx,cs:prog_sp_store ; get stack entry values
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

; profiler traps this point to monitor returns
$$_ovl_prof_return:
    nop

    mov cx,cs:cx_store
    mov bx,cs:bx_store
    mov ax,cs:ax_store
    push    cs:flag_store   ; get original flag value
    popf                    ; restore to flag word
    mov cs:$$_ovl_in_manager, 0  ; flag used by profiler

    jmp DWORD PTR cs:call_offset    ; transfer back to calling routine

$$_ovl_mgr_ret  ENDP

;*****************************
;* $$_OVL_READY_OVERLAY      *
;*****************************

; load overlay, if necessary
; upon entry jump_segment holds overlay identifier
; update LRU counter of overlay
; update jump_segment to overlay's current segment
; destroys ax,bx,cx
;
; Format of loaded overlay array structure:
;   Overlay identification number   DW  ?
;   Unused                          DW  ?
;   Activity level, 0 if not active DW  ?
;   Overlay segment/start para      DW  ?
;   Overlay ending paragraph        DW  ?
;   LRU counter value               DD  ?
;   Filler to pad to one paragraph  DW  ?

$$_ovl_ready_overlay    PROC    NEAR
    push    ds              ; save critical registers
    push    es
    push    dx
    mov ax,$$_ovl_data
    mov ds,ax               ; ds -> overlay manager data

    cmp $$_ovl_loaded_count,0   ; see if any overlays loaded
    je  ready_must_load     ; no, first overlay must always be loaded

; look for loaded overlay entry
ready_look:
    mov bx,cs:jump_segment  ; get identification number to match against in bx
    cmp cs:last_access_slot,0
    je  ready_1
    mov es,cs:last_access_slot  ; es -> last access overlay slot
    cmp bx,es:[0]           ; see if id matches last accessed id
    je  ready_match         ; yes

ready_1:
    mov ax,$$_ovl_array_start
    mov es,ax
    xor cx,cx               ; cx == number of loaded overlays found

ready_loop:
    mov ax,es:[0]           ; get overlay identification number
    cmp ax,bx               ; see if a match
    je  ready_match         ; yes

; no match try next entry, if any
    or  ax,ax               ; see if loaded overlay in this entry
    jne ready_3             ; yes

ready_2:
    mov ax,es               ; get old entry segment
    inc ax
    mov es,ax               ; bump to next entry
    jmp SHORT ready_loop    ; check next entry for match

ready_3:
    inc cx                  ; increment count of loaded overlays found
    cmp cx,$$_ovl_loaded_count  ; see if all loaded overlays checked
    jb  ready_2             ; no, move to next entry

; overlay must be loaded
ready_must_load:
    call    $$_ovl_get_info     ; get overlay system information from file
    call    $$_ovl_find_slot    ; find the best overlay pool slot to load overlay
    call    $$_ovl_load_overlay ; load overlay at segment found
    inc $$_ovl_loaded_count     ; increment count of loaded overlays
    mov ax,$$_ovl_load_seg
    jmp SHORT ready_loaded      ; bypass already loaded code

ready_stackerr:
    mov ax,1                ; get error value in ax
    jmp NEAR PTR $$_ovl_op_error    ; stack overflow

; entry matches this overlay
ready_match:
    cmp cs:$$_ovl_reload_flag,0 ; see if reload overlays option is in effect
    je  match_2             ; no

    cmp cs:is_calling_ovl,0 ; see if calling overlay
    je  match_3             ; no, returning from overlay, bypass activity level update

; calling an already loaded overlay
; push a zero on the overlay stack for number of overlays swapped out

; set stack to calling routine seg/id/offset stack
    mov cs:$$_ovl_mgr_stack_ptr,sp  ; first save current stack pointer
    mov dx,cs:$$_ovl_stack_ptr
    cmp dx,2                ; must be room for zero
    jb  ready_stackerr      ; not enough room

match_1:
    mov ax,cs:$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,dx               ; set stack
    sti                     ; turn interrupts back on
    xor ax,ax               ; put zero on stack
    push    ax
    mov cs:$$_ovl_stack_ptr,sp  ; save new stack pointer value

; reset stack to overlay manager internal stack
    mov dx,cs:$$_ovl_mgr_stack_ptr
    mov ax,SEG $$_ovl_mgr_stack_end
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,dx               ; set internal stack
    sti                     ; turn interrupts back on

match_2:
    inc WORD PTR es:[4]     ; bump activity level

match_3:
    mov ax,es:[6]           ; get overlay segment

; overlay loaded
; es -> array element of loaded overlay
ready_loaded:
    mov cs:jump_segment,ax  ; update jump_segment

; save last accessed overlay
    mov cs:last_access_slot,es  ; save last access slot

    mov ax,WORD PTR $$_ovl_lru_counter  ; get old counter value low word
    add ax,1
    mov WORD PTR $$_ovl_lru_counter,ax  ; update low word
    adc WORD PTR $$_ovl_lru_counter+2,0 ; carry to high word
    mov bx,WORD PTR $$_ovl_lru_counter+2    ; get counter value high word
    mov es:[10],ax          ; update overlay loaded array LRU low word
    mov es:[12],bx          ; update high word
    or  ax,bx               ; check if lru counter overflow
    je  lru_overflow        ; yes

ready_ret:
    pop dx                  ; restore critical registers
    pop es
    pop ds
    ret

; lru counter overflowed, reset counters in all array entries
lru_overflow:
    inc ax
    mov WORD PTR $$_ovl_lru_counter,ax  ; reset lru counter to 1

; cycle through all entries, setting the lru counter to zero
    mov ax,$$_ovl_array_start
    mov es,ax
    xor ax,ax
    mov cx,$$_ovl_max_load

ready_zero_loop:
    mov es:[10],ax          ; zero lru low word
    mov es:[12],ax          ; zero lru high word
    mov bx,es
    inc bx
    mov es,bx               ; move to next entry
    loop    ready_zero_loop ; loop until done
    jmp SHORT ready_ret

$$_ovl_ready_overlay    ENDP

;*****************************
;* $$_OVL_FIND_SLOT          *
;*****************************

; find best place to load overlay in overlay pool, based on
; lru counter values and number of overlays swapped out.
; lowest LRU value has precedence over count swapped.
; cs:jump_segment variable holds overlay identifier
; returns load segment in $$_ovl_load_seg
; returns load array element in es
; destroys ax,bx,cx,dx,es

; no overlays loaded
no_ovl_loaded:
    mov ax,cs:$$_ovl_load_start
    mov $$_ovl_load_seg,ax  ; use start of load area for load segment
    mov dx,ax               ; dx holds start paragraph
    mov bx,ofh_ovl_size
    add bx,dx               ; bx holds end paragraph
    jmp NEAR PTR slot_ret   ; bypass search for area to load

$$_ovl_find_slot    PROC    NEAR
    push    ds              ; save critical register
    push    si
    push    di
    mov si,$$_ovl_array_start   ; si holds array element for new overlay
    xor ax,ax
    mov cs:$$_ovl_stackcount,ax ; init count of overlays on stack
    cmp $$_ovl_loaded_count,ax  ; see if any overlays loaded
    je  no_ovl_loaded       ; no

;********** start rewrite

COMMENT #
    mov ax,WORD PTR $$_ovl_pool+1   ; get count of words*2 in check array
    shr ax,1                ; compute count of words in check array
    mov cx,ax
    add ax,ax               ; get number of bytes in check array
    mov check_array_size,ax ; keep it
    xor ax,ax
    mov di,ax
    mov es,check_array
    rep stosw               ; clear the check array
END COMMENT #

; 9/2/92
    mov cx,check_array_size	; get number of bytes in check array
	shr	cx,1				; convert to words
    xor ax,ax
    mov di,ax
    mov es,check_array
    rep stosw               ; clear the check array

    mov cx,$$_ovl_loaded_count  ; get count of loaded overlays
    push    ds              ; save -> manager data
    mov ds,si               ; ds -> overlay loaded array

; es -> check array
check_loop:
    mov ax,ds:[0]           ; get overlay identifier number
    or  ax,ax               ; see if zero
    je  next_check          ; yes, try next element
    dec cx                  ; drop count of overlays to check
    mov dx,ds
    cmp dx,cs:last_access_slot  ; see if this is last accessed overlay
    jne check_2             ; no
    or ah,80h               ; flag last accessed overlay
    jmp SHORT store_value

check_2:
    cmp WORD PTR ds:[4],0   ; see if an active overlay
    je  store_inactive      ; no
    or  ah,40h              ; flag active overlay
    jmp SHORT store_value

; store slot pointer, not overlay id
store_inactive:
    mov ax,ds
    sub ax,si               ; subtract start of array
    inc ax                  ; make relative 1

store_value:
    mov dx,ds:[8]
    mov di,ds:[6]
    sub dx,di               ; get size of overlay
    add dx,1fh              ; round up to next 512 byte boundary (in paras)
    shr dx,1
    shr dx,1
    shr dx,1
    shr dx,1
    shr dx,1                ; convert to 512 byte chunks
    sub di,cs:$$_ovl_load_start ; di -> para offset of overlay start
    shr di,1
    shr di,1
    shr di,1
    shr di,1                ; convert para offset to 512 word (256 byte) offset

store_loop:
    stosw                   ; save modified overlay id
    dec dx                  ; drop count of 512 byte overlay chunks
    jne store_loop          ; save until all chunks represented

next_check:
    jcxz    store_done      ; more more loaded overlays to check
    mov ax,ds
    inc ax
    mov ds,ax               ; ds -> next overlay loaded array element
    jmp SHORT check_loop

store_done:
    pop ds                  ; restore ds -> manager data

    mov ax,0ffffh
    mov $$_ovl_best_count,ax    ; set best count such that any count of swapped overlays is better than init value
;***    mov $$_ovl_best_active,ax   ; init best active swapout count high
    mov WORD PTR $$_ovl_hi_best_lru,ax  ; init so any comparison will have lower LRU
    mov WORD PTR $$_ovl_hi_best_lru+2,ax
    mov ovl_best_size,ax    ; init best load size so fails on any comparison
    mov es,check_array
    xor si,si               ; es:si -> check array

    mov dx,ofh_ovl_size     ; get overlay file size in paragraphs
    add dx,1fh              ; round up to next 512 byte boundary (in paras)
    mov cl,5
    shr dx,cl               ; convert to 512 byte chunks

main_check_loop:
    xor ax,ax
    mov $$_ovl_swap_count,ax    ; init count of overlays swapped by proposed load position
;***    mov $$_ovl_swap_active,ax   ; init count of active overlays swapped by proposed load
    mov WORD PTR $$_ovl_highest_lru,ax  ; init so any comparison will force a higher LRU
    mov WORD PTR $$_ovl_highest_lru+2,ax
    mov ovl_act_size,ax     ; init size of active swapouts
    mov bx,si               ; offset into check_array

block_check_loop:
    mov ax,es:[bx]          ; get overlay id/slot #
    test    ah,80h          ; see if last used overlay
    jne to_bump_proposed    ; yes, don't use this position
    or  ax,ax               ; see if used
    je  check_next_block    ; no, try next block in overlay, if any

    inc $$_ovl_swap_count   ; bump count of overlays swapped out
    test    ah,40h
    je  check_inactive      ; inactive overlay swapped out

; active overlay swapout at this position
;***    inc $$_ovl_swap_active  ; bump count of active overlays swapped out
    inc ovl_act_size        ; bump load size by fake amount to compensate for each discrete active overlay

check_act_loop:
    inc ovl_act_size        ; bump load size in 512 byte blocks
    cmp bx,check_array_size ; see if at end
    jae check_chunk_count   ; yes
    cmp ax,es:[bx+2]        ; see if next chunk matches this
    jne check_chunk_count   ; no
    dec dx                  ; drop count of chunks to check
    inc bx
    inc bx
    jmp SHORT check_act_loop    ; loop until end 

to_bump_proposed:
    jmp NEAR PTR bump_proposed

check_chunk_count:
    cmp dx,0                ; see if all of overlay used up (could be negative)
    jle check_next_pos
    jmp SHORT check_next_block  ; more positions to check for proposed overlay

; inactive overlay swapout
check_inactive:
    dec ax                  ; make slot # relative zero
    add ax,$$_ovl_array_start
    mov es,ax               ; es -> inactive overlay array entry
    mov ax,es:[12]          ; get LRU counter high word
    cmp ax,WORD PTR $$_ovl_highest_lru+2    ; compare to previous lowest LRU high word
    jb  no_update           ; lower than previous highest, don't update
    ja  update_high_lru     ; higher than previous highest, update
    mov ax,es:[10]          ; get LRU counter low word
    cmp ax,WORD PTR $$_ovl_highest_lru  ; compare to previous lowest LRU low word
    jb  no_update           ; lower than previous, don't update

; update the highest LRU values
update_high_lru:
    mov ax,es:[10]
    mov WORD PTR $$_ovl_highest_lru,ax  ; update low word
    mov ax,es:[12]
    mov WORD PTR $$_ovl_highest_lru+2,ax    ; update high word

; no update or already performed drop-through
no_update:
    mov es,check_array
    mov ax,es:[bx]          ; get slot # in ax

check_inact_loop:
    cmp bx,check_array_size ; see if at end
    jae check_chunk_count   ; yes
    cmp ax,es:[bx+2]        ; see if next chunk matches this
    jne check_chunk_count   ; no
    dec dx                  ; drop count of chunks to check
    inc bx
    inc bx
    jmp SHORT check_inact_loop  ; loop until end 

check_next_block:
    inc bx
    inc bx                  ; bump word offset in check array
    dec dx                  ; drop count of chunks to check
    jne block_check_loop    ; more chunks to check

check_next_pos:
    cmp $$_ovl_swap_count,0 ; if zero swapped overlays, use this position
    je  perfect_swap

; current position has overlays being swapped out
    mov ax,ovl_best_size    ; get best load size
    cmp ax,ovl_act_size
    jb  bump_proposed       ; best has less overlay loads than highest
    ja  best_update         ; current has least overlay movement, update best position

; best position and current position have same active load size,
; check LRU for highest inactive, keep the lowest
    mov ax,WORD PTR $$_ovl_hi_best_lru+2    ; get high word of best highest LRU
    cmp ax,WORD PTR $$_ovl_highest_lru+2    ; compare to lowest LRU this pass
    jb  bump_proposed       ; best below lowest, don't update
    ja  best_update         ; best above highest, update position
    mov ax,WORD PTR $$_ovl_hi_best_lru  ; get low word of best LRU
    cmp ax,WORD PTR $$_ovl_highest_lru  ; compare to low word of this pass LRU
    jb  bump_proposed       ; best below lowest, don't update
    ja  best_update         ; best above lowest, update position

; LRU's match, compare swap counts, use lowest
    mov ax,WORD PTR $$_ovl_best_count
    cmp ax,WORD PTR $$_ovl_swap_count
    jb  bump_proposed       ; best swap count lower than current, don't update

; update best load position variables
best_update:
    mov ax,WORD PTR $$_ovl_highest_lru+2
    mov WORD PTR $$_ovl_hi_best_lru+2,ax    ; update high word of best LRU
    mov ax,WORD PTR $$_ovl_highest_lru
    mov WORD PTR $$_ovl_hi_best_lru,ax  ; update low word of best LRU

    mov ax,$$_ovl_swap_count
    mov $$_ovl_best_count,ax    ; update count of swapped overlays
    mov ax,ovl_act_size
    mov ovl_best_size,ax    ; update best load size
    mov $$_ovl_best_load,si ; save check array pointer new best load position

bump_proposed:
    inc si
    inc si                  ; bump base check address
    mov ax,si
    mov dx,ofh_ovl_size     ; get overlay file size in paragraphs
    add dx,1fh              ; round up to next 512 byte boundary (in paras)
    mov cl,5
    shr dx,cl               ; convert to 512 byte chunks
    add ax,dx               ; get final check array element
    add ax,dx               ; adjust for word count
    cmp ax,check_array_size ; see if past end
    ja  swap_ovls           ; last overlay area that fits was checked
    jmp NEAR PTR main_check_loop    ; more overlay positions to check

; no overlays swapped at this position, use it
perfect_swap:
    mov ax,si               ; get current check array position word
    mov cl,4
    shl ax,cl               ; convert to paras (x16)
    add ax,cs:$$_ovl_load_start ; get real address
    mov $$_ovl_load_seg,ax  ; save load position
    mov dx,ax               ; dx holds start segment of overlay to load
    mov bx,ofh_ovl_size     ; get overlay file size in paragraphs
    add bx,dx               ; bx holds end segment of overlay to load
    mov es,$$_ovl_array_start   ; es -> overlay loaded array

find_zero:
    mov si,es:[0]           ; get overlay identifier of array element
    or  si,si               ; see if zero
    jne find_2              ; no, keep looking
    jmp NEAR PTR slot_ret2  ; yes, use this array element (es->array element)

find_2:
    mov ax,es
    inc ax
    mov es,ax               ; bump to next array element
    jmp SHORT find_zero     ; keep looking for unused array element

swap_ovls:
    mov ax,$$_ovl_best_load ; get best check array position word
    mov cl,4
    shl ax,cl               ; convert to paras (x16)
    add ax,cs:$$_ovl_load_start ; get real address
    mov $$_ovl_load_seg,ax  ; use best proposed load area for load segment
    mov dx,ax               ; dx holds start segment of overlay to load
    mov bx,ofh_ovl_size     ; get overlay file size in paragraphs
    add bx,dx               ; bx holds end segment of overlay to load
    mov es,$$_ovl_array_start   ; es -> overlay array
    mov cx,$$_ovl_best_count    ; get count of overlays to swap out

	cmp	cx,-1				; see if best never init'ed (only last overlay exists)
	jne	zero_loop			; no
	mov	cx,1				; change to one overlay to swapout

;********** end rewrite

zero_loop:
    mov ax,es:[0]           ; get overlay identifier number
    or  ax,ax               ; see if nonzero
    jne zero_2              ; nonzero, valid overlay
    mov si,es               ; si -> empty slot

    or  cx,cx               ; see if more overlays to swap out
    jne next_element        ; yes
    jmp NEAR PTR slot_ret   ; any and all necessary overlays swapped out

zero_2:
    jcxz    next_element    ; no overlays to swap, just looking for empty slot
    cmp dx,es:[8]           ; check proposed start against overlay end
    jae next_element        ; above end, overlay not swapped
    cmp bx,es:[6]           ; check proposed end against overlay start
    jbe next_element        ; below start, overlay not swapped

    cmp cs:$$_ovl_reload_flag,0 ; see if reload overlays option in effect
    je  zero_3              ; no

    mov ax,es:[4]
    or  ax,ax               ; see if active overlay
    je  zero_4              ; no

; set stack to calling routine seg/id/activity stack
    mov cs:$$_ovl_mgr_stack_ptr,sp  ; first save current stack pointer
    mov di,cs:$$_ovl_stack_ptr
    mov ax,cs:$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,di               ; set internal stack
    sti                     ; turn interrupts back on
    push    es:[6]          ; save load segment to stack
    push    es:[0]          ; save overlay identifier to stack
    push    es:[10]         ; save LRU low word
    push    es:[12]         ; save LRU high word
    push    es:[4]          ; save activity level

    inc cs:$$_ovl_stackcount    ; bump count of overlays saved to stack
    mov cs:$$_ovl_stack_ptr,sp  ; save new stack pointer value

; set stack to overlay manager internal stack
    mov di,cs:$$_ovl_mgr_stack_ptr
    mov ax,SEG $$_ovl_mgr_stack_end
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,di               ; set internal stack
    sti                     ; turn interrupts back on

    call    chk_xmsems_swapout  ; see if can swapout to XMS/EMS

zero_3:
    xor ax,ax

zero_4:
    mov es:[0],ax           ; zero overlay identifier for this array element
    mov es:[4],ax           ; zero activity level

    mov si,es               ; si -> empty slot
    dec $$_ovl_loaded_count ; drop count of loaded overlays
    dec cx                  ; drop count of overlays left to swap out
    jcxz    slot_ret        ; any and all necessary overlays swapped out, si -> empty slot

next_element:
    mov ax,es               ; get array element segment
    inc ax                  ; bump to next element
    mov es,ax               ; update array element pointer
    jmp NEAR PTR zero_loop  ; loop back to check next array element for zero'ing

slot_ret:
    mov es,si               ; es -> overlay loaded array element for new overlay

slot_ret2:
    cmp cs:$$_ovl_reload_flag,0     ; see if reload overlays option in effect
    je  slot_ret3           ; no

; set stack to calling routine seg/id/offset stack
    mov cs:$$_ovl_mgr_stack_ptr,sp  ; first save current stack pointer
    mov di,cs:$$_ovl_stack_ptr
    mov ax,cs:$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,di               ; set internal stack
    sti                     ; turn interrupts back on

    push    cs:$$_ovl_stackcount    ; put count of active overlays swapped out on stack
    mov cs:$$_ovl_stack_ptr,sp  ; save new stack pointer value

; reset stack to overlay manager internal stack
    mov di,cs:$$_ovl_mgr_stack_ptr
    mov ax,SEG $$_ovl_mgr_stack_end
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,di               ; set internal stack
    sti                     ; turn interrupts back on

slot_ret3:
    mov ax,cs:jump_segment  ; get overlay identifier
    mov es:[0],ax
    mov es:[6],dx           ; save start paragraph
    mov es:[8],bx           ; save end paragraph

    mov WORD PTR es:[4],1   ; set activity level to 1 (first time active)

slot_ret4:
    pop di
    pop si
    pop ds                  ; restore critical register
    ret
$$_ovl_find_slot    ENDP

;***********************************************************

; all overlays concatenated in one .OVL file, 
; each rounded to nearest paragraph
; OVL file format:
;   Count of overlays           DW  ?
;   Offset to overlay 1         DD  ?
;       ...
;   Offset to overlay n         DD  ?
;   Overlay files
;
; Overlay file format in .OVL file
;   Overlay number              DW  ?
;   Offset to overlay code      DW  ?
;   Overlay code size in paras  DW  ?
;   Offset to near/shared vectors   DW  ?
;   Relocation entry count      DW  ?
;   Relocation entry 1          DW  ?
;       ...
;   Relocation entry n          DW  ?
;   Overlay code begins

;***********************************************************

;*****************************
;* $$_OVL_GET_INFO           *
;*****************************

; get overlay file system information
; jump_segment variable holds overlay identifier
; destroys ax,bx,cx,dx

$$_ovl_get_info PROC    NEAR
	mov	info_from_emsxms_flag,0	; init overlay stashed in EMS/XMS flag

err_retry6:
	push	ds				; save -> overlay manager data
    mov bx,cs:jump_segment  ; get overlay identifier
    dec bx                  ; make relative zero
    shl bx,1                ; make doubleword offset into file position table
    shl bx,1
    mov ds,$$_ovl_filepos   ; ds -> overlay file position table
    mov dx,[bx]             ; get file position
    mov cx,[bx+2]
	pop	ds					;  restore ds -> overlay manager data
    mov WORD PTR $$_ovl_file_offset,dx  ; save position low word
    mov WORD PTR $$_ovl_file_offset+2,cx    ; save  position high word

; check if can load from overlay file stashed in EMS or XMS
	test	ch,0c0h			; see if two high bits set (file stash to EMS/XMS)
	je	ogi_seek			; no
	call	ovl_emsxms_get_info	; get info from EMS or XMS
	ret

; seek to overlay file in OVL file
; dx:cx already set to proper values
ogi_seek:
    mov bx,$$_ovl_file_handle
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h
    jnc read_sys

    call NEAR PTR $$_ovl_dos_error
    jmp SHORT err_retry6

; read system information for overlay file (first 5 words)
read_sys:
    mov bx,$$_ovl_file_handle
    mov cx,10               ; read 10 bytes
    mov dx,OFFSET $$_ovl_file_header    ; read overlay file header bytes
    mov ah,3fh              ; read file
    int 21h
    jnc ogi_ret             ; no error

    call NEAR PTR $$_ovl_dos_error
    jmp SHORT read_sys

ogi_ret:
    ret

$$_ovl_get_info ENDP

;*****************************
;* OVL_EMSXMS_GET_INFO       *
;*****************************

; get overlay file system information from EMS or XMS
; upon entry jump_segment variable holds overlay identifier,
;   two MSB of ch flag whether XMS (80h) or EMS(40h) stashing
;   $$_ovl_file_offset holds offset with EMS/XMS
; destroys ax,bx,cx,dx

ovl_emsxms_get_info	PROC	NEAR
	push	si				; save critical register
	push	di
	mov	info_from_emsxms_flag,ch	; set overlay stashed in EMS/XMS flag

	mov	dx,ds
    mov di,OFFSET $$_ovl_file_header
	mov	cx,10				; count of chars to transfer
	mov	si,WORD PTR $$_ovl_file_offset	; source offset
	mov	bx,WORD PTR $$_ovl_file_offset+2	; logical page number/high word source offset
	and	bh,3fh				; mask off EMS/XMS flag bits

; bx==logical page number (EMS)/high word source offset (XMS)
; cx==count of chars to transfer
; dx==destination segment
; si==source offset
; di==destination offset
	call	ovl_emsxms_read	; read in the info from EMS/XMS

oeg_ret:
	pop	di					; restore critical register
	pop	si
	ret
ovl_emsxms_get_info	ENDP

;*****************************
;* OVL_EMSXMS_READ           *
;*****************************

; read from EMS/XMS to conventional memory
; upon entry:
;   bx==logical page number (EMS)/high byte source offset (XMS)
;   cx==count of chars to transfer
;   dx==destination segment
;   si==source offset
;   di==destination offset
; destroys ax,bx,cx,dx,si,di

ovl_emsxms_read	PROC	NEAR
	push	es				; save critical register
	mov	es,dx				; es -> destination
	mov	dx,ohpoht_handle	; get source handle
	test	info_from_emsxms_flag,80h	; see if XMS flag set
	je	ord_ems				; no, get from EMS

	mov	WORD PTR ohtbuff.os_len,cx	; length of block
	mov	WORD PTR ohtbuff.os_src_offset,si	; offset of source, low word
	mov	BYTE PTR ohtbuff.os_src_offset+2,bl	; offset of source, high word low byte
	mov	ohtbuff.os_src_handle,dx	; source handle
    mov WORD PTR ohtbuff.os_dest_offset,di	; destination low word offset (offset)
    mov WORD PTR ohtbuff.os_dest_offset+2,es	; destination high word offset (segment)
	xor	ax,ax
	mov	WORD PTR ohtbuff.os_len+2,ax	; zero high word of length
	mov	BYTE PTR ohtbuff.os_src_offset+3,al	; zero high byte of source offset
	mov	ohtbuff.os_dest_handle,ax	; zero destination handle (conventional memory)

    mov ah,0bh				; move extended memory block
    mov si,OFFSET ohtbuff	; ds:si -> parameter block
    call    DWORD PTR xms_addr
    call    check_xms_error	; see if error occurred
	jmp	NEAR PTR ord_ret

ord_ems:
	cmp	$$_ems3_flag,0		; see if using EMS 3.0 compatibility
	jne	ord_ems3			; yes

	mov	WORD PTR ohpbuff.os_len,cx	; length of block
	mov	ohpbuff.os_stype,1	; expanded memory source type
	mov	ohpbuff.os_shandle,dx	; source handle
	mov	ohpbuff.os_ssegpage,bx	; logical page number

ord_loop:
	cmp	si,4000h			; see if page overflow
	jb	ord_off
	sub	si,4000h			; drop offset
	inc	ohpbuff.os_ssegpage	; increase logical page number
	jmp	SHORT ord_loop		; drop until si is in range

ord_off:
	mov	ohpbuff.os_soffset,si	; offset of source

	mov	ohpbuff.os_doffset,di	; destination offset
	mov	ohpbuff.os_dsegpage,es	; destination segment
	xor	ax,ax
	mov	ohpbuff.os_dtype,al	; conventional memory destination type
	mov	ohpbuff.os_dhandle,ax	; destination handle

    mov ax,5700h			; move memory region
    mov si,OFFSET ohpbuff	; ds:si -> parameter block
    int 67h
	call	check_ems_error
	jmp	SHORT ord_ret

; bx==logical page number
; cx==count of chars to transfer
; dx==handle of EMS page
; si==source offset
; es==destination segment
; di==destination offset
ord_ems3:
	mov	ah,47h				; save page map
    int 67h
	call	check_ems_error
	push	ds				; save -> overlay manager data
	mov	ds,$$_ovl_ems_base	; ds:si -> source

ord_map:
    mov ax,4400h			; map expanded memory page to page 0
    int 67h
	call	check_ems_error

; copy to destination
	mov	ax,cx
	add	ax,si
	cmp	ax,4000h			; see if overflow to next page
	jbe	ord_noover			; no

; transfer to end of page
	mov	ax,4000h
	sub	ax,si				; get chars to transfer for this page
	sub	cx,ax				; update total transferred char count
	xchg	ax,cx			; ax==total transfer char count, cx==page transfer char count
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
	mov	cx,ax				; total transfer char count back in cx

	inc	bx					; move to next logical page
	xor	si,si				; zero offset on next page
	jmp	SHORT ord_map		; do next page transfer

ord_noover:
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

	mov	ah,48h				; restore page map
    int 67h
	call	check_ems_error
	pop	ds					; restore ds -> overlay manager data

ord_ret:
	pop	es					; restore critical register
	ret
ovl_emsxms_read	ENDP

;*****************************
;* OVL_EMSXMS_WRITE          *
;*****************************

; write from conventional memory to EMS/XMS
; upon entry:
;   cx==count of chars to transfer
;   di==destination offset relative to emsxms_offset start position
;   si==source offset
;   dx==source segment
; destroys ax,bx,cx,dx,si,di

ovl_emsxms_write	PROC	NEAR
	mov	bx,ohpoht_handle	; get source handle
	cmp	$$_ovl_oht,0		; see if stashing to XMS
	je	owr_ems				; no, write to EMS

	mov	WORD PTR ohtbuff.os_len,cx	; length of block
	mov	ohtbuff.os_dest_handle,bx	; destination handle
    mov WORD PTR ohtbuff.os_src_offset,si	; source low word offset (offset)
    mov WORD PTR ohtbuff.os_src_offset+2,dx	; source high word offset (segment)
	mov	ax,WORD PTR emsxms_offset+2
	add	di,WORD PTR emsxms_offset	; compute destination offset
	adc	ax,0
	mov	WORD PTR ohtbuff.os_dest_offset,di	; offset of destination, low word
	mov	BYTE PTR ohtbuff.os_dest_offset+2,al	; offset of destination, high word low byte
	xor	ax,ax
	mov	WORD PTR ohtbuff.os_len+2,ax	; zero high word of length
	mov	BYTE PTR ohtbuff.os_dest_offset+3,al	; zero high byte of destination offset
	mov	ohtbuff.os_src_handle,ax	; zero source handle (conventional memory)

    mov ah,0bh				; move extended memory block
    mov si,OFFSET ohtbuff	; ds:si -> parameter block
    call    DWORD PTR xms_addr
    call    check_xms_error	; see if error occurred
	ret

owr_ems:
	cmp	$$_ems3_flag,0		; see if using EMS 3.0 compatibility
	jne	owr_ems3			; yes

	mov	WORD PTR ohpbuff.os_len,cx	; length of block
	mov	ohpbuff.os_dtype,1	; expanded memory destination type
	mov	ohpbuff.os_dhandle,bx	; destination handle
	mov	ohpbuff.os_soffset,si	; source offset
	mov	ohpbuff.os_ssegpage,dx	; source segment
	mov	dx,WORD PTR emsxms_offset+2
	mov	ax,WORD PTR emsxms_offset
	add	ax,di				; add in offset
	adc	dx,0
	mov	bx,ax
	and	ah,3fh				; mask to 16K page
	shl	bx,1				; convert bytes in dx:bx to page with remainder truncation
	rcl	dx,1
	shl	bx,1
	rcl	dx,1				; dx==page number
	mov	ohpbuff.os_dsegpage,dx	; logical page number

owr_loop:
	cmp	ax,4000h			; see if page overflow
	jb	owr_off
	sub	ax,4000h			; drop offset
	inc	ohpbuff.os_dsegpage	; increase logical page number
	jmp	SHORT owr_loop		; drop until si is in range

owr_off:
	mov	ohpbuff.os_doffset,ax	; offset of destination

	xor	ax,ax
	mov	ohpbuff.os_stype,al	; conventional memory source type
	mov	ohpbuff.os_shandle,ax	; source handle

    mov ax,5700h			; move memory region
    mov si,OFFSET ohpbuff	; ds:si -> parameter block
    int 67h
	call	check_ems_error
	ret

; cx==count of chars to transfer
; bx==handle of EMS page
; si==source offset
; di==destination offset
; dx==source segment
owr_ems3:
	push	es				; save critical register
	mov	es,dx				; es:si -> source

	mov	dx,bx
	mov	ah,47h				; save page map
    int 67h
	call	check_ems_error

	mov	bx,WORD PTR emsxms_offset+2
	add	di,WORD PTR emsxms_offset
	adc	bx,0
	mov	ax,di				; byte offset in bx:ax
	and	di,3fffh			; mask to 16K page
	shl	ax,1				; convert bytes in bx:ax to page with remainder truncation
	rcl	bx,1
	shl	ax,1
	rcl	bx,1				; bx==page number

	push	ds				; save critical registers
	push	es
	mov	es,$$_ovl_ems_base	; es:di -> destination
	pop	ds					; ds:si -> source

owr_map:
    mov ax,4400h			; map expanded memory page to page 0
    int 67h
	call	check_ems_error

; copy to destination
	mov	ax,cx
	add	ax,di
	cmp	ax,4000h			; see if overflow to next page
	jbe	owr_noover			; no

; transfer to end of page
	mov	ax,4000h
	sub	ax,di				; get chars to transfer for this page
	sub	cx,ax				; update total transferred char count
	xchg	ax,cx			; ax==total transfer char count, cx==page transfer char count
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
	mov	cx,ax				; total transfer char count back in cx

	inc	bx					; move to next logical page
	xor	di,di				; zero offset on next page
	jmp	SHORT owr_map		; do next page transfer

owr_noover:
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

	mov	ah,48h				; restore page map
    int 67h
	call	check_ems_error
	pop	ds					; restore critical registers
	pop	es
	ret
ovl_emsxms_write	ENDP

;*****************************
;* $$_OVL_LOAD_OVERLAY       *
;*****************************

; load overlay from .OVL file, performing segment fixups as necessary
; destroys ax,bx,cx,dx

$$_ovl_load_overlay PROC    NEAR
    nop                     ; trapped by profiler
	mov emsxms_stash_flag,0	; init stash flag to failure status
    push    es              ; save critical register
    push    di

    mov di,ofh_near_vector  ; save offset to near vector

; maintain load/reload count for profiler
    cmp cs:is_calling_ovl, 0  ; load or reload?
    je load_1
    add word ptr $$_ovl_load_count+0,1  ; increment load count
    adc word ptr $$_ovl_load_count+2,0
    jmp SHORT load_2
load_1:
    add word ptr $$_ovl_reload_count+0,1  ; increment reload count
    adc word ptr $$_ovl_reload_count+2,0
load_2:

; check if can load from overlay file stashed in EMS or XMS
	cmp	info_from_emsxms_flag,0
	je	load_seek			; overlay not stashed
	call	ovl_emsxms_load_ovl	; load overlay from EMS/XMS
	jmp	SHORT load_fixup

; seek to overlay file code
load_seek:
    mov bx,$$_ovl_file_handle   ; get handle of overlay file
    cmp ofh_reloc_count,0   ; see if any relocation items
    je load_3               ; no, file pointer already at overlay file code

    mov dx,ofh_ovl_offset   ; get offset to overlay code
    sub dx,10               ; adjust for system info bytes read in
    xor cx,cx               ; zero high word of offset
    mov ax,4201h            ; move file pointer, relative to current file position
    int 21h

; load overlay file code
load_3:
    push    ds              ; save -> overlay manager data
    xor dx,dx               ; zero load offset
    mov cx,ofh_ovl_size     ; get size of overlay in paragraphs
    shl cx,1                ; convert to bytes, should always be <64K
    shl cx,1                ; x4
    shl cx,1                ; x8
    shl cx,1                ; x16

err_retry7:
    mov ds,$$_ovl_load_seg  ; ds:dx -> read buffer

;***    mov ah,3fh              ; read file
;***    int 21h
	call	read_to_ems

    jnc load_4              ; no errors

    call NEAR PTR $$_ovl_dos_error
    jmp SHORT err_retry7

load_4:
	pop	ds					; restore ds -> overlay manager data

; check if can stash overlay in EMS or XMS, including header info
	call	ovl_emsxms_stash_ovl

; perform segment fixups on overlay vectors
load_fixup:
	push	ds				; save -> overlay manager data
    mov ds,$$_ovl_load_seg  ; ds:dx -> read buffer, KEEP FLAG STATUS
    mov ax,$$_ovl_code      ; get current segment of overlay manager code
    mov ds:[di+0dh],ax      ; fixup near vector
    mov ds:[di+1ah],ax      ; fixup shared vector
    pop ds                  ; restore ds -> overlay manager data

; seek to start of relocation table if any relocation items
    cmp ofh_reloc_count,0   ; see if any relocation items
    je load_ret             ; no

    mov dx,WORD PTR $$_ovl_file_offset  ; get low word of overlay file offset
    mov cx,WORD PTR $$_ovl_file_offset+2    ; get high word of overlay file offset
    add dx,10               ; adjust past system info bytes to start of relocation entries
    adc cx,0

; bypass file seek if loading from EMS or XMS, but update $$_ovl_file_offset
	cmp	info_from_emsxms_flag,0
	je	load_seek2			; overlay not stashed
    mov WORD PTR $$_ovl_file_offset,dx  ; update overlay file offset
    mov WORD PTR $$_ovl_file_offset+2,cx
	jmp	SHORT load_loop

load_seek2:
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h
	mov	WORD PTR $$_ovl_file_offset,10	; init low word overlay file offset to reloc entries in case of stashing

; load and fixup relocation items, if any.  Reload as necessary
load_loop:
    cmp ofh_reloc_count,0   ; see if any more relocation items
    je load_ret             ; nope, done
    call    $$_ovl_load_reloc   ; load relocation entries (up to 512)
    call    $$_ovl_fixup    ; perform relocation fixups
    jmp SHORT load_loop     ; loop back for more

load_ret:
	cmp	emsxms_stash_flag,0	; see if stashing current overlay
	je	load_out			; no

; update free EMS/XMS space
	mov	ax,ofh_ovl_offset	; subtract off overlay file header size
	sub	WORD PTR emsxms_free,ax
	sbb	WORD PTR emsxms_free+2,0
	add	WORD PTR emsxms_offset,ax	; update stash offset
	adc	WORD PTR emsxms_offset+2,0
	mov	ax,ofh_ovl_size
	mov	cl,4
	shl	ax,cl				; convert overlay size in paras to bytes
	sub	WORD PTR emsxms_free,ax	; subtract off overlay size
	sbb	WORD PTR emsxms_free+2,0
	add	WORD PTR emsxms_offset,ax	; update stash offset
	adc	WORD PTR emsxms_offset+2,0

load_out:
    pop di					; restore critical register
    pop es
    ret
$$_ovl_load_overlay ENDP

;*****************************
;* OVL_EMSXMS_LOAD_OVL       *
;*****************************

; load from overlay file stashed in EMS or XMS
; destroys ax,bx,cx,dx

ovl_emsxms_load_ovl	PROC	NEAR
	push	si				; save critical register
	push	di

	xor	di,di				; zero offset within load area
    mov cx,ofh_ovl_size     ; get size of overlay in paragraphs
    shl cx,1                ; convert to bytes, should always be <64K
    shl cx,1				; x4
    shl cx,1				; x8
    shl cx,1				; x16
	mov	si,WORD PTR $$_ovl_file_offset	; source offset
	mov	bx,WORD PTR $$_ovl_file_offset+2	; logical page number/high word source offset
	and	bh,3fh				; mask off EMS/XMS flag bits

	test	info_from_emsxms_flag,80h	; see if XMS flag set
	jne	oel_xms				; yes

; EMS, when adjusting source offset must check if flowing into next page
	add	si,ofh_ovl_offset	; si -> code after header info
	cmp	si,4000h			; see if into next page
	jb	oel_readin			; no
	inc	bx					; bump page count
	and	si,3fffh			; mask to page
	jmp	SHORT oel_readin

; XMS, adjust source offset in si,bl
oel_xms:
	add	si,ofh_ovl_offset	; si -> code after header info
	adc	bl,0				; carry to high byte

; bx==logical page number (EMS)/high byte source offset (XMS)
; cx==count of chars to transfer
; si==source offset
; di==destination offset
oel_readin:
    mov dx,$$_ovl_load_seg  ; dx==destination segment
	call	ovl_emsxms_read	; read in the info from EMS/XMS
	pop	di					; restore critical register
	pop	si
	ret
ovl_emsxms_load_ovl	ENDP

;*****************************
;* OVL_EMSXMS_STASH_OVL      *
;*****************************

; check if can stash overlay in EMS or XMS, including header info
;   update file offset table if so
; upon entry jump_segment variable holds overlay identifier,
; destroys ax,cx,dx

ovl_emsxms_stash_ovl	PROC	NEAR
	mov	al,$$_ovl_ohp
	or	al,$$_ovl_oht		; see if stashing was indicated
	je	oes_fail			; no

; check if EMS/XMS free is >= size of stash
	mov	ax,ofh_ovl_size
	mov	cl,4
	shl	ax,cl				; convert overlay size in paras to bytes
	xor	cx,cx
	add	ax,ofh_ovl_offset	; add in overlay file header size
	adc	cx,0				; carry to high word, total size in cx:ax
	cmp	cx,WORD PTR emsxms_free+2
	ja	oes_fail			; not enough room
	jb	oes_okay			; enough room

; high words match, check low
	cmp	ax,WORD PTR emsxms_free
	jbe	oes_okay			; enough room

; not enough room to stash this overlay
oes_fail:
	ret						; return with stash flag reset

oes_okay:
	push	bx				; save critical register
	push	si
	push	di
	mov	cx,10				; overlay system header size
    mov si,OFFSET $$_ovl_file_header
	xor	di,di
	mov	dx,ds

; write header info to stash
; cx==count of chars to transfer
; di==destination offset relative to emsxms_offset start position
; si==source offset
; dx==source segment
	call	ovl_emsxms_write	; write info to EMS/XMS

; write overlay code to stash
	mov	di,ofh_ovl_offset
    mov cx,ofh_ovl_size     ; get size of overlay in paragraphs
    shl cx,1                ; convert to bytes, should always be <64K
    shl cx,1				; x4
    shl cx,1				; x8
    shl cx,1				; x16
    mov dx,$$_ovl_load_seg  ; overlay load (source) segment
	xor	si,si				; zero offset within overlay load area
	call	ovl_emsxms_write	; write info to EMS/XMS

; calculate page/offset (EMS) or offset (XMS) in dx:bx to update file position table
	cmp	$$_ovl_oht,0		; see if XMS stashing
	je	oes_ems				; no
	mov	bx,WORD PTR emsxms_offset
	mov	dx,WORD PTR emsxms_offset+2
	mov	al,80h				; XMS stash flag
	jmp	SHORT oes_update

oes_ems:
	mov	bx,WORD PTR emsxms_offset
	mov	cx,bx
	and	bh,3fh				; mask to 16K page
	mov	dx,WORD PTR emsxms_offset+2
	shl	cx,1
	rcl	dx,1
	shl	cx,1
	rcl	dx,1				; dx==page number
	mov	al,40h				; EMS stash flag

oes_update:
	or	dh,al				; set stash flags
	or	emsxms_stash_flag,al	; set stash flag to indicate successful stash

	push	ds				; save -> overlay manager data
    mov si,cs:jump_segment	; get overlay identifier
    dec si					; make relative zero
    shl si,1				; make doubleword offset into file position table
    shl si,1
    mov ds,$$_ovl_filepos	; ds -> overlay file position table
    mov [si],bx				; save new file position
    mov [si+2],dx
	pop	ds					; restore ds -> overlay manager data

	pop	di					; restore critical register
	pop	si
	pop	bx

	ret
ovl_emsxms_stash_ovl	ENDP

;*****************************
;* $$_OVL_LOAD_RELOC         *
;*****************************

; load relocation table, if any
; update relocation items count
; destroys ax,bx,cx,dx

$$_ovl_load_reloc   PROC    NEAR

err_retry8:
    mov bx,$$_ovl_file_handle   ; get handle of overlay file
    mov dx,OFFSET $$_ovl_rel_table  ; ds:dx -> place to load table
    mov cx,ofh_reloc_count  ; get count of relocation items
    shl cx,1                ; cx holds byte count of items (2 bytes/item)
    cmp cx,1024             ; see if table would overflow
    jbe reloc_2             ; no
    mov cx,1024             ; set to maximum

; if loading from overlay file in EMS or XMS, get relocation entries
reloc_2:
	cmp	info_from_emsxms_flag,0
	je	reloc_read			; overlay not stashed
	call	ovl_emsxms_load_reloc
	ret

reloc_read:
    mov ah,3fh              ; read file
    int 21h
    jnc reloc_ret           ; no errors

    call NEAR PTR $$_ovl_dos_error
    jmp SHORT err_retry8

; if stashing overlay file to EMS or XMS, write relocation entries
reloc_ret:
	call	ovl_emsxms_stash_reloc
    ret
$$_ovl_load_reloc   ENDP

;*****************************
;* OVL_EMSXMS_LOAD_RELOC     *
;*****************************

; get relocation entries from stashed overlay in EMS/XMS
; update $$_ovl_file_offset with bytes read in
; upon entry cx==bytes to read, ds:dx -> destination area
; destroys ax,bx,cx,dx

ovl_emsxms_load_reloc	PROC	NEAR
	push	si				; save critical register
	push	di
	mov	di,dx
	mov	si,WORD PTR $$_ovl_file_offset	; source offset
	mov	bx,WORD PTR $$_ovl_file_offset+2	; logical page number/high word source offset
	and	bh,3fh				; mask off EMS/XMS flag bits
	mov	dx,ds

; update file offset for next load
	add	WORD PTR $$_ovl_file_offset,cx
	adc	WORD PTR $$_ovl_file_offset+2,0

; bx==logical page number (EMS)/high byte source offset (XMS)
; cx==count of chars to transfer
; dx==destination segment
; si==source offset
; di==destination offset
	call	ovl_emsxms_read	; read in the info from EMS/XMS
	pop	di					; restore critical register
	pop	si
	ret
ovl_emsxms_load_reloc	ENDP

;*****************************
;* OVL_EMSXMS_STASH_RELOC    *
;*****************************

; stash relocation entries from overlay in EMS/XMS if appropriate
; upon entry cx==bytes to stash, ds:dx -> source
; destroys ax,bx,cx,dx

ovl_emsxms_stash_reloc	PROC	NEAR
	cmp	emsxms_stash_flag,0	; see if stashing current overlay
	jne	oer_stash			; yes
	ret						; no

oer_stash:
	push	si				; save critical register
	push	di
	mov	si,dx
	mov	dx,ds
	mov	di,WORD PTR $$_ovl_file_offset	; running offset of relocation entries

; update relocation entry file offset for next stash
	add	WORD PTR $$_ovl_file_offset,cx

; cx==count of chars to transfer
; di==destination offset relative to emsxms_offset start position
; si==source offset
; dx==source segment
	call	ovl_emsxms_write	; write info to EMS/XMS
	pop	di					; restore critical register
	pop	si
	ret
ovl_emsxms_stash_reloc	ENDP

;*****************************
;* $$_OVL_FIXUP              *
;*****************************

; perform relocation entry segment fixups for overlay file
; destroys ax,bx,cx,dx,es

$$_ovl_fixup    PROC    NEAR
    push    si              ; save critical register
    push    di
    mov ax,ofh_reloc_count  ; get count of relocation items
    mov cx,ax
    cmp ax,512              ; check if at least 512 items (full table)
    jae fixup_2             ; yes
    mov ax,512              ; force ax to minimum value for update by subtraction
    jmp SHORT fixup_3

fixup_2:
    mov cx,512              ; don't perform more than 512 fixups at once

fixup_3:
    sub ax,512              ; back off count of items fixed-up this pass (512 or all)
    mov ofh_reloc_count,ax  ; update count of fixup items
    mov dx,$$_ovl_load_seg
    mov es,dx               ; es -> overlay file code
    mov bx,OFFSET $$_ovl_rel_table  ; ds:bx -> relocation table

fixup_loop:
    mov si,[bx]             ; es:si -> overlay location to fixup
    or  si,si               ; see if zero (due to fixup of absolute segment being counted)
    je  fixup_5             ; yes, don't fixup value
    mov ax,es:[si]          ; get original value

; see if original value is nonzero.
; if so, assume root relative fixup
    or  ax,ax
    je  fixup_ovl           ; zero, overlay segment relative

; root relative fixup
    add ax,cs:prog_psp      ; add in program's psp segment
    add ax,10h              ; adjust for psp segment
    jmp SHORT fixup_4       ; bypass overlay segment relative fixup

fixup_ovl:
    mov ax,dx               ; use overlay load segment as fixup

fixup_4:
    mov es:[si],ax          ; update value

fixup_5:
    add bx,2                ; move to next relocation item
    loop    fixup_loop      ; loop until complete

    pop di                  ; restore critical register
    pop si
    ret
$$_ovl_fixup    ENDP

;*****************************
;* $$_OVL_SWAP_IN            *
;*****************************

; swap in all all active overlays swapped out by terminated overlay
; upon entry ds -> overlay manager data
; destroys ax,bx,cx

$$_ovl_swap_in  PROC    NEAR
    push    es              ; save critical register
    push    dx

; set stack to calling routine seg/id/offset stack
swap_loop:
    mov cs:$$_ovl_mgr_stack_ptr,sp  ; first save current stack pointer
    mov bx,cs:$$_ovl_stack_ptr
    mov ax,cs:$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    pop $$_ovl_activity     ; get activity level
    pop swapped_lru_high    ; get swapped out overlay LRU high word
    pop swapped_lru_low     ; get swapped out overlay LRU low word
    pop cs:jump_segment     ; get overlay identifier in jump_segment variable
    pop $$_ovl_load_seg     ; get overlay load segment
    mov cs:$$_ovl_stack_ptr,sp  ; save new stack pointer value

; set stack to overlay manager internal stack
    mov bx,cs:$$_ovl_mgr_stack_ptr
    mov ax,SEG $$_ovl_mgr_stack_end
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    call    xmsems_swapin   ; try to swap in from active overlay stash area, if there
    or  al,al               ; see if success
    jne swap_loaded         ; yes

    call    $$_ovl_get_info     ; get overlay information
    call    $$_ovl_load_overlay ; load the overlay

swap_loaded:
    mov es,$$_ovl_array_start   ; es-> array element for new overlay

swap_loop2:
    mov ax,es:[0]
    or  ax,ax               ; see if array element used
    je  swap_3              ; no, use it
    mov ax,es
    inc ax
    mov es,ax               ; bump to next array element
    jmp SHORT swap_loop2    ; loop until unused array element found

swap_3:
    push    es              ; save -> unused array element
    mov dx,$$_ovl_load_seg  ; ax holds start paragraph
    mov bx,dx
    add bx,ofh_ovl_size     ; add in size of overlay in paragraphs to get end paragraph

; check if swapped in overlay overwrites any nonactive overlays
    mov ax,$$_ovl_array_start
    mov es,ax               ; es -> overlay array
    mov cx,$$_ovl_loaded_count  ; get count of loaded overlays to check

swap_zero_loop:
    jcxz    swap_4          ; no more overlays to check
    mov ax,es:[0]           ; get overlay identifier number
    or  ax,ax               ; check overlay id
    je  swap_next           ; zero id number

    dec cx                  ; drop count of overlays to check
    cmp ax,cs:jump_segment  ; see if swapped-in overlay matches previous overlay
    je SHORT do_swap        ; yes, zero out previous overlay entry and activate this one

swap_cont:
    cmp dx,es:[8]           ; check swap-in start against entry overlay end
    jae swap_next           ; above end, overlay not swapped
    cmp bx,es:[6]           ; check swap-in end against entry overlay start
    jbe swap_next           ; below start, overlay not swapped

do_swap:
    xor ax,ax
    mov es:[0],ax           ; zero overlay identifier for this array element
    mov es:[4],ax           ; zero activity level
    dec $$_ovl_loaded_count ; drop count of loaded overlays

swap_next:
    mov ax,es               ; get array element segment
    inc ax                  ; bump to next element
    mov es,ax               ; update array element pointer
    jmp SHORT swap_zero_loop    ; loop back until all loaded overlays checked

; all non-active overlays overwritten by swapped-in overlay have their entries zeroed
; update swapped-in, active overlay entry data
swap_4:
    pop es                  ; es -> unused array element
    mov es:[6],dx           ; save start paragraph
    mov es:[8],bx           ; save end paragraph

    mov ax,cs:jump_segment
    mov es:[0],ax           ; save overlay identifier

    mov ax,$$_ovl_activity
    mov es:[4],ax           ; save activity level

; set LRU counter to what it was before swapout
    mov ax,swapped_lru_low
    mov es:[10],ax
    mov ax,swapped_lru_high
    mov es:[12],ax
    inc $$_ovl_loaded_count     ; increment count of loaded overlays

swap_decstack:
    dec cs:$$_ovl_stackcount    ; drop count of loaded overlays on stack
    je  swap_ret            ; done
    jmp NEAR PTR swap_loop  ; more loaded overlays on stack

swap_ret:
    pop dx                  ; restore critical register
    pop es
    ret
$$_ovl_swap_in  ENDP

;*****************************
;* $$_OVL_DOS_ERROR          *
;*****************************

; DOS error occurred in overlay manager
; error code in ax
; if installed error handler, destroy ax,cx,bx,dx
; otherwise, trash registers as necessary, this procedure terminates to DOS

$$_ovl_dos_error    PROC    NEAR
    push    ds              ; save critical register
    mov dx,SEG _ovlmgr_error_vector
    mov ds,dx               ; ds -> error handler data segment
    mov dx,WORD PTR _ovlmgr_error_vector
    mov cx,WORD PTR _ovlmgr_error_vector+2
    or  dx,cx               ; see if installed error handler
    je  doserr2             ; no, address is zero
	cmp	cx,cs:prog_psp		; additional check for valid error handler
	jbe	doserr2				; invalid

; installed error handler
    mov cx,ss
    mov dx,sp
    xchg    cx,cs:prog_ss_store ; save overlay manager ss, get program ss in cx
    xchg    dx,cs:prog_sp_store ; save overlay manager sp, get program sp in dx
    mov ss,cx
    mov sp,dx               ; restore program stack values
    push    ax
    call    DWORD PTR _ovlmgr_error_vector  ; call error handler
    pop ax
    mov cx,ss
    mov dx,sp
    xchg    cx,cs:prog_ss_store ; save program ss, get overlay manager ss in cx
    xchg    dx,cs:prog_sp_store ; save program sp, get overlay manager sp in dx
    mov ss,cx
    mov sp,dx               ; restore overlay manager stack values
    pop ds                  ; restore critical register
    ret                     ; return and try again

doserr2:
    mov dx,OFFSET dos_err_text  ; ds:dx -> message to write
    mov cx,24

$$_ovl_err_shared:
    mov si,ax               ; save error code
    mov ax,$$_ovl_data
    mov ds,ax               ; ds -> overlay manager data
    mov bx,STDOUT           ; write error message to standard output
    mov ah,40h              ; write to device
    int 21h

    mov ax,si               ; restore error code to ax
    mov ah,al
    and ah,15               ; only save low nybble of error code
    shr al,1                ; convert high nybble of error code to relative zero
    shr al,1
    shr al,1
    shr al,1

; low 4 bits of value in ah, high 4 bits in al
    cmp al,9                ; see if need to convert high nybble to hex code
    jbe shared_2            ; no
    add al,7                ; adjust for alpha hex char

shared_2:
    add al,30h              ; make ASCII representation
    cmp ah,9                ; see if need to convert low nybble to hex code
    jbe shared_3            ; no
    add ah,7                ; adjust for alpha hex char

shared_3:
    add ah,30h              ; make ASCII representation
    mov di,OFFSET code_text ; point to code to modify
    mov [di],ax             ; modify '00' code to actual value in hex

    mov dx,di               ; ds:dx -> message to print
    mov cx,5                ; print 5 chars
    mov ah,40h              ; write to device
    int 21h

    mov ax,si               ; get low byte of error code in al
    mov ah,4ch              ; terminate with return code
    int 21h
$$_ovl_dos_error    ENDP

;*****************************
;* $$_OVL_OP_ERROR           *
;*****************************

; Operation error occurred in overlay manager
; error code in ax
; trash registers as necessary, this procedure terminates to DOS or error handler

$$_ovl_op_error     PROC    NEAR
    mov dx,SEG _ovlmgr_error_vector
    mov ds,dx               ; ds -> error handler data segment
    mov dx,WORD PTR _ovlmgr_error_vector
    mov cx,WORD PTR _ovlmgr_error_vector+2
    or  dx,cx               ; see if installed error handler
    je  operr2              ; no, address is zero
	cmp	cx,cs:prog_psp		; additional check for valid error handler
	jbe	operr2				; invalid

; installed error handler
    or  ah,80h              ; set high bit to indicate overlay manager error
    mov cx,ss
    mov dx,sp
    xchg    cx,cs:prog_ss_store ; save overlay manager ss, get program ss in cx
    xchg    dx,cs:prog_sp_store ; save overlay manager sp, get program sp in dx
    mov ss,cx
    mov sp,dx               ; restore program stack values
    push    ax
    call    DWORD PTR _ovlmgr_error_vector  ; call error handler

; if they are fool enough to return, drop through to regular handler

operr2:
    mov dx,OFFSET ovl_err_text
    mov cx,36
    jmp SHORT $$_ovl_err_shared ; jump to code shared with DOS error

$$_ovl_op_error     ENDP

;*****************************
;* _OVLMGR_CLOSE_OVL_FILE    *
;*****************************

; provide a way for application to externally close overlay file
; returns ax==dos error code if unsuccessful close
; destroys ax

_ovlmgr_close_ovl_file  PROC    FAR
    push    ds              ; save critical register
    push    bx
    mov ax,$$_ovl_data
    mov ds,ax               ; ds -> overlay manager data
    mov bx,$$_ovl_file_handle   ; get handle of overlay file
    or  bx,bx               ; make sure nonzero (valid handle)
    je  oco_2               ; zero, file not open
    mov ah,3eh              ; close file
    int 21h

oco_2:
    pop bx
    pop ds
    ret
_ovlmgr_close_ovl_file  ENDP

;*****************************
;* _OVLMGR_OPEN_OVL_FILE     *
;*****************************

; provide a way for application to externally open overlay file
; returns ax==dos error code if unsuccessful open
; destroys ax

_ovlmgr_open_ovl_file   PROC    FAR
    push    ds              ; save critical register
    push    dx
    mov dx,$$_ovl_data
    mov ds,dx
    mov dx,OFFSET _ovlmgr_overlay_filename  ; ds:dx -> filename to open
    mov ax,3d40h            ; open file for read access
    int 21h
    mov $$_ovl_file_handle,ax   ; save overlay file handle, if no error
    jc  ooo_ret             ; error occurred
    xor ax,ax               ; zero file handle, indicate no errors on return

ooo_ret:
    pop dx
    pop ds
    ret
_ovlmgr_open_ovl_file   ENDP

;*****************************
;* _OVLMGR_FREE_EMS          *
;*****************************

; provide a way for application to free EMS used by overlay manager
; returns ah==EMS error code if unsuccessful
; destroys ax

_ovlmgr_free_ems    PROC    FAR
_om_fr_ems  EQU $
    push    ds              ; save critical register
    push    dx
    mov dx,$$_ovl_data
    mov ds,dx

    xor ax,ax               ; assume no error
    cmp $$_ovl_ems_pool_flag,al ; see if any EMS to free
    je  ofe_ret             ; no

    mov dx,$$_ovl_ox_handle	; get EMS handle for /ox
    xor al,al
    mov ah,45h              ; release handle and memory pages
    int 67h

ofe_ret:
    pop dx
    pop ds
    ret
_ovlmgr_free_ems    ENDP

;*****************************
;* NEW_TERMINATE             *
;*****************************

; perform termination code
; destroys no registers

new_terminate   PROC    FAR
    push    ax              ; save all used registers
    push    bx
    push    dx
    push    ds
    call    _ovlmgr_free_ems

    mov ax,$$_ovl_data
    mov ds,ax
    mov dx,orport_handle
    cmp $$_ovl_orp,0        ; see if expanded memory to free
    je  nt_2                ; no
    mov ah,45h              ; release EMS handle and pages
    int 67h

nt_2:
    cmp $$_ovl_ort,0        ; see if extended memory to free
    je  nt_3                ; no
    mov ah,0ah              ; free extended memory block
    call    DWORD PTR xms_addr

nt_3:
    cmp $$_ovl_umb,0        ; see if UMB to free
    je  nt_4                ; no
	cmp	dos5_umb_flag,0		; see if DOS 5 umb
	jne	nt_4				; yes, don't go through XMS
    mov ah,11h              ; free upper memory block
	mov	dx,cs:$$_ovl_load_start	; get segment base of block
    call    DWORD PTR xms_addr

nt_4:
    mov dx,ohpoht_handle
    cmp $$_ovl_oht,0        ; see if XMS to free
    je  nt_5                ; no
    mov ah,0ah              ; free extended memory block
    call    DWORD PTR xms_addr

nt_5:
    cmp $$_ovl_ohp,0        ; see if EMS to free
    je  nt_6                ; no
    mov ah,45h              ; release EMS handle and pages
    int 67h

nt_6:
    pop ds
    pop dx
    pop bx
    pop ax
    jmp DWORD PTR cs:psp_terminate  ; transfer to old terminate address
new_terminate   ENDP

;*****************************
;* UMB_CHECK_SETUP           *
;*****************************

; check if UMB is available, get largest if so
; returns carry flag set on error, reset if okay
; destroys bx,cx

umb_check_setup	PROC	NEAR
	push	ax
	push	dx				; save critical register

    call    xms_check_setup ; check that XMS exists, setup if so
    jc  ucs_noumb           ; carry if error
	mov	ah,10h				; allocate upper memory block
	mov	dx,0fffeh			; force failure
    call    DWORD PTR xms_addr
	mov	umb_avail,dx		; save largest size available
	cmp	bl,0b0h				; error code must be smaller UMB available or no UMB
	je	ucs_ret				; okay

; error accessing UMB, check if DOS 5 UMB's exist
ucs_noumb:
	mov	ax,3000h			; get dos version number
	int	21h
	cmp	al,5				; see if version 5 or greater
	jb	ucs_cancel			; no

	mov	ax,5800h			; get memory allocation strategy
	int	21h
	jc	ucs_cancel			; error occurred
	mov	dl,al				; save old strategy in dl
	mov	ax,5802h			; get UMB link state
	int	21h
	jc	ucs_cancel			; error occurred
	mov	dh,al				; save link state in dh

	mov	bx,40h				; high memory first fit
	mov	ax,5801h			; set allocation strategy
	int	21h
	jc	ucs_cancel			; error occurred
	mov	bx,1				; UMB's part of memory chain
	mov	ax,5803h			; set UMB link state
	int	21h
	mov	bx,0fffeh			; force failure
	mov	ah,48h				; allocate memory
	int	21h					; will always give error, bx has largest free
	mov	umb_avail,bx
	mov	bl,dh				; get old link state
	xor	bh,bh				; zero high byte
	mov	ax,5803h			; set UMB link state
	int	21h
	mov	bl,dl				; get old strategy
	xor	bh,bh				; zero high byte
	mov	ax,5801h			; set allocation strategy
	int	21h
	mov	dos5_umb_flag,1		; flag dos 5 umbs in use
	jmp	SHORT ucs_ret

; no XMS UMB'S, no DOS 5 UMB's
ucs_cancel:
    mov $$_ovl_umb,0        ; reset UMB flag

ucs_ret:
	pop	dx					; restore critical register
	pop	ax
	ret
umb_check_setup	ENDP

;*****************************
;* UMB_POOL_ALLOC            *
;*****************************

; allocate UMB for overlay pool
; upon entry cx==pool size in paras
; destroys ax,bx,cx

umb_pool_alloc	PROC	NEAR
	push	dx				; save critical register
	cmp	dos5_umb_flag,0		; see if dos 5 umb allocation
	je	upa_not5			; no

; use DOS 5 memory chain linked umb
	mov	ax,5800h			; get memory allocation strategy
	int	21h
	jc	upa_dos5bomb		; error occurred
	mov	dl,al				; save old strategy in dl
	mov	ax,5802h			; get UMB link state
	int	21h
	jc	upa_dos5bomb		; error occurred
	mov	dh,al				; save link state in dh

	mov	ax,5801h			; set allocation strategy
	mov	bx,40h				; high memory first fit
	int	21h
	jc	upa_dos5bomb		; error occurred
	mov	bx,1				; UMB's part of memory chain
	mov	ax,5803h			; set UMB link state
	int	21h
	mov	bx,cx				; get number of paras to allocate
	mov	ah,48h				; allocate memory
	int	21h
	pushf					; save allocation error flag (must reset old strategy)
	push	ax				; save segment of allocated block
	mov	bl,dh				; get old link state
	xor	bh,bh				; zero high byte
	mov	ax,5803h			; set UMB link state
	int	21h
	mov	bl,dl				; get old strategy in bx
	xor	bh,bh				; zero high byte
	mov	ax,5801h			; set allocation strategy
	int	21h
	pop	bx					; bx == segment of allocated block
	popf					; restore allocation error status
	jc	upa_dos5bomb		; error occurred during allocation

	mov	dx,cx				; cx holds paras allocated
	jmp	SHORT upa_update	; success, update overlay variables

; previous DOS 5 UMB size determination bombed during allocation
; VERY SLEAZY, POP OFF RETURN ADDRESS AND JUMP BACK TO NO UMB CODE
upa_dos5bomb:
	pop	dx					; restore dx
	pop	ax					; kill return address
	jmp	NEAR PTR init_noumb	; nasty kludge jump back

upa_not5:
	mov	ah,10h				; allocate upper memory block
	mov	dx,cx				; get paras to allocate
    call    DWORD PTR xms_addr
    call    check_xms_error ; see if error occurred

upa_update:
	mov	ax,dx				; get paras allocated
    mov $$_ovl_amt_alloc,ax ; save amount of memory allocated
    dec ax                  ; make relative zero
	mov	cl,5
	shr	ax,cl				; convert to 512 blocks, /32
    inc ax                  ; adjust for beginning block
    mov $$_ovl_amt_512,ax   ; save 512 blocks allocated

	mov	ax,bx				; get overlay load area segment
    mov cs:$$_ovl_load_start,ax
    add ax,$$_ovl_amt_alloc ; calculate paragraph just above overlay load area
    mov cs:$$_ovl_load_end,ax   ; save to memory variable
	pop	dx					; restore critical register
	ret
umb_pool_alloc	ENDP

;*****************************
;* EMS_CHECK_SETUP           *
;*****************************

; check if EMS is available, setup if so
; returns carry flag set on error, reset if okay
; destroys ax,bx

ems_check_setup	PROC	NEAR
	push	ax
	push	dx
    mov dx,OFFSET emmname   ; ds:dx -> device name
    mov ax,3d00h            ; open for reading
    int 21h
    jc  ecs_noems           ; open failed

    mov bx,ax               ; bx holds handle
    mov ax,4400h            ; IOCTL get device info
    int 21h
    jc  ecs_noems           ; IOCTL call failed

    and dl,80h              ; get high bit, set if char device
    je  ecs_noems           ; file device

    mov ax,4407h            ; IOCTL get output status
    int 21h
    jc  ecs_noems           ; IOCTL call failed

    mov ah,3eh              ; close file
    int 21h
    jc  ecs_noems           ; close failed

    mov ah,40h              ; get EMS system status
    int 67h                 ; call EMM
    or  ah,ah               ; check for EMM error
    jne ecs_noems           ; EMM error occurred

    mov ah,46h              ; get EMM version
    int 67h
    or  ah,ah               ; check for error
    jne ecs_noems           ; error occurred
    cmp al,30h              ; must be EMS version 3.0 or greater
    jb	ecs_noems           ; bad version

    mov ah,41h              ; get page frame address
    int 67h
    or  ah,ah
    jne ecs_noems           ; EMM error occurred

    mov $$_ovl_ems_base,bx  ; save EMS base (page frame)
	clc						; clear carry flag to show no error
	pop	dx
	pop	ax
	ret

ecs_noems:
	stc						; set carry flag to show error
	pop	dx
	pop	ax
	ret
ems_check_setup	ENDP

;*****************************
;* OVL_EMS_POOLCHECK         *
;*****************************

; check if EMS is available for overlay pool, if not reset $$_ovl_ems_pool_flag
; destroys bx

ovl_ems_poolcheck	PROC    NEAR
    push    ax              ; save critical register
    push    dx

; check if /ox e-var is set
    mov al,$$_ovl_ox_evar   ; see if environment variable set for /ox
    or  al,al
    je  oec_chkemm          ; no, bypass check for proper setting

; check the environment block for the /ox e-var
    push    es
    push    si
    xor si,si               ; starting location for target string check
    mov ax,es:[2ch]         ; get environment segment from offset 2ch in PSP
    mov es,ax               ; es -> environment segment

ox_find_evar:
    xor bx,bx               ; offset into target string

ox_loop:
    mov al,es:[si]          ; get byte from environment string, point to next
    inc si                  ; point to next char in environment
    cmp al,[bx+OFFSET $$_ovl_ox_evar]   ; does environment char match ox string char
    je  ox_byte_match       ; yes, try next location
    or  al,es:[si]          ; two zero values in a row mean the end of the environment
    jne ox_find_evar        ; not the end of the environment
    jmp SHORT ox_fail       ; at end of environment, no ox environment char

; check that /ox e-var is not part of another environment string
ox_byte_match:
    or  bx,bx               ; bx is zero if first char is matched
    jne ox_2                ; not first char, test already done
    cmp si,1                ; si equals one if e-var is first string in environment block
    je  ox_2                ; no previous environment string
    cmp BYTE PTR es:[si-2],0    ; check if char before e-var was nonzero
    jne ox_find_evar        ; yes, e-var is a subset of another string, keep looking

ox_2:
    inc bx                  ; a match, move to next byte of target string
    cmp BYTE PTR [bx+OFFSET $$_ovl_ox_evar],0   ; see if at end of target
    jne ox_loop             ; not yet, keep comparing
    jmp SHORT ox_success    ; matched

ox_fail:
    pop si
    pop es
    jmp SHORT oec_noems     ; fail EMS usage

ox_success:
    pop si
    pop es

oec_chkemm:
	call	ems_check_setup	; get EMS status
	jnc	oec_checkmem		; checked out okay

; no, or can't use, EMS
oec_noems:
    mov $$_ovl_ems_pool_flag,0	; reset EMS flag

oec_ret:
    pop dx                  ; restore critical register
    pop ax
    ret

oec_checkmem:
    mov ah,42h              ; get number of pages
    int 67h
    or  ah,ah
    jne oec_noems           ; error occurred, don't use EMS
    cmp bx,4                ; see if at least 4 unused EMS pages
    jb  oec_noems           ; no
    mov bx,4                ; only allocate four pages
    mov ah,43h              ; allocate handle and pages
    int 67h
    or  ah,ah
    jne oec_noems           ; error occurred

    mov $$_ovl_ox_handle,dx	; save handle, if successful

; map in pages to use logical page 0->0, 1->1, 2->2, 3->3
    xor bx,bx
    mov al,bl
    mov ah,44h              ; map expanded memory page
    int 67h
    or  ah,ah               ; see if error occurred
    jne oec_noems           ; error occurred

    mov bx,1
    mov al,bl
    mov ah,44h              ; map expanded memory page
    int 67h
    or  ah,ah               ; see if error occurred
    jne oec_noems           ; error occurred

    mov bx,2
    mov al,bl
    mov ah,44h              ; map expanded memory page
    int 67h
    or  ah,ah               ; see if error occurred
    jne oec_noems           ; error occurred

    mov bx,3
    mov al,bl
    mov ah,44h              ; map expanded memory page
    int 67h
    or  ah,ah               ; see if error occurred
    jne oec_noems           ; error occurred

	mov	$$_ovl_umb,0		; reset UMB overlay pool flag
    jmp SHORT oec_ret       ; done

ovl_ems_poolcheck	ENDP

;*****************************
;* XMS_CHECK_SETUP           *
;*****************************

; check that XMS exists, setup if so
; returns carry flag set on error, reset if okay
; destroys bx,cx

xms_check_setup PROC    NEAR
    push    ax
    push    dx
    push    es
    mov ax,4300h            ; check if XMM is present
    int 2fh                 ; multiplex interrupt
    cmp al,80h              ; check if driver present
    jne xcs_noxms           ; no

    mov ax,4310h            ; get XMS driver entry point
    int 2fh
    mov WORD PTR xms_addr,bx    ; save it
    mov WORD PTR xms_addr+2,es
    clc						; return success indication

xcs_ret:
    pop es
    pop dx
    pop ax
    ret

; no, or can't use, XMS
xcs_noxms:
    stc                     ; set carry flag to indicate error
    jmp SHORT xcs_ret

xms_check_setup ENDP

;*****************************
;* GET_XMS_FOR_ORT           *
;*****************************

; allocate 128K XMS for /ort overlay swapout, if possible
; destroys bx,cx

get_xms_for_ort PROC	NEAR
    push    ax
    push    dx
    push    es
    call    xms_check_setup ; check that XMS exists, setup if so
    jc  gxt_noort           ; carry if error

    mov dx,128              ; allocate 128K for active overlay swapping
    mov ah,9                ; allocate extended memory block
    call    DWORD PTR xms_addr
    or  ax,ax               ; see if error
    je  gxt_noort           ; yes, don't use XMS for ort

    mov orport_handle,dx
    mov $$_ovl_orp,0        ; reset EMS flag as XMS is used
    jmp SHORT gxt_ret

gxt_noort:
    mov $$_ovl_ort,0        ; reset XMS flag

gxt_ret:
    pop es
    pop dx
    pop ax
    ret
get_xms_for_ort ENDP

;*****************************
;* GET_XMS_FOR_OHT           *
;*****************************

; allocate XMS for /oht overlay file stashing, if possible
; use $$_oht_size, $$_oht_flag, and $$_ovl_file_size to
;  compute stash size to allocate
; destroys ax,bx,cx,dx

get_xms_for_oht	PROC	NEAR
    call    xms_check_setup ; check that XMS exists, setup if so
    jc  gxh_nooht           ; carry if error

    mov ah,8                ; query free memory
    call    DWORD PTR xms_addr
    or  ax,ax               ; see if error
    je  gxh_nooht           ; yes, don't use XMS for /orh
	cmp	ax,80				; must be at least 80K XMS free in one block
	jb	gxh_nooht

	cmp	$$_oht_flag,0		; see if leave amount free
	je	gxh_leave			; yes

; allocate up to specified amount, ax holds K free, $$_oht_size holds K to allocate
	cmp	ax,$$_oht_size		; see if more than enough free to allocate to amount
	jbe	gxh_alloc			; no, use amount free
	mov	ax,$$_oht_size		; allocate amount free
	jmp	SHORT gxh_alloc

; leave free the amount of XMS in $$_oht_size
gxh_leave:
	mov	dx,$$_oht_size		; amount must leave free
	sub	ax,dx				; back off of total amount allocatable
	jc	gxh_nooht			; not enough free
	cmp	ax,80				; must be at least 80K XMS still free
	jb	gxh_nooht

; ax holds amount to allocate, up to $$_ovl_file_size
gxh_alloc:
	cmp	ax,$$_ovl_file_size	; see if amount to allocate > overlay file size
	jbe	gxh_doalloc			; no
	mov	ax,$$_ovl_file_size	; only allocate up to overlay file size

gxh_doalloc:
	mov	dx,ax				; requested block size in dx
	mov	cx,ax				; save size in K
	mov	ah,9				; allocate extended memory block
    call    DWORD PTR xms_addr
    or  ax,ax               ; see if error
    je  gxh_nooht           ; yes, don't use XMS for ort

    mov ohpoht_handle,dx

; convert 1K block count in cx to byte count in ax:cx (shift left 10 bits, *1024)
	xor	ah,ah
	mov	al,ch
	mov	ch,cl
	mov	cl,ah				; register moves do effective *256
	shl	cx,1
	rcl	ax,1
	shl	cx,1
	rcl	ax,1				; ax:cx==1024*block count
	mov	WORD PTR emsxms_free,cx
	mov	WORD PTR emsxms_free+2,ax
	mov	$$_ovl_ohp,0		; reset EMS file stashing flag
	ret

gxh_nooht:
    mov $$_ovl_oht,0        ; reset XMS file stashing flag
	ret
get_xms_for_oht	ENDP

;*****************************
;* OVL_LIM40_CHECK           *
;*****************************

; check that EMS version 4.0 (with lower memory mapping) exists
; returns carry flag set on error, reset if okay
; destroys ax,bx,dx

lim_ems40_check	PROC	NEAR
	call	ems_check_setup	; get EMS status
    jc  olc_noems           ; EMS access failed

    mov ah,46h              ; get EMM version
    int 67h
    or  ah,ah               ; check for error
    jne olc_noems           ; error occurred
    cmp al,40h              ; must be EMS version 4.0 or greater
    jb  olc_noems           ; no

    clc						; return success indication
	ret

; no, or can't use, EMS 4.0
olc_noems:
    stc                     ; set carry flag to indicate error
	ret
lim_ems40_check	ENDP

;*****************************
;* GET_EMS_FOR_ORP           *
;*****************************

; allocate 128K EMS for /orp overlay swapout, if possible
; destroys bx

get_ems_for_orp	PROC    NEAR
    push    ax
    push    dx
	call	lim_ems40_check	; get LIM EMS 4.0 status
    jc  gep_noems           ; EMS access failed

    mov bx,8                ; allocate 8 16K pages (128K)
    mov ah,43h
    int 67h
    or  ah,ah               ; check for error
    jne gep_noems           ; error occurred

    mov orport_handle,dx
    mov xbuff.xs_shandle,dx
    mov xbuff.xs_dhandle,dx

gep_ret:
    pop dx
    pop ax
    ret

; no, or can't use, EMS
gep_noems:
    mov $$_ovl_orp,0        ; reset EMS flag
    jmp SHORT gep_ret

get_ems_for_orp	ENDP

;*****************************
;* GET_EMS_FOR_OHP           *
;*****************************

; allocate EMS 4.0 pages for /ohp overlay file stashing, if possible
; use $$_ohp_size, $$_ohp_flag, and $$_ovl_file_size to
;  compute stash size to allocate
; destroys ax,bx,cx,dx

get_ems_for_ohp	PROC	NEAR
	call	ems_check_setup	; check for EMS 3.0 availability
    jc  gfp_noohp           ; EMS access failed

	call	lim_ems40_check	; get LIM EMS 4.0 status
    jnc gfp_2				; EMS 4.0 available

	mov	$$_ems3_flag,1		; set EMS 3.0 compability mode

gfp_2:
	cmp	$$_ovl_ems_pool_flag,0	; see if overlay pool in EMS page frame
	je	gfp_3				; no
	cmp	$$_ems3_flag,0		; see if EMS 3.0 compatibility mode
	je	gfp_3				; no

; cannot have overlay pool in EMS page frame and use EMS 3.0 overlay file stashing
; no, or can't use, EMS
gfp_noohp:
    mov $$_ovl_ohp,0        ; reset EMS flag
	ret

gfp_3:
	mov	ah,42h				; get number of pages
	int	67h
    or  ah,ah               ; check for error
    jne gfp_noohp           ; error occurred
	cmp	bx,5				; must at least 80K EMS free (5 pages)
	jb	gfp_noohp

	mov	ax,$$_ohp_size		; get stash size in K
	add	ax,15				; round up to next 16K page
	mov	cl,4
	shr	ax,cl				; /16, convert to 16K pages

	cmp	$$_ohp_flag,0		; see if leave amount free
	je	gfp_leave			; yes

; allocate up to specified amount, ax holds pages to allocate, bx holds pages free
	cmp	bx,ax				; see if more than enough free to allocate to amount
	jbe	gfp_alloc			; no, use amount free
	mov	bx,ax				; allocate amount free
	jmp	SHORT gfp_alloc

; leave free the amount of EMS in $$_ohp_size
gfp_leave:
	sub	bx,ax				; back off amount to leave free from total amount allocatable
	jc	gfp_noohp			; not enough free
	cmp	bx,5				; must be at least 5 pages EMS still free
	jb	gfp_noohp

; bx holds number of pages to allocate, up to $$_ovl_file_size
gfp_alloc:
	mov	ax,$$_ovl_file_size	; get overlay file size in K
	add	ax,15				; round up to next 16K page
	mov	cl,4
	shr	ax,cl				; /16, convert to 16K pages

; ax holds overlay file size in 16K blocks
	cmp	bx,ax				; check if amount to allocate > overlay file size
	jbe	gfp_doalloc			; no
	mov	bx,ax				; only allocate up to overlay file size

gfp_doalloc:
	mov	ah,43h				; allocate handle and pages
	int	67h
    or  ah,ah               ; check for error
    jne gfp_noohp           ; error occurred

    mov ohpoht_handle,dx

; convert 16K pages allocated in bx to byte count,
; make bx high word then shift right 2 bits (shift left 14 bits or *16384)
	xor	ax,ax
	shr	bx,1
	rcr	ax,1
	shr	bx,1
	rcr	ax,1				; byte count in bx:ax
	mov	WORD PTR emsxms_free,ax
	mov	WORD PTR emsxms_free+2,bx
	ret
get_ems_for_ohp	ENDP

;*****************************
;* CHK_XMSEMS_SWAPOUT        *
;*****************************

; upon entry es -> overlay loaded array entry
; destroys ax,di,si

chk_xmsems_swapout  PROC    NEAR
    mov al,$$_ovl_ort
    or  al,$$_ovl_orp
    jne cxs_2               ; active overlay swapout exists
    ret

cxs_2:
    push    bx
    push    cx
    push    dx
    push    es
    mov ax,es:[8]
    sub ax,es:[6]           ; get size of overlay in paras

    dec ax                  ; make relative zero
    mov cl,5
    shr ax,cl               ; size of overlay in 512 blocks, relative 0
    inc ax                  ; make relative 1
    mov bx,ax               ; bx holds entry count in table (512 block size)
    mov si,OFFSET xe_table
    mov dx,si               ; dx will track loading area (first zero entry) in case overlay doesn't fit

cxs_search1:
    mov cx,bx
    mov ax,cx               ; get current block
    dec ax                  ; make relative zero
    add ax,ax               ; make word offset
    add ax,si               ; ax holds last entry that will be checked
    cmp ax,OFFSET xe_table+512  ; see if past end of table
    jae cxs_3               ; yes

cxs_search2:
    lodsw                   ; get overlay id from entry
    or  ax,ax               ; see if nonzero
    jne cxs_search1         ; yes, reset count of overlay blocks to search through
    mov dx,si               ; save load area

cxs_nextent:
    loop    cxs_search2     ; loop through all entries

; found enough zero entries to store current overlay
    mov ax,bx               ; get entry count
    add ax,ax               ; word/entry
    sub dx,ax               ; dx -> load area entry
    jmp SHORT cxs_stash

; not enough zero entries to store current overlay, use dx as start of store
cxs_3:
    mov ax,es:[0]           ; get overlay identifier
    push    ds
    pop es
    mov cx,bx               ; cx holds overlay size in 512 blocks
    mov di,dx               ; es:di -> start of active overlay stash area

cxs_loop:
    cmp cx,1                ; see if zero'ing last entry
    jne cxs_4               ; no
    mov si,[di]             ; get last entry value

cxs_4:
    stosw                   ; update active overlay stash table entry
    loop    cxs_loop

; si holds last entry value, zero any following entries with same value
    mov ax,si

cxs_loop2:
    cmp di,OFFSET xe_table+512  ; see if at end of table
    jae cxs_5               ; yes
    scasw                   ; see if a match
    jne cxs_5               ; no
    mov WORD PTR es:[di-2],0    ; zero out entry (clean out partial)
    jmp SHORT cxs_loop2

cxs_5:
    pop es                  ; restore es -> overlay load array entry
    push    es

; dx -> start of active overlay stash area
; bx holds 512 block size
cxs_stash:
    mov di,dx
    mov ax,es:[0]
    push    ds
    pop es
    mov cx,bx
    rep stosw               ; update active overlay stash table

; now copy the overlay out to XMS/EMS memory
    mov ax,bx
    mov cl,9                ; x512
    shl ax,cl               ; ax holds bytes to load
    pop es                  ; restore es -> overlay load array entry
    push    es

    sub dx,OFFSET xe_table  ; adjust relative to start of table
    mov si,dx
    mov cl,6
    shr si,cl               ; si holds EMS logical page of active overlay  start
    xor cx,cx               ; convert entry count in dx into bytes in cx:dx
    mov dh,dl               ; register move does effective x256 value multiply
    xor dl,dl               ; zero low byte

;***    shr cx,1                ; /2 (256 bytes)
;***    rcr dx,1

    cmp $$_ovl_ort,0        ; see if swapped to extended memory
    je  cxs_expand          ; no

; swapped in extended memory
    mov WORD PTR emb.es_len,ax
    xor ax,ax
    mov WORD PTR emb.es_len+2,ax
    mov emb.es_src_handle,ax
    mov WORD PTR emb.es_src_offset,ax
    mov WORD PTR emb.es_dest_offset,dx
    mov WORD PTR emb.es_dest_offset+2,cx
    mov ax,orport_handle
    mov emb.es_dest_handle,ax
    mov ax,es:[6]           ; load segment
    mov WORD PTR emb.es_src_offset+2,ax

    mov ah,0bh              ; move extended memory block
    mov si,OFFSET emb       ; ds:si -> parameter block
    call    DWORD PTR xms_addr
    call    check_xms_error ; see if error occurred
    jmp SHORT cxs_ret

; swapped in expanded memory
cxs_expand:
    mov WORD PTR xbuff.xs_len,ax
    xor ax,ax
    mov WORD PTR xbuff.xs_len+2,ax
    mov xbuff.xs_stype,al
    mov xbuff.xs_soffset,ax
    mov ax,es:[6]
    mov xbuff.xs_ssegpage,ax
    mov xbuff.xs_dtype,1
    and dx,03fffh           ; round to 16K page
    mov xbuff.xs_doffset,dx
    mov xbuff.xs_dsegpage,si

    mov ax,5700h            ; move memory region
    mov si,OFFSET xbuff     ; ds:si -> parameter block
    int 67h
	call	check_ems_error

cxs_ret:
    pop es
    pop dx
    pop cx
    pop bx
    ret
chk_xmsems_swapout  ENDP

;*****************************
;* XMSEMS_SWAPIN             *
;*****************************

; returns al != 0 if successful swap-in from swapout area
; destroys ax,bx,cx,dx,es

xmsems_swapin   PROC    NEAR
    push    di
    push    si
    mov al,$$_ovl_ort
    or  al,$$_ovl_orp
    jne xs_2                ; active overlay stashing exists

xs_fail:
    xor al,al
    pop si
    pop di
    ret

xs_2:
    mov cx,256
    push    ds
    pop es
    mov di,OFFSET xe_table  ; di will offset into stash table
    mov ax,cs:jump_segment  ; ax holds overlay identifier
    repne   scasw
    jne xs_fail             ; not found

; at least 512 bytes of segment stashed
    mov WORD PTR es:[di-2],0    ; zero out swapped in overlay from stash
    mov cx,1
    mov dx,di
    sub dx,OFFSET xe_table+2    ; dx holds starting block word offset

xs_loop:
    cmp di,OFFSET xe_table+512  ; see if at end of stash table
    jae xs_3                ; yes
    scasw                   ; see if next entry matches too
    jne xs_3                ; no
    mov WORD PTR es:[di-2],0    ; zero out swapped in overlay from stash
    inc cx                  ; bump count of K stashes
    jmp SHORT xs_loop

; cx holds 512 bytes to load
xs_3:
    mov ax,cx
    mov cl,5
    shl ax,cl               ; ax holds paragraphs
    mov ofh_ovl_size,ax     ; fake out overlay manager, giving amount as read in size
    mov cl,4
    shl ax,cl               ; ax holds bytes to load

    mov si,dx
    mov cl,6
    shr si,cl               ; si holds EMS logical page of stash start
    xor cx,cx               ; convert entry offset in dx into bytes in cx:dx
    mov dh,dl               ; register move does effective x256 value multiply
    xor dl,dl               ; zero low byte

;***    shr cx,1                ; /2 (256 bytes)
;***    rcr dx,1

    cmp $$_ovl_ort,0        ; see if stashed in extended memory
    je  xs_expand           ; no

; swapped in extended memory
    mov WORD PTR emb.es_len,ax
    xor ax,ax
    mov WORD PTR emb.es_len+2,ax
    mov emb.es_dest_handle,ax
    mov WORD PTR emb.es_dest_offset,ax
    mov WORD PTR emb.es_src_offset,dx
    mov WORD PTR emb.es_src_offset+2,cx
    mov ax,orport_handle
    mov emb.es_src_handle,ax
    mov ax,$$_ovl_load_seg
    mov WORD PTR emb.es_dest_offset+2,ax

    mov ah,0bh              ; move extended memory block
    mov si,OFFSET emb       ; ds:si -> parameter block
    call    DWORD PTR xms_addr
    call    check_xms_error ; see if error occurred
    jmp SHORT xs_success

; swapped in expanded memory
xs_expand:
    mov WORD PTR xbuff.xs_len,ax
    xor ax,ax
    mov WORD PTR xbuff.xs_len+2,ax
    mov xbuff.xs_dtype,al
    mov xbuff.xs_doffset,ax
    mov ax,$$_ovl_load_seg
    mov xbuff.xs_dsegpage,ax
    mov xbuff.xs_stype,1
    and dx,03fffh           ; round to 16K page
    mov xbuff.xs_soffset,dx
    mov xbuff.xs_ssegpage,si

    mov ax,5700h            ; move memory region
    mov si,OFFSET xbuff     ; ds:si -> parameter block
    int 67h
	call	check_ems_error

xs_success:

; maintain load/reload count for profiler
    cmp cs:is_calling_ovl, 0  ; load or reload?
    je xs_load_1
    add word ptr $$_ovl_load_count+0,1  ; increment load count
    adc word ptr $$_ovl_load_count+2,0
    jmp SHORT xs_load_2
xs_load_1:
    add word ptr $$_ovl_reload_count+0,1  ; increment reload count
    adc word ptr $$_ovl_reload_count+2,0
xs_load_2:

    mov al,1
    pop si   
    pop di
    ret
xmsems_swapin   ENDP

;*****************************
;* CHECK_XMS_ERROR           *
;*****************************

; check if XMS error, ax is ZERO if error upon entry
; if error, bl has xms error upon entry
; convert ax to warplink error code

check_xms_error PROC	NEAR
    or  ax,ax
    je  cxe_err             ; error occurred

cxe_ret:
    ret

; XMS error occurred
cxe_err:
    cmp bl,0a0h             ; see if no more free memory (not really an error)
    je  cxe_ret             ; no more free memory
    mov al,bl               ; get 8 bit error code in al
    mov ah,1
    jmp NEAR PTR $$_ovl_op_error    ; all XMS errors are fatal, don't return

check_xms_error ENDP

;*****************************
;* CHECK_EMS_ERROR           *
;*****************************

; check if EMS error, ah is nonzero if error upon entry
; if error, bl has xms error upon entry
; convert ax to warplink error code

check_ems_error PROC	NEAR
    or  ah,ah				; see if error occurred
    jne	cee_err				; yes
    ret

; EMS error occurred
cee_err:
    mov al,ah               ; get 8 bit error code in al
    mov ah,2
    jmp NEAR PTR $$_ovl_op_error    ; all EMS errors are fatal, don't return

check_ems_error ENDP

;*****************************
;* READ_TO_EMS               *
;*****************************

; read from file to EMS transfer buffer, transfer to EMS, if EMS used
;  otherwise straight file read
; upon entry ds:dx-> EMS area to transfer to, bx == file handle,
;  cx holds byte count
; destroys NO registers other than typical function 3fh return values

read_to_ems PROC	NEAR
    push    ds
    mov ax,$$_ovl_data
    mov ds,ax               ; ds -> overlay manager data
    cmp $$_ovl_ems_pool_flag,0	; see if EMS page frame used for pool
    pop ds
    jne	rte_ems				; yes
    mov ah,3fh
    int 21h
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
    cmp cx,2048            	; write only 2K chunk max
    jbe rte_2
    mov cx,2048

rte_2:
    mov ax,$$_ovl_data
    mov ds,ax               ; ds -> overlay manager data
	mov	dx,OFFSET ems_trans_block
	mov	si,dx
    mov ah,3fh              ; read file
    int 21h
	pop	dx					; get bytes to read in dx
    jc  rte_out				; error reading file

    add bp,ax               ; update total bytes read
    mov cx,ax               ; get bytes read in cx

; transfer from transfer block to EMS block
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

    mov cx,dx				; cx==bytes to read
    sub cx,2048            	; subtract maximum read
    jbe rte_ret             ; all read
    cmp ax,2048				; see if actual bytes read matched requested
    je  rte_readloop        ; yes, not at end of file

rte_ret:
	clc						; clear carry for carry on file read is error checks
    mov ax,bp               ; get total bytes read in ax

rte_out:
    pop ds
    pop bp
    pop dx
    pop cx
    pop es
    pop di
    pop si
    ret
read_to_ems ENDP

$$_ovl_code ENDS

END
