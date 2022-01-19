;*********************************************************************
;*   MLPASS2.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          07/10/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker pass 2 main driver                                       *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlpass2
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
PUBLIC  pass2,get_symbol_offset
PUBLIC  no_match_okay

; variables
PUBLIC	filesize_text
PUBLIC	compression_flag

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   data_offset:DWORD
EXTRN	ovl_filesize:WORD
EXTRN   lookup_segment:WORD,ind_segment:WORD,segcall_segment:WORD
EXTRN	is_summer87:BYTE,is_clipper5:BYTE,current_clipmod_ptr:WORD

; initialized local variables

; byte values
no_match_okay   DB  0       ; nonzero if no match is okay on public declaration lookup
EVEN

.DATA?
; uninitialized local variables

; byte values
compression_flag	DB	0	; nonzero if Clipper symbol table compression in force

; word values
min_prog_size   LABEL   WORD    ; minimum program size in paragraphs
min_alloc_calc  DW  ?       ; largest overlay size in paragraphs rounded to 1K page

;*****************************
;* Constant data             *
;*****************************

.CONST

EVEN
image_text      DB  '$$_EXE_IMAGE_SIZE',0

EVEN

IFNDEF JUNIOR
filename_text   DB  '$$_OVL_FILENAME',0
lookup_text     DB  '$$_OVL_LOOKUP_TABLE',0
ind_text        DB  '$$_OVL_IND_CALL_TABLE',0
seg_text        DB  '$$_OVL_SEG_CALL_TABLE',0
max_text        DB  '$$_OVL_MAX_LOAD',0
mem_text        DB  '$$_OVL_MEM_ALLOC_FLAG',0
reload_text     DB  '$$_OVL_RELOAD_FLAG',0
clarion_text    DB  '$$_OVL_CLARION_FLAG',0
internal_text   DB  '$$_OVL_INTERNAL_FLAG',0
min_text        DB  '$$_OVL_MIN_ALLOC',0
pool_text       DB  '$$_OVL_POOL',0
stack_text      DB  '$$_OVL_STACK',0
prog_text       DB  '$$_MIN_PROG_SIZE',0
emspool_text    DB  '$$_OVL_EMS_POOL_FLAG',0
ox_text         DB  '$$_OVL_OX_EVAR',0
ort_text        DB  '$$_OVL_ORT',0
orp_text        DB  '$$_OVL_ORP',0
oht_text		DB	'$$_OVL_OHT',0
oht_size_text	DB	'$$_OHT_SIZE',0
oht_flag_text	DB	'$$_OHT_FLAG',0
ohp_text		DB	'$$_OVL_OHP',0
ohp_size_text	DB	'$$_OHP_SIZE',0
ohp_flag_text	DB	'$$_OHP_FLAG',0
umb_text		DB	'$$_OVL_UMB',0
filesize_text	DB	'$$_OVL_FILE_SIZE',0
ems3_text		DB	'$$_EMS3_FLAG',0
ENDIF

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   perform_pass:NEAR,find_pubdecl_entry:NEAR
EXTRN   link_error:NEAR,write_bytes:NEAR

; main driver for pass 2 of linker
; all registers used or destroyed

;*****************************
;* PASS2                     *
;*****************************

pass2       PROC
	xor	ax,ax
	cmp	is_sympac,al		; check if symbol table compression
	je	p2_dosseg			; no
	mov	al,is_summer87
	or	al,is_clipper5
	mov	compression_flag,al	; set symbol table compression flag if indicated
	xor	ax,ax
	mov	current_clipmod_ptr,ax	; init current clipper module pointer

p2_dosseg:
    cmp is_dosseg,al        ; see if dosseg switch set
    je  p2                  ; no
    call    resolve_symbols ; resolve _edata and _end symbols

p2:
    mov al,2                ; flag pass 2
    call    perform_pass    ; perform pass functions, al flags pass 2 specific routines

IFNDEF JUNIOR
    xor ax,ax
    cmp ovl_count,ax        ; see if any overlays
    je  p3                  ; no
    mov ovl_code_id,ax      ; zero overlay code so writes to exe and not ovl file
    mov ovl_data_id,ax      ; zero overlaid data flag
    call    res_ovl_symbols ; resolve symbols for overlay manager

p3:
    cmp is_comfile,0        ; see if com file
    jne p2_ret              ; yes, don't add information symbols
    call    res_info_symbols    ; resolve warplink information symbols
ENDIF

p2_ret:
    ret
pass2       ENDP

IFNDEF JUNIOR

;*****************************
;* RES_INFO_SYMBOLS          *
;*****************************

; resolve $$_EXE_IMAGE_SIZE variable if exists, fill with image_size
; destroys ax,bx,cx,dx,di,es

res_info_symbols    PROC
    mov no_match_okay,1     ; set flag that no matches in public search are okay

; write exe image size double word if predefined
    mov di,OFFSET DGROUP:image_text
    call    find_pubdecl_entry  ; find the symbol
    or  ax,ax               ; make sure nonzero
    je  ris_ret             ; not found

; get offset from start of program in cx:dx
    xor cx,cx               ; zero high word
    mov es,ax               ; es -> public declaration entry
    mov al,es:[14]          ; get definition flag
    and al,3
    cmp al,2                ; make sure publicly declared
    jne ris_ret             ; no, don't use it

    mov dx,es:[8]           ; get public offset
    mov es,es:[0]           ; es -> segment partition entry
    add dx,es:[0]           ; add in segment partition offset
    adc cx,0                ; carry to high word
    mov es,es:[4]           ; es -> master segdef entry

    add dx,es:[2]           ; add in low word
    adc cx,es:[4]           ; add in high word
    mov WORD PTR data_offset,dx ; save low word
    mov WORD PTR data_offset+2,cx   ; save high word

    mov si,OFFSET DGROUP:image_size
	push	ds
	pop	es					; es:si -> source buffer

    mov cx,4                ; write one doubleword
    call    write_bytes

ris_ret:
    mov no_match_okay,0     ; done, reset flag so remaining no matches generate internal errors
    ret
res_info_symbols    ENDP

ENDIF

;*****************************
;* RESOLVE_SYMBOLS           *
;*****************************

; resolve _edata and _end symbols
; destroys ax,bx,dx,es

resolve_symbols PROC
    mov ax,_edata_pubaddr   ; get segment of _edata public declaration entry
    mov es,ax               ; es -> pubdef declaration
    mov ax,_edata_segaddr   ; get segment of first class BSS segment segdef entry
    or  ax,ax               ; check if nonzero
    jne rs_2                ; yes
    mov BYTE PTR es:[14],1  ; make it an unresolved external

rs_2:
    call    NEAR PTR rs_3   ; sleazy way to save code, end of routine shares same code

    mov ax,_end_pubaddr     ; get segment of _edata public declaration entry
    mov es,ax               ; es -> pubdef declaration
    mov ax,_end_segaddr     ; get segment of first class BSS segment segdef entry
    or  ax,ax               ; check if nonzero
    jne rs_3                ; yes
    mov BYTE PTR es:[14],1  ; make it an unresolved external

rs_3:
    mov bx,es               ; save es -> pubdef declaration
    mov es,ax               ; es -> segdef entry
    mov ax,es:[22]          ; get first segment partition entry pointer
    mov dx,es:[16]          ; get pointer to group entry
    mov es,bx               ; es -> pubdef declaration
    mov es:[0],ax           ; update segdef partition entry pointer in public declaration entry
    mov es:[2],dx           ; update group entry pointer
    mov BYTE PTR es:[15],80h    ; show group associated with public declaration
    ret
resolve_symbols ENDP

IFNDEF JUNIOR

;*****************************
;* RES_OVL_SYMBOLS           *
;*****************************

; resolve overlay symbols
; $$_OVL_FILENAME from ovl_nopath byte string OR
; from exe_name if internal overlays flag set
; $$_OVL_LOOKUP_TABLE from lookup_segment word
; $$_OVL_IND_CALL_TABLE from ind_segment word
; $$_OVL_SEG_CALL_TABLE from segcall_segment word
; $$_OVL_MAX_LOAD from ovl_max_load word
; $$_OVL_MEM_ALLOC_FLAG from ovl_mem_alloc byte
; $$_OVL_RELOAD_FLAG from is_reload byte
; $$_OVL_CLARION_FLAG from is_clarion byte
; $$_OVL_INTERNAL_FLAG from is_internal byte
; $$_OVL_MIN_ALLOC from largest_ovl word
; $$_OVL_POOL from ovl_pool doubleword
; $$_OVL_STACK from ovl_stack word
; $$_MIN_PROG_SIZE from image_size
; $$_OVL_EMS_POOL_FLAG from is_ems_ovlpool
; destroys ax,bx,cx,dx,di,es

res_ovl_symbols PROC
    mov no_match_okay,1     ; set flag that no matches in public search are okay

; write filename bytes (no path prepended)
    mov di,OFFSET DGROUP:filename_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_nopath ; es:si -> buffer
    cmp is_internal,0       ; see if internal overlay flag set
    je  sym_loop1           ; no

; internal overlay flag set, use EXE name for OVL name
    mov si,OFFSET DGROUP:exe_name
    xor cx,cx               ; cx will hold count of chars in name, including terminator

ros_loop:
    lodsb
    inc cx                  ; bump count of chars in name
    or  al,al               ; see if zero terminator reached
    jne ros_loop            ; no

ros_loop2:
    dec si                  ; back up one char
    dec cx                  ; drop count of chars to scan back
    jcxz    sym_loop1       ; at beginning of name, use it all
    cmp BYTE PTR [si],':'   ; see if possible drive specifier
    jne ros_2               ; no
    mov ax,si
    dec ax
    cmp ax,OFFSET DGROUP:exe_name   ; drive specifier if and only if ':' second char in name
    je  ros_3               ; drive specifier

ros_2:
    cmp BYTE PTR [si],'\'   ; see if backed up to directory indicator
    jne ros_loop2           ; no

ros_3:
    inc si                  ; point just past directory indicator or drive specifier

sym_loop1:
    mov cx,1                ; write one byte
    call    write_bytes
    add WORD PTR data_offset,1
    adc WORD PTR data_offset+2,0    ; increment the data_offset variable
    cmp BYTE PTR [si-1],0   ; see if null terminator written
    jne sym_loop1           ; no

; write lookup table segment
    mov di,OFFSET DGROUP:lookup_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:lookup_segment ; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes

; write indirect call table segment
    mov di,OFFSET DGROUP:ind_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ind_segment    ; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes

; write segment call table segment
    mov di,OFFSET DGROUP:seg_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:segcall_segment    ; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes

; write maximum overlays loaded word
    mov di,OFFSET DGROUP:max_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_max_load   ; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes

; write minimum program size in paragraphs word
    mov di,OFFSET DGROUP:prog_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:min_prog_size  ; es:si -> buffer

; calculate minimum program size in paragraphs
    mov ax,WORD PTR image_size  ; get low word of program size
    mov bx,WORD PTR image_size+2    ; get high word
    add ax,15               ; round up to paragraph
    adc bx,0
    shr bx,1
    rcr ax,1                ; /2, convert to paragraphs
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1
    rcr ax,1                ; /16
    mov [si],ax             ; save to memory variable
    mov cx,2                ; write one word
    call    write_bytes

; write memory allocation flag byte
    mov di,OFFSET DGROUP:mem_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_mem_alloc  ; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write reload overlay flag byte
    mov di,OFFSET DGROUP:reload_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:is_reload  ; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write clarion flag byte
    mov di,OFFSET DGROUP:clarion_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:is_clarion ; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write internal overlays flag byte
    mov di,OFFSET DGROUP:internal_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:is_internal    ; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write EMS overlay pool flag byte
    mov di,OFFSET DGROUP:emspool_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:is_ems_ovlpool ; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write EMS 4.0 overlay stash byte
    mov di,OFFSET DGROUP:orp_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:is_xpstash ; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write XMS overlay stash byte
    mov di,OFFSET DGROUP:ort_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:is_xtstash ; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write /ohp setting byte
    mov di,OFFSET DGROUP:ohp_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:is_ohp	; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write /ohp allocation byte
    mov di,OFFSET DGROUP:ohp_flag_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_ohp_alloc	; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write /ohp size word
    mov di,OFFSET DGROUP:ohp_size_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_ohp_size	; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes

; write /oht setting byte
    mov di,OFFSET DGROUP:oht_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:is_oht	; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write /oht allocation byte
    mov di,OFFSET DGROUP:oht_flag_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_oht_alloc	; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write /oht size word
    mov di,OFFSET DGROUP:oht_size_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_oht_size	; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes

; write UMB overlay pool byte
    mov di,OFFSET DGROUP:umb_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:is_umb	; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write EMS 3.0 compatibility byte
    mov di,OFFSET DGROUP:ems3_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ems3_flag	; es:si -> buffer
    mov cx,1                ; write one byte
    call    write_bytes

; write overlay file size word
    mov di,OFFSET DGROUP:filesize_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_filesize	; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes

; write EMS /ox environment variable
    mov di,OFFSET DGROUP:ox_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_ox_evar    ; es:si -> buffer
    mov cx,32               ; write 32 bytes
    call    write_bytes

; write minimum allocation word
    mov di,OFFSET DGROUP:min_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:min_alloc_calc ; es:si -> buffer

; calculate minimum allocation in paragraphs, rounded to 1K page
    mov ax,largest_ovl      ; get size of largest overlay in bytes
    add ax,1023             ; round up to next 1K boundary
    jnc sym_2               ; no overflow
    mov ax,1000h            ; 64K segment paragraph size
    jmp SHORT sym_3         ; bypass segment paragraph size calculation

sym_2:
    and ax,0fc00h           ; put on 1K boundary
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1                ;  ax holds paragraph value of overlay, rounded to 1K boundary
    mov [si],ax             ; save to memory variable

sym_3:
    mov cx,2                ; write one word
    call    write_bytes

; write overlay pool doubleword
    mov di,OFFSET DGROUP:pool_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_pool   ; es:si -> buffer
    mov cx,4                ; write one doubleword
    call    write_bytes

; write overlay stack size word
    mov di,OFFSET DGROUP:stack_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:ovl_stack  ; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes

    mov no_match_okay,0     ; done, reset flag so remaining no matches generate internal errors
    ret

res_ovl_symbols ENDP

;*****************************
;* GET_SYMBOL_OFFSET         *
;*****************************

; get symbol's absolute offset in program
; return offset in data_offset and es -> WarpLink data segment
; ds:di -> symbol name upon entry
; destroys ax,bx,cx,dx,es

get_symbol_offset   PROC
    call    find_pubdecl_entry  ; find the symbol
    or  ax,ax               ; make sure nonzero
    je  no_ovl_mgr          ; not found, assume overlay manager wasn't linked in

; get offset from start of program in cx:dx
    xor cx,cx               ; zero high word
    mov es,ax               ; es -> public declaration entry
    mov dx,es:[8]           ; get public offset
    mov es,es:[0]           ; es -> segment partition entry
    add dx,es:[0]           ; add in segment partition offset
    adc cx,0                ; carry to high word
    mov es,es:[4]           ; es -> master segdef entry
    add dx,es:[2]           ; add in low word
    adc cx,es:[4]           ; add in high word
    mov WORD PTR data_offset,dx ; save low word
    mov WORD PTR data_offset+2,cx   ; save high word
	push	ds
	pop	es
    ret

; overlay manager not linked in, fatal error
no_ovl_mgr:
    mov ax,MISSING_OVLMGR_ERR
    jmp NEAR PTR link_error

get_symbol_offset   ENDP

ENDIF

END
