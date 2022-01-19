;*********************************************************************
;*   MLOVLFIL.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/31/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker overlay file routines                                    *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlovlfil
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
PUBLIC  setup_ovl_file,write_vectors,flush_ovl_page,ovl_file_write
PUBLIC  write_ovl_reloc,flush_reloc_array,get_symbol_segoff
PUBLIC  write_clar_data,convert_to_page
PUBLIC	safe_xms_addr

PUBLIC	read_new_ovl_page

; variables
PUBLIC  lookup_segment,ind_segment,segcall_segment
PUBLIC  large_ovl_name,is_emsxms_ovl,ems_ovl_to_file
PUBLIC  ovl_in_xms,xms_ovl_handle
PUBLIC	ovl_filesize

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   data_offset:DWORD,ovlpubblk_pos:WORD,no_match_okay:BYTE
EXTRN   data_segment:WORD,rec_offset:WORD
EXTRN   ems_first_avail:WORD,ems_trans_block:WORD
EXTRN   ems_pages_flushed:BYTE

; initialized local variables

; byte values
is_emsxms_ovl  DB  0		; nonzero if overlay file is in EMS/XMS
EVEN
ovl_in_xms  DB  0			; nonzero if overlay file is in XMS only
EVEN

; word values
ovl_idnum   DW  0			; overlay identifier number
reloc_ovl_id    DW  0		; overlay owning relocation array
reloc_ovl_pos   DW  0		; current position in relocation array
ovl_page_owner  DW  0		; id code of overlay owning overlay page
ovl_page_base   DW  0		; base of overlay page, always on 4K boundary
ovl_high_page_byte  DW  0	; highest byte written to on overlay page
lookup_tbl_pos  DW  0		; current position in lookup table array
ovl_filesize	DW	0		; size of overlay file in K

; dword values
curr_ems_pos    DD  0		; current EMS/XMS write position (analogous to file position)
ovl_file_size   DD  0		; size of overlay file

; byte strings

dirty_bit_table DB  16 DUP (0)	; indicates whether need to read overlay page from disk

near_vector_bytes   DB  2eh				; CS:
					DB  8fh,6			; POP CS:CALL_ADDRESS
near_pop            DB  0,0
					DB  2eh,0ffh,16h	; CALL CS:[CALL_ADDRESS]
near_call			DB	0,0
					DB  0eah			; JMP FAR xxxx:xxxx
ret_vector_bytes    DB  0,0,0,0
					DB  0				; 1 filler byte

shared_vector_bytes DB  53h				; PUSH BX
					DB  0bbh			; MOV BX,xxxx
source_id_word      DB  0,0
					DB  51h				; PUSH CX
					DB  8ch,0c9h		; MOV CX,CS
					DB  0eah			; JMP FAR xxxx:xxxx
to_vector_bytes     DB  0,0,0,0
					DB  0,0				; CALL_ADDRESS DW ?

public_vector_bytes DB  50h				; PUSH AX
					DB  0b8h			; MOV AX,xxxx
dest_id_word        DB  0,0
					DB  0e9h			; CALL SHARED_VECTOR_CODE
offset_to_shared    DB  0,0

; used by indirect call routine
ind_vector_bytes    DB  0eah			; JMP FAR xxxx:xxxx
to_2o_bytes         DB  0,0,0,0

; used by segment call routine
segcall_vect_bytes  DB  0eah		; JMP FAR xxxx:xxxx
to_segcall_bytes    DB  0,0,0,0

.DATA?

; uninitialized local variables

; word values
ovl_sys_info    DW  7 DUP (?)	; overlay file system info buffer
total_ref   DW  ?			; total referenced overlaid publics for segment partition in overlay public block
lookup_segment  DW  ?		; segment of overlay lookup table
ind_segment DW  ?			; segment of overlay indirect call table
segcall_segment DW  ?		; segment of segment call table
prev_vect_bytes DW  ?		; previously written vector bytes count for non-overlaid segment
nearv_offset    DW  ?		; offset to near vector call for offset
ems_first_ovl   DW  ?		; first EMS logical page used by overlay file
xms_ovl_handle  DW  ?		; handle for overlay file in XMS
ovl_sysblk_ptr  LABEL   WORD	; pointer to temporary block of system information bytes
tbuff   DW  ?				; temporary buffer
ovl_zeroblk_ptr DW  ?		; pointer to zero'd block of bytes
ovl_zeroblk_size	DW	?	; size of zero'd block of bytes

; doubleword values
ovl_filepos     DD  ?		; current overlay file pointer position
large_ovl_name  DD  ?		; seg:offset of largest overlay segment name

; structures
EMPB_STRUC  STRUC
	es_len  DD  ?			; length of block in bytes
	es_src_handle   DW  ?	; source EMB handle
	es_src_offset   DD  ?	; source offset
	es_dest_handle  DW  ?	; destination EMB handle
	es_dest_offset  DD  ?	; destination offset
EMPB_STRUC  ENDS

ovl_empb    EMPB_STRUC  <>

;*****************************
;* Constant data             *
;*****************************

.CONST

EVEN
vector_text     DB  '$$_OVL_VECTOR',0
ret_text        DB  '$$_OVL_RET',0
ovl_2o_text     DB  '$$_OVL_IND_2OVL_ENT',0
ovl_segcall_text    DB  '$$_OVL_SEG_CALL_ENTRY',0

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,alloc_memory:NEAR
EXTRN   zero_mem_image:NEAR,write_bytes:NEAR,find_pubdecl_entry:NEAR
EXTRN   make_reloc_entry:NEAR,map_ems_page:NEAR,restore_ems_map:NEAR
EXTRN   read_to_ems:NEAR,check_xms_error:NEAR

;*****************************
;* SETUP_OVL_FILE            *
;*****************************

; create overlay file, allocate 4K memory block for writing to it
; allocate block to hold overlay file positions
; pad to proper length with zero bytes
; destroys ax,bx,cx,dx,di,si,es

setup_ovl_file  PROC
	mov bx,100h				; get 4K block for use in writing to overlay file
	call	alloc_memory
	mov ovl_ioblk,ax		; save pointer to block

; zero the 4K block
	mov es,ax
	xor di,di				; es:di -> 4K block
	mov ax,di
	mov cx,2048				; 2K words
	rep stosw

	mov ax,ovl_count
	mov dx,ax
	add ax,3				; round up for dword to paragraph conversion
	shr ax,1				; /2
	shr ax,1				; /4, 4 dword entries in each paragraph
	mov bx,ax
	call	alloc_memory
	mov ovl_filepos_blk,ax	; save file position block segment

	mov ax,10				; assume non-Clarion (10 bytes/system info)
	cmp is_clarion,0		; see if clarion
	je  sof_alloc			; no
	mov ax,14				; 14 bytes/system info

; dx holds overlay count, ax holds bytes/system info
sof_alloc:
	mul dx					; get size of system info block to allocate in bytes
	add ax,15
	and ax,0fff0h			; round up to paragraph boundary

; use memory between allocation_base and allocation_top as temporary storage
; for system information, if not enough room then fail with out of memory
	mov bx,allocation_top
	sub bx,allocation_base	; free memory in paragraphs
	cmp bx,0fffh			; if at least 64K-16 then enough room
	jae sof_memok
	shl bx,1				; x2
	shl bx,1				; x4
	shl bx,1				; x8
	shl bx,1				; x16, bx holds byte count of free memory
	cmp bx,ax				; see if enough free memory
	jae sof_memok			; yes

; force out of memory error
sof_oomerr:
	mov ax,8				; force DOS out of memory error
	jmp NEAR PTR dos_error

sof_memok:
	mov dx,ax				; save number of bytes used in free memory
	mov ax,allocation_base
	mov ovl_sysblk_ptr,ax	; save -> temporary system information storage

	shr dx,1				; /2, convert number of bytes used to paragraphs
	shr dx,1				; /4
	shr dx,1				; /8
	shr dx,1				; /16
	mov ax,allocation_top
	sub ax,allocation_base	; compute free memory in paragraphs
	sub ax,dx				; compute free memory not used for temporary system information
	cmp ax,100h				; see if >4K (100h paragraphs)
	ja  sof_newzero			; yes

; <4K free memory unused for system info, use 4K block allocated for overlay file i/o
	mov ax,ovl_ioblk
	mov ovl_zeroblk_ptr,ax
	mov es,ax
	mov cx,4096
	jmp SHORT sof_zeroblk

sof_newzero:
	mov cx,0fff0h			; preset zero block size to highest amount
	cmp ax,0fffh			; don't use more than 64K-16 bytes for zero block
	jae sof_newz2
	mov cx,ax
	shl cx,1				; x2, convert to bytes
	shl cx,1				; x4
	shl cx,1				; x8
	shl cx,1				; x16

sof_newz2:
	mov ax,allocation_base
	add ax,dx				; compute base of zero block
	mov ovl_zeroblk_ptr,ax	; save it
	mov es,ax

sof_zeroblk:
	xor di,di				; es:di -> memory to be zero'ed
	mov ax,di				; store a zero value
	mov ovl_zeroblk_size,cx	; save zero block size
	shr cx,1				; convert byte count to write to words
	rep stosw				; zero the string
	rcl cx,1				; pick up carry
	rep stosb				; zero leftover byte, if any

	mov cx,ovl_count
	shl cx,1
	shl cx,1				; 4 bytes/overlay
	add cx,2				; adjust for overlay count word
	mov WORD PTR ovl_file_size,cx	; update overlay file size

; step through each overlaid segment partition entry
; update the overlay identifier number
; write zero word for each of relocation items and zero bytes for total length
	mov ax,first_segdefblk_ptr	; get first segdef block pointer

segdef_blkloop:
	or  ax,ax				; see if another block
	jne sof_segblk			; yes
	jmp NEAR PTR sof_open	; no, open file and write file positions

sof_segblk:
	mov es,ax				; es -> segdef block
	push	es				; keep -> segdef block

	mov cx,es:[0]			; get count of entries in block
	inc ax					; bump to first segdef entry in block

segdef_entloop:
	push	cx				; save entry count on stack
	mov es,ax				; es -> segdef entry
	mov si,ax				; keep -> segdef entry
	test	BYTE PTR es:[28],1	; see if overlay class
	jne first_segpart		; yes, get first segment partition entry
	jmp NEAR PTR next_segdef	; no

; this segdef is overlay class, check segment partitions owned by it
first_segpart:
	mov ax,es:[22]			; get pointer to first segment partition entry

segpart_loop:
	or  ax,ax				; see if valid segment partition entry
	jne good_segpart		; yes, non-null
	jmp NEAR PTR next_segdef	; no

good_segpart:
	mov es,ax				; es -> segment partition entry
	mov al,es:[15]
	and al,80h				; get overlaid flag bit
	jne overlaid_segpart	; overlaid
	jmp NEAR PTR next_segpart	; not overlaid

; overlay segment too long
ovllen_err:
	mov ax,OVLSEG_LEN_ERR	; overlay segment too long
	jmp NEAR PTR link_error	; transfer control to error handler

; overlaid segment, assign overlay identifier number
overlaid_segpart:
	inc ovl_idnum			; bump identifier number
	mov ax,ovl_idnum
	mov bx,es:[4]			; save -> segdef entry in bx
	mov es:[4],ax			; overwrite back pointer to segdef entry with overlay id

	mov di,ax				; di holds overlay id
	dec di					; make relative zero
	shl di,1				; convert to word offset
	mov ax,ds				; save -> warplink data
	mov ds,master_segblk	; ds -> master segdef entries block
	mov [di],bx				; save -> master segdef entry in appropriate element
	mov ds,ax				; restore ds -> warplink data

; see if near/shared vector space was allocated for this segment
; allocate if not
	test	BYTE PTR es:[15],10h	; check vector space allocated flag
	jne chk_largest			; set
	test	BYTE PTR es:[11],2		; see if Big bit set
	jne ovllen_err			; yes, fatal error, segment too large
	add WORD PTR es:[12],20h	; add space for vectors
	jnc chk_largest			; no overflow
	mov ax,es:[12]
	or  ax,ax				; must be zero for 64K segment
	jne ovllen_err			; segment too long
	or  BYTE PTR es:[11],2	; set Big bit

chk_largest:
	mov ax,es:[12]			; get new overlay size
	add ax,15
	and ax,0fff0h			; round up to paragraph boundary

	cmp is_clarion,0		; see if clarion switch set
	je  sof_3a				; no

	mov di,ovl_idnum
	dec di					; make relative zero
	shl di,1
	shl di,1				; di holds doubleword entry offset for overlay
	push	ds				; save critical register
	mov ds,data_ovlblk_ptr	; ds -> get first data overlay block

clar_loop:
	cmp di,252				; see if entry in current data overlay block (126 word entries)
	jb  clar_curblk			; yes
	sub di,252				; no, get next block, reduce entry count by block amount
	mov cx,ds:[0]			; get pointer to next block
	mov ds,cx				; ds -> next block
	jmp SHORT clar_loop		; loop until at proper block

clar_curblk:
	mov cx,[di+2]			; get _DT segment size
	mov di,[di+4]			; get _DAT segment size
	pop ds					; restore critical register
	add cx,15
	and cx,0fff0h			; round up to paragraph boundary
	add di,15
	and di,0fff0h			; round up to paragraph boundary
	mov _dt_seg_size,cx		; save segment sizes
	mov _dat_seg_size,di
	add ax,cx				; add them to largest overlay figure
	add ax,di

sof_3a:
	cmp ax,largest_ovl		; check if should update largest overlay variable
	jb  chk_2nd				; no
	mov cx,largest_ovl		; keep old largest value
	mov largest_ovl,ax		; yes, do it
	mov second_ovl,cx		; old largest to second largest
	mov cx,es				; save -> segment partition entry
	mov es,bx				; es -> master segdef entry
	les ax,es:[8]			; es:ax -> lnames entry
	add ax,8				; bump ax past doubleword pointers
	mov WORD PTR large_ovl_name,ax	; save offset of name
	mov ax,es
	mov WORD PTR large_ovl_name+2,ax	; save segment of name
	mov es,cx				; restore es -> segment partition entry
	jmp SHORT get_ovl_offset	; bypass second largest check

chk_2nd:
	cmp ax,second_ovl		; check if should update second largest overlay
	jb  get_ovl_offset		; no
	mov second_ovl,ax		; yes, do it

; save file position
get_ovl_offset:
	mov ax,WORD PTR ovl_file_size
	mov WORD PTR ovl_filepos,ax	; save LSW of file position
	mov ax,WORD PTR ovl_file_size+2
	mov WORD PTR ovl_filepos+2,ax	; save MSW of file position

; save overlay file system information, structure as follows:
; overlay number DW ?
; offset to overlay code DW ?
; overlay code size in paragraphs DW ?
; IF Clarion, overlay _DT segment size in paragraphs
; IF Clarion, overlay _DAT segment size in paragraphs
; near vector offset DW ?
; relocation entry count DW ?
; relocation entries DW ? DUP (?)
; code DB ? DUP (?)

	mov ax,ovl_idnum
	mov WORD PTR ovl_sys_info,ax	; save overlay number
	mov ax,es:[12]			; get size of overlay code
	add ax,15				; round up
	jnc no_overflow
	jmp NEAR PTR ovllen_err	; overflow occurred

no_overflow:
	shr ax,1				; convert to paragraphs, /2
	shr ax,1				; /4
	shr ax,1				; /8
	shr ax,1				; /16
	mov WORD PTR ovl_sys_info+4,ax	; size of overlay code in paragraphs

	mov cx,es:[12]			; get segment length
	mov ax,es:[14]			; get vector count
	and ah,7				; mask off flag bits (only bit 0-2 used)
	mov dx,7				; 7 bytes per vector
	mul dx					; dx:ax holds vector bytes, dx always zero
	add ax,20h				; adjust for near/shared vector paragraphs
	sub cx,ax				; cx holds near vector offset value
	mov ax,es:[0]			; get count of relocation words in ax

	cmp is_clarion,0		; see if Clarion switch set
	je  sof_notclar			; no
	mov WORD PTR ovl_sys_info+10,cx	; save near vector offset
	mov WORD PTR ovl_sys_info+12,ax	; save relocation entries count
	mov cx,_dt_seg_size		; get _DT segment size of this overlay
	add WORD PTR ovl_file_size,cx	; update overlay file size
	adc WORD PTR ovl_file_size+2,0	; carry to high word
	shr cx,1				; convert to paragraphs, /2
	shr cx,1				; /4
	shr cx,1				; /8
	shr cx,1				; /16
	mov WORD PTR ovl_sys_info+6,cx	; save _DT segment size in paragraphs

	mov cx,_dat_seg_size
	add WORD PTR ovl_file_size,cx	; update overlay file size
	adc WORD PTR ovl_file_size+2,0	; carry to high word
	shr cx,1				; convert to paragraphs, /2
	shr cx,1				; /4
	shr cx,1				; /8
	shr cx,1				; /16
	mov WORD PTR ovl_sys_info+8,cx	; save _DAT segment size in paragraphs

	mov cx,14				; 7 system words to write
	jmp SHORT sof_sysadj

sof_notclar:
	mov WORD PTR ovl_sys_info+6,cx	; save near vector offset
	mov WORD PTR ovl_sys_info+8,ax	; save relocation entries count
	mov cx,10				; 5 system words to  write

sof_sysadj:
	shl ax,1				; convert relocation words to bytes
	add ax,cx				; adjust for system info bytes
	mov WORD PTR ovl_sys_info+2,ax	; offset to overlay code

; save system information to temporary storage, update overlay file size
	push	es				; save critical register
	mov es,ovl_sysblk_ptr	; es -> temporary storage
	mov ax,ovl_idnum
	dec ax					; make relative zero
	mul cx					; ax holds offset into system information
	mov di,ax				; es:di -> proper system information entry

	mov ax,WORD PTR ovl_sys_info	; get overlay number
	stosw
	mov ax,WORD PTR ovl_sys_info+2	; offset to overlay code
	stosw
	mov ax,WORD PTR ovl_sys_info+4	; size of overlay code in paragraphs
	stosw
	mov ax,WORD PTR ovl_sys_info+6	; get near vector offset/_DT segment size in paragraphs
	stosw
	mov ax,WORD PTR ovl_sys_info+8	; get relocation entries/_DAT segment size in paragraphs
	stosw

	cmp is_clarion,0		; see if clarion switch set
	je  sof_updsize			; no

; clarion system information
	mov ax,WORD PTR ovl_sys_info+10	; get near vector offset
	stosw
	mov ax,WORD PTR ovl_sys_info+12	; get relocation entries count
	stosw

sof_updsize:
	pop es					; restore critical register
	add WORD PTR ovl_file_size,cx	; update overlay file size
	adc WORD PTR ovl_file_size+2,0	; carry to high word

	mov ax,es:[0]			; get relocation word count
	shl ax,1				; convert to byte count
	add WORD PTR ovl_file_size,ax	; update overlay file size
	adc WORD PTR ovl_file_size+2,0	; carry to high word

	mov ax,es:[12]			; get code size
	add ax,15
	and ax,0fff0h			; round up to paragraph boundary
	add WORD PTR ovl_file_size,ax	; update overlay file size
	adc WORD PTR ovl_file_size+2,0	; carry to high word

	mov di,ovl_idnum		; get overlay identification number
	dec di					; make relative zero
	shl di,1
	shl di,1				; convert to doubleword offset
	mov ax,WORD PTR ovl_filepos	; get file offset low word
	mov dx,WORD PTR ovl_filepos+2	; get file offset high word
	mov cx,ds				; save -> warplink data
	mov ds,ovl_filepos_blk	; ds -> overlay file positions
	mov [di],ax				; save offset low word
	mov [di+2],dx			; save offset high word
	mov ds,cx				; restore ds -> warplink data

next_segpart:
	mov ax,es:[2]			; get pointer to next segment partition entry
	jmp NEAR PTR segpart_loop	; loop back and check it

next_segdef:
	pop cx					; restore entry count from stack
	mov ax,si				; get old segdef entry
	add ax,2				; move to next segdef entry in block
	dec cx
	je  next_segblk			; all entries in block done
	jmp NEAR PTR segdef_entloop	; get next entry in block

next_segblk:
	pop es					; restore es -> segdef block
	mov ax,es:[2]			; get pointer to next block, if any
	jmp NEAR PTR segdef_blkloop	; loop for next block

; ovl_file_size holds overlay file size in bytes
sof_open:
	call	ems_ovl_setup	; setup ems for overlay file, if possible/available

	mov dx,OFFSET DGROUP:ovl_filename	; DS:DX -> ASCIIZ file specification
	mov ah,3ch				; create/truncate file
	xor cx,cx				; normal file attribute
	int 21h
	call	restore_ems_map
	jnc sof_2				; no errors

sof_to_doserr:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

sof_2:
	mov ovl_handle,ax		; save handle of overlay file
	mov bx,ax				; handle in bx for dos calls
	mov dx,OFFSET DGROUP:ovl_count	; write count of overlays to .OVL file as first word
	mov cx,2				; write two bytes
	cmp is_emsxms_ovl,0		; see if overlay stashed in EMS/XMS
	je  sof_noems1			; overlay not stashed in EMS/XMS
	call	ems_ovl_write	; write to overlay in EMS
	jmp	SHORT sof_fpos

sof_noems1:
	mov ah,40h				; write to file
	int 21h
	call	restore_ems_map
	jc  sof_to_doserr		; error occurred

; write overlay file positions saved in ovl_filepos_blk at start of overlay file
sof_fpos:
	mov cx,ovl_count
	shl cx,1
	shl cx,1				; 4 bytes/overlay
	push	ds				; save critical register
	xor dx,dx				; ds:dx -> block to write
	cmp is_emsxms_ovl,0		; see if overlay stashed in EMS/XMS
	mov ds,ovl_filepos_blk	; KEEP FLAGS
	je  sof_noems2			; overlay not stashed in EMS/XMS
	call	ems_ovl_write	; write to overlay in EMS
	jmp	SHORT sof_syst

sof_noems2:
	mov ah,40h				; write to file
	int 21h
	call	restore_ems_map
	jc  sof_to_doserr		; error occurred

sof_syst:
	pop ds					; restore ds -> warplink data

; read through each file position entry, write system info and zero bytes following
	mov es,ovl_sysblk_ptr
	xor si,si				; es:si -> start of temporary storage
	mov cx,ovl_count

sof_writeloop:
	push	cx				; save count of overlays to write

; get system identification bytes
	lods    WORD PTR es:0	; overlay id number
	mov WORD PTR ovl_sys_info,ax
	lods    WORD PTR es:0	; offset to overlay code
	mov WORD PTR ovl_sys_info+2,ax
	lods    WORD PTR es:0	; size of overlay code in paragraphs
	mov WORD PTR ovl_sys_info+4,ax

	lods    WORD PTR es:0	; get near vector offset/_DT segment size
	mov WORD PTR ovl_sys_info+6,ax
	lods    WORD PTR es:0	; get relocation entries/_DAT segment size
	mov WORD PTR ovl_sys_info+8,ax

	cmp is_clarion,0		; see if clarion switch set
	jne sof_wrcl			; yes

	mov cx,10				; system bytes to write
	jmp SHORT sof_write

; clarion system information
sof_wrcl:
	lods    WORD PTR es:0	; get near vector offset
	mov WORD PTR ovl_sys_info+10,ax
	lods    WORD PTR es:0	; get relocation entries count
	mov WORD PTR ovl_sys_info+12,ax

	mov cx,14				; system bytes to write

sof_write:
	mov dx,OFFSET DGROUP:ovl_sys_info	; ds:dx -> block to write
	cmp is_emsxms_ovl,0		; see if overlay stashed in EMS/XMS
	je  sof_noems3			; overlay not stashed in EMS/XMS
	call	ems_ovl_write	; write to overlay in EMS
	jmp	SHORT sof_code

sof_noems3:
	mov ah,40h				; write to file
	int 21h
	call	restore_ems_map
	jc  sof_doserr2			; error occurred

sof_code:
	mov di,WORD PTR ovl_sys_info+4
	shl di,1
	shl di,1
	shl di,1
	shl di,1				; convert paragraphs to bytes
	call	write_zero_to_ovl	; write zeroed bytes for overlay code

	cmp is_clarion,0		; see if clarion switch set
	jne sof_wrcl2			; yes

	mov di,WORD PTR ovl_sys_info+8
	jmp	SHORT sof_reloc		; write zeroed bytes for relocation entries

; clarion specific code
sof_wrcl2:
	mov di,WORD PTR ovl_sys_info+6
	shl di,1
	shl di,1
	shl di,1
	shl di,1				; convert paragraphs to bytes
	call	write_zero_to_ovl	; write zeroed bytes for _DT segment

	mov di,WORD PTR ovl_sys_info+8
	shl di,1
	shl di,1
	shl di,1
	shl di,1				; convert paragraphs to bytes
	call	write_zero_to_ovl	; write zeroed bytes for _DAT segment

	mov di,ovl_sys_info+12

sof_reloc:
	shl di,1				; convert word count to bytes
	call	write_zero_to_ovl	; write zeroed bytes for relocation entries

sof_nextovl:
	pop cx					; restore count of overlays to write
	dec cx
	je  sof_wrdone			; all done writing overlays
	jmp NEAR PTR sof_writeloop	; loop until complete

sof_doserr2:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

; get overlay file size (+11 bytes/overlay) in ovl_filesize variable
; bx still == overlay file handle
sof_wrdone:
COMMENT #
	xor cx,cx				; move file pointer to start of file
	mov dx,cx
	mov ax,4202h			; move file pointer, offset from file end
	int 21h
	call	restore_ems_map
	mov	cx,ax				; save file size in di:cx
	mov	di,dx
END COMMENT #
; 12/30/92
; get overlay file size in di:cx
	mov	cx,WORD PTR ovl_file_size
	mov	di,WORD PTR ovl_file_size+2

	mov	dx,11
	mov	ax,ovl_count
	mul	dx					; get overlay count*11 in dx:ax
	add	ax,cx
	adc	dx,di
	add	ax,1023
	adc	dx,0				; round up file size in dx:ax to next 1K boundary
	mov	al,ah
	mov	ah,dl				; register moves effective divide by 256 (assume file size <16M)
	shr	ax,1				; /512
	shr	ax,1				; /1024, ax holds file size in K
	mov	ovl_filesize,ax		; save overlay file size in K

;*** feedback required stash amount if EMS or XMS stashing?
;*** 'X EMS pages (XXK) required to completely stash overlay file in EMS.'
;*** 'XK of XMS required to completely stash overlay file in XMS.'

	ret
setup_ovl_file  ENDP

COMMENT #

;*****************************
;* WRITE_ZERO_TO_OVL         *
;*****************************

; write di zero bytes to overlay file
; upon entry bx holds file handle
; destroys ax,cx,di

write_zero_to_ovl   PROC

zero_loop:
	mov cx,di
	cmp cx,ovl_zeroblk_size	; only write up to ovl_zeroblk_size bytes each pass thru loop
	jbe wz_2				; ovl_zeroblk_size bytes or less to write
	mov cx,ovl_zeroblk_size

wz_2:
	push	ds
	xor dx,dx
	mov ds,ovl_zeroblk_ptr	; ds:dx -> zero'd block
	mov ah,40h				; write to file
	int 21h
	pop ds
	call	restore_ems_map
	jnc wz_3				; no errors

; error occurred writing file
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

wz_3:
	sub di,cx				; subtract number of bytes written this loop pass
	jne zero_loop			; more bytes to write, loop back
	ret
write_zero_to_ovl   ENDP

;*****************************
;* EMS_OVL_SETUP             *
;*****************************

; setup EMS or XMS to hold overlay file if it fits, read it into EMS/XMS
; destroys ax,bx,cx,dx,si,di

ems_ovl_setup   PROC
	cmp is_xms,0			; see if XMS used
	jne eos_chksize			; yes
	cmp is_no_ems,0			; see if EMS used
	je  eos_chksize			; yes
	ret

eos_chksize:
	mov ax,WORD PTR ovl_file_size	; get overlay file size in dx:ax
	mov dx,WORD PTR ovl_file_size+2
	add ax,16383			; round up to nearest 16K boundary
	adc dx,0				; carry to high word
	call	convert_to_page	; convert bytes in dx:ax to 16K blocks in ax

; check if enough XMS to save overlay file, priority over EMS
	cmp is_xms,0			; see if XMS used
	je  eos_chkems			; no
	mov di,ax				; save 16K block count in di
	shl ax,1				; *2
	shl ax,1				; *4
	shl ax,1				; *8
	shl ax,1				; *16, ax holds 1K block count
	mov cx,ax				; save 1K block count
	mov ah,8				; query free extended memory
	call	DWORD PTR xms_addr
	call	check_xms_error	; see if error occurred
	cmp ax,cx				; see if largest free block meets 1K block count
	jb  eos_noxms			; no
	jmp NEAR PTR eos_usexms	; yes, use XMS

eos_ret:
	ret

; not enough XMS to stuff overlay file in it, try EMS
eos_noxms:
	cmp is_no_ems,0			; see if EMS used
	jne eos_ret				; no
	mov ax,di				; get 16K block count back in ax

eos_chkems:
	cmp ax,ems_page_avail	; see if enough pages are available to hold overlay file
	ja	eos_chkflush		; no, see if flushing will work

; enough pages to hold overlay, now see if enough pages for maximum temp file
; (640K) or 40 pages, otherwise flush
	mov	bx,ems_page_avail
	sub	bx,ax
	cmp	bx,40				; see if 40 pages left over
	jae	eos_emsok			; yes

; not enough ems to hold overlay file+40 pages for temp file if needed,
; check if total ems pages allocated-4
; is enough (if you flush the modules stashed in EMS)
eos_chkflush:
	mov di,ems_pagecount	; get total ems pages
	sub di,4				; subtract amount used for file i/o buffer
	cmp ax,di				; see if enough total ems pages allocated to hold overlay file
	ja  eos_ret				; no
	mov ems_pages_flushed,1	; set pages flushed flag
	mov ems_first_avail,4	; reset first available page flag
	mov ems_page_avail,di	; adjust pages available back to pre-module stashed status
	mov di,ax				; save overlay file pages
	xor ax,ax				; al holds physical page
	mov bx,ax				; bx holds logical page
	mov cx,4				; update all 4 pages in ems page frame

eos_mapin:
	call	map_ems_page	; map 0->0, 1->1, 2->2, 3->3
	inc bx					; bump logical page
	mov ax,bx				; bump physical page
	loop    eos_mapin		; loop until map in complete
	mov ax,di				; ax holds overlay file pages

eos_emsok:
	sub ems_page_avail,ax	; subtract off pages used by overlay file
	mov di,ax				; save overlay page count
	mov ax,ems_first_avail	; get first available page
	mov ems_first_ovl,ax	; save as first used overlay page
	add ems_first_avail,di	; bump up first available page past those used by overlay file
	mov is_emsxms_ovl,1		; set overlay file in EMS/XMS flag

; rewind to start of file
	mov bx,ovl_handle
	xor cx,cx
	mov dx,cx				; zero file offset
	mov ax,4200h			; move file pointer, absolute offset
	int 21h
	call	restore_ems_map

; read overlay file into EMS
	push	WORD PTR ems_currmap	; save current logical state of physical page 0
	mov si,ems_first_ovl	; si holds first overlay page

eos_readloop:
	xor al,al
	mov bx,si
	call	map_ems_page
	mov bx,ovl_handle
	mov cx,16384			; read 16K block
	xor dx,dx
	push	ds				; save critical register
	mov ds,ems_base			; ds:dx -> read buffer, EMS physical page 0

	call	read_to_ems

	pop ds					; restore ds -> warplink data

	inc si					; bump to next overlay page
	dec di					; drop count of pages to read
	jne eos_readloop		; not done yet

; restore previous logical state of physical page 0
	xor al,al
	pop bx
	call	map_ems_page
	ret

eos_doserr:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

eos_usexms:
	mov dx,cx				; get blocks to allocate in K
	mov ah,9				; allocate extended memory block
	call	DWORD PTR xms_addr
	call	check_xms_error	; see if error occurred
	mov xms_ovl_handle,dx	; save xms handle
	mov ovl_empb.es_dest_handle,dx	; save in parameter block

	mov bx,ovl_handle
	xor cx,cx
	mov dx,cx				; zero file offset
	mov ax,4200h			; move file pointer, absolute offset
	int 21h
	call	restore_ems_map

	xor ax,ax
	mov WORD PTR ovl_empb.es_dest_offset,ax	; zero init the XMS offset in parameter block
	mov WORD PTR ovl_empb.es_dest_offset+2,ax
	mov WORD PTR ovl_empb.es_len+2,ax	; zero high word of block length
	mov ovl_empb.es_src_handle,ax	; zero handle (conventional memory source)
	mov WORD PTR ovl_empb.es_src_offset,ax	; zero offset

	inc al
	mov is_emsxms_ovl,al	; set overlay file in EMS/XMS flag
	mov ovl_in_xms,al		; set overlay file in XMS only flag

	mov ax,16384
	mov WORD PTR ovl_empb.es_len,ax	; length of block to transfer low word
	mov ax,ems_trans_block
	mov WORD PTR ovl_empb.es_src_offset+2,ax	; source segment

eos_xmsread:
	mov bx,ovl_handle
	mov cx,16384			; read 16K block
	xor dx,dx
	push	ds				; save critical register
	mov ds,ems_trans_block	; ds:dx -> read buffer
	mov ah,3fh				; read file
	int 21h
	pop ds					; restore ds -> warplink data
	call	restore_ems_map
	jc  eos_doserr			; error reading file

; read from file, transfer to XMS
	mov ah,0bh				; move extend memory block
	mov si,OFFSET DGROUP:ovl_empb	; ds:si -> parameter block
	call	DWORD PTR xms_addr
	call	check_xms_error	; see if error occurred

	add WORD PTR ovl_empb.es_dest_offset,16384	; bump dword offset in XMS
	adc WORD PTR ovl_empb.es_dest_offset+2,0	; carry to high word

	dec di					; drop count of pages to read
	jne eos_xmsread			; not done yet

	ret

ems_ovl_setup   ENDP

END COMMENT #

;*****************************
;* WRITE_ZERO_TO_OVL         *
;*****************************

; write di zero bytes to overlay file
; upon entry bx holds file handle
; destroys ax,cx,di

write_zero_to_ovl   PROC

zero_loop:
	mov cx,di
	cmp cx,ovl_zeroblk_size	; only write up to ovl_zeroblk_size bytes each pass thru loop
	jbe wz_2				; ovl_zeroblk_size bytes or less to write
	mov cx,ovl_zeroblk_size

wz_2:
	push	ds
	xor dx,dx
	cmp is_emsxms_ovl,0		; see if overlay stashed in EMS/XMS
	mov ds,ovl_zeroblk_ptr	; ds:dx -> zero'd block, KEEP FLAGS
	je  wz_noems			; overlay not stashed in EMS/XMS
	call	ems_ovl_write	; write to overlay in EMS
	jmp	SHORT wz_3

wz_noems:
	mov ah,40h				; write to file
	int 21h
	call	restore_ems_map
	jc	wz_error			; error occurred

wz_3:
	pop ds

	sub di,cx				; subtract number of bytes written this loop pass
	jne zero_loop			; more bytes to write, loop back
	ret

; error occurred writing file
wz_error:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

write_zero_to_ovl   ENDP

;*****************************
;* EMS_OVL_SETUP             *
;*****************************

; setup EMS or XMS to hold overlay file if it fits
; destroys ax,bx,cx,dx,si,di

ems_ovl_setup   PROC
	cmp is_xms,0			; see if XMS used
	jne eos_chksize			; yes
	cmp is_no_ems,0			; see if EMS used
	je  eos_chksize			; yes
	ret

eos_chksize:
	mov ax,WORD PTR ovl_file_size	; get overlay file size in dx:ax
	mov dx,WORD PTR ovl_file_size+2
	add ax,16383			; round up to nearest 16K boundary
	adc dx,0				; carry to high word
	call	convert_to_page	; convert bytes in dx:ax to 16K blocks in ax

; check if enough XMS to save overlay file, priority over EMS
	cmp is_xms,0			; see if XMS used
	je  eos_chkems			; no
	mov di,ax				; save 16K block count in di
	shl ax,1				; *2
	shl ax,1				; *4
	shl ax,1				; *8
	shl ax,1				; *16, ax holds 1K block count
	mov cx,ax				; save 1K block count
	mov ah,8				; query free extended memory
	call	DWORD PTR xms_addr
	call	check_xms_error	; see if error occurred
	cmp ax,cx				; see if largest free block meets 1K block count
	jb  eos_noxms			; no

	mov dx,cx				; get blocks to allocate in K
	mov ah,9				; allocate extended memory block
	call	DWORD PTR xms_addr
	call	check_xms_error	; see if error occurred
	mov xms_ovl_handle,dx	; save xms handle

; successful allocation for overlay file stashing
	mov	al,1
	mov is_emsxms_ovl,al	; set overlay file in EMS/XMS flag
	mov ovl_in_xms,al		; set overlay file in XMS only flag

eos_ret:
	ret

; not enough XMS to stuff overlay file in it, try EMS
eos_noxms:
	cmp is_no_ems,0			; see if EMS used
	jne eos_ret				; no
	mov ax,di				; get 16K block count back in ax

eos_chkems:
	cmp ax,ems_page_avail	; see if enough pages are available to hold overlay file
	ja	eos_chkflush		; no, see if flushing will work

; enough pages to hold overlay, now see if enough pages for maximum temp file
; (640K) or 40 pages, otherwise flush
	mov	bx,ems_page_avail
	sub	bx,ax
	cmp	bx,40				; see if 40 pages left over
	jae	eos_emsok			; yes

; not enough ems to hold overlay file+40 pages for temp file if needed,
; check if total ems pages allocated-4
; is enough (if you flush the modules stashed in EMS)
eos_chkflush:
	mov di,ems_pagecount	; get total ems pages
	sub di,4				; subtract amount used for file i/o buffer
	cmp ax,di				; see if enough total ems pages allocated to hold overlay file
	ja  eos_ret				; no
	mov ems_pages_flushed,1	; set pages flushed flag
	mov ems_first_avail,4	; reset first available page flag
	mov ems_page_avail,di	; adjust pages available back to pre-module stashed status
	mov di,ax				; save overlay file pages
	xor ax,ax				; al holds physical page
	mov bx,ax				; bx holds logical page
	mov cx,4				; update all 4 pages in ems page frame

eos_mapin:
	call	map_ems_page	; map 0->0, 1->1, 2->2, 3->3
	inc bx					; bump logical page
	mov ax,bx				; bump physical page
	loop    eos_mapin		; loop until map in complete
	mov ax,di				; ax holds overlay file pages

eos_emsok:
	sub ems_page_avail,ax	; subtract off pages used by overlay file
	mov di,ax				; save overlay page count
	mov ax,ems_first_avail	; get first available page
	mov ems_first_ovl,ax	; save as first used overlay page
	add ems_first_avail,di	; bump up first available page past those used by overlay file
	mov is_emsxms_ovl,1		; set overlay file in EMS/XMS flag
	ret
ems_ovl_setup   ENDP

;*****************************
;* CONVERT_TO_PAGE           *
;*****************************

; convert bytes in dx:ax to 16K blocks in ax
; destroys ax,dx

convert_to_page PROC
	mov al,ah
	mov ah,dl
	mov dl,dh
	xor dh,dh				; register moves perform /256
	shr dx,1
	rcr ax,1				; /512
	shr dx,1
	rcr ax,1				; /1024
	shr dx,1
	rcr ax,1				; /2048
	shr dx,1
	rcr ax,1				; /4096
	shr dx,1
	rcr ax,1				; /8192
	shr dx,1
	rcr ax,1				; /16384, assume dx==0 (no 1G overlay files, pretty safe)
	ret
convert_to_page ENDP

;*****************************
;* CALC_SEG_VALUE            *
;*****************************

; calculates segment value from segdef segment passed in ax
; returns value in ax
; destroys ax,cx,es

calc_seg_value  PROC
	mov es,ax				; es -> segdef entry
	mov ax,es:[2]			; get low word of offset
	mov cx,es:[4]			; get high word of offset
	shr cx,1				; convert to paragraphs
	rcr ax,1
	shr cx,1
	rcr ax,1
	shr cx,1
	rcr ax,1
	shr cx,1
	rcr ax,1				; ax holds segment value
	ret
calc_seg_value  ENDP

;*****************************
;* WRITE_VECTORS             *
;*****************************

; write overlay vectors in appropriate segments and lookup table vector values
; destroys ax,bx,cx,dx,di,si,es

write_vectors   PROC
	mov ax,0ffffh
	mov buffer_end,ax		; set buffer end so that writes from warplink data will never trip buffer wrap code

; calculate overlaid public lookup table segment for later use
	mov ax,lookup_tbl_segdef
	call	calc_seg_value
	mov lookup_segment,ax

; calculate overlay public indirect call table segment
	mov ax,ind_tbl_segdef
	call	calc_seg_value
	mov ind_segment,ax

; calculate segment call table segment
	mov ax,segcall_tbl_segdef
	call	calc_seg_value
	mov segcall_segment,ax

; setup the constant vectors to and from overlay manager in vector byte tables
	call	set_vector_bytes

; Step through all segment entries in overlaid public table.
; If overlay class, write near and shared vector values at start of segment,
; public vector values to other segments at end of segment.
; Update vector written bit for the segment.

	mov ax,first_ovlpubblk_ptr	; get first allocate overlaid public block
	mov bx,2				; init block position
	mov es,ax				; es -> public block

; no more overlaid publics for this segment, move to next, if any
wv_next_seg:
	cmp bx,OVLPUB_BLK_BYSIZE-9	; see if at end of block for possible trailing invalid publics
	jb  wv_1				; no
	mov es,es:[0]			; get pointer to next block in ax
	mov bx,2				; init block position
	mov ax,es
	or  ax,ax				; make sure haven't shot past last block    
	je  wv_to_chk_segdefs	; past last block, all done

wv_1:
	mov al,es:[bx]			; get flag byte
	or  al,al				; see if zero (trailing invalid public declaration bytes)
	jne wv_1a				; no

	add bx,3				; move to next position in block
	mov ax,es
	cmp ax,alloc_ovlpubblk_ptr	; see if on last block
	jne wv_next_seg			; no, keep looking for segment partition info
	cmp bx,ovlpubblk_pos	; see if at last position
	jb  wv_next_seg			; no
	jmp NEAR PTR wv_chk_segdefs	; yes, done

wv_1a:
	mov ax,es
	cmp ax,alloc_ovlpubblk_ptr	; see if on last block
	jne wv_more_segs		; no
	cmp bx,ovlpubblk_pos	; see if at last position
	jb  wv_more_segs		; no

wv_to_chk_segdefs:
	jmp NEAR PTR wv_chk_segdefs	; yes, done

; check if room in block for reading in next segment partition info
wv_more_segs:
	cmp bx,OVLPUB_BLK_BYSIZE-9	; see if at end of block
	jb  wv_get_seg			; no
	mov es,es:[0]			; get pointer to next block in ax
	mov bx,2				; init block position

wv_get_seg:
	inc bx
	mov dx,es:[bx]			; get segment partition entry segment in dx
	mov cx,es:[bx+2]		; get total overlaid references in cx
	or  dx,dx				; see if nonzero segment partition entry
	jne wv_ovl_class		; yes

; zero segment partition entry, this is an indirect call references entry
	add bx,8				; scan past info to first overlaid public
	jcxz    wv_next_seg		; no references

wv_nonovl_loop:
	cmp bx,OVLPUB_BLK_BYSIZE-3	; see if at end of block, no room for public declaration entry info
	jb  wv_nonovl_2			; no
	mov es,es:[0]			; get pointer to next block in ax
	mov bx,2				; init block position

wv_nonovl_2:
	mov al,es:[bx]			; get overlaid public flag byte
	and al,80h				; see if still valid
	je  wv_nonovl_next		; no

	call	write_lookup_vectors	; write lookup vector for indirectly far referenced public
	dec cx					; drop count of overlaid publics

wv_nonovl_next:
	add bx,3				; move to next overlaid public entry

to_wv_next_seg:
	jcxz    wv_next_seg		; no more overlaid publics
	jmp SHORT wv_nonovl_loop	; loop back for next entry

wv_ovl_class:
	mov ax,es:[bx+6]		; get previous vector bytes count
	mov prev_vect_bytes,ax	; save to memory variable
	add bx,8				; scan past info to first overlaid public
	call	write_start_vectors
	jcxz    to_wv_next_seg	; no end vectors to write

;12/21/90, subtract off far vector count from total overlaid reference in cx
	mov ax,cx
	sub ax,es:[bx-4]
	mov total_ref,ax		; save total overlaid reference for NEARS to memory variable

	xor di,di				; di holds current overlaid public count, relative 0

wv_get_pubs:
	cmp bx,OVLPUB_BLK_BYSIZE-3	; see if at end of block, no room for public declaration entry info
	jb  wv_2				; no
	mov es,es:[0]			; get pointer to next block in ax
	mov bx,2				; init block position

wv_2:
	mov al,es:[bx]			; get overlaid public flag byte
	and al,0c0h				; see if still valid
	je  wv_next_pub			; no, try next

	call	write_end_vectors

wv_2a:
	call	write_lookup_vectors	; write vector values in lookup table

; decrement the value in cx for each near and far bit set
	mov al,es:[bx]
	and al,0c0h				; get near and far flag bits
	test	al,80h			; see if far bit set
	je  wv_near_bit			; no

	dec cx

wv_near_bit:
	and al,40h				; see if near bit set
	je  wv_next_pub			; no
	dec cx

wv_next_pub:
	add bx,3				; move to next overlaid public entry
	or  cx,cx
	jne wv_get_pubs			; more overlaid publics, get them
	jmp NEAR PTR wv_next_seg	; no overlaid publics left

; step through all segdef entries, and overlaid segment partition entries
; if overlay class and vector written bit was not set, write near vector values
; also, write relocation entry for near vectors to overlay manager
wv_chk_segdefs:
	mov ax,first_segdefblk_ptr	; get first segdef block pointer

wv_seg_blkloop:
	or  ax,ax				; see if another block
	jne wv_moreblk			; yes
	jmp NEAR PTR wv_ret		; no

wv_moreblk:
	mov es,ax				; es -> segdef block
	mov si,es				; keep -> segdef block

	mov cx,es:[0]			; get count of entries in block
	inc ax					; bump to first segdef entry in block

wv_seg_entloop:
	mov es,ax				; es -> segdef entry
	mov di,ax				; save -> segdef entry in di
	mov al,es:[28]
	test	al,1			; see if overlay class
	je  wv_next_segdef		; no

	mov ax,es:[22]			; get pointer to first segment partition entry

wv_loop:
	or  ax,ax				; see if valid segment partition pointer
	je  wv_next_segdef		; no, no more segment partition entries for this segdef
	mov es,ax				; es -> segment partition entry
	test	BYTE PTR es:[15],80h	; see if overlaid segment
	je  wv_3				; no

; overlaid segment
	test	BYTE PTR es:[15],10h	; see if near vectors were written for segment
	jne wv_next_segpart		; yes

; either overlaid segment that hasn't had near vectors or non-overlaid segment
; if non-overlaid segment, write_start_vectors will weed out those that
; already have the near vectors written, so don't bother to test for it
wv_3:
	mov dx,es				; get segment partition entry in dx for write_start_vectors
	push	si				; save critical register
	call	write_start_vectors
	pop si					; restore critical register

wv_next_segpart:
	mov ax,es:[2]			; get pointer to next segment partition entry, if any
	jmp SHORT wv_loop		; loop back until all entries for this segdef exhausted

wv_next_segdef:
	mov ax,di				; ax -> old segdef entry
	add ax,2				; move to next segdef entry in block
	loop    wv_seg_entloop	; loop through all entries in block

	mov es,si				; restore es -> segdef block
	mov ax,es:[2]			; get pointer to next block, if any
	jmp NEAR PTR wv_seg_blkloop	; loop for next block

wv_ret:
	mov ovl_code_id,0		; zero overlay identifer
	call	flush_lookup	; flush any remaining entries in lookup array to disk
	call	write_ind_call	; write indirect call table segment
	call	write_seg_call	; write segment call table for segment fixups
	ret
write_vectors   ENDP

;*****************************
;* WRITE_IND_CALL            *
;*****************************

; write indirect call table to $$_ovl_ind_call_table segment
; at offset zero is the far jump to to-root entry in the overlay manager,
; and the far jump to to-overlay entry in the overlay manager,
; destroys ax,bx,cx,dx,di,si,es

write_ind_call  PROC

; get offset of indirect call segment in data_offset
	mov ax,ind_tbl_segdef	; get segdef segment of indirect call table
	mov es,ax				; es -> segdef entry
	les ax,es:[2]			; get low/high word of offset in es:ax
	mov WORD PTR data_offset,ax
	mov ax,es
	mov WORD PTR data_offset+2,ax

; get segment of indirect call segment in data_segment variable for make_reloc_entry
	mov ax,ind_segment
	mov data_segment,ax

	push	ovl_pubcount	; save number of near calls to write on the stack
	mov si,OFFSET DGROUP:ind_vector_bytes	; es:si -> bytes to write

; code shared with segment call via segment fixup routine
write_shared:
	mov ax,ds
	mov es,ax
	mov cx,5				; write 1 vector to overlay manager
	call	write_bytes		; write the bytes

	pop cx					; write as many near calls as are in cx

	mov dx,3				; fixup location in segment for root to overlay
	call	make_reloc_entry

	add WORD PTR data_offset,10h	; adjust past paragraph of vectors
	adc WORD PTR data_offset+2,0	; carry to high word
	mov di,0ffedh			; initialize CALL disp16 offset

ind_loop2:
	mov bx,OFFSET DGROUP:ind_tbl_array	; init table position

write_ind_loop:
	jcxz    write_ind_out	; done, pass any remaining calls to write_bytes routine

	mov BYTE PTR [bx],0e8h	; CALL disp16 opcode
	mov [bx+1],di			; save computed offset
	sub di,3				; adjust di for next call

	dec cx					; drop count of calls to write
	add bx,3				; adjust to new near call position
	cmp bx,OFFSET DGROUP:ind_tbl_array+800h-3	; see if table overflowed, time to write to disk
	jb  write_ind_loop		; no, keep writing call bytes to table
	call	write_ind_out	; sneaky call that shares procedure return with subprocedure return
	jmp SHORT ind_loop2		; loop back for next set of calls, reinit'ing bx

; write the indirect call tables to the write_bytes routine
write_ind_out:
	push	cx				; save critical register
	push	di
	cmp bx,OFFSET DGROUP:ind_tbl_array	; see if any entries to write
	jbe wic_ret				; no
	mov cx,bx
	mov si,OFFSET DGROUP:ind_tbl_array	; es:si -> bytes to write
	sub cx,si				; get number of bytes to write
	mov dx,cx				; save in dx

	push	ovl_code_id		; save current overlay code identifier
	xor ax,ax
	mov ovl_code_id,ax		; zero for write_bytes routine so writes to root file
	call	write_bytes		; write the bytes
	pop ovl_code_id			; restore current overlay code identifier

	add WORD PTR data_offset,dx	; update data_offset variable for next write
	adc WORD PTR data_offset+2,0	; carry to high word

wic_ret:
	pop di
	pop cx					; restore critical register
	ret
write_ind_call  ENDP

;*****************************
;* WRITE_SEG_CALL            *
;*****************************

; write segment call table to $$_ovl_seg_call_table segment
; at offset zero is the far jump to segmet call entry in the overlay manager,
; destroys ax,bx,cx,dx,di,si,es

write_seg_call  PROC

; get offset of indirect call segment in data_offset
	mov ax,segcall_tbl_segdef	; get segdef segment of indirect call table
	mov es,ax				; es -> segdef entry
	les ax,es:[2]			; get low/high word of offset in es:ax
	mov WORD PTR data_offset,ax
	mov ax,es
	mov WORD PTR data_offset+2,ax

; get segment of segment call segment in data_segment variable for make_reloc_entry
	mov ax,segcall_segment
	mov data_segment,ax

	push	ovl_count		; save number of near calls to write on the stack
	mov si,OFFSET DGROUP:segcall_vect_bytes	; es:si -> bytes to write

	jmp NEAR PTR write_shared	; jump to code shared with the indirect call segment

write_seg_call  ENDP

;*****************************
;* WRITE_SEG_REL_ENTRY       *
;*****************************

; write relocation entry in main program for root overlay class segment vectors
; destroys ax,bx,dx,di

write_seg_rel_entry PROC
	mov ax,es:[2]			; get segment offset into data_offset variable for make_reloc_entry proc
	mov WORD PTR data_offset,ax
	mov bx,es:[4]			; get high word
	mov WORD PTR data_offset+2,bx

; convert bytes in bx:ax to paragraphs in ax for data_segment variable for make_reloc_entry
	shr bx,1
	rcr ax,1				; /2
	shr bx,1
	rcr ax,1				; /4
	shr bx,1
	rcr ax,1				; /8
	shr bx,1
	rcr ax,1				; /16
	mov data_segment,ax

	mov dx,1dh				; near vector fixup location in segment
	call	make_reloc_entry
	mov dx,2ah				; shared vector fixup location in segment
	call	make_reloc_entry
	ret
write_seg_rel_entry ENDP

;*****************************
;* WRITE_LOOKUP_VECTORS      *
;*****************************

; write offset and segment values for each overlaid public entry
; at the appropriate place in the $$_ovl_lookup_table segment.
; offset  == the public declarations offset, plus segment partition offset and
; master segdef entry normalized offset if the segment partition is not overlaid.
; segment == the segment partition overlay identifier if overlaid, or
; the master segdef entry offset (normalized) if not overlaid.
; upon entry es:bx -> first overlaid public flag byte in overlaid public table
; dx holds segment partition entry segment
; destroys ax,si

write_lookup_vectors    PROC
	push	bx				; save critical registers
	push	cx
	push	dx
	push	es

	mov es,es:[bx+1]		; es -> overlaid public
	test	BYTE PTR es:[15],2	; see if vector already written for public
	je  wlv_1				; no
	jmp NEAR PTR wlv_ret	; yes

wlv_1:
	or  BYTE PTR es:[15],2	; set vector written flag
	mov cx,es:[8]			; get public offset
	mov es,es:[0]			; es -> segment partition entry owning public
	mov dl,es:[15]			; get overlay flag for segment
	and dl,80h				; see if segment overlaid
	jne wlv_2				; yes

	add cx,es:[0]			; add in segment partition offset
	push	es				; save es -> segment partition entry
	pop es					; restore es -> segment partition entry

wlv_2:
	mov bx,lookup_tbl_pos	; get current position in lookup table
	and bx,7ffh				; get position in array
	add bx,OFFSET DGROUP:lookup_tbl_array	; add in array's offset in memory
	mov [bx],cx				; save offset
	add bx,2				; move to segment position in array
	cmp dl,80h				; see if segment if overlaid
	jne wlv_3				; no

; overlaid segment
; 12/25/90, set flag to indicate overlaid segment (reset bit 6 of byte 6)
; write offset to near vector value
	mov cx,es:[12]			; get segment length
	mov ax,es:[14]			; get vector count
	and ah,7				; mask off flag bits (only bit 0-2 used)
	mov dx,7				; 7 bytes per vector
	mul dx					; dx:ax holds vector bytes, dx always zero
	add ax,20h				; adjust for near/shared vector paragraphs
	sub cx,ax				; cx holds near vector offset value
	mov [bx+2],cx			; save offset to near vector
	mov WORD PTR [bx+4],0	; reset root segment bit
	mov cx,es:[4]			; get overlay identifier
	jmp SHORT wlv_4			; bypass root segment code

; root segment
; 12/25/90, set flag to indicate root segment (set bit 6 of byte 6)
wlv_3:
	mov es,es:[4]			; es -> master segdef entry

; write offset to near vector value
	mov cx,es:[6]			; get segment length
	mov ax,es:[30]			; get count of vector bytes
	add ax,20h				; adjust for near/shared vector paragraphs
	sub cx,ax				; cx holds near vector offset value
	mov [bx+2],cx			; save offset to near vector
	mov WORD PTR [bx+4],40h	; set root segment bit

	mov cx,es:[2]			; cx holds low word of segment offset
	mov ax,es:[4]			; ax holds high word of segment offset
	shr ax,1				; convert to paragraph for segment value
	rcr cx,1
	shr ax,1
	rcr cx,1
	shr ax,1
	rcr cx,1
	shr ax,1
	rcr cx,1				; cx holds normalized segment value

wlv_4:
	mov [bx],cx				; save segment

	mov ax,lookup_tbl_pos
	add ax,8
	mov lookup_tbl_pos,ax	; bump position to next seg:off pair
	and ax,7ffh				; see if at end of array
	jne wlv_ret				; no
	call	flush_lookup	; flush the previous lookup array to disk

wlv_ret:
	pop es					; restore critical registers
	pop dx
	pop cx
	pop bx
	ret
write_lookup_vectors    ENDP

;*****************************
;* FLUSH_LOOKUP              *
;*****************************

; flush lookup array to disk
; destroys ax,bx,cx,dx,si,es

flush_lookup    PROC
	push	di				; save critical register
	mov ax,lookup_tbl_segdef	; get segdef segment of overlay lookup table
	mov es,ax				; es -> segdef entry
	mov ax,es:[2]			; get low word of offset
	mov WORD PTR data_offset,ax
	mov ax,es:[4]			; get high word
	mov WORD PTR data_offset+2,ax

	mov ax,lookup_tbl_pos
	mov cx,ax
	jcxz    fl_ret			; no bytes to write
	and cx,7ffh
	jne fl_1				; not an even 800h byte block to write
	mov cx,800h				; an even 800h byte block

fl_1:
	cmp ax,800h				; see if previous bytes written
	jbe fl_2				; no, make no adjustment

	sub ax,cx				; ax -> place in lookup table segment to write to

	add WORD PTR data_offset,ax	; adjust offset to proper place in segment
	adc WORD PTR data_offset+2,0	; carry to high word

fl_2:
	mov ax,ds
	mov es,ax
	mov si,OFFSET DGROUP:lookup_tbl_array	; es:si -> bytes to write

	push	ovl_code_id		; save current overlay code identifier
	xor ax,ax
	mov ovl_code_id,ax		; zero for write_bytes routine so writes to root file
	call	write_bytes
	pop ovl_code_id			; restore current overlay code identifier

fl_ret:
	pop di					; restore critical register
	ret
flush_lookup    ENDP

;*****************************
;* SET_VECTOR_BYTES          *
;*****************************

; setup the constant vectors to and from overlay manager in vector byte tables
; destroys ax,bx,cx,dx,di,es

set_vector_bytes    PROC

	mov no_match_okay,1		; set flag that no matches in public search are okay

; write to_vector_bytes segment:offset
	mov di,OFFSET DGROUP:vector_text
	call	find_pubdecl_entry	; find the symbol
	or  ax,ax				; make sure nonzero
	je  svb_mgrerr			; not found, assume overlay manager wasn't linked in

	call	get_symbol_segoff
	mov WORD PTR to_vector_bytes,ax	; save offset value
	mov WORD PTR to_vector_bytes+2,dx	; save segment value

; write ret_vector_bytes segment:offset
	mov di,OFFSET DGROUP:ret_text
	call	find_pubdecl_entry	; find the symbol
	or  ax,ax				; make sure nonzero
	je  svb_mgrerr			; not found, assume overlay manager wasn't linked in

	call	get_symbol_segoff
	mov WORD PTR ret_vector_bytes,ax	; save offset value
	mov WORD PTR ret_vector_bytes+2,dx	; save segment value

; write to_2o_bytes segment:offset
	mov di,OFFSET DGROUP:ovl_2o_text
	call	find_pubdecl_entry	; find the symbol
	or  ax,ax				; make sure nonzero
	je  svb_mgrerr			; not found, assume overlay manager wasn't linked in

	call	get_symbol_segoff
	mov WORD PTR to_2o_bytes,ax		; save offset value
	mov WORD PTR to_2o_bytes+2,dx	; save segment value

; write to_segcall_bytes segment:offset
	mov di,OFFSET DGROUP:ovl_segcall_text
	call	find_pubdecl_entry	; find the symbol
	or  ax,ax				; make sure nonzero
	je  svb_mgrerr			; not found, assume overlay manager wasn't linked in

	call	get_symbol_segoff
	mov WORD PTR to_segcall_bytes,ax	; save offset value
	mov WORD PTR to_segcall_bytes+2,dx	; save segment value

	mov no_match_okay,0		; done, reset flag so remaining no matches generate internal errors

	ret

; overlay manager not linked in, fatal error
svb_mgrerr:
	mov ax,MISSING_OVLMGR_ERR
	jmp NEAR PTR link_error

set_vector_bytes    ENDP

;*****************************
;* GET_SYMBOL_SEGOFF         *
;*****************************

; return segment:offset of variable in ax:dx
; upon entry ax -> public declaration entry of symbol
; destroys ax,bx,cx,dx,es

get_symbol_segoff   PROC
	mov es,ax				; es -> public declaration entry
	mov bx,es:[8]			; get public offset
	mov es,es:[0]			; es -> segment partition entry
	mov es,es:[4]			; es -> master segdef entry
	mov dx,es:[2]			; get segment offset low word
	mov ax,dx
	mov cx,es:[4]			; get segment offset high word
	and ax,15				; get leftover bytes from canonical (normalized) segment
	shr cx,1				; convert bytes to paragraphs for segment value
	rcr dx,1
	shr cx,1
	rcr dx,1
	shr cx,1
	rcr dx,1
	shr cx,1
	rcr dx,1				; dx now holds variable segment value
	add ax,bx				; ax holds variable offset value
	ret
get_symbol_segoff   ENDP

;*****************************
;* WRITE_START_VECTORS       *
;*****************************

; write the near and shared vectors
; upon entry dx holds segment partition entry segment
; destroys ax,si

write_start_vectors PROC
	push	cx				; save critical registers
	push	dx
	push	di
	push	es
	mov es,dx				; es -> segment partition entry
	mov al,es:[15]			; get overlay flag
	and al,80h				; see if overlaid
	je  wsv_not_overlaid	; no

	or  BYTE PTR es:[15],40h	; set vector values written flag
	mov ax,es:[4]			; get overlay identifier
	mov ovl_code_id,ax		; save to identifier flag
	mov WORD PTR source_id_word,ax	; save identifier in source id

	mov cx,es:[12]			; get segment length
	mov ax,es:[14]			; get vector count
	and ah,7				; mask off flag bits (only bit 0-2 used)
	mov dx,7				; 7 bytes per vector
	mul dx					; dx:ax holds vector bytes, dx always zero
	add ax,20h				; adjust for near/shared vector paragraphs
	sub cx,ax				; cx holds near vector offset value
	mov WORD PTR data_offset,cx	; save offset to near vector
	xor ax,ax
	mov WORD PTR data_offset+2,ax	; zero high word of offset
	jmp SHORT wsv_shared	; jump to shared portion of code

wsv_not_overlaid:
	mov ovl_code_id,0		; zero overlay identifer
	mov es,es:[4]			; es -> master segdef entry for segment partition
	mov al,40h
	test	es:[28],al		; see if vector values already written
	jne wsv_ret				; yes, don't write them again
	or  es:[28],al			; set vector values written flag
	mov al,80h
	mov BYTE PTR source_id_word+1,al	; set high bit of source id to indicate call from root

	mov dx,es:[6]			; get segment length
	or  dx,dx				; make sure nonzero length segment
	je  wsv_ret				; zero length
	mov ax,es:[30]			; get count of vector bytes
	add ax,20h				; adjust for near/shared vector paragraphs
	sub dx,ax				; dx holds near vector offset value
	mov cx,dx				; save value in cx
	mov ax,es:[2]			; get low word of segment offset
	add ax,dx				; calculate offset to near vector
	mov WORD PTR data_offset,ax
	mov dx,es:[4]			; get high word of segment offset
	adc dx,0				; carry to high word
	mov WORD PTR data_offset+2,dx

; convert bytes in dx:ax to paragraphs in ax for data_segment variable for make_reloc_entry
	shr dx,1
	rcr ax,1				; /2
	shr dx,1
	rcr ax,1				; /4
	shr dx,1
	rcr ax,1				; /8
	shr dx,1
	rcr ax,1				; /16
	mov data_segment,ax

; write relocation items for root near/shared vector jumps to overlay manager
	mov dx,0dh				; near vector
	call	make_reloc_entry
	mov dx,1ah				; shared vector
	call	make_reloc_entry

wsv_shared:
; write the pop/call cs-relative data address, offset to vector in cx
	mov nearv_offset,cx		; save near vector offset for write_end_vectors use
	add cx,1ch				; bump offset to cs_relative data address
	mov WORD PTR near_pop,cx	; update pop address
	mov WORD PTR near_call,cx	; update call address

	mov ax,ds
	mov es,ax
	mov si,OFFSET DGROUP:near_vector_bytes	; es:si -> bytes to write
	mov cx,28				; write 28 bytes
	call	write_bytes

wsv_ret:
	pop es					; restore critical registers
	pop di
	pop dx
	pop cx
	ret
write_start_vectors ENDP

;*****************************
;* WRITE_END_VECTORS         *
;*****************************

; write vectors at end of segment, public vectors
; upon entry es:bx -> first overlaid public flag byte in overlaid public table
; cx holds total count of overlaid publics
; di holds current count of vectored overlaid publics, relative 0
; dx holds segment partition entry segment
; updates di
; destroys ax,si

write_end_vectors   PROC
	push	bx				; save critical registers
	push	cx
	push	dx
	push	es

	mov cl,es:[bx]			; get overlaid public flag byte
	push	dx				; save -> segment partition entry on stack
	mov ax,es:[bx+1]		; get public declaration entry segment
	pop es					; es -> segment partition entry
	or  BYTE PTR es:[15],20h	; set end vectors written bit

	test	cl,80h			; see if far reference, don't physically write far vectors
	je  wev_writem			; near reference, write the vector
	jmp NEAR PTR wev_ret	; far vector, ignore it

wev_writem:
	push	ax				; save public declaration entry segment to stack
	test	BYTE PTR es:[15],80h	; see if segment overlaid
	jne wev_segovl			; yes

; segment not overlaid, get master segdef length
	mov es,es:[4]			; es -> segdef entry
	mov ax,es:[2]
	mov WORD PTR data_offset,ax	; get offset of segment in data_offset for write_bytes
	mov ax,es:[4]
	mov WORD PTR data_offset+2,ax	; get high word
	mov bx,es:[6]			; bx holds length of segment

	sub bx,es:[30]			; subtract off all end vector bytes
	add bx,prev_vect_bytes	; add in previous vector bytes total for this segment
	mov ax,7
	mul di					; compute proper adjustment for vector in ax
	add bx,ax				; bx holds offset to proper vector within segment
	jmp SHORT wev_2			; bypass overlaid segment code

wev_segovl:
	mov prev_vect_bytes,0	; zero previous vector bytes adjustment
	mov bx,es:[12]			; get length of segment in bx
	xor ax,ax
	mov WORD PTR data_offset,ax	; zero the data_offset for writes to .OVL file
	mov WORD PTR data_offset+2,ax

; calculate proper segment offset to write vector bytes
; segment length-(total_ref-di)*7
	mov ax,total_ref		; get total count
	sub ax,di				; subtract off current count
	mov dx,7
	mul dx					; get amount to subtract off of segment length
	sub bx,ax				; bx holds segment offset

wev_2:
	add WORD PTR data_offset,bx
	adc WORD PTR data_offset+2,0

; calculate offset to shared vector
	mov ax,9
	add ax,nearv_offset
	sub ax,bx				; ax should hold offset to shared vector from near call
	mov WORD PTR offset_to_shared,ax 

	pop es					; es -> public declaration entry
	mov ax,es:[2]			; get overlaid public number
	mov WORD PTR dest_id_word,ax	; save in destination id word

; set or reset bit 6 to reflect whether calling root or overlay
	test	BYTE PTR es:[15],1	; see if in overlay
	je  wev_3				; no
	and BYTE PTR dest_id_word+1,0bfh	; reset bit 6 to indicate calling overlay
	jmp SHORT wev_4			; bypass root code bit setup

wev_3:
	or BYTE PTR dest_id_word+1,40h	; set bit 6 to indicate calling root

wev_4:
	mov ax,ds
	mov es,ax				; es -> warplink data

	mov dl,cl				; dl holds flag byte

wev_near:
	and dl,40h				; see if near bit set
	je  wev_ret				; no
	and BYTE PTR dest_id_word+1,7fh	; reset high bit to indicate near call
	mov si,OFFSET DGROUP:public_vector_bytes	; es:si -> bytes to write
	mov cx,7				; write 7 bytes
	push	di				; save critical register
	call	write_bytes
	pop di					; restore critical register
	inc di					; bump count of vectored publics

wev_ret:
	pop es					; restore critical registers
	pop dx					; restore critical registers
	pop cx
	pop bx
	ret
write_end_vectors   ENDP

;*****************************
;* WRITE_OVL_RELOC           *
;*****************************

; write relocation item to overlay file
; upon entry dx == record offset
; destroys ax,dx,di

write_ovl_reloc PROC
	mov ax,WORD PTR data_offset	; get program offset of data low word, high word should be zero
	add ax,dx				; add in offset in data record
	push	ax				; save it on the stack
	mov ax,ovl_code_id		; get current id code
	cmp ax,reloc_ovl_id		; see if matches relocation array id
	je  wor_2				; yes
	call	flush_reloc_array	; no, flush relocation array
	xor ax,ax
	mov reloc_ovl_pos,ax	; reset relocation array position
	mov ax,ovl_code_id
	mov reloc_ovl_id,ax		; init relocation array overlay id to current value

wor_2:
	mov ax,reloc_ovl_pos

	or  ax,ax				; see if at zero position
	je  wor_3				; yes

	and ax,3ffh				; see if array overflow
	jne wor_3				; no

; relocation array will overflow, flush it
	call	flush_reloc_array

wor_3:
	mov di,reloc_ovl_pos
	and di,3ffh				; get position in array, adjusting for any wraparound
	shl di,1				; make a word offset
	add di,OFFSET DGROUP:ovl_reloc_array	; add in array's position in memory
	pop ax					; get offset in data record off of stack
	mov [di],ax				; save it to the array
	inc reloc_ovl_pos		; bump current position in array
	ret
write_ovl_reloc ENDP

;*****************************
;* FLUSH_RELOC_ARRAY         *
;*****************************

; flush overlay relocation array to disk
; destroys ax,di

flush_reloc_array   PROC
	push	bx				; save critical register
	push	cx
	push	dx

	mov ax,reloc_ovl_id		; get relocation array overlay
	or  ax,ax				; check if valid
	je  fra_ret				; zero, no flush needed

	call	seek_to_ovl
	mov cx,reloc_ovl_pos	; get current relocation array position
	jcxz    fra_ret			; no bytes to write
	shl cx,1				; convert to word count
	mov di,cx				; save relocation array byte count in di
	and di,7ffh				; ignore any wrap (bytes previously written)

	jne fra_1				; remainder bytes to write
	mov di,800h				; even block to write

fra_1:

	cmp is_clarion,0		; see if clarion switch set
	je  fra_notclar			; no
	mov dx,14				; adjust file seek for overlay system info
	jmp SHORT fra_zerohigh

fra_notclar:
	mov dx,10				; adjust file seek for overlay system info

fra_zerohigh:
	xor cx,cx				; zero high word of offset

; if reloc_ovl_pos is > EXTDEF_MAX, must adjust past previously written chunks
	mov ax,reloc_ovl_pos
	shl ax,1				; convert to byte count
	cmp ax,800h
	jbe fra_1a				; no previous bytes

	dec ax
	and ax,0f800h			; get previous byte count amount

	add dx,ax				; adjust file offset to proper position in overlay relocation table
	adc cx,0				; carry to high word

fra_1a:
	cmp is_emsxms_ovl,0		; see if overlay stashed in EMS/XMS
	je  fra_noems			; no

	call	ems_ovl_seek	; seek to proper EMS position
	mov cx,di				; get count of bytes to write
	mov dx,OFFSET DGROUP:ovl_reloc_array	; ds:dx -> write buffer
	call	ems_ovl_write	; write to overlay in EMS
	jmp SHORT fra_ret		; bypass file i/o

fra_noems:
	mov bx,ovl_handle
	mov ax,4201h			; move file pointer, offset from current file position
	int 21h
	call	restore_ems_map
	jnc fra_2				; no errors

fra_to_doserr:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

fra_2:
	mov cx,di				; get count of bytes to write
	mov dx,OFFSET DGROUP:ovl_reloc_array	; ds:dx -> write buffer
	mov ah,40h				; write to file
	int 21h
	call	restore_ems_map
	jc  fra_to_doserr		; error occurred

fra_ret:
	pop dx					; restore critical register
	pop cx
	pop bx
	ret
flush_reloc_array   ENDP

;*****************************
;* SEEK_TO_OVL               *
;*****************************

; seek to overlay file position in .OVL, based on overlay identifier in ax
; returns overlay file handle in bx
; destroys ax,bx,cx,dx

seek_to_ovl PROC

; get overlay file position from table
	dec ax					; make identifier relative zero
	shl ax,1
	shl ax,1				; convert to doubleword
	mov bx,ax
	push	ds				; save critical register
	mov ds,ovl_filepos_blk
	mov dx,[bx]				; get low word file offset
	mov cx,[bx+2]			; get high word file offset
	pop ds					; restore ds -> warplink data

	cmp is_emsxms_ovl,0		; see if overlay stashed in EMS/XMS
	je  sto_seek			; no

	mov WORD PTR curr_ems_pos,dx	; save current EMS write position
	mov WORD PTR curr_ems_pos+2,cx
	ret

; seek to file position
sto_seek:
	mov bx,ovl_handle
	mov ax,4200h			; move file pointer, offset from start of file
	int 21h
	call	restore_ems_map
	jc  sto_to_doserr		; error occurred

sto_ret:
	ret

sto_to_doserr:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

seek_to_ovl ENDP

;*****************************
;* OVL_FILE_WRITE            *
;*****************************

; write bytes to overlay file, write to memory page if possible
; upon entry es:[si] -> buffer to write from, cx == number of bytes to write,
; bp==buffer end, data_offset == offset from start of program
; updates si
; destroys ax,bx,cx,dx,di

ovl_file_write  PROC
	mov di,cx				; save number of bytes to write in di
	mov ax,ovl_code_id		; get current overlay code id
	cmp ax,ovl_page_owner	; see if matches owner of overlay page
	je  ofw_2				; yes

; current overlay page does not match current overlay to write, zero out dirty bit table
	xor ax,ax
	mov WORD PTR dirty_bit_table,ax
	mov WORD PTR dirty_bit_table+2,ax
	mov WORD PTR dirty_bit_table+4,ax
	mov WORD PTR dirty_bit_table+6,ax
	mov WORD PTR dirty_bit_table+8,ax
	mov WORD PTR dirty_bit_table+10,ax
	mov WORD PTR dirty_bit_table+12,ax
	mov WORD PTR dirty_bit_table+14,ax

; need to flush page, new bytes don't map onto it
ofw_flush:
	call	flush_ovl_page	; flush the old page to disk
	mov ax,ovl_code_id
	mov ovl_page_owner,ax	; update owner of overlay page
	mov ax,WORD PTR data_offset
	and ax,0f000h			; clear out page remainder
	mov ovl_page_base,ax	; save new page base
	call	read_new_ovl_page	; read the new overlay page from disk
	jmp SHORT ofw_write		; bypass other checking code

ofw_2:
	mov ax,ovl_page_base	; ax holds start byte of overlay page
	mov bx,ax
	add bx,0fffh			; bx holds highest byte of overlay page
	mov cx,WORD PTR data_offset	; cx holds start byte of offset to write
	mov dx,di				; get count of bytes to write
	dec dx					; make relative zero
	add dx,cx				; dx holds highest byte to write

; see if highest byte to write is below page start or lowest byte to write
; is above page end, forces a page flush
	cmp dx,ax
	jb  ofw_flush			; doesn't map on page

	cmp cx,bx
	ja  ofw_flush			; doesn't map onto page at all

; at least part of the bytes to write map onto current page
ofw_write:
	mov bx,WORD PTR data_offset
	and bx,0fffh			; convert to 4K offset
	mov cx,di				; get count of bytes to write
	dec cx					; make relative zero
	add cx,bx				; cx holds highest byte to write on page
	cmp cx,0fffh			; see if cx is off the page
	jbe ofw_4				; no
	mov cx,0fffh			; put cx at end of page

ofw_4:
	cmp cx,ovl_high_page_byte	; see if highest byte exceeds previous
	jbe ofw_4a				; no
	mov ovl_high_page_byte,cx	; save highest byte written to value

ofw_4a:
	sub cx,bx
	inc cx					; cx holds count of bytes to write on this page
	sub di,cx				; subtract from total count of bytes to write

	xchg bx,di				; save di value in bx
	push	ds
	mov dx,es
	mov ax,ovl_ioblk
	mov es,ax				; es:di -> destination buffer
	mov ds,dx				; ds:si -> source buffer

	mov ax,si				; get offset of source
	add ax,cx				; add in number of bytes to write
	jc  ofw_buff_wrap		; overflow, buffer will wrap
	cmp ax,bp				; see if past buffer end for total bytes written
	jbe ofw_no_buff_wrap	; no

ofw_buff_wrap:
	mov dx,cx				; save old byte count to write
	mov cx,bp				; get buffer end
	sub cx,si				; compute bytes to buffer end
	mov ax,cx				; save byte count written this pass
	shr cx,1				; convert byte count to write to words
	rep movsw				; move the string
	rcl cx,1				; pick up carry
	rep movsb				; transfer leftover byte, if any
	xor si,si				; wrap source offset to start of i/o buffer
	sub dx,ax				; update total count, subtracting off byte count written
	mov cx,dx				; get new total in cx

ofw_no_buff_wrap:
	shr cx,1				; convert byte count to write to words
	rep movsw				; move the string
	rcl cx,1				; pick up carry
	rep movsb				; transfer leftover byte, if any

ofw_5:
	mov ax,ds
	mov es,ax				; restore es -> buffer
	pop ds					; restore ds -> warplink data
	mov di,bx				; restore di value
	or  di,di				; see if more bytes to write
	je  ofw_ret				; no

	mov ax,WORD PTR data_offset	; get data_offset
	and ax,0f000h			; clear out remainder
	add ax,1000h			; bump to next page
	mov WORD PTR data_offset,ax	; save back to data_offset variables
	jmp NEAR PTR ofw_flush	; loop back to flush and write bytes

ofw_ret:
	ret
ovl_file_write  ENDP

;*****************************
;* FLUSH_OVL_PAGE            *
;*****************************

; flush overlay page to disk
; destroys ax,bx,cx,dx

flush_ovl_page  PROC
	mov ax,ovl_page_owner	; get id of overlay owning page
	or  ax,ax				; make sure non-null
	jne fop_flush			; yes
	ret						; null, no page to flush

fop_flush:
	call	seek_to_ovl		; seek to overlay file of overlay owning page (id in ax)

; read system info bytes
	cmp is_clarion,0		; see if clarion switch set
	je  fop_notclar			; no
	mov cx,14				; 14 system bytes, 7 words
	jmp SHORT fop_1

fop_notclar:
	mov cx,10				; 10 system bytes, 5 words

fop_1:
	mov dx,OFFSET DGROUP:ovl_sys_info	; ds:dx -> read buffer
	cmp is_emsxms_ovl,0		; see if overlay in EMS/XMS
	je  fop_noems			; no
	call	ems_ovl_read	; read from EMS
	jmp SHORT fop_2

fop_noems:
	mov ah,3fh				; read file
	int 21h
	call	restore_ems_map
	jnc fop_2

fop_to_doserr:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

; seek past relocation table to proper overlay code page
fop_2:
	mov dx,ovl_sys_info+2	; get offset to overlay code
	xor cx,cx				; zero high word of offset
	add dx,ovl_page_base	; add in overlay code base
	adc cx,0				; carry to high word

	cmp is_clarion,0		; see if clarion switch set
	je  fop_notclar2		; no
	sub dx,14				; subtract off system info bytes already read
	jmp SHORT fop_3

fop_notclar2:
	sub dx,10				; subtract off system info bytes already read

fop_3:
	sbb cx,0				; borrow from high word
	cmp is_emsxms_ovl,0		; see if overlay in EMS/XMS
	je  fop_4				; no

	call	ems_ovl_seek	; read from EMS
	jmp SHORT fop_5

fop_4:
	mov ax,4201h			; move file pointer, offset from current file position
	int 21h
	call	restore_ems_map
	jc  fop_to_doserr		; error occurred

fop_5:
	mov ax,ovl_ioblk
	mov cx,ovl_high_page_byte	; get count of bytes to write, relative zero
	inc cx					; make relative 1
	xor dx,dx				; zero offset
	push	ds				; save ds -> warplink data

	cmp is_emsxms_ovl,0		; see if overlay in EMS/XMS
	mov ds,ax				; ds:dx -> write buffer, NO CHANGE TO FLAGS
	je  fop_6				; no
	call	ems_ovl_write
	pop ds					; restore ds -> warplink data
	jmp SHORT fop_7

fop_6:
	mov ah,40h				; write to file
	int 21h
	call	restore_ems_map

	pop ds					; restore ds -> warplink data
	jc  fop_to_doserr		; error occurred

fop_7:
	mov ovl_high_page_byte,0	; re-init highest page byte written

fop_ret:
	ret
flush_ovl_page  ENDP

;*****************************
;* READ_NEW_OVL_PAGE         *
;*****************************

; read new overlay page in from disk, if necessary (checked from dirty bit table)
; otherwise just zero out page.
; This assumes that binary data from one overlaid code segment is not intermixed
; with binary data from another overlaid code segment
; ovl_page_base holds offset in overlay file
; destroys ax,bx,cx,dx

read_new_ovl_page   PROC
	push	si				; save critical register

	mov al,BYTE PTR ovl_page_base+1	; get high byte of offset (always on 4K boundary)
	xor ah,ah				; ax holds offset/256
	shr al,1				; /512
	shr al,1				; /1024
	shr al,1				; /2048
	shr al,1				; /4096
	mov bx,OFFSET DGROUP:dirty_bit_table
	add bx,ax				; bx offsets to proper dirty bit (byte)
	mov al,[bx]				; get dirty bit status
	or  al,al				; see if dirty bit set
	jne rn_dirtyset			; yes

	inc ah					; ah==1
	mov [bx],ah				; set dirty bit for future reads

; 12/29/92
; if overlay size <4K (next overlay pos-current overlay pos), use that
; figure for zero'ing
	mov ax,ovl_code_id
	cmp	ax,ovl_count
	jae	rn_2048				; last overlay, no next to compute size from

	dec ax					; make identifier relative zero
	shl ax,1
	shl ax,1				; convert to doubleword
	mov bx,ax
	push	ds				; save critical register
	mov ds,ovl_filepos_blk
	mov	dx,[bx+4]			; get next overlay low word file offset
	mov	cx,[bx+6]			; get next overlay high word file offset
	sub	dx,[bx]				; subtract off current overlay position
	sbb	cx,[bx+2]
	pop ds					; restore ds -> warplink data
	or	cx,cx				; see if >64K
	jne	rn_2048				; yes, use default 2K words
	cmp	dx,4096				; see if >=4K
	jae	rn_2048				; yes, use default 2K words
	mov	cx,dx
	inc	cx
	shr	cx,1				; convert to words
	jmp	SHORT rn_zero

rn_2048:
	mov cx,2048				; zero out 4K page (2K words)

rn_zero:
	push	es				; save critical registers
	push	di
	mov ax,ovl_ioblk
	mov es,ax
	xor ax,ax
	mov di,ax				; es:di -> start of page
	rep stosw
	pop di					; restore critical registers
	pop es
	pop	si
	ret						; done zero'ing page, return

rn_dirtyset:
	mov ax,ovl_code_id
	call	seek_to_ovl		; seek to overlay file

; read system info bytes
	cmp is_clarion,0		; see if clarion switch set
	je  rn_notclar			; no
	mov cx,14				; 14 system bytes, 7 words
	jmp SHORT rn_1

rn_notclar:
	mov cx,10				; 10 system bytes, 5 words

rn_1:
	mov dx,OFFSET DGROUP:ovl_sys_info	; ds:dx -> read buffer
	cmp is_emsxms_ovl,0		; see if overlay in EMS/XMS
	je  rn_noems			; no
	call	ems_ovl_read	; read from EMS
	jmp SHORT rn_2

rn_noems:
	mov ah,3fh				; read file
	int 21h
	call	restore_ems_map
	jnc rn_2

rn_to_doserr:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

; seek past relocation table to proper overlay code page
rn_2:
	mov dx,ovl_sys_info+2	; get offset to overlay code
	xor cx,cx				; zero high word of offset
	add dx,ovl_page_base	; add in overlay code base
	adc cx,0				; carry to high word

	cmp is_clarion,0		; see if clarion switch set
	je  rn_notclar2			; no
	sub dx,14				; subtract off system info bytes already read
	jmp SHORT rn_3

rn_notclar2:
	sub dx,10				; subtract off system info bytes already read

rn_3:
	sbb cx,0				; borrow from high word

	cmp is_emsxms_ovl,0		; see if overlay in EMS/XMS
	je  rn_4				; no

	call	ems_ovl_seek	; read from EMS
	jmp SHORT rn_5

rn_4:
	mov ax,4201h			; move file pointer, offset from current file position
	int 21h
	call	restore_ems_map
	jc  rn_to_doserr		; error occurred

rn_5:
	mov ax,ovl_ioblk

; 12/29/92
	cmp	BYTE PTR ovl_sys_info+5,0	; see if 4K or greater overlay code size (100h paras, ignore low byte)
	jne	rn_4096				; yes
	mov	cx,WORD PTR ovl_sys_info+4	; only read in as many bytes as in overlay code
	add	cx,cx				; convert paras to bytes, *2
	add	cx,cx				; *4
	add	cx,cx				; *8
	add	cx,cx				; *16
	jmp	SHORT rn_read

rn_4096:
	mov cx,4096				; read 4K bytes

rn_read:
	xor dx,dx				; zero offset
	push	ds				; save ds -> warplink data

	cmp is_emsxms_ovl,0		; see if overlay in EMS/XMS
	mov ds,ax				; ds:dx -> write buffer, NO CHANGE TO FLAGS
	je  rn_6				; overlay not in EMS/XMS

; 12/29/92
	call	ems_ovl_read
	jmp SHORT rn_done

rn_6:
	mov ah,3fh				; read file
	int 21h
	call	restore_ems_map
	jc  rn_to_doserr		; error occurred

rn_done:
	pop	ds					; restore ds -> warplink data
	pop	si					; restore critical register
	ret
read_new_ovl_page   ENDP

;*****************************
;* WRITE_CLAR_DATA           *
;*****************************

; write bytes to overlaid Clarion _DT or _DAT segment
; upon entry es:[si] -> buffer to write from, cx == number of bytes to write,
; updates si
; destroys ax,bx,cx,dx,di

write_clar_data PROC
	push	cx				; save bytes to write
	mov ax,ovl_data_id		; get overlay identifier for overlaid data
	shr ax,1				; convert to byte value
	call	seek_to_ovl		; seek to start of code

; read system info bytes
wcd_readsys:
	mov cx,14				; 14 system bytes, 7 words
	mov dx,OFFSET DGROUP:ovl_sys_info	; ds:dx -> read buffer

	cmp is_emsxms_ovl,0		; see overlay in EMS/XMS
	je  wcd_1				; no
	call	ems_ovl_read
	jmp SHORT wcd_2

wcd_1:
	mov ah,3fh				; read file
	int 21h
	call	restore_ems_map
	jnc wcd_2
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

; seek past relocation table to proper overlay code page
wcd_2:
	mov dx,ovl_sys_info+2	; get offset to overlay code
	xor cx,cx				; zero high word of offset
	sub dx,14				; subtract off system info bytes already read
	sbb cx,0				; borrow from high word

	cmp is_emsxms_ovl,0		; see overlay in EMS/XMS
	je  wcd_2a				; no
	call	ems_ovl_seek
	jmp SHORT wcd_2b

wcd_2a:
	mov ax,4201h			; move file pointer, offset from current file position
	int 21h
	call	restore_ems_map

wcd_2b:
	mov dx,ovl_sys_info+4	; get overlay code size in paragraphs, position past it
	xor cx,cx				; zero high word of offset
	shl dx,1
	rcl cx,1				; x2
	shl dx,1
	rcl cx,1				; x4
	shl dx,1
	rcl cx,1				; x8
	shl dx,1
	rcl cx,1				; x16, converted to bytes

	cmp is_emsxms_ovl,0		; see overlay in EMS/XMS
	je  wcd_2c				; no
	call	ems_ovl_seek
	jmp SHORT wcd_2d

wcd_2c:
	mov ax,4201h			; move file pointer, offset from current file position
	int 21h
	call	restore_ems_map

wcd_2d:
	mov ax,ovl_data_id		; get overlay identifier for overlaid code
	mov ovl_data_id,0		; zero out the overlay identifier after use
	and al,1				; see if _DT segment or _DAT segment
	je   wcd_3				; _DT segment, at start of code

; need to position to _DAT segment, past _DT segment
	mov dx,ovl_sys_info+6	; get _DT segment size in paragraphs, position past it
	xor cx,cx				; zero high word of offset
	shl dx,1
	rcl cx,1				; x2
	shl dx,1
	rcl cx,1				; x4
	shl dx,1
	rcl cx,1				; x8
	shl dx,1
	rcl cx,1				; x16, converted to bytes

	cmp is_emsxms_ovl,0		; see overlay in EMS/XMS
	je  wcd_2e				; no
	call	ems_ovl_seek
	jmp SHORT wcd_3

wcd_2e:
	mov ax,4201h			; move file pointer, offset from current file position
	int 21h
	call	restore_ems_map

wcd_3:
	mov dx,WORD PTR rec_offset	; get LEDATA enumerated data offset
	xor cx,cx				; zap high word

	cmp is_emsxms_ovl,0		; see overlay in EMS/XMS
	je  wcd_3a				; no
	call	ems_ovl_seek
	jmp SHORT wcd_3b

wcd_3a:
	mov ax,4201h			; move file pointer, offset from current file position
	int 21h
	call	restore_ems_map

wcd_3b:
	pop cx					; restore number of bytes to write
	mov dx,si				; dx -> write buffer

	mov ax,si				; get offset of source
	add ax,cx				; add in number of bytes to write
	jc  wcd_buff_wrap		; overflow, buffer will wrap
	cmp ax,bp				; see if past buffer end for total bytes written
	jbe wcd_no_buff_wrap	; no

wcd_buff_wrap:
	mov di,cx				; save old byte count to write
	mov cx,bp				; get buffer end
	sub cx,si				; compute bytes to buffer end
	sub di,cx				; update total count, subtracting off byte count written this pass

	call	trans_clar_data	; write clarion data

	mov cx,di				; get new total bytes to write in cx
	xor si,si				; wrap source offset to start of i/o buffer
	mov dx,si				; update start of write buffer

wcd_no_buff_wrap:
	add si,cx				; update si past bytes written

	call	trans_clar_data	; write clarion data
	ret

write_clar_data ENDP

;*****************************
;* TRANS_CLAR_DATA           *
;*****************************

; write clarion data to file,
; if EMS write clarion data to transfer buffer first
; upon entry es:dx -> write buffer, cx holds bytes to write,
; bx holds file handle, if overlay on disk
; destroys ax,bx,cx,dx

trans_clar_data PROC
	cmp is_emsxms_ovl,0		; see overlay in EMS/XMS
	je  tcd_2				; no

tcd_wloop:
	push	cx				; save bytes to write
	cmp cx,16384			; don't write more than 16K each pass
	jbe tcd_write
	mov cx,16384

; move bytes to write to transfer buffer first, then make es:dx -> transfer buffer
tcd_write:
	push	es				; save critical register
	push	si
	push	di
	mov ax,es				; save source segment
	mov es,ems_trans_block
	xor di,di				; es:di -> destination (transfer buffer)
	mov ds,ax
	mov si,dx				; ds:si -> source
	mov ax,cx				; save bytes to write
	shr cx,1				; convert byte count to read to words
	rep movsw				; move the string
	rcl cx,1				; pick up carry
	rep movsb				; transfer leftover byte, if any
	mov cx,ax				; restore bytes to write
	xor dx,dx				; es:dx -> transfer buffer
	mov ax,DGROUP
	mov ds,ax				; ds -> warplink data

	push	es
	pop	ds					; ds:dx  -> write buffer
	call	ems_ovl_write
	jmp SHORT wcd_5

tcd_2:
	push	es
	pop	ds					; ds:dx  -> write buffer
	mov ah,40h				; write to file
	int 21h
	call	restore_ems_map
	jc  tcd_to_doserr		; error occurred
	mov ax,DGROUP
	mov ds,ax				; ds -> warplink data
	ret

wcd_5:
	pop di
	pop si
	pop es
	mov ax,DGROUP
	mov ds,ax				; ds -> warplink data

	pop cx
	sub cx,16384			; subtract off max bytes written
	ja  tcd_wloop			; more bytes to write

tcd_ret:
	ret

tcd_to_doserr:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

trans_clar_data ENDP

;*****************************
;* EMS_OVL_SEEK              *
;*****************************

; seek to overlay page/position in EMS/XMS, relative to curr_ems_pos,
; analogous to seek from current file position, function 4201h
; upon entry cx:dx -> file position relative to curr_ems_pos
; destroys ax,bx,cx,dx

ems_ovl_seek    PROC
	add WORD PTR curr_ems_pos,dx
	adc WORD PTR curr_ems_pos+2,cx	; carry to high word
	ret
ems_ovl_seek    ENDP

;*****************************
;* EMS_OVL_READ              *
;*****************************

; read bytes from overlay in EMS/XMS at curr_ems_pos
; ds:dx -> read buffer, cx holds bytes to read
; do NOT assume ds -> DGROUP upon entry
; destroys ax,bx,cx,dx

ems_ovl_read    PROC
	mov bx,ds				; save -> write buffer
	mov ax,DGROUP
	mov ds,ax				; ds -> warplink data

	cmp ovl_in_xms,0		; see if overlay file in XMS
	je  eor_ems				; no, in EMS
	jmp NEAR PTR xms_ovl_read

eor_ems:
	push	si				; save critical register
	push	di
	push	es
	push	WORD PTR ems_currmap	; save current logical state of physical page 0

	mov	es,bx				; es -> read buffer
	push	bx				; save -> read buffer

	mov di,dx				; es:di -> read buffer
	mov dx,WORD PTR curr_ems_pos+2
	mov ax,WORD PTR curr_ems_pos
	mov si,ax
	and si,16383			; si offsets into EMS page
	call	convert_to_page	; ax holds 16K page of position

	add WORD PTR curr_ems_pos,cx	; adjust for bytes read
	adc WORD PTR curr_ems_pos+2,0	; carry to high word
	add ax,ems_first_ovl	; adjust for first page used

	mov bx,ax				; bx holds EMS logical page
	xor al,al
	call	map_ems_page	; map in proper page

	mov ds,ems_base			; ds:si -> source buffer in EMS

eor_readloop:
	mov ax,si				; check if read from more than one page
	add ax,cx
	jc  eor_offpage			; carry implies read off page
	cmp ax,16384			; check page limit
	ja  eor_offpage			; off of page
	shr cx,1				; convert byte count to read to words
	rep movsw				; move the string
	rcl cx,1				; pick up carry
	rep movsb				; transfer leftover byte, if any
	jmp SHORT eor_2

eor_offpage:
	mov ax,16384
	sub ax,si				; ax holds bytes to read on page
	xchg    ax,cx			; cx holds bytes to read on page, ax holds total bytes
	sub ax,cx				; ax holds bytes to read beside current page
	shr cx,1				; convert byte count to read to words
	rep movsw				; move the string
	rcl cx,1				; pick up carry
	rep movsb				; transfer leftover byte, if any
	mov cx,ax				; update total bytes to read
	xor si,si				; position to start of next page
	inc bx
	xor al,al
	call	map_ems_page	; map in next page
	jmp SHORT eor_readloop	; loop to read more bytes

eor_2:
	pop	ds					; restore ds -> read buffer

; restore previous logical state of physical page 0
	xor al,al
	pop bx
	call	map_ems_page
	pop es					; restore critical register
	pop di
	pop si
	ret
ems_ovl_read    ENDP

;*****************************
;* XMS_OVL_READ              *
;*****************************

; read bytes from overlay in XMS at curr_ems_pos
; bx:dx -> read buffer, cx holds bytes to read
; safe to assume ds -> DGROUP upon entry
; destroys ax,bx,cx,dx

xms_ovl_read    PROC
	push	bx				; save -> read buffer
	push	si				; save critical register
	mov WORD PTR ovl_empb.es_dest_offset+2,bx

	mov ax,xms_ovl_handle
	mov ovl_empb.es_src_handle,ax	; source is XMS memory
	xor ax,ax
	mov WORD PTR ovl_empb.es_len+2,ax	; zero high word of block length
	mov ovl_empb.es_dest_handle,ax	; destination is conventional memory

xor_loop:
	mov ax,cx
	and al,0feh				; round down to word boundary for XMM acceptable length
	mov WORD PTR ovl_empb.es_len,ax	; length of block to transfer low word
	mov ax,WORD PTR curr_ems_pos	; compute source offset
	mov WORD PTR ovl_empb.es_src_offset,ax
	mov ax,WORD PTR curr_ems_pos+2
	mov WORD PTR ovl_empb.es_src_offset+2,ax
	mov WORD PTR ovl_empb.es_dest_offset,dx

	mov ah,0bh				; move extend memory block
	mov si,OFFSET DGROUP:ovl_empb	; ds:si -> parameter block
	call	DWORD PTR xms_addr
	call	check_xms_error	; see if error occurred

	add WORD PTR curr_ems_pos,cx	; adjust for bytes read
	adc WORD PTR curr_ems_pos+2,0	; carry to high word
	add dx,cx				; update destination offset
	and cx,1				; see if extra byte to transfer
	je  xor_ret				; no
	sub WORD PTR curr_ems_pos,2	; back up one byte not written, back up one byte for rewrite
	sbb WORD PTR curr_ems_pos+2,0	; carry to high word
	sub dx,2				; adjust back one byte, back one for byte not written
	inc cx					; two bytes to write
	jmp SHORT xor_loop		; write the bytes

xor_ret:
	pop si					; restore critical register
	pop	ds					; restore ds -> read buffer
	ret
xms_ovl_read    ENDP

;*****************************
;* EMS_OVL_WRITE             *
;*****************************

; write bytes to overlay in EMS at curr_ems_pos
; ds:dx -> write buffer, cx holds bytes to write
; do NOT assume ds -> DGROUP upon entry
; destroys ax,bx,dx

ems_ovl_write   PROC
	push	cx				; save critical register
	push	si
	push	di
	push	es

	mov bx,ds				; save -> write buffer
	mov ax,DGROUP
	mov ds,ax				; ds -> warplink data

	cmp ovl_in_xms,0		; see if overlay file in XMS
	je  eow_ems				; no, in EMS
	call	xms_ovl_write	; write in XMS
	jmp SHORT eow_ret

eow_ems:
	push	WORD PTR ems_currmap	; save current logical state of physical page 0
	push	bx				; save -> write buffer on stack

	mov si,dx				; si -> write buffer
	mov dx,WORD PTR curr_ems_pos+2
	mov ax,WORD PTR curr_ems_pos
	mov di,ax
	and di,16383			; di offsets into EMS page
	call	convert_to_page	; ax holds 16K page of position

	add WORD PTR curr_ems_pos,cx	; adjust for bytes written
	adc WORD PTR curr_ems_pos+2,0	; carry to high word
	add ax,ems_first_ovl	; adjust for first page used

	mov bx,ax				; bx holds EMS logical page
	xor al,al
	call	map_ems_page	; map in proper page

	mov es,ems_base			; es:di -> destination EMS buffer
	pop ds					; ds:si -> write buffer

eow_writeloop:
	mov ax,di				; check if write to more than one page
	add ax,cx
	jc  eow_offpage			; carry implies write off page
	cmp ax,16384			; check page limit
	ja  eow_offpage			; off of page
	shr cx,1				; convert byte count to write to words
	rep movsw				; move the string
	rcl cx,1				; pick up carry
	rep movsb				; transfer leftover byte, if any
	jmp SHORT eow_2

eow_offpage:
	mov ax,16384
	sub ax,di				; ax holds bytes to write on page
	xchg    ax,cx			; cx holds bytes to write on page, ax holds total bytes
	sub ax,cx				; ax holds bytes to write beside current page
	shr cx,1				; convert byte count to write to words
	rep movsw				; move the string
	rcl cx,1				; pick up carry
	rep movsb				; transfer leftover byte, if any
	mov cx,ax				; update total bytes to write
	xor di,di				; position to start of next page
	inc bx
	xor al,al
	call	map_ems_page	; map in next page
	jmp SHORT eow_writeloop	; loop to read more bytes

eow_2:
	pop bx					; bx holds previous logical page of EMS page 0

; restore previous logical state of EMS physical page 0
	xor al,al
	call	map_ems_page

eow_ret:
	pop es					; restore critical register
	pop di
	pop si
	pop	cx
	ret
ems_ovl_write   ENDP

;*****************************
;* XMS_OVL_WRITE             *
;*****************************

; write bytes to overlay in EMS at curr_ems_pos
; bx:dx -> write buffer, cx holds bytes to write
; ds -> DGROUP upon entry
; destroys ax,bx,cx,dx,si

xms_ovl_write   PROC
	cmp cx,1				; see if only one byte to write
	je  xow_1byte			; yes

	xor ax,ax
	mov WORD PTR ovl_empb.es_len+2,ax
	mov ovl_empb.es_src_handle,ax	; source is conventional memory
	mov ax,xms_ovl_handle
	mov ovl_empb.es_dest_handle,ax	; destination is XMS memory

xow_loop:
	mov ax,cx
	and al,0feh				; round down to word boundary for XMM acceptable length
	mov WORD PTR ovl_empb.es_len,ax	; length of block to transfer low word
	mov WORD PTR ovl_empb.es_src_offset,dx
	mov WORD PTR ovl_empb.es_src_offset+2,bx
	mov ax,WORD PTR curr_ems_pos	; compute destination offset
	mov WORD PTR ovl_empb.es_dest_offset,ax
	mov ax,WORD PTR curr_ems_pos+2
	mov WORD PTR ovl_empb.es_dest_offset+2,ax

	mov ah,0bh				; move extended memory block
	mov si,OFFSET DGROUP:ovl_empb	; ds:si -> parameter block
	call	safe_xms_addr
	call	check_xms_error	; see if error occurred

	add WORD PTR curr_ems_pos,cx	; adjust for bytes read
	adc WORD PTR curr_ems_pos+2,0	; carry to high word
	add dx,cx				; update destination offset
	and cx,1				; see if extra byte to transfer
	je  xow_ret				; no
	sub WORD PTR curr_ems_pos,2	; back up one byte not written, back up one byte for rewrite
	sbb WORD PTR curr_ems_pos+2,0	; carry to high word
	sub dx,2				; adjust back one byte, back one for byte not written
	inc cx					; two bytes to write
	jmp SHORT xow_loop		; write the bytes

xow_ret:
	mov ds,bx				; restore ds -> write buffer
	ret

; only one byte to write, can't back up to previous
xow_1byte:
	inc cx					; make 2 bytes for valid length
	push	dx				; save new source pointer offset
	push	bx				; save segment
	mov dx,OFFSET DGROUP:tbuff	; ds:dx -> read buffer
	mov bx,ds				; bx -> write buffer
	call	xms_ovl_read	; get old source byte+following byte in tbuff
	pop ds
	pop bx					; ds:bx -> new source
	push	ds				; save -> original write buffer
	mov al,[bx]				; get new source byte
	mov bx,DGROUP			; bx -> warplink data for xms_ovl_write
	mov ds,bx				; ds -> warplink data
	mov BYTE PTR tbuff,al	; update old source byte to new, following byte stays same
	mov dx,OFFSET DGROUP:tbuff	; ds:dx -> write buffer
	mov cx,2				; two bytes to write
	sub WORD PTR curr_ems_pos,2	; back up two bytes in pseudo-file position
	sbb WORD PTR curr_ems_pos+2,0	; borrow to high word
	call	xms_ovl_write	; write the buffer
	pop ds					; restore ds -> original write buffer
	ret

xms_ovl_write   ENDP

;*****************************
;* EMS_OVL_TO_FILE           *
;*****************************

; flush overlays in EMS to overlay file
; destroys ax,bx,cx,dx,di,si

ems_ovl_to_file PROC

; rewind to start of file
	mov bx,ovl_handle
	xor cx,cx
	mov dx,cx				; zero file offset
	mov ax,4200h			; move file pointer, absolute offset
	int 21h
	call	restore_ems_map

	cmp ovl_in_xms,0		; see if overlay file in XMS
	jne eot_xms				; yes

; read overlay file into EMS
	push	WORD PTR ems_currmap	; save current logical state of physical page 0
	mov di,ems_first_ovl	; di holds first overlay page

eot_writeloop:
	xor al,al
	mov bx,di
	call	map_ems_page

	mov ax,WORD PTR ovl_file_size+2	; get high word of bytes to write
	or  ax,ax				; see if nonzero
	jne eot_2				; nonzero, write 16K block
	mov cx,WORD PTR ovl_file_size	; get low word of bytes to write
	jcxz    eot_ret			; no bytes to write, done
	cmp cx,16384			; only write up to 16K
	jb  eot_3				; below 16K

eot_2:
	mov cx,16384			; write 16K block

eot_3:
	mov bx,ovl_handle
	xor dx,dx
	push	ds				; save critical register
	mov ds,ems_base			; ds:dx -> write buffer, EMS physical page 0
	mov ah,40h				; write file
	int 21h
	call	restore_ems_map
	pop ds					; restore ds -> warplink data
	jc  eot_doserr			; error occurred
	inc di					; bump to next overlay page
	sub WORD PTR ovl_file_size,cx		; subtract off bytes written
	sbb WORD PTR ovl_file_size+2,0	; borrow to high word
	mov ax,WORD PTR ovl_file_size
	or  ax,WORD PTR ovl_file_size+2	; see if written all bytes in overlay file
	jne eot_writeloop		; not done yet

; restore previous logical state of physical page 0
	xor al,al
	pop bx
	call	map_ems_page

eot_ret:
	ret

eot_doserr:
	mov dx,OFFSET DGROUP:ovl_filename	; dx -> name of file with error
	jmp NEAR PTR dos_error	; error opening file

; read overlay file from XMS into transfer area, write to file
eot_xms:
	xor ax,ax
	mov WORD PTR ovl_empb.es_src_offset,ax	; zero init the XMS offset in parameter block
	mov WORD PTR ovl_empb.es_src_offset+2,ax
	mov WORD PTR ovl_empb.es_len+2,ax	; zero high word of block length
	mov ovl_empb.es_dest_handle,ax	; zero handle (conventional memory destination)
	mov WORD PTR ovl_empb.es_dest_offset,ax	; zero offset

	mov ax,xms_ovl_handle
	mov ovl_empb.es_src_handle,ax	; source is XMS
	mov ax,ems_trans_block
	mov WORD PTR ovl_empb.es_dest_offset+2,ax	; destination segment

eot_xloop:
	mov ax,WORD PTR ovl_file_size+2	; get high word of bytes to write
	or  ax,ax				; see if nonzero
	jne eot_4				; nonzero, write 16K block
	mov cx,WORD PTR ovl_file_size	; get low word of bytes to write
	jcxz    eot_ret			; no bytes to write, done
	cmp cx,16384			; only write up to 16K
	jb  eot_5				; below 16K

eot_4:
	mov cx,16384			; write 16K block

eot_5:
	mov ax,cx
	inc ax
	and al,0feh				; round to word boundary for XMM acceptable length
	mov WORD PTR ovl_empb.es_len,ax	; length of block to transfer low word

	mov ah,0bh				; move extend memory block
	mov si,OFFSET DGROUP:ovl_empb	; ds:si -> parameter block
	call	DWORD PTR xms_addr
	call	check_xms_error	; see if error occurred

	mov bx,ovl_handle
	xor dx,dx
	push	ds				; save critical register
	mov ds,ems_trans_block	; ds:dx -> write buffer
	mov ah,40h				; write file
	int 21h
	pop ds					; restore ds -> warplink data
	call	restore_ems_map
	jc  eot_doserr			; error writing file

	add WORD PTR ovl_empb.es_src_offset,cx	; bump dword offset in XMS
	adc WORD PTR ovl_empb.es_src_offset+2,0	; carry to high word
	sub WORD PTR ovl_file_size,cx		; subtract off bytes written
	sbb WORD PTR ovl_file_size+2,0	; borrow to high word
	mov ax,WORD PTR ovl_file_size
	or  ax,WORD PTR ovl_file_size+2	; see if written all bytes in overlay file
	jne eot_xloop			; not done yet

	ret

ems_ovl_to_file ENDP

;*****************************
;* SAFE_XMS_ADDR             *
;*****************************

; save bx and call xms_addr for stupid XMS drivers that eat the bx
; register but shouldn't

safe_xms_addr	PROC
	push	bx
	call	DWORD PTR xms_addr
	pop	bx
	ret
safe_xms_addr	ENDP

END
