;*********************************************************************
;*   CNDDLMG2.ASM                                                    *
;*   By:            Michael Devore                                   *
;*   Date:          04/23/93                                         *
;*   Version:       2.50                                             *
;*   Assembler:     TASM 2.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*   Copyright 1990-93, Michael E. Devore                            *
;*                                                                   *
;*   Clarion DDL manager code (non-overlaid & transient stuff)       *
;*                                                                   *
;*********************************************************************

TITLE   DDLMGR2.ASM
PAGE    50,80

STDOUT  EQU 1
DDL_HEADER_SIZE EQU 128		; size of DDL header
MOD_HEADER_SIZE EQU 64		; size of module header in DDL file (rounded to 16-byte boundary)
BIN_HEADER_SIZE EQU 20		; size of segment binary header
DDL_STACK_SIZE  EQU 320		; size of DDL manager stack

LNAMES_LIST_SIZE    EQU (10*(4+4+128)+16-4)	; allow worst case of 10 entries in lnames logical names block
LNAMES_BLK_SIZE EQU (LNAMES_LIST_SIZE+4)/16	; size of block in paragraphs

; structures
DDL_HEADER_STRUC    STRUC
	dh_sig1 DB  ?			; DDL file signature bytes
	dh_sig2 DB  ?
	dh_sig3 DB  ?
	dh_sig4 DB  ?
	dh_majorver DB  ?		; major version number
	dh_minor1   DB  ?		; minor version number 1
	dh_minor2   DB  ?		; minor version number 2
	dh_minor3   DB  ?		; minor version alpha
	dh_hdrsize  DW  ?		; size of module header
	dh_loadsize DW  ?		; size of DDL loader
	dh_loadstart    DD  ?	; file position of start of loader
	dh_flags    DD  ?		; DDL flags
							; bit 0==main module flag
							; bit 1==required root modules flag
							; bit 2==required overlay modules flag
							; bit 3==elective root modules flag
							; bit 4==elective overlay modules flag
							; bit 5==DOSSEG flag
							; bit 6==contains pre-loading routine
	dh_op       DD  ?		; /op option value
	dh_st       DW  ?		; /st option value
	dh_as       DW  ?		; /as option value
	dh_os       DW  ?		; /os option value
	dh_ol       DW  ?		; /ol option value
	dh_mem      DB  ?		; 0==free mem op, nonzero==alloc mem op
	dh_minop    DB  ?		; 0==normal /op, 1==/op:m
	dh_ox       DB  ?		; 0==regular memory for op, nonzero==EMS page frame for op
	dh_r        DB  ?		; /r option setting
	dh_cla      DB  ?		; /cla option setting
    dh_ou       DB  ?       ; /ou option setting
	dh_ort      DB  ?		; /ort setting
	dh_orp      DB  ?		; /orp setting
	dh_modcount DW  ?		; module count in DDL
	dh_ddlcount DW  ?		; count of DDL's in dependency list (for main module)
	dh_reqroot  DW  ?		; count of required root modules in DDL
	dh_reqovl   DW  ?		; count of required overlay modules in DDL
	dh_elecroot DW  ?		; count of elective root modules in DDL
	dh_elecovl  DW  ?		; count of elective overlay modules in DDL
	dh_ddlstart DD  ?		; file position of start of DDL dependency list
	dh_preload  DD  ?		; file position of pre-load module
	dh_modstart DD  ?		; file position of start of DDL module file position dword entries
	dh_dictstart    DD  ?	; file position of start of DDL dictionary

	dh_ohp3		DB	?		; flag ohp3 use
	dh_ohp_flag	DB	?		; nonzero if /ohp allocate to amount flag set
	dh_oht_flag	DB	?		; nonzero if /oht allocate to amount flag set
	dh_pad		DB	?		; pad value
	dh_ohp		DW	?		; /ohp size in K
	dh_oht		DW	?		; /oht size in K

	dh_reser3   DD  ?		; reserved for future
	dh_reser4   DD  ?		; reserved for future
	dh_oxevar   DB  32 DUP (?)	; specified /ox environment variable
DDL_HEADER_STRUC    ENDS

MOD_HEADER_STRUC    STRUC
	mh_flags    DD  ?		; module flags
							; bit 0==main module flag
							; bit 1==required root module flag
							; bit 2==required overlay module flag
							; bit 3==elective root module flag
							; bit 4==elective overlay module flag
							; bit 6==pre-load module
							; bit 7==contains communal variables (COMDEFs)
	mh_id       DW  ?		; module identifier
	mh_segcount DW  ?		; count of segments in module
	mh_grpcount DW  ?		; count of groups in module
	mh_pubcount DW  ?		; count of publics
	mh_comcount DW  ?		; count of communals
	mh_extcount DW  ?		; count of externals
	mh_lnames   DD  ?		; file position of start of lnames name block
	mh_segdef   DD  ?		; file position of start of segment entries
	mh_grpdef   DD  ?		; file position of start of group entries
	mh_symbols  DD  ?		; file position of start of symbols (pub/ext/comdef) name block
	mh_pubdef   DD  ?		; file position of start of pubdef entries
	mh_comdef   DD  ?		; file position of start of comdef entries
	mh_extdef   DD  ?		; file position of start of extdef entries
	mh_binary   DD  ?		; file position of start of binary data
	mh_startup  DB  7 DUP (?)	; start address if main module in fixup notation
	mh_pad      DB  ?		; pad to keep at dword boundary
	mh_binfpos  DD  ?		; file position of each binary entry file position table
	mh_reser2   DD  ?		; reserved for future
MOD_HEADER_STRUC    ENDS

BIN_HEADER_STRUC    STRUC
	bhs_flags   DD  ?		; flags
							; bit 0==overlay class flag
	bhs_segind  DW  ?		; segment index
	bhs_length  DW  ?		; length of segment binary data (not necessarily segment length)
	bhs_fixptr  DD  ?		; file position of start of fixups for segment (0 if none)
	bhs_nextptr DD  ?		; file position of next binary block (0 if last)
	bhs_offset  DW  ?		; start offset within segment
	bhs_truelen DW  ?		; true length of segment
BIN_HEADER_STRUC    ENDS

FIXUP_HEADER_STRUC  STRUC
	fhs_lowcount    DW  ?	; count of low-order byte fixups
	fhs_lowinfo     DD  ?	; file position of low-order byte fixup info entries
	fhs_lowloc      DD  ?	; file position of low-order byte fixup locations
	fhs_nearcount   DW  ?	; count of near fixups
	fhs_nearinfo    DD  ?	; file position of near fixup information entries
	fhs_nearloc     DD  ?	; file position of near fixup locations
	fhs_farcount    DW  ?	; count of far fixups
	fhs_farinfo     DD  ?	; file position of far fixup information entries
	fhs_farloc      DD  ?	; file position of far fixup locations
FIXUP_HEADER_STRUC  ENDS

XMSBUFF_STRUC	STRUC
	xs_len  DD  ?			; length of block in bytes
	xs_src_handle   DW  ?	; source xms buffer handle
	xs_src_offset   DD  ?	; source offset
	xs_dest_handle  DW  ?	; destination xms buffer handle
	xs_dest_offset  DD  ?	; destination offset
XMSBUFF_STRUC	ENDS

EMSBUFF_STRUC STRUC
	es_len  DD  ?			; length of block in bytes
	es_stype    DB  ?		; source type (0==conventional, 1==expanded)
	es_shandle  DW  ?		; source handle
	es_soffset  DW  ?		; source offset
	es_ssegpage DW  ?		; source memory segment or logical page number
	es_dtype    DB  ?		; destination type (0==conventional, 1==expanded)
	es_dhandle  DW  ?		; destination handle
	es_doffset  DW  ?		; destination offset
	es_dsegpage DW  ?		; destination memory segment or logical page number
EMSBUFF_STRUC ENDS

; publics
PUBLIC	start_here

; externals
EXTRN	xbuff:XMSBUFF_STRUC	; XMS buffer for stashing built image
EXTRN	ebuff:EMSBUFF_STRUC	; EMS buffer for stashing built image

EXTRN   binheader_seg:WORD,ddl_mainhdr_seg:WORD
EXTRN   segment_start:DWORD
EXTRN   ovl_seg_count:WORD
EXTRN   typeflag:BYTE
EXTRN   is_absseg:BYTE,is_abspub:BYTE,is_resolved:BYTE,fixdat:BYTE
EXTRN   target_index:WORD,target_segment:WORD,frame_index:WORD
EXTRN   target_prog_off:DWORD,frame_offset:DWORD,lseg_canon:DWORD
EXTRN   frame_method:BYTE,target_method:BYTE

EXTRN	free_640k_alloc:NEAR,xms_check_setup:NEAR,lim_ems40_check:NEAR
EXTRN	setup_and_go:NEAR,ovlmgr_vector:NEAR
EXTRN	cut_here:NEAR,startup:NEAR

EXTRN   prog_seg:WORD,top_of_mem:WORD,prog_psp:WORD
EXTRN   start_offset:WORD,start_segment:WORD
EXTRN	largest_ovl:WORD,second_ovl:WORD
EXTRN	ovl_usedpub_tbl_seg:WORD,ovlpub_tbl_seg:WORD
EXTRN	ovl_usedseg_tbl_seg:WORD,ovl_usedcan_tbl_seg:WORD,ovl_usedgrp_tbl_seg:WORD
EXTRN	ovl_stash_tbl_seg:WORD,ovl_correl_tbl_seg:WORD,ovl_filepos_tbl_seg:WORD
EXTRN	ovl_handle_tbl_seg:WORD
EXTRN   stack_offval:WORD,stack_segval:WORD
EXTRN   call_tbl_offset:DWORD,ovl_call_tbl_seg:WORD
EXTRN	xms_addr:DWORD
EXTRN	ems_flag:BYTE,xms_flag:BYTE
EXTRN	emsxms_handle:WORD
EXTRN	image_written:DWORD
EXTRN	ovl_alloc_end:WORD,ovl_alloc_start:WORD
EXTRN	overlay_id:WORD
EXTRN	ovl_call_tbl_size:WORD
EXTRN	_cla_xqu_offset:WORD,_cla_xqu_segment:WORD
EXTRN	runit_offset:WORD,runit_segment:WORD
EXTRN	beforerun_offset:WORD,beforerun_segment:WORD
EXTRN	afterrun_offset:WORD,afterrun_segment:WORD
EXTRN	_cla_restart_offset:WORD,_cla_restart_segment:WORD

; upon entry DDL file is open and bx holds the file handle
cseg    segment byte public 'CODE'
	assume  cs:cseg,ds:NOTHING

start_here:
	mov ax,es				; get PSP segment
	push	cs
	pop	ds					; ds -> code segment data
	mov ds:prog_psp,ax		; save it
	add ax,10h				; compute program load address
	mov ds:prog_seg,ax		; save it
	mov ds:start_segment,ax	; default start segment
	mov ds:load_base,ax		; load temporary stuff in low memory
	mov ds:lowalloc_base,ax	; save as low allocation base
	mov di,ax
	mov ax,es:[2]			; get top of available memory
	mov ds:top_of_mem,ax	; save it
	mov cx,OFFSET endcode	; get end of manager code
	add cx,15				; round to next paragraph boundary
	shr cx,1				; convert to paragraph, /2
	shr cx,1				; /4
	shr cx,1				; /8
	shr cx,1				; /16
	sub ax,cx
	cmp ax,di
	jae mgr_2				; above program load address

to_mem_error:
	jmp NEAR PTR mem_error	; not enough memory

mgr_2:
	sub ax,DDL_STACK_SIZE/16	; get room for DDL manager stack
	cmp ax,di
	jb  to_mem_error		; not enough room for stack

	cli
	mov ss,ax				; set up DDL manager stack
	mov sp,DDL_STACK_SIZE-2
	sti

; allocate room for module binary header
	sub ax,2
	jc  to_mem_error		; not enough room for header
	cmp ax,di
	jb  to_mem_error
	mov ds:binheader_seg,ax	; save segment of module binary header

	sub ax,DDL_HEADER_SIZE/16	; allocate buffer for DDL header
	jc  to_mem_error		; not enough room for header
	cmp ax,di
	jb  to_mem_error

	mov ds:ddl_mainhdr_seg,ax	; save segment of DDL header info for main DDL
	mov	ds:header_array,ax	; save in array
	mov ddl_transhdr_seg,ax	; save segment of transient DDL header

	mov ds:depend_seg,ax	; save as segment dependency list in case null list
	mov es:[2],ax			; save PSP top of memory pointer to below permanent DDL code and data
	push	ax				; -> DDL header buffer for later ds storage
	push	bx				; save file handle

; must use DOS 3.0 or above
	mov	ax,3000h
	int	21h
	cmp	al,3				; check major version
	jae	init_allocems		; okay, DOS 3.0+
	jmp	NEAR PTR dos3_error	; 2.x

; must allocate 640K XMS or EMS 4.0 for load image stashing
init_allocems:
	push	es				; save critical segment register values
	call	alloc_640k_xms	; try to allocate 640K XMS
	jc	mgr_noxms
	mov	xms_flag,1			; flag stash to XMS
	jmp	SHORT mgr_allocdone

; no EMS or XMS, fail
mgr_failalloc:
	jmp	NEAR PTR noemsxms_error

; no EMS 4.0, try XMS
mgr_noxms:
	call	alloc_640k_ems	; try to allocate 640K EMS
	jc	mgr_failalloc
	mov	ds:ems_flag,1		; flag stash to EMS

mgr_allocdone:
	pop	es
	pop	bx					; restore bx == DDL file handle
	pop	ds					; ds -> DDL header buffer

; note DS no longer points to ddl manager data, use cs: override to access it
	xor cx,cx
	mov dx,cx				; rewind file
	mov ax,4200h			; move file pointer, absolute position
	int 21h

; read main DDL header
	xor dx,dx
	mov cx,DDL_HEADER_SIZE	; bytes to read
	mov ah,3fh				; read from file
	int 21h
	jnc mgr_4				; no errors

to_access_error:
	jmp NEAR PTR access_error

; check if dependency list, if so allocate space
mgr_4:
	mov dx,ds:[dh_ddlcount]	; see if any DDL's in dependency list
	or  dx,dx
	je  mgr_5				; null dependency list

	mov	cx,dx
	shl dx,1				; x2
	shl dx,1				; x4
	add	dx,cx				; x5 (5 paragraphs/name, 80 bytes)
	mov ax,es:[2]			; get PSP top of memory pointer
	sub ax,dx				; allocate room for dependency list
	jc  to_memerr2			; not enough room for list
	cmp ax,di
	jae	mgr_depend

to_memerr2:
	jmp NEAR PTR mem_error	; not enough memory

mgr_depend:
	mov cs:depend_seg,ax	; save segment of dependency list

; seek to dependency list
	mov dx,WORD PTR ds:[dh_ddlstart]	; get low word of dependency list
	mov cx,WORD PTR ds:[dh_ddlstart+2]	; get high word of dependency list
	mov ax,4200h			; move file pointer, absolute position
	int 21h

; read dependency list
	mov ax,ds:[dh_ddlcount]	; get count of DDL's in list
	mov cs:ddl_depend,ax	; save running count of dependent DDL's
	mov	cs:ddl_totdepend,ax	; save fixed count
	mov cl,80				; 80 bytes/entry
	mul cl					; get byte count of list
	mov cx,ax
	mov ds,cs:depend_seg	; ds -> dependency list
	xor dx,dx
	mov ah,3fh				; read from file
	int 21h
	jc  to_access_error

mgr_5:
	mov cs:handle_array,bx	; save handle of main DDL file
	mov ax,cs:depend_seg	; get last used memory block

; allocate room for DDL module header
; ax holds start of last allocated block
	sub ax,(MOD_HEADER_SIZE+15)/16	; allocate room for module header
	jc  to_memerr2			; not enough room for header
	cmp ax,di
	jb  to_memerr2			; no enough room for header
	mov cs:modhdr_seg,ax	; save segment of module header

; allocate room for temporary external list
; ax holds start of last allocated block
	sub ax,80h				; allocate room for maximum externals list
	jc  to_memerr2			; not enough room for header
	cmp ax,di
	jb  to_memerr2			; no enough room for header
	mov cs:extlist_seg,ax	; save segment of temporary external list storage

	mov cs:hialloc_base,ax	; save base of semi-permanent high memory allocations
	mov	ds,cs:ddl_transhdr_seg	; ds -> DDL header
	jmp	SHORT mgr_setvar	; just past header re-read

; allocate space for transient DDL header
mgr_gettrans:
	mov ax,cs:lowalloc_base	; allocate in low memory
	mov si,cs:ddl_counter	; get count of current DDL
	shl si,1				; convert to word offset
	mov cs:[si+OFFSET header_array],ax	; save DDL header pointer
	mov	cs:ddl_transhdr_seg,ax	; save current segment
	mov ds,ax				; ds -> DDL header position load area
	add ax,DDL_HEADER_SIZE/16
	mov cs:lowalloc_base,ax	; update low memory pointer

; read header of transient DDL file into storage
	xor dx,dx
	mov cx,DDL_HEADER_SIZE	; bytes to read
	mov ah,3fh				; read from file
	int 21h
	jc  to_doserr2

mgr_setvar:
	mov ax,ds:[dh_reqroot]
	add ax,ds:[dh_reqovl]	; ax holds total count of required modules
	mov cs:required_count,ax	; save to variable
	mov al,BYTE PTR ds:[dh_flags]
	and al,20h				; get DOSSEG status
	or  cs:dosseg_flag,al	; merge into previous DOSSEG status

; seek to DDL module file positions
	mov dx,WORD PTR ds:[dh_modstart]	; get low word of module file positions
	mov cx,WORD PTR ds:[dh_modstart+2]	; get high word of module file positions
	mov ax,4200h			; move file pointer, absolute position
	int 21h

; allocate space for module file positions in low memory, save pointer
	mov ax,ds:[dh_modcount]
	mov	dx,ax				; save count
	add ax,3				; round up to next para
	shr	ax,1
	shr	ax,1				; ax == paras to allocation for dword file positions
	mov cx,ax
	mov ax,cs:lowalloc_base	; allocate in low memory
	mov si,cs:ddl_counter	; get count of current DDL
	shl si,1				; convert to word offset
	mov cs:[si+OFFSET fpos_array],ax	; save file position table pointer
	mov	cs:filepos_seg,ax	; save current file position segment
	mov ds,ax				; ds -> file position load area
	add ax,cx
	mov cs:lowalloc_base,ax	; update low memory pointer

; read DDL module file positions
	mov cx,dx				; get count of modules in cx
	shl cx,1				; change to dword count/entry, /2
	shl cx,1				; /4
	xor dx,dx
	mov ah,3fh				; read from file
	int 21h
	jnc mgr_setelect

to_doserr2:
	jmp NEAR PTR access_error

; set elective count variable
mgr_setelect:
	mov ds,cs:ddl_transhdr_seg	; ds -> transient DDL header storage area
	mov ax,ds:[dh_elecroot]
	or	ax,ds:[dh_elecovl]	; see if any elective modules
	jne dict_seek			; elective modules exist
	jmp NEAR PTR p1_procmods	; no electives

; seek to dictionary location
dict_seek:
	mov dx,WORD PTR ds:[dh_dictstart]	; get dictionary position
	mov cx,WORD PTR ds:[dh_dictstart+2]
	mov ax,4200h			; move file pointer, absolute position
	int 21h

; allocate space for dictionary index in low memory, save pointer
	mov ax,ds:[dh_modcount]
	mov dx,ax				; save module count for later use
	mov di,ax
	add ax,15				; round up to next para
	mov cl,3
	shr ax,cl				; ax == paras for dictionary index
	mov cx,ax
	mov ax,cs:lowalloc_base	; allocate in low memory
	mov si,cs:ddl_counter	; get count of current DDL
	shl si,1				; convert to word offset
	mov cs:[si+OFFSET index_array],ax	; save index pointer
	mov ds,ax				; ds -> index load area
	add ax,cx
	mov cs:lowalloc_base,ax	; update low memory pointer

; read dictionary index
	mov cx,dx				; get module count
	add cx,cx				; double it
	xor dx,dx
	mov ah,3fh				; read from file
	int 21h

to_to_doserr2:
	jc  to_doserr2

; find space to allocate for dictionary
	dec di					; get module count relative 0
	add di,di				; modcount*2

dict_loop:
	mov ax,ds:[di]			; get final value in index array
	or  ax,ax				; make sure nonzero
	jne dict_alloc
	sub di,2				; backup one entry until find valid
	jmp SHORT dict_loop

; allocate room for dictionary, ax holds paras to allocate
dict_alloc:
	mov cx,ax
	mov ax,cs:lowalloc_base	; allocate in low memory
	mov cs:[si+OFFSET dict_array],ax	; save dictionary pointer
	mov ds,ax				; ds -> dictionary load area
	add ax,cx
	mov cs:lowalloc_base,ax	; update low memory pointer

; read dictionary, cx holds paras to read
	shl cx,1
	shl cx,1
	shl cx,1
	shl cx,1				; x16
	xor dx,dx
	mov ah,3fh				; read from file
	int 21h
	jc  to_to_doserr2

p1_procmods:
	mov ds,cs:ddl_transhdr_seg	; ds -> transient DDL header
	mov ax,ds:[dh_elecroot]
	add ax,ds:[dh_elecovl]
	mov cs:elective_count,ax	; save count of elective modules
	xor ax,ax
	mov cs:current_mod,ax	; init current module
	mov cs:mods_added,ax	; init modules from DDL added
	mov cs:opt_flag,al		; init no optional modules used

p1_reqloop:
	cmp cs:required_count,0	; see if any required modules left to be read
	je  p1_chkopt			; no, check out optional modules, if any
	cmp	cs:multiple_pass_flag,0	; see if multiple passes
	jne	p1_chkopt			; yes, don't relink required modules

	dec cs:required_count	; drop count of required modules
	inc cs:current_mod		; bump current module

	call    add_module		; add the module
	jmp SHORT p1_reqloop	; loop until all required modules added

p1_chkopt:
	cmp cs:elective_count,0	; see if any optional modules to read
	je  mgr_nextopen		; nope

; process optional modules
p1_optional:
	mov ax,WORD PTR cs:total_sym_count	; get total symbol table count low word
	cmp ax,WORD PTR cs:res_sym_count	; see if matches resolved
	jne p1_tryopt			; no, try another optional module, if any
	mov ax,WORD PTR cs:total_sym_count+2	; check high word
	cmp ax,WORD PTR cs:res_sym_count+2
	je  mgr_nextopen		; match, try next DDL file, if any

p1_tryopt:
	mov ax,cs:mods_added	; get modules added from DDL
	mov ds,cs:ddl_transhdr_seg	; ds -> transient DDL header
	cmp ax,ds:[dh_modcount]	; see if matches total modules in DDL
	je  mgr_nextopen		; yes, no more passes to perform

p1_optloop:
	mov ds,cs:ddl_transhdr_seg	; ds -> transient DDL header
	mov ax,ds:[dh_modcount]
	cmp cs:current_mod,ax	; see if current module is beyond total module count
	jb  p1_bumpmod			; no
	xor ax,ax
	cmp cs:opt_flag,al		; see if any modules found this pass
	je  mgr_nextopen		; no
	mov cs:current_mod,ax	; reset current module
	mov cs:opt_flag,al		; reset optional modules found this pass flag
	jmp SHORT p1_tryopt		; reloop through optional modules

p1_bumpmod:
	inc cs:current_mod		; bump current module
	call    check_module	; check module for processing, process if needed
	or  al,al				; al nonzero if module was processed
	je  p1_optloop			; not processed, loop for another optional module
	mov cs:opt_flag,1		; set optional module found this pass flag
	jmp SHORT p1_optional	; module was processed, check symbols

mgr_nextopen:
	mov ax,cs:ddl_depend	; get count of DDL dependents
	or  ax,ax				; see if any left
	je  mgr_chkres			; no, see if should move to resolution phase
	dec ax
	mov cs:ddl_depend,ax	; update DDL dependents
	cmp	cs:multiple_pass_flag,0	; see if multiple passes
	je	mgr_nomult			; no

; multiple passes, get next DDL info already saved
	push	cs
	pop	ds
	inc ds:ddl_counter		; bump count of current DDL

mgr_multsetup:
	mov si,ds:ddl_counter	; get count of current DDL
	shl si,1				; convert to word offset
	mov bx,ds:[si+OFFSET handle_array]	; get file handle in bx
	mov	ax,ds:[si+OFFSET fpos_array]
	mov	ds:filepos_seg,ax
	mov	ax,ds:[si+OFFSET header_array]
	mov	ds:ddl_transhdr_seg,ax
	jmp NEAR PTR p1_procmods	; parse next DDL file

mgr_nomult:
	call    open_ddl_file	; open the next transient ddl file
	inc cs:ddl_counter		; bump count of current DDL
	mov si,cs:ddl_counter	; get count of current DDL
	shl si,1				; convert to word offset
	mov cs:[si+OFFSET handle_array],bx	; save file handle
	jmp NEAR PTR mgr_gettrans	; parse next DDL file

; see if there are new unresolved symbols, reloop if so
mgr_chkres:
	push	cs
	pop	ds					; ds -> code segment data
	mov	ds:is_unresolved,0	; init unresolveds flag
	mov ax,WORD PTR ds:total_sym_count+2	; check high word of total vs. resolved symbols
	cmp ax,WORD PTR ds:res_sym_count+2
	jne	mgr_checkold		; at least 1 unresolved
	mov ax,WORD PTR ds:total_sym_count	; check low word of total vs. resolved symbols
	cmp ax,WORD PTR ds:res_sym_count
	jne	mgr_checkold		; unresolveds exist
	jmp	NEAR PTR mgr_resolve	; no unresolved symbols

; unresolved symbols remain, see if another pass is called for, check DOSSEG externals first
mgr_checkold:
	cmp ds:dosseg_flag,0		; see if DOSSEG flag set
	je	mgr_nodosseg		; no
	mov si,WORD PTR ds:total_sym_count
	sub si,WORD PTR ds:res_sym_count
	mov ax,WORD PTR ds:total_sym_count+2
	sbb ax,WORD PTR ds:res_sym_count+2	; if DOSSEG externals then si<=2 && ax==0
	jne	mgr_nodosseg		; more than 2 externals
	cmp	si,2
	ja	mgr_nodosseg		; more than 2 externals

; one or two externals, see if DOSSEG externals
	push    cs
	pop es
	mov ds:search_flag,1	; set flag to indicate one symbol search for exist
	mov di,OFFSET _edatahigh	; es:di -> _EDATA symbol name
	call    ap_altent		; check if exists
	or  al,al				; see if found
	jne mgr_isedata			; yes
	mov di,OFFSET _edatalow	; es:di -> _edata symbol name
	call    ap_altent		; check if exists
	or  al,al				; see if found
	je  mgr_noedata			; no, check for _end

mgr_isedata:
	mov es,bx				; es -> public entry
	mov al,es:[14]			; get definition flag
	and al,3
	cmp al,2				; see if public or absolute
	jae mgr_noedata			; yes, don't count as external
	dec	si					; drop count of externals
	je	mgr_resolve			; that was only the only external

mgr_noedata:
	cmp	si,1				; must only be one unresolved external
	jne	mgr_nodosseg

	push    cs
	pop es
	mov di,OFFSET _endhigh	; es:di -> _END symbol name
	call    ap_altent		; check if exists
	or  al,al				; see if found
	jne mgr_isend			; yes
	mov di,OFFSET _endlow	; es:di -> _end symbol name
	call    ap_altent		; check if exists
	or  al,al				; see if found
	je  mgr_nodosseg		; no, other unresolved externals to blame

mgr_isend:
	mov es,bx				; es -> public entry
	mov al,es:[14]			; get definition flag
	and al,3
	cmp al,2				; see if public or absolute
	jb	mgr_resolve			; no, this is the unresolved external

; unresolved externals unrelated to DOSSEG exist
mgr_nodosseg:
	push    cs
	pop ds
	mov	ds:is_unresolved,1	; flag unresolveds exist
	mov ax,WORD PTR ds:total_sym_count+2	; check high word of total vs. resolved symbols
	cmp ax,WORD PTR ds:old_sym_count+2
	jne	mgr_neednew			; at least 1 new symbol
	mov ax,WORD PTR ds:total_sym_count	; check low word of total vs. resolved symbols
	cmp ax,WORD PTR ds:old_sym_count
	je	mgr_resolve			; no new symbols

mgr_neednew:
	mov	ax,WORD PTR ds:total_sym_count+2	; update old total symbols
	mov	WORD PTR ds:old_sym_count+2,ax
	mov	ax,WORD PTR ds:total_sym_count
	mov	WORD PTR ds:old_sym_count,ax
	mov	ax,ds:ddl_totdepend	; reset dependency count
	mov	ds:ddl_depend,ax
	mov	ds:ddl_counter,0	; reset DDL counter
	mov	ds:multiple_pass_flag,1	; flag multiple DDL passes (2nd through nth)
	jmp	NEAR PTR mgr_multsetup

; start of resolution phase after pass 1, before pass 2
mgr_resolve:
	cmp cs:total_seg_count,0	; see if any segments
	jne chk_dosseg			; yes
	jmp NEAR PTR p2_closeall	; no, close files and terminate

; see if DOSSEG flag set
chk_dosseg:
	cmp cs:dosseg_flag,0	; see if DOSSEG flag set
	je  chk_communal		; no

	call    make_dummy_text	; make dummy _TEXT segment if any _TEXT segments exist

; keep pointers to _edata and _end if they were externally declared
	push    cs
	pop es
	mov cs:search_flag,1	; set flag to indicate one symbol search for exist
	mov di,OFFSET _edatahigh	; es:di -> _EDATA symbol name
	call    ap_altent		; check if exists
	or  al,al				; see if found
	jne edata_found			; yes
	mov di,OFFSET _edatalow	; es:di -> _edata symbol name
	call    ap_altent		; check if exists
	or  al,al				; see if found
	je  chk_end				; no, check for _end

edata_found:
	mov ds,bx				; ds -> public entry
	mov al,ds:[14]			; get definition flag
	and al,3
	cmp al,2				; see if public or absolute
	jae chk_end				; yes, don't modify to value
	mov cs:_edata_ptr,bx	; save pointer to symbol entry

chk_end:
	mov di,OFFSET _endhigh	; es:di -> _END symbol name
	call    ap_altent		; check if exists
	or  al,al				; see if found
	jne end_found			; yes
	mov di,OFFSET _endlow	; es:di -> _end symbol name
	call    ap_altent		; check if exists
	or  al,al				; see if found
	je  chk_communal		; no, check for unresolved communals

end_found:
	mov ds,bx				; ds -> public entry
	mov al,ds:[14]			; get definition flag
	and al,3
	cmp al,2				; see if public or absolute
	jae chk_communal		; yes, don't modify to value
	mov cs:_end_ptr,bx		; save pointer to symbol entry

; see if any communal variables (bit 40h of general flag set, bit 0-1 of definition flag<2
chk_communal:
	call    resolve_communals
	cmp	cs:is_unresolved,0		; see if unresolved symbols
	je	mgr_doresolve		; no
	jmp	NEAR PTR unrez_error	; yes, abort with error

mgr_doresolve:
	call    resolve_info	; resolve segment order, update lengths, update
							; extern lists to -> public entries, resolve groups
	cmp cs:ovl_seg_count,0	; see if using overlays
	je  p2start				; no

; keep segment:offset of _cla_xqu,runit,beforerun,afterrun,_cla_restart
clar_vars:
    push    cs
    pop es
    mov cs:search_flag,1    ; set flag to indicate one symbol search for exist
    mov di,OFFSET _cla_xqu_text	; es:di -> _CLA_XQU symbol name
    call    ap_altent       ; check if exists
    call    get_cla_seg_off
    mov cs:_cla_xqu_offset,di
    mov cs:_cla_xqu_segment,ax

    mov di,OFFSET runit_text	; es:di -> RUNIT symbol name
    call    ap_altent       ; check if exists
    call    get_cla_seg_off
    mov cs:runit_offset,di
    mov cs:runit_segment,ax

    mov di,OFFSET beforerun_text	; es:di -> BEFORERUN symbol name
    call    ap_altent       ; check if exists
    call    get_cla_seg_off
    mov cs:beforerun_offset,di
    mov cs:beforerun_segment,ax

    mov di,OFFSET afterrun_text	; es:di -> AFTERRUN symbol name
    call    ap_altent       ; check if exists
    call    get_cla_seg_off
    mov cs:afterrun_offset,di
    mov cs:afterrun_segment,ax

    mov di,OFFSET _cla_restart_text	; es:di -> _CLA_RESTART symbol name
    call    ap_altent       ; check if exists
    call    get_cla_seg_off
    mov cs:_cla_restart_offset,di
    mov cs:_cla_restart_segment,ax

	call    get_ovl_seg_id	; assign overlay ids to overlaid segments
	call    ovl_setup		; setup for overlays

; start of pass 2
p2start:
	mov ax,cs:prog_seg
	mov cs:load_base,ax		; re-init load base to start of program for segment+fixup loads
	mov bp,1				; init current segment to load count

; look up proper segdef entry for segment
mainseg_loop:
	mov ds,cs:segdef_seg

chksegblk_loop:
	mov cx,ds:[2]			; get count of entries in block
	cmp	WORD PTR ds:[12],0	; see if all loaded
	je	chkseg_next			; yes
	mov dx,ds				; save -> segdef block

; preset ds 1 para below start of block so double increment will -> first entry
	mov ax,ds
	dec ax
	mov ds,ax

chksegent_loop:
	mov ax,ds				; get old entry pointer
	inc ax
	inc ax
	mov ds,ax				; ds -> next entry in block

	cmp ds:[24],bp			; see if matches segment to load count
	je  segmatch			; yes
	loop    chksegent_loop	; loop through all entries in segdef block
	mov ds,dx				; restore ds -> segdef block

chkseg_next:
	mov ds,ds:[0]			; ds -> next block
	jmp SHORT chksegblk_loop

to_doserr3:
	jmp NEAR PTR access_error  

; get file handle and file position of segment to load, seek to it
segmatch:
	mov cs:curr_segblk,dx	; save -> owning segdef block
	push	ds
	mov	ds,dx				; ds -> segdef block
	dec	WORD PTR ds:[12]	; drop count of unloaded in block
	pop	ds
	cmp WORD PTR ds:[18],-1	; see if module id is -1 (DDLMGR supplied segment)
	je  to_nextseg			; yes, ignore it

	mov al,ds:[28]
	and al,14h				; see if segment is overlaid (no loads or fixups now)
	jne to_nextseg			; yes

	mov bl,ds:[29]			; get DDL library number
	xor bh,bh
	mov cs:ddl_id,bx		; save DDL identifier
	shl bx,1				; convert to word offset
	mov bx,cs:[bx+OFFSET handle_array]
	mov dx,ds:[20]			; get file position low word
	mov WORD PTR cs:seg_filepos,dx
	mov cx,ds:[22]			; get file position high word
	mov WORD PTR cs:seg_filepos+2,cx
	mov ax,cx
	or  ax,dx				; see if zero (no binary data for segment)
	jne seek_to_bin			; no, binary data exists

to_nextseg:
	jmp NEAR PTR nextseg	; zero, no binary data

seek_to_bin:
	mov ax,4200h			; seek to absolute position in file
	int 21h

; load the binary header
	mov ax,ds
	mov es,ax				; es -> segdef entry
	mov ds,cs:binheader_seg
	xor dx,dx
	mov	cs:common_seg_flag,dl	; init common segment variable
	mov	cs:excess_bytes,dx	; init count of bytes written beyond last segment
	mov cx,BIN_HEADER_SIZE
	mov ah,3fh
	int 21h
	jc  to_doserr3			; error occurred

; load the initialized segment image and fixups following (if any)
	mov ax,es:[18]			; save module identifier
	mov cs:module_id,ax

; compare offset to load with running total of image written
; pad with zero to offset if necessary
	mov cx,es:[2]			; get segment offset in ax:cx
	mov ax,es:[4]
	cmp ax,WORD PTR cs:image_written+2	; see if > last byte written in image
	jb  update_image		; no
	ja  zero_pad			; yes

; high words are equal
	cmp cx,WORD PTR cs:image_written	; see if > last byte written in image
	jbe update_image		; no

zero_pad:
	call    zero_mem_block

	mov cx,es:[2]			; get segment offset in ax:cx
	mov ax,es:[4]
;***	mov	dx,cx
;***	mov	si,ax
;***	sub	dx,1				; highest address just below segment start
;***	sbb	si,0
;***	mov WORD PTR cs:image_written,dx	; update highest address binary image written
;***	mov WORD PTR cs:image_written+2,si
	mov WORD PTR cs:image_written,cx	; update highest address binary image written
	mov WORD PTR cs:image_written+2,ax

update_image:
    mov dl,es:[26]          ; get acbp byte
    and dl,1ch              ; get combine field
    cmp dl,18h              ; see if common combine
	jne	ui_2
	mov	cs:common_seg_flag,dl	; flag common segment

ui_2:
	mov si,cx
	mov dx,ax				; segment offset in dx:si
	add si,ds:[bhs_length]
	adc dx,0				; dx:si hold last byte to be written

	cmp dx,WORD PTR cs:image_written+2	; see if exceeds high written so far
	jb  compute_load		; no, less than
	ja  do_upd_image

; high words of program length match
	cmp si,WORD PTR cs:image_written	; check low word
	jbe	compute_load		; same high address

do_upd_image:
	mov	cs:excess_bytes,cx	; compute bytes written beyond last
	mov	si,WORD PTR cs:image_written
	sub si,ds:[bhs_length]	; adjust for size of segment image to be written
	sub	cs:excess_bytes,si
	mov WORD PTR cs:image_written,cx	; save highest address binary image written
	mov WORD PTR cs:image_written+2,ax
	mov si,ds:[bhs_length]	; add in size of segment image to be written
	add WORD PTR cs:image_written,si
	adc WORD PTR cs:image_written+2,0
	cmp	WORD PTR cs:image_written+2,0ah	; see if overflow of program beyond 640K
	jb	compute_load		; no
	jmp NEAR PTR mem_error  ; not enough memory

compute_load:
	mov dx,cx				; save low word
	shr ax,1
	rcr cx,1				; /2
	shr ax,1
	rcr cx,1				; /4
	shr ax,1
	rcr cx,1				; /8
	shr ax,1
	rcr cx,1				; /16
	mov ax,cs:prog_seg		; ax -> load area (start of program)
	add ax,cx				; ax -> location within load area
	and dx,0fh				; get offset
	mov cs:load_base,ax		; save updated load area
	mov cs:load_offset,dx

	xor	si,si				; init bytes to load
	mov	di,si

; get size of fixup block (bhs_nextptr-bhs_fixptr)
	mov	ax,WORD PTR ds:[bhs_fixptr]
	or	ax,WORD PTR ds:[bhs_fixptr+2]
	je	seg_img				; no fixups, add in segment image
    mov si,WORD PTR ds:[bhs_nextptr]
    mov di,WORD PTR ds:[bhs_nextptr+2]
    sub si,WORD PTR ds:[bhs_fixptr]
    sbb di,WORD PTR ds:[bhs_fixptr+2]

seg_img:
	add si,ds:[bhs_length]	; add in size of segment image
	adc di,0				; di:si == byte count to load
	mov ds,cs:prog_seg		; ds:dx -> load area
	call    read_file
	mov ds,cs:binheader_seg
	mov ax,WORD PTR ds:[bhs_fixptr]
	or  ax,WORD PTR ds:[bhs_fixptr+2]
	je  seg_to_emsxms		; no fixups

	call    do_fixups		; perform fixups

; copy load segment to EMS/XMS
; [bhs_length] holds segment length (bytes to copy)
; [cs:prog_seg]:load_offset -> source
; (load_base-prog_seg)*16+load_offset -> destination offset
seg_to_emsxms:
	mov ds,cs:binheader_seg
	mov	ax,ds:[bhs_length]	; ax holds segment length
	push	cs
	pop	ds					; ds -> code segment
	xor	dx,dx
	mov	cx,ds:load_base
	sub	cx,ds:prog_seg
	shl	cx,1
    rcl dx,1                ; x2
	shl	cx,1
    rcl dx,1                ; x4
    shl cx,1
    rcl dx,1                ; x8
    shl cx,1
    rcl dx,1                ; x16, paragraph converted to bytes in dx:cx
	add	cx,ds:load_offset
	adc	dx,0				; dx:cx holds byte offset

	call	common_seg_check	; check and adjust for common segment

	or	ax,ax				; see if any bytes to write
	je	to_nextseg2			; no
	cmp	xms_flag,0			; see if stashing image to XMS
	jne	seg_to_xms			; yes

; copy to EMS
	mov	WORD PTR ds:ebuff.es_len,ax
	mov	ax,ds:load_offset
	mov	ds:ebuff.es_soffset,ax	; source offset
	mov	ax,ds:prog_seg
	mov	ds:ebuff.es_ssegpage,ax	; source segment
	mov	ds:ebuff.es_dtype,1	; expanded memory destination type
	mov	ax,ds:emsxms_handle
	mov	ds:ebuff.es_dhandle,ax	; destination handle
	xor	ax,ax
	mov	ds:ebuff.es_stype,al	; conventional memory source type
	mov	ds:ebuff.es_shandle,ax	; source handle
	mov	WORD PTR ds:ebuff.es_len+2,ax
	mov	ax,cx
	and	ch,3fh
	mov	ds:ebuff.es_doffset,cx	; offset of destination
	shl	ax,1				; get top two bytes into high word for page number
    rcl dx,1
	shl	ax,1
    rcl dx,1
	mov	ds:ebuff.es_dsegpage,dx	; logical page number
	call	ems_move

to_nextseg2:
	jmp	SHORT nextseg		; go to next segment

; copy to XMS
seg_to_xms:
	mov	WORD PTR ds:xbuff.xs_len,ax
	mov	ax,ds:load_offset
	mov	WORD PTR ds:xbuff.xs_src_offset,ax	; source offset, offset
	mov	ax,ds:prog_seg
	mov	WORD PTR ds:xbuff.xs_src_offset+2,ax	; source offset, segment
	mov	ax,ds:emsxms_handle
	mov	ds:xbuff.xs_dest_handle,ax	; destination handle
	mov	WORD PTR ds:xbuff.xs_dest_offset,cx	; offset of destination
	mov	WORD PTR ds:xbuff.xs_dest_offset+2,dx
	xor	ax,ax
	mov	ds:xbuff.xs_src_handle,ax	; source handle
	mov	WORD PTR ds:xbuff.xs_len+2,ax
	call	xms_move

; loop until all segments resolved
nextseg:
	inc bp					; bump current segment to load
	cmp cs:total_seg_count,bp	; see if all segments loaded
	jb  p2_closeall			; yes
	jmp NEAR PTR mainseg_loop	; no, loop back and load them

; close all DDL files if not using overlays
p2_closeall:
	cmp cs:ovl_seg_count,0	; see if using overlays
	jne p2_done				; yes, don't close DDL files
	mov si,OFFSET handle_array
	push    cs
	pop ds

p2_closeloop:
	lodsw					; get file handle
	or  ax,ax				; zero if done
	je  p2_done				; all files closed, move to startup phase
	mov bx,ax
	mov ah,3eh				; close DDL file
	int 21h
	jmp SHORT p2_closeloop	; loop until all files closed

; program image has been built in memory
; compute start address
p2_done:
	mov ax,cs:prog_seg		; use initial program load segment as default start
	mov cs:start_segment,ax
	cmp cs:modend_modid,0	; see if any main modules
	je  setup				; no

; main module exists, get start address from it
	mov ax,cs:modend_libid	; update identifiers
	mov cs:ddl_id,ax
	mov ax,cs:modend_modid
	mov cs:module_id,ax

; find start module segdef block
	mov es,cs:segdef_seg	; get first sefdef block

main_segloop:
	mov ax,cs:modend_libid	; check segment DDL identifier
	cmp ax,es:[4]
	jne main_nextseg		; doesn't match
	mov ax,cs:modend_modid	; check segment module identifier
	cmp ax,es:[6]
	je  main_segmatch		; match

main_nextseg:
	mov es,es:[0]			; es -> next segment
	jmp SHORT main_segloop

; es -> start module segdef block
main_segmatch:
	mov cs:curr_segblk,es	; update current segdef block
	call    find_grpdef_seg
	call    find_extlist_ptr
	mov al,cs:modend_info	; get enddat value
	or  al,80h				; force segment relative in modified fixdat format
	mov cs:fixdat,al
	mov ax,WORD PTR cs:modend_info+1	; get frame index
	mov cs:frame_index,ax
	mov ax,WORD PTR cs:modend_info+3	; get target index
	mov cs:target_index,ax
	call    compute_fixvals	; get fixup offset in ax, segment in target_segment
	mov cs:start_offset,ax
	mov ax,cs:target_segment
	add cs:start_segment,ax	; update start segment
	mov ax,WORD PTR cs:modend_info+5	; add in target displacement
	add cs:start_offset,ax

; clear to top of memory
; transfer image to conventional memory
; free allocated EMS/XMS
setup:
	jmp NEAR PTR setup_and_go	; if overlays make dynamic allocations
							; for all programs, prepare stack, top of memory
							; restore ds,es; transfer to program

; data in code segment
; permanent info (if overlays)

; new stuff
; transient info
depend_seg  DW  ?			; segment of dependency list
ddl_transhdr_seg    DW  ?	; segment of DDL header info for transient DDL
modhdr_seg  DW  ?			; segment of module header info
extlist_seg DW  ?			; segment of externals list storage (2K, 1 word/external)
filepos_seg DW  ?			; segment of DDL module position

hialloc_base    DW  ?		; segment of semi-permanent high memory allocations
lowalloc_base   DW  ?		; segment of low memory allocations (lnames, symbol names)
load_offset DW  ?			; offset of file loads
load_base   DW  ?			; segment of file loads
;***load_adjust DW  ?			; amount to adjust to segment of file loads for common segment writebacks
lnames_seg  DW  0			; segment of first lnames block
segdef_seg  DW  0			; segment of first segdef block
allocseg_seg    DW  0		; last allocated segdef block
allocgrp_seg    DW  0		; last allocated grpdef block
justalloc_seg   DW  ?		; just allocated segdef block, 0 if none
justalloc_grp   DW  ?		; just allocated grpdef block, 0 if none
grpdef_seg  DW  0			; segment of first grpdef block
publics_seg DW  32 DUP (0)	; segment of first public blocks (hashed)
extdef_seg  DW  0			; segment of first externals list
allocext_seg    DW  0		; last allocated extdef block

; variables
required_count  DW  ?		; count of required modules, root and overlaid
elective_count  DW  ?		; count of elective modules, root and overlaid
ddl_depend  DW  ?			; count of dependent DDL's (updated as parsed)
ddl_totdepend	DW	?		; count of dependent DDL's (fixed at initial total)
ddl_counter DW  0			; counter of current DDL, relative 0
current_mod DW  ?			; ID of current module
mods_added  DW  ?			; count of modules added from DDL file
total_sym_count DD  0		; total count of all symbols nonlocal
res_sym_count   DD  0		; count of resolved symbols nonlocal
old_sym_count	DD	0		; old total count (see if passes done)
currmod_fpos    DD  ?		; file position of current module
seg_filepos DD  ?			; file position of loaded segment
ovl_mod_count   DW  0		; count of overlaid modules used
ovl_pub_count   DW  0		; count of overlaid publics with nonzero offset
total_seg_count DW  0		; count of all segment entries
unresolved_segs DW  ?		; count of unresolved segments during resolution phase
res_seg_count   DW  0		; count of resolved segments
symseg_ptr  DW  ?			; segment of currently loaded symbols block
symoff_ptr  DW  ?			; offset of currently loaded symbols block
lnames_blkend   DW  ?		; end (in memory) of lnames block from DDL module
handle_array    DW  17 DUP (0)	; array of DDL file handles (last entry always zero)
index_array DW  16 DUP (0)	; array of dictionary index pointers
dict_array  DW  16 DUP (0)	; array of dictionary pointers
header_array	DW	16 DUP (0)	; array of DDL header segments
fpos_array	DW	16 DUP (0)	; array of module file position table segments

classname_off   DW  0		; offset of current resolution class name
classname_seg   DW  0		; segment of current resolution class name
segname_off DW  0			; offset of current resolution segment name
segname_seg DW  0			; segment of current resolution segment name
true_seg_len    DD  ?		; true segment length (can be 10000h from Big bit)
segment_stop    DD  ?		; segment stop address
curr_extdef DW  ?			; current extdef number, relative zero
master_segdef   DW  ?		; master segdef entry for combined segments
module_id   DW  ?			; module identifier
ddl_id  DW  ?				; DDL identifier
curr_segblk DW  ?			; owning segdef block of currently loaded segment segdef
curr_grpblk DW  ?			; owning grpdef block of currently loaded segment grpdefs
extlist_ptr DW  0			; segment pointer to current external list for fixups
modend_libid    DW  0		; DDL id holding starting address modend record
modend_modid    DW  0		; module id holding starting address modend record
dgroup_ptr  DW  0			; segment of initial DGROUP group entry
_edata_ptr  DW  0			; segment of public entry for _edata
_end_ptr    DW  0			; segment of public entry for _end

near_comm_segptr    DW  0	; pointer to segdef entry of segment used for near communals
far_comm_segptr     DW  0	; pointer to segdef entry of segment currently used for far communals
huge_comm_len   DW  0		; length of huge communal variable modulo 64K
far_comm_len    DD  0		; running length of far communal variables

modend_info DB  7 DUP (?)	; enddat, frame datum, target datum, target displacment of modend
phase   DB  0				; DOSSEG segment ordering phase
res_occurred    DB  0		; nonzero flags a segment resolved in resolution pass
opt_flag    DB  ?			; nonzero if optional modules have been processed in DDL pass
dosseg_flag DB  0			; nonzero if using DOSSEG
search_flag DB  0			; nonzero if search add_pubcom for _edata and _end for DOSSEG
nomatch_flag    DB  0		; nonzero if no match on segment resolution pass (for DOSSEG)
bss_flag    DB  0			; nonzero if transition to BSS phase occurred (for DOSSEG)
stack_flag  DB  0			; nonzero if transition to STACK phase occurred (for DOSSEG)
splitfix_flag   DB  ?		; nonzero if segment image and fixups loaded separately, noncontiguously
mod_ovl_flag    DB  ?		; nonzero if module is overlaid
multiple_pass_flag	DB	0	; nonzero if on 2nd through nth pass of DDL processing
highest_align	DB	?		; highest alignment encountered in seg/grp resolution
master_group_ptr	DW	?	; master segdef group pointer in resolution
first_segptr	DW	0		; segdef position pointer list first block pointer
current_segptr	DW	?		; segdef position pointer list current block pointer

number_buff DB  6 DUP (?)   ; temporary buffer for decimal ASCII value (reversed)
number_buff2    DB  6 DUP (?)   ; temporary buffer for decimal ASCII value (unreversed)
is_unresolved	DB	?		; unresolved symbols flag
common_seg_flag	DB	?		; nonzero if segment being loaded is common segment
excess_bytes	DW	?		; count of bytes written beyond last segment

ovl_used_pub    DW  0       ; count of publics used by overlays
ovl_used_seg    DW  0       ; count of segments used by overlays
ovl_used_grp    DW  0       ; count of groups used by overlays
ovl_used_slave  DW  0       ; count of slave segments (count of canonical entries in table)
ovl_mod_flag    DB  ?       ; nonzero if overlaid module

memtext_len	DB	memtext_stop-memtext
memtext 	DB  13,10,'Not enough memory'
memtext_stop	=	$
foundtext_len	DB	foundtext_stop-foundtext
foundtext	DB	13,10,'DDL file not found: '
foundtext_stop	=	$
loadtext_len	DB	loadtext_stop-loadtext
loadtext	DB	13,10,'Error accessing DDL file: '
loadtext_stop	=	$
emsxmstext_len	DB	emsxmstext_stop-emsxmstext
emsxmstext	DB	13,10,'640K free XMS or EMS 4.0 memory required'
emsxmstext_stop	=	$
dos3text_len	DB	dos3text_stop-dos3text
dos3text	DB	13,10,'DOS version 3.0 or higher required'
dos3text_stop	=	$
emstext_len	DB	emstext_stop-emstext
emstext	DB	13,10,'EMS error occurred'
emstext_stop	=	$
xmstext_len	DB	xmstext_stop-xmstext
xmstext	DB	13,10,'XMS error occurred'
xmstext_stop	=	$

unrez_tlen   DB  unrez_tstop-unrez_text
unrez_text   DB  13,10,'Unresolved symbols exist.  Total: '
unrez_tstop  =   $
list_tlen   DB  list_tstop-list_text
list_text   DB  13,10,'List of unresolved symbols:',13,10
list_tstop  =   $

ddl_path    DB  'DDLPATH=',0
codetext    DB  'CODE',0
begdatatext DB  'BEGDATA',0
bsstext     DB  'BSS',0
stacktext   DB  'STACK',0
_edatahigh  DB  8,'_EDATA',0
_endhigh    DB  6,'_END',0
_edatalow   DB  8,'_edata',0
_endlow     DB  6,'_end',0
c_commontext    DB  'c_common',0	; near communal segment name
far_bsstext DB  'FAR_BSS',0	; segment/class name for far communal variables
huge_bsstext    DB  'HUGE_BSS',0	; segment/class name for huge communal variables

_cla_xqu_text	DB	10,'_CLA_XQU',0
runit_text	DB	7,'RUNIT',0
beforerun_text	DB	11,'BEFORERUN',0
afterrun_text	DB	10,'AFTERRUN',0
_cla_restart_text	DB	14,'_CLA_RESTART',0

; unresolved externals error
unrez_error:
	push    cs
	pop ds
	mov bx,OFFSET unrez_text
	mov cl,[bx-1]           ; get length of string
	mov dx,bx               ; ds:dx -> string
	xor ch,ch               ; zap high byte of cx
	mov bx,STDOUT           ; write to standard output device
	mov ah,40h              ; write to device
	int 21h

	mov di,OFFSET number_buff   ; point to temporary number buffer
	push    cs
	pop es                  ; es -> data
	xor cx,cx               ; init count of digits
	mov ax,WORD PTR cs:total_sym_count  ; get total symbol table count low word
	sub ax,WORD PTR cs:res_sym_count    ; subtract resolved (good for up to 65535 unresolved)

gwc_divloop:
	xor  dx,dx              ; zero high word value
	mov  bx,0AH             ; divide by 10
	div  bx
	xchg    dx,ax           ; swap quotient into dx, remainder into ax
	or   al,30H             ; make remainder into ASCII number
	stosb                   ; save char to buffer
	inc cx                  ; bump count of digits
	xchg    dx,ax           ; restore quotient to ax
	or   ax,ax              ; check if quotient is zero
	jne  gwc_divloop        ; no, continue dividing

	mov si,di
	mov di,OFFSET number_buff2  ; place to put unreversed number
	mov bx,cx               ; save count of chars in string

gwc_revloop:
	dec si                  ; si -> char in reversed number buffer
	mov al,[si]             ; get reversed char
	stosb                   ; put in unreversed buffer
	loop    gwc_revloop     ; unreverse as many chars as in number

	mov cx,bx               ; get count of chars in string
	mov dx,OFFSET number_buff2  ;  ds:dx -> buffer to write
	mov bx,STDOUT           ; write to standard output device
	mov ah,40h              ; write to device
	int 21h

; give a trailing CR/LF
gwc_ret:
	mov cx,2
	mov dx,OFFSET unrez_text
	mov bx,STDOUT
	mov ah,40h              ; write to device
	int 21h

; unresolved externals list
	mov bx,OFFSET list_text
	mov cl,[bx-1]           ; get length of string
	mov dx,bx               ; ds:dx -> string
	xor ch,ch               ; zap high byte of cx
	mov bx,STDOUT           ; write to standard output device
	mov ah,40h              ; write to device
	int 21h

	mov ax,extdef_seg

unres_loop:
	or  ax,ax
	je  unres_done			; all done with unresolved list
	mov ds,ax
	mov cx,ds:[0]           ; get count of externals
	jcxz    next_unres      ; no externals this block

	mov si,14

unres_symloop:
	lodsw                   ; get -> symbol entry
	mov es,ax               ; es -> symbol entry
	mov ax,es:[14]          ; definition flag in al, communal flag in ah
	and ah,40h              ; see if communal
	jne unres_nextsym       ; yes
	and al,3
	cmp al,2                ; see if public or absolute
	jae unres_nextsym       ; yes

; unresolved external, display it
	push    ds
	push    cx
	mov bx,STDOUT
	lds di,es:[4]           ; ds:di -> symbol name
	inc	di					; point past length byte

unres_printloop:
	mov dx,di
	mov cx,1
	mov ah,40h              ; print to device
	int 21h
	inc di
	cmp BYTE PTR [di],0     ; see if at end of name yet
	jne unres_printloop     ; no

; print cr/lf after symbol name
	push    cs
	pop ds
	mov cx,2
	mov dx,OFFSET unrez_text
	mov ah,40h              ; write to device
	int 21h

	pop cx
	pop ds
	or  BYTE PTR es:[14],2  ; set public/absolute flag, so only printed once

unres_nextsym:
	loop    unres_symloop   ; loop through all symbols

next_unres:
	mov ax,ds:[6]           ; ax -> next external list
	jmp SHORT unres_loop

unres_done:
	mov	ax,1
	push	ax				; error code on stack for error exit
	jmp	NEAR PTR error_exit

; not 640K free XMS or EMS 4.0
noemsxms_error:
	xor	ah,ah				; no file message
	mov dx,OFFSET emsxmstext
	mov	al,88h				; preset to expanded memory out of memory
	cmp	cs:ems_flag,0		; see if using EMS
	je	print_feedback		; yes
	mov	al,0a0h				; xms out of memory
	jmp	SHORT print_feedback

; version of DOS less than 3.0
; al has error code
dos3_error:
	mov	al,1				; invalid function return code
	xor	ah,ah				; no file message
	mov dx,OFFSET emstext
	jmp	SHORT print_feedback

; error accessing EMS
; al has error code
ems_error:
	xor	ah,ah				; no file message
	mov dx,OFFSET emstext
	jmp	SHORT print_feedback

; error accessing XMS
; al has error code
xms_error:
	xor	ah,ah				; no file message
	mov dx,OFFSET xmstext
	jmp	SHORT print_feedback

; not enough memory for all necessary allocations
mem_error:
	mov	ax,8				; al== error code; ah==0, no file message
	mov dx,OFFSET memtext

print_feedback:
	push	ds				; save ds:bx -> filename, if any
	push	bx
	push    cs
	pop ds
	push	ax				; save error code
	mov	bx,dx
	mov	cl,[bx-1]
	xor	ch,ch
	mov bx,STDOUT
	mov ah,40h				; print to device
	int 21h
	pop	ax					; restore error code
	pop	dx
	pop	ds					; ds:dx -> file name, if any
	push	ax				; save error code
	or	ah,ah				; see if filename message
	je	printcrlf			; no

; print file name one char at a time
printloop:
	mov	bx,dx
	cmp	BYTE PTR [bx],0		; see if at end of file name
	je	printcrlf			; yes
	mov	cx,1
	mov	bx,STDOUT
	mov ah,40h				; print to device
	int 21h
	inc	dx					; move to next char in file name
	jmp	SHORT printloop

; print trailing CR/LF
printcrlf:
	push	cs
	pop	ds
	mov	dx,OFFSET memtext	; start of string has cr/lf pair
	mov	cx,2
	mov bx,STDOUT
	mov ah,40h				; print to device
	int 21h

; exiting in error
error_exit:
	call	free_640k_alloc	; free up memory image EMS/XMS allocation
	pop	ax					; restore error code
	mov ah,4ch				; terminate with return code
	int 21h

; error accessing file, al has error code
access_error:
	mov	bx,dx				; save -> filename
	mov dx,OFFSET loadtext
	mov	ah,1				; flag file name feedback
	jmp	SHORT print_feedback

; error finding file
found_error:
	mov	bx,dx				; save -> filename
	mov dx,OFFSET foundtext
	mov	ax,102h				; flag file name feedback, file not found return code
	jmp	SHORT print_feedback

;*****************************
;                            *
; SUBROUTINES BEGIN          *
;                            *
;*****************************

;*****************************
;* GET_CLA_SEG_OFF           *
;*****************************

; return segment:offset of clarion internal in ax:di
; upon entry, bx -> public entry of internal
; destroys ax,bx,di,ds

get_cla_seg_off	PROC	NEAR
    mov ds,bx               ; ds -> public entry
    xor bx,bx
    mov di,ds:[8]           ; get public offset
    mov ds,ds:[0]           ; ds -> owning segment
    mov ax,ds:[2]           ; segment offset
    mov bx,ax
    and bx,0fh
    add di,bx               ; di == offset within segment frame

    mov bx,ds:[4]           ; bx:ax == absolute segment address
    shr bx,1
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1
    rcr ax,1                ; /16
    add ax,cs:prog_seg      ; add in program load segment
    ret
get_cla_seg_off	ENDP

;*****************************
;* COMMON_SEG_CHECK          *
;*****************************

; check if common segment, adjust length and offset if so
; upon entry ax==length (# of bytes), dx:cx == offset
; prog_seg:load_offset -> bytes to write, ds -> code segment
; updates ax, dx:cx, load_offset (assume load_offset will remain <16K)
; destroys di,si,es

common_seg_check	PROC
	cmp	common_seg_flag,0
	jne	csc_iscommon		; is a common segment
	ret						; not common, make no adjustment

csc_iscommon:
	mov	di,ax				; di holds length/count
	mov	es,prog_seg
	mov	si,load_offset

csc_loop:
    lods    BYTE PTR es:0   ; get common segment byte
	or	al,al				; see if a zero
	jne	csc_ret				; no, transfer from this address
	cmp	di,excess_bytes		; see if at excess (new) bytes written
	jbe	csc_ret				; yes, done
	dec	di					; drop length
	add	cx,1				; bump destination offset
	adc	dx,0
	inc	load_offset			; bump source offset
	jmp	SHORT csc_loop		; check next bytes

csc_ret:
	mov	ax,di				; ax holds updated length
	ret
common_seg_check	ENDP

;*****************************
;* MAKE_DUMMY_TEXT           *
;*****************************

; DOSSEG set, put in a dummy _TEXT segment with 16 zero bytes,
; if any other _TEXT segments exist
; destroys ax,cx,dx,di,ds,es

make_dummy_text PROC    NEAR
	push	bx				; save critical register
	xor	bx,bx				; init previous segdef block pointer

; cycle through all segments looking for a _TEXT segment name
	mov ax,cs:segdef_seg

mdt_segloop:
	or  ax,ax				; see if valid block
	je  mdt_ret				; no
	mov ds,ax
	mov cx,ds:[2]			; get count of entries in block
	mov dx,ds				; save -> segdef block

; preset ds 1 para below start of block so double increment will -> first entry
	mov ax,ds
	dec ax
	mov ds,ax

mdt_chkloop:
	mov ax,ds				; get old entry pointer
	inc ax
	inc ax
	mov ds,ax				; ds -> next entry in block
	mov di,ds:[8]
	mov es,ds:[10]			; es:di -> lnames name entry
	add di,8				; adjust past 2 dword pointers
	cmp BYTE PTR es:[di],'_'	; see if _TEXT segment
	jne mdt_nextent
	cmp BYTE PTR es:[di+1],'T'
	jne mdt_nextent
	cmp BYTE PTR es:[di+2],'E'
	jne mdt_nextent
	cmp BYTE PTR es:[di+3],'X'
	jne mdt_nextent
	cmp BYTE PTR es:[di+4],'T'
	jne mdt_nextent
	cmp BYTE PTR es:[di+5],0
	je  mdt_match			; success

mdt_nextent:
	loop    mdt_chkloop		; loop through all entries in segdef block
	mov ds,dx				; restore ds -> segdef block
	mov	bx,dx				; keep -> previous block
	mov ax,ds:[0]			; ax -> next block
	jmp SHORT mdt_segloop

mdt_ret:
	pop	bx
	ret

; ds -> segdef entry of first _TEXT segment
; dx -> current segdef block
; bx -> previous segdef block, zero if none
mdt_match:
;***	mov dx,cs:segdef_seg	; change segdef_seg to point to dummy _TEXT segment
	mov ax,cs:hialloc_base
	sub ax,3
	mov cs:hialloc_base,ax
;***	mov cs:segdef_seg,ax
	mov es,ax				; es -> dummy segdef block for dummy _TEXT segment
	xor ax,ax				; use ax == 0 as constant

	cmp	bx,ax				; see if first segdef block
	jne	mdt_notfirst

; current is first segdef block, change segdef_seg to new block
	mov	cs:segdef_seg,es
	jmp	SHORT mdt_setblk

; patch new block between previous and current pointers
; previous -> new -> current (old)
mdt_notfirst:
	mov	di,es				; di -> new block
	mov	es,bx				; es -> previous block
	mov	es:[0],di			; previous block -> new
	mov	es,di				; es -> new block

; setup dummy segment block
; es -> new block
; dx -> current (old) block
; ax == 0
mdt_setblk:
	mov es:[0],dx			; new block -> old
	mov	di,1				; use as constant
	mov es:[2],di			; 1 entry
	mov	es:[8],di
	mov	es:[10],di
	mov	es:[12],di
	mov es:[4],ax			; zero lib, module id
	mov WORD PTR es:[6],-1

; setup dummy segment entry
	mov di,10h
	mov WORD PTR es:[di+18],-1		; module id
	mov es:[di+16],ax		; group index

	mov es:[di+2],ax		; zero offset of segment
	mov es:[di+4],ax

	mov es:[di+27],ax		; resolved flag, overlay flags
	mov	es:[di+16],ax		; zero group index
	mov WORD PTR es:[di+6],10h	; segment length
	mov BYTE PTR es:[di+26],48h	; acbp byte
	mov ax,ds:[8]			; segment name seg:off
	mov es:[di+8],ax
	mov ax,ds:[10]
	mov es:[di+10],ax
	mov ax,ds:[12]			; class name seg:off
	mov es:[di+12],ax
	mov ax,ds:[14]
	mov es:[di+14],ax
	inc cs:total_seg_count	; bump count of segments
	pop	bx
	ret

make_dummy_text ENDP

;*****************************
;* CHECK_MODULE              *
;*****************************

; pass 1, check if optional module should be added
; returns al != 0 if module was added, otherwise al==0
; destroys ax,cx,dx,si,di,ds,es

check_module    PROC    NEAR
	mov ds,cs:filepos_seg
	mov si,cs:current_mod
	dec si					; make relative 0
	shl si,1				; convert to dword offset
	shl si,1
	lodsw					; get low word of file position
	or  ax,ds:[si]			; merge in high word of file position
	jne cm_valid			; not processed, try it

	xor al,al				; show module failed check
	ret

; get entry from dictionary index
cm_valid:
	mov di,cs:ddl_counter	; get count of current DDL
	shl di,1				; convert to word offset
	mov ds,cs:[di+OFFSET index_array]	; ds -> dictionary index
	mov si,di				; save offset
	mov di,cs:current_mod
	dec di					; make relative 0
	shl di,1				; convert to word offset
	mov dx,ds:[di]			; get offset end paragraph
	mov cl,4
	shl dx,cl				; compute offset end byte

cm_firstchk:
	or  di,di				; see if first entry
	je  cm_getcount			; yes, at 0 offset by definition
	mov ax,ds:[di-2]		; get previous entry paragraph end (this entry start)
	or	ax,ax				; see if nonzero
	jne	cm_nonzero			; yes
	sub	di,2				; no, back up to next previous entry
	jmp	SHORT cm_firstchk

cm_nonzero:
	mov di,ds:[di-2]		; get previous entry paragraph end (this entry start)
	shl di,cl				; convert to byte count

; get pubdef end, di -> start of module dictionary
cm_getcount:
	mov es,cs:[si+OFFSET dict_array]	; es:di -> pubdef name in dictionary

;***	mov ds,cs:modhdr_seg	; ds -> module header storage area
	mov cx,dx				; get offset end byte

cm_publoop:
	mov dx,di				; save -> start of new name
	xor	ah,ah
	mov al,es:[di]			; get length byte of name
	sub	ax,2
	add	di,ax				; di -> last char of name before null terminator
	add	al,es:[di]			; add to hash code
	mov	di,dx				; restore di -> name
	add	al,es:[di+2]		; add in second char of name, after length byte & first char
	and	al,31				; convert to hash code

	shl al,1				; convert to word offset
	mov si,ax
	mov ax,cs:[si+OFFSET publics_seg]	; get starting entry
	or  ax,ax				; see if exists
	je  cm_nextpub			; no

	mov ds,ax
	mov bp,ax				; save -> current pubdef entry matching against
	lds si,ds:[4]			; ds:si -> current pubdef entry name

; es:di -> new pubdef name, ds:si -> current pubdef name
cm_bploop:
	cmpsb					; compare length bytes
	jne	cm_fail

; length bytes matched
	push	cx				; save offset end byte
	mov	cl,[si-1]
	sub	cl,2				; name bytes-length byte and null terminator
	xor	ch,ch				; cx holds remaining name bytes
	repe	cmpsb			; compare the names
	pop	cx					; restore offset end byte
	je	cm_pubmatch			; public names match

;***	mov al,[si]
;***	or  al,es:[di]			; see if both values are zero (matched to null terminator)
;***	je  cm_pubmatch			; yes, public name matches

;***	cmpsb					; compare a nonzero byte in the two names
;***	je  cm_bploop			; bytes match, loop for next byte test

; byte in names did not match	
cm_fail:
	mov di,dx				; restore di -> start of name entry
	mov	si,10				; setup for old name greater than new name
	jc  cm_pubshared		; old name greater than new name

; new name greater than old name
	mov	si,12				; si -> segment of pubdef entry having greater name

cm_pubshared:
	mov ds,bp				; ds -> owning pubdef entry of old name
	mov ax,[si]				; get segment of pubdef entry have lesser or greater name
	or  ax,ax				; see if exists
	je  cm_nextpub			; no
	mov ds,ax
	mov bp,ax				; save -> current pubdef entry matching against
	lds si,ds:[4]			; ds:si -> current pubdef entry name
	jmp SHORT cm_bploop		; check next pubdef entry

; public name matches pre-existing, check if pre-existing is nonlocal comdef or extdef
cm_pubmatch:
	push	es				; save es -> name
	mov es,bp				; es -> matched public entry

	mov al,es:[14]			; get old public definition flag
	and al,3				; mask off extraneous bits
	cmp al,2				; see if old is public or absolute
	pop	es					; restore es -> name
	jb  cm_success			; no, successful match

cm_nextpub:
	mov di,dx				; restore di -> start of name entry
;***	mov dx,cx				; save offset end byte

;***	mov cx,255				; all names <255 chars
;***	xor ax,ax				; search for end of name
;***	repne   scasb			; find end of name string
	mov	al,es:[di]			; get length of name byte
	xor	ah,ah				; zap high byte
	add	di,ax				; di -> byte after name

	cmp BYTE PTR es:[di],0	; see if at zero padding
	je  cm_nomatch			; yes, done with names

;***	mov cx,dx				; restore offset end byte to cx
	cmp di,cx				; see if at or beyond offset end byte
	jb  cm_publoop			; no

; no matches found
cm_nomatch:
	xor al,al				; show module failed check
	ret

; successfully found a pubdef entry that resolves a symbol
cm_success:
	call    add_module		; add the module
	mov al,1				; flag that module was added
	ret
check_module    ENDP

;*****************************
;* ADD_MODULE                *
;*****************************

; pass 1, process and add module to DDL list
; destroys ax,cx,dx,si,di,bp,ds,es

add_module  PROC    NEAR
	call    read_mod_header	; read module's header
	call    compute_load_base	; update the file load area
	call    read_mod_nonbinary	; read non-binary information from module

	push    bx				; save critical register
	mov ds,cs:modhdr_seg	; ds -> module header storage area
	mov al,BYTE PTR ds:[mh_flags]	; get low byte of module flags
	mov ah,al
	and al,14h				; see if overlaid module
	mov cs:mod_ovl_flag,al	; save overlaid state
	je  am_chkmain			; no
	inc cs:ovl_mod_count	; bump count of overlaid modules

am_chkmain:
	and ah,1				; see if main module
	je  am_notmain			; no
	cmp cs:modend_modid,0	; see if main module selected yet
	jne am_notmain			; yes
	mov ax,ds:[mh_id]		; save module id
	mov cs:modend_modid,ax
	mov ax,cs:ddl_counter	; save library id
	mov cs:modend_libid,ax
	push    cs
	pop es
	mov di,OFFSET modend_info
	mov si,OFFSET ds:[mh_startup]	; ds:si -> startup info
	mov cx,7
	rep movsb				; save startup info

am_notmain:
	mov di,WORD PTR ds:[mh_segdef]	; get file position of symbols block (block following lnames)

	mov dx,WORD PTR ds:[mh_lnames]	; get file position of lnames block in ax:dx
	mov ax,WORD PTR ds:[mh_lnames+2]
	sub di,dx				; get size of lnames block in di (must be <64K)

	call    filepos_to_segoff	; convert file position in ax:dx
	add di,si				; compute end of lnames block
	mov cs:lnames_blkend,di	; save it

	mov ax,cs:lnames_seg	; get first lnames block
	or  ax,ax				; see if it exists
	jne am_mainloop			; yes

; allocate a block for lnames storage
	mov ax,cs:lowalloc_base
	mov cs:lnames_seg,ax	; use old low allocation base as new segment
	mov es,ax				; es -> lnames block
	add ax,LNAMES_BLK_SIZE
	mov cs:lowalloc_base,ax	; update low allocation base
	xor di,di
	mov es:[di],LNAMES_LIST_SIZE	; all lnames entry space is free
	mov es:[2],di			; zero pointer to next block
	mov cl,2				; flag no parent entry
	jmp SHORT am_firstname

am_mainloop:
	mov di,4				; init past 2 system words
	push    si				; save -> new name entry

am_testloop:
	mov es,ax				; es -> lnames block
	mov dx,di				; save -> start of test name
	add di,8				; scan past segment:offset high and low pointers
	pop si					; si -> new name entry
	push    si				; save -> name entry back on stack
	add si,8				; ds:si -> actual name past 2 dword pointers

; ds:si -> name, es:di -> name to test against
am_byteloop:
	mov al,[si]
	or  al,es:[di]			; see if both values are zero (matched to null terminator)
	jne am_chkchar			; no
	jmp NEAR PTR am_match	; yes, don't add name entry

am_chkchar:
	cmpsb					; compare a nonzero byte in the two names
	je  am_byteloop			; bytes match, loop for next byte test
	mov di,dx				; restore di -> start of name entry
	jc  am_oldgreat			; old name greater than new name

; new name greater than old name
	xor cl,cl				; flag new > old, update high pointer
	mov ax,es:[di+6]		; get segment of lnames entry having a greater name
	or  ax,ax				; see if exists
	je  am_newname			; no
	mov di,es:[di+4]		; get offset of lnames entry having greater name
	jmp SHORT am_testloop	; check next entry

; old name greater than new name
am_oldgreat:
	mov cl,1				; flag old > new update low pointer
	mov ax,es:[di+2]		; get segment of lnames entry having lesser name
	or  ax,ax				; see if exists
	je  am_newname			; no
	mov di,es:[di]			; get offset of lnames entry having lesser name
	jmp SHORT am_testloop	; check next entry

; unique lname, add to list
; es:di -> parent entry
; cl==0 if new name > old name, ==1 if old name > new name, ==2 if no parent
am_newname:
	pop si					; ds:si -> new entry

am_firstname:
	mov dx,si
	add si,8				; ds:si -> actual name past 2 dword pointers

am_lenloop:
	lodsb					; get name char
	or  al,al				; see if at end of name
	jne am_lenloop			; no

	mov bp,es				; save -> parent entry
	mov ax,si
	sub ax,dx				; ax holds length of name including null terminator+ 2 dword pointers (8 bytes)
	mov si,dx				; restore si -> new entry
	mov dx,ax				; save space

am_comploop:
	cmp dx,es:[0]			; compare to free lname entry space
	jbe am_fit				; this entry will fit

; not enough room for name in this block, check other lnames block
	mov ax,es:[2]			; get pointer to next block
	or  ax,ax				; see if valid
	je  am_lnalloc			; no, make new block
	mov es,ax				; es -> next block
	jmp SHORT am_comploop	; check it next block

; no more blocks to check for free space, allocate one
am_lnalloc:
	mov ax,cs:lowalloc_base
	mov es:[2],ax			; save pointer to new block in old block
	mov es,ax				; es -> new lnames block
	add ax,LNAMES_BLK_SIZE
	mov cs:lowalloc_base,ax	; update low allocation base
	mov WORD PTR es:[0],LNAMES_LIST_SIZE	; all lnames entry space is free
	mov WORD PTR es:[2],0	; zero pointer to next block

; bp:di -> parent entry, es -> lname block to place new entry
; ds:si -> new entry, dx == name length
; cl==0 if new name>old name, ==1 if old name>new name, ==2 if no parent
am_fit:
	mov ax,es:[0]			; get free lnames space
	sub es:[0],dx			; subtract off new entry amount
	push    dx				; save length of entry
	mov dx,LNAMES_BLK_SIZE*16	; compute offset in block
	sub dx,ax				; dx holds offset in block
	mov ax,es				; ax:dx -> location in block for new entry
	mov es,bp				; es:di -> parent entry
	cmp cl,1				; see if old name>new name
	je  am_oldgreat2		; yes
	ja  am_noparent			; no parent

; new name > old name
	mov es:[di+4],dx		; update offset of high name pointer
	mov es:[di+6],ax		; update segment of high name pointer
	jmp SHORT am_noparent

; old name > new name
am_oldgreat2:
	mov es:[di],dx			; update offset of high name pointer
	mov es:[di+2],ax		; update segment of high name pointer

am_noparent:
	mov di,dx
	mov es,ax				; es:di -> lname block location to place new entry
	pop cx					; get length of entry
	mov ax,cx

; put entry in lnames block
    shr cx,1                ; convert to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
	sub si,ax				; restore si -> name
	mov di,dx
	xor ax,ax
	stosw					; zero out low/high pointers
	stosw
	stosw
	stosw
	jmp SHORT am_updname	; update segdef and grpdef name pointers

; matched, don't add to lnames block
am_match:
	pop si					; si -> lnames entry from DDL module

; es:dx -> lnames entry in lnames block
; ds:si -> lnames entry in DDL module
; step through each segdef and grpdef entry and wherever a name pointer
; equals si in low word, change to es:dx value
am_updname:
	push    ds				; save -> lnames entry
	mov di,si
	mov ds,cs:modhdr_seg
	mov cx,ds:[mh_segcount]	; get count of segments
	jcxz    am_nextscan		; no segments, hence no groups

	mov bp,dx				; save updated lnames block offset
	mov dx,WORD PTR ds:[mh_segdef]	; get file position of segdef block in ax:dx
	mov ax,WORD PTR ds:[mh_segdef+2]
	call    filepos_to_segoff	; convert file position in ax:dx to ds:si
	mov ax,es				; ax:bp == lnames block position

am_updloop:
	cmp ds:[si+8],di		; see if segment name match
	jne am_noseg			; no
	cmp WORD PTR ds:[si+10],0	; see if already updated
	jne am_noseg			; yes
	mov ds:[si+8],bp		; update with new pointer
	mov ds:[si+10],ax

am_noseg:
	cmp ds:[si+12],di		; see if class name match
	jne am_noclass			; no
	cmp WORD PTR ds:[si+14],0	; see if already updated
	jne am_noclass			; yes
	mov ds:[si+12],bp		; update with new pointer
	mov ds:[si+14],ax

am_noclass:
	add si,32				; bump to next segdef entry
	loop    am_updloop

	mov ds,cs:modhdr_seg
	mov cx,ds:[mh_grpcount]	; get count of groups
	jcxz    am_nextscan		; no groups

	mov dx,WORD PTR ds:[mh_grpdef]	; get file position of grpdef block in ax:dx
	mov ax,WORD PTR ds:[mh_grpdef+2]
	call    filepos_to_segoff	; convert file position in ax:dx to ds:si
	mov ax,es				; ax:bp == lnames block position

am_updloop2:
	cmp ds:[si+4],di		; see if group name match
	jne am_nextgrp			; no
	cmp WORD PTR ds:[si+6],0	; see if already updated
	jne am_nextgrp			; yes
	mov ds:[si+4],bp		; update with new pointer
	mov ds:[si+6],ax

am_nextgrp:
	add si,16				; bump to next grpdef entry
	loop    am_updloop2

; scan to next lnames entry in DDL module
am_nextscan:
	pop ds					; restore ds:si -> lnames entry in DDL module
	mov si,di
	add si,8				; bump past 2 dword pointers in front of name

am_scanloop:
	lodsb					; get name char
	or  al,al				; see if null terminator
	jne am_scanloop			; not yet
	cmp si,cs:lnames_blkend	; see if at end of module lnames block
	jae am_seggrp			; yes
	mov ax,cs:lnames_seg	; get first lnames block in ax
	jmp NEAR PTR am_mainloop	; ds:si -> next lnames entry

to_am_publics:
	jmp NEAR PTR am_publics

am_seggrp:
	mov ds,cs:modhdr_seg
	mov cx,ds:[mh_segcount]	; get count of segments
	mov cs:justalloc_seg,cx	; save as just allocated block (if zero then zero)
	jcxz    to_am_publics	; no segments, hence no groups

; save the grpdef entries
	mov cx,ds:[mh_grpcount]	; get count of groups
	mov cs:justalloc_grp,cx	; save as just allocated block (if zero then zero)
	jcxz    am_saveseg		; no groups

	mov dx,cx				; get count of paragraphs
	inc dx					; adjust for system info paragraph
	xor di,di				; offset in grpdef blocks

; allocate a block for grpdef storage
	mov ax,cs:hialloc_base
	sub ax,dx				; subtract entries+system info allocation (high memory allocs build down)
	cmp cs:grpdef_seg,0		; see if any grpdef block previously allocated
	jne am_walkgrp			; yes, walk to last one
	mov cs:grpdef_seg,ax	; no, use old high allocation base as new segment
	jmp SHORT am_grpbase

am_walkgrp:
	mov es,cs:allocgrp_seg
	mov es:[di],ax			; update block pointer

am_grpbase:
	mov es,ax				; es -> grpdef block
	mov cs:hialloc_base,ax	; update high allocation base
	mov cs:allocgrp_seg,ax	; save last allocated grpdef block
	mov cs:justalloc_grp,ax	; save just allocated grpdef block
	mov es:[di],di			; zero pointer to next block
	mov es:[2],cx			; save count of entries in block
	mov ax,cs:ddl_counter
	mov es:[4],ax			; save DDL identifier
	mov ax,ds:[mh_id]
	mov es:[6],ax			; save module identifer

	mov dx,WORD PTR ds:[mh_grpdef]	; get file position of grpdef block in ax:dx
	mov ax,WORD PTR ds:[mh_grpdef+2]
	call    filepos_to_segoff	; convert file position in ax:dx to ds:si
	mov di,16				; point past system paragraph
	xor ax,ax
	mov ds:[si+10],ax		; zero out back reference pointer
	mov ds:[si+12],ax		; zero out flags
	mov ax,cx				; save count

am_grptrans:
	push    cx				; save count of entries to write
	mov cx,8				; write 8 words
	rep movsw
	pop cx					; restore entry count
	loop    am_grptrans		; loop until all entries written

	mov cx,ax				; restore group count to cx
	mov ax,cs:dgroup_ptr
	or  ax,ax				; see if a dgroup pointer yet
	jne am_saveseg			; yes

; check if dgroup exists, save pointer if so
	call    get_dgroup_ptr

; save the segdef entries
am_saveseg:
	mov ds,cs:modhdr_seg
	mov cx,ds:[mh_segcount]	; get count of segments
	add cs:total_seg_count,cx	; add to total count of segments in program
	mov dx,cx
	add dx,dx				; get count of paragraphs (2/entry)
	inc dx					; adjust for system info paragraph
	xor di,di				; offset in segdef blocks

; allocate a block for segdef storage
	mov ax,cs:hialloc_base
	sub ax,dx				; subtract in entries+system info (high memory allocs build down)
	cmp cs:segdef_seg,0		; see if any segdef block previously allocated
	jne am_walkseg			; yes, walk to last one
	mov cs:segdef_seg,ax	; no, use old high allocation base as new segment
	jmp SHORT am_segbase

am_walkseg:
	mov es,cs:allocseg_seg
	mov es:[di],ax			; update block pointer

am_segbase:
	mov es,ax				; es -> segdef block
	mov cs:allocseg_seg,ax	; save last allocated segdef block
	mov cs:justalloc_seg,ax	; save just allocated segdef block
	mov cs:curr_segblk,ax
	mov cs:hialloc_base,ax	; update high allocation base
	mov es:[di],di			; zero pointer to next block
	mov es:[2],cx			; save count of entries in block
	mov	es:[8],cx
	mov	es:[10],cx
	mov	es:[12],cx
	mov ax,cs:ddl_counter
	mov es:[4],ax			; save DDL identifier
	mov ax,ds:[mh_id]
	mov es:[6],ax			; save module identifer
	mov cs:module_id,ax

	mov dx,WORD PTR ds:[mh_binfpos]	; get file position of binary file position block in ax:dx
	mov ax,WORD PTR ds:[mh_binfpos+2]
	call    filepos_to_segoff	; convert file position in ax:dx to ds:si
	mov bx,si				; bx offsets into binary file position table
	mov bp,ds				; bp:bx -> binary file position table

	mov ds,cs:modhdr_seg
	mov dx,WORD PTR ds:[mh_segdef]	; get file position of segdef block in ax:dx
	mov ax,WORD PTR ds:[mh_segdef+2]
	call    filepos_to_segoff	; convert file position in ax:dx to ds:si
	mov di,16				; point past system paragraph

am_segtrans:
	push    cx				; save count of entries to write
	mov al,BYTE PTR cs:ddl_counter	; save DDL library number
	mov ds:[si+29],al
	mov ax,cs:module_id		; save module id
	mov ds:[si+18],ax

	mov al,ds:[si+28]		; get overlay class status
	and al,1 
	je  am_segtrans2		; not overlay class
	cmp WORD PTR ds:[si+6],0	; see if zero length segment
	je  am_segtrans2		; yes, ignore it
	mov al,cs:mod_ovl_flag	; get overlaid module status
	or  al,al
	je  am_segtrans2		; not an overlaid module
	or  ds:[si+28],al		; overlay class in overlaid module, flag overlaid
	inc cs:ovl_seg_count	; bump count of overlaid segments

am_segtrans2:
	push    ds
	mov ds,bp
	mov ax,ds:[bx]			; get binary file position table dword entry
	mov dx,ds:[bx+2]
	pop ds
	mov ds:[si+20],ax		; save binary file position in entry
	mov ds:[si+22],dx
	mov cx,16				; write 16 words
	rep movsw
	pop cx					; restore entry count
	add bx,4				; move to next binary file position dword entry
	loop    am_segtrans		; loop until all entries written

am_publics:
	mov ds,cs:modhdr_seg
	mov dx,WORD PTR ds:[mh_symbols]	; get file position of symbols block in ax:dx
	mov ax,WORD PTR ds:[mh_symbols+2]
	call    filepos_to_segoff	; convert file position in ax:dx to ds:si
	mov cs:symseg_ptr,ds	; save -> symbols block
	mov cs:symoff_ptr,si
	xor bp,bp				; init count of symbol chars saved

	mov ds,cs:modhdr_seg
	mov cx,ds:[mh_pubcount]	; get count of publics
	jcxz    am_comdefs		; no publics, parse any comdefs

	mov dx,WORD PTR ds:[mh_pubdef]	; get file position of pubdef block in ax:dx
	mov ax,WORD PTR ds:[mh_pubdef+2]
	call    filepos_to_segoff	; convert file position in ax:dx to ds:si
	call    add_pubcom		; add/update public entries and symbols

am_comdefs:
	mov ds,cs:modhdr_seg
	mov cx,ds:[mh_comcount]	; get count of communals
	jcxz    am_extdefs		; no communals, parse any extdefs

	mov dx,WORD PTR ds:[mh_comdef]	; get file position of comdef block in ax:dx
	mov ax,WORD PTR ds:[mh_comdef+2]
	call    filepos_to_segoff	; convert file position in ax:dx to ds:si
	call    add_pubcom		; add/update public entries and symbols

am_extdefs:
	mov ds,cs:modhdr_seg
	mov cx,ds:[mh_extcount]	; get count of externals
	jcxz    am_lowupd		; no communals, update low memory allocation

	mov dx,WORD PTR ds:[mh_extdef]	; get file position of extdef block in ax:dx
	mov ax,WORD PTR ds:[mh_extdef+2]
	call    filepos_to_segoff	; convert file position in ax:dx to ds:si
	call    add_extdef		; add external entries and symbols

; update low base past added symbol names (bp holds char count past original low alloc)
am_lowupd:
	add bp,15				; round up to next para
	shr bp,1				; /2
	shr bp,1				; /4
	shr bp,1				; /8
	shr bp,1				; /16
	mov ax,cs:lowalloc_base
	add ax,bp				; adjust past saved symbol names
	mov cs:lowalloc_base,ax	; save new low allocation base

; copy extern list to high memory allocation, saving info on extern count
;   library id and module id
	mov ds,cs:modhdr_seg
	mov cx,ds:[mh_extcount]	; get count of externals
	mov dx,cx
	mov di,cx
	add di,di				; double count of entries (1 word/entry)
	add di,29				; di holds total byte count with 7 system words rounded to next para
	shr di,1				; /2
	shr di,1				; /4
	shr di,1				; /8
	shr di,1				; /16, di holds paragraphs to allocate
	mov ax,cs:hialloc_base	; allocate space high for module extern list
	sub ax,di
	mov cs:hialloc_base,ax	; ax -> extern list

	cmp cs:extdef_seg,0		; see if any extdef block previously allocated
	jne am_walkext			; yes, walk to last one
	mov cs:extdef_seg,ax	; no, use old high allocation base as new segment
	jmp SHORT am_extbase

am_walkext:
	mov es,cs:allocext_seg
	mov es:[6],ax			; update block pointer

am_extbase:
	mov es,ax
	mov cs:allocext_seg,ax	; save last allocated extdef block

	xor di,di
	mov ax,dx				; save count of externals
	stosw
	mov ax,cs:ddl_counter	; save DDL library number
	stosw
	mov ax,ds:[mh_id]		; save module id
	stosw
	xor ax,ax				; zero block pointer
	stosw
	mov ax,WORD PTR ds:[mh_flags]	; save overlay flag low word
	and ax,14h
	stosw
	mov ax,cs:justalloc_seg	; get just allocated segdef block (zero if none)
	stosw
	mov ax,cs:justalloc_grp	; get just allocated grpdef block (zero if none)
	stosw

	mov ds,cs:extlist_seg
	xor si,si				; ds:si -> stored extern list
	rep movsw				; transfer list

	inc cs:mods_added		; bump count of modules added to program

; zero out file position to show module was processed
	mov es,cs:filepos_seg
	mov di,cs:current_mod
	dec di					; make relative 0
	shl di,1				; convert to dword offset
	shl di,1
	xor ax,ax
	stosw
	stosw
	mov ax,cs:lowalloc_base
	mov cs:load_base,ax		; update file load base beyond new allocations (if any)
	pop bx					; restore critical register
	ret
add_module  ENDP

;*****************************
;* READ_MOD_HEADER           *
;*****************************

; read module header of current module (current_mod)
; AND binary file position table following
; upon entry bx== file handle
; returns ds -> module header data
; update currmod_fpos variable
; destroys ax,cx,dx,ds

read_mod_header PROC    NEAR
	mov ds,cs:filepos_seg
	mov si,cs:current_mod
	dec si					; make relative 0
	shl si,1				; convert to dword offset
	shl si,1
	lodsw					; get low word of file position
	mov WORD PTR cs:currmod_fpos,ax	; save it
	mov dx,ax				; get low word of module position
	lodsw
	mov WORD PTR cs:currmod_fpos+2,ax	; save it
	mov cx,ax				; get high word of module position
	mov ax,4200h			; move file pointer, absolute position
	int 21h

	mov ds,cs:modhdr_seg	; ds -> module header storage area
	xor dx,dx
	mov cx,MOD_HEADER_SIZE	; bytes to read
	mov ah,3fh				; read from file
	int 21h
	jnc rmh_ret

rmh_todoserr:
	jmp NEAR PTR access_error	; error occurred

rmh_ret:
	mov ds,cs:modhdr_seg	; ds -> module header storage area
	ret
read_mod_header ENDP

;*****************************
;* COMPUTE_LOAD_BASE         *
;*****************************

; update load_base past all possible low memory allocations for pass 1 loading
; (lowalloc_base+(mh_pubdef-mh_lnames+lnames blocksize)/16)
; upon entry ds -> module header data
; destroys ax,dx

compute_load_base   PROC    NEAR
	mov dx,WORD PTR ds:[mh_pubdef]
	mov ax,WORD PTR ds:[mh_pubdef+2]	; get offset to pubdefs in ax:dx
	sub dx,WORD PTR ds:[mh_lnames]
	sbb ax,WORD PTR ds:[mh_lnames+2]	; compute maximum new allocation amount
	add dx,LNAMES_BLK_SIZE*16	; add in 1 lnames block allocation in case of boundary crossing
	adc ax,0
	add dx,15				; round up to next para
	adc ax,0
	shr ax,1
	rcr dx,1				; /2
	shr ax,1
	rcr dx,1				; /4
	shr ax,1
	rcr dx,1				; /8
	shr ax,1
	rcr dx,1				; /16
	mov ax,cs:lowalloc_base
	add ax,dx				; compute new file load base
	mov cs:load_base,ax
	ret
compute_load_base   ENDP

;*****************************
;* READ_MOD_NONBINARY        *
;*****************************

; read nonbinary module information
; including lnames, symbol names, segdef, grpdef, pubdef, comdef, extdef info
; upon entry ds -> module header info

read_mod_nonbinary  PROC    NEAR

; get bytes to read in di:si (binary file position start -> binary start)
	mov si,WORD PTR ds:[mh_binary]
	mov di,WORD PTR ds:[mh_binary+2]
	sub si,WORD PTR ds:[mh_binfpos]
	sbb di,WORD PTR ds:[mh_binfpos+2]
	xor dx,dx
	mov ds,cs:load_base		; ds:dx -> load area in low memory
	call    read_file

	ret
read_mod_nonbinary  ENDP

;*****************************
;* GET_DGROUP_PTR            *
;*****************************

; check all group entries for DGROUP name, if match then keep
; pointer to it in dgroup_ptr
; upon entry cx holds count of group entries, es -> group entry block
; destroys cx,si,di,ds

get_dgroup_ptr  PROC    NEAR
	mov di,16

gdp_loop:
	mov si,es:[di+4]
	mov ds,es:[di+6]		; ds:si -> group name lnames entry
	add si,8				; adjust past 2 dword pointers
	cmp BYTE PTR [si],'D'	; check for DGROUP
	jne gdp_fail
	cmp BYTE PTR [si+1],'G'
	jne gdp_fail
	cmp BYTE PTR [si+2],'R'
	jne gdp_fail
	cmp BYTE PTR [si+3],'O'
	jne gdp_fail
	cmp BYTE PTR [si+4],'U'
	jne gdp_fail
	cmp BYTE PTR [si+5],'P'
	jne gdp_fail
	cmp BYTE PTR [si+6],0
	jne gdp_fail

; successful match
	mov cl,4
	shr di,cl				; convert di to paragraphs
	mov ax,es
	add ax,di				; ax -> group entry segment
	mov cs:dgroup_ptr,ax
	ret

gdp_fail:
	add di,16				; move to next group entry
	loop    gdp_loop
	ret
get_dgroup_ptr  ENDP

;*****************************
;* ADD_EXTDEF                *
;*****************************

; add external entries and symbol names (comdefs too for extern list)
; upon entry ds:si -> new extdef entry
; extdef entries consist of 3 words, name ptr low, name ptr high, 2 flag bytes

add_extdef  PROC    NEAR
	mov cs:curr_extdef,0	; init the current extdef number

ae_publoop:
	mov di,ds:[si]			; get offset to symbol name in block
	add di,cs:symoff_ptr
	mov dx,di				; save -> symbol name
	mov es,cs:symseg_ptr	; es:di -> extdef symbol name
	push    cx				; save count of publics
	push    ds				; save -> new pubdef
	push    si

	mov cl,2				; init flag to no parent entries (first extdef)

	mov bx,di				; save -> start of name
	xor	ah,ah
	mov al,es:[di]			; get length byte of name
	sub	ax,2
	add	di,ax				; di -> last char of name before null terminator
	add	al,es:[di]			; add to hash code
	mov	di,bx				; restore di -> name
	add	al,es:[di+2]		; add in second char of name, after length byte & first char
	and	al,31				; convert to hash code

	shl al,1				; convert to word offset
	mov bx,ax
	mov ax,cs:[bx+OFFSET publics_seg]	; get starting entry, if any

	or  ax,ax				; see if any publics stored yet
	je  ae_newpub			; no, automatic save of extdef entry and name
	mov ds,ax
	mov bx,ax				; save -> current public entry matching against
	lds si,ds:[4]			; ds:si -> current extdef entry name

; es:di -> new extdef name, ds:si -> current public name
	mov dx,di				; save -> start of new name

ae_bploop:
	cmpsb					; compare length bytes
	jne	ae_fail

; length bytes matched
	push	cx				; save offset end byte
	mov	cl,[si-1]
	sub	cl,2				; name bytes-length byte and null terminator
	xor	ch,ch				; cx holds remaining name bytes
	repe	cmpsb			; compare the names
	pop	cx					; restore offset end byte
	jne	ae_fail				; no match
	jmp	NEAR PTR ae_pubmatch	; public names match

; byte in names did not match	
ae_fail:
	mov di,dx				; restore di -> start of name entry
	mov	si,10				; setup for old name greater than new name
	mov cl,1				; init flag to old > new update low pointer
	jc  ae_pubshared		; old name greater than new name

; new name greater than old name
	xor cl,cl				; flag new > old, update high pointer
	mov	si,12				; si -> segment of pubdef entry having greater name

ae_pubshared:
	mov ds,bx				; ds -> owning pubdef entry of old name
	mov ax,[si]				; get segment of pubdef entry have lesser or greater name
	or  ax,ax				; see if exists
	je  ae_newpub			; no
	mov ds,ax
	mov bx,ax				; save -> current pubdef entry matching against
	lds si,ds:[4]			; ds:si -> current pubdef entry name
	jmp SHORT ae_bploop		; check next pubdef entry

; unique extdef
; bx -> parent entry, es:dx -> new name
; cl==0 if new name > old name, ==1 if old name > new name, ==2 if no parent
ae_newpub:
	mov ax,es

; save name entry in low memory
	mov di,bp				; get starting offset above low allocation base
	mov es,cs:lowalloc_base	; es:di -> destination for new name
	mov si,dx
	mov ds,ax				; ds:si -> new name
	mov dx,di				; save starting offset of new name

; first byte of name is length of name
	mov	al,cl				; save cl value
	mov	cl,[si]				; get number of bytes to transfer
	xor	ch,ch				; zap high byte
	add	bp,cx				; bump total char count/offset
    shr cx,1                ; convert to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
	mov	cl,al				; restore cl value

;***ae_nameloop:
;***	movsb
;***	inc bp					; bump total char count/offset
;***	mov al,[si-1]
;***	or  al,al				; see if null terminator transferred
;***	jne ae_nameloop			; not yet

	mov si,dx				; si -> new name offset

	xor	ah,ah
	mov al,es:[si]			; get length byte of name
	sub	ax,2
	add	si,ax				; si -> last char of name before null terminator
	add	al,es:[si]			; add to hash code
	mov	si,dx				; restore si -> name offset
	add	al,es:[si+2]		; add in second char of name, after length byte & first char
	and	al,31				; convert to hash code

	shl al,1				; convert to word offset
	mov si,ax
	mov di,cs:[si+OFFSET publics_seg]	; get starting entry, if any

	mov ax,cs:hialloc_base	; get high memory allocation
	dec ax					; drop 1 para for new pubdef entry
	mov cs:hialloc_base,ax

	or  di,di				; see if first public entry for hash code saved yet
	jne ae_restore			; yes
	mov cs:[si+OFFSET publics_seg],ax	; no, save it

ae_restore:
	pop si
	pop ds					; restore ds:si -> new pubdef entry

	mov ds:[si],dx			; save new name offset
	mov ds:[si+2],es		; save new name segment

	add WORD PTR cs:total_sym_count,1	; bump count of total nonlocal symbols
	adc WORD PTR cs:total_sym_count+2,0	; carry to high word

	xor ch,ch
	cmp cl,2				; see if parent entry
	je  ae_addpublic		; no
	mov es,bx				; es -> parent entry
	jcxz    ae_updhigh		; update high pointer of parent entry

; update low pointer of parent entry
	mov es:[10],ax
	jmp SHORT ae_addpublic

; update high pointer of parent entry
ae_updhigh:
	mov es:[12],ax

; store new public in new high memory allocation
; dx -> new public entry
ae_addpublic:
	mov es,ax
	mov di,4
	movsw					; save name ptr low word
	movsw					; save name ptr high word
	xor ax,ax
	mov di,10
	stosw					; zero out low pointer
	stosw					; zero out high pointer
	movsw					; save flag bytes
	sub si,6				; restore si to start
	mov bx,es				; save -> new entry in bx
	jmp SHORT ae_nextpub	; check next extdef entry

; public name matches a previous one
; bx -> matching entry
ae_pubmatch:
	pop si
	pop ds					; restore ds:si -> new pubdef entry

ae_nextpub:
	pop cx					; restore extdef count

; add pointer to extdef/comdef in extern list, bx -> entry
	mov es,cs:extlist_seg
	mov di,cs:curr_extdef
	add di,di				; 2 bytes/entry
	mov ax,bx
	stosw					; save extern pointer

	inc cs:curr_extdef		; bump count of current extdef
	dec cx					; drop count of extdefs
	jcxz    ae_ret			; no more to extdefs to parse
	add si,6				; move to next extdef entry
	jmp NEAR PTR ae_publoop

ae_ret:
	ret
add_extdef  ENDP

;*****************************
;* ADD_PUBCOM                *
;*****************************

; add or update public and communal entries and symbol names
; upon entry ds:si -> new public entry
; cx holds count of publics
; destroys ax,bx,dx,di

add_pubcom  PROC    NEAR

am_publoop:
	mov di,ds:[si+4]		; get offset to symbol name in block
	add di,cs:symoff_ptr
	mov dx,di				; save -> symbol name
	mov es,cs:symseg_ptr	; es:di -> pubdef symbol name
	push    cx				; save count of publics
	push    ds				; save -> new pubdef

ap_altent:
	push    si
	mov cl,2				; init flag to no parent entries (first pubdef)

	mov bx,di				; save -> start of new name
	xor	ah,ah
	mov al,es:[di]			; get length byte of name
	sub	ax,2
	add	di,ax				; di -> last char of name before null terminator
	add	al,es:[di]			; add to hash code
	mov	di,bx				; restore di -> name
	add	al,es:[di+2]		; add in second char of name, after length byte & first char
	and	al,31				; convert to hash code

	shl al,1				; convert to word offset
	mov bx,ax
	mov ax,cs:[bx+OFFSET publics_seg]	; get starting entry, if any

	or  ax,ax				; see if any publics stored yet
	je  am_newpub			; no, automatic save of public entry and name
	mov ds,ax
	mov bx,ax				; save -> current pubdef entry matching against
	lds si,ds:[4]			; ds:si -> current pubdef entry name

; es:di -> new pubdef name, ds:si -> current pubdef name
	mov dx,di				; save -> start of new name

am_bploop:
	cmpsb					; compare length bytes
	jne	am_fail

; length bytes matched
	push	cx				; save offset end byte
	mov	cl,[si-1]
	sub	cl,2				; name bytes-length byte and null terminator
	xor	ch,ch				; cx holds remaining name bytes
	repe	cmpsb			; compare the names
	pop	cx					; restore offset end byte
	jne	am_fail				; no match
	jmp	NEAR PTR am_pubmatch	; public names match

; byte in names did not match	
am_fail:
	mov di,dx				; restore di -> start of name entry
	mov	si,10				; setup for old name greater than new name
	mov cl,1				; init flag to old > new update low pointer
	jc  am_pubshared		; old name greater than new name

; new name greater than old name
	xor cl,cl				; flag new > old, update high pointer
	mov	si,12				; si -> segment of pubdef entry having greater name

am_pubshared:
	mov ds,bx				; ds -> owning pubdef entry of old name
	mov ax,[si]				; get segment of pubdef entry have lesser or greater name
	or  ax,ax				; see if exists
	je  am_newpub			; no
	mov ds,ax
	mov bx,ax				; save -> current pubdef entry matching against
	lds si,ds:[4]			; ds:si -> current pubdef entry name
	jmp SHORT am_bploop		; check next pubdef entry

; _edata/_end not found
am_notfound:
	pop si
	xor al,al				; return al==0 if not found
	ret

; unique pubdef
; bx -> parent entry, es:dx -> new name
; cl==0 if new name > old name, ==1 if old name > new name, ==2 if no parent
am_newpub:
	cmp cs:search_flag,0	; see if searching for _edata/_end
	jne am_notfound			; yes
	mov ax,es

; save name entry in low memory
	mov di,bp				; get starting offset above low allocation base
	mov es,cs:lowalloc_base	; es:di -> destination for new name
	mov si,dx
	mov ds,ax				; ds:si -> new name
	mov dx,di				; save starting offset of new name

; first byte of name is length of name
	mov	al,cl				; save cl value
	mov	cl,[si]				; get number of bytes to transfer
	xor	ch,ch				; zap high byte
	add	bp,cx				; bump total char count/offset
    shr cx,1                ; convert to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
	mov	cl,al				; restore cl value

;***am_nameloop:
;***	movsb
;***	inc bp					; bump total char count/offset
;***	cmp al,[si-1]			; see if null terminator transferred
;***	jne am_nameloop			; not yet

	mov si,dx				; si -> new name offset

	xor	ah,ah
	mov al,es:[si]			; get length byte of name
	sub	ax,2
	add	si,ax				; di -> last char of name before null terminator
	add	al,es:[si]			; add to hash code
	mov	si,dx				; restore si -> name
	add	al,es:[si+2]		; add in second char of name, after length byte & first char
	and	al,31				; convert to hash code

	shl al,1				; convert to word offset
	mov si,ax
	mov di,cs:[si+OFFSET publics_seg]	; get starting entry, if any

	mov ax,cs:hialloc_base	; get high memory allocation
	dec ax					; drop 1 para for new pubdef entry
	mov cs:hialloc_base,ax

	or  di,di				; see if first public entry for hash code saved yet
	jne am_restore			; yes
	mov cs:[si+OFFSET publics_seg],ax	; no, save it

am_restore:
	pop si
	pop ds					; restore ds:si -> new pubdef entry

	mov ds:[si+4],dx		; save new name offset
	mov ds:[si+6],es		; save new name segment
	mov WORD PTR ds:[si+12],0	; zero out high pointer

	mov dx,si				; save start of new pubdef entry
	mov ch,ds:[si+14]
	and ch,3				; get definition flag

am_bumptot:
	add WORD PTR cs:total_sym_count,1	; bump count of total nonlocal symbols
	adc WORD PTR cs:total_sym_count+2,0	; carry to high word
	cmp ch,2				; see if public/absolute
	jae am_newres			; yes

; extdef or comdef, check
	test    BYTE PTR ds:[si+15],40h	; see if comdef
	je  am_chkpar			; no, extdef, no resolution

am_newres:
	add WORD PTR cs:res_sym_count,1		; bump count of resolved nonlocal symbols
	adc WORD PTR cs:res_sym_count+2,0	; carry to high word

; update segment index to segment pointer (only valid if pubdef)
am_indptr:
	mov di,ds:[si]			; get segment index
	add di,di				; 2 paras/segdef entry
	dec di					; adjust for system info
	add di,cs:curr_segblk
	mov ds:[si],di			; save it

am_chkpar:
	xor ch,ch
	cmp cl,2				; see if parent entry
	je  am_addpubdef		; no
	mov es,bx				; es -> parent entry
	jcxz    am_updhigh		; update high pointer of parent entry

; update low pointer of parent entry
	mov es:[10],ax
	jmp SHORT am_addpubdef

; update high pointer of parent entry
am_updhigh:
	mov es:[12],ax

; store new pubdef in new high memory allocation
; dx -> new pubdef entry
am_addpubdef:
	mov es,ax
	xor di,di				; es:di -> storage
	mov ds:[si+10],di		; zero out low pointer (high may be properly set)
	mov cx,8				; move 8 words
	rep movsw
	mov si,dx				; restore si -> new pubdef entry

to_am_nextpub:
	jmp NEAR PTR am_nextpub	; check next pubdef entry

; _edata/_end was found
am_found:
	pop si
	mov al,1				; return al != 0 if found
	ret

; public name matches a previous one
; bx -> matching entry
am_pubmatch:
	cmp cs:search_flag,0	; see if searching for _edata/_end
	jne am_found			; yes
	pop si
	pop ds					; restore ds:si -> new pubdef entry

; new pubdef is nonlocal, search for nonlocal match
am_globsearch:
	mov es,bx				; es -> matched public entry

; nonlocal match
am_nonloc:
	mov al,es:[14]			; get old public definition flag
	and al,3				; mask off extraneous bits
	cmp al,2				; see if old is public or absolute
	jae to_am_nextpub		; yes, ignore it

; old is comdef or extdef
	mov al,ds:[si+14]		; get new public definition flag
	and al,3				; mask off extraneous bits
	cmp al,2				; see if public or absolute
	jb  am_newcom			; no

; new is public, check if old is extdef (bump resolved symbols) or comdef (don't)
	test    BYTE PTR es:[15],40h	; see if old is comdef
	je  am_bumpres			; no, extdef, bump resolved symbols and overwrite the entry
	jmp SHORT am_indptr2	; comdef, just overwrite entry

; new is comdef
; ds -> new, es -> old
am_newcom:
	test    BYTE PTR es:[15],40h	; see if old is comdef
	je  am_bumpres			; no, old was extdef, bump resolved symbols and overwrite the entry

; old is comdef, keep highest communal length
	mov ax,es:[2]			; get old high word communal length
	cmp ax,ds:[si+2]		; see if exceed new length
	ja  to_am_nextpub		; yes, ignore
	jb  am_overwrite		; no, below, update with new
	mov ax,es:[8]			; get old low word communal length
	cmp ax,ds:[si+8]		; see if exceed new
	jb  am_overwrite		; no, below
	jmp NEAR PTR am_nextpub	; exceeds or matches, ignore

; no nonlocal (or local) match, save pubdef entry
; update chain bit and next pointer in old final link
; es -> old final link
am_nomatch:
	or  BYTE PTR es:[15],2	; set chain bit of old final link
	mov ax,es:[12]			; get high pointer
	mov ds:[si+12],ax		; update new entry with old high pointer

	mov ax,cs:hialloc_base	; get high memory allocation
	dec ax					; drop 1 para for new pubdef entry
	mov cs:hialloc_base,ax
	mov es:[12],ax			; chain old final link to new pubdef

	mov dx,es:[4]			; get old name pointer offset
	mov ds:[si+4],dx		; save it in new
	mov dx,es:[6]			; get old name pointer segment
	mov ds:[si+6],dx		; save it in new
	mov dx,si				; save start of new pubdef entry
	jmp NEAR PTR am_addpubdef	; save the new pubdef entry

; bump the count of resolved symbols
am_bumpres:
	add WORD PTR cs:res_sym_count,1		; bump count of resolved nonlocal symbols
	adc WORD PTR cs:res_sym_count+2,0	; carry to high word

; update segment index to segment pointer (only valid if pubdef)
am_indptr2:
	mov ax,ds:[si]			; get segment index
	add ax,ax				; 2 paras/segdef entry
	dec ax					; adjust for system info
	add ax,cs:curr_segblk
	mov ds:[si],ax			; save it

; overwrite old entry
; ds:si -> new, es:0 -> old
; update word offsets at [0],[2],[8],[14]
am_overwrite:
	xor di,di
	movsw					; update [0]
	movsw					; update [2]
	sub si,4				; restore si
	mov ax,ds:[si+8]
	mov es:[8],ax
	mov ax,ds:[si+14]
	mov es:[14],ax

am_nextpub:
	pop cx					; restore pubdef count
	dec cx					; drop count of pubdefs
	jcxz    ap_ret			; no more to pubdefs to parse
	add si,16				; move to next pubdef entry
	jmp NEAR PTR am_publoop

ap_ret:
	ret
add_pubcom  ENDP

;*****************************
;* FILEPOS_TO_SEGOFF         *
;*****************************

; convert module file position to canonical memory segment:offset
; upon entry ax:dx contain file position
; return ds:si -> memory segment:offset
; destroys ax,dx,si,ds

filepos_to_segoff   PROC    NEAR
	sub dx,MOD_HEADER_SIZE	; adjust for module header
	sbb ax,0

; alternate entry to compute file position if no module header adjustment
filepos_alt_ent:
	mov si,dx				; save low word
	shr ax,1
	rcr dx,1				; /2
	shr ax,1
	rcr dx,1				; /4
	shr ax,1
	rcr dx,1				; /8
	shr ax,1
	rcr dx,1				; /16
	mov ax,cs:load_base		; ax -> load area
	add ax,dx				; ax -> location within load area
	and si,0fh				; get offset
	mov ds,ax				; ds:si -> memory location corresponding to file position
	ret
filepos_to_segoff   ENDP

;*****************************
;* RESOLVE_INFO              *
;*****************************

; resolve segment order, update lengths, update extern lists to -> public entries,
; and resolve group entries
; destroys ax,bx,cx,dx,si,di,bp,ds,es

resolve_info    PROC    NEAR
	mov cs:res_occurred,0	; reset resolution flag

; step through all grpdef entries, make matching ones point back to first
rig_maingrp:
	mov ax,cs:grpdef_seg
	or  ax,ax				; see if grpdef block exists
	je  to_ris_resstart		; no

rig_grpblk:
	mov dx,ax				; save -> grpdef block system
	mov ds,ax				; ds -> grpdef block
	mov cx,ds:[2]			; get count of entries in block

rig_grploop:
	inc ax
	mov ds,ax				; ds -> grpdef block entry
	xor ax,ax
	cmp ds:[12],al			; see if unresolved grpdef
	jne rig_nextent			; no, try next entry

	cmp cs:res_occurred,al	; see if looking for new grpdef master entry
	jne rig_slave			; no

	mov ax,ds
	mov es,ax				; es -> grpdef master entry
	mov al,1
	mov BYTE PTR ds:[12],al	; flag that grpdef was resolved
	mov res_occurred,al		; set resolution occurred flag (have master entry)

rig_nextent:
	mov ax,ds				; setup ax for next entry update
	loop    rig_grploop		; check it
	mov ds,dx				; ds -> grpdef block
	mov ax,ds:[0]			; get pointer to next block
	or  ax,ax				; see if exists
	jne rig_grpblk			; yes, check it

; no more grpdef entries, if no resolution this pass then done, if resolution
; then zero resolution flag and look from start
	xor al,al
	cmp cs:res_occurred,al	; see if complete
	je  ris_segdef			; yes
	mov cs:res_occurred,al	; zero out resolution flag
	jmp SHORT rig_maingrp	; loop back to start

; already have grpdef master entry, look for slave entry
rig_slave:
	mov ax,es:[4]			; get master name offset
	cmp ax,ds:[4]			; compare to current entry
	jne rig_nextent			; not equal, not a slave
	mov ax,es:[6]			; get master name segment
	cmp ax,ds:[6]			; compare  to current entry
	jne rig_nextent			; not equal, not a slave

; name pointers match, this is a slave to the master grpdef
	mov ds:[10],es			; update entry to point to master grpdef
	mov BYTE PTR ds:[12],1	; flag that grpdef was resolved
	jmp SHORT rig_nextent

; step through segdef entries, updating grpdef index to point to grpdef
ris_segdef:
	mov ax,cs:grpdef_seg
	mov bp,cs:segdef_seg
	or  ax,bp				; see if segdefs and grpdefs exist

; vector to rsi_resstart IF the zero flag is set
to_ris_resstart:
	je  rsi_resstart		; no

ris_segblk:
	mov dx,bp				; save -> segdef block system
	mov es,bp				; es -> segdef block
	mov cx,es:[2]			; get count of entries in block
	mov ax,cs:grpdef_seg

ris_grploop:
	mov ds,ax				; ds -> grpdef block
	mov ax,ds:[4]			; get library number
	cmp es:[4],ax			; see if matches
	jne ris_findgrp			; no
	mov ax,ds:[6]			; get module id
	cmp es:[6],ax			; see if matches
	je  ris_segloop			; yes

ris_findgrp:
	mov ax,ds:[0]			; get pointer to next
	or  ax,ax				; see if at end block
	je  ris_nextblk			; yes, match not found
	jmp SHORT ris_grploop

rsi_ret:
	ret

; ds -> matching group block, es -> segdef block
ris_segloop:
	inc bp
	mov es,bp				; es -> segdef block entry
	mov si,es:[16]			; get group index, if any
	or  si,si				; see if exists
	je  ris_nextent			; no
	mov ax,ds
	add ax,si				; ax -> proper group entry
	mov es:[16],ax			; update entry

ris_nextent:
	inc bp					; adjust for half a segdef entry
	loop    ris_segloop		; check next entry

ris_nextblk:
	mov es,dx				; es -> segdef block
	mov bp,es:[0]			; get pointer to next block
	or  bp,bp				; see if exists
	jne ris_segblk			; yes, check it

; resolve segment order and update start and length of segments
rsi_resstart:
	call    update_seggrps	; update any segment members of groups in other modules
	mov ax,cs:total_seg_count
	mov cs:unresolved_segs,ax	; init count of unresolved address segments

	xor ax,ax
	jmp SHORT rsi_resloop

; get current segment class type, save it
rsi_getclass:
	call    get_class_type	; get class type, returned in al
	mov ah,al
	inc ah					; make relative 1
	shl ah,1
	shl ah,1
	or  ds:[27],ah			; save type in bits 2-4
	jmp NEAR PTR rsi_chkphase	; see if proper type

; oh the things we have to do to optimize cycle-munching loops
to_rsi_getclass:
	jmp SHORT rsi_getclass

rsi_resloop:
	cmp cs:unresolved_segs,ax	; check if all segments have been resolved
	je  rsi_ret				; yes

;***	cmp cs:res_occurred,al	; see if nonprivate segment resolved this pass
;***	jne rsi_res3			; yes
	cmp cs:segname_seg,ax	; see if valid segment name to match
	mov cs:segname_seg,ax	; zero out segment name to match, KEEP FLAG
	jne rsi_res3			; was valid segment name, keep class pointer
	mov cs:classname_seg,ax	; no segment for last class, zero class pointer
	cmp cs:nomatch_flag,al	; see if no matches last resolution pass
	mov cs:nomatch_flag,1	; set no match flag, KEEP FLAGS FROM CMP
	je  rsi_res3			; previous pass had resolution
	inc cs:phase			; bump phase of segment ordering (for DOSSEG)
	mov cs:nomatch_flag,al	; reset no matches flag

rsi_res3:
	mov cs:res_occurred,al	; reset segment resolution flag
	mov di,ax				; init entry number
	mov si,cs:segdef_seg	; init current block to segdef block
	mov	ds,si
	cmp	WORD PTR ds:[10],0	; see if any unresolveds in block
	je	rsi_nextblk			; no

	dec di					; drop one for next_entry increment

rsi_next_entry:
	inc di					; bump to next segdef entry

rsi_segblk_loop:
	mov ds,si
	cmp di,ds:[2]			; check if any more entries in block
	jb  rsi_4				; yes

rsi_nextblk:
	mov ax,ds:[0]			; point to next block
	or  ax,ax				; check if exists
	je  rsi_resloop			; no
	mov si,ax				; save -> current block
	mov ds,ax
	xor di,di				; reinit entry number
	cmp	ds:[10],di			; see if any unresolveds in block (di==0)
	je	rsi_nextblk			; no

; get an entry from the current block
rsi_4:
	mov bx,di				; entry number
	shl bx,1				; each entry takes up two paragraphs
	mov ax,ds				; get block segment address
	add bx,ax				; get entry's segment value
	inc bx					; adjust for block system info size of 1 paragraph
	mov ds,bx				; ds -> segdef entry

	mov al,ds:[27]
	mov ah,al				; save it
	and al,1				; check segment resolution flag
	jne rsi_next_entry		; segment already resolved

	cmp cs:dosseg_flag,al	; see if DOSSEG ordering
	je  rsi_chkclass		; no

	mov al,ah				; get flags from ds:[27]
	and al,1ch				; get phase+1
	je  to_rsi_getclass		; not computed yet
	shr al,1
	shr al,1				; make relative 1
	dec al					; make relative 0

rsi_chkphase:
	cmp al,cs:phase			; see if phase matches type
	jne rsi_next_entry		; no

rsi_chkclass:
	xor ax,ax
	cmp cs:classname_seg,ax	; see if a class name has been selected yet
	jne rsi_gotclass		; yes

; no class selected, use this entry's
	mov cs:master_segdef,ax	; reset master segdef entry
	mov ax,ds:[12]
	mov cs:classname_off,ax	; save segment name offset
	mov ax,ds:[14]
	mov cs:classname_seg,ax	; save segment name segment

; see if this segment class matches selected class
rsi_gotclass:
	mov ax,ds:[12]			; compare class name offset
	cmp cs:classname_off,ax
	jne rsi_next_entry		; no match
	mov ax,ds:[14]			; compare class name segment
	cmp cs:classname_seg,ax
	jne rsi_next_entry		; no match

	xor ax,ax
	cmp cs:segname_seg,ax	; see if a segment name has been selected yet
	jne rsi_gotseg			; yes

; no segment name selected, use this entry's
	mov cs:master_segdef,ax	; reset master segdef entry
	mov ax,ds:[8]
	mov cs:segname_off,ax	; save segment name offset
	mov ax,ds:[10]
	mov cs:segname_seg,ax	; save segment name segment

; see if the segment name matches
rsi_gotseg:
	mov ax,ds:[8]			; compare segment name offset
	cmp cs:segname_off,ax
	jne to_rsi_next_entry	; no match, try next entry
	mov ax,ds:[10]			; compare segment name segment
	cmp cs:segname_seg,ax
	jne to_rsi_next_entry	; no match

	mov dl,ds:[26]			; dl holds acbp byte
	mov dh,dl
	and dh,1ch				; get combine field
	je  rsi_private			; zero if private
	cmp cs:res_occurred,0	; see if nonprivate resolution occurred previous pass
	mov cs:res_occurred,1	; flag that a nonprivate segment was resolved this pass, KEEP FLAGS
	je  rsi_newmaster		; no nonprivate resolution occurred previous pass
	mov ax,cs:master_segdef
	or  ax,ax				; see if a master segdef entry exists
	je  rsi_newmaster		; no

; master segdef entry exists for this segdef, keep pointer to it
	mov ds:[30],ax
	jmp SHORT rsi_resit

; private segment, see if resolution occurred previous pass (don't combine private with public)
rsi_private:
	xor ax,ax
	cmp cs:res_occurred,al	; see if nonprivate resolution occurred previous pass
	jne to_rsi_next_entry	; yes, don't match this segment
	mov cs:master_segdef,ax	; zero out the master segdef entry
	mov cs:segname_seg,ax	; zero out segment name so two privates in a row with same name don't match
	jmp SHORT rsi_resit

; set the master segdef entry
rsi_newmaster:
	mov cs:master_segdef,ds	; keep pointer to new master segdef entry

rsi_resit:
	mov dh,dl
	and dh,0e0h				; dh holds align field
	or  BYTE PTR ds:[27],1	; flag that this segment was resolved
	mov cs:nomatch_flag,0	; reset no matches flag
	dec cs:unresolved_segs	; decrement number of unresolved segments
	inc cs:res_seg_count	; bump count of resolved segments
	mov ax,cs:res_seg_count	; get count
	mov ds:[24],ax			; save resolution order

; see if overlaid segment, if so bypass length updates, group checks
	mov al,ds:[28]
	and al,14h
	jne rsi_res_go_next		; overlaid segment

	or  dh,dh				; check if absolute frame address, align field==0
	jne rsi_chkcom			; not absolute, resolve segment

; absolute segment, do not resolve segment
rsi_res_go_next:
	mov	ds,si				; ds -> block
	dec	WORD PTR ds:[10]	; drop count of unresolveds in block

to_rsi_next_entry:
	jmp NEAR PTR rsi_next_entry	; bump entry count and reloop

; not an absolute frame address resolve segment address
rsi_chkcom:
	xor cx,cx				; init master pointer, nonzero if common combined slaved segdef
	mov al,dl
	and al,1ch				; get combine field
	cmp al,18h				; see if common combine
	jne rsi_notcom			; no
	cmp WORD PTR ds:[30],0	; see if slaved
	je  rsi_notcom			; no

; common combined slaved segdef entry
	mov bx,ds
	mov cx,ds:[30]			; get master entry
	mov ds,cx				; es -> master entry
	mov ax,ds:[2]			; get real common segment start
	mov WORD PTR cs:segment_start,ax
	mov ax,ds:[4]
	mov WORD PTR cs:segment_start+2,ax
	mov ds,bx

rsi_notcom:
	mov ah,BYTE PTR cs:segment_start	; get low byte of segment start (modulo 256)
	xor al,al
	cmp dh,80h				; check if page aligned
	jne rsi_9				; no
	sub al,ah				; get page alignment adjustment in al
	jmp SHORT rsi_adj_segstart	; bypass remaining alignment adjustment code

rsi_9:
	cmp dh,60h				; check if paragraph aligned
	jne rsi_10				; no
	and ah,15
	sub al,ah
	and al,15				; get paragraph alignment adjustment
	jmp SHORT rsi_adj_segstart	; bypass remaining alignment adjustment code

rsi_10:
	cmp dh,40h				; check if word aligned
	jne rsi_savestart		; no, no adjustment to segment start
	and ah,1
	mov al,ah				; get word alignment adjustment

; al holds amount to add to segment start
rsi_adj_segstart:
	xor ah,ah				; zap high byte
	add WORD PTR cs:segment_start,ax	; adjust low word
	adc WORD PTR cs:segment_start+2,0	; add in carry from low word

rsi_savestart:
	mov ax,WORD PTR cs:segment_start
	mov ds:[2],ax			; save segment's start address low word
	mov ax,WORD PTR cs:segment_start+2
	mov ds:[4],ax			; save segment's start address high word

	mov al,dl				; get acbp byte
	and al,2				; check big bit status
	shr al,1				; make byte value from bit either 1 or 0
	xor ah,ah				; zap high byte
	mov bx,ds:[6]			; get segment length from segdef entry
	jcxz    rsi_notcom2		; cx is zero if not a common combine slaved segdef

; common combine slaved segdef
	push    ds
	mov ds,cx
	cmp bx,ds:[6]			; see if current segment is bigger than master common segment
	jbe rsi_mastbig			; no
	mov ds:[6],bx			; update master segment length to longest common segment
	jmp SHORT rsi_11		; and continue

; master segment is bigger than current segment
rsi_mastbig:
	mov bx,ds:[6]			; make bx == longest of common segments

rsi_11:
	pop ds

rsi_notcom2:
	mov WORD PTR cs:true_seg_len,bx	; save low word of segment length
	mov WORD PTR cs:true_seg_len+2,ax	; save high word of segment length
	mov dh,dl				; get acbp byte
	and dh,1ch				; get combine field
	cmp dh,14h				; check if segment is a stack segment
	jne rsi_getstop			; no
	and bl,1				; check if stack segment length is odd
	je  rsi_isstack			; no

; stack segment cannot have odd byte length
	inc WORD PTR ds:[6]		; bump segment length by one
	add WORD PTR cs:true_seg_len,1	; bump true segment length by one byte adjustment
	adc WORD PTR cs:true_seg_len,0	; carry bit to high word

; check program's initial SS:SP value
rsi_isstack:
	mov ax,WORD PTR cs:segment_start	; get low word of start in ax
	mov bx,WORD PTR cs:segment_start+2	; get high word of start in bx
	shr bx,1
	rcr ax,1				; /2
	shr bx,1
	rcr ax,1				; /4
	shr bx,1
	rcr ax,1				; /8
	shr bx,1
	rcr ax,1				; /16

; ax == segment frame paragraph (segment start/16)
	cmp cs:stack_segval,0	; see if stack segment already set
	jne rsi_stackoff		; yes, don't set it again
	mov cs:stack_segval,ax	; save initial SS

rsi_stackoff:
	mov ax,WORD PTR cs:true_seg_len	; get end of stack segment (high word is zero)
	add cs:stack_offval,ax	; save/update initial SP

; compute segment stop address (segment_start+true_seg_len-1)
rsi_getstop:
	mov ax,WORD PTR cs:segment_start
	add ax,WORD PTR cs:true_seg_len	; ax contains low word of sum
	mov bx,WORD PTR cs:segment_start+2
	adc bx,WORD PTR cs:true_seg_len+2	; bx contains high word of sum
	mov cx,ax
	or  cx,bx				; see if segment stop address is zero (zero length starting segment)
	je  rsi_zerolen_seg		; yes, zero the segment stop
	sub ax,1				; subtract one from value
	sbb bx,0				; borrow from high word

; bx:ax == segment stop value
	cmp bx,WORD PTR cs:segment_start+2	; check that segment stop>=segment length in high word
	ja  rsi_newstop			; yes
	je  rsi_14				; maybe, check low word

rsi_zerolen_seg:
	mov bx,WORD PTR cs:segment_start+2	; make segment stop = segment length
	mov ax,WORD PTR cs:segment_start
	jmp SHORT rsi_newstop	; bypass low word check

rsi_14:
	cmp ax,WORD PTR cs:segment_start	; check that segment stop>=segment length in low word
	jb rsi_zerolen_seg		; no, adjust segment stop

rsi_newstop:
	mov WORD PTR cs:segment_stop,ax	; update segment stop variable
	mov WORD PTR cs:segment_stop+2,bx

	mov ax,WORD PTR cs:true_seg_len
	add WORD PTR cs:segment_start,ax	; update low word of segment start (next segment)
	mov ax,WORD PTR cs:true_seg_len+2
	adc WORD PTR cs:segment_start+2,ax	; update high word of segment start (next segment)

	mov ax,ds:[16]
	or  ax,ax				; see if segment has associated group
	je  rsi_15				; nope
	call    update_grp_off	; update group entry's offset, if necessary

rsi_15:
	jmp NEAR PTR rsi_res_go_next	; drop unresolveds in block, try next entry

resolve_info    ENDP

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
; upon entry ds -> segdef entry
; returns class type in al
; destroys ax,bx,cx,dx,ds

get_class_type  PROC    NEAR
	push    si				; save critical register
	push    di
	les di,ds:[12]
	add di,8				; es:di -> class name
	mov bx,di				; bx -> class name
	mov si,di
	xor ax,ax
	mov cx,129

	repne   scasb			; find end of string
	mov cx,di
	sub cx,si				; cx holds count of chars+1
	dec cx

	cmp cx,4				; see if possible class CODE
	jb  gct_nodgroup_chk	; no, name string too small
	mov di,cx				; di will offset into end of string
	sub di,4
	add di,bx				; es:di -> last four chars of class name string

	mov si,OFFSET codetext	; cs:si -> target string
	call    caseless_strcmp	; see if strings match
	or  al,al				; check return value
	jne gct_nodgroup_chk	; not class CODE

gct_ret:
	pop di
	pop si
	ret

; see if segment is NOT in group DGROUP
gct_nodgroup_chk:
	mov ax,ds:[16]			; get group entry segment, if any
	or  ax,ax				; check if entry exists
	jne gct_2				; yes

; no group exists for this segment, not in group DGROUP
gct_not_dgroup:
	mov al,1				; return al ==1 for segment not in DGROUP
	jmp SHORT gct_ret

gct_2:
	mov di,ds				; save -> segdef entry
	mov ds,ax				; ds -> group entry
	mov si,ds:[10]
	or  si,si				; check if master group entry
	je  gct_master			; yes
	mov ax,si				; ax -> master group entry

gct_master:
	cmp ax,cs:dgroup_ptr	; see if group entry matches dgroup
	mov ds,di				; restore ds -> segdef entry
	jne gct_not_dgroup		; no

; see if segment is class BEGDATA
	cmp cx,7				; see if possible class BEGDATA
	jb  gct_bss_chk			; no, name string too small
	mov di,cx				; di will offset into end of string
	sub di,7
	add di,bx				; es:di -> last seven chars of class name string

	mov si,OFFSET begdatatext	; si -> target string
	call    caseless_strcmp	; see if strings match
	or  al,al				; check return value
	jne gct_bss_chk			; nonzero return value, string didn't match
	mov al,2				; return al == 2 for class BEGDATA
	jmp SHORT gct_ret

; see if segment is class BSS
gct_bss_chk:
	cmp cx,3				; see if possible class BSS
	jb  gct_stack_chk		; no, name string too small
	mov di,cx				; di will offset into end of string
	sub di,3
	add di,bx				; es:di -> last three chars of class name string

	mov si,OFFSET bsstext	; si -> target string
	call    caseless_strcmp	; see if strings match
	or  al,al				; check return value
	jne gct_stack_chk		; nonzero return value, string didn't match

	mov cl,4				; save class status of BSS
	cmp cs:bss_flag,0		; see if transition to BSS already occurred
	jne gct_ret_class		; yes
	mov cs:bss_flag,1		; flag that transition has occurred
	mov ax,cs:_edata_ptr
	or  ax,ax				; see if _edata symbol was used
	je  gct_ret_class		; no

gct_sym_update:
	mov es,ax				; es -> symbol
	mov al,es:[14]			; get definition flag
	and al,0fch				; mask off old value
	or  al,2				; set public bit values
	mov es:[14],al
	mov es:[0],ds			; save segment partition pointer
	xor ax,ax
	mov es:[8],ax			; zero out public offset
	cmp cs:dgroup_ptr,ax	; see if a DGROUP to set symbol in
	je  gct_ret_class		; no
	or  BYTE PTR es:[15],80h	; flag group association

gct_ret_class:
	mov al,cl				; return proper class value in al
	jmp NEAR PTR gct_ret

; see if segment is class STACK
gct_stack_chk:
	cmp cx,5				; see if possible class STACK
	jb  gct_other			; no, name string too small
	mov di,cx				; di will offset into end of string
	sub di,5
	add di,bx				; ds:di -> last five chars of class name string

	mov si,OFFSET stacktext	; si -> target string
	call    caseless_strcmp	; see if strings match
	or  al,al				; check return value
	jne gct_other			; nonzero return value, string didn't match

	mov cl,5				; save class status of STACK
	cmp cs:stack_flag,0		; see if transition to STACK already occurred
	jne gct_ret_class		; yes
	mov cs:stack_flag,1		; flag that transition has occurred
	mov ax,cs:_end_ptr
	or  ax,ax				; see if _end symbol was used
	je  gct_ret_class		; no
	jmp SHORT gct_sym_update	; update _end symbol values

; segment is in DGROUP but does NOT have class BEGDATA, BSS, or STACK
gct_other:
	mov al,3				; resturn al == 3 for non- BEGDATA, BSS, and STACK DGROUP segment
	jmp NEAR PTR gct_ret

get_class_type  ENDP

;*****************************
;* CASELESS_STRCMP           *
;*****************************

; caseless string compare, upper and lowercase chars match 
; checks string -> by cs:si against string -> by es:di
; returns al == 0 if match, al == 1 if no match
; destroy ax,di,si

caseless_strcmp PROC    NEAR

cs_comp_loop:
	lods    BYTE PTR cs:0	; get char
	or  al,al				; if zero then at end of string
	je  cs_1				; done with string compare

	cmp al,'a'				; check lowercase lower bound
	jb  cs_3				; no
	cmp al,'z'				; check lowercase upper bound
	ja  cs_3				; no
	sub al,20h				; convert to uppercase

cs_3:
	mov ah,es:[di]			; get target string char
	cmp ah,'a'				; check lowercase lower bound
	jb  cs_4				; no
	cmp ah,'z'				; check lowercase upper bound
	ja  cs_4				; no
	sub ah,20h				; convert to uppercase

cs_4:
	cmp al,ah				; see if chars match
	jne cs_nomatch			; no
	inc di					; this char matches, try next char in string, si already incremented
	jmp SHORT cs_comp_loop	; loop till string complete

cs_1:
	or  al,es:[di]			; merge in second string char, still zero (successful match) if both terminated
	jne cs_nomatch			; not zero, flag unsuccessful match
	ret

cs_nomatch:
	mov al,1				; flag unsuccessful compare
	ret
caseless_strcmp ENDP

;*****************************
;* UPDATE_SEGGRPS            *
;*****************************

; update segment group pointers that are members of groups in other modules
; destroys ax,bx,cx,dx,si,di,bp,ds,es

update_seggrps  PROC    NEAR
	mov ax,cs:segdef_seg

us_mainloop:
	mov ds,ax				; ds -> segdef block
	mov dx,ds				; save -> segdef block
	mov cx,ds:[8]			; get count of unused group check entries in block
	jcxz	us_nextmainblk	; none, move to next segdef block, if any

	inc ax

us_entloop:
	mov ds,ax				; ds -> segdef entry
	mov al,ds:[27]
	and al,2				; see if used group check
	jne	us_nextmainent		; yes

; segment not used yet
	mov	es,dx				; es -> segdef block
	dec	WORD PTR es:[8]		; drop count of unuseds
	dec	cx					; drop running count
	or  BYTE PTR ds:[27],2	; flag that segment has been used for group check

; see if private segment
	mov al,ds:[26]			; get acbp byte
	and al,1ch				; get combine field
	jne	us_unproc			; non-private, unprocessed

; move to next main segdef entry, if any
us_nextmainent:
	jcxz	us_nextmainblk	; no more entries in block
	mov ax,ds
	add ax,2
	jmp SHORT us_entloop

; move to next segdef block, if any
us_nextmainblk:
	mov ds,dx				; restore ds -> main segment block
	mov ax,ds:[0]
	or  ax,ax				; see if another block
	jne	us_mainloop			; yes

us_ret:
	ret

; cycle through segdef pointers updating first with highest align and
; all of them with group pointers
us_updloop:
	push	cx				; save critical register
	mov	es,cs:first_segptr
	mov	es,es:[4]			; es -> first segdef
	mov al,es:[26]
	and al,1fh				; mask off old segdef alignment
	or	al,cs:highest_align	; merge in new highest alignment
	mov	es:[26],al
	mov	ax,cs:first_segptr

us_grpblkloop:
	mov	bx,4				; bx -> segdef block offset
	mov	es,ax				; es -> block
	mov	cl,es:[3]			; get count
	xor	ch,ch				; zap high byte

us_grpentloop:
	mov	si,es				; save -> segdef block
	mov	es,es:[bx]			; es -> segdef
	mov	ax,cs:master_group_ptr
	mov	es:[16],ax			; update group pointer to master
	add	bx,2				; move to next entry in block
	mov	es,si				; es -> segdef block
	loop	us_grpentloop
	mov	ax,es:[0]			; ax -> next block, if any
	cmp	BYTE PTR es:[2],0	; see if full flag set
	jne	us_grpblkloop		; yes, next block must have been used

	pop	cx					; restore critical register
	jmp	SHORT us_nextmainent

; ds -> segdef entry
us_unproc:
	mov	ax,cs:first_segptr	; get current segdef pointer list
	or	ax,ax				; see if previously allocated
	jne	us_ptrinit			; yes, init the pointer list
	mov	ax,cs:lowalloc_base
	inc	cs:lowalloc_base
	mov	cs:first_segptr,ax	; allocate for first segdef pointer list block
	mov	es,ax				; es -> first segdef pointer list block
	mov	WORD PTR es:[0],0	; init next pointer

us_ptrinit:
	mov	cs:current_segptr,ax	; save -> current segdef pointer list block
	mov	es,ax				; es -> first segdef pointer list block
	mov	WORD PTR es:[2],100h	; init block count==1, full flag==0
	mov	ax,ds
	mov	WORD PTR es:[4],ax	; first entry -> first segdef

	mov al,ds:[26]
	and al,0e0h				; al holds segdef alignment
	mov	cs:highest_align,al	; save it as highest known

	mov bx,ds:[16]			; get group entry
	or  bx,bx				; see if member of group
	je  us_retry			; no
	mov es,bx
	mov ax,es:[10]
	or  ax,ax				; see if master group entry
	je  us_retry			; yes
	mov bx,ax				; bx holds master group entry

; cycle through following segdef entries in other blocks checking for name/class match
us_retry:
	mov	cs:master_group_ptr,bx	; init master group pointer
	mov es,dx				; es -> current segdef block
	mov ax,es:[0]			; get next block
	or  ax,ax				; see if exists
	jne	us_chkmain			; yes
	ret						; no

us_chkmain:
	mov es,ax				; es -> next block
	mov di,es				; save -> segdef block
	mov bp,es:[8]			; get count of unused entries in block
	or	bp,bp				; see if zero
	je	us_nextchkblk		; no unused entries, continue
	inc ax

us_chkent:
	mov es,ax				; es -> segdef entry to check
	mov al,es:[27]
	and al,2
	jne us_nextchkent		; already used, get next entry to check

; segment not used yet
	dec	bp					; drop running count
	mov si,8
	mov	ax,[si]
	cmp ax,es:[si]			; check if segment names match, low word
	je  us_chkacbp			; yes

; no match, move to next segdef entry to check main against
us_nextchkent:
	or	bp,bp				; see if more entries to check
	je	us_nextchkblk		; no
	mov ax,es
	add ax,2
	jmp	SHORT us_chkent		; more entries in block

; move to next segdef block to check main against
us_nextchkblk:
	mov es,di				; restore es -> check segment block
	mov ax,es:[0]
	or  ax,ax				; see if another block
	jne us_chkmain			; yes
	jmp NEAR PTR us_updloop	; no

; private segment
us_private:
	or  BYTE PTR es:[27],2	; flag so not checked again
	push	es
	mov	es,di				; es -> segdef block
	dec	WORD PTR es:[8]		; drop count of unuseds
	pop	es
	jmp SHORT us_nextchkent

; segment names low word matches
us_chkacbp:
	mov al,es:[26]			; get acbp byte
	and al,1ch				; get combine field
	je  us_private			; private, don't add to list

	mov	si,10
	lodsw
	cmp ax,es:[10]
	jne us_nextchkent

	lodsw
	cmp ax,es:[12]			; check if class names match
	jne us_nextchkent		; no

	lodsw
	cmp ax,es:[14]
	jne us_nextchkent

; segment/class match and neither segment is private
	or  BYTE PTR es:[27],2	; flag so not checked again

; update highest align check entry's alignment, if higher
	mov al,es:[26]
	and al,0e0h				; al holds segdef alignment
	cmp	al,cs:highest_align	; see if > current highest
	jbe	us_3				; no
	mov	cs:highest_align,al

; save -> to this segdef in segdef pointer list
; es -> segdef
us_3:
	mov	si,es				; si -> segdef
	cmp	cs:master_group_ptr,0	; see if master group for segments
	jne	us_4				; yes
	mov	ax,es:[16]			; get group entry
	or  ax,ax				; see if member of group
	je  us_4				; no
	mov es,ax
	cmp WORD PTR es:[10],0	; see if master group entry
	je  us_savegrp			; no
	mov ax,es:[10]

; ax holds master group entry of check segdef, or zero if none
us_savegrp:
	mov	cs:master_group_ptr,ax	; save it

us_4:
	mov	es,cs:current_segptr
	cmp	BYTE PTR es:[3],6	; see if list block is full
	jb	us_saveseg			; no
	mov	BYTE PTR es:[2],1	; flag full block
	mov	ax,es:[0]			; get -> next block if any
	mov	es,ax				; es -> block
	or	ax,ax				; see if block exists
	jne	us_initcount		; yes, init the count

; must allocate new segdef pointer list block
	mov	es,cs:current_segptr
	mov	ax,cs:lowalloc_base
	inc	cs:lowalloc_base
	mov	es:[0],ax			; old block -> new
	mov	es,ax				; es -> new current segdef pointer list block
	mov	WORD PTR es:[0],0	; init next pointer

us_initcount:
	mov	WORD PTR es:[2],0	; init count in block==0, full flag==0

; es -> current segdef pointer list block
; si -> segdef
us_saveseg:
	mov	cs:current_segptr,es
	mov	al,es:[3]			; get count
	xor	ah,ah				; zap high byte
	xchg	ax,si			; ax -> segdef, si == count
	add	si,2				; adjust for system info in block
	add	si,si				; word offset into block
	mov	es:[si],ax			; save -> segdef
	inc	BYTE PTR es:[3]		; bump count in block
	mov	es,di				; es -> segdef block
	dec	WORD PTR es:[8]		; drop count of unuseds
	jmp NEAR PTR us_nextchkblk	; check next segdef block

update_seggrps  ENDP

;*****************************
;* UPDATE_GRP_OFF            *
;*****************************

; make group offset the lowest address of the segments in the group
; upon entry ds -> segdef entry, ax -> group entry
; destroys ax,es

update_grp_off  PROC    NEAR
	mov es,ax				; es -> group entry
	mov ax,es:[10]			; see if slave grpdef
	or  ax,ax
	je  ugo_1				; master grpdef
	mov es,ax				; es -> master grpdef

ugo_1:
	mov ax,ds:[4]			; get segment offset high word
	cmp ax,es:[2]			; compare to group offset high word
	ja  ugo_ret				; segment offset higher than group offset
	jb  ugo_2				; segment offset lower, update group offset
	mov ax,ds:[2]			; get segment offset low word
	cmp ax,es:[0]			; compare to group offset low word
	jae ugo_ret				; segment offset higher or equal to group offset

ugo_2:
	mov ax,ds:[4]			; get high word
	mov es:[2],ax			; update group offset high word
	mov ax,ds:[2]			; get low word
	mov es:[0],ax			; update group offset low word
	mov ax,ds
	mov es:[8],ax			; save segdef entry pointer of lowest segment

ugo_ret:
	ret
update_grp_off  ENDP

;*****************************
;* READ_FILE                 *
;*****************************

; read from DDL file
; upon entry ds:dx -> load area, dx should not exceed 15 (0fh)
; bx==file handle
; di:si == bytes to read
; destroys ax,cx,dx,di,si,ds

read_file   PROC    NEAR

; check and make sure no overflow into hialloc_base
	xor cx,cx
	mov ax,ds				; get load base
	shl ax,1
	rcl cx,1
	shl ax,1
	rcl cx,1
	shl ax,1
	rcl cx,1
	shl ax,1
	rcl cx,1				; converted to byte count in cx:ax
	add ax,dx				; add in read offset
	adc cx,0				; carry to high word
	add ax,si				; add in low word of bytes to read
	adc cx,0				; carry to high word
	add cx,di				; add in high word of bytes to read
	add ax,15				; round up to next paragraph
	adc cx,0

; convert absolute size in cx:ax to paragraphs
	shr cx,1
	rcr ax,1				; /2
	shr cx,1
	rcr ax,1				; /4
	shr cx,1
	rcr ax,1				; /8
	shr cx,1
	rcr ax,1				; /16
	cmp ax,cs:hialloc_base	; see if overflow into high memory allocation base
	jbe rf_loop				; no

; overlow into allocation base, out of memory
	jmp NEAR PTR mem_error

rf_loop:
	or  di,di				; see if 64K or more bytes to read
	je  rf_2				; no

rf_max:
	mov cx,0fff0h			; read 64K-16 bytes
	jmp SHORT rf_read

rf_2:
	mov cx,si
	cmp cx,0fff0h			; see if more than 64K-16 bytes to read
	ja  rf_max				; yes, use maximum of 64K-16 bytes

rf_read:
	mov ah,3fh				; read from file
	int 21h
	jc  rf_todoserr			; error occurred

	sub si,ax				; subtract off bytes read
	sbb di,0
	mov cx,di
	or  cx,si				; see if any bytes left to read
	je  rf_ret				; no
	mov ax,ds
	add ax,0fffh			; bump to next read segment
	mov ds,ax
	xor dx,dx
	jmp SHORT rf_loop

rf_ret:
	ret

rf_todoserr:
	jmp NEAR PTR access_error	; error occurred

read_file   ENDP

;*****************************
;* OPEN_DDL_FILE             *
;*****************************

; open transient DDL file, using ddl_counter to determine proper file
; from dependency list
; destroys ax,bx,cx,dx,di,si,bp,ds,es
; returns file handle in bx

open_ddl_file   PROC    NEAR
	mov es,cs:prog_psp		; es -> PSP
	mov ds,cs:depend_seg
	mov ax,cs:ddl_counter
	mov dl,80				; 80 bytes/ dependency list entry
	mul dl
	mov dx,ax				; ds:dx -> DDL file name
	mov bp,ax				; save -> DDL file name
	mov ax,3d00h			; open for reading
	int 21h
	jc  not_cwd				; error, not in current working directory
	mov bx,ax				; save file handle in bx
	ret						; no errors

; check if file not found related problem
not_cwd:
	cmp ax,2				; check for file not found error
	je  scan_path			; yes, scan the DDL path for file
	cmp ax,3				; check if path not found or file doesn't exist error
	je  scan_path			; yes
	cmp ax,5				; check for access denied (not in current directory)
	jne to_founderr			; unexpected DOS error

; find DDL file using DDLPATH environment string
; prior to using DDLPATH, if using DOS 3.x+, check the EXE's path that is found
; after the local environment block (argv[0])
scan_path:
	mov ds,cs:load_base		; ds -> temporary storage for fully qualified DDL name
	push    es				; put PSP value on stack
	mov ax,es:[2ch]			; get environment segment from offset 2ch in PSP
	mov es,ax				; es -> environment segment

	mov ah,30h				; get MS-DOS version number
	int 21h
	cmp al,2				; should be 3.x or above
	jbe dos_2x_entry		; no, can't check path of EXE file after environment blockk

; search for end of environment block
	push    es
	push    ds
	pop es					; es -> file name buffer
	pop ds
	xor si,si				; ds:si -> start of environment

end_loop:
	lodsb					; get environment char
	or  al,[si]				; merge in next char
	jne end_loop			; not at end of environment block

	add si,3				; bump si -> start of exe file path
	mov di,si				; save start pointer

path_loop:
	lodsb					; get char of file path
	or  al,al				; see if at end
	je  calc_path			; yes
	cmp al,'\'				; see if directory indicator
	jne path_loop			; no
	mov bx,si				; save -> to char past directory
	jmp SHORT path_loop		; get next char

calc_path:
	mov si,di				; ds:si -> start  of file path
	xor di,di				; es:di -> program name with path prefix slot

calc_loop:
	movsb					; transfer a char
	cmp si,bx				; see if at end of path
	jne calc_loop			; no

	mov bx,ds				; save -> environment block
	push    cs
	pop ds					; ds -> loader data
	mov si,bp				; append program name to end of path prefix

append_loop:
	movsb					; transfer a char
	cmp BYTE PTR [si-1],0	; see if null terminator transferred
	jne append_loop			; no

; have EXE file path and DDL file name
	push    es
	pop ds					; restore ds -> file name
	xor dx,dx
	mov ax,4300h			; get file attributes (check for file existence)
	int 21h
	jc  check_errors		; program not found, make sure non-DOS error
	jmp NEAR PTR found_ddl	; program found

check_errors:
	cmp ax,2				; file not found
	je  try_path
	cmp ax,3				; path not found
	je  try_path
	cmp ax,5				; access denied error
	je  try_path

to_founderr:
	mov ds,cs:depend_seg
	mov ax,cs:ddl_counter
	mov dl,80				; 80 bytes/ dependency list entry
	mul dl
	mov dx,ax				; ds:dx -> DDL file name
	jmp NEAR PTR found_error

; file wasn't found in EXE's path, try DDLPATH environment variable
try_path:
	mov es,bx				; es -> environment block

dos_2x_entry:
	mov bx,OFFSET ddl_path	; bx holds target string address for compares
	xor si,si				; starting location for target string check

ddl_find_path:
	xor di,di				; offset into target string

ddl_loop2:
	mov al,es:[si]			; get byte from environment string
	inc si					; point to next char in environment
	cmp al,cs:[bx+di]		; does environment char match PATH string char
	je  ddl_byte_match		; yes, try next location
	or  al,es:[si]			; two zero values in a row mean the end of the environment
	jne ddl_find_path		; not the end of the environment

ddl_not_found:
	mov ax,2				; force file not found error

to_to_founderr:
	jmp SHORT to_founderr	; transfer to error handler

; check that PATH is not part of another environment string
ddl_byte_match:
	or  di,di				; di is zero if first char is matched
	jne ddl_2				; not first char, test already done
	cmp si,1				; si equals one if DDLPATH is first string in environment block
	je  ddl_2				; no previous environment string
	cmp BYTE PTR es:[si-2],0	; check if char before DDLPATH was nonzero
	jne ddl_find_path		; yes, LIB is a subset of another string, keep looking

ddl_2:
	inc di					; a match, move to next byte of target string
	cmp di,8				; check if all bytes matched
	jb  ddl_loop2			; not yet, keep comparing

; es -> environment block
; ds -> file name buffer
; cs -> loader data
ddl_3:
	xor di,di				; offset into path prefix

ddl_4:
	mov al,es:[si]			; get path character
	cmp al,';'				; check if path terminator character
	je  ddl_prefix_complete	; yes, check file's existence with the current path prefix
	cmp al,' '				; anything less than a space is also a terminator character
	jb  ddl_prefix_complete
	mov [di],al				; save path character
	inc di					; move to next name slot
	inc si					; move to next byte location
	jmp SHORT ddl_4			; loop for next character

ddl_prefix_complete:
	push    si				; save si -> current environment position
	mov si,bp				; append program name to end of path prefix
	cmp BYTE PTR [di-1],'\'	; check for backslash already in place
	je  ddl_5
	mov BYTE PTR [di],'\'	;put a backslash between the path and program name
	inc di					; point di past backslash

ddl_5:
	mov es,cs:depend_seg
	mov al,es:[si]			; get program name character
	mov [di],al				; transfer program name
	or  al,al				; stop transfer after first zero byte transfer
	je  ddl_search			; now see if file exists in this path
	inc si					; move to next name character slot
	inc di
	jmp SHORT ddl_5			; loop for next character

ddl_search:
	mov ax,4300h			; get file attributes (check for file existence)
	int 21h
	jnc ddl_prog_found		; found the program
	cmp ax,2				; file not found
	je  ddl_6
	cmp ax,3				; path not found
	je  ddl_6
	cmp ax,5				; access denied
	jne to_to_founderr

ddl_6:
	mov es,cs:prog_psp		; es -> PSP
	pop si					; restore es:si -> environment block position
	xor al,al
	cmp es:[si],al			; check last terminator
	je  ddl_not_found		; if zero then no more path prefixes to try
	cmp es:[si+1],al		; check character following last terminator
	je  ddl_not_found		; if zero then no more path prefixes to try

	inc si					; point to first character after last terminator
	jmp SHORT ddl_3			; try next path

ddl_prog_found:
	pop ax					; trash the values stored in the stack

; ds:dx -> fully qualified file name, transfer to dependency list storage
found_ddl:
	mov si,dx
	mov es,cs:depend_seg
	mov di,bp				; es:di -> dependency list
	mov cx,40				; move 80 bytes worth of name
	rep movsw				; transfer file name
	mov ax,3d00h			; open for reading
	int 21h
	jc  to_access_err
	mov bx,ax				; save file handle in bx
	pop es					; restore es -> PSP
	ret

to_access_err:
	jmp	NEAR PTR access_error

open_ddl_file   ENDP

;*****************************
;* FIND_GRPDEF_SEG           *
;*****************************

; find grpdef block for lib/module id, if any, keep pointer in grpdef_seg
; destroys ax,cx,dx,ds

find_grpdef_seg PROC    NEAR
	mov cx,cs:ddl_id
	mov dx,cs:module_id

	cmp cs:grpdef_seg,0		; see if any grpdef blocks allocated
	je  fgs_ret				; no

	mov ds,cs:grpdef_seg	; get first grpdef block

df_grploop:
	cmp dx,ds:[6]			; check segment module identifier
	jne df_nextgrp			; doesn't match
	cmp cx,ds:[4]			; check DDL id
	je  df_grpmatch			; match

df_nextgrp:
	mov ax,ds:[0]			; get pointer to next group list
	mov ds,ax				; ds -> new block, if any
	or  ax,ax				; see if at end block
	jne df_grploop			; not yet
	ret						; yes, no match occurred

df_grpmatch:
	mov cs:curr_grpblk,ds	; save -> group block

fgs_ret:
	ret
find_grpdef_seg ENDP

;*****************************
;* FIND_EXTLIST_PTR          *
;*****************************

; find extern list for segment, keep pointer in extlist _ptr
; destroys ax,cx,dx,ds

find_extlist_ptr    PROC    NEAR
	mov cx,cs:ddl_id
	mov dx,cs:module_id

	cmp cs:extdef_seg,0		; see if any extdef blocks allocated
	je  fep_ret				; no

	mov ds,cs:extdef_seg	; get first extdef block

df_extloop:
	cmp dx,ds:[4]			; check segment module identifier
	jne df_nextent			; doesn't match
	cmp cx,ds:[2]			; check DDL id
	je  df_extmatch			; match

df_nextent:
	mov ax,ds:[6]			; get pointer to next extern list
	mov ds,ax				; ds -> new block, if any
	or  ax,ax				; see if at end block
	jne df_extloop			; not yet
	ret						; yes, no match occurred

df_extmatch:
	mov cs:extlist_ptr,ds	; save -> extern list

fep_ret:
	ret
find_extlist_ptr    ENDP

;*****************************
;* DO_FIXUPS                 *
;*****************************

; setup for and do fixups on loaded segment
; upon entry es -> segdef entry
; destroys ax,bx,cx,dx,si,di,ds

do_fixups   PROC    NEAR
	mov ds,cs:binheader_seg
	mov dx,WORD PTR ds:[bhs_fixptr]	; get offset to fixup block
	mov ax,WORD PTR ds:[bhs_fixptr+2]

; fixup block file position is zero if no fixups
	mov cx,ax
	or  cx,dx
	jne	df_exist			; fixups exist
	ret						; no fixups

df_exist:
	sub dx,WORD PTR cs:seg_filepos
	sbb ax,WORD PTR cs:seg_filepos+2	; get offset to fixup block in ax:dx
	sub dx,BIN_HEADER_SIZE	; adjust for binary header
	sbb ax,0
	add dx,cs:load_offset	; adjust for load offset
	adc ax,0

	mov	si,cs:prog_seg		; adjust so load base==program segment temporarily
	push	cs:load_base
	mov	cs:load_base,si
	call    filepos_alt_ent	; ds:si -> fixup block
	pop	cs:load_base

;***	mov ax,ds
;***	add ax,cs:load_adjust
;***	mov ds,ax				; adjust for any fixups offset

	mov di,ds				; save critical register

; find grpdef block for lib/module id, if any, keep pointer in grpdef_seg
	call    find_grpdef_seg

; find extern list for segment, keep pointer in extlist_ptr
	call    find_extlist_ptr

	mov ds,di				; restore critical register
	mov cs:typeflag,0		; init fixup type flag
	mov cx,ds:[si+fhs_lowcount]	; get count of low byte fixups
	jcxz    df_near			; no low byte fixups
	mov dx,WORD PTR ds:[si+fhs_lowinfo]	; get information pointer in ax:dx
	mov ax,WORD PTR ds:[si+fhs_lowinfo+2]
	mov bx,WORD PTR ds:[si+fhs_lowloc]	; get location pointer in di:bx
	mov di,WORD PTR ds:[si+fhs_lowloc+2]
	call    perform_fixups	; perform the fixups

df_near:
	inc cs:typeflag
	mov cx,ds:[si+fhs_nearcount]	; get count of near fixups
	jcxz    df_far			; no near fixups
	mov dx,WORD PTR ds:[si+fhs_nearinfo]	; get information pointer in ax:dx
	mov ax,WORD PTR ds:[si+fhs_nearinfo+2]
	mov bx,WORD PTR ds:[si+fhs_nearloc]		; get location pointer in di:bx
	mov di,WORD PTR ds:[si+fhs_nearloc+2]
	call    perform_fixups	; perform the fixups

df_far:
	inc cs:typeflag
	mov cx,ds:[si+fhs_farcount]	; get count of far fixups
	jcxz    df_ret			; no far fixups
	mov dx,WORD PTR ds:[si+fhs_farinfo]	; get information pointer in ax:dx
	mov ax,WORD PTR ds:[si+fhs_farinfo+2]
	mov bx,WORD PTR ds:[si+fhs_farloc]	; get location pointer in di:bx
	mov di,WORD PTR ds:[si+fhs_farloc+2]
	call    perform_fixups	; perform the fixups

df_ret:
	ret
do_fixups   ENDP

;*****************************
;* PERFORM_FIXUPS            *
;*****************************

; upon entry ax:dx -> fixup info, di:bx -> fixup location
; cx holds number of fixups to perform
; es -> segdef entry
; cs:curr_segblk -> owning segdef block
;*** cs:load_base:cs:load_offset -> segment load image
; cs:prog_seg:cs:load_offset -> segment load image
; cs:extlist_ptr -> externals list starting at offset 14
; fixup info is 4 bytes for each fixup, consisting of modified
; fixdat byte (M bit replaces F bit), merged frame index word and target index word (3 bytes)
; fixup location is word offset into segment loaded of each fixup
; destroys ax,bx,cx,dx,di

perform_fixups  PROC    NEAR
	push    ds				; save critical register
	push    si

	sub bx,dx				; compute difference between info and location
	sbb di,ax

; convert fixup info pointer in ax:dx into ds:si pointer
	sub dx,WORD PTR cs:seg_filepos
	sbb ax,WORD PTR cs:seg_filepos+2	; get offset to fixup info in ax:dx
	sub dx,BIN_HEADER_SIZE	; adjust for binary header
	sbb ax,0
	add dx,cs:load_offset	; adjust for load offset
	adc ax,0

	mov	si,cs:prog_seg		; adjust so load base==program segment temporarily
	push	cs:load_base
	mov	cs:load_base,si
	call    filepos_alt_ent	; ds:si -> fixup info
	pop	cs:load_base

;***	mov ax,ds
;***	add ax,cs:load_adjust
;***	mov ds,ax				; adjust for any fixups offset

	add bx,si				; ds:bx -> fixup location

pf_loop:
	lodsb					; get modified fixdat
	mov cs:fixdat,al
	lodsw					; get frame index

	mov dl,ah				; save merged byte
	and ah,0fh				; make off target index bits
	mov cs:frame_index,ax

	mov ah,dl
	shr ah,1				; convert 4 MSB to proper value
	shr ah,1
	shr ah,1
	shr ah,1
	lodsb					; get target index low byte
	mov cs:target_index,ax

	call    compute_fixvals	; compute all necessary fixup values, return add value in ax
	mov dx,ax				; save add value
	mov di,ds:[bx]			; get location to fix up in di
	push    ds				; save ds -> fixup info

	mov al,cs:is_resolved
	or  al,al
	je  pf_nextfix			; unresolved

;***	mov ds,cs:load_base
	mov ds,cs:prog_seg

	add di,cs:load_offset	; ds:di -> segment image location to fix up

	mov al,cs:typeflag
	or  al,al				; check if low_order byte
	jne pf_notlow			; no

; low order byte
	add ds:[di],dl			; save new offset value back to data record
	jmp SHORT pf_nextfix

pf_notlow:
	mov ax,dx				; restore fixup value to ax
	cmp cs:typeflag,1		; check if offset
	je  pf_addin			; yes

; base or far fixup
	mov al,cs:is_absseg		; check if absolute or relocatable
	or  al,cs:is_abspub
	mov ax,cs:target_segment	; KEEP FLAGS from 'or'
	jne pf_addin			; absolute, don't adjust for program load address

	add ax,cs:prog_seg		; relocatable, add in program load address

; offset or near fixup
pf_addin:
	add ds:[di],ax			; save new offset

pf_nextfix:
	pop ds					; restore ds -> fixup info
	add bx,2				; move to next fixup location
	loop    pf_loop			; loop back until all fixups complete

	pop si
	pop ds					; restore critical register
	ret
perform_fixups  ENDP

;*****************************
;* COMPUTE_FIXVALS           *
;*****************************

; compute all necessary fixup values
; upon entry es -> segdef entry
; return fixup offset value in ax, cs:target_segment has fixup segment
; destroys ax,dx,di

compute_fixvals PROC    NEAR
	xor al,al
	mov cs:is_absseg,al		; init absolute flags
	mov cs:is_abspub,al

	mov al,cs:fixdat
	mov ah,al
	and al,70h
	mov cs:frame_method,al	; frame method is the frame field

	and ah,3				; break targt field out of fixdat
	mov al,ah
	mov cs:target_method,al	; target method is the targt field
	inc al					; make al known nonzero value
	mov cs:is_resolved,al	; init is_resolved flag to assume fixup okay

	push    es				; save critical register
	mov al,cs:target_method
	or  al,al				; check if segment index
	je  gta_seg
	cmp al,1				; check if group index
	je  gta_grp
	jmp NEAR PTR gta_ext	; external index

; overlaid segment, direct to overlay call table
gta_ovl_seg:
	mov ax,es:[14]			; get overlay id #
	jmp NEAR PTR gta_ovlshared

; segment index, method 0 or 4
gta_seg:
	mov ax,cs:target_index
	shl ax,1				; adjust ax for 2 paras/segment
	dec ax					; adjust for system info
	add ax,cs:curr_segblk	; ax -> proper segment entry
	mov es,ax				; es -> segdef entry

	mov al,es:[28]			; get overlay flags
	and al,14h				; see if overlaid
	jne gta_ovl_seg			; yes

	mov al,es:[26]			; get acbp byte
	and al,0e0h				; get align field
	je  gta_is_abs			; zero, absolute segment

; not an absolute segment
	mov ax,es:[2]			; get low word of segment offset
	mov WORD PTR cs:target_prog_off,ax	; update target program offset low word

gta_canon:
	mov ax,es:[30]
	or  ax,ax				; see if slave
	je  gta_segmaster		; no
	mov es,ax				; es -> master segdef

gta_segmaster:
	mov ax,es:[2]			; get low word of segment offset
	mov WORD PTR cs:lseg_canon,ax	; save low word of offset for canonical computation
	mov ax,es:[4]			; get high word of segment offset
	mov WORD PTR cs:lseg_canon+2,ax	; save high word of offset for canonical computation
	jmp NEAR PTR gta_done

; absolute segment
gta_is_abs:
	mov cs:is_absseg,1		; set absolute segment flag
	mov ax,es:[0]			; get frame number, convert to bytes
	mov cs:target_segment,ax	; get frame number in target_segment in case of LOC frame fixup
	shl ax,1
	shl ax,1
	shl ax,1
	shl ax,1
	mov WORD PTR cs:target_prog_off,ax	; update target program offset
	jmp NEAR PTR gta_done

; group index, method 1 or 5
gta_grp:
	mov ax,cs:target_index
	add ax,cs:curr_grpblk	; ax -> proper grpdef entry
	mov es,ax				; es -> grpdef entry

	mov ax,es:[10]			; see if master group entry
	or  ax,ax
	je  gta_grp2			; this is master
	mov es,ax				; es -> master group entry

gta_grp2:
	mov ax,es:[0]			; get low word of group offset
	mov WORD PTR cs:lseg_canon,ax	; save low word of offset for canonical computation
	mov WORD PTR cs:target_prog_off,ax	; save target program offset low word
	mov ax,es:[2]
	mov WORD PTR cs:lseg_canon+2,ax	; save high word of offset for canonical computation
	jmp NEAR PTR gta_done

; external index, method 2 or 6
gta_ext:
	mov di,cs:target_index
	add di,di				; make word offset
	add di,12				; adjust past 7 system words (relative 0 from relative 1 so add 12)
	mov es,cs:extlist_ptr	; es:di -> proper external list entry
	mov es,es:[di]			; es -> pubdef declarations entry

	mov al,es:[15]
	and al,1				; see if overlaid extdef
	jne gta_ovlext

	mov ax,es:[8]			; add in offset of pubdef declarations entry
	mov WORD PTR cs:target_prog_off,ax	; update target program offset low word

	mov al,es:[14]			; get definitions byte value
	and al,3				; only keep pubdef/extdef/comdef field
	cmp al,2				; check if unresolved external
	jb gta_unres			; unresolved external

; external resolved, fixup okay
	cmp al,3				; see if absolute public declaration
	je  gta_abs				; yes

; pubdef, not absolute
	mov es,es:[0]			; es -> segdef entry

	mov ax,es:[2]			; get low word of segment offset
	add WORD PTR cs:target_prog_off,ax	; update target program offset low word
	jmp NEAR PTR gta_canon	; compute canonical segment

; overlaid public
gta_ovlext:
	mov ax,es:[2]			; get overlay id

gta_ovlshared:
	dec ax					; make relative zero
	mov dx,ax
	add ax,ax
	add ax,dx
	add ax,5				; 3*id+5 is offset
	xor dx,dx
	add ax,WORD PTR cs:call_tbl_offset	; add in offset of call table
	adc dx,0				; carry to dx
	mov WORD PTR cs:target_prog_off,ax	; save target program offset low word
	mov WORD PTR cs:lseg_canon,ax	; save low word of offset for canonical computation
	mov ax,WORD PTR cs:call_tbl_offset+2
	add ax,dx				; add in carry from low word offset
	mov WORD PTR cs:lseg_canon+2,ax	; save high word of offset for canonical computation
	jmp SHORT gta_done

; unresolved external, no fixup
gta_unres:
	mov cs:is_resolved,0	; flag no fixup
	jmp SHORT gta_done

; absolute, no segment partition or segdef entry offsets
gta_abs:
	mov cs:is_abspub,1		; flag absolute public declaration
	mov ax,es:[2]			; get frame number in target_segment in case of LOC frame fixup
	mov cs:target_segment,ax

gta_done:
	pop es					; restore critical register

; see if absolute segment or absolute public declaration, no frame computation
	mov al,cs:is_absseg
	or  al,cs:is_abspub
	je  gta_notabs
	jmp NEAR PTR pef_resolved	; either absolute segment or absolute public declaration

gta_notabs:
	mov al,cs:frame_method
	cmp al,40h				; check if frame determined by location segment
	jne pef_11				; no

	mov ax,WORD PTR cs:lseg_canon	; get canonical segment low word offset containing lseg
	mov dx,WORD PTR cs:lseg_canon+2	; get high word
	shr dx,1
	rcr ax,1				; /2
	shr dx,1
	rcr ax,1				; /4
	shr dx,1
	rcr ax,1				; /8
	shr dx,1
	rcr ax,1				; /16, paragraph value (largest possible frame number)
	mov cs:target_segment,ax
	jmp NEAR PTR pef_chkres	; bypass other target segment computation code

pef_11:
	cmp al,50h				; check if frame determined by target's index
	jne pef_notarg			; no
	mov ax,cs:target_index	; pass target info to get_frame_addr
	mov dl,cs:target_method
	jmp SHORT pef_getframe

; frame determined by segment, group, or external index
pef_notarg:
	mov ax,cs:frame_index	; pass frame info to get_frame_addr
	mov dl,cs:frame_method

pef_getframe:
	push    es				; save critical register
	or  dl,dl				; check if segment index
	jne gfa_chkgrp			; no

; segment index
	shl ax,1				; adjust ax for 2 paras/segment
	dec ax					; adjust for system info
	add ax,cs:curr_segblk	; ax -> proper segment entry
	mov es,ax				; es -> segdef entry

	mov al,es:[28]			; get overlay flags
	and al,14h				; see if overlaid
	jne gfa_ovl_seg			; yes

	mov al,es:[26]			; get acbp byte
	and al,0e0h				; get align field
	je  gfa_absseg			; zero, absolute segment

	mov ax,es:[2]
	mov WORD PTR cs:frame_offset,ax	; save absolute frame offset for self-relative fixups

gfa_chkseg:
	mov ax,es:[30]
	or  ax,ax				; see if slave
	je  gfa_segmaster		; no
	mov es,ax				; es -> master segdef

gfa_segmaster:
	mov ax,es:[2]			; get master segment offset
	mov di,es:[4]

gfa_shared:
	shr di,1
	rcr ax,1				; /2
	shr di,1
	rcr ax,1				; /4
	shr di,1
	rcr ax,1				; /8
	shr di,1
	rcr ax,1				; /16, have paragraph value of offset (segment value)
	mov cs:target_segment,ax	; save to memory variable
	jmp NEAR PTR gfa_done

; overlaid segment or extdef
gfa_ovl_seg:
	mov ax,WORD PTR cs:call_tbl_offset
	mov WORD PTR cs:frame_offset,ax	; save absolute frame offset for self-relative fixups
	mov di,WORD PTR cs:call_tbl_offset+2
	jmp SHORT gfa_shared

gfa_chkgrp:
	cmp dl,10h				; check if group index
	je  gfa_grpind			; yes
	cmp dl,1
	jne gfa_extind			; no

; group index
gfa_grpind:
	add ax,cs:curr_grpblk	; ax -> proper grpdef entry
	mov es,ax				; es -> grpdef entry

gfa_group_entry:
	mov ax,es:[10]			; see if master group entry
	or  ax,ax
	je  gfa_group2			; this is master
	mov es,ax				; es -> master group entry

gfa_group2:
	mov ax,es:[0]			; get low word of group offset
	mov di,es:[2]			; get high word of group offset

	mov WORD PTR cs:frame_offset,ax	; save absolute frame offset for self-relative fixups
	jmp NEAR PTR gfa_shared	; jump to code shared with segment index

; absolute segment for frame
gfa_absseg:
	mov cs:is_absseg,1		; set absolute segment flag
	mov ax,es:[0]			; get frame number
	mov cs:target_segment,ax	; save to memory variable
	jmp NEAR PTR gfa_done

; external index
gfa_extind:
	mov di,ax
	add di,di				; make word offset
	add di,12				; adjust past 7 system words (relative 0 from relative 1 so add 12)
	mov es,cs:extlist_ptr	; es:di -> proper external list entry
	mov es,es:[di]			; es -> pubdef declarations entry

	mov al,es:[15]
	and al,1				; see if overlaid extdef
	jne gfa_ovl_seg			; yes, same code as overlaid segment (same frame)

	mov al,es:[14]			; get definitions byte value
	and al,3				; only keep pubdef/extdef/comdef/absolute field
	cmp al,2				; check if unresolved external
	jb  gfa_unres_ext		; unresolved external
	ja  gfa_abs				; absolute public declaration

; pubdef declaration (not absolute)

; check if pubdef has a group associated with it
	mov al,es:[15]
	and al,80h				; high bit set if group associated with public declaration
	je  gfa_nogroup			; no group

	mov es,es:[0]			; es -> segdef entry
	mov es,es:[16]			; es -> group
	jmp SHORT gfa_group_entry	; perform code in common with group index

gfa_nogroup:
	mov dx,es:[0]			; dx -> segment partition entry
	pop es					; es -> current segdef entry (loaded segment)
	push    es
	mov ax,es:[2]
	mov WORD PTR cs:frame_offset,ax	; save absolute frame offset for self-relative fixups
	mov es,dx				; es -> segment partition entry
	jmp NEAR PTR gfa_chkseg	; external resolved, perform code in common with segment

; absolute public declaration
gfa_abs:
	mov cs:is_abspub,1		; flag absolute public declaration
	mov ax,es:[2]			; get frame number of public entry
	mov cs:target_segment,ax	; save to target segment
	jmp SHORT gfa_done

; unresolved external, no fixup
gfa_unres_ext:
	mov cs:is_resolved,0	; flag no fixup

gfa_done:
	pop es					; restore critical register

pef_chkres:
	mov al,cs:is_resolved
	or  al,al
	je  pef_ret				; unresolved external, no fixup

; convert target segment and target program offset to segment:offset format
pef_resolved:

; see if absolute segment or absolute public declaration, no address adjustment
	mov al,cs:is_absseg
	or  al,cs:is_abspub
	jne pef_seg_rel			; either absolute segment or absolute public declaration

; check M bit for self-relative fixup
	mov al,cs:fixdat
	and al,80h				; get M bit field value
	je  pef_self_rel		; M bit reset, fixup self-relative

; fixup segment relative
pef_seg_rel:
	mov di,cs:target_segment
	shl di,1
	shl di,1
	shl di,1
	shl di,1

; get target offset in di:ax
	mov ax,WORD PTR cs:target_prog_off	; get absolute offset low word
	sub ax,di				; compute low word difference

pef_ret:
	ret

; fixup is self-relative
pef_self_rel:
	mov ax,WORD PTR cs:target_prog_off	; get absolute offset low word

	sub ax,WORD PTR cs:frame_offset	; subtract off frame that fixup is relative too

	sub ax,ds:[bx]			; subtract off location to fix up
	cmp cs:typeflag,0		; see if lowbyte fixup
	jne cf_selfnear			; no
	dec ax
	ret

cf_selfnear:
	sub ax,2				; adjust two bytes for offset/near
;**    sbb di,0
	ret
compute_fixvals ENDP

;*****************************
;* RESOLVE_COMMUNALS         *
;*****************************

; resolve communal variables addresses that have no corresponding PUBDEF,
; adjusting or creating segments as necessary
; destroys ax,bx,cx,dx,di,si,ds

resolve_communals   PROC    NEAR
	mov ax,cs:extdef_seg

rc_blkloop:
	or  ax,ax				; check if any extdef blocks left
	je  rc_ret				; no, all done
	mov ds,ax				; ds -> block
	mov dx,ax				; save -> block
	mov cx,ds:[0]			; get count of entries
	jcxz    rc_next_block	; no entries in block
	mov si,14				; ds:si -> extdef entry pointer

rc_entloop:
	lodsw					; ax -> pubdef entry
	mov ds,ax
	mov al,ds:[15]
	and al,40h				; see if communal bit set
	jne rc_is_communal		; yes

rc_next_entry:
	mov ds,dx				; ds -> extdef block
	loop    rc_entloop		; loop thru all entries

rc_next_block:
	mov ax,ds:[6]			; get pointer to next block
	jmp SHORT rc_blkloop

rc_is_communal:
	test    BYTE PTR ds:[15],10h	; see if near communal
	jne rc_near				; yes
	call    res_far_communal	; resolve far communal
	jmp SHORT rc_reset		; reset communal bit and continue

rc_near:
	call    res_near_communal	; resolve near communal

rc_reset:
	and BYTE PTR ds:[15],0bfh	; reset communal bit
	jmp SHORT rc_next_entry

rc_ret:
	ret
resolve_communals   ENDP

;*****************************
;* UPDATE_COMM_SEGMENT       *
;*****************************

; update segment and class name and acbp byte of far or huge communal segment
; upon entry bx -> offset of segment/class name (adjusted back 8 bytes to emulate lnames entries)
; destroys ax,di,es

update_comm_segment PROC    NEAR
	call    create_comm_segment
	mov ax,cs				; update segment and class names
	mov es:[10],ax
	mov es:[14],ax			; save segment pointer to segment and class names
	mov es:[8],bx			; save offset pointer to segment name
	mov es:[12],bx			; save offset pointer to segment class
	mov BYTE PTR es:[26],01100000b	; para aligned, private combine
	ret
update_comm_segment ENDP

;*****************************
;* CREATE_COMM_SEGMENT       *
;*****************************

; create segment for communal variable
; returns segment of segdef entry in es
; destroys ax,di,es

create_comm_segment PROC    NEAR
	xor di,di				; offset in segdef blocks

; allocate a block for segdef storage
	mov ax,cs:hialloc_base
	sub ax,3
	cmp cs:segdef_seg,0		; see if any segdef block previously allocated
	jne ccs_walkseg			; yes
	mov cs:segdef_seg,ax	; no, use old high allocation base as new segment
	jmp SHORT ccs_segbase

ccs_walkseg:
	mov es,cs:allocseg_seg
	mov es:[di],ax			; update block pointer

ccs_segbase:
	mov es,ax				; es -> segdef block
	mov cs:allocseg_seg,ax	; save last allocated segdef block
	mov cs:hialloc_base,ax	; update high allocation base
	xor ax,ax
	stosw					; zero pointer to next block
	inc ax					; ax == 1
	stosw					; count of entries in block [2]
	mov	es:[8],ax			; save count of entries
	mov	es:[10],ax
	mov	es:[12],ax
	dec ax					; ax == 0
	stosw					; DDL, module id [4],[6]
	mov	ax,-1
	stosw
	mov ax,es
	inc ax
	mov es,ax				; es -> segdef entry
	xor ax,ax
	mov es:[6],ax			; zero out length
	mov WORD PTR es:[18],-1	; module identifier
	mov es:[20],ax			; zero out binary pointer (no init'ed, loadable data)
	mov es:[22],ax
	mov es:[27],ax			; zero miscellaneous flag bytes
	inc cs:total_seg_count	; bump count of segments
	ret
create_comm_segment ENDP

;*****************************
;* RES_NEAR_COMMUNAL         *
;*****************************

; resolve near communal variables
; upon entry ds -> communal declaration entry
; destroys ax,di,es

res_near_communal   PROC    NEAR
	push    cx				; save critical register

	mov ax,cs:near_comm_segptr	; get pointer to segdef entry of segment for near communal variables
	mov es,ax
	or  ax,ax				; see if set yet
	jne rnc_chk_dgroup		; yes, bypass segment search

; create segment c_common, class BSS, group DGROUP
	call    create_comm_segment	; create segdef and segment partition entry for new segment
	mov cs:near_comm_segptr,es	; save pointer to near communal variables segdef entry
	mov ax,cs				; update segment and class names
	mov es:[10],ax
	mov es:[14],ax			; save segment pointer to segment and class names
	mov ax,OFFSET c_commontext-8	; get offset to segment name, back off 8 bytes to match lnames entry format
	mov es:[8],ax			; save offset pointer to segment name
	mov ax,OFFSET bsstext-8	; get offset to segment class, back off 8 bytes to match lnames entry format
	mov es:[12],ax			; save offset pointer to segment class
	mov BYTE PTR es:[26],01100000b	; paragraph aligned, private combine
	xor ax,ax
	mov es:[27],ax			; zero miscellaneous flag bytes

; point segment at group DGROUP if it exists
	mov ax,cs:dgroup_ptr
	mov es:[16],ax			; save -> group entry in segdef entry

; segment created, es -> segdef entry, now resolve communal
rnc_chk_dgroup:
	mov ax,cs:dgroup_ptr
	or  ax,ax				; see if a group pointer to near communals
	je  rnc_5				; no

	or  BYTE PTR ds:[15],80h	; flag that group is associated with communal declaration
	mov ds:[2],ax			; save pointer to group entry in communal declaration

rnc_5:
	mov ds:[0],es			; save pointer in communal declaration entry
	mov al,ds:[14]			; get definition flag
	and al,0fch				; mask off other bits
	or  al,2
	mov ds:[14],al			; flag as resolved public
	mov ax,es:[6]			; get old segment length
	mov cx,ds:[8]			; get low word of communal length
	mov ds:[8],ax			; save communal variable offset (previous end of segment)
	add ax,cx
	jnc rnc_6				; no overflow
	or  BYTE PTR es:[26],2	; set Big bit

rnc_6:
	mov es:[6],ax			; save new segment length
	pop cx
	ret
res_near_communal   ENDP

;*****************************
;* RES_FAR_COMMUNAL          *
;*****************************

; resolve far communal variables
; upon entry es -> communal declaration entry
; destroys ax,bx,di

res_far_communal    PROC    NEAR
	push    si				; save critical register
	push    cx
	push    dx
	mov ax,ds:[8]			; get low word of communal length in ax
	cmp WORD PTR ds:[2],1	; high word length more than 1 if huge
	jb  rfc_far				; zero if far
	ja  rfc_huge			; huge communal
	or  ax,ax				; low word length nonzero if huge
	jne rfc_huge			; nonzero with nonzero high word, huge communal

; far communal variable (<64k)
rfc_far:
	add ax,WORD PTR cs:far_comm_len		; add in previous communal lengths
	mov si,ds:[2]			; get high word of communal length
	adc si,WORD PTR cs:far_comm_len+2	; add with carry in high word of previous lengths

	cmp si,1				; see if >64K total length
	jb  rfc_nonew			; no, add to total running communal variable length
	ja  rfc_new				; yes, create a new segment for previous communals
	or  ax,ax				; could be exactly 64K, check for nonzero low word
	jne rfc_new				; nonzero, >64K length, create new segment

rfc_nonew:
	mov dx,cs:far_comm_segptr	; get pointer to segdef entry for far communals
	or  dx,dx				; see if null (no far communal segment created yet)
	jne rfc_1				; non-null, communal segment exists

rfc_new:
	mov bx,OFFSET far_bsstext-8	; get offset to segment name, back off 8 bytes to match lnames entry format
	call    update_comm_segment

	mov cs:far_comm_segptr,dx	; save pointer to segdef entry in memory variable
	mov ax,ds:[8]			; get low word of communal length
	mov si,ds:[2]			; get high word of communal length

rfc_1:
	mov dx,es:[6]			; get previous segment length for public offset
	mov es:[6],ax			; save new segment length
	or  si,si				; see if high word nonzero
	je  rfc_2				; no
	or  BYTE PTR es:[26],2	; nonzero (64K segment), set Big bit

rfc_2:
	mov WORD PTR cs:far_comm_len,ax
	mov WORD PTR cs:far_comm_len+2,si	; save new communal variable length
	mov ds:[0],es			; update communal entry segdef entry pointer
	mov ds:[8],dx			; save offset of variable
	mov al,ds:[14]			; get definition flag
	and al,0fch				; mask off other bits
	or  al,2
	mov ds:[14],al			; flag as resolved public
	jmp SHORT rfc_ret		; return

; huge communal variable (>=64k)
rfc_huge:
	mov cs:huge_comm_len,ax	; save length of huge communal variable modulo 64K
	mov cx,ds:[2]			; get count of 64K huge segments to create
	xor dx,dx				; nonzero dx flags not first time thru segment creation loop

rfc_loop:
	mov bx,OFFSET huge_bsstext-8	; get offset to segment name, back off 8 bytes to match lnames entry format
	call    update_comm_segment

	mov WORD PTR es:[6],0	; zero segment length
	or  BYTE PTR es:[26],2	; set Big bit
	or  dx,dx				; see if first time thru loop
	jne rfc_3				; no, bypass communal entry update

	mov dx,1				; set flag so update only occurs once
	mov ds:[0],es			; update communal entry segdef entry pointer
	mov al,ds:[14]			; get definition flag
	and al,0fch				; mask off other bits
	or  al,2
	mov ds:[14],al			; flag as resolved public
	mov WORD PTR es:[8],0	; zero offset of variable

rfc_3:
	loop rfc_loop			; loop until all 64K segments created

	mov bx,OFFSET huge_bsstext-8	; get offset to segment name, back off 8 bytes to match lnames entry format
	call    update_comm_segment

	mov ax,cs:huge_comm_len	; get low word of communal length
	mov es:[6],ax			; save length of segment (communal length modulo 64K)

rfc_ret:
	pop dx
	pop cx					; restore critical register
	pop si
	ret
res_far_communal    ENDP

;*****************************
;* GET_OVL_SEG_ID            *
;*****************************

; step through each segdef entry, if overlaid then store overlay id in entry,
; flag used by overlay
; destroys ax,bx,cx,dx,ds

get_ovl_seg_id  PROC    NEAR
	mov ax,cs:segdef_seg

gos_blkloop:
	or  ax,ax               ; see if at end block
	jne	gos_notend
	ret						; at end block, return

gos_notend:
	mov dx,ax               ; save -> segdef block system
	mov ds,ax
	mov cx,ds:[2]           ; get count of entries in block
	jcxz    gos_nextblk     ; no entries

; preset ds 1 para below start of block so double increment will -> first entry
	dec ax
	mov ds,ax

gos_entloop:
	mov ax,ds               ; get old entry pointer
	inc ax
	inc ax
	mov ds,ax               ; ds -> next entry in block
	mov al,ds:[28]          ; get overlay flags
	and al,14h
	je  gos_next            ; not overlaid
	or  BYTE PTR ds:[27],20h    ; flag as used by overlay
	mov ax,cs:overlay_id
	inc ax
	mov cs:overlay_id,ax
	mov ds:[14],ax          ; overwrite class name segment with id, not needed anymore

	push	cx				; save critical registers
	push	dx
	mov	cl,4				; use as shift constant
	xor	dx,dx				; init total size of overlaid _DT, _DAT, CODE segments
	mov	bx,dx				; init offset

gos_chkloop:
	mov	ax,ds:[bx+6]		; get size of segment
	add ax,15
	shr	ax,cl				; convert to paras
	add	dx,ax				; add to total length
	les	di,ds:[bx+8]		; es:di -> segment name
	add	bx,32				; update to next segment entry, if any
	cmp	BYTE PTR es:[di+9],'C'	; see if CODE segment (last length to compute)
	jne	gos_chkloop			; no

; dx holds total length of segments
gos_largechk:
	cmp dx,cs:largest_ovl   ; check if should update largest overlay variable
	jb  chk_2nd             ; no

	mov bx,cs:largest_ovl   ; keep old largest value
	mov cs:largest_ovl,dx   ; update largest
	mov cs:second_ovl,bx    ; old largest to second largest
	jmp SHORT gos_compdone

chk_2nd:
	cmp dx,cs:second_ovl    ; check if should update second largest overlay
	jb  gos_compdone		; no
	mov cs:second_ovl,dx

; done computing overlay sizes
gos_compdone:
	pop	dx					; restore critical registers
	pop	cx

gos_next:
	loop    gos_entloop

gos_nextblk:
	mov ds,dx               ; restore ds -> segdef block
	mov ax,ds:[0]           ; ds -> next block
	jmp NEAR PTR gos_blkloop

get_ovl_seg_id  ENDP

;*****************************
;* BUMP_OVL_USED_SEG         *
;*****************************

; bump count of overlay used segments,
; check for slave segment, adjust accordingly

bump_ovl_used_seg   PROC    NEAR
	or  BYTE PTR es:[27],20h    ; set flag
	mov ax,cs:ovl_used_seg  ; bump count of segments used by overlays
	inc ax
	mov cs:ovl_used_seg,ax
	mov es:[12],ax          ; save entry offset in main segment table, overwrite class name offset
	mov ax,es:[30]          ; get master pointer, if any
	or  ax,ax
	je  bs_savecanon        ; no master pointer
	mov ax,cs:ovl_used_slave    ; bump count of slaved segments used by overlays
	inc ax
	mov cs:ovl_used_slave,ax

bs_savecanon:
	mov es:[8],ax           ; save canonical id, zero if no master

	ret
bump_ovl_used_seg   ENDP

;*****************************
;* BUMP_OVL_USED_GRP         *
;*****************************

; bump count of overlay used groups, set flag

bump_ovl_used_grp   PROC    NEAR
	mov ax,es:[10]          ; see if master group entry
	or  ax,ax
	je  bg_2                ; this is master
	mov es,ax               ; es -> master group entry

bg_2:
	mov al,es:[14]
	and al,1                ; see if grpdef previously flagged as used by overlays
	jne bg_ret              ; yes

	inc ax                  ; al == 1
	or  BYTE PTR es:[14],al ; set flag
	mov ax,cs:ovl_used_grp  ; bump count of groups used by overlays
	inc ax
	mov cs:ovl_used_grp,ax
	mov es:[4],ax           ; save entry offset in main group table

bg_ret:
	ret
bump_ovl_used_grp   ENDP

;*****************************
;* OVL_SETUP                 *
;*****************************

; setup entries and table for overlays
; destroys ax,bx,cx,dx,si,di,bp,ds,es

ovl_setup   PROC    NEAR
	cmp cs:extdef_seg,0     ; see if any extdef blocks allocated
	jne os_procext          ; yes
	jmp NEAR PTR os_ctalloc ; no

; cycle through all segdefs owned by overlaid module, set flag if not
; previously marked as used, update pointers and counters
os_modseg:
	mov ax,ds:[10]          ; get -> segdefs
	or  ax,ax               ; see if any
	je  os_modgrp           ; no
	mov es,ax
	mov cx,es:[2]           ; get count of segdefs in block
	jcxz    os_modgrp       ; no segdefs
	dec ax                  ; setup for first segdef entry computation
	mov es,ax

os_modsegloop:
	mov ax,es
	inc ax
	inc ax
	mov es,ax               ; es -> next segdef entry

	mov al,es:[27]
	and al,20h              ; see if segdef previously flagged as used by overlays
	jne os_modsavenext      ; yes
	call    bump_ovl_used_seg   ; processed overlay used segment

os_modsavenext:
	loop    os_modsegloop   ; loop until complete

; cycle through all grpdefs owned by overlaid module, set flag if not
; previously marked as used, update pointer and counter
os_modgrp:
	mov ax,ds:[12]          ; get -> grpdefs
	or  ax,ax               ; see if any
	je  os_doextdef         ; no
	mov es,ax
	mov cx,es:[2]           ; get count of grpdefs in block
	jcxz    os_doextdef     ; no segdefs
	inc ax                  ; setup for first grpdef entry computation

os_modgrploop:
	mov es,ax               ; es -> grpdef entry
	call    bump_ovl_used_grp

os_modgrpblk:
	mov ax,es
	inc ax
	loop    os_modgrploop   ; loop until complete
	jmp SHORT os_doextdef   ; done with segdefs and grpdefs, continue processing extdefs

os_procext:
	mov dx,cs:extdef_seg    ; get first extdef block

os_extblkloop:
	mov ds,dx
	mov al,ds:[8]           ; get flag byte
	and al,14h              ; see if overlaid module
	mov cs:ovl_mod_flag,al  ; save overlaid module flag
	jne os_modseg           ; yes, first cycle through segdefs owned by overlaid module

os_doextdef:
	mov cx,ds:[0]           ; get count of extdef entries
	jcxz    os_nextextblk   ; no entries exist

	mov si,14               ; offset into extdef entries, past 7 system words

os_extentloop:
	lodsw
	mov ds,ax               ; ds -> public entry
	mov al,ds:[15]
	mov bl,al
	and al,8                ; see if previously used by overlay
	je  os_newext           ; no

os_nextextent:
	mov ds,dx               ; ds -> external block
	loop    os_extentloop

os_nextextblk:
	mov dx,ds:[6]           ; get pointer to next extern list
	or  dx,dx               ; see if at end block
	jne os_extblkloop       ; not yet
	jmp NEAR PTR os_ctalloc ; yes

os_newext:
	and bl,1                ; see if overlaid public
	jne os_ovlpub           ; yes

	cmp cs:ovl_mod_flag,0   ; see if overlaid module
	je  os_nextextent       ; no, don't track nonoverlaid publics for nonoverlaid modules

	mov ax,cs:ovl_used_pub  ; bump count of publics used by overlays
	inc ax
	mov cs:ovl_used_pub,ax
	mov ds:[4],ax           ; save entry offset in overlay used public table, overwrite name offset
	jmp SHORT os_chkseg

os_ovlpub:
	mov ax,ds:[8]           ; get public offset
	or  ax,ax               ; see if zero (use overlaid segment identifier)
	je  os_offzero          ; yes

	inc cs:ovl_pub_count    ; bump count of overlaid publics with nonzero offset
	mov ax,cs:overlay_id
	inc ax
	mov cs:overlay_id,ax    ; bump total overlay identifiers (discrete overlay entry points)
	jmp SHORT os_2

os_offzero:
	mov es,ds:[0]           ; es -> owning segdef
	mov ax,es:[14]          ; get segdef overlay id

os_2:
	mov ds:[2],ax           ; save overlay id

	cmp cs:ovl_mod_flag,0   ; see if overlaid module
	je  os_nextextent       ; no, only track overlaid publics for nonoverlaid modules

; get count of segments used by overlays, count of slaved segdefs used by overlays
os_chkseg:
	or  BYTE PTR ds:[15],8  ; flag as previously used by overlay
	mov es,ds:[0]           ; es -> owning segdef
	mov al,es:[27]
	and al,20h              ; see if segdef previously flagged as used by overlays
	jne os_chkgrp           ; yes

	call    bump_ovl_used_seg   ; processed overlay used segment

; update count of groups used by overlays
os_chkgrp:
	mov al,ds:[15]
	and al,80h              ; high bit set if group associated with public declaration
	je  to_os_nextextent    ; no group

	mov es,ds:[0]           ; es -> segdef entry
	mov es,es:[16]          ; es -> group
	call    bump_ovl_used_grp

to_os_nextextent:
	jmp NEAR PTR os_nextextent

; compute address of overlay call table (3*overlay_id)+5 bytes
; directly below DDL manager in memory after relocation
os_ctalloc:
	mov	ax,OFFSET cut_here
	sub	ax,OFFSET startup
	mov	dx,ax				; keep byte count to move
	add	ax,15				; round up to next para
	mov	cl,4
	shr	ax,cl				; convert to paras of code to move
	mov	di,cs:top_of_mem
	sub	di,8				; allow 8 paras of slop in reallocation process
	sub	di,ax				; di -> new location of DDL manager code after relocation

	mov ax,cs:overlay_id
	mov dx,ax
	add ax,ax               ; x2
	add ax,dx               ; x3
	add ax,20               ; round up, plus 5 jump bytes
	shr ax,cl				; cl==4
	mov cx,ax				; cx == table size in paras
	mov	cs:ovl_call_tbl_size,cx	; save size of table
	mov ax,di				; ax -> new location of DDL manager code after relocation
	sub ax,cx
	mov cs:ovl_call_tbl_seg,ax
	sub ax,cs:prog_seg      ; compute offset from start of program (in paras)
	xor cx,cx
	shl ax,1                ; convert to bytes in cx:ax
	rcl cx,1
	shl ax,1
	rcl cx,1
	shl ax,1
	rcl cx,1
	shl ax,1
	rcl cx,1
	mov WORD PTR cs:call_tbl_offset,ax
	mov WORD PTR cs:call_tbl_offset+2,cx

; allocate overlay stash position table (2*ovl_seg_count)
	mov ax,cs:ovl_seg_count
	add ax,ax
	add ax,15               ; round up
	mov cl,4
	shr ax,cl
	mov cx,ax
	mov ax,cs:hialloc_base
	mov	cs:ovl_alloc_end,ax	; save end of overlay allocations except overlay call table
	sub ax,cx
	mov cs:ovl_stash_tbl_seg,ax

; allocate overlay correlation table (2*ovl_pub_count)
	mov ax,cs:ovl_pub_count
	add ax,ax
	add ax,15               ; round up
	mov cl,4
	shr ax,cl
	mov cx,ax
	mov dx,ax               ; save size of table in paras, same as overlaid public table
	mov ax,cs:ovl_stash_tbl_seg
	sub ax,cx
	mov cs:ovl_correl_tbl_seg,ax

; allocate overlaid public with nonzero offset table (2*ovl_pub_count)
	sub ax,dx
	mov cs:ovlpub_tbl_seg,ax

; allocate overlay used public table (2*ovl_used_pub) not including overlaid publics
	mov ax,cs:ovl_used_pub
	add ax,ax
	add ax,15               ; round up
	mov cl,4
	shr ax,cl
	mov cx,ax
	mov ax,cs:ovlpub_tbl_seg
	sub ax,cx
	mov cs:ovl_usedpub_tbl_seg,ax

; update overlay used public table and overlay correlation table
	cmp cs:extdef_seg,0     ; see if any extdef blocks allocated
	jne os_procpub          ; yes
	jmp NEAR PTR os_grpalloc

os_procpub:
	mov dx,cs:extdef_seg    ; get first extdef block

os_pubblkloop:
	mov ds,dx
	mov al,ds:[8]           ; get flag byte
	and al,14h              ; see if overlaid module

	mov cs:ovl_mod_flag,al  ; save overlaid module flag

	mov cx,ds:[0]           ; get count of extdef entries
	jcxz    os_nextpubblk   ; no entries exist

	mov si,14               ; offset into extdef entries, past 7 system words

os_pubentloop:
	lodsw
	mov ds,ax               ; ds -> public entry
	mov al,ds:[15]
	and al,1                ; see if overlaid public
	jne os_pubovl           ; yes

	cmp cs:ovl_mod_flag,al  ; see if overlaid module
	je  os_nextpubent       ; no, don't save used publics by nonoverlaid modules

	mov di,ds:[4]           ; get used public number
	dec di                  ; make relative zero
	add di,di               ; convert to word offset in table
	mov ax,ds:[8]           ; get public offset
	mov es,cs:ovl_usedpub_tbl_seg
	stosw                   ; save it
	jmp SHORT os_nextpubent

os_pubovl:
	mov ax,ds:[2]           ; get overlay id
	sub ax,cs:ovl_seg_count ; adjust for overlaid segments/zero offset publics
	jbe os_nextpubent       ; zero offset public matching overlaid segment

	dec ax                  ; make relative zero
	mov di,ax
	add di,di               ; make word offset in table
	mov es,ds:[0]           ; es -> owning segment
	mov ax,es:[14]          ; get segdef overlay id
	mov es,cs:ovl_correl_tbl_seg
	stosw                   ; save it

; nonzero overlaid public offset
	mov ax,ds:[8]           ; get public offset
	sub di,2                ; di holds proper offset in both tables, restore
	mov es,cs:ovlpub_tbl_seg
	stosw                   ; save it

os_nextpubent:
	mov ds,dx               ; ds -> external block
	loop    os_pubentloop

os_nextpubblk:
	mov dx,ds:[6]           ; get pointer to next extern list
	or  dx,dx               ; see if at end block
	jne os_pubblkloop       ; not yet

; allocate main group table (4*ovl_used_grp)
os_grpalloc:
	mov ax,cs:ovl_used_grp
	add ax,ax
	add ax,ax
	add ax,15               ; round up
	mov cl,4
	shr ax,cl
	mov cx,ax
	mov ax,cs:ovl_usedpub_tbl_seg
	sub ax,cx
	mov cs:ovl_usedgrp_tbl_seg,ax

; step through group entries and update main group table
	mov ax,cs:grpdef_seg
	or  ax,ax               ; see if grpdef block exists
	je  os_fpalloc          ; no

os_grpblk:
	mov dx,ax               ; save -> grpdef block system
	mov ds,ax               ; ds -> grpdef block
	mov cx,ds:[2]           ; get count of entries in block
	jcxz    os_nextgrpblk   ; no entries exist

os_grploop:
	inc ax
	mov ds,ax               ; ds -> grpdef block entry
	mov ax,ds:[10]
	or  ax,ax               ; see if master group entry
	je  os_mastgrp          ; yes
	mov ds,ax

os_mastgrp:
	mov al,ds:[14]          ; get group flag
	and al,1                ; see if used by overlay
	je  os_nextgrp          ; no
	mov di,ds:[4]           ; get group id
	dec di                  ; make relative zero
	add di,di
	add di,di               ; make doubleword offset in table
	mov es,cs:ovl_usedgrp_tbl_seg
	mov ax,ds:[0]           ; save low word of group offset
	stosw
	mov ax,ds:[2]           ; save high word of group offset
	stosw

os_nextgrp:
	mov ax,ds               ; setup ax for next entry update
	loop    os_grploop      ; check it

os_nextgrpblk:
	mov ds,dx               ; ds -> grpdef block
	mov ax,ds:[0]           ; get pointer to next block
	or  ax,ax               ; see if exists
	jne os_grpblk           ; yes, check it

; allocate ovl handle table (2*ovl_seg_count)
; allocate ovl file position table (4*ovl_seg_count)
os_fpalloc:
	mov ax,cs:ovl_seg_count
	add ax,ax				; x2
	add ax,15               ; round up
	mov cl,4
	shr ax,cl				; ax holds paras for handle table
	mov cx,ax
	mov ax,cs:ovl_usedgrp_tbl_seg
	sub ax,cx
	mov cs:ovl_handle_tbl_seg,ax

	add cx,cx               ; x4, paras for file position table 
	sub ax,cx
	mov cs:ovl_filepos_tbl_seg,ax

; allocate main segment table (4*ovl_used_seg) not including overlaid segments
	mov ax,cs:ovl_used_seg
	add ax,ax
	add ax,ax
	add ax,15               ; round up
	mov cl,4
	shr ax,cl
	mov cx,ax
	mov ax,cs:ovl_filepos_tbl_seg
	sub ax,cx
	mov cs:ovl_usedseg_tbl_seg,ax

; allocate canonical segment table (4*ovl_used_slave)
	mov ax,cs:ovl_used_slave
	add ax,ax
	add ax,ax
	add ax,15               ; round up
	mov cl,4
	shr ax,cl
	mov cx,ax
	mov ax,cs:ovl_usedseg_tbl_seg
	sub ax,cx
	mov cs:ovl_usedcan_tbl_seg,ax
	mov	cs:ovl_alloc_start,ax	; update start of overlay allocations

; update main segment table, canonical segment table,
; overlay file position/handle table
	mov ax,cs:segdef_seg

os_segblkloop:
	or  ax,ax               ; see if at end block
	jne os_validseg         ; no
	jmp NEAR PTR os_stash

os_validseg:
	mov dx,ax               ; save -> segdef block system
	mov ds,ax
	mov cx,ds:[2]           ; get count of entries in block
	jcxz    os_nextsegblk   ; no segments in block

; preset ds 1 para below start of block so double increment will -> first entry
	dec ax
	mov ds,ax

os_segentloop:
	mov ax,ds               ; get old entry pointer
	inc ax
	inc ax
	mov ds,ax               ; ds -> next entry in block

	mov al,ds:[27]
	and al,20h              ; see if segment used by overlay
	je  os_nextseg          ; no

	mov al,ds:[28]          ; get overlay flags
	mov ah,al
	and al,14h
	jne os_ovlseg           ; overlaid

; not overlaid segment, used by overlay
	mov di,ds:[12]          ; get segment used id
	dec di                  ; make relative zero
	add di,di
	add di,di               ; make doubleword offset in table
	mov ax,ds:[2]           ; get low word of segment offset
	mov es,cs:ovl_usedseg_tbl_seg
	stosw
	mov ax,ds:[4]           ; get high word of segment offset
	stosw
	sub di,2                ; back up to high word of entry

	mov al,ds:[26]          ; get acbp byte
	and al,0e0h             ; get A bit field value
	jne os_notabsseg        ; nonzero, not an absolute segment
	mov ax,ds:[2]           ; get frame number
	mov es:[di-2],ax        ; overwrite invalid offset with frame number
	mov ax,0ffffh           ; flag absolute segment
	mov es:[di],ax
	jmp SHORT os_nextseg

os_notabsseg:
	mov ax,ds:[30]
	or  ax,ax               ; see if slaved segdef
	je  os_nextseg          ; no

; slaved segdef, save offset of master segdef in used segment canonical table
	mov bx,ds:[8]           ; get canonical table offset+1
	mov si,bx
	shl bx,1
	shl bx,1
	shl bx,1
	shl bx,1                ; shift to upper twelve bits
	or  es:[di],bx          ; merge in canonical offset+1, shifted 20 bits (16+4)

	mov es,ax               ; es -> master segdef
	mov ax,es:[2]           ; get master segdef offset low word
	mov bx,es:[4]           ; get offset high word
	mov di,si               ; di holds canonical table offset+1
	dec di                  ; make relative zero
	add di,di
	add di,di               ; make dword offset in table
	mov es,cs:ovl_usedcan_tbl_seg
	stosw                   ; save canonical low word
	mov ax,bx
	stosw                   ; save canonical high word

os_nextseg:
	loop    os_segentloop

os_nextsegblk:
	mov ds,dx               ; restore ds -> segdef block
	mov ax,ds:[0]           ; ds -> next block
	jmp NEAR PTR os_segblkloop

os_ovlseg:
	mov di,ds:[14]          ; get overlay id
	dec di                  ; make relative zero
	add di,di				; word offset in table
	mov bl,ds:[29]          ; get DDL library number
	xor bh,bh
	add bx,bx               ; convert to word offset
	mov ax,cs:[bx+OFFSET handle_array]  ; get file handle in ax, ah should be zero (handles < 256)
	mov	es,cs:ovl_handle_tbl_seg
	mov	es:[di],ax			; save file handle

	add di,di               ; make dword offset in table
	mov ax,ds:[20]          ; get file position low word
	mov es,cs:ovl_filepos_tbl_seg
	stosw                   ; save file position low word
	mov ax,ds:[22]          ; get file position high word
	stosw                   ; save file position high word
	jmp SHORT os_nextseg

; step through overlaid modules, allocate overlay stash entry, update overlay
; stash position table for @ overlaid segdef, write offset to segdefs,
; offset to grpdefs, extdef list, segdef list, grpdef list
os_stash:
	mov dx,cs:extdef_seg    ; get first extdef block

os_modblkloop:
	mov ds,dx
	mov al,ds:[8]           ; get flag byte
	and al,14h              ; see if overlaid module
	je  os_nextmodblk       ; no
	mov bx,ds:[0]           ; get count of extdef entries
	add bx,bx
	add bx,bx               ; 4 bytes/extdef
	mov si,bx               ; si == 4*extdef value
	mov bp,bx

	mov ax,ds:[10]          ; see if any segdefs
	or  ax,ax
	je  os_noseg            ; no
	mov es,ax
	mov ax,es:[2]           ; get segdef count
	add ax,ax
	add bx,ax               ; bx == 4*extdef+2*segdef
	mov bp,bx               ; bp == 4*extdef+2*segdef count

os_noseg:
	mov ax,ds:[12]          ; see if any grpdefs
	or  ax,ax
	je  os_nogrp
	mov es,ax
	mov ax,es:[2]           ; get grpdef count
	add ax,ax
	add bx,ax               ; bx == 4*extdef+2*segdef+2*grpdef

os_nogrp:
	mov ax,bx
	add ax,19               ; ax == 4+4*extdef+2*segdef+2*grpdef+15 (number of bytes in stash entry rounded up)
	mov cl,4
	shr ax,cl               ; ax holds paras to allocate for stash entry
	mov cx,ax
	mov	ax,cs:ovl_alloc_start
	sub ax,cx
	mov	cs:ovl_alloc_start,ax	; update start of overlay allocations
	cmp ax,cs:filepos_seg   ; see if overflow into real data
	jae os_memokay          ; no
	jmp NEAR PTR mem_error  ; not enough memory

os_nextmodblk:
	mov dx,ds:[6]           ; get pointer to next extern list
	or  dx,dx               ; see if at end block
	jne os_modblkloop       ; not yet
	jmp NEAR PTR os_rangechk

os_memokay:
	mov ax,ds:[10]          ; see if any segdefs
	or  ax,ax
	je  os_savestash        ; no

	mov es,ax               ; es -> segdef block
	mov bx,es:[2]           ; get count of entries in block
	or  bx,bx
	je  os_savestash        ; no entries

	push    ds
	dec ax
	mov ds,ax               ; preset ds for first entry

os_stashloop:
	mov ax,ds               ; get old entry pointer
	inc ax
	inc ax
	mov ds,ax               ; ds -> next entry in block

	mov al,ds:[28]          ; get overlay flags
	and al,14h
	je  os_stashnext        ; not overlaid

	mov di,ds:[14]          ; di == overlay id
	dec di
	add di,di               ; make word offset relative 0

	push    es
	mov es,cs:ovl_stash_tbl_seg
	mov ax,cs:ovl_alloc_start
	stosw                   ; save -> current stash entry
	pop es

os_stashnext:
	dec bx                  ; drop count of segdef entries
	jne os_stashloop        ; more left to process
	pop ds

os_savestash:
	xor di,di
	mov es,cs:ovl_alloc_start	; es:di -> overlay stash entry
	mov ax,si               ; get 4*extdef count (offset to segdef-4)
	add ax,4
	stosw
	mov ax,bp               ; get 4*extdef+2*segdef count (offset to grpdef-4)
	add ax,4
	stosw

; es:di -> start of extdef list in overlay stash entry
	mov cx,ds:[0]           ; get count of extdef entries
	jcxz    os_saveseg      ; no entries exist
	mov si,14               ; offset into extdef entries, past 7 system words

os_modentloop:
	lodsw
	mov ds,ax               ; ds -> public entry
	mov al,ds:[15]
	and al,1                ; see if overlaid public
	mov bl,al               ; save overlaid status, KEEP FLAGS
	je  os_4                ; no

	mov ax,ds:[2]           ; get overlay id
	or  ah,80h              ; set high bit to flag overlaid
	jmp SHORT os_savepub

os_4:
	mov ax,ds:[4]           ; save offset in overlay used table

os_savepub:
	stosw                   ; save public portion of dword entry
	mov al,ds:[14]
	and al,3
	cmp al,3                ; see if absolute public
	jne os_notabsext        ; no

; absolute segment, set bit 14 in public portion of dword entry
	or  BYTE PTR es:[di-1],40h
	mov ax,ds:[2]           ; get frame
	stosw                   ; save segment portion of dword entry
	jmp SHORT os_nextmodent ; save it

os_notabsext:
	mov ds,ds:[0]           ; ds -> segdef entry
	mov ax,ds:[12]          ; get offset in segment used table
	or  bl,bl               ; see if overlaid public (implies overlaid segment)
	je  os_savepubseg       ; no, use overlay segment used table
	mov ax,ds:[14]          ; get segment overlay id

os_savepubseg:
	stosw                   ; save segment portion of dword entry

	mov ax,ds:[16]          ; get group entry, if any
	or  ax,ax
	je  os_nextmodent       ; no group

	mov ds,ax
	mov ax,ds:[10]          ; see if master group entry
	or  ax,ax
	je  os_6                ; this is master
	mov ds,ax               ; ds -> master group entry

os_6:
	mov ax,ds:[4]           ; get entry in group table
	mov ah,al
	and al,7                ; get lower 3 bits of group entry (bits 2->0)
	shl al,1
	shl al,1
	shl al,1
	shl al,1                ; x4, shift to positions 6->4
	or  al,80h              ; set high bit to flag group
	or  es:[di-1],al        ; merge into bits 15->12 of segment entry
	and ah,18h              ; get middle 2 bits of group entry (4->3)
	shl ah,1                ; shift to positions 5->4
	or  es:[di-3],ah        ; merge into bits 13->12 of public entry

os_nextmodent:
	mov ds,dx               ; ds -> external block
	loop    os_modentloop

; save entry values in main segment table for each used segdef
os_saveseg:
	mov ax,ds:[10]          ; get -> segdefs
	or  ax,ax               ; see if any
	je  os_savegrp          ; no
	mov ds,ax
	mov cx,ds:[2]           ; get count of segdefs in block
	jcxz    os_savegrp      ; no segdefs
	dec ax                  ; setup for first segdef entry computation
	mov ds,ax

os_savesegloop:
	mov ax,ds
	inc ax
	inc ax
	mov ds,ax               ; ds -> next segdef entry
	mov al,ds:[28]
	and al,14h              ; see if segment is overlaid
	je  os_segusedstash     ; no
	mov ax,ds:[14]          ; get segment overlay id
	or  ah,80h              ; set high bit to flag overlaid
	jmp SHORT os_segstash

os_segusedstash:
	mov ax,ds:[12]          ; get entry in overlay used segment table

os_segstash:
	stosw                   ; save it
	loop    os_savesegloop  ; loop until complete

; save entry values in main group table for each used grpdef
os_savegrp:
	mov ds,dx               ; ds -> external block
	mov ax,ds:[12]          ; get -> grpdefs
	or  ax,ax               ; see if any
	je  to_os_nextmodblk    ; no

	mov ds,ax
	mov cx,ds:[2]           ; get count of grpdefs in block
	jcxz    to_os_nextmodblk    ; no grpdefs
	inc ax
	mov si,ax               ; ax -> first entry

os_savegrploop:
	mov ds,si               ; ds -> next grpdef entry
	mov ax,ds:[10]          ; see if master group entry
	or  ax,ax
	je  os_5                ; this is master
	mov ds,ax               ; ds -> master group entry

os_5:
	mov ax,ds:[4]           ; get entry in overlay used group table
	stosw                   ; save it
	inc si                  ; move to next entry
	loop    os_savegrploop  ; loop until complete

to_os_nextmodblk:
	mov ds,dx               ; ds -> external block
	jmp NEAR PTR os_nextmodblk

; check if used groups > 31 or used segments > 4095 or used publics > 4095
; or overlaid segments > 4095 or overlaid publics w/nonzero offset > 4095
; if so then abort with out of memory error
os_rangechk:
	mov ax,cs:ovl_used_grp
	cmp ax,32
	jae oom_err             ; too many groups
	mov bx,4096
	mov ax,cs:ovl_used_seg
	cmp ax,bx  
	jae oom_err             ; too many segments
	mov ax,cs:ovl_used_pub
	cmp ax,bx  
	jae oom_err             ; too many publics
	mov ax,cs:ovl_seg_count
	cmp ax,bx  
	jae oom_err             ; too many overlaid segments
	mov ax,cs:ovl_pub_count
	cmp ax,bx  
	jae oom_err             ; too many overlaid publics w/nonzero offset
	ret

oom_err:
	jmp NEAR PTR mem_error  ; not enough memory

ovl_setup   ENDP

;*****************************
;* ALLOC_640K_XMS            *
;*****************************

; check that XMS exists, setup if so
; upon entry ds -> code segment data
; returns carry flag set on error, reset if okay
; destroys ax,bx,cx,dx,es,ds

alloc_640k_xms	PROC	NEAR
	call	xms_check_setup	; get XMS status
    jc  ax_noxms			; XMS access failed

; attempt to allocate 640K of XMS
	mov	dx,640				; requested block size in dx
	mov	ah,9				; allocate extended memory block
    call    DWORD PTR ds:xms_addr
    or  ax,ax               ; see if error
    je  ax_noxms			; yes, don't use XMS
    mov ds:emsxms_handle,dx
    clc						; return success indication
    ret

; no, or can't use, XMS
ax_noxms:
    stc                     ; set carry flag to indicate error
	ret
alloc_640k_xms	ENDP

;*****************************
;* ALLOC_640K_EMS            *
;*****************************

; check that EMS exists, setup if so
; upon entry ds -> code segment data
; returns carry flag set on error, reset if okay
; destroys ax,bx,cx,dx,es,ds

alloc_640k_ems	PROC	NEAR
	call	lim_ems40_check	; get LIM EMS 4.0 status
    jc  ae_noems			; EMS access failed

; attempt to allocate 640K of EMS
	mov	bx,40				; 40 pages (640K)
	mov	ah,43h				; allocate handle and pages
	int	67h
    or  ah,ah               ; check for error
    jne ae_noems			; error occurred
    mov ds:emsxms_handle,dx
    clc						; return success indication
    ret

; no, or can't use, EMS 4.0
ae_noems:
    stc                     ; set carry flag to indicate error
	ret
alloc_640k_ems	ENDP

;*****************************
;* EMS_MOVE                  *
;*****************************

; transfer to/from EMS
; destroys ax,si

ems_move	PROC
    mov ax,5700h            ; move memory region
    mov si,OFFSET ebuff		; ds:si -> parameter block
    int 67h
    or  ah,ah				; see if error occurred
    jne	em_err				; yes
    ret

; EMS error occurred
em_err:
    mov al,ah               ; get 8 bit error code in al
	jmp	NEAR PTR ems_error
ems_move	ENDP

;*****************************
;* XMS_MOVE                  *
;*****************************

; transfer to/from XMS
; destroys ax,si

xms_move	PROC
	push	bx				; save critical register
	mov	ax,WORD PTR ds:xbuff.xs_len
	push	ax				; save length in case of odd
	push	WORD PTR ds:xbuff.xs_len+2
	test	al,1			; see if odd
	je	xm_2				; even
	add	WORD PTR ds:xbuff.xs_len,1
	adc	WORD PTR ds:xbuff.xs_len+2,0

xm_2:
    mov ah,0bh              ; move extended memory block
    mov si,OFFSET xbuff		; ds:si -> parameter block
    call    DWORD PTR ds:xms_addr
    or  ax,ax
    je  xm_err				; error occurred
	pop	WORD PTR ds:xbuff.xs_len+2	; restore original length
	pop	WORD PTR ds:xbuff.xs_len
	pop	bx					; restore critical register
    ret

xm_err:
    mov al,bl               ; get 8 bit error code in al
	jmp	NEAR PTR xms_error
xms_move	ENDP

;*****************************
;* ZERO_MEM_BLOCK            *
;*****************************

; zero bytes and transfer to EMS/XMS destination offset
; at image_written
; upon entry ax:cx contain count of bytes to zero+image_written
; destroys ax,cx,dx,di,si

zero_mem_block  PROC    NEAR
	push    es				; save critical register
	push	ds
	push	cs
	pop	ds					; ds -> code segment
	mov	si,cx
	mov dx,ax				; dx:si hold byte count+image_written
	mov ax,WORD PTR ds:image_written
	sub si,ax
	mov ax,WORD PTR ds:image_written+2
	sbb dx,ax				; dx:si holds true byte count

	mov ax,si
	or  ax,dx				; make sure that memory image size is nonzero
	jne	zmi_nonzero
	jmp	NEAR PTR zmi_ret	; zero memory image size

; zero 16K block at prog_seg for transfer
zmi_nonzero:
	mov	es,ds:prog_seg
	mov	cx,8192				; 8K words
	xor	ax,ax
	mov	di,ax
	rep	stosw

	mov	cx,si				; dx:cx holds byte count

; zero EMS/XMS variables, ax == 0
	mov	ds:ebuff.es_stype,al	; conventional memory source type
	mov	ds:ebuff.es_shandle,ax	; source handle
	mov	WORD PTR ds:ebuff.es_len+2,ax
	mov	ds:xbuff.xs_src_handle,ax	; source handle
	mov	WORD PTR ds:xbuff.xs_len+2,ax
	mov	WORD PTR ds:xbuff.xs_src_offset,ax	; source offset, offset
	mov	ds:ebuff.es_soffset,ax	; source offset

; init EMS/XMS variables
	mov	ax,ds:emsxms_handle
	mov	ds:ebuff.es_dhandle,ax	; EMS destination handle
	mov	ds:xbuff.xs_dest_handle,ax	; XMS destination handle
	mov	ax,ds:prog_seg
	mov	ds:ebuff.es_ssegpage,ax	; EMS source segment
	mov	WORD PTR ds:xbuff.xs_src_offset+2,ax	; XMS source offset, segment

	mov	si,WORD PTR ds:image_written+2
	mov	WORD PTR ds:xbuff.xs_dest_offset+2,si
	mov	ax,WORD PTR ds:image_written
	mov	WORD PTR ds:xbuff.xs_dest_offset,ax

	push	ax
	and	ah,3fh
	mov	ds:ebuff.es_doffset,ax	; offset of destination
	pop	ax
	shl	ax,1				; get top two bytes into high word for page number
    rcl si,1
	shl	ax,1
    rcl si,1
	mov	ds:ebuff.es_dsegpage,si	; logical page number

zmi_loop:
	mov	ax,cx				; get byte count load word

zmi_boundchk:
	cmp	ax,4000h			; see if more than zero'd bytes
	jb	zmi_2				; no

; higher than 16K
zmi_16k:
	mov	ax,4000h
	jmp	SHORT zmi_doit

zmi_2:
	or	ax,ax				; see if any bytes to transfer
	jne	zmi_doit			; yes

; zero bytes in low word
	or	dx,dx				; see if any bytes in high word
	je	zmi_ret				; no, done
	jmp	SHORT zmi_16k		; use up 16K of it

zmi_doit:
	cmp	ds:xms_flag,0		; see if zero'ing image in XMS
	jne	zmi_to_xms			; yes

; copy to EMS
	mov	WORD PTR ds:ebuff.es_len,ax
	mov	ds:ebuff.es_dtype,1	; expanded memory destination type
	call	ems_move
	mov	ax,WORD PTR ds:ebuff.es_len
	jmp	SHORT zmi_next

; copy to XMS
zmi_to_xms:
	mov	WORD PTR ds:xbuff.xs_len,ax
	call	xms_move
	mov	ax,WORD PTR ds:xbuff.xs_len

; ax holds length of last transfer
; dx:cx holds total byte count left to transfer
zmi_next:
	sub	cx,ax
	sbb	dx,0
	add	WORD PTR ds:xbuff.xs_dest_offset,ax
	adc	WORD PTR ds:xbuff.xs_dest_offset+2,0
	add	ds:ebuff.es_doffset,ax
	cmp	ds:ebuff.es_doffset,4000h
	jb	zmi_loop
	sub	ds:ebuff.es_doffset,4000h
	inc	ds:ebuff.es_dsegpage
	jmp	SHORT zmi_loop

zmi_ret:
	pop	ds					; restore critical register
	pop es
	ret
zero_mem_block  ENDP

; end of code marker to compute DDL manager size
endcode EQU $

cseg    ends
end
