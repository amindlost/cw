;*********************************************************************
;*   CNDDLMG1.ASM                                                    *
;*   By:            Michael Devore                                   *
;*   Date:          04/24/93                                         *
;*   Version:       2.51                                             *
;*   Assembler:     TASM 2.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*   Copyright 1990-93, Michael E. Devore                            *
;*                                                                   *
;*   Clarion DDL manager code (overlaid & resident stuff)            *
;*                                                                   *
;*********************************************************************

TITLE   CNDDLMG1.ASM
PAGE    50,80

STDOUT  EQU 1
CR      EQU     13          ; carriage return
LF      EQU     10          ; line feed
DDL_HEADER_SIZE EQU 128     ; size of DDL header
BIN_HEADER_SIZE EQU 20      ; size of segment binary header
FIX_HEADER_SIZE EQU 30      ; size of fixup header
DDL_STACK_SIZE  EQU 320     ; size of DDL manager stack

; pubdef entry modifications:
; [2] -> overlay id
; [4] -> offset in overlay used public table (if not overlaid)
; [15] and 1 == overlaid
; [15] and 8 == used by overlays

; segdef entry modifications:
; [8] -> offset in canonical table (if slave)
; [12] -> entry number in overlay used segment table (if not overlaid)
; [14] -> overlay id (if overlaid)
; [27] and 20h == segment used by overlay
; [28] and 14h == segment is overlaid

; grpdef entry modifications:
;   [4] -> entry number in overlay used group table
;   [14] and 1 == used by overlays

; structures
DDL_HEADER_STRUC    STRUC
	dh_sig1 DB  ?           ; DDL file signature bytes
	dh_sig2 DB  ?
	dh_sig3 DB  ?
	dh_sig4 DB  ?
	dh_majorver DB  ?       ; major version number
	dh_minor1   DB  ?       ; minor version number 1
	dh_minor2   DB  ?       ; minor version number 2
	dh_minor3   DB  ?       ; minor version alpha
	dh_hdrsize  DW  ?       ; size of module header
	dh_loadsize DW  ?       ; size of DDL loader
	dh_loadstart    DD  ?   ; file position of start of loader
	dh_flags    DD  ?       ; DDL flags
	                        ; bit 0==main module flag
	                        ; bit 1==required root modules flag
	                        ; bit 2==required overlay modules flag
	                        ; bit 3==elective root modules flag
	                        ; bit 4==elective overlay modules flag
	                        ; bit 5==DOSSEG flag
	                        ; bit 6==contains pre-loading routine
	dh_op       DD  ?       ; /op option value
	dh_st       DW  ?       ; /st option value
	dh_as       DW  ?       ; /as option value
	dh_os       DW  ?       ; /os option value
	dh_ol       DW  ?       ; /ol option value
	dh_mem      DB  ?       ; 0==free mem op, nonzero==alloc mem op
	dh_minop    DB  ?       ; 0==normal /op, 1==/op:m
	dh_ox       DB  ?       ; 0==regular memory for op, nonzero==EMS page frame for op
	dh_r        DB  ?       ; /r option setting
	dh_cla      DB  ?       ; /cla option setting
	dh_ou       DB  ?       ; /ou flag
	dh_ort      DB  ?       ; /ort setting
	dh_orp      DB  ?       ; /orp setting
	dh_modcount DW  ?       ; module count in DDL
	dh_ddlcount DW  ?       ; count of DDL's in dependency list (for main module)
	dh_reqroot  DW  ?       ; count of required root modules in DDL
	dh_reqovl   DW  ?       ; count of required overlay modules in DDL
	dh_elecroot DW  ?       ; count of elective root modules in DDL
	dh_elecovl  DW  ?       ; count of elective overlay modules in DDL
	dh_ddlstart DD  ?       ; file position of start of DDL dependency list
	dh_preload  DD  ?       ; file position of pre-load module
	dh_modstart DD  ?       ; file position of start of DDL module file position dword entries
	dh_dictstart    DD  ?   ; file position of start of DDL dictionary

	dh_ohp3		DB	?		; flag ohp3 use
	dh_ohp_flag	DB	?		; nonzero if /ohp allocate to amount flag set
	dh_oht_flag	DB	?		; nonzero if /oht allocate to amount flag set
	dh_pad		DB	?		; pad value
	dh_ohp		DW	?		; /ohp size in K
	dh_oht		DW	?		; /oht size in K

	dh_reser3   DD  ?       ; reserved for future
	dh_reser4   DD  ?       ; reserved for future
	dh_oxevar   DB  32 DUP (?)  ; specified /ox environment variable
DDL_HEADER_STRUC    ENDS

MOD_HEADER_STRUC    STRUC
	mh_flags    DD  ?       ; module flags
	                        ; bit 0==main module flag
	                        ; bit 1==required root module flag
	                        ; bit 2==required overlay module flag
	                        ; bit 3==elective root module flag
	                        ; bit 4==elective overlay module flag
	                        ; bit 6==pre-load module
	                        ; bit 7==contains communal variables (COMDEFs)
	mh_id       DW  ?       ; module identifier
	mh_segcount DW  ?       ; count of segments in module
	mh_grpcount DW  ?       ; count of groups in module
	mh_pubcount DW  ?       ; count of publics
	mh_comcount DW  ?       ; count of communals
	mh_extcount DW  ?       ; count of externals
	mh_lnames   DD  ?       ; file position of start of lnames name block
	mh_segdef   DD  ?       ; file position of start of segment entries
	mh_grpdef   DD  ?       ; file position of start of group entries
	mh_symbols  DD  ?       ; file position of start of symbols (pub/ext/comdef) name block
	mh_pubdef   DD  ?       ; file position of start of pubdef entries
	mh_comdef   DD  ?       ; file position of start of comdef entries
	mh_extdef   DD  ?       ; file position of start of extdef entries
	mh_binary   DD  ?       ; file position of start of binary data
	mh_startup  DB  7 DUP (?)   ; start address if main module in fixup notation
	mh_pad      DB  ?       ; pad to keep at dword boundary
	mh_binfpos  DD  ?       ; file position of each binary entry file position table
	mh_reser2   DD  ?       ; reserved for future
MOD_HEADER_STRUC    ENDS

BIN_HEADER_STRUC    STRUC
	bhs_flags   DD  ?       ; flags
	                        ; bit 0==overlay class flag
	bhs_segind  DW  ?       ; segment index
	bhs_length  DW  ?       ; length of segment binary data (not necessarily segment length)
	bhs_fixptr  DD  ?       ; file position of start of fixups for segment (0 if none)
	bhs_nextptr DD  ?       ; file position of next binary block (0 if last)
	bhs_offset  DW  ?       ; start offset within segment
	bhs_truelen DW  ?       ; true length of segment
BIN_HEADER_STRUC    ENDS

FIXUP_HEADER_STRUC  STRUC
	fhs_lowcount    DW  ?   ; count of low-order byte fixups
	fhs_lowinfo     DD  ?   ; file position of low-order byte fixup info entries
	fhs_lowloc      DD  ?   ; file position of low-order byte fixup locations
	fhs_nearcount   DW  ?   ; count of near fixups
	fhs_nearinfo    DD  ?   ; file position of near fixup information entries
	fhs_nearloc     DD  ?   ; file position of near fixup locations
	fhs_farcount    DW  ?   ; count of far fixups
	fhs_farinfo     DD  ?   ; file position of far fixup information entries
	fhs_farloc      DD  ?   ; file position of far fixup locations
FIXUP_HEADER_STRUC  ENDS

; publics
PUBLIC	ovl_alloc_start,ovl_alloc_end
PUBLIC  setup_and_go,ovlmgr_vector
PUBLIC  prog_seg,top_of_mem,prog_psp
PUBLIC  start_offset,start_segment
PUBLIC  binheader_seg,ddl_mainhdr_seg
PUBLIC  segment_start
PUBLIC  ovl_seg_count
PUBLIC  call_tbl_offset
PUBLIC  stack_offval,stack_segval
PUBLIC  typeflag
PUBLIC  is_absseg,is_abspub,is_resolved,fixdat
PUBLIC  target_index,target_segment,frame_index
PUBLIC  target_prog_off,frame_offset,lseg_canon
PUBLIC  frame_method,target_method

PUBLIC	xms_check_setup,lim_ems40_check
PUBLIC	free_640k_alloc
PUBLIC	ems_flag,xms_flag
PUBLIC	emsxms_handle
PUBLIC	image_written

PUBLIC	xms_addr
PUBLIC  ovl_usedpub_tbl_seg,ovlpub_tbl_seg
PUBLIC	xbuff,ebuff
PUBLIC	largest_ovl,second_ovl
PUBLIC	ovl_stash_tbl_seg,ovl_correl_tbl_seg,ovl_filepos_tbl_seg
PUBLIC	ovl_usedseg_tbl_seg,ovl_usedcan_tbl_seg,ovl_usedgrp_tbl_seg
PUBLIC	ovl_handle_tbl_seg
PUBLIC	ovl_call_tbl_seg
PUBLIC	cut_here,startup
PUBLIC	overlay_id
PUBLIC	ovl_call_tbl_size

PUBLIC	_cla_xqu_offset,_cla_xqu_segment
PUBLIC	runit_offset,runit_segment
PUBLIC	beforerun_offset,beforerun_segment
PUBLIC	afterrun_offset,afterrun_segment
PUBLIC	_cla_restart_offset,_cla_restart_segment

; externals
EXTRN	start_here:NEAR

cseg    SEGMENT PARA PUBLIC 'CODE'
	assume  cs:cseg,ds:cseg

startup:
	jmp	NEAR PTR start_here	; do transient, disposable code placed at end

;*************************************************

; data used by DDL manager and overlay manager

;*************************************************

ems_flag	DB	0			; nonzero if using EMS for load image stashing
xms_flag	DB	0			; nonzero if using XMS for load image stashing
emsxms_handle	DW	?		; EMS/XMS handle
image_written   DD  0		; running address of last address written
binheader_seg   DW  ?       ; segment of module binary header (used by overlays)
ddl_mainhdr_seg DW  ?       ; segment of DDL header info for main DDL
ovl_alloc_start	DW	?		; segment of lowest overlay manager allocation
ovl_alloc_end	DW	?		; segment of highest overlay manager allocation+1
high_ovl_alloc  DW  ?       ; segment of highest overlay manager allocation from reusable code
ovl_seg_count   DW  0       ; count of overlaid segments
segment_start   DD  0       ; running start of segment being resolved
stack_offval    DW  0       ; program's initial stack pointer (SP) value
stack_segval    DW  0       ; program's initial stack segment (SS) value
prog_seg    DW  ?           ; program load address segment (PSP+10h)
top_of_mem  DW  ?           ; top of memory from PSP:[2]
start_offset    DW  0       ; offset of start address
start_segment   DW  0       ; segment of start address
reusable_code   DW  ?       ; paragraphs of reusable code (cut_here-start_here)
frame_index DW  ?           ; fixup frame index
target_index    DW  ?       ; target frame index
target_segment  DW  ?       ; target segment
target_prog_off DD  ?       ; target program offset
frame_offset    DD  ?       ; absolute frame offset in program
lseg_canon      DD  ?       ; use to compute canonical (normalized) segment value
	                        ; of LSEG (logical segment), used for LOC type fixup
typeflag    DB  ?           ; type of fixup flag, 0==low byte 1==near, 2==far
is_ovlrel   DB  ?           ; nonzero if base fixup is overlay load relative, instead of program load relative
is_absseg   DB  ?           ; nonzero if segment is absolute
is_abspub   DB  ?           ; nonzero if absolute public declaration
is_resolved DB  ?           ; nonzero if external index is unresolved
fixdat  DB  ?               ; fix dat field value for fixup
frame_method    DB  ?       ; frame method for fixup
target_method   DB  ?       ; target method for fixup
fix_info_off    DW  ?       ; running offset of fixup info location w/n fixup block
fix_loc_off     DW  ?       ; running offset of fixup locations location w/n fixup block
fix_size        DW  ?       ; size of fixup block
fix_info_end    DW  ?       ; end of current fixup information block
fix_loc_end     DW  ?       ; end of current fixup location block
ovl_fixup_fpos  DD  ?       ; position of current overlay fixup block
fix_lownear_count   DW  ?   ; count of low byte and near fixups
fix_done        DW  ?       ; count of fixups processed
ovl_fix_info_tbl    DW  680 DUP (?) ; overlay fixup information table (max 680 entries)
ovl_fix_loc_tbl     DW  340 DUP (?) ; overlay fixup location table (max 340 entries)
overlay_id  DW  0           ; running overlay identifier

;*************************************************

; overlay manager code starts

;*************************************************

fix_header  FIXUP_HEADER_STRUC  <0,0,0,0,0,0,0,0,0>
ovl_usedpub_tbl_seg DW  ?   ; segment of overlay used public table
ovlpub_tbl_seg  DW  ?       ; segment overlaid public with nonzero offset table
ovl_usedseg_tbl_seg DW  ?   ; segment of overlay used segment table
ovl_usedcan_tbl_seg DW  ?   ; segment of overlay used canonical segment table
ovl_usedgrp_tbl_seg DW  ?   ; segment of overlay used group table
ovl_stash_tbl_seg   DW  ?   ; segment of overlay stash position table
ovl_correl_tbl_seg  DW  ?   ; segment of overlay correlation table
ovl_filepos_tbl_seg DW  ?   ; segment of overlay file positions
ovl_handle_tbl_seg	DW	?	; segment of overlay file handles
ovl_call_tbl_seg    DW  ?   ; segment of overlay call table
call_tbl_offset DD  ?       ; offset of overlay call table from start of program
largest_ovl DW  0           ; size of largest overlay
second_ovl  DW  0           ; size of second largest overlay
ovldata_seg_adjust	DW	?	; overlay data segment adjustment value for code relocation
start_of_code	DW	?		; set of start of DDL manager code after relocation
stack_setting	DW	?		; /st option setting
progsize_paras	DW	?		; size of program in paragraphs
prog_end		DW	?		; end of program paragraph
ovl_size_para   DW  ?       ; size of overlay file in paragraphs
orig_alloc_start	DW	?	; start of original table allocations before relocation
alloc_tbl_size	DW	?		; size of table allocations in paras
alloc_strategy	DW	?		; DOS memory allocation strategy
high_alloc_addr	DW	?		; high memory allocation address after relocation
ovl_call_tbl_size	DW	?	; size of overlay call table in paragraphs

code_size_para	DW	?		; CODE segment size in paragraphs
dt_size_para	DW	?		; DT segment size in paragraphs
dat_size_para	DW	?		; DAT segment size in paragraphs

;*****************************
;* STACK                     *
;*****************************

    DW  160 DUP (?)
$$_ovl_mgr_stack_end    =   $

;*****************************
;* DATA                      *
;*****************************

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
ovl_file_handle	DW	0		; overlay file handle

$$_ovl_filepos_size DW  ?   ; size of file position table, # of entries, changed to # of bytes
;***$$_ovl_total_count  DW  ?   ; total number of overlays

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

; clarion internal var storage
_cla_xqu_offset	DW	?
_cla_xqu_segment	DW	?
runit_offset	DW	?
runit_segment	DW	?
beforerun_offset	DW	?
beforerun_segment	DW	?
afterrun_offset	DW	?
afterrun_segment	DW	?
_cla_restart_offset	DW	?
_cla_restart_segment	DW	?

ems_trans_block	LABEL	BYTE
; location of temporary swapfile buffer
swapfile_buffer LABEL   BYTE
; relocation table of overlay file (1K max each time, 512 entries)
$$_ovl_rel_table    EQU $
    DB  1024 DUP (?)
    DB  1024 DUP (?)

dt_load_seg     DW  ?       ; load segment of current overlay _DT segment
dat_load_seg    DW  ?       ; load segment of current overlay _DAT segment
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


swapfile_count  DW  0       ; count of active overlays in active overlay data swapfile
swapfile_size   DD  0       ; swap file size, updated
swapfile_basename   DB  'OVL_SWAP.$01',0
swapfile_extptr DW  OFFSET swapfile_fullname+11 ; -> digit byte of swapfile extension
swapfile_fullname   DB  'OVL_SWAP.$01',0,115 DUP (0) ; swap file name
swapfile_evar   DB  'OVL_SWAP=' ; swap file environment variable
swapfile_active DB  0       ; nonzero if swap file in active use

XMSBUFF_STRUC	STRUC
	xs_len  DD  ?           ; length of block in bytes
	xs_src_handle   DW  ?   ; source EMB handle
	xs_src_offset   DD  ?   ; source offset
	xs_dest_handle  DW  ?   ; destination EMB handle
	xs_dest_offset  DD  ?   ; destination offset
XMSBUFF_STRUC   ENDS
xbuff	XMSBUFF_STRUC	<>	; XMS buffer for stashing built image

EMSBUFF_STRUC STRUC
	es_len  DD  ?           ; length of block in bytes
	es_stype    DB  ?       ; source type (0==conventional, 1==expanded)
	es_shandle  DW  ?       ; source handle
	es_soffset  DW  ?       ; source offset
	es_ssegpage DW  ?       ; source memory segment or logical page number
	es_dtype    DB  ?       ; destination type (0==conventional, 1==expanded)
	es_dhandle  DW  ?       ; destination handle
	es_doffset  DW  ?       ; destination offset
	es_dsegpage DW  ?       ; destination memory segment or logical page number
EMSBUFF_STRUC ENDS
ebuff	EMSBUFF_STRUC	<>	; EMS buffer for stashing built image

OHTBUFF_STRUC	STRUC
    os_len  DD  ?           ; length of block in bytes
    os_src_handle   DW  ?   ; source EMB handle
    os_src_offset   DD  ?   ; source offset
    os_dest_handle  DW  ?   ; destination EMB handle
    os_dest_offset  DD  ?   ; destination offset
OHTBUFF_STRUC	ENDS
ohtbuff	OHTBUFF_STRUC	<>

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

ovl_alloc_fpos  DD  ?       ; file position of overlay allocations in swapfile
check_array DW  ?           ; address of array to check for loaded overlays
dt_file_offset	DD	?		; offset to _DT OVL in current overlay file
dat_file_offset	DD	?		; offset to _DAT OVL in current overlay file

;*****************************
;* CODE                      *
;*****************************

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

cla_bx_runval   DW  ?       ; value of bx for clarion RUN modify memory block
cla_es_runval   DW  ?       ; value of es for clarion RUN modify memory block
mod_mem_alloc   DW  ?       ; amount memory was modified to for overlay allocation
total_alloc DW  ?           ; total overlay allocations in paras
swapfile_handle DW  0       ; swap file handle
restart_back    DD  ?       ; address of _cla_restart+6, for vectoring back to patched routine
overlay_info_seg	DW	?	; allocated overlay information segment address

; if using overlays:
;    relocate DDL manager code low
;    adjust memory allocation
;    allocate memory high for DDL code and overlay internal tables
;    relocate DDL manager code and overlay internal tables to top of memory
;    set new top of memory
; clear to top of memory
; transfer image to conventional memory
; free allocated EMS/XMS
; if overlays make necessary dynamic allocations for /os /ol /op
; prepare stack, top of memory, restore ds,es,
; transfer to program

setup_and_go:
	cmp cs:ovl_seg_count,0	; see if using overlays
	jne	ovl_setup			; yes
	jmp	NEAR PTR zero_to_top	; no

; using overlays, do setup
ovl_setup:
	call	save_system_variables	; save system variables
	push	cs
	pop	ds					; ds -> DDL manager code
	call	$$_ovl_init		; allocate overlay tables and load area

; relocate code and tables low
	mov	ax,ovl_alloc_end
	mov	cx,ovl_alloc_start
	mov	orig_alloc_start,cx	; save original allocation start
	sub	ax,cx				; ax holds number of bytes to move in paras for table
	mov	alloc_tbl_size,ax	; save size of allocated tables in paras

	mov	es,prog_seg			; es -> table destination (low memory)
	mov	ds,cx				; ds -> table source
	xor	dx,dx
	shl	ax,1
	rcl dx,1				; x2
	shl	ax,1
	rcl dx,1				; x4
	shl	ax,1
	rcl dx,1				; x8
	shl	ax,1
	rcl dx,1				; x16, bytes to transfer in dx:ax

jb_loop:
	xor	si,si
	mov	di,si				; zero init offsets
	or	dx,dx				; see if 64K bytes or more to move
	je	jb_2				; no

jb_max:
	mov	cx,0fff0h			; move 64K-16 bytes
	jmp	SHORT jb_move

jb_2:
	mov	cx,ax
	cmp ax,0fff0h			; see if more than 64K-16 bytes to move
	ja  jb_max				; yes, use maximum of 64K-16 bytes

jb_move:
	push	cx				; save byte count transferred
	shr	cx,1				; convert to words
	rep	movsw
	pop	cx					; cx == byte count transferred
	sub	ax,cx				; subtract off bytes transferred
	sbb	dx,0
	mov cx,dx
	or  cx,ax				; see if any bytes left to move
	je  jb_done				; no

	mov cx,ds
	add cx,0fffh			; bump to next source segment
	mov ds,cx
	mov cx,es
	add cx,0fffh			; bump to next transfer segment
	mov es,cx
	jmp	SHORT jb_loop

; internal tables located low
; locate DDL manager code right above tables and stack
jb_done:
	push	cs
	pop	ds					; ds -> DDL manager code
	mov	cx,OFFSET cut_here
	sub	cx,OFFSET startup	; keep byte count to move
	mov	ax,prog_seg
	add	ax,alloc_tbl_size

; relocate stack pointers so stack space won't be transferred and freed
    mov dx,0feh
	cli                     ; shut off interrupts while stack is set
	mov ss,ax
	mov sp,dx               ; set internal stack
	sti                     ; turn interrupts back on

	add	ax,10h				; allow for 256 byte stack
	mov	es,ax				; es -> just above relocated tables and stack
	xor	si,si
	mov	di,si				; zero offsets
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
	push	es
	mov	ax,OFFSET jumpback
	push	ax
	retf					; transfer to code in low memory

; code now operating in low memory after relocation
; shrink memory allocation
jumpback:
	push	cs
	pop	ds					; ds -> code segment data

	mov	cl,4
	add	di,15				; round up to next para
	shr	di,cl				; convert bytes moved to paras
	mov	ax,es
	add	ax,di				; ax -> para above last move
	sub	ax,prog_seg			; bytes of code+tables+stack
	add	ax,10h				; adjust for PSP
	mov	es,prog_psp			; es -> PSP for memory allocation shrink
	mov	bx,ax				; bx holds new memory size
	mov	ah,4ah				; resize memory block
	int	21h
	jnc	low_ok				; low allocation okay

to_memerr:
	jmp	NEAR PTR memerr		; force out of memory error

; get old memory allocation strategy, save it
; set memory allocation strategy to use highest available block that fits
; allocate block for relocated DDL manager code and tables
; restore old memory allocation strategy
low_ok:
	mov	ax,5800h			; get allocation strategy
	int	21h
	mov	alloc_strategy,ax	; save allocation strategy
	mov	bx,2				; use highest available block
	mov	ax,5801h			; set allocation strategy
	int	21h
	mov	bx,top_of_mem
	sub	bx,ovl_call_tbl_seg	; get amount of memory needed for DDL manager+overlay call table
	add	bx,alloc_tbl_size	; add in internal overlay table allocations
	add	bx,8				; add 8 paras of slop at this end, too (16 total)
	mov	ah,48h
	int	21h
	pushf					; save carry status

	mov	high_alloc_addr,ax	; save high allocation address
	mov	bx,alloc_strategy	; get old allocation strategy
	mov	ax,5801h			; set allocation strategy
	int	21h
	popf					; get memory allocation status
	jc	to_memerr			; out of memory

; make sure that overlay call table segment and tables below will lie
; within this allocation
	mov	ax,ovl_call_tbl_seg
	sub	ax,alloc_tbl_size
	cmp	ax,high_alloc_addr
	jb	to_memerr			; tables outside of allocation

; transfer tables up to high memory
	mov	ax,alloc_tbl_size	;get size of allocated tables in paras in ax
	mov	es,high_alloc_addr	; es -> destination of tables (high memory)
	mov	ds,prog_seg			; ds -> table source
	xor	dx,dx
	shl	ax,1
	rcl dx,1				; x2
	shl	ax,1
	rcl dx,1				; x4
	shl	ax,1
	rcl dx,1				; x8
	shl	ax,1
	rcl dx,1				; x16, bytes to transfer in dx:ax

ja_loop:
	xor	si,si
	mov	di,si				; zero init offsets
	or	dx,dx				; see if 64K bytes or more to move
	je	ja_2				; no

ja_max:
	mov	cx,0fff0h			; move 64K-16 bytes
	jmp	SHORT ja_move

ja_2:
	mov	cx,ax
	cmp ax,0fff0h			; see if more than 64K-16 bytes to move
	ja  ja_max				; yes, use maximum of 64K-16 bytes

ja_move:
	push	cx				; save byte count transferred
	shr	cx,1				; convert to words
	rep	movsw
	pop	cx					; cx == byte count transferred
	sub	ax,cx				; subtract off bytes transferred
	sbb	dx,0
	mov cx,dx
	or  cx,ax				; see if any bytes left to move
	je  ja_done				; no

	mov cx,ds
	add cx,0fffh			; bump to next source segment
	mov ds,cx
	mov cx,es
	add cx,0fffh			; bump to next transfer segment
	mov es,cx
	jmp	SHORT ja_loop

; internal tables located high
; locate DDL manager code right above overlay call table
ja_done:
	push	cs
	pop	ds					; ds -> DDL manager code
	mov	cx,OFFSET cut_here
	sub	cx,OFFSET startup	; keep byte count to move
	mov	ax,ovl_call_tbl_seg
	add	ax,ovl_call_tbl_size
	mov	es,ax				; es -> just above overlay call table
	xor	si,si
	mov	di,si				; zero offsets
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
	push	es
	mov	ax,OFFSET jumpup
	push	ax
	retf					; transfer to code in low memory

; code now operating in high memory after relocation, relocate stack,
; expand memory allocation to just below new allocation
jumpup:
	push	cs
	pop	ds					; ds -> code segment data

; relocate stack back up to high memory
    mov cx,OFFSET $$_ovl_mgr_stack_end
	mov ax,cs
	cli                     ; shut off interrupts while stack is set
	mov ss,ax
	mov sp,cx               ; set internal stack
	sti                     ; turn interrupts back on

	mov	ax,high_alloc_addr
	sub	ax,prog_seg			; bytes for program
	add	ax,8				; +10h for PSP -8 for more slop factor

	mov	es,prog_psp			; es -> PSP for memory allocation shrink
	mov	bx,ax				; bx holds new memory size
	mov	ah,4ah				; resize memory block
	int	21h
	jnc	high_ok				; high allocation okay
	jmp	NEAR PTR memerr		; force out of memory error

high_ok:
	mov	dx,high_alloc_addr
	sub	dx,orig_alloc_start	; dx holds segment adjustment for shift in tables

; adjust the table addresses, and internal pointers where necessary
	mov	ax,ovl_stash_tbl_seg
	add	ax,dx
	mov	ovl_stash_tbl_seg,ax

	mov	ax,ovl_correl_tbl_seg
	add	ax,dx
	mov	ovl_correl_tbl_seg,ax

	mov	ax,ovlpub_tbl_seg
	add	ax,dx
	mov	ovlpub_tbl_seg,ax

	mov	ax,ovl_usedpub_tbl_seg
	add	ax,dx
	mov	ovl_usedpub_tbl_seg,ax

	mov	ax,ovl_usedgrp_tbl_seg
	add	ax,dx
	mov	ovl_usedgrp_tbl_seg,ax

	mov	ax,ovl_handle_tbl_seg
	add	ax,dx
	mov	ovl_handle_tbl_seg,ax

	mov	ax,ovl_filepos_tbl_seg
	add	ax,dx
	mov	ovl_filepos_tbl_seg,ax

	mov	ax,ovl_usedseg_tbl_seg
	add	ax,dx
	mov	ovl_usedseg_tbl_seg,ax

	mov	ax,ovl_usedcan_tbl_seg
	add	ax,dx
	mov	ovl_usedcan_tbl_seg,ax

	mov	ax,$$_ovl_array_start
	add	ax,dx
	mov	$$_ovl_array_start,ax
	mov	ax,$$_ovl_stack_start
	add	ax,dx
	mov	$$_ovl_stack_start,ax
	mov	ax,$$_ovl_load_start
	add	ax,dx
	mov	$$_ovl_load_start,ax
	mov	ax,$$_ovl_load_end
	add	ax,dx
	mov	$$_ovl_load_end,ax
	mov	ax,check_array
	add	ax,dx
	mov	check_array,ax
	mov	ax,binheader_seg
	add	ax,dx
	mov	binheader_seg,ax

; update internal pointers
	mov	es,ovl_stash_tbl_seg	; modify stash table pointers for segment delta
	xor	di,di
	mov	cx,ovl_seg_count

stash_loop:
	mov	ax,es:[di]
	add	ax,dx
	stosw
	loop	stash_loop

; setup overlay call table with near calls to start of table/entry
; and jump to overlay manager
	xor di,di               ; offset into call table
	mov es,ovl_call_tbl_seg
	mov al,0eah
	stosb                   ; set up far jump to overlay manager vector code
	mov ax,OFFSET ovlmgr_vector
	stosw
	mov ax,cs
	stosw

	mov dx,0fff8h           ; initialize CALL disp16 offset
	mov cx,overlay_id		; count of near calls

os_nearloop:
	mov al,0e8h
	stosb
	mov ax,dx
	stosw
	sub dx,3                ; adjust for next call
	loop    os_nearloop

; update termination vector and top of memory pointer
	mov	es,prog_psp
    mov es:[10],OFFSET new_terminate
    mov es:[12],cs
	mov ax,high_alloc_addr
	dec ax                  ; adjust for MCB paragraph
	mov top_of_mem,ax		; update top of memory pointer
	jmp	SHORT init_transfer

; no overlays, zero from image_written to DDL header info for main DDL
zero_to_top:
	push	cs
	pop	ds					; ds -> code segment data
	mov cx,ddl_mainhdr_seg
	sub cx,prog_seg			; make relative to start of program
	xor ax,ax				; convert paras in cx to bytes in ax:cx
	shl cx,1
	rcl ax,1				; x2
	shl cx,1
	rcl ax,1				; x4
	shl cx,1
	rcl ax,1				; x8
	shl cx,1
	rcl ax,1				; x16
	sub	cx,WORD PTR image_written	; adjust off program bytes that will overwrite
	sbb	ax,WORD PTR image_written+2
	call    zero_mem_block2

; transfer image_written bytes in EMS/XMS to prog_seg
init_transfer:
	push	cs
	pop	ds					; ds -> code segment
	mov	cx,WORD PTR ds:image_written
	mov	dx,WORD PTR ds:image_written+2
	mov	ax,ds:emsxms_handle
	cmp	ds:xms_flag,0		; see if image stashed in XMS
	jne	trans_from_xms		; yes

	mov	ds:ebuff.es_shandle,ax	; source handle
	mov	WORD PTR ds:ebuff.es_len,cx
	mov	WORD PTR ds:ebuff.es_len+2,dx
	mov	ds:ebuff.es_stype,1	; expanded memory source type
	mov	ax,ds:prog_seg
	mov	ds:ebuff.es_dsegpage,ax	; destination segment
	xor	ax,ax
	mov	ds:ebuff.es_dtype,al	; conventional memory destination type
	mov	ds:ebuff.es_dhandle,ax	; destination handle
	mov	ds:ebuff.es_doffset,ax	; offset of destination
	mov	ds:ebuff.es_ssegpage,ax	; logical page number
	mov	ds:ebuff.es_soffset,ax	; source offset
	call	ems_move2
	jmp	SHORT trans_done

trans_from_xms:
	mov	ds:xbuff.xs_src_handle,ax	; source handle
	mov	WORD PTR ds:xbuff.xs_len,cx
	mov	WORD PTR ds:xbuff.xs_len+2,dx
	mov	ax,ds:prog_seg
	mov	WORD PTR ds:xbuff.xs_dest_offset+2,ax	; destination offset, segment
	xor	ax,ax
	mov	ds:xbuff.xs_dest_handle,ax	; destination handle
	mov	WORD PTR ds:xbuff.xs_dest_offset,ax	; destination offset, offset
	mov	WORD PTR ds:xbuff.xs_src_offset,ax	; offset of source
	mov	WORD PTR ds:xbuff.xs_src_offset+2,ax
	call	xms_move2

trans_done:
	call	free_640k_alloc	; free up the 640K allocation, set cs==ds
	cmp cs:ovl_seg_count,0	; see if using overlays
	je	init_stack			; no

; using overlays, do swapfile stuff and patch the Clarion routines
    call    open_clar_swapfile  ; open the active overlay data swap file
    call    set_cla_xqu_vect    ; set new vector for overlay manager

; prepare stack
init_stack:
	mov ax,ds:stack_setting	; get /st option setting
	or  ax,ax               ; zero if no /st
	jne setstack            ; use /st setting

	mov ax,ds:stack_offval  ; set up stack

setstack:
	sub ax,2                ; for compatibility with DOS loader setup

	mov bx,ds:stack_segval
	add bx,ds:prog_seg      ; relocate for loaded program
	cli
	mov ss,bx
	mov sp,ax
	sti

; restore critical register(s) to proper value(s)
	mov ax,ds:prog_psp
	mov ds,ax
	mov es,ax

; prepare top of memory
	mov ax,cs:top_of_mem    ; restore top of memory to original or overlay altered value
	mov es:[2],ax

; transfer to built program image
	jmp DWORD PTR cs:start_offset

;*****************************
;* FREE_640K_ALLOC           *
;*****************************

; free 640K EMS or XMS allocation
; destroys ax,bx,dx

free_640k_alloc	PROC	NEAR
	push	cs
	pop	ds
    mov dx,ds:emsxms_handle
    cmp ds:xms_flag,0      	; see if XMS to free
    je  fa_2                ; no
    mov ah,0ah              ; free extended memory block
    call    DWORD PTR ds:xms_addr

fa_2:
    cmp ds:ems_flag,0      	; see if EMS to free
    je  fa_3                ; no
    mov ah,45h              ; release EMS handle and pages
    int 67h

fa_3:
	ret
free_640k_alloc	ENDP

;*****************************
;* EMS_MOVE2                 *
;*****************************

; transfer to/from EMS
; destroys ax,si

ems_move2	PROC
    mov ax,5700h            ; move memory region
    mov si,OFFSET ebuff		; ds:si -> parameter block
    int 67h
    or  ah,ah				; see if error occurred
    jne	em_err				; yes
    ret

; EMS error occurred
em_err:
	jmp	NEAR PTR cee_err
ems_move2	ENDP

;*****************************
;* XMS_MOVE2                 *
;*****************************

; transfer to/from XMS
; destroys ax,si

xms_move2	PROC
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
	jmp	NEAR PTR xt_error
xms_move2	ENDP

;*****************************
;* ZERO_MEM_BLOCK2           *
;*****************************

; zero ax:cx bytes at memory block at
; (cs:prog_seg+image_written/16):(image_written mod 16)
; upon entry ax:cx contain count of bytes to zero
; destroys ax,cx,dx,di,si

zero_mem_block2	PROC	NEAR
    push    es              ; save critical register
    mov dx,ax               ; dx:cx hold byte count
    mov si,WORD PTR ds:image_written
    mov ax,WORD PTR ds:image_written+2	; ax:si hold image_written offset
    shr ax,1
    rcr si,1                ; /2
    shr ax,1
    rcr si,1                ; /4
    shr ax,1
    rcr si,1                ; /8
    shr ax,1
    rcr si,1                ; /16
    add si,ds:prog_seg      ; si:(di mod 0fh) -> start of block to zero
    mov es,si               ; es -> memory image
    mov ax,cx
    or  ax,dx               ; make sure that memory image size is nonzero
    je  zmi_ret             ; zero memory image size
    mov si,cx               ; low word count of bytes to zero in si

zmi_loop:
    and di,0fh              ; es:di -> block to zero
    xor ax,ax               ; zero word to store
    or  dx,dx               ; see if high word set
    jne zmi_2               ; yes, a 32K can be zero'ed
    cmp si,32768            ; see if 32K chunk to zero out in low word
    jb  zmi_3               ; no, zero out leftover amount

zmi_2:
    sub si,32768            ; subtract bytes zero'd
    sbb dx,0                ; borrow to high word
    mov cx,16384            ; zero 16K words (32K bytes)
    rep stosw               ; do it
    mov cx,es
    add cx,800h             ; adjust past 32K block zero'd (800h paragraphs)
    mov es,cx               ; update segment pointer
    jmp SHORT zmi_loop      ; loop back for next chunk to zero

zmi_3:
    mov cx,si               ; get byte count in cx
    shr cx,1                ; convert byte count to zero to words
    rep stosw               ; store zeros
    rcl cx,1                ; pick up carry
    rep stosw               ; zero leftover byte, if any

zmi_ret:
    pop es                  ; restore critical register
    ret
zero_mem_block2	ENDP

;*****************************
;* $$_OVL_CLA_XQU_VECT       *
;*****************************

; vector to here from _cla_xqu to transfer control to an overlay

$$_ovl_cla_xqu_vect PROC
    mov cs:ax_store,ax
    pushf                   ; save the flag register
    pop cs:flag_store

    mov ax,es               ; get segment of called routine
    cmp ax,cs:$$_ovl_load_start ; see if segment below overlay load area
    jb  clav_root           ; yes, root call
    cmp ax,cs:$$_ovl_load_end   ; see if segment above overlay load area
    jb  clav_ovl            ; no, overlay call

; call to root, from root (always?), continue without overlay manager intervention
clav_root:
    push    cs:flag_store
    popf                    ; restore flags
    mov ax,cs:ax_store      ; restore ax register
    push    es              ; perform clarion style call as per old _cla_xqu executor
    push    bx
    retf

; call to overlay, always from root (executor always in root)
clav_ovl:
    mov cs:bx_store,bx
    mov cs:cx_store,cx
    mov cs:ds_store,ds      ; save data segment, used for walking load overlay areas

    pop cx                  ; get calling routine offset in cx
    pop ax                  ; get calling routine segment in ax
    mov cs:call_offset,cx   ; save calling routine seg:offset
    mov cs:call_segment,ax
    cmp ax,cs:$$_ovl_load_start ; see if segment below overlay load area
    jb  clav_fromroot       ; yes, call from root
    cmp ax,cs:$$_ovl_load_end   ; see if segment above overlay load area
    jae clav_fromroot       ; yes, call from root

; call from overlay, ax holds load segment of calling overlay
    mov bx,cs
    mov ds,bx               ; ds -> overlay manager data
    mov bx,$$_ovl_array_start   ; bx -> overlay loaded array element
    xor cx,cx

; bx -> overlay loaded array element
; ax holds overlay load segment
; cx == 0
clav_loop:
    mov ds,bx               ; ds -> overlay loaded array element
    cmp cx,ds:[0]           ; see if this array element was used
    je  clav_next           ; no, try next one
    cmp ax,ds:[6]           ; see if calling segment matches current entry segment
    je  clav_found          ; yes, segment found

clav_next:
    inc bx                  ; bump to next overlay loaded array element
    jmp SHORT clav_loop     ; see if matches

clav_found:
    mov ax,ds:[0]           ; get id of calling routine
    mov cs:source_id,ax     ; save as source
    jmp SHORT clav_getdest  ; get destination overlay

; call from root
clav_fromroot:
    mov cs:source_id,0		; save id of calling routine as root

clav_getdest:
    mov bx,cs
    mov ds,bx               ; ds -> overlay manager data
    mov bx,$$_ovl_array_start   ; bx -> overlay loaded array element
    xor cx,cx
    mov ax,es               ; ax holds load segment of overlay being called

; bx -> overlay loaded array element
; ax holds overlay load segment
; cx == 0
clav_loop2:
    mov ds,bx               ; ds -> overlay loaded array element
    cmp cx,ds:[0]           ; see if this array element was used
    je  clav_next2          ; no, try next one
    cmp ax,ds:[6]           ; see if called segment matches current entry segment
    je  clav_found2         ; yes, segment found

clav_next2:
    inc bx                  ; bump to next overlay loaded array element
    jmp SHORT clav_loop2    ; see if matches

clav_found2:

    mov cs:jump_segment,ax  ; save segment of called routine

;***    mov ax,ds:[0]           ; get id of called routine

    mov ds,cs:ds_store      ; restore data segment

;***    mov cs:jump_segment,ax  ; pure overlay identifier goes in jump_segment
;***    or  ah,80h              ; flag far call in identifier for destination id
;***    mov cs:destination_id,ax    ; save id of called routine

    mov ax,cs:bx_store      ; initial bx storage holds called routine offset
    mov cs:jump_offset,ax

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

    pop ax                  ; get old calling routine's offset, throw away
    pop ax                  ; get calling routine's identification, throw away
    pop ax                  ; get calling routine's segment, throw away

    push    cs:call_segment ; put proper values on stack
    push    cs:source_id
    push    cs:call_offset

; restore stack to program values
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

; far call from calling routine
    push    cs:flag_store   ; get original flag value
    popf                    ; restore to flag word
    call    DWORD PTR cs:jump_offset    ; transfer control to overlay

    jmp NEAR PTR $$_ovl_ret ; upon return, transfer to overlay manager return handler

;***    jmp FAR PTR vect_3      ; transfer control to main overlay routine

$$_ovl_cla_xqu_vect ENDP

;*****************************
;* OVLMGR_VECTOR             *
;*****************************

; Call to or from overlaid segment
; Check if segment shows this is call from root or overlay
; If overlay walk the overlay loaded array checking the calling routine's
; segment against loaded overlays' segments to find the proper source id for bx

ovlmgr_vector   PROC
	mov cs:ax_store,ax      ; save ax,bx,cx,dx value
	mov cs:bx_store,bx
	mov cs:cx_store,cx
	mov cs:dx_store,dx
	mov cs:ds_store,ds      ; save data segment
	pushf                   ; save the flag register
	pop cs:flag_store
	pop ax                  ; get offset from near call to vector jump
	sub ax,8                ; adjust for jump vector code at beginning of segment and for call offset
	xor dx,dx               ; zero high word of dividend
	mov bx,3
	div bx                  ; call every three bytes, this gets overlay id number in ax

; ax now holds the overlaid identifer after division, relative zero
	inc ax                  ; make overlay id number relative 1

	pop cx                  ; get calling routine offset in cx
	pop dx                  ; get calling routine segment in dx

	xor bx,bx               ; init flag to call from root
	mov cs:call_offset,cx   ; save calling routine offset
	cmp dx,cs:$$_ovl_load_start ; see if segment below overlay load area
	jb  vect_saveinfo       ; yes, this is a call from the root
	cmp dx,cs:$$_ovl_load_end   ; see if segment above overlay load area
	jae vect_saveinfo       ; yes, call from root

; call from overlay to overlay
	mov bx,cs:$$_ovl_array_start    ; bx -> overlay loaded array element
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

vect_stackerr:
	mov ax,1                ; get error value in ax
	jmp NEAR PTR $$_ovl_op_error    ; stack overflow

ovl_seg_found:
	mov bx,ds:[0]           ; get overlay identifier in bx

vect_saveinfo:
	mov cs:call_segment,dx  ; save segment of calling routine
	mov cs:source_id,bx     ; save id of calling routine, 0 if root
	mov cs:destination_id,ax    ; save id of routine to call

; save program's stack pointer and segment
	mov ax,ss
	mov bx,sp               ; get program stack values
	mov cs:prog_ss_store,ax
	mov cs:prog_sp_store,bx ; save them

; set stack to DDL manager stack
    mov bx,OFFSET $$_ovl_mgr_stack_end
	mov ax,cs
	cli                     ; shut off interrupts while stack is set
	mov ss,ax
	mov sp,bx               ; set internal stack
	sti                     ; turn interrupts back on

; Make the distinction.  There is an overlaid public id, which does not
; necessarily correspond to the overlay file id (the overlay loaded when
; the public is called). IF calling from a segment fixup, or calling from
; a public with zero offset then public/segment id==file id, otherwise
; you have to look up the overlay file id from correlation table
; and the overlay public offset from an overlaid public offset table.

	xor ax,ax
	mov cs:jump_offset,ax   ; assume zero offset call
	mov ax,cs:destination_id    ; get id of routine to call
	cmp ax,cs:ovl_seg_count ; see if nonzero offset overlaid public
	jbe vect_2              ; no

; offset of public is nonzero, need to find offset and get overlay file id
; corresponding to this overlaid public id
	sub ax,cs:ovl_seg_count ; adjust for overlaid segments/zero offset publics
	dec ax                  ; make overlaid public id relative zero
	add ax,ax               ; make word offset
	mov bx,ax
	mov ds,cs:ovlpub_tbl_seg
	mov ax,ds:[bx]          ; get overlaid public offset
	mov cs:jump_offset,ax

	mov ds,cs:ovl_correl_tbl_seg
	mov ax,ds:[bx]          ; get overlay file id
	mov cs:destination_id,ax    ; update destination id with overlay file id

vect_2:
	mov cs:jump_segment,ax  ; save overlay file id, relative 1

	mov cs:is_calling_ovl,1 ; flag that overlay being called
	push	cs
	pop	ds					; ds -> code segment data
	call    $$_ovl_ready_overlay    ; load overlay if necessary, modify jump_segment

; set stack to calling routine seg/id/offset stack
	mov bx,$$_ovl_stack_ptr
	cmp bx,8                ; check for stack overflow
	jb  vect_stackerr       ; overflow

	mov ax,$$_ovl_stack_start
	cli                     ; shut off interrupts while stack is set
	mov ss,ax
	mov sp,bx               ; set internal stack
	sti                     ; turn interrupts back on

	push    destination_id	; save destination routine
	push    call_segment	; save segment to stack
	push    source_id		; save source id code to stack
	push    call_offset
	mov $$_ovl_stack_ptr,sp	; save new stack pointer value

; restore stack to program value
	mov ax,prog_ss_store
	mov bx,prog_sp_store	; get stack entry values
	cli                     ; shut off interrupts while stack is set
	mov ss,ax
	mov sp,bx               ; set internal stack
	sti                     ; turn interrupts back on

; restore ax,bx,cx,dx,ds to original values
	mov dx,dx_store
	mov cx,cx_store
	mov bx,bx_store
	mov ax,ax_store
	mov ds,ds_store			; restore data segment
	push    cs:flag_store   ; get original flag value
	popf                    ; restore to flag word

call_overlay_here:
	call    DWORD PTR cs:jump_offset    ; transfer control to overlay

    mov cs:cx_store,cx      ; save original ax,bx,cx
    mov cs:bx_store,bx      ; save original ax,bx
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
    pop cs:call_segment     ; get calling routine's segment

    push    cs:call_segment ; restore to stack
    push    cs:source_id
    push    cs:call_offset

; restore stack to program values
omc_restack:
    mov ax,cs:prog_ss_store
    mov bx,cs:prog_sp_store ; get stack entry values
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

; restore ax,bx to original values
    mov bx,cs:bx_store
    mov ax,cs:ax_store
    push    cs:flag_store   ; get original flag value
    popf                    ; restore to flag word
    jmp DWORD PTR cs:call_offset    ; transfer back to calling routine

ovlmgr_vector   ENDP

;*****************************
;* $$_OVL_MGR_RET            *
;*****************************

$$_ovl_mgr_ret  PROC

$$_ovl_ret:
	mov cs:ds_store,ds      ; save data segment
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

;***    cmp cs:$$_ovl_reload_flag,0 ; see if reload overlay option in effect
;***    je  omr_2               ; no, bypass reload overlay stack adjustment

; overlay called with reload option, extra info on stack
    pop cs:$$_ovl_stackcount
    mov cs:$$_ovl_stack_ptr,sp  ; save new stack pointer value

    mov cs:is_calling_ovl,0 ; flag that overlay being returned from

; set stack to overlay manager internal stack
omr_2:
    mov bx,OFFSET $$_ovl_mgr_stack_end
    mov ax,cs
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

;***    mov al,cs:$$_ovl_reload_flag
;***    or  al,al               ; see if reload overlay option in effect
;***    je  omr_3               ; no, bypass reload specific code

; drop activity level of overlay
	mov ax,cs:destination_id    ; get overlay file id

; overlay identifer in ax, search the overlays loaded array to find it
    cmp cs:last_access_slot,0
    je  deact_4
    mov ds,cs:last_access_slot  ; ds -> last access overlay slot
    cmp ax,ds:[0]           ; see if id matches last accessed id
    je  deact_found         ; yes, update the array element

deact_4:
    mov bx,cs:$$_ovl_array_start
    mov ds,bx                   ; ds -> overlay array

deact_loop:
    cmp ax,ds:[0]           ; see if array element matches
    je  deact_found         ; yes, update the array element
    inc bx
    mov ds,bx
    jmp SHORT deact_loop    ; loop until overlay is found

deact_found:
    dec WORD PTR ds:[4]     ; drop activity level
	jne	omr_3				; overlay still active
    mov WORD PTR ds:[0],0	; zero identifier
    dec cs:$$_ovl_loaded_count ; drop count of loaded overlays to reflect terminated overlay swapout

omr_restchk:
    cmp cs:$$_ovl_stackcount,0  ; see if any overlays to restore
    je  omr_3				; no

; this overlay will be swapped out by the overlays that it swapped out
; zero the array entry and drop the overlays loaded count
;***    xor ax,ax
;***    mov ds:[0],ax
;***    mov ds:[4],ax           ; zero activity level
;***    dec $$_ovl_loaded_count ; drop count of loaded overlays to reflect terminated overlay swapout
	push	cs
	pop	ds					; ds -> overlay manager data
    call    $$_ovl_swap_in  ; load the swapped out active overlays

omr_3:
	push	cs
	pop	ds					; ds -> overlay manager data
	mov ax,source_id
	or  ax,ax               ; see if calling routine was in root
    jne	omr_4				; no

; calling routine was in root
    push    ds
    push    si
    push    dx
    mov cx,$$_ovl_loaded_count
    mov ds,$$_ovl_array_start   ; ds -> overlay loaded array
    xor ax,ax
    mov bx,ax
    mov dx,ax
    mov cs:last_access_slot,ax

last_access_loop:
    cmp ax,ds:[0]           ; see if unused array element
    je  next_access_check   ; yes
    dec cx                  ; drop count of loaded overlays checked
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

omr_4:
    mov ax,source_id		; get source id
    mov jump_segment,ax		; place in jump_segment for get overlay ready routine

    mov is_calling_ovl,0	; flag that overlay being returned from
    call    $$_ovl_ready_overlay    ; load overlay if necessary, modify jump_segment

    mov ax,jump_segment		; get jump segment in call segment for return
    mov call_segment,ax

; restore stack to program values, restore registers, and return
mgret_exit:
    mov ax,prog_ss_store
    mov bx,prog_sp_store	; get stack entry values
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    mov cx,cx_store
    mov bx,bx_store
    mov ax,ax_store
	mov ds,ds_store			; restore data segment
    push    cs:flag_store   ; get original flag value
    popf                    ; restore to flag word
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
;   Clarion data segment size       DW  ?
;   Activity level, 0 if not active DW  ?
;   Overlay segment/start para      DW  ?
;   Overlay ending paragraph        DW  ?
;   LRU counter value               DD  ?
;   Filler to pad to one paragraph  DW  ?

$$_ovl_ready_overlay    PROC    NEAR
    push    es
    push    dx

    cmp $$_ovl_loaded_count,0   ; see if any overlays loaded
    je  ready_must_load     ; no, first overlay must always be loaded

; look for loaded overlay entry
ready_look:
    mov bx,jump_segment		; get identification number to match against in bx
    cmp last_access_slot,0
    je  ready_1
    mov es,last_access_slot	; es -> last access overlay slot
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

    mov ax,dt_size_para
    add ax,dat_size_para
    mov es:[2],ax           ; save size of data segments

    cmp is_calling_ovl,0	; see if calling overlay or returning
    jne ready_bump          ; calling overlay

; load Clarion data code from swap file
    call    ovl_swapfile_read
    dec swapfile_count      ; drop count of overlaid data in swapfile

ready_bump:
    inc $$_ovl_loaded_count ; increment count of loaded overlays
    mov ax,$$_ovl_load_seg
    jmp SHORT ready_loaded	; bypass already loaded code

ready_stackerr:
    mov ax,1                ; get error value in ax
    jmp NEAR PTR $$_ovl_op_error    ; stack overflow

; entry matches this overlay
ready_match:
;***    cmp $$_ovl_reload_flag,0	; see if reload overlays option is in effect
;***    je  match_2             ; no

    cmp cs:is_calling_ovl,0 ; see if calling overlay
    je  match_3             ; no, returning from overlay, bypass activity level update

; calling an already loaded overlay
; push a zero on the overlay stack for number of overlays swapped out

; set stack to calling routine seg/id/offset stack
    mov $$_ovl_mgr_stack_ptr,sp	; first save current stack pointer
    mov dx,$$_ovl_stack_ptr
    cmp dx,2                ; must be room for zero
    jb  ready_stackerr      ; not enough room

match_1:
    mov ax,$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,dx               ; set stack
    sti                     ; turn interrupts back on
    xor ax,ax               ; put zero on stack
    push    ax
    mov $$_ovl_stack_ptr,sp	; save new stack pointer value

; reset stack to overlay manager internal stack
    mov dx,$$_ovl_mgr_stack_ptr
    mov ax,cs
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
    mov jump_segment,ax		; update jump_segment

; save last accessed overlay
    mov last_access_slot,es	; save last access slot

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
    mov ax,$$_ovl_load_start
    mov $$_ovl_load_seg,ax  ; use start of load area for load segment
    mov dx,ax               ; dx holds start paragraph
    mov bx,ovl_size_para
    add bx,dx               ; bx holds end paragraph
    jmp NEAR PTR slot_ret   ; bypass search for area to load

$$_ovl_find_slot    PROC    NEAR
    push    si
    push    di
    mov si,$$_ovl_array_start   ; si holds array element for new overlay
    xor ax,ax
    mov $$_ovl_stackcount,ax	; init count of overlays on stack
    cmp $$_ovl_loaded_count,ax  ; see if any overlays loaded
    je  no_ovl_loaded       ; no

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
;***    cmp WORD PTR ds:[4],0   ; see if an active overlay
;***    je  store_inactive      ; no
    or  ah,40h              ; flag active overlay
;***    jmp SHORT store_value

;***; store slot pointer, not overlay id
;***store_inactive:
;***    mov ax,ds
;***    sub ax,si               ; subtract start of array
;***    inc ax                  ; make relative 1

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
    mov WORD PTR $$_ovl_hi_best_lru,ax  ; init so any comparison will have lower LRU
    mov WORD PTR $$_ovl_hi_best_lru+2,ax
;***    mov ovl_best_size,ax    ; init best load size so fails on any comparison
    mov es,check_array
    xor si,si               ; es:si -> check array

    mov dx,ovl_size_para	; get overlay file size in paragraphs
    add dx,1fh              ; round up to next 512 byte boundary (in paras)
    mov cl,5
    shr dx,cl               ; convert to 512 byte chunks

main_check_loop:
    xor ax,ax
    mov $$_ovl_swap_count,ax    ; init count of overlays swapped by proposed load position
    mov WORD PTR $$_ovl_highest_lru,ax  ; init so any comparison will force a higher LRU
    mov WORD PTR $$_ovl_highest_lru+2,ax
;***    mov ovl_act_size,ax     ; init size of active swapouts
    mov bx,si               ; offset into check_array

block_check_loop:
    mov ax,es:[bx]          ; get overlay id/slot #
    test    ah,80h          ; see if last used overlay
    jne to_bump_proposed    ; yes, don't use this position
    or  ax,ax               ; see if used
    je  check_next_block    ; no, try next block in overlay, if any

    inc $$_ovl_swap_count   ; bump count of overlays swapped out
;***    test    ah,40h
;***    je  check_inactive      ; inactive overlay swapped out

;***; active overlay swapout at this position
;***    inc ovl_act_size        ; bump load size by fake amount to compensate for each discrete active overlay

	push	ax				; save critical registers
	push	dx
	push	es
	and	ah,3fh				; mask off flag bits
	mov	es,$$_ovl_array_start

find_ovlid:
    cmp	ax,es:[0]			; check overlay identifier of array element
	je	lru_check			; found it
    mov dx,es
    inc dx
    mov es,dx               ; bump to next array element
    jmp SHORT find_ovlid	; keep looking for overlay id

lru_check:
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
	pop	es					; restore critical registers
	pop	dx
	pop	ax

check_act_loop:
;***    inc ovl_act_size        ; bump load size in 512 byte blocks
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
;***    jmp SHORT check_next_block  ; more positions to check for proposed overlay

COMMENT !
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
END COMMENT !

check_next_block:
    inc bx
    inc bx                  ; bump word offset in check array
    dec dx                  ; drop count of chunks to check
    jne block_check_loop    ; more chunks to check

check_next_pos:
    cmp $$_ovl_swap_count,0 ; if zero swapped overlays, use this position
    je  perfect_swap

;***; current position has overlays being swapped out
;***    mov ax,ovl_best_size    ; get best load size
;***    cmp ax,ovl_act_size
;***    jb  bump_proposed       ; best has less overlay loads than highest
;***    ja  best_update         ; current has least overlay movement, update best position

;***; best position and current position have same active load size,
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
;***    mov ax,ovl_act_size
;***    mov ovl_best_size,ax    ; update best load size
    mov $$_ovl_best_load,si ; save check array pointer new best load position

bump_proposed:
    inc si
    inc si                  ; bump base check address
    mov ax,si
    mov dx,ovl_size_para	; get overlay file size in paragraphs
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
    add ax,$$_ovl_load_start	; get real address
    mov $$_ovl_load_seg,ax  ; save load position
    mov dx,ax               ; dx holds start segment of overlay to load
    mov bx,ovl_size_para	; get overlay file size in paragraphs
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
    add ax,$$_ovl_load_start	; get real address
    mov $$_ovl_load_seg,ax  ; use best proposed load area for load segment
    mov dx,ax               ; dx holds start segment of overlay to load
    mov bx,ovl_size_para	; get overlay file size in paragraphs
    add bx,dx               ; bx holds end segment of overlay to load
    mov es,$$_ovl_array_start   ; es -> overlay array
    mov cx,$$_ovl_best_count    ; get count of overlays to swap out

	cmp	cx,-1				; see if best never init'ed (only last overlay exists)
	jne	zero_loop			; no
	mov	cx,1				; change to one overlay to swapout

zero_loop:
    mov ax,es:[0]           ; get overlay identifier number
    or  ax,ax               ; see if nonzero
    jne zero_2              ; nonzero, valid overlay
    mov si,es               ; si -> empty slot

    or  cx,cx               ; see if more overlays to swap out
    jne to_next_element     ; yes
    jmp NEAR PTR slot_ret   ; any and all necessary overlays swapped out

to_next_element:
    jmp NEAR PTR next_element

zero_2:
    jcxz    to_next_element	; no overlays to swap, just looking for empty slot
    cmp dx,es:[8]           ; check proposed start against overlay end
    jae to_next_element		; above end, overlay not swapped
    cmp bx,es:[6]           ; check proposed end against overlay start
    jbe to_next_element		; below start, overlay not swapped

;***    cmp $$_ovl_reload_flag,0	; see if reload overlays option in effect
;***    je  zero_3              ; no

    mov ax,es:[4]
    or  ax,ax               ; see if active overlay
    je  zero_4              ; no

; set stack to calling routine seg/id/activity stack
    mov $$_ovl_mgr_stack_ptr,sp	; first save current stack pointer
    mov di,$$_ovl_stack_ptr
    mov ax,$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,di               ; set internal stack
    sti                     ; turn interrupts back on
    push    es:[6]          ; save load segment to stack
    push    es:[0]          ; save overlay identifier to stack
    push    es:[10]         ; save LRU low word
    push    es:[12]         ; save LRU high word
    push    es:[4]          ; save activity level

; write Clarion active data overlay to swap file
    cmp swapfile_active,0   ; see if swapfile is now active
    jne save_data           ; yes
    call    ovl_swapfile_init   ; init the swapfile

save_data:
    call    ovl_swapfile_write  ; write overlay to swap file
    inc swapfile_count      ; bump count of active overlaid data in swapfile

    inc $$_ovl_stackcount	; bump count of overlays saved to stack
    mov $$_ovl_stack_ptr,sp	; save new stack pointer value

; set stack to overlay manager internal stack
    mov di,$$_ovl_mgr_stack_ptr
    mov ax,cs
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,di               ; set internal stack
    sti                     ; turn interrupts back on

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
;***    cmp $$_ovl_reload_flag,0	; see if reload overlays option in effect
;***    je  slot_ret3           ; no

; set stack to calling routine seg/id/offset stack
    mov $$_ovl_mgr_stack_ptr,sp	; first save current stack pointer
    mov di,$$_ovl_stack_ptr
    mov ax,$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,di               ; set internal stack
    sti                     ; turn interrupts back on

    push    $$_ovl_stackcount	; put count of active overlays swapped out on stack
    mov $$_ovl_stack_ptr,sp	; save new stack pointer value

; reset stack to overlay manager internal stack
    mov di,$$_ovl_mgr_stack_ptr
    mov ax,cs
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,di               ; set internal stack
    sti                     ; turn interrupts back on

slot_ret3:
    mov ax,jump_segment		; get overlay identifier
    mov es:[0],ax
    mov es:[6],dx           ; save start paragraph
    mov es:[8],bx           ; save end paragraph

    mov WORD PTR es:[4],1   ; set activity level to 1 (first time active)

slot_ret4:
    pop di					; restore critical registers
    pop si
    ret
$$_ovl_find_slot    ENDP

;*****************************
;* $$_OVL_GET_INFO           *
;*****************************

; get overlay file system information
; jump_segment variable holds overlay identifier
; destroys ax,bx,cx,dx

$$_ovl_get_info PROC    NEAR
	mov	info_from_emsxms_flag,0	; init overlay stashed in EMS/XMS flag

; _DT information and file handle (same for all three segments)
    mov bx,jump_segment		; get overlay identifier
	sub	bx,3				; back up 2 to _DT overlay, make relative zero
    shl bx,1                ; word offset into file handle table
	push	ds				; save -> overlay manager data
	mov ds,cs:ovl_handle_tbl_seg   ; ds -> overlay file position table
	mov	ax,ds:[bx]			; get file handle
    shl bx,1				; make doubleword offset into file position table
	mov ds,cs:ovl_filepos_tbl_seg   ; ds -> overlay file position table
    mov dx,ds:[bx]			; get _DT file position
    mov cx,ds:[bx+2]
	pop	ds					;  restore ds -> overlay manager data
	mov	ovl_file_handle,ax	; save file handle
    mov WORD PTR dt_file_offset,dx	; save _DT position low word
    mov WORD PTR dt_file_offset+2,cx	; save _DT position high word

; seek to _DT overlay file in OVL file
; dx:cx set to proper values
    mov bx,ovl_file_handle
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h

; read _DT binary file header
	push	ds				; save -> overlay manager data
	mov ds,cs:binheader_seg
	xor dx,dx
	mov cx,BIN_HEADER_SIZE
    mov ah,3fh              ; read file
    int 21h

; get overlaid DT segment true size in paras
	mov ax,ds:[bhs_truelen]
	add ax,15               ; round up
	mov cl,4
	shr ax,cl               ; convert to paras
	pop	ds
	mov	dt_size_para,ax		; save Clarion _DT size

; _DAT information
    mov bx,jump_segment		; get overlay identifier
	sub	bx,2				; back up 1 to _DAT overlay, make relative zero
    shl bx,1                ; word offset into file handle table
    shl bx,1				; make doubleword offset into file position table
	push	ds				; save -> overlay manager data
	mov ds,cs:ovl_filepos_tbl_seg   ; ds -> overlay file position table
    mov dx,ds:[bx]			; get _DAT file position
    mov cx,ds:[bx+2]
	pop	ds					;  restore ds -> overlay manager data
    mov WORD PTR dat_file_offset,dx	; save _DAT position low word
    mov WORD PTR dat_file_offset+2,cx	; save _DAT position high word

; seek to _DAT overlay file in OVL file
; dx:cx set to proper values
    mov bx,ovl_file_handle
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h

; read _DAT binary file header
	push	ds				; save -> overlay manager data
	mov ds,cs:binheader_seg
	xor dx,dx
	mov cx,BIN_HEADER_SIZE
    mov ah,3fh              ; read file
    int 21h

; get overlaid DAT segment true size in paras
	mov ax,ds:[bhs_truelen]
	add ax,15               ; round up
	mov cl,4
	shr ax,cl               ; convert to paras
	pop	ds
	mov	dat_size_para,ax	; save Clarion _DAT size

; CODE information
    mov bx,jump_segment		; get overlay identifier
    dec bx                  ; make relative zero
    shl bx,1                ; word offset into file handle table
    shl bx,1				; make doubleword offset into file position table
	push	ds				; save -> overlay manager data
	mov ds,cs:ovl_filepos_tbl_seg   ; ds -> overlay file position table
    mov dx,ds:[bx]			; get file position
    mov cx,ds:[bx+2]
	pop	ds					;  restore ds -> overlay manager data

    mov WORD PTR $$_ovl_file_offset,dx  ; save position low word
    mov WORD PTR $$_ovl_file_offset+2,cx    ; save  position high word

;***; check if can load from overlay file stashed in EMS or XMS
;***	test	ch,0c0h			; see if two high bits set (file stash to EMS/XMS)
;***	je	ogi_seek			; no
;***	call	ovl_emsxms_get_info	; get info from EMS or XMS
;***	ret

; seek to overlay file in OVL file
; dx:cx already set to proper values
ogi_seek:
    mov bx,ovl_file_handle
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h
    jnc read_sys

ogi_doserr:
    jmp	NEAR PTR $$_ovl_dos_error

; read binary file header
read_sys:
	push	ds				; save -> overlay manager data
	mov ds,cs:binheader_seg
	xor dx,dx
	mov cx,BIN_HEADER_SIZE
    mov ah,3fh              ; read file
    int 21h
	jc	ogi_doserr

; get overlaid segment true size in paras
	mov ax,ds:[bhs_truelen]
	add ax,15               ; round up
	mov cl,4
	shr ax,cl               ; convert to paras
	pop	ds

	mov	code_size_para,ax	; save Clarion code size
	add	ax,dt_size_para		; add in Clarion data segment sizes
	add	ax,dat_size_para
	mov ovl_size_para,ax	; keep total overlay size

    ret
$$_ovl_get_info ENDP

;*****************************
;* $$_OVL_LOAD_OVERLAY       *
;*****************************

; load overlay from .OVL file, performing segment fixups as necessary
; destroys ax,bx,cx,dx

$$_ovl_load_overlay PROC    NEAR
	mov emsxms_stash_flag,0	; init stash flag to failure status
    push    es              ; save critical register
    push    di

; check if can load from overlay file stashed in EMS or XMS
;***	cmp	info_from_emsxms_flag,0
;***	je	load_seg			; overlay not stashed
;***	call	ovl_emsxms_load_ovl	; load overlay from EMS/XMS
;***	jmp	SHORT load_fixup

; seek to and load _DT segment image at cs:$$_ovl_load_seg+code_size_para
    mov dx,WORD PTR dt_file_offset	; get _DT position low word
    mov cx,WORD PTR dt_file_offset+2	; get _DT position high word
	add dx,BIN_HEADER_SIZE
	adc	cx,0				; adjust for binary header
    mov bx,ovl_file_handle
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h

	mov	ax,$$_ovl_load_seg
	add	ax,code_size_para
	mov	dt_load_seg,ax
	mov bx,ovl_file_handle
	mov	cx,dt_size_para		; get size of segment to load in paras
	shl	cx,1
	shl	cx,1
	shl	cx,1
	shl	cx,1				; convert to bytes (may be more than real segment length)
	mov ds,ax
	xor dx,dx
	call	read_to_ems

; seek to and load _DAT segment image at cs:$$_ovl_load_seg+code_size_para+dt_size_para
; NOTE ds != cs
    mov dx,WORD PTR cs:dat_file_offset	; get _DAT position low word
    mov cx,WORD PTR cs:dat_file_offset+2	; get _DAT position high word
	add dx,BIN_HEADER_SIZE
	adc	cx,0				; adjust for binary header
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h

	mov	ax,cs:$$_ovl_load_seg
	add	ax,cs:code_size_para
	add	ax,cs:dt_size_para
	mov	cs:dat_load_seg,ax
	mov	cx,cs:dat_size_para	; get size of segment to load in paras
	shl	cx,1
	shl	cx,1
	shl	cx,1
	shl	cx,1				; convert to bytes (may be more than real segment length)
	mov ds,ax
	xor dx,dx
	call	read_to_ems

; seek to CODE segment image
    mov dx,WORD PTR cs:$$_ovl_file_offset	; get CODE position low word
    mov cx,WORD PTR cs:$$_ovl_file_offset+2	; get CODE position high word
	add dx,BIN_HEADER_SIZE
	adc	cx,0				; adjust for binary header
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h

; load segment image at cs:$$_ovl_load_seg
load_seg:
	mov ds,cs:binheader_seg
	mov cx,ds:[bhs_length]
	mov ds,cs:$$_ovl_load_seg
	xor dx,dx

;***	mov ah,3fh              ; read from file
;***	int 21h
	call	read_to_ems

	jnc olo_2

olo_doserr2:
	jmp NEAR PTR $$_ovl_dos_error

olo_2:
	push    cs
	pop ds
;***	call	ovl_emsxms_stash_ovl

	mov es,cs:binheader_seg
	mov ax,WORD PTR es:[bhs_fixptr]
	or  ax,WORD PTR es:[bhs_fixptr+2]
	jne olo_fixexist        ; fixups exist

load_ret:
	pop di                  ; restore critical register
	pop es
	ret

; read fixup header
olo_fixexist:
	mov dx,OFFSET fix_header    ; ds:dx -> fixup header load area
	mov cx,FIX_HEADER_SIZE
	mov ah,3fh
	int 21h
	jc  olo_doserr2

; save position of start of fixup info after header
	xor cx,cx
	mov dx,cx
	mov ax,4201h            ; move file pointer, offset from current position
	int 21h
	mov WORD PTR ovl_fixup_fpos,ax	; save fixup block position
	mov WORD PTR ovl_fixup_fpos+2,dx

; initialize offsets and counters
	xor ax,ax
	mov ds:fix_info_off,ax
	mov ds:fix_loc_off,ax
	mov ds:fix_done,ax
	mov ds:typeflag,al

	mov ax,ds:fix_header.fhs_lowcount
	add ax,ds:fix_header.fhs_nearcount
	mov ds:fix_lownear_count,ax ; save combine low byte and near fixup count

; ignore high words, assume <64K sized fixup
	add ax,fix_header.fhs_farcount	; ax holds total count of fixups
	add ax,ax               ; x2
	mov fix_loc_end,ax		; save end of fixup location block, relative to start
	mov cx,ax               ; keep fixup count*2
	add ax,ax               ; x4
	mov fix_info_end,ax		; save end of fixup info block ,relative to start
	add ax,cx               ; x6, ax holds fixup block size
	mov fix_size,ax			; save fixup block size

	mov es,ds:ovl_stash_tbl_seg
	mov di,jump_segment		; get file overlay identifier
	dec di                  ; make relative 0
	add di,di               ; make word offset
	mov es,es:[di]          ; es -> overlay stash entry

; seek to fixup info
olo_loop:
	mov dx,WORD PTR ovl_fixup_fpos
	mov cx,WORD PTR ovl_fixup_fpos+2
	add dx,fix_info_off		; adjust past bytes already read
	adc cx,0
	mov ax,4200h            ; move file pointer, relative beginning of file
	int 21h

	mov dx,OFFSET ovl_fix_info_tbl  ; ds:dx -> fixup info load area

; read info bytes
	cmp fix_size,1020		; see if more than 1 pass needed to read fixups
	jbe olo_3               ; no
	mov cx,680
	jmp SHORT olo_info_read

olo_3:
	mov cx,fix_info_end
	sub cx,fix_info_off		; compute fixup info bytes to read

olo_info_read:
	mov ah,3fh              ; read from file
	int 21h
	jnc olo_loc

olo_doserr:
	jmp NEAR PTR $$_ovl_dos_error

; seek to fixup location
olo_loc:
	xor ax,ax
	mov dx,WORD PTR ovl_fixup_fpos
	mov cx,WORD PTR ovl_fixup_fpos+2
	add dx,fix_info_end		; adjust past info bytes
	adc cx,ax
	add dx,fix_loc_off		; adjust past bytes already read
	adc cx,ax
	mov ah,42h              ; move file pointer, relative beginning of file (al==0)
	int 21h

	mov dx,OFFSET ovl_fix_loc_tbl   ; ds:dx -> fixup location load area

; read location bytes
	cmp fix_size,1020		; see if more than 1 pass needed to read fixups
	jbe olo_4               ; no
	mov cx,340
	jmp SHORT olo_loc_read

olo_4:
	mov cx,fix_loc_end
	sub cx,fix_loc_off		; compute fixup loc bytes to read

olo_loc_read:
	mov di,cx
	shr di,1                ; di contains number of fixups to process this pass
	mov ah,3fh              ; read from file
	int 21h
	jc  olo_doserr

	call    do_ovl_fixups   ; process fixups

	add fix_info_off,680
	add fix_loc_off,340
	sub fix_size,1020		; subtract off bytes read/processed
	jbe to_load_ret         ; no
	jmp NEAR PTR olo_loop   ; more fixups left, loop and process

to_load_ret:
	mov ds,cs:$$_ovl_load_seg
    mov ax,cs:dt_load_seg	; get load segment of _DT
    mov ds:[5],ax           ; fixup Clarion code segment
    mov ax,cs:dat_load_seg	; get load segment of _DAT
    mov ds:[9],ax           ; fixup Clarion code segment
    xor ax,ax
    mov ds:[3],ax           ; zero code offsets
    mov ds:[7],ax
	push    cs
	pop ds					; ds -> code segment data

	jmp NEAR PTR load_ret
$$_ovl_load_overlay ENDP

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
	mov	cx,BIN_HEADER_SIZE	; count of chars to transfer
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

ovl_emsxms_write	ENDP

;*****************************
;* OVL_EMSXMS_LOAD_OVL       *
;*****************************

; load from overlay file stashed in EMS or XMS
; destroys ax,bx,cx,dx

ovl_emsxms_load_ovl	PROC	NEAR
	push	si				; save critical register
	push	di

	xor	di,di				; zero offset within load area
    mov cx,ovl_size_para	; get size of overlay in paragraphs
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
;* DO_OVL_FIXUPS             *
;*****************************

; process the fixups for the overlay loaded
; upon entry di == count of fixups to process,
; es -> overlay stash entry
; destroys ax,cx,dx,di

do_ovl_fixups   PROC    NEAR
	push    bx
	push    si
	push    es
	push	ds

	mov cx,di               ; get count of fixups in cx
	mov si,OFFSET ovl_fix_info_tbl  ; si will offset into fixup info
	mov bx,OFFSET ovl_fix_loc_tbl   ; bx will offset into fixup location

dof_loop:
	push    cs
	pop ds

; ds == cs
	mov ax,ds:fix_done      ; get count of fixups completed
	cmp ax,ds:fix_header.fhs_lowcount   ; see if fixup type transition from low to near
	jne dof_2               ; no
	inc ds:typeflag

dof_2:
	cmp ax,ds:fix_lownear_count ; see if fixup type transition from near to far
	jne dof_3               ; no
	inc ds:typeflag

dof_3:
	lodsb                   ; get modified fixdat
	mov ds:fixdat,al
	lodsw                   ; get frame index

	mov dl,ah               ; save merged byte
	and ah,0fh              ; make off target index bits
	mov ds:frame_index,ax

	mov ah,dl
	shr ah,1                ; convert 4 MSB to proper value
	shr ah,1
	shr ah,1
	shr ah,1
	lodsb                   ; get target index low byte
	mov ds:target_index,ax

; ds == cs upon entry, destroyed
	call    ovl_comp_fix    ; compute all necessary fixup values, return add value in ax

	mov dx,ax               ; save add value
	push    cs
	pop ds

; ds == cs
	mov di,ds:[bx]          ; get location to fix up in di

; ds != cs
	mov ds,cs:$$_ovl_load_seg   ; ds:di -> segment image location to fix up

	mov al,cs:typeflag
	or  al,al               ; check if low_order byte
	jne dof_notlow           ; no

; low order byte
	add ds:[di],dl          ; save new offset value back to data record
	jmp SHORT dof_nextfix

dof_notlow:
	mov ax,dx               ; restore fixup value to ax
	cmp cs:typeflag,1       ; check if offset
	je  dof_addin           ; yes

; base or far fixup
	mov al,cs:is_absseg     ; check if absolute or relocatable
	or  al,cs:is_abspub
	mov ax,cs:target_segment    ; KEEP FLAGS from 'or'
	jne dof_addin           ; absolute, don't adjust for program load address

	cmp cs:is_ovlrel,0      ; see if overlay relative fixup
	je  dof_progrel
	add ax,cs:$$_ovl_load_seg   ; add in overlay load segment
	jmp SHORT dof_addin

; program load address relative fixup
dof_progrel:
	add ax,cs:prog_seg      ; relocatable, add in program load address

; offset or near fixup
dof_addin:
	add ds:[di],ax          ; save new offset

dof_nextfix:
	inc cs:fix_done         ; bump count of fixups completed
	add bx,2                ; move to next fixup location
	dec cx
	je dof_ret              ; no more fixups
	jmp NEAR PTR dof_loop   ; loop until all fixups for this pass processed

dof_ret:
	pop	ds					; restore critical registers
	pop es
	pop si
	pop bx
	ret
do_ovl_fixups   ENDP

;*****************************
;* OVL_COMP_FIX              *
;*****************************

; compute all necessary fixup values
; upon entry es -> overlay stash entry, ds == cs
; return fixup offset value in ax, cs:target_segment has fixup segment
; destroys ax,dx,di,ds

ovl_comp_fix    PROC    NEAR
	push    si
	xor al,al
	mov ds:is_absseg,al     ; init absolute flags
	mov ds:is_abspub,al
	mov ds:is_ovlrel,al     ; init overlay relative flag

	mov al,ds:fixdat
	mov ah,al
	and al,70h
	mov ds:frame_method,al  ; frame method is the frame field

	and ah,3                ; break targt field out of fixdat
	mov al,ah
	mov ds:target_method,al ; target method is the targt field
	inc al                  ; make al known nonzero value
	mov ds:is_resolved,al   ; init is_resolved flag to assume fixup okay

	mov si,ds:target_index
	dec si
	add si,si               ; convert index to relative 0 word offset

	mov al,ds:target_method
	or  al,al               ; check if segment index
	je  gta_seg
	cmp al,1                ; check if group index
	jne to_gta_ext
	jmp NEAR PTR gta_grp

to_gta_ext:
	jmp NEAR PTR gta_ext    ; external index

; overlaid segment, direct to overlay call table
gta_ovl_seg:
	and ax,0fffh            ; get overlay id #
	cmp ax,cs:jump_segment  ; see if matches current id
	je  gta_ovl_sameseg     ; yes
	jmp NEAR PTR gta_ovlshared

; fixing up overlaid segment reference within the same overlay
gta_ovl_sameseg:
	xor ax,ax               ; use zero offset
	mov WORD PTR cs:target_prog_off,ax  ; save target program offset low word

gta_ovl_sameshared:
	mov WORD PTR cs:lseg_canon,ax   ; save low word of offset for canonical computation
	mov WORD PTR cs:lseg_canon+2,ax ; save high word of offset for canonical computation
	mov cs:is_ovlrel,1		; flag overlay relative fixup
	jmp NEAR PTR gta_done

; segment index, method 0 or 4
gta_seg:
	add si,es:[0]           ; add in offset to segdef entries
	mov ax,es:[si]          ; ax == stashed segdef value
	test    ah,80h          ; see if overlaid segment
	jne gta_ovl_seg         ; yes

; nonoverlaid segment
	mov si,ax
	dec si
	add si,si
	add si,si               ; si == segdef table dword offset relative 0
	mov ds,cs:ovl_usedseg_tbl_seg
	mov ax,ds:[si+2]
	cmp ax,0ffffh           ; see if absolute segment
	je  gta_is_abs          ; yes

; not an absolute segment
	mov dx,ax               ; save high word of segment offset/canonical entry
	mov ax,ds:[si]          ; get low word of segment offset
	mov WORD PTR cs:target_prog_off,ax  ; update target program offset low word

gta_segcanon:
	shr dx,1
	shr dx,1
	shr dx,1
	shr dx,1
	je  gta_notslave        ; no canonical entry, this segment is not a slave

; dx holds offset into canonical segment table
	mov si,dx
	dec si
	add si,si
	add si,si               ; si holds dword offset relative 0 into canonical table
	mov ds,cs:ovl_usedcan_tbl_seg

gta_notslave:
	mov ax,ds:[si]          ; get low word of segment offset
	jmp SHORT gta_canshared ; jump to canonical code shared with groups

; absolute segment
gta_is_abs:
	mov cs:is_absseg,1      ; set absolute segment flag
	mov ax,ds:[si]          ; get frame number, convert to bytes
	mov cs:target_segment,ax    ; frame number in target_segment in case of LOC frame fixup
	shl ax,1
	shl ax,1
	shl ax,1
	shl ax,1
	mov WORD PTR cs:target_prog_off,ax  ; update target program offset
	jmp NEAR PTR gta_done

; group index, method 1 or 5
gta_grp:
	add si,es:[2]           ; add in offset to grpdef entries
	mov si,es:[si]          ; si == grpdef entry value in stash table
	dec si
	add si,si
	add si,si               ; si == grpdef table dword offset relative 0
	mov ds,cs:ovl_usedgrp_tbl_seg
	mov ax,ds:[si]          ; get low word of group offset
	mov WORD PTR cs:target_prog_off,ax  ; save target program offset low word

gta_canshared:
	mov WORD PTR cs:lseg_canon,ax   ; save low word of offset for canonical computation
	mov ax,ds:[si+2]
	mov WORD PTR cs:lseg_canon+2,ax ; save high word of offset for canonical computation
	jmp NEAR PTR gta_done

; external index, method 2 or 6
gta_ext:
	add si,si               ; convert word offset to dword offset
	add si,4                ; add in offset to extdef entries
	mov ax,es:[si]          ; ax == stashed extdef value
	test    ah,80h          ; see if overlaid extdef
	jne gta_ovlext          ; yes

; nonoverlaid extdef
	mov dx,ax               ; save entry value
	and ah,0fh              ; mask off overlaid, absolute flag, group bits
	mov di,ax
	dec di
	add di,di               ; make relative 0 word offset
	mov ds,cs:ovl_usedpub_tbl_seg
	mov ax,ds:[di]          ; get public offset
	mov WORD PTR cs:target_prog_off,ax  ; update target program offset low word

	mov ax,dx

	and ah,40h              ; see if absolute extdef
	jne gta_abs             ; yes

; not absolute extdef
	mov ax,es:[si+2]        ; get segdef portion of pubdef entry
	and ah,0fh              ; mask off group flag, group bits
	mov si,ax
	dec si
	add si,si
	add si,si               ; make relative 0 dword offset
	mov ds,cs:ovl_usedseg_tbl_seg
	mov ax,ds:[si]          ; get low word of segment offset
	add WORD PTR cs:target_prog_off,ax  ; update target program offset low word
	mov dx,ds:[si+2]        ; get high word of segment offset/canonical entry in dx
	jmp NEAR PTR gta_segcanon  ; compute canonical segment

; overlaid public
gta_ovlext:
	and ah,0fh              ; get overlay id
	mov dx,es:[si+2]        ; get overlaid segdef id
	cmp dx,cs:jump_segment  ; see if matches current overlay
	jne gta_ovlshared       ; no

; overlaid public within current overlay
	cmp ax,cs:ovl_seg_count ; see if nonzero offset
	jbe gta_pubzero         ; no

; nonzero offset overlaid public within overlaid segment, get offset
	sub ax,cs:ovl_seg_count ; adjust for overlaid segments/zero offset publics
	mov si,ax
	dec si
	add si,si               ; make relative 0 word offset
	mov ds,cs:ovlpub_tbl_seg
	mov ax,ds:[si]          ; get public offset

	mov WORD PTR cs:target_prog_off,ax  ; save target program offset low word
	jmp NEAR PTR gta_ovl_sameshared ; go to code shared with same segment overlay (no offset change)

; zero offset overlaid public
gta_pubzero:
	jmp NEAR PTR gta_ovl_sameseg    ; go to code shared with same segment overlay

gta_ovlshared:
	dec ax                  ; make relative zero
	mov dx,ax
	add ax,ax
	add ax,dx
	add ax,5                ; 3*id+5 is offset
	xor dx,dx
	add ax,WORD PTR cs:call_tbl_offset  ; add in offset of call table
	adc dx,0                ; carry to dx
	mov WORD PTR cs:target_prog_off,ax  ; save target program offset low word
	mov WORD PTR cs:lseg_canon,ax   ; save low word of offset for canonical computation
	mov ax,WORD PTR cs:call_tbl_offset+2
	add ax,dx               ; add in carry from low word offset
	mov WORD PTR cs:lseg_canon+2,ax ; save high word of offset for canonical computation
	jmp SHORT gta_done

; absolute, no segment partition or segdef entry offsets
gta_abs:
	mov cs:is_abspub,1     ; flag absolute public declaration
	mov ax,es:[si+2]       ; get frame number in target_segment in case of LOC frame fixup
	mov cs:target_segment,ax

; see if absolute segment or absolute public declaration, no frame computation
gta_done:
	mov al,cs:is_absseg
	or  al,cs:is_abspub
	je  gta_notabs
	jmp NEAR PTR gfa_done   ; either absolute segment or absolute public declaration

gta_notabs:
	mov al,cs:frame_method
	cmp al,40h              ; check if frame determined by location segment
	jne pef_11              ; no

	mov ax,WORD PTR cs:lseg_canon   ; get canonical segment low word offset containing lseg
	mov dx,WORD PTR cs:lseg_canon+2 ; get high word
	shr dx,1
	rcr ax,1                ; /2
	shr dx,1
	rcr ax,1                ; /4
	shr dx,1
	rcr ax,1                ; /8
	shr dx,1
	rcr ax,1                ; /16, paragraph value (largest possible frame number)
	mov cs:target_segment,ax
	jmp NEAR PTR gfa_done   ; bypass other target segment computation code

pef_11:
	cmp al,50h              ; check if frame determined by target's index
	jne pef_notarg          ; no
	mov si,cs:target_index  ; pass target info to get_frame_addr
	mov dl,cs:target_method
	jmp SHORT pef_getframe

; frame determined by segment, group, or external index
pef_notarg:
	mov si,cs:frame_index   ; pass frame info to get_frame_addr
	mov dl,cs:frame_method

pef_getframe:
	dec si
	add si,si               ; convert index to relative 0 word offset
	or  dl,dl               ; check if segment index
	je  gfa_segind          ; yes
	jmp NEAR PTR gfa_chkgrp ; no

; segment index
gfa_segind:
	add si,es:[0]           ; add in offset to segdef entries
	mov ax,es:[si]          ; ax == stashed segdef value
	test    ah,80h          ; see if overlaid segment
	jne gfa_ovl_seg         ; yes

; nonoverlaid segment
gfa_nonovlseg:
	mov si,ax
	dec si
	add si,si
	add si,si               ; si == segdef table dword offset relative 0
	mov ds,cs:ovl_usedseg_tbl_seg
	mov ax,ds:[si+2]
	cmp ax,0ffffh           ; see if absolute segment
	je  to_gfa_absseg       ; yes
	mov dx,ax               ; save high word of segment offset/canonical entry
	mov ax,ds:[si]          ; get low word of segment offset
	mov WORD PTR cs:frame_offset,ax ; save absolute frame offset for self-relative fixups

	shr dx,1
	shr dx,1
	shr dx,1
	shr dx,1
	je  gfa_notslave        ; no canonical entry, this segment is not a slave

; dx holds offset into canonical segment table
	mov si,dx
	dec si
	add si,si
	add si,si               ; si holds dword offset relative 0 into canonical table
	mov ds,cs:ovl_usedcan_tbl_seg

gfa_notslave:
	mov ax,ds:[si]          ; get master segment offset
	mov di,ds:[si+2]

gfa_shared:
	shr di,1
	rcr ax,1                ; /2
	shr di,1
	rcr ax,1                ; /4
	shr di,1
	rcr ax,1                ; /8
	shr di,1
	rcr ax,1                ; /16, have paragraph value of offset (segment value)
	mov cs:target_segment,ax    ; save to memory variable
	jmp NEAR PTR gfa_done

to_gfa_absseg:
	jmp NEAR PTR gfa_absseg

; overlaid extdef
gfa_ovlext:
	mov ax,es:[si+2]        ; get overlaid segdef id

; overlaid segment
gfa_ovl_seg:
	and ax,0fffh            ; get overlay id #
	cmp ax,cs:jump_segment  ; see if matches current id
	je  gfa_ovl_sameseg     ; yes
	mov ax,WORD PTR cs:call_tbl_offset
	mov di,WORD PTR cs:call_tbl_offset+2
	jmp SHORT gfa_shared

; fixing up overlaid segment reference within the same overlay
gfa_ovl_sameseg:
	xor ax,ax               ; use zero offset
	xor di,di
	mov WORD PTR cs:frame_offset,ax ; zero absolute frame offset for self-relative fixups
	mov cs:is_ovlrel,1		; flag overlay relative fixup
	jmp SHORT gfa_shared

gfa_chkgrp:
	cmp dl,10h              ; check if group index
	je  gfa_grpind          ; yes
	cmp dl,1
	jne gfa_extind          ; no

; group index
gfa_grpind:
	add si,es:[2]           ; add in offset to grpdef entries
	mov si,es:[si]          ; si == grpdef entry value in stash table

gfa_group_entry:
	dec si
	add si,si
	add si,si               ; si == grpdef table dword offset relative 0
	mov ds,cs:ovl_usedgrp_tbl_seg
	mov ax,ds:[si]          ; get low word of group offset
	mov di,es:[si+2]        ; get high word of group offset
	mov WORD PTR cs:frame_offset,ax ; save absolute frame offset for self-relative fixups
	jmp SHORT gfa_shared    ; jump to code shared with segment index

; absolute segment for frame
gfa_absseg:
	mov cs:is_absseg,1      ; set absolute segment flag
	mov ax,ds:[si]          ; get frame number, convert to bytes
	mov cs:target_segment,ax    ; save to memory variable
	jmp NEAR PTR gfa_done

; external index
gfa_extind:
	add si,si               ; convert word offset to dword offset
	add si,4                ; add in offset to extdef entries
	mov ax,es:[si]          ; ax == stashed extdef value
	test    ah,80h          ; see if overlaid extdef
	jne gfa_ovlext          ; yes
	test    ah,40h          ; see if absolute extdef
	jne gfa_absseg          ; yes

; nonoverlaid, not absolute extdef
; check if pubdef has a group associated with it
	mov dx,ax
	mov ax,es:[si+2]
	test    ah,80h          ; high bit set if group associated with public
	je  gfa_nogroup         ; no group

	and ax,7000h            ; get lower 3 bits of group entry
	and dx,3000h            ; get middle 2 bits of group entry
	xchg    al,ah
	shr al,1
	shr al,1
	shr al,1
	shr al,1                ; al holds lower 3 bits of group entry
	shr dh,1                ; dh holds middle 2 bits of group entry
	or  al,dh
	mov si,ax               ; si == grpdef entry value in stash table
	jmp SHORT gfa_group_entry   ; perform code in common with group index

gfa_nogroup:
	and ah,0fh              ; mask off group flag, group bits
	jmp NEAR PTR gfa_nonovlseg  ; perform code in common with segment index

; absolute public declaration
gfa_abs:
	mov cs:is_abspub,1      ; flag absolute public declaration
	mov ax,es:[si+2]       ; get frame number in target_segment in case of LOC frame fixup
	mov cs:target_segment,ax    ; save to target segment

; see if absolute segment or absolute public declaration, no address adjustment
gfa_done:
	mov al,cs:is_absseg
	or  al,cs:is_abspub
	jne pef_seg_rel         ; either absolute segment or absolute public declaration

; check M bit for self-relative fixup
	mov al,cs:fixdat
	and al,80h              ; get M bit field value
	je  pef_self_rel        ; M bit reset, fixup self-relative

; fixup segment relative
pef_seg_rel:
	mov di,cs:target_segment
	shl di,1
	shl di,1
	shl di,1
	shl di,1

; get target offset in di:ax
	mov ax,WORD PTR cs:target_prog_off  ; get absolute offset low word
	sub ax,di               ; compute low word difference

pef_ret:
	pop si
	ret

; fixup is self-relative
pef_self_rel:
	mov ax,WORD PTR cs:target_prog_off  ; get absolute offset low word
	sub ax,WORD PTR cs:frame_offset ; subtract off frame that fixup is relative too
	sub ax,cs:[bx]          ; subtract off location to fix up
	cmp cs:typeflag,0       ; see if lowbyte fixup
	jne cf_selfnear         ; no
	dec ax
	pop si
	ret

cf_selfnear:
	sub ax,2                ; adjust two bytes for offset/near
	pop si
	ret
ovl_comp_fix    ENDP

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
    mov $$_ovl_mgr_stack_ptr,sp	; first save current stack pointer
    mov bx,$$_ovl_stack_ptr
    mov ax,$$_ovl_stack_start
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

    pop $$_ovl_activity     ; get activity level
    pop swapped_lru_high    ; get swapped out overlay LRU high word
    pop swapped_lru_low     ; get swapped out overlay LRU low word
    pop jump_segment		; get overlay identifier in jump_segment variable
    pop $$_ovl_load_seg     ; get overlay load segment
    mov $$_ovl_stack_ptr,sp	; save new stack pointer value

; set stack to overlay manager internal stack
    mov bx,$$_ovl_mgr_stack_ptr
    mov ax,cs
    cli                     ; shut off interrupts while stack is set
    mov ss,ax
    mov sp,bx               ; set internal stack
    sti                     ; turn interrupts back on

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
    add bx,ovl_size_para	; add in size of overlay in paragraphs to get end paragraph

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
    cmp ax,jump_segment		; see if swapped-in overlay matches previous overlay
    je SHORT do_swap        ; yes, zero out previous overlay entry and activate this one

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
    mov ax,jump_segment
    mov es:[0],ax           ; save overlay identifier

    mov ax,$$_ovl_activity
    mov es:[4],ax           ; save activity level

    mov ax,dt_size_para
    add ax,dat_size_para
    mov es:[2],ax           ; save size of data segments

; load Clarion data code from swap file
    call    ovl_swapfile_read
    dec swapfile_count      ; drop count of overlaid data in swapfile

; set LRU counter to what it was before swapout
    mov ax,swapped_lru_low
    mov es:[10],ax
    mov ax,swapped_lru_high
    mov es:[12],ax
    inc $$_ovl_loaded_count	; increment count of loaded overlays

swap_decstack:
    dec $$_ovl_stackcount	; drop count of loaded overlays on stack
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
    mov dx,cs
    mov ds,dx               ; ds -> error handler data segment

doserr2:
    mov dx,OFFSET dos_err_text  ; ds:dx -> message to write
    mov cx,24

$$_ovl_err_shared:
    mov si,ax               ; save error code
    mov ax,cs
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
    mov dx,cs
    mov ds,dx               ; ds -> error handler data segment

operr2:
    mov dx,OFFSET ovl_err_text
    mov cx,36
    jmp SHORT $$_ovl_err_shared ; jump to code shared with DOS error

$$_ovl_op_error     ENDP

;*****************************
;* _OVLMGR_FREE_EMS          *
;*****************************

; provide a way for application to free EMS used by overlay manager
; returns ah==EMS error code if unsuccessful
; destroys ax

_ovlmgr_free_ems    PROC
_om_fr_ems  EQU $
    push    ds              ; save critical register
    push    dx
    mov dx,cs
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

new_terminate   PROC
    push    ax              ; save all used registers
    push    bx
    push    dx
    push    ds
    call    _ovlmgr_free_ems

    mov ax,cs
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
	mov	dx,$$_ovl_load_start	; get segment base of block
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
    mov bx,swapfile_handle
    mov ah,3eh              ; close file
    int 21h
    mov dx,OFFSET swapfile_fullname
    mov ah,41h              ; delete file
    int 21h
    pop ds
    pop dx
    pop bx
    pop ax
    jmp DWORD PTR cs:psp_terminate  ; transfer to old terminate address
new_terminate   ENDP

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

xt_error:
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
    mov ax,cs
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
    mov ax,cs
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

;*****************************
;* WRITE_FROM_EMS            *
;*****************************

; transfer EMS to buffer, write to file from EMS transfer buffer, if EMS used
;  otherwise straight file write
; upon entry ds:dx-> EMS area to transfer from, bx == file handle,
;  cx holds byte count
; destroys NO registers other than typical function 40h return values

write_from_ems PROC	NEAR
    push    ds
    mov ax,cs
    mov ds,ax               ; ds -> overlay manager data
    cmp $$_ovl_ems_pool_flag,0	; see if EMS page frame used for pool
    pop ds
    jne	wfe_ems				; yes
    mov ah,40h
    int 21h
    ret

wfe_ems:
    push    si              ; save critical registers
    push    di
    push    es
    push    cx
    push    dx
    push    bp
    push    ds

    mov si,dx				; ds:si -> source (EMS) block
    mov ax,cs
    mov es,ax               ; es -> overlay manager data
    xor bp,bp               ; bp holds total bytes written

wfe_readloop:
    push    cx              ; save bytes to write
    cmp cx,2048            	; write only 2K chunk max
    jbe wfe_2
    mov cx,2048

; transfer from EMS block to transfer block
wfe_2:
	pop	ax					; get cx bytes to write value off of stack temporarily
	pop	ds					; ds -> source (EMS) block
	push	ds				; restore stack
	push	ax
	mov	dx,OFFSET ems_trans_block
	mov	di,dx				; es:di -> transfer block
	push	cx
    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any
	pop	cx

    mov ax,cs
    mov ds,ax               ; ds -> overlay manager data
    mov ah,40h              ; write file
    int 21h
    pop cx                  ; get bytes to write
    jc  wfe_out				; error reading file

    add bp,ax               ; update total bytes written
    sub cx,2048            	; subtract maximum write
    jbe wfe_ret             ; all written
    cmp ax,2048				; see if actual bytes written matched requested
    je  wfe_readloop        ; yes, not weird error

wfe_ret:
	clc						; clear carry for carry on file read is error checks
    mov ax,bp               ; get total bytes read in ax

wfe_out:
    pop ds
    pop bp
    pop dx
    pop cx
    pop es
    pop di
    pop si
    ret
write_from_ems ENDP

;*****************************
;* OVL_SWAPFILE_INIT         *
;*****************************

; init swapfile by writing zero'd dword file offsets in swap file header
; for as many entries as there are overlays
; destroys ax

ovl_swapfile_init   PROC
    push    bx              ; save critical register
    push    cx
    push    dx
    push    di
    push    es

;***    mov ax,ds
;***    mov es,ax               ; es -> overlay manager data
	push	ds
	pop	es					; es -> overlay manager data

    mov dx,OFFSET swapfile_buffer
    mov di,dx
	mov	di,OFFSET swapfile_buffer
    xor ax,ax
    mov cx,512
    rep stosw               ; zero out swap file buffer

    mov bx,swapfile_handle
;***    mov cx,$$_ovl_total_count   ; get total number of overlay dwords
    mov cx,ovl_seg_count	; get total number of overlays
	shl	cx,1
	shl	cx,1				; dword/overlay

    add cx,4                ; adjust for prefixed top of memory swap location dwords
    mov WORD PTR swapfile_size,cx   ; save current swapfile size (header only)

osi_1:
    push    cx              ; save count of bytes to write
    cmp cx,1024             ; see if more than 1K of header bytes to write
    jbe osi_2               ; no
    mov cx,1024             ; write max allowed

osi_2:
    mov ah,40h              ; write to file
    int 21h
    jc  osi_err             ; error writing to swap file

    pop cx                  ; restore count of bytes to write
    sub cx,ax               ; subtract off bytes written
    jne osi_1               ; not done yet

    mov swapfile_active,1   ; flag that swapfile is now active
    pop es                  ; restore critical register
    pop di
    pop dx
    pop cx
    pop bx
    ret

osi_err:
    JMP NEAR PTR $$_ovl_dos_error

ovl_swapfile_init   ENDP

;*****************************
;* OVL_SWAPFILE_READ         *
;*****************************

; read from Clarion active overlay data swap file
; upon entry es -> overlay loaded entry
; destroys ax,cx,dx

ovl_swapfile_read   PROC
    mov ax,es:[2]           ; get size of data in paras
    or  ax,ax               ; see if zero
    je  osr_ret             ; zero, nothing to read

    push    bx              ; save critical register
    call    ovl_swapfile_seek

; seek to data image
    mov dx,WORD PTR $$_ovl_temp_buff
    mov cx,WORD PTR $$_ovl_temp_buff+2
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h

; read _DT and _DAT data image
    mov ax,es:[8]           ; get overlay end segment
    mov cx,es:[2]           ; get size of data in paragraphs
    sub ax,cx               ; compute start of data overlays in dx
    xor dx,dx
    push    ds              ; save -> overlay manager data
    mov ds,ax               ; ds:dx -> read buffer
    shl cx,1                ; x2
    shl cx,1                ; x4
    shl cx,1                ; x8
    shl cx,1                ; x16

;***    mov ah,3fh              ; read file
;***    int 21h
	call	read_to_ems

    pop ds                  ; restore ds -> overlay manager data
    jc  osr_err             ; error occurred

    pop bx                  ; restore critical register

osr_ret:
    ret

osr_err:
    jmp NEAR PTR $$_ovl_dos_error

ovl_swapfile_read   ENDP

;*****************************
;* OVL_SWAPFILE_SEEK         *
;*****************************

; seek to proper overlay entry in swap file header and
; read it into $$_ovl_temp_buff
; upon entry es -> overlay loaded entry
; destroys ax,bx,cx,dx
; returns bx == swapfile handle

ovl_swapfile_seek   PROC
    mov bx,swapfile_handle
    mov dx,es:[0]           ; get overlay identifier
    dec dx                  ; make relative zero
    add dx,1                ; adjust for 1 system dword
    shl dx,1
    shl dx,1                ; convert to dword offset in header
    xor cx,cx               ; zero high word of offset
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h

    mov cx,4                ; read 1 dword entry
    mov dx,OFFSET $$_ovl_temp_buff
    mov ah,3fh              ; read file
    int 21h
    jc  oss_err             ; error occurred

    ret

oss_err:
    jmp NEAR PTR $$_ovl_dos_error

ovl_swapfile_seek   ENDP

;*****************************
;* OVL_SWAPFILE_WRITE        *
;*****************************

; write to Clarion active overlay data swap file
; upon entry es -> overlay loaded entry
; destroys ax

ovl_swapfile_write  PROC
    mov ax,es:[2]           ; get size of data in paras
    or  ax,ax               ; see if zero
    jne osw_1               ; nonzero, data exists

    ret                     ; zero, no data to write

osw_1:
    push    bx              ; save critical register
    push    cx
    push    dx
    call    ovl_swapfile_seek

; check if previous data image written (use that space)
    mov ax,WORD PTR $$_ovl_temp_buff
    or  ax,WORD PTR $$_ovl_temp_buff+2
    jne osw_2               ; previous data image

; no previous data image, rewind back to entry
    mov cx,0ffffh           ; negative sign high word of file offset
    mov dx,-4               ; rewind back 1 dword entry
    mov ax,4201h            ; move file pointer from current location
    int 21h

;, write old end of file as data image location
    mov ax,WORD PTR swapfile_size
    mov $$_ovl_temp_buff,ax
    mov ax,WORD PTR swapfile_size+2
    mov $$_ovl_temp_buff+2,ax
    mov cx,4                ; write 1 dword entry
    mov dx,OFFSET $$_ovl_temp_buff
    mov ah,40h              ; write file
    int 21h
    jc  osw_err             ; error occurred

    mov ax,es:[2]           ; get size of data in paragraphs
    shl ax,1                ; x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    shl ax,1                ; x16
    add WORD PTR swapfile_size,ax   ; update swapfile size
    adc WORD PTR swapfile_size+2,0  ; carry to high word

; seek to file position in $$_ovl_temp_buff
osw_2:
    mov dx,WORD PTR $$_ovl_temp_buff    ; get lsw of file position
    mov cx,WORD PTR $$_ovl_temp_buff+2  ; get msw
    mov ax,4200h            ; move file pointer, relative beginning of file
    int 21h

; write _DT and _DAT data image
    mov ax,es:[8]           ; get overlay end segment
    mov cx,es:[2]           ; get size of data in paragraphs
    sub ax,cx               ; compute start of data overlays in dx
    xor dx,dx
    push    ds              ; save -> overlay manager data
    mov ds,ax               ; ds:dx -> write buffer
    shl cx,1                ; x2
    shl cx,1                ; x4
    shl cx,1                ; x8
    shl cx,1                ; x16
;***    mov ah,40h              ; write file
;***    int 21h
	call	write_from_ems
    pop ds                  ; restore ds -> overlay manager data
    jc  osw_err             ; error occurred

    pop dx
    pop cx
    pop bx
    ret

osw_err:
    jmp NEAR PTR $$_ovl_dos_error

ovl_swapfile_write  ENDP

;*****************************
;* READ_SWAPFILE_IN          *
;*****************************

; read overlay allocations from swapfile
; destroys ax,bx,cx,dx,si,di

read_swapfile_in    PROC    NEAR
    mov bx,swapfile_handle
    xor cx,cx
    mov dx,cx
    mov ax,4200h
    int 21h                 ; rewind to start of file

    mov dx,OFFSET ovl_alloc_fpos
    mov cx,4
    mov ah,3fh              ; read from file directly into variables
    int 21h
    jc  rsi_error

    mov dx,WORD PTR ovl_alloc_fpos
    mov cx,WORD PTR ovl_alloc_fpos+2
    mov ax,4200h
    int 21h                 ; go to swapfile position

; get bytes to read in di:si
    mov si,total_alloc		; get total allocation in paras
    xor di,di
    shl si,1
    rcl di,1                ; x2
    shl si,1
    rcl di,1                ; x4
    shl si,1
    rcl di,1                ; x8
    shl si,1
    rcl di,1                ; x16

    push    ds
	mov	ds,overlay_info_seg
    xor dx,dx               ; ds:dx -> read buffer
;***    mov ax,si
;***    mov cx,di

rs_loop:
    or  di,di               ; see if 64K or more bytes to read
    je  rs_3                ; no

rs_max:
    mov cx,0fff0h           ; read 64K-16 bytes
    jmp SHORT rs_read

rsi_error:
    jmp NEAR PTR $$_ovl_dos_error

rsi_operror:
	mov	al,11h
    jmp NEAR PTR $$_ovl_op_error

rs_3:
    mov cx,si
    cmp cx,0fff0h           ; see if more than 64K-16 bytes to read
    ja  rs_max              ; yes, use maximum of 64K-16 bytes

rs_read:
;***    mov ah,3fh              ; read from file
;***    int 21h
	call	read_to_ems

    jc  rsi_error           ; DOS error occurred
    or  ax,ax
    je  rsi_operror			; error reading from file (no bytes read)

    sub si,ax               ; subtract off bytes read
    sbb di,0
    mov cx,di
    or  cx,si               ; see if any bytes left to read
    je  rs_ret              ; no
    mov ax,ds
    add ax,0fffh            ; bump to next read segment
    mov ds,ax
    jmp SHORT rs_loop

rs_ret:
    pop ds
    ret

read_swapfile_in    ENDP

;*****************************
;* WRITE_SWAPFILE_OUT        *
;*****************************

; write overlay allocations to swapfile
; destroys ax,bx,cx,dx,si,di

write_swapfile_out  PROC    NEAR
    cmp swapfile_active,0   ; see if swapfile is now active
    jne wswap_2             ; yes
    call    ovl_swapfile_init   ; init the swapfile

wswap_2:
    mov bx,swapfile_handle

    xor cx,cx
    mov dx,cx
    mov ax,4200h
    int 21h                 ; rewind to start of file

    mov dx,OFFSET ovl_alloc_fpos
    mov cx,4
    mov ah,3fh              ; read from file directly into variable
    int 21h
    jc  wso_error
    mov ax,WORD PTR ovl_alloc_fpos
    or  ax,WORD PTR ovl_alloc_fpos+2
    jne ws_2                ; already written once

    xor cx,cx
    mov dx,cx
    mov ax,4202h
    int 21h                 ; go to end of file
    mov WORD PTR ovl_alloc_fpos,ax  ; save overlay allocations position
    mov WORD PTR ovl_alloc_fpos+2,dx

    xor cx,cx
    mov dx,cx
    mov ax,4200h
    int 21h                 ; rewind to start of file

    mov dx,OFFSET ovl_alloc_fpos
    mov cx,4
    mov ah,40h              ; write variable into swap file
    int 21h
    jc  wso_error

ws_2:
    mov dx,WORD PTR ovl_alloc_fpos
    mov cx,WORD PTR ovl_alloc_fpos+2
    mov ax,4200h
    int 21h                 ; go to swapfile position

; get bytes to write in di:si
    mov si,total_alloc		; get total allocation in paras
    xor di,di
    shl si,1
    rcl di,1                ; x2
    shl si,1
    rcl di,1                ; x4
    shl si,1
    rcl di,1                ; x8
    shl si,1
    rcl di,1                ; x16

    push    ds
	mov	ds,overlay_info_seg
    xor dx,dx               ; ds:dx -> write buffer

ws_loop:
    or  di,di               ; see if 64K or more bytes to write
    je  ws_3                ; no

ws_max:
    mov cx,0fff0h           ; write 64K-16 bytes
    jmp SHORT ws_write

wso_error:
    jmp NEAR PTR $$_ovl_dos_error

wso_operror:
	mov	al,10h
    jmp NEAR PTR $$_ovl_op_error

ws_3:
    mov cx,si
    cmp cx,0fff0h           ; see if more than 64K-16 bytes to write
    ja  ws_max              ; yes, use maximum of 64K-16 bytes

ws_write:
    mov ah,40h              ; write to file
    int 21h
;***	call	write_from_ems

    jc  wso_error           ; error occurred
    or  ax,ax
    je  wso_operror			; error writing to file (no bytes written)

    sub si,ax               ; subtract off bytes written
    sbb di,0
    mov cx,di
    or  cx,si               ; see if any bytes left to read
    je  ws_ret              ; no
    mov ax,ds
    add ax,0fffh            ; bump to next write segment
    mov ds,ax
    jmp SHORT ws_loop

ws_ret:
    pop ds
    ret

write_swapfile_out  ENDP

;*****************************
;* OVLMGR_BEFORE_RUN         *
;*****************************

; save to swap file
; deallocate memory before Clarion RUN
; re-init proper pointers

ovlmgr_before_run   PROC    FAR
    push    ax              ; save all used registers
    push    bx
    push    cx
    push    dx
    push    si
    push    di
    push    ds
    push    es
    mov ax,cs
    mov ds,ax               ; ds -> overlay manager data

	mov	ah,47h				; save page map
	mov	dx,$$_ovl_ox_handle
	or	dx,dx
	je	obr_1
	int	67h

obr_1:
COMMENT #
    call    write_swapfile_out

; deallocate overlay pointers
    mov es,overlay_info_seg
    mov ah,49h
    int 21h

    mov ax,old_top_of_mem
    mov ds,cs:prog_psp
    mov ds:[2],ax           ; restore top of memory pointer to PSP
END COMMENT #

    pop es
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    jmp DWORD PTR cs:beforerun_offset
ovlmgr_before_run   ENDP

;*****************************
;* OVLMGR_AFTER_RUN          *
;*****************************

; restore from swap file
; reallocate memory after Clarion RUN
; set up proper pointers

ovlmgr_after_run    PROC    FAR
    push    ax              ; save all used registers
    push    bx
    push    cx
    push    dx
    push    si
    push    di
    push    ds
    push    es
    mov ax,cs
    mov ds,ax               ; ds -> overlay manager data

	mov	ah,48h				; restore page map
	mov	dx,$$_ovl_ox_handle
	or	dx,dx
	je	oar_1
	int	67h

; restore memory to original status
oar_1:
COMMENT #
    mov bx,mod_mem_alloc
    mov es,prog_psp
    mov ah,4ah              ; modify memory block
    int 21h

; reallocate overlay pointers
    mov bx,total_alloc
    mov ah,48h
    int 21h
    call    read_swapfile_in

; restore memory to what clarion set it to
    mov si,cla_bx_runval
    mov di,cla_es_runval
    mov ax,runit_segment
    mov es,ax
    mov bx,es:[si]
    mov es,es:[di]
    mov ah,4ah              ; modify memory block
    int 21h

    mov ax,new_top_of_mem
    mov ds,cs:prog_psp
    mov ds:[2],ax           ; restore top of memory pointer to PSP
END COMMENT #

    pop es
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    jmp DWORD PTR cs:afterrun_offset
ovlmgr_after_run    ENDP

;*****************************
;* NEW_CLA_RESTART           *
;*****************************

new_cla_restart PROC
    push    ds
    push    cx
    push    ax
    mov ax,cs
    mov ds,ax               ; ds -> overlay manager data
    mov ax,$$_ovl_stack     ; get stack size in bytes
    mov $$_ovl_stack_ptr,ax	; init stack pointer to top of stack
    xor ax,ax
    mov $$_ovl_loaded_count,ax
	mov	last_access_slot,ax
    mov swapfile_count,ax   ; zero count of active overlays in swapfile
    mov WORD PTR $$_ovl_lru_counter+2,ax    ; re-init least recently used counter
    inc ax
    mov WORD PTR $$_ovl_lru_counter,ax

    mov cx,$$_ovl_max_load  ; get array elements to zero
    mov ds,$$_ovl_array_start   ; get segment of overlay loaded array
    xor ax,ax
    mov si,ax               ; si offsets into array area

ncr_loop:
    mov [si],ax             ; zero the array element identifiers
    mov [si+4],ax           ; zero activity level
    add si,16               ; move to next element
    loop    ncr_loop        ; loop until all ids are zeroed

    pop ax
    pop cx
    pop ds

restart_instr:
    DB  6 DUP (?)           ; instructions overwritten by jump to new restart
    jmp DWORD PTR cs:[restart_back]
new_cla_restart ENDP

;*****************************
;* OPEN_CLAR_SWAPFILE        *
;*****************************

; open clarion active overlay data swap file
; destroys ax,bx,cx,dx,si

open_clar_swapfile  PROC
    call    $$_ovl_swapfile_evar    ; use swapfile name in environment variable if any
    or  cl,cl               ; see if e-var match
    jne ocs_2               ; yes

; transfer swapfile base name to full name
    push    di              ; save critical register
    push    es
    push    ds
    pop es
    mov di,OFFSET swapfile_fullname ; es:di -> overlay manager data
    mov si,OFFSET swapfile_basename
    mov cx,13
    rep movsb
    pop es                  ; restore critical register
    pop di

ocs_2:
    mov dx,OFFSET swapfile_fullname ; ds:dx -> file spec
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    jc  ocs_makefile        ; error, assume doesn't exist

; file exists, try next file (.$02, .$03, etc.)
    mov si,swapfile_extptr
    cmp BYTE PTR [si],'9'   ; see if at end of one digits
    jae ocs_bumptens        ; yes
    inc BYTE PTR [si]       ; bump file digits
    jmp SHORT ocs_2         ; try this file

ocs_bumptens:
    cmp BYTE PTR [si-1],'9' ; see if at end of tens
    jae ocs_err             ; problem finding file, error from $01-$99
    mov BYTE PTR [si],'0'   ; reset ones digit
    inc BYTE PTR [si-1]
    jmp SHORT ocs_2         ; try new tens digit

ocs_makefile:
    xor cx,cx               ; normal file
    mov ah,3ch              ; create or truncate file
    int 21h
    jc  ocs_err             ; error occurred

    mov cs:swapfile_handle,ax   ; save swap file handle
    ret

ocs_err:
    jmp NEAR PTR $$_ovl_dos_error

open_clar_swapfile  ENDP

;*****************************
;* $$_OVL_SWAPFILE_EVAR      *
;*****************************

; find swapfile name using LPM_SWAP environment string
; es -> PSP upon entry
; dx -> file name upon entry
; destroys ax,cx,dx
; if swapfile found return cl != 0, ax holds file handle
; if not found return cl==0

$$_ovl_swapfile_evar    PROC    NEAR
    push    es              ; save PSP value on stack
    push    di              ; save critical register
    push    si

    mov ax,es:[2ch]         ; get environment segment from offset 2ch in PSP
    mov es,ax               ; es -> environment segment
    mov bx,OFFSET swapfile_evar ; bx holds target string address for compares
    xor si,si               ; starting location for target string check

ose_find_evar:
    xor di,di               ; offset into target string

ose_loop:
    lods    BYTE PTR es:0   ; get byte from environment string
    cmp al,[bx+di]          ; does environment char match LPM_SWAP= string char
    je  ose_byte_match      ; yes, try next location
    or  al,es:[si]          ; two zero values in a row mean the end of the environment
    jne ose_find_evar       ; not the end of the environment

ose_not_found:
    xor cl,cl               ; flag no match

ose_ret:
    pop si                  ; restore critical register
    pop di
    pop es
    ret

; check that LPM_SWAP= is not part of another environment string
ose_byte_match:
    or  di,di               ; di is zero if first char is matched
    jne ose_2               ; not first char, test already done
    cmp si,1                ; si equals one if LPM_SWAP is first string in environment block
    je  ose_2               ; no previous environment string
    cmp BYTE PTR es:[si-2],0    ; check if char before LPM_SWAP was nonzero
    jne ose_find_evar       ; yes, LIB is a subset of another string, keep looking

ose_2:
    inc di                  ; a match, move to next byte of target string
    cmp di,9                ; check if all bytes matched
    jb  ose_loop            ; not yet, keep comparing

ose_evar_found:
    mov bx,OFFSET swapfile_fullname ; bx-> place to put swapfile name

ose_4:
    lods    BYTE PTR es:0   ; get filename char
    cmp al,';'              ; check if terminator character
    je  ose_prefix_complete ; yes, check file's existence with the current path prefix
    cmp al,' '              ; anything less than a space is also a terminator character
    jb  ose_prefix_complete
    mov [bx],al             ; save path character
    inc bx                  ; move to next name slot
    jmp SHORT ose_4         ; loop for next character

ose_prefix_complete:
    mov BYTE PTR [bx],'.'   ; add '.$01' extension
    mov BYTE PTR [bx+1],'$'
    mov BYTE PTR [bx+2],'0'
    add bx,3
    mov swapfile_extptr,bx  ; save -> extension digit
    mov BYTE PTR [bx],'1'
    mov BYTE PTR [bx+1],0
    mov cl,1                ; flag file found
    jmp SHORT ose_ret       ; and return

$$_ovl_swapfile_evar    ENDP

;*****************************
;* SET_CLA_XQU_VECT          *
;*****************************

; set from vector from _cla_xqu to overlay manager control
; look for signature bytes of 6,53h,0cbh (push es, push bx, retf)
; destroys ax,cx,si

set_cla_xqu_vect    PROC
	push	ds				; save critical register
    mov ax,cs:_cla_xqu_segment	; get segment of clarion control handling routine
    mov ds,ax               ; ds -> routine
    mov si,cs:_cla_xqu_offset	; init offset into routine

set2:
    lodsb                   ; get first byte

set3:
    cmp al,6                ; check if proper first byte value
    jne set2                ; no
    lodsb                   ; get second byte
    cmp al,53h
    jne set3                ; no match
    lodsb                   ; get third byte
    cmp al,0cbh
    jne set3                ; no match

; bytes match, target found, change opcodes to far jump vector
    sub si,3                ; back si up to start of match
    mov BYTE PTR [si],0eah  ; put in JMP addr opcodes
    mov ax,OFFSET $$_ovl_cla_xqu_vect
    mov [si+1],ax
    mov ax,cs
    mov [si+3],ax

COMMENT !
    mov ax,cs:runit_segment	; get segment of clarion RUN dispatcher
    mov ds,ax               ; ds -> routine
    mov si,cs:runit_offset	; init offset into routine
    mov cx,2                ; search for second of mov ah,4ah int 21 (B4 4A CD 21)

set_4:
    lodsb
    cmp al,0b4h
    jne set_4
    lodsb
    cmp al,4ah
    jne set_4
    lodsb
    cmp al,0cdh
    jne set_4
    lodsb
    cmp al,21h
    jne set_4
    loop    set_4           ; find second of matches

    sub si,11
    lodsw
    mov cs:cla_bx_runval,ax ; get bx modify memory address
    add si,3
    lodsw
    mov cs:cla_es_runval,ax ; get es modify memory address

; modify runit's beforerun call to vector to overlay manager
    mov si,cs:runit_offset	; init offset into routine
    mov cx,cs:beforerun_offset
    mov bx,cs:beforerun_segment

set_5:
    lodsb
    cmp al,09ah
    jne set_5
    lodsb
    cmp al,cl
    jne set_5
    lodsb
    cmp al,ch
    jne set_5
    lodsb
    cmp al,bl
    jne set_5
    lodsb
    cmp al,bh
    jne set_5

    sub si,4
    mov ax,OFFSET ovlmgr_before_run
    mov ds:[si],ax
    mov ax,cs
    mov ds:[si+2],ax

; modify runit's beforerun call to vector to overlay manager
    mov si,cs:runit_offset		; init offset into routine
    mov cx,cs:afterrun_offset
    mov bx,cs:afterrun_segment

set_6:
    lodsb
    cmp al,09ah
    jne set_6
    lodsb
    cmp al,cl
    jne set_6
    lodsb
    cmp al,ch
    jne set_6
    lodsb
    cmp al,bl
    jne set_6
    lodsb
    cmp al,bh
    jne set_6

    sub si,4
    mov ax,OFFSET ovlmgr_after_run
    mov ds:[si],ax
    mov ax,cs
    mov ds:[si+2],ax
END COMMENT !

; revector _cla_restart so can reset several overlay manager variables during RESTART
    mov ax,cs:_cla_restart_segment
    mov WORD PTR cs:[restart_back+2],ax ; save segment to return to from new restart
    mov ds,ax
    mov ax,cs:_cla_restart_offset
    mov si,ax
    add ax,6                ; adjust past 6 bytes saved
    mov WORD PTR cs:[restart_back],ax   ; save offset to return to past 6 saved instruction bytes overwritten

; ds:si -> start of _cla_restart, save/overwrite 6 bytes
    lodsw
    mov WORD PTR cs:[restart_instr],ax
    lodsw
    mov WORD PTR cs:[restart_instr+2],ax
    lodsw
    mov WORD PTR cs:[restart_instr+4],ax
    sub si,6                ; restore si to start of _cla_restart
    mov al,0eah             ; punch in jump to NEW_CLA_RESTART procedure
    mov ds:[si],al
    mov ax,OFFSET new_cla_restart
    mov ds:[si+1],ax
    mov ax,cs
    mov ds:[si+3],ax
	pop	ds					; restore critical register
    ret
set_cla_xqu_vect    ENDP

cut_here	EQU	$			; used to compute number of DDL manager bytes to relocate

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
    mov ebuff.es_shandle,dx
    mov ebuff.es_dhandle,dx

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
;***    jnc gfp_2				; EMS 4.0 available
    jnc gfp_3				; EMS 4.0 available

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
;* $$_OVL_INIT               *
;*****************************

; initial code performed when executable linked with overlays
; upon entry ds -> code segment data
; destroys ax,bx,cx,dx,di,si

$$_ovl_init PROC
	mov	ax,ovl_alloc_start
	mov	cx,$$_ovl_max_load
	sub ax,cx				; allocate space for overlay load array elements
	mov	$$_ovl_array_start,ax

	mov	ds,ax				; ds -> overlay array start
	xor ax,ax
	mov bx,ax               ; bx offsets into array area

array_zloop:
	mov [bx],ax             ; zero the array element identifiers
	mov [bx+4],ax           ; zero activity level
	add bx,16               ; move to next element
	loop    array_zloop     ; loop until all ids are zeroed

	push	cs
	pop	ds					; ds -> code segment data

	mov	ax,$$_ovl_array_start
	mov	bx,$$_ovl_stack
	mov $$_ovl_stack_ptr,bx	; init stack pointer to top of stack
	mov cl,4
	shr bx,cl               ; convert stack to paras
	sub ax,bx				; allocate space for overlay stack
	mov	ovl_alloc_start,ax
	mov	$$_ovl_stack_start,ax

	sub	ax,prog_end			; get free space
	jc	memerr				; out of memory

    mov cx,WORD PTR $$_ovl_pool+1 ; get 512 byte blocks*2
    add cx,15               ; round up
    shr cx,1
    shr cx,1
    shr cx,1
    shr cx,1                ; get paragraph count
    sub ax,cx               ; subtract off 2 bytes/512 byte page
	jnc	init_chkumb			; memory okay

memerr:
	mov	ax,8				; force out of memory
    jmp NEAR PTR $$_ovl_dos_error

; check if UMB specified, if so, see if XMS available get largest UMB
init_chkumb:
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

    mov $$_ovl_load_start,ax	; save segment of overlay load area
    add ax,$$_ovl_amt_alloc ; calculate paragraph just above overlay load area
    mov $$_ovl_load_end,ax	; save to memory variable

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

; allocate up to amount of memory in ovl_pool
init_use_pool:
	cmp	$$_ovl_umb,0		; check if using UMB for overlay pool
	je	init_noumb			; no
	cmp	umb_avail,cx		; see if enough UMB space for overlay pool
	jb	init_noumb			; no
	call	umb_pool_alloc	; allocate UMB for overlay pool, using size in cx
    xor cx,cx               ; zero overlay pool size to allocate
	mov	ax,$$_ovl_amt_512	; 512 byte chunks allocated in ax
    jmp SHORT init_compalloc

init_noumb:
	mov	$$_ovl_umb,0		; fail using UMB for overlay pool
    cmp ax,cx               ; see if free memory is less than overlay pool size
	ja	init_memok			; no
	jmp	NEAR PTR memerr		; yes force out of memory error

init_memok:
    mov ax,cx               ; only allocate amount of memory up to maximum

; ax holds, in paragraphs, the amount of memory to allocate for the memory pool
init_allocate:
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
	add	ax,ax				; check array size is 512 bytes*2 (word per)
    mov check_array_size,ax ; keep it

	add	ax,15				; round up to next para
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1				; convert to paras
	mov	dx,ovl_alloc_start
	sub dx,ax				; allocate space for check array
	mov	check_array,dx

	sub	dx,(BIN_HEADER_SIZE+15)/16	; allocate space for binary header
	mov	binheader_seg,dx	; save segment
	mov	ovl_alloc_start,dx

	mov	al,$$_ovl_ems_pool_flag
	or	al,$$_ovl_umb		; see if EMS or UMB overlay pool used
    jne init_term			; yes, bypass allocation and setup

    mov $$_ovl_load_end,dx	; binary header is end of overlay load area
	sub	dx,cx				; subtract off overlay pool allocation
    mov $$_ovl_load_start,dx	; save segment of overlay load area
	mov	ovl_alloc_start,dx

; save old termination address in PSP
init_term:
	mov	es,prog_psp
    mov ax,es:[10]
    mov WORD PTR psp_terminate,ax
    mov ax,es:[12]
    mov WORD PTR psp_terminate+2,ax

; allocate space for /oht or /ohp overlay file swapouts if requested
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
	ret
$$_ovl_init ENDP

;*****************************
;* SAVE_SYSTEM_VARIABLES     *
;*****************************

; transfer system variable values to permanent storage from DDL header
; destroys ax,cx,dx,si,di,ds,es

save_system_variables	PROC	NEAR
	mov ds,cs:ddl_mainhdr_seg   ; ds -> header info for main DDL

	mov	ax,ds:[dh_st]
	mov	cs:stack_setting,ax
	mov	ax,ds:[dh_ol]
	mov	cs:$$_ovl_max_load,ax
	mov	ax,ds:[dh_os]
	mov	cs:$$_ovl_stack,ax
	mov	al,ds:[dh_ou]
	mov	cs:$$_ovl_umb,al
	mov	al,ds:[dh_r]
;***	mov	cs:$$_ovl_reload_flag,al
	mov	cs:$$_ovl_reload_flag,1
	mov	al,ds:[dh_orp]
;***	mov	cs:$$_ovl_orp,al
	mov	cs:$$_ovl_orp,0
	mov	al,ds:[dh_ort]
;***	mov	cs:$$_ovl_ort,al
	mov	cs:$$_ovl_ort,0
	mov	al,ds:[dh_mem]
	mov	cs:$$_ovl_mem_alloc_flag,al
	mov	al,ds:[dh_ox]
	mov	cs:$$_ovl_ems_pool_flag,al
	mov	ax,ds:[dh_ohp]
	or	ax,ax
	je	ssv_2				; no /ohp setting
;***	mov	cs:$$_ovl_ohp,1
	mov	cs:$$_ovl_ohp,0

ssv_2:
	mov	cs:$$_ohp_size,ax
	mov	al,ds:[dh_ohp_flag]
	mov	cs:$$_ohp_flag,al

	mov	ax,ds:[dh_oht]
	or	ax,ax
	je	ssv_3				; no /oht setting
;***	mov	cs:$$_ovl_oht,1
	mov	cs:$$_ovl_oht,0

ssv_3:
	mov	cs:$$_oht_size,ax
	mov	al,ds:[dh_oht_flag]
	mov	cs:$$_oht_flag,al

	mov	si,OFFSET ds:[dh_oxevar]	; ds:si -> /ox e-var source
	mov	di,OFFSET $$_ovl_ox_evar
	push	cs
	pop	es					; es:di -> /ox e-var storage
	mov	cx,16				; 32 bytes, 16 words
	rep	movsw

	mov al,ds:[dh_minop]
	or  al,al               ; see if /op:m specified
	je  ssv_notopm			; no

; have to compute minimum overlay pool size
	mov ax,cs:largest_ovl	; get largest overlay size in paras
;***	cmp	ds:[dh_r],0			; see if /r specified
;***	je	ssv_round			; no, just use largest overlay size for pool
	add ax,63               ; round up to next 1K block
	and al,0c0h				; cut off at 1K boundary
	add ax,ax
	mov	dx,cs:second_ovl
	add dx,63               ; round up to next 1K block
	and dl,0c0h				; cut off at 1K boundary
	add ax,dx
	dec ax                  ; ax == 2*largest overlay + 2nd largest overlay - 1

ssv_round:
	add ax,63               ; round up to next 1K block
	and al,0c0h				; cut off at 1K boundary
	xor	dx,dx
	shl	ax,1				; convert paras in ax to bytes in dx:ax
	rcl	dx,1				; x2
	shl	ax,1
	rcl	dx,1				; x4
	shl	ax,1
	rcl	dx,1				; x8
	shl	ax,1
	rcl	dx,1				; x16
	jmp	SHORT ssv_op

ssv_notopm:
	mov	ax,WORD PTR ds:[dh_op]
	mov	dx,WORD PTR ds:[dh_op+2]

; save overlay pool size in bytes, value in dx:ax
ssv_op:
	mov	WORD PTR cs:$$_ovl_pool,ax
	mov	WORD PTR cs:$$_ovl_pool+2,dx

    mov ax,WORD PTR cs:image_written	; get low word of image size
    add ax,15				; round up for paragraph computation
    mov dx,WORD PTR cs:image_written+2	; get high word of image size
    adc dx,0				; carry to high word

; convert image size in dx:ax to paragraphs
    shr dx,1
    rcr ax,1                ; /2
    shr dx,1
    rcr ax,1                ; /4
    shr dx,1
    rcr ax,1                ; /8
    shr dx,1                ; dx should be zero by the final shift
    rcr ax,1                ; /16
	mov	cs:progsize_paras,ax	; save paragraphs of program size
	add	ax,cs:prog_seg
	mov	cs:prog_end,ax		; save program end

	ret
save_system_variables	ENDP

cseg    ENDS
END	startup
