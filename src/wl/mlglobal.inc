;*********************************************************************
;*   MLGLOBAL.INC                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/21/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Global data public declarations                                 *
;*                                                                   *
;*********************************************************************

PUBLIC  is_args,is_comfile,is_mapfile,is_casesense,is_nodeflib
PUBLIC  is_ondisk,is_msextlib,is_dosseg,is_dgroup
PUBLIC  is_inlib,is_inmem,is_mapexpand,is_reload,is_stackval
PUBLIC  is_maxparval,is_nowarn,is_exit0,is_beep,is_linkinfo
PUBLIC  is_tempfile
PUBLIC	is_quick,is_sympac,is_ohp,is_oht,is_umb,ems3_flag
PUBLIC  ctrlbreak,dos_version,wrap_flag,prev_flag,eof_flag
PUBLIC  warn_count,stack_value,maxpar_value
PUBLIC  obj_count,lib_count,seg_count,current_obj, current_lib
PUBLIC  module_count,segdef_count
PUBLIC  number_reloc,communal_count
PUBLIC  pub_sym_count,tot_sym_count
PUBLIC  highest_exe_write

; linker overlay and language specific related values
IFNDEF JUNIOR
PUBLIC  is_anyovls,is_clip5,is_min_pool
PUBLIC  obj_ovl_flag
PUBLIC  ovl_ox_evar
PUBLIC  ovl_class,exact_ovl_class,ovl_pool,ovl_mem_alloc
PUBLIC  ovl_max_load,ovl_max_load_size,ovl_stack
PUBLIC  ovl_filename,ovl_nopath,ddldat_filename
PUBLIC  ovl_count
PUBLIC  ovl_handle
PUBLIC  ovl_ioblk
PUBLIC  seg_ovlclass
PUBLIC  ovlpub_array,ovl_reloc_array,lookup_tbl_array,ind_tbl_array
PUBLIC  first_ovlpubblk_ptr,alloc_ovlpubblk_ptr
PUBLIC  ovl_pubcount
PUBLIC  lookup_tbl_segdef
PUBLIC  ind_tbl_segdef
PUBLIC  segcall_tbl_segdef
PUBLIC  largest_ovl
PUBLIC  second_ovl
PUBLIC  ovl_code_id
PUBLIC  ovl_data_id
PUBLIC  data_ovlblk_ptr
PUBLIC  nonovl_rvect
PUBLIC  inc_padval,is_clpinc,is_smartmem,is_xms
PUBLIC  is_clarion,is_internal,is_no_ems,is_ems_ovlpool
PUBLIC  inc_seg_clcode
PUBLIC  _dt_seg_size,_dat_seg_size
PUBLIC  _dt_seg_index,_dat_seg_index
PUBLIC  ems_handle,ems_base,ems_pagecount
PUBLIC  ems_currmap,ems_page_avail
PUBLIC  tmp_in_emsxms,tmp_in_xms
PUBLIC  xms_addr
PUBLIC  is_ddl,use_ddl,any_ddl
PUBLIC  is_xtstash,is_xpstash
PUBLIC  udl_proc_pass
PUBLIC	ovl_ohp_size,ovl_oht_size
PUBLIC	ovl_ohp_alloc,ovl_oht_alloc
ENDIF

PUBLIC  first_objblk_ptr,last_objblk_ptr,first_libblk_ptr,last_libblk_ptr
PUBLIC  first_pdeclblk_ptr,first_cdeclblk_ptr
PUBLIC  alloc_pdeclblk_ptr,alloc_cdeclblk_ptr
PUBLIC  alloc_pdnameblk_ptr,alloc_lnamesblk_ptr
PUBLIC  first_segdefblk_ptr,alloc_segdefblk_ptr
PUBLIC  first_segpartblk_ptr,alloc_segpartblk_ptr
PUBLIC  first_grpblk_ptr,alloc_grpblk_ptr
PUBLIC  first_relblk_ptr,alloc_relblk_ptr
PUBLIC  first_libent_ptr,alloc_libent_ptr
;***PUBLIC  first_local_ptr
PUBLIC  first_fixblk_ptr,alloc_fixblk_ptr
PUBLIC  first_binblk_ptr,alloc_binblk_ptr
PUBLIC  psp
PUBLIC  _edata_pubaddr,_end_pubaddr,_edata_segaddr,_end_segaddr
PUBLIC  image_handle,image_mem_ptr,map_handle
PUBLIC  memory_blk_base,memory_blk_size,memory_blk_end
PUBLIC  allocation_base,allocation_top
PUBLIC  buffer_base,buffer_end,buffer_head,buffer_tail,buffer_size
PUBLIC  read_buff_ptr,prev_read_ptr,prev_rec_len
PUBLIC  current_lnames,current_extdef,current_segdef,current_grpdef
PUBLIC  eof_pos,data_fixup_count
PUBLIC  stack_segval,stack_offval
PUBLIC  entry_segval,entry_offval

PUBLIC  file_pos_adj,prev_pos_adj,lib_pos_adj
PUBLIC  image_size,lib_id

PUBLIC  cmd_line,prev_libname,tmod_name,exe_name,map_name,ovl_class_name
PUBLIC  pubdecl_hash,segdef_hash
PUBLIC  zero_table
PUBLIC  lnames_hash
PUBLIC  frame_thrd_meth,target_thrd_meth
PUBLIC  seg_partent_indptr,seg_defent_indptr
PUBLIC  grp_ent_indptr,ext_defent_indptr
PUBLIC  env_opt_storage,lib_page_storage,data_fixup_flag
PUBLIC  frame_thrd_index,target_thrd_index
PUBLIC  lnames_ent_indptr,ddl_symbol_lookup,ddl_hold_buff
PUBLIC  master_segblk
PUBLIC  ovl_filepos_blk

;*****************************
;   Initialized data         *
;*****************************

.DATA

; linker defaults, byte values
EVEN                        ; maximize speed on 8086 and better
is_args         DB  0       ; nonzero if arguments passed to linker
EVEN
is_comfile      DB  0       ; nonzero if .COM file creation indicated
EVEN
is_mapfile      DB  0       ; nonzero if .MAP file creation indicated
EVEN
is_mapexpand    DB  0       ; nonzero if expanded .MAP file indicated
EVEN
is_casesense    DB  0       ; nonzero if symbol names are case sensitive
EVEN
is_ondisk       DB  0       ; nonzero if program image in temporary disk file
EVEN
is_inmem        DB  0       ; nonzero if program image is in memory
EVEN
is_msextlib     DB  0       ; nonzero if use extended ms lib format
EVEN
is_nodeflib     DB  0       ; nonzero if no default libraries
EVEN
is_dosseg       DB  0       ; nonzero if DOSSEG segment ordering specified
EVEN
is_stackval     DB  0       ; nonzero if stack size set by /st option
EVEN
wrap_flag       DB  0       ; nonzero if read from file buffer wraps to beginning of buffer
EVEN
prev_flag       DB  0       ; nonzero if keeping previously read record (pass 2 L?DATA record prior to fixup)
EVEN
eof_flag        DB  0       ; nonzero if end of file was encountered loading file buffer
EVEN
is_dgroup       DB  0       ; nonzero if DGROUP group encountered
EVEN
is_inlib        DB  0       ; nonzero if currently processing LIB file
EVEN
is_anyovls      DB  0       ; nonzero if any overlaid modules were specified
EVEN
is_reload       DB  0       ; nonzero if reload active but swapped out overlays
EVEN
is_maxparval    DB  0       ; maximum paragraph allocation specified
EVEN
is_nowarn       DB  0       ; nonzero if warnings disabled
EVEN
is_exit0        DB  0       ; nonzero if warnings generate exit code of 0
EVEN
is_beep         DB  0       ; nonzero if beep on exit
EVEN
is_linkinfo     DB  0       ; nonzero if print linker info
EVEN
is_tempfile     DB  0       ; nonzero if temporary file name specified
EVEN
is_no_ems       DB  1       ; default is set, nonzero if don't use available EMS during link
EVEN
is_ems_ovlpool  DB  0       ; nonzero if EMS used for overlay pool
EVEN
is_internal     DB  0       ; nonzero if internal overlays specified
EVEN
is_clarion      DB  0       ; clarion overlays specified
EVEN
is_smartmem     DB  0       ; nonzero if using SMARTMEM.XXX functions
EVEN
is_clpinc       DB  0       ; nonzero if Clipper incremental link
EVEN
inc_padval      DB  48      ; pad value to add to segments for incremental link
EVEN
is_clip5        DB  0       ; zero if Clipper 5 overlays specified
EVEN
tmp_in_emsxms   DB  0       ; nonzero if temporary file image is in EMS/XMS
EVEN
tmp_in_xms      DB  0       ; nonzero if temporary file image in XMS only
EVEN
is_xms          DB  0       ; nonzero if XMS present and available
EVEN
is_min_pool     DB  0       ; nonzero if minimum pool specified through op:m option
EVEN
is_ddl          DB  0       ; nonzero if creating DDL
EVEN
use_ddl         DB  0       ; nonzero if using DDL(s)
EVEN
any_ddl         DB  0       ; nonzero if creating OR using DDL(s)
EVEN
is_xtstash      DB  0       ; nonzero if stashing swapped overlays in extended memory (/ort)
EVEN
is_xpstash      DB  0       ; nonzero if stashing swapped overlays in EMS 4.0 (/orp)
EVEN
is_quick		DB	0		; nonzero if QuickLinker option set (/ql)
EVEN
is_sympac		DB	0		; nonzero if automatic symbol table compaction for Clipper
EVEN
is_ohp			DB	0		; nonzero if ohp option used
EVEN
is_oht			DB	0		; nonzero if oht option used
EVEN
is_umb			DB	0		; nonzero if UMB overlay option (/ou) used
EVEN
ems3_flag		DB	0		; nonzero if explicit EMS 3.0 compatibility specified for /ohp

; linker defaults, word values
EVEN
stack_value DW  0           ; stack size set by /st option
maxpar_value    DW  0       ; maximum paragraph allocation set by /pa option
warn_count  DW  0           ; count of warning messages
obj_count   DW  0           ; count of object modules
lib_count   DW  0           ; count of libraries
module_count    DW  0       ; count of all modules linked, object and library
seg_count   DW  0           ; count of all discrete (after combined) segments in all object modules
segdef_count    DW  0       ; count of all different segments
current_obj DW  0           ; current object module
current_lib DW  0           ; current library
number_reloc    DW  0       ; number of relocation items in .EXE header
communal_count  DW  0       ; count of communal variables
stack_segval    DW  0       ; program's initial stack segment (SS) value
stack_offval    DW  0       ; program's initial stack pointer (SP) value
entry_segval    DW  0       ; program's initial code segment (CS) value
entry_offval    DW  0       ; program's initial instruction pointer (IP) value

; linker defaults, doubleword values
pub_sym_count   DD  0       ; count of all public symbols
tot_sym_count   DD  0       ; count of all symbols
highest_exe_write   DD  0   ; value of highest write for beginning of program

; linker defaults, overlay related values
IFNDEF JUNIOR
ovl_ox_evar DB  32 DUP (0)  ; /ox environment variable setting
ovl_class   DB  'CODE',0,123 DUP (0)    ; overlay class name
exact_ovl_class DB  0       ; nonzero if exact overlay class
EVEN
ovl_pool    DD  147456      ; overlay pool size, user set maximum 512K
ovl_ohp_size	DW	512		; overlay expanded file stash amount in K
ovl_oht_size	DW	0		; overlay extended file stash amount in K
ovl_mem_alloc   DB  0       ; zero if overlay pool is free memory, nonzero if allocated memory
udl_proc_pass   DB  0       ; 0 if not creating a LIB'ed UDL, 1 if first pass, 2 if second
ovl_ohp_alloc	DB	0		; zero if overlay stash amount in expanded is free, nonzero if allocated
ovl_oht_alloc	DB	0		; zero if overlay stash amount in extended is free, nonzero if allocated
EVEN
ovl_max_load    DW  96     	; maximum number of loaded overlays, user set maximum 512
ovl_max_load_size   DW  96*16	; size of array for loaded overlay (ovl_max_load*16)
ovl_stack   DW  2048        ; overlay stack size, user set maximum 63K
ovl_count   DW  0           ; count of overlays
ovl_pubcount    DW  0       ; count of overlaid publics
ddldat_filename LABEL   BYTE    ; name of DAT file for DDL file to read
ovl_filename    DB  128 DUP (0) ; name of overlay file, including .OVL extension and path
ovl_nopath  DB  128 DUP (0) ; name of overlay file WITHOUT prepended path (if any)
first_ovlpubblk_ptr DW  0   ; segment of first overlaid public declarations block
alloc_ovlpubblk_ptr DW  0   ; segment of last allocated overlaid public declarations block
largest_ovl DW  0           ; size of largest overlay
second_ovl  DW  0           ; size of second largest overlay
ovl_code_id DW  0           ; overlay code segment identifier, for writing L?DATA segments
ovl_data_id DW  0           ; overlay data segment identifier, for writing Clarion _DT and _DAT LEDATA segments
ovl_handle  DW  0           ; handle of .OVL overlay file
data_ovlblk_ptr DW  0       ; segment of first data overlay block
ems_handle  DW  0           ; handle of EMS blocks used by WarpLink
ems_currmap DW  4 DUP (-1)  ; current logical page mapping of EMS physical pages
ENDIF

; zero initialized segment word pointers to various control blocks

first_objblk_ptr    DW  0   ; segment of first object module name block
last_objblk_ptr     DW  0   ; segment of last object module name block
first_libblk_ptr    DW  0   ; segment of first library block
last_libblk_ptr     DW  0   ; segment of last library block
first_pdeclblk_ptr  DW  0   ; segment of first allocated pubdef declaration block
first_cdeclblk_ptr  DW  0   ; segment of first allocated comdef declaration block
alloc_pdeclblk_ptr  DW  0   ; segment of last allocated pubdef declaration block
alloc_cdeclblk_ptr  DW  0   ; segment of last allocated comdef declaration block
alloc_pdnameblk_ptr DW  0   ; segment of last allocated pubdef names block
alloc_lnamesblk_ptr DW  0   ; segment of last allocated lnames logical names block
first_segdefblk_ptr DW  0   ; segment of first allocated segdef block
alloc_segdefblk_ptr DW  0   ; segment of last allocated segdef block
first_grpblk_ptr    DW  0   ; segment of first allocated group block
alloc_grpblk_ptr    DW  0   ; segment of last allocated group block
first_segpartblk_ptr    DW  0   ; segment of first allocated segment partition block
alloc_segpartblk_ptr    DW  0   ; segment of last allocated segment partition block
first_relblk_ptr    DW  0   ; pointer to first allocated relocation table block
alloc_relblk_ptr    DW  0   ; pointer to last allocated relocation table block
first_libent_ptr    DW  0   ; pointer to first allocated library module entry
alloc_libent_ptr    DW  0   ; pointer to last allocated library module entry
;***first_local_ptr     DW  0   ; pointer to first allocated local declaration
first_fixblk_ptr    DW  0   ; pointer to first allocated fixup block (for DDL)
alloc_fixblk_ptr    DW  0   ; pointer to last allocated fixup block (for DDL)
first_binblk_ptr    DW  0   ; pointer to first allocated binary block (for DDL)
alloc_binblk_ptr    DW  0   ; pointer to last allocated binary block (for DDL)
; used only if DOSSEG switch set
_edata_pubaddr  DW  0       ; address of public declaration entry of _edata
_end_pubaddr    DW  0       ; address of public declaration entry of _end
_edata_segaddr  DW  0       ; address of BSS segment's segdef entry that _edata points to
_end_segaddr    DW  0       ; address of STACK segment's segdef entry that _end points to

; zero init'ed character or byte strings
EVEN
exe_name    DB  128 DUP (0) ; executable file name, including any path
EVEN
map_name    DB  128 DUP (0) ; map file name, including any path

EVEN
; word arrays
pubdecl_hash    DW  HASH_ARRAY_SIZE DUP (0) ; hashed pointers to public declaration entries
segdef_hash     DW  HASH_ARRAY_SIZE DUP (0) ; hashed pointers to segdef entries
zero_table  DW  256 DUP (0) ; table of 512-byte page of zeros

; doubleword arrays
lnames_hash     DD  HASH_ARRAY_SIZE DUP (0) ; hashed pointers to lnames logical name entries

;*****************************
;   Uninitialized data       *
;*****************************

.DATA?

; byte values
EVEN                        ; maximize speed on 8086 and better
ctrlbreak   DB  ?           ; status of Ctrl-Break checking
EVEN
dos_version DB  ?           ; major version number of DOS
EVEN
obj_ovl_flag    DB  ?       ; nonzero if object module is in an overlay

IFNDEF JUNIOR
EVEN
    seg_ovlclass    DB  ?   ; nonzero if at least one segment in module is overlay class
EVEN
    nonovl_rvect    DB  ?   ; nonzero if module is overlaid but overlay->root calls aren't vectored
EVEN
    ovl_ioblk   DW  ?       ; segment of 4K .OVL i/o block
    lookup_tbl_segdef   DW  ?   ; segdef entry of lookup table segment
    ind_tbl_segdef   DW  ?  ; segdef entry of indirect call table segment
    segcall_tbl_segdef  DW  ?   ; segdef entry of segment call segment
    _dt_seg_size    DW  ?   ;  size of current Clarion _DT segment
    _dat_seg_size   DW  ?   ;  size of current Clarion _DAT segment
    _dt_seg_index   DW  ?       ; segment index of clarion _DT segment
    _dat_seg_index  DW  ?       ; segment index of clarion _DAT segment
    ems_base    DW  ?       ; base of EMS page
    ems_pagecount   DW  ?   ; count of WarpLink allocated EMS pages
    ems_page_avail  DW  ?   ; EMS pages available for WarpLink use after i/o buffer
    xms_addr    DD  ?       ; XMS entry point address
ENDIF

EVEN                        ; maximize speed on 8086 and better
; word values
psp DW  ?                   ; machlink's PSP value
image_handle    DW  ?       ; handle of temporary file containing program's memory image
image_mem_ptr   DW  ?       ; segment pointer to program's image in memory
map_handle      DW  ?       ; handle of map file
memory_blk_base DW  ?       ; segment of memory block allocated thru DOS function 48h
memory_blk_size DW  ?       ; size of allocated memory block in paragraphs
memory_blk_end  DW  ?       ; end of memory block (first segment value past end)
allocation_base DW  ?       ; first available segment for block allocations
allocation_top  DW  ?       ; last+1 available segment for block allocations
buffer_base     DW  ?       ; base of buffer for file i/o (segment)
buffer_end      DW  ?       ; end of buffer in memory (offset)
buffer_head     DW  ?       ; address to begin load in buffer (offset)
buffer_tail     DW  ?       ; address to end load in buffer (offset)
buffer_size     DW  ?       ; size of buffer for file i/o in paragraphs
read_buff_ptr   DW  ?       ; file i/o buffer read offset (curr_buff_ptr before update)
prev_read_ptr   DW  ?       ; file i/o buffer read offset for previous object record
prev_rec_len    DW  ?       ; length of previous (L?DATA) record
current_lnames  DW  ?       ; number of current LNAMES record
current_extdef  DW  ?       ; number of current EXTDEF record
current_segdef  DW  ?       ; number of current SEGDEF record
current_grpdef  DW  ?       ; number of current GRPDEF record
eof_pos     DW  ?           ; offset position of end of file in i/o buffer
data_fixup_count    DW  ?   ; count of elements in data_fixup_flag array
master_segblk   DW  ?       ; address of block holding master segdef pointers for overlaid segparts
ovl_filepos_blk DW  ?       ; segment of block holding dword file positions of overlays

; double word values
file_pos_adj    DD  ?       ; file bytes adjustment from wrapped buffer to get true offset of file
prev_pos_adj    DD  ?       ; adjustment value for previous record pointer
lib_pos_adj     DD  ?       ; adjustment for library files, to compute offset from module start
image_size      DD  ?       ; program's executable image size
lib_id          DD  ?       ; library identification number (((current_lib|0x8000)*65536L)+lib_module)

; character or byte strings
EVEN
cmd_line        DB  128 DUP (?) ; command line from PSP
EVEN
prev_libname    LABEL   BYTE    ; name of library holding last processed module, shared with tmod_name
tmod_name       DB  128 DUP (?) ; name from T-module name field of THEADR record
EVEN
ovl_class_name  DB  32 DUP (?)  ; overlay class name

; byte arrays
EVEN
frame_thrd_meth     DB  4 DUP (?)   ; frame fixup thread method
EVEN
target_thrd_meth    DB  4 DUP (?)   ; target fixup thread method

; word arrays
EVEN
seg_partent_indptr  LABEL   WORD    ; seg_partent_indptr and seg_defent_indptr share same memory space
seg_defent_indptr   DW  SEGDEF_MAX DUP (?)  ; indexed segment pointers to segdef entries of last read segdef record
grp_ent_indptr      DW  GRPDEF_MAX DUP (?)  ; indexed segment pointers to grpdef entries of last read grpdef record
IFNDEF JUNIOR
inc_seg_clcode      LABEL   BYTE    ; shared memory space with ext_defent_indptr, used by incremental link
ENDIF
ext_defent_indptr   DW  EXTDEF_MAX DUP (?)  ; indexed segment pointers to pubdef declaration entries of extdefs in object module

IFNDEF JUNIOR
ind_tbl_array       LABEL   WORD    ; share memory space with lookup_tbl_array, used after lookup_tbl_array
lookup_tbl_array    LABEL   WORD    ; share memory space with ovl_reloc_array and ovlpub_array, used between pass 1 and 2
ovl_reloc_array     LABEL   WORD    ; share memory space with ovlpub_array, used in pass 2
ovlpub_array        DW  EXTDEF_MAX+1 DUP (?)    ; index byte of overlaid public declaration entries, use in pass 1
                                                ; bit 15 set if far called, bit 14 if near
ENDIF

env_opt_storage     LABEL   BYTE    ; shared, used prior to pass one
lib_page_storage    LABEL   WORD    ; shared memory space with data_fixup_flag,used in pass 1
ddl_hold_buff       LABEL   WORD    ; used by ddl's to hold pub/com/extdef's prior to file write
data_fixup_flag     DW  1025 DUP (?)    ; positions in data record that flag a segment fixup, used pass 2
frame_thrd_index    DW  4 DUP (?)   ; frame fixup thread index
target_thrd_index   DW  4 DUP (?)   ; target fixup thread index

; doubleword arrays
ddl_symbol_lookup   LABEL   DWORD   ; used by ddl's to translate symbol seg:off to file position
lnames_ent_indptr   DD  LNAMES_MAX DUP (?)  ; indexed segment:offset pointers to lnames entries of last read lnames record
