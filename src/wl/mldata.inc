;*********************************************************************
;*   MLDATA.INC                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/21/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   External global data declarations                               *
;*                                                                   *
;*********************************************************************

; DATA variables
EXTRN   is_args:BYTE
EXTRN   is_comfile:BYTE
EXTRN   is_mapfile:BYTE
EXTRN   is_mapexpand:BYTE
EXTRN   is_casesense:BYTE
EXTRN   is_ondisk:BYTE
EXTRN   is_inmem:BYTE
EXTRN   is_msextlib:BYTE
EXTRN   is_nodeflib:BYTE
EXTRN   is_dosseg:BYTE
EXTRN   is_dgroup:BYTE
EXTRN   is_inlib:BYTE
EXTRN   is_reload:BYTE
EXTRN   is_stackval:BYTE
EXTRN   is_maxparval:BYTE
EXTRN   is_nowarn:BYTE
EXTRN   is_exit0:BYTE
EXTRN   is_beep:BYTE
EXTRN   is_linkinfo:BYTE
EXTRN   is_tempfile:BYTE
EXTRN	is_quick:BYTE
EXTRN	is_sympac:BYTE
EXTRN	is_ohp:BYTE
EXTRN	is_oht:BYTE
EXTRN	is_umb:BYTE
EXTRN	ems3_flag:BYTE
EXTRN   wrap_flag:BYTE
EXTRN   prev_flag:BYTE
EXTRN   eof_flag:BYTE

; linker overlay and language specific related values
IFNDEF JUNIOR
EXTRN   obj_ovl_flag:BYTE
EXTRN   ovl_ox_evar:BYTE
EXTRN   ovl_class:BYTE
EXTRN   exact_ovl_class:BYTE
EXTRN   ovl_pool:DWORD
EXTRN   ovl_mem_alloc:BYTE
EXTRN   ovl_max_load:WORD
EXTRN   ovl_max_load_size:WORD
EXTRN   ovl_stack:WORD
EXTRN   ovl_filename:BYTE
EXTRN   ovl_nopath:BYTE
EXTRN   ovl_count:WORD
EXTRN   ovl_handle:WORD
EXTRN   ovl_ioblk:WORD
EXTRN   seg_ovlclass:BYTE
EXTRN   ovlpub_array:WORD
EXTRN   ovl_reloc_array:WORD
EXTRN   lookup_tbl_array:WORD
EXTRN   ind_tbl_array:WORD
EXTRN   first_ovlpubblk_ptr:WORD
EXTRN   alloc_ovlpubblk_ptr:WORD
EXTRN   ovl_pubcount:WORD
EXTRN   lookup_tbl_segdef:WORD
EXTRN   ind_tbl_segdef:WORD
EXTRN   segcall_tbl_segdef:WORD
EXTRN   largest_ovl:WORD
EXTRN   second_ovl:WORD
EXTRN   ovl_code_id:WORD
EXTRN   ovl_data_id:WORD
EXTRN   data_ovlblk_ptr:WORD
EXTRN   nonovl_rvect:BYTE
EXTRN   is_anyovls:BYTE
EXTRN   is_clarion:BYTE
EXTRN   is_internal:BYTE
EXTRN   inc_padval:BYTE
EXTRN   is_clpinc:BYTE
EXTRN   is_clip5:BYTE
EXTRN   is_smartmem:BYTE
EXTRN   is_xms:BYTE
EXTRN   is_min_pool:BYTE
EXTRN   is_no_ems:BYTE
EXTRN   is_ems_ovlpool:BYTE
EXTRN   inc_seg_clcode:BYTE
EXTRN   tmp_in_emsxms:BYTE
EXTRN   tmp_in_xms:BYTE
EXTRN   _dt_seg_size:WORD
EXTRN   _dat_seg_size:WORD
EXTRN   _dt_seg_index:WORD
EXTRN   _dat_seg_index:WORD
EXTRN   ems_handle:WORD
EXTRN   ems_base:WORD
EXTRN   ems_pagecount:WORD
EXTRN   ems_currmap:WORD
EXTRN   ems_page_avail:WORD
EXTRN   xms_addr:DWORD
EXTRN   is_ddl:BYTE
EXTRN   use_ddl:BYTE
EXTRN   any_ddl:BYTE
EXTRN   ddldat_filename:BYTE
EXTRN   is_xtstash:BYTE
EXTRN   is_xpstash:BYTE
EXTRN   udl_proc_pass:BYTE
EXTRN	ovl_ohp_size:WORD
EXTRN	ovl_oht_size:WORD
EXTRN	ovl_ohp_alloc:BYTE
EXTRN	ovl_oht_alloc:BYTE
ENDIF

EXTRN   warn_count:WORD
EXTRN   obj_count:WORD
EXTRN   lib_count:WORD
EXTRN   module_count:WORD
EXTRN   seg_count:WORD
EXTRN   segdef_count:WORD
EXTRN   current_obj:WORD
EXTRN   current_lib:WORD
EXTRN   number_reloc:WORD
EXTRN   communal_count:WORD
EXTRN   stack_value:WORD
EXTRN   maxpar_value:WORD

EXTRN   pub_sym_count:DWORD
EXTRN   tot_sym_count:DWORD
EXTRN   highest_exe_write:DWORD
                 
EXTRN   first_objblk_ptr:WORD
EXTRN   last_objblk_ptr:WORD
EXTRN   first_libblk_ptr:WORD
EXTRN   last_libblk_ptr:WORD
EXTRN   first_pdeclblk_ptr:WORD
EXTRN   first_cdeclblk_ptr:WORD
EXTRN   alloc_pdeclblk_ptr:WORD
EXTRN   alloc_cdeclblk_ptr:WORD
EXTRN   alloc_pdnameblk_ptr:WORD
EXTRN   alloc_lnamesblk_ptr:WORD
EXTRN   first_segdefblk_ptr:WORD
EXTRN   alloc_segdefblk_ptr:WORD
EXTRN   first_segpartblk_ptr:WORD
EXTRN   alloc_segpartblk_ptr:WORD
EXTRN   first_grpblk_ptr:WORD
EXTRN   alloc_grpblk_ptr:WORD
EXTRN   first_relblk_ptr:WORD
EXTRN   alloc_relblk_ptr:WORD
EXTRN   first_libent_ptr:WORD
EXTRN   alloc_libent_ptr:WORD
;***EXTRN   first_local_ptr:WORD
EXTRN   first_fixblk_ptr:WORD
EXTRN   alloc_fixblk_ptr:WORD
EXTRN   first_binblk_ptr:WORD
EXTRN   alloc_binblk_ptr:WORD
EXTRN   _edata_pubaddr:WORD
EXTRN   _end_pubaddr:WORD
EXTRN   _edata_segaddr:WORD
EXTRN   _end_segaddr:WORD

EXTRN   exe_name:BYTE
EXTRN   map_name:BYTE

EXTRN   pubdecl_hash:WORD
EXTRN   segdef_hash:WORD
EXTRN   zero_table:WORD

EXTRN   lnames_hash:DWORD

; DATA? variables
EXTRN   ctrlbreak:BYTE
EXTRN   dos_version:BYTE

EXTRN   psp:WORD
EXTRN   image_handle:WORD
EXTRN   image_mem_ptr:WORD
EXTRN   map_handle:WORD
EXTRN   memory_blk_base:WORD
EXTRN   memory_blk_size:WORD
EXTRN   memory_blk_end:WORD
EXTRN   allocation_base:WORD
EXTRN   allocation_top:WORD
EXTRN   buffer_base:WORD
EXTRN   buffer_end:WORD
EXTRN   buffer_head:WORD
EXTRN   buffer_tail:WORD
EXTRN   buffer_size:WORD
EXTRN   read_buff_ptr:WORD
EXTRN   prev_read_ptr:WORD
EXTRN   prev_rec_len:WORD
EXTRN   current_lnames:WORD
EXTRN   current_extdef:WORD
EXTRN   current_segdef:WORD
EXTRN   current_grpdef:WORD
EXTRN   eof_pos:WORD
EXTRN   data_fixup_count:WORD
EXTRN   stack_segval:WORD
EXTRN   stack_offval:WORD
EXTRN   entry_segval:WORD
EXTRN   entry_offval:WORD
EXTRN   master_segblk:WORD
EXTRN   ovl_filepos_blk:WORD

EXTRN   image_size:DWORD
EXTRN   file_pos_adj:DWORD
EXTRN   prev_pos_adj:DWORD
EXTRN   lib_pos_adj:DWORD

EXTRN   cmd_line:BYTE
EXTRN   prev_libname:BYTE
EXTRN   tmod_name:BYTE
EXTRN   ovl_class_name:BYTE

EXTRN   frame_thrd_meth:BYTE
EXTRN   target_thrd_meth:BYTE

EXTRN   seg_partent_indptr:WORD
EXTRN   seg_defent_indptr:WORD
EXTRN   grp_ent_indptr:WORD
EXTRN   ext_defent_indptr:WORD
EXTRN   lib_page_storage:WORD
EXTRN   env_opt_storage:WORD
EXTRN   data_fixup_flag:WORD
EXTRN   ddl_hold_buff:WORD
EXTRN   frame_thrd_index:WORD
EXTRN   target_thrd_index:WORD

EXTRN   lnames_ent_indptr:DWORD
EXTRN   ddl_symbol_lookup:DWORD
EXTRN   lib_id:DWORD
