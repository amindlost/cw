;*********************************************************************
;*   MLEQUATE.INC                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          04/29/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Global file equates                                             *
;*                                                                   *
;*********************************************************************

; modifiable equate, tracks number of internal error types
INTERNAL_ERR_COUNT  EQU 14

IFDEF	DEMO
	LINKER		EQU	"Freelink"
	LINKERCAPS	EQU	"FREELINK"
ELSE
	LINKER	EQU	"WarpLink"
	LINKERCAPS	EQU	"WARPLINK"
ENDIF

; general use equates

STDIN   EQU 0               ; standard input device
STDOUT  EQU 1               ; standard output device
STDERR  EQU 2               ; standard error device

BELL    EQU     7           ; beep the speaker
CR      EQU     13          ; carriage return
LF      EQU     10          ; line feed

; object record names

BLKDEF  EQU     7ah
BLKEND  EQU     7ch
DEBSYM  EQU     7eh

THEADR  EQU     80h
LHEADR  EQU     82h
COMENT  EQU     88h
MODEND  EQU     8ah
EXTDEF  EQU     8ch
TYPDEF  EQU     8eh
PUBDEF  EQU     90h
LOCSYM  EQU     92h
LINNUM  EQU     94h
LNAMES  EQU     96h
SEGDEF  EQU     98h
GRPDEF  EQU     9ah
FIXUPP  EQU     9ch
LEDATA  EQU     0a0h
LIDATA  EQU     0a2h
COMDEF  EQU     0b0h

BAKPAT1 EQU     0b2h        ; Quick C back patch record type 1, not implemented
BAKPAT2 EQU     0b3h        ; Quick C back patch record type 1, not implemented

LEXTDEF EQU     0b4h
LPUBDEF1    EQU 0b6h
LPUBDEF2    EQU 0b7h
LCOMDEF EQU     0b8h
CEXTDEF	EQU	0bch
SEGDEF32	EQU	99h
LEDATA32	EQU	0a1h

WKEXT   EQU     0a8h        ; weak extern, used by PDS 7.x, COMENT class

MSLIBR  EQU     0f0h        ; MS library header record (aka MSLHED)
MSLHED  EQU     0f0h        ; MS library header record (aka MSLIBR)
MSEXTDICT   EQU 0f2h        ; MS extended dictionary identifier byte
MSLEND  EQU     0f1h        ; MS library end record

PASS_TWO_MARK   EQU 0a2h    ; COMENT class, linker pass two marker
DOSSEG_MARK EQU 9eh         ; COMENT class, DOSSEG segment ordering
LIB_SEARCH_1    EQU 81h     ; COMENT class, default library search name type 1
LIB_SEARCH_2    EQU 9fh     ; COMENT class, default library search name type 2

; boundary value equates

GRPDEF_MAX  EQU 21          ; maximum number of GRPDEF records in one object module 
SEGDEF_MAX  EQU 255         ; maximum number of SEGDEF records in one object module
LNAMES_MAX  EQU 511         ; maximum number of logical names (lnames entries)
EXTDEF_MAX  EQU 1023        ; maximum number of external definitions in one object module
                            ; must be 2^n-1
RELOC_MAX   EQU 32768       ; maximum number of relocation items in .exe file
IDATA_BLK_MAX   EQU 512     ; maximum size of iterated data block

IO_BUFF_SIZE    EQU 16384   ; buffer size for all file i/o
REC_BUFF_SIZE   EQU 4096    ; maximum length of object record
OVERLAY_MAX_COUNT   EQU 4095        ; maximum number of overlays and overlaid publics
OVL_MOD_PUB_MAX EQU 2047    ; maximum number of overlaid publics in one overlay

HASH_ARRAY_SIZE EQU 1024    ; size of hash pointer array (10-bit)

LIB_MAX_READ    EQU 4096    ; maximum size of file read from library module

;*****************************
;* Block structure equates   *
;*****************************

; object module name block equates and structure
; make all block sizes a multiple of 16 bytes for allocate memory DOS function 48h
OBJ_NAMELIST_SIZE   EQU (4*128+16-4)    ; allows worst case 4 entries per block
OBJ_NAMBLK_SIZE EQU (OBJ_NAMELIST_SIZE+4)/16    ; size of block in paragraphs
OBJ_NAMBLK_BYSIZE   EQU OBJ_NAMBLK_SIZE*16  ; size of block in bytes

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
OBJ_NAMBLK_STRUC    STRUC
    free    DW  ?           ; free name list space in block
    objblk_ptr  DW  ?       ; segment of next block, zero if none
    namelist    DB  OBJ_NAMELIST_SIZE DUP (?)   ; space for object module names
OBJ_NAMBLK_STRUC    ENDS


; library name block equates and structure
; make all block sizes a multiple of 16 bytes for allocate memory DOS function 48h
LIB_NAMELIST_SIZE   EQU (2*128+16-4)    ; allows worst case 2 entries per block
LIB_NAMBLK_SIZE EQU (LIB_NAMELIST_SIZE+4)/16    ; size of block in paragraphs
LIB_NAMBLK_BYSIZE   EQU LIB_NAMBLK_SIZE*16  ; size of block in bytes

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
LIB_NAMBLK_STRUC STRUC
    lns_free    DW  ?       ; free name list space in block
    lib_namblk_ptr  DW  ?   ; segment of next block, zero if none
    lns_namelist    DB  LIB_NAMELIST_SIZE DUP (?)   ; space for library names
LIB_NAMBLK_STRUC ENDS


; public declaration entry equates and structure
PUB_DECLENT_SIZE    EQU 16  ; size of an entry

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
PUB_DECLENT_STRUC   STRUC
    seg_partent_ptr DW  ?   ; segment pointer to segdef partition entry
                            ; if weak extdef, pointer to default resolution declaration entry
    pdes_frame_number   DW  ?   ; frame number if absolute, pointer to group entry if not and group index
                                ; high word of communal length if communal
                                ; overlaid public number, if overlaid or referenced from overlay
    pub_nameent_ptr DW  ?   ; offset pointer to pubdef name in pubdef names block
                            ; current extdef count if local communal in local block
    pub_nameblk_ptr DW  ?   ; segment pointer to pubdef name in pubdef names block
                            ; length of name string if local communal in local block
    pub_offset      DW  ?   ; public offset
                            ; low word of communal length if communal
                            ;  (changed to communal offset at resolution time)
    pub_declent_loptr   DW  ?   ; segment pointer to entry with pubdef name alphabetically lower
                                ; low word of lib_id in nonlocal communal block if local communal from library
                                ; current_obj in nonlocal communal block if local communal from object module
    pub_declent_hiptr   DW  ?   ; segment pointer to entry with pubdef name alphabetically higher
                                ; high word of lib_id in inlocal communal block if local communal from library
    def_flag        DB  ?   ; ==0 if weak extdef, ==1 if extdef or comdef entry, ==2 if pubdef, ==3 if absolute public
                            ; high bit (80h) set if extdef and either is or could be weak
                            ; bit 6 (40h) set if used in fixup
    gen_flag        DB  ?   ; high bit set (80h) if group index associated with public declaration
                            ; bit 6 (40h) set if communal variable
                            ; bit 5 (20h) set if local communal variable
                            ; bit 4 (10h) set if NEAR communal
                            ; bit 3 (8) set if overlay class
                            ; bit 2 (4) set if local public
                            ; bit 1 (2) set if vector written to lookup table
                            ; bit 0 (1) set if in overlay
PUB_DECLENT_STRUC   ENDS


; public declaration block equates and structure
PUB_DECLENT_COUNT   EQU 128 ; number of entries in pubdef declarations block
PUB_DECLBLK_SIZE    EQU (4+PUB_DECLENT_COUNT*PUB_DECLENT_SIZE+12)/16    ; size of block in paragraphs

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
PUB_DECLBLK_STRUC   STRUC
    entry_count DW  ?       ; count of used entries in block
    pub_declblk_ptr DW  ?   ; segment pointer to next block
    pdbs_filler DB  12 DUP (?)  ; unused, pads block size out to even paragraph
    pdbs_entry  DB  PUB_DECLENT_COUNT*PUB_DECLENT_SIZE DUP (?)  ; space for pubdef declaration entries
PUB_DECLBLK_STRUC   ENDS


; segdef entry equates and structure
SEG_DEFENT_SIZE EQU 32      ; size of an entry

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
SEG_DEFENT_STRUC    STRUC
    sdes_frame_number   DW  ?   ; frame number
    sdes_offset DD  ?       ; offset from start of program
    seg_len DW  ?           ; segment length
    seg_nament_ptr  DW  ?   ; offset pointer to segment name in lnames block
    seg_namblk_ptr  DW  ?   ; segment pointer to segment name in lnames block
    class_nament_ptr    DW  ?   ; offset pointer to class name in lnames block
    class_namblk_ptr    DW  ?   ; segment pointer to class name in lnames block
    sdes_grp_ent_ptr DW  ?   ; pointer to group entry
    sdes_fill2  DW  ?       ; unused, pads entry size to 32 bytes
    seg_defent_nextptr  DW  ?   ; segment pointer to next segment name entry with same hash code
    seg_partent_firstptr    DW  ?   ; segment pointer to first segment partition entry
    seg_partent_lastptr DW  ?   ; segment pointer to last segment partition entry
    sdes_acbp_byte   DB  ?      ; acbp byte
    addr_resolved_flag  DB  ?   ; nonzero if segment was resolved to an address
    sdes_ovl_flag   DB  ?       ; bit 0 set if overlay class
                                ; bit 1 set if length updated due to overlay vectors
                                ; bit 2 set if DDLs and overlaid segment
                                ; bit 5 set if segment has end vectors (calls to overlays)
                                ; bit 6 set if vector values written for segment
    sdes_filler DB  1           ; unused, pads entry size out to 32 bytes
    sdes_end_vect   DW  ?       ; byte count of end vectors added to segment
                                ; only used if end vectors for this segment
SEG_DEFENT_STRUC    ENDS


; segdef block equates and structure
SEG_DEFENT_COUNT    EQU 32  ; number of entries in segdef block
SEG_DEFBLK_SIZE EQU (4+SEG_DEFENT_COUNT*SEG_DEFENT_SIZE+12)/16  ; size of block in paragraphs

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
SEG_DEFBLK_STRUC    STRUC
    sdbs_entry_count    DW  ?   ; count of used entries in block
    seg_defblk_ptr  DW  ?   ; segment pointer to next block
    sdbs_filler DB  12 DUP (?)  ; unused, pads block size out to even paragraph
    sdbs_entry  DB  SEG_DEFENT_COUNT*SEG_DEFENT_SIZE DUP (?)    ; space for segdef entries
SEG_DEFBLK_STRUC    ENDS


; segment partition entry equates and structure
SEG_PARTENT_SIZE    EQU 16  ; size of an entry

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
SEG_PARTENT_STRUC   STRUC
    spes_offset DW  ?           ; offset of start from true segment base
                                ; if overlaid segment, count of segment fixups/relocation items
    seg_partent_nextptr DW  ?   ; segment pointer to next entry
    seg_defent_backptr  DW  ?   ; segment pointer to master segdef entry, the
                                ; owner of linked segment partition entries
                                ; for overlaid segment after 1st pass changed to overlay identifier
    file_mod_id DD  ?       ; id number for object module or library file+module
    record_index    DB  ?   ; record index, used for pass 2 segdef entry lookup
    spes_acbp_byte  DB  ?   ; segment partition acbp byte
    spes_part_len   DW  ?   ; segment partition length (prior to alignment adjustment)
    spes_vect_low   DB  ?   ; low byte of vector count
    spes_ovl_flag   DB  ?   ; high bit set if segment partition is an overlay
                            ; bit 6 set if vector values written for segment
                            ; bit 5 set if end vectors written, calls to overlays
                            ; bit 4 set if length updated from near/shared vectors
                            ; bit 3 set if Clipper object module and incremental link
                            ; bit 0-2 contain high byte of vector count
SEG_PARTENT_STRUC   ENDS


; segment partition block equates and structure
SEG_PARTENT_COUNT   EQU 64  ; number of entries in segment partition block
SEG_PARTBLK_SIZE    EQU (4+SEG_PARTENT_COUNT*SEG_PARTENT_SIZE+12)/16    ; size of block in paragraphs

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
SEG_PARTBLK_STRUC   STRUC
    spbs_entry_count    DW  ?   ; count of used entries in block
    seg_partblk_ptr DW  ?   ; segment pointer to next block
    spbs_filler DB  12 DUP (?)  ; unused, pads block size out to even paragraph
    spbs_entry  DB  SEG_PARTENT_COUNT*SEG_PARTENT_SIZE DUP (?)  ; space for segdef partition entries
SEG_PARTBLK_STRUC   ENDS


; pubdef block equates and structure
; make all block sizes a multiple of 16 bytes for allocate memory DOS function 48h
PUB_NAMELIST_SIZE   EQU (20*128+16-4)   ; allow worst case of 20 entries in pubdef names block
PUB_NAMBLK_SIZE EQU (PUB_NAMELIST_SIZE+4)/16    ; size of block in paragraphs

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
PUB_NAMBLK_STRUC    STRUC
    pns_free    DW  ?       ; free name list space in block
    pdnameblk_ptr   DW  ?   ; segment of next block, zero if none
    pns_namelist    DB  PUB_NAMELIST_SIZE DUP (?)   ; space for pubdef names
PUB_NAMBLK_STRUC    ENDS

; lnames logical name block equates and structure
; make all block sizes a multiple of 16 bytes for allocate memory DOS function 48h
; * IMPORTANT LNAMES NAME LIST (lbs_namelist) NOTE *
;     each lnames block logical name entry consists of a low segment:offset pointer to
;     another entry or NULL, a high segment:offset pointer to another entry or NULL,
;     and a variable length name
LNAMES_LIST_SIZE    EQU (10*(4+4+128)+16-4)   ; allow worst case of 10 entries in lnames logical names block
LNAMES_BLK_SIZE EQU (LNAMES_LIST_SIZE+4)/16 ; size of block in paragraphs

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
LNAMES_BLK_STRUC    STRUC
    lbs_free    DW  ?       ; free name list space in block
    lnamesblk_ptr   DW  ?   ; segment of next block, zero if none
    lbs_namelist    DB  LNAMES_LIST_SIZE DUP (?)    ; space for lnames logical names, including hi/lo pointers
LNAMES_BLK_STRUC    ENDS



; group entry equates and structure
GRP_ENT_SIZE    EQU 16      ; size of an entry

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
GRP_ENT_STRUC   STRUC
    ges_offset  DD  ?           ; offset from start of program, start of group
    ges_grp_nament_ptr  DW  ?   ; offset pointer to group name in lnames block
    ges_grp_namblk_ptr  DW  ?   ; segment pointer to group name in lnames block
    ges_grp_index   DW  ?       ; group index value
    ges_filler  DB  6 DUP (?)   ; unused, pads entry size out to 16 bytes
GRP_ENT_STRUC   ENDS


; group block equates and structure
GRP_ENT_COUNT   EQU 16      ; number of entries in group block
GRP_BLK_SIZE    EQU (4+GRP_ENT_COUNT*GRP_ENT_SIZE+12)/16    ; size of block in paragraphs

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
GRP_BLK_STRUC   STRUC
    gbs_entry_count DW  ?   ; count of used entries in block
    grp_blk_ptr DW  ?       ; segment pointer to next block
    gbs_filler  DB  12 DUP (?)  ; unused, pads block size out to even paragraph
    gbs_entry   DB  GRP_ENT_COUNT*GRP_ENT_SIZE DUP (?)  ; space for group entries
GRP_BLK_STRUC   ENDS

; relocation entry block equates and structure
; compute REL_ITEM_COUNT so that block size is an even paragraph including the 2 system information words
REL_ITEM_COUNT  EQU 16*8-1  ; number of entries in group block
REL_ITEM_SIZE   EQU 4       ; each relocation item is 2 words long
REL_BLK_SIZE    EQU (4+REL_ITEM_COUNT*REL_ITEM_SIZE)/16    ; size of block in paragraphs

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
REL_BLK_STRUC   STRUC
    rbs_entry_count DW  ?   ; count of used entries in block
    rel_blk_ptr DW  ?       ; segment pointer to next block
    rel_items   DB  REL_ITEM_COUNT*REL_ITEM_SIZE DUP (?)    ; space for relocation items
REL_BLK_STRUC   ENDS


; library module entry equates and structure
LIB_MODENT_SIZE   EQU 2     ; size of block in paragraphs

; REFERENCE ONLY, block structure is not used (allocations made dynamically)
LIB_MODENT_STRUC STRUC
    lms_lib_number  DW  ?   ; number of library (current_lib at time of entry creation)
    lms_page_number DW  ?   ; number of page where library module starts
    lms_page_size   DW  ?   ; size of library pages
    lms_mod_len     DW  ?   ; size of library module (60K max if exceeded)
    lms_ems_page1   DW  ?   ; first logical EMS page of library module, if used
    lms_ems_page2   DW  ?   ; second logical EMS page of library module
    lms_ems_page3   DW  ?   ; third logical EMS page of library module
    lms_ems_page4   DW  ?   ; fourth logical EMS page of library module
    lms_ems_buffptr DW  ?   ; start of buffer segment in EMS pages
    lms_filler  DB  12 DUP (?)   ; filler to pad entry out to full two paragraphs
    lms_modent_ptr  DW  ?   ; segment pointer to next block
LIB_MODENT_STRUC ENDS

IFNDEF JUNIOR

; overlaid public declarations block
OVLPUB_BLK_SIZE EQU 40h     ; size of block in paragraphs
OVLPUB_BLK_BYSIZE   EQU 16*OVLPUB_BLK_SIZE  ; size of block in bytes

OVLPUB_BLK_STRUC    STRUC
    obs_blk_ptr     DW  ?   ; pointer to next overlaid public block

; repeated sub-block
    obs_flag1       DB  ?   ; general use flag before all segment partition and public declaration pointers
    obs_segpart_ptr DW  ?   ; pointer to segment partition entry owning following pubdefs
    obs_near_pubcount   DW  ?   ; count of NEAR referenced publics/total overlaid public references
    obs_far_pubcount    DW  ?   ; count of FAR referenced publics
    obs_vect_count      DW  ?   ; running count of vector bytes for nonoverlaid segment

; repeated sub-sub-block
    obs_flag2       DB  ?   ; general use flag
    obs_pubdecl_ptr DW  ?   ; pointer to overlaid public owned by preceding segment partition entry
OVLPUB_BLK_STRUC    ENDS

; data overlay block
DATA_OVLBLK_BYSIZE  EQU 2+126*2+2   ; next block pointer, 126 word entries, word padding
DATA_OVLBLK_SIZE    EQU DATA_OVLBLK_BYSIZE/16

ENDIF
