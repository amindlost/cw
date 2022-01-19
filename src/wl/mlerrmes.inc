;*********************************************************************
;*   MLERRMES.INC                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          02/10/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Error message feedback strings with lookup table                *
;*   Data space is only allocated if ALLOCATE_ERROR_DATA is defined  *
;*                                                                   *
;*********************************************************************

PAGE    50,80
     
;*****************************
;* Constant data             *
;*****************************

.CONST

HIGH_ERR_CODE   EQU 74      ; highest error code

;*****************************************************************************

; equates and address table for error/warning feedback
; four word values per entry
; first word flags the type of error using the following table
;   bit     value   type of feedback
;   ---     -----   ----------------
;    0        0     warning [0]
;    0        1     fatal [1]
;   2,1      0,0    unknown error [0]
;   2,1      0,1    internal error [2]
;   2,1      1,0    command line (user) error [4]
;   2,1      1,1    error in file [6]
;   4,3      0,0    no error value given [0]
;   4,3      0,1    8-bit error value [8]
;   4,3      1,0    16-bit error value [10h]
;   4,3      1,1    no offset or error value for file [18h]
;    5        0     non-DOS error
;    5        1     DOS error
;    6        1     Unspecified fatal error
; second word is offset to the problem message
; third word is offset to the cause of the problem, i.e. user, machlink, dos, software
; fourth word is offset to the solution message

; error type equates
WARNING_ERRVAL  EQU 0
FATAL_ERRVAL    EQU 1
UNKNOWN_ERRVAL  EQU 0
INTERNAL_ERRVAL EQU 2
COMMAND_ERRVAL  EQU 4
FILE_ERRVAL     EQU 6
NOVAL_ERRVAL    EQU 0
BIT8_ERRVAL     EQU 8
BIT16_ERRVAL    EQU 16
NO_OFF_ERRVAL   EQU 24
DOS_ERRVAL      EQU 32
NOSPEC_ERRVAL   EQU 64

; error name equates and address table
INTERNAL_ERR        EQU 0           ; machlink internal error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
error_table_start   EQU $           ; for init'ing variables to start of error message table
    DW  FATAL_ERRVAL OR INTERNAL_ERRVAL
    DW  OFFSET  stpro_internal
    DW  OFFSET  stcause_machlink
    DW  OFFSET  stsol_contact
ENDIF

IFNDEF JUNIOR

UNM_LEFT_PAREN_ERR  EQU 1           ; unmatched left parenthesis
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_left_paren
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_bal_paren     ; left and right unmatched parenthesis have same solution
ENDIF

UNM_RIGHT_PAREN_ERR EQU 2           ; unmatched right parenthesis
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_right_paren
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_bal_paren     ; left and right unmatched parenthesis have same solution
ENDIF

NEST_PAREN_ERR  EQU 3               ; unmatched right parenthesis
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_nest_paren
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_nest_paren    ; left and right unmatched parenthesis have same solution
ENDIF

ELSE
    DW  3*4 DUP (0)                 ; null the pointers
ENDIF

UNKNOWN_ERR     EQU 4               ; unknown error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR UNKNOWN_ERRVAL
    DW  OFFSET  stpro_unknown
    DW  OFFSET  stcause_machlink
    DW  OFFSET  stsol_contact
ENDIF

BAD_OPTION_ERR  EQU 5               ; bad linker option
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_bad_option
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_bad_option
ENDIF

BAD_RECTYPE_ERR EQU 6               ; bad record type
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_bad_rectype
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

MULT_THEADR_ERR EQU 7               ; more than one THEADR record
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_mult_theadr
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

GRPDEF_VAL_ERR  EQU 8               ; GRPDEF index value too large
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_grpdef_val
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

SEGDEF_VAL_ERR  EQU 9               ; SEGDEF index value too large
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_segdef_val
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

UNEXPECT_EOF_ERR    EQU 10          ; unexpected end of file error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_unexpect_eof
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_unexpect_eof
ENDIF

INV_DSTYPE_ERR  EQU 11              ; invalid data segment type value
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_inv_dstype
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

INV_COMFIELD_VAL_ERR    EQU 12      ; invalid communal length field value
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_inv_comfval
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

LNAMES_COUNT_ERR    EQU 13          ; too many LNAMES logical names in file
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NOVAL_ERRVAL
    DW  OFFSET  stpro_lnames_count
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

GRPIND_VAL_ERR    EQU 14            ; Invalid GRPDEF group name index value
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT16_ERRVAL
    DW  OFFSET  stpro_grpind_val
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

SEGNAM_IND_ERR    EQU 15            ; Invalid SEGDEF segment name index value
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT16_ERRVAL
    DW  OFFSET  stpro_segnam_ind
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

CLASSNAM_IND_ERR    EQU 16          ; Invalid SEGDEF class name index value
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT16_ERRVAL
    DW  OFFSET  stpro_classnam_ind
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

COMBINE_VAL_ERR EQU 17              ; Invalid SEGDEF combine field value
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_comfld_val
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

SEGMENT_LEN_ERR EQU 18              ; segment size exceeds 64K
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_seglen
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_seglen
ENDIF

NO_OBJMOD_ERR   EQU 19              ; no object modules were specified
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_no_objmod
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_no_objmod
ENDIF

STACK_SIZE_ERR  EQU 20              ; stack segment larger than 65534 bytes
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_stack_size
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_stack_size
ENDIF

COM_STACK_ERR   EQU 21              ; COM file contains stack segment
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_com_stack
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_com_stack
ENDIF

COM_FIXUP_ERR   EQU 22              ; COM file has segment-relative fixups
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_com_fixup
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_com_fixup
ENDIF

COM_SIZE_ERR    EQU 23              ; COM file is greater than 64K
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_com_size
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_com_size
ENDIF

EXTDEF_COUNT_ERR    EQU 24          ; too many external names
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_extdef_count
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_extdef_count
ENDIF

ENDDAT_VAL_ERR  EQU 25              ; bad enddat field value in MODEND record
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_enddat
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

LOC_VAL_ERR EQU 26                  ; bad loc field value in FIXUPP record
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_loc
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

EXTDEF_VAL_ERR  EQU 27              ; EXTDEF index value too large
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT16_ERRVAL
    DW  OFFSET  stpro_extdef_val
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

INV_THRD_METH_ERR   EQU 28          ; invalid thread method field value
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_inv_thrdmeth
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

IMAGE_BOUNDS_ERR    EQU 29          ; attempt to write data outside of program bounds
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_bounds
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

RELOC_COUNT_ERR EQU 30              ; more than 32768 relocation table entries
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_rel_count
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_rel_count
ENDIF

IDATA_BLK_SIZE_ERR  EQU 31          ; LIDATA iterated data block larger than 512 bytes
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_idata
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

FIXUPP_DATA_ERR EQU 32              ; FIXUPP records contains a fixup field without preceding L?DATA record
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_fix_data
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

RSP_LINE_ERR    EQU 33              ; response file line too long
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_rsp_line
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_rsp_line
ENDIF

RSP_NEST_ERR    EQU 34              ; attempt to nest response files
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_rsp_nest
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_rsp_nest
ENDIF

SEG_GRP_CONF_ERR    EQU 35          ; PUBDEF group and segment index values conflict
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_seg_grp
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

DOS_FILE_FND_ERR    EQU 36          ; DOS file not found error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR DOS_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_filefnd
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_filefnd
ENDIF

DOS_PATH_FND_ERR    EQU 37          ; DOS path not found error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR DOS_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_filefnd
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_filefnd
ENDIF

DOS_OUT_HAND_ERR    EQU 38          ; DOS out of file handles error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR DOS_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_outhand
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_outhand
ENDIF

DOS_ACC_DEN_ERR     EQU 39          ; DOS access denied error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR DOS_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_accden
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_accden
ENDIF

DOS_INV_HAND_ERR    EQU 40          ; DOS invalid handle error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR DOS_ERRVAL
    DW  OFFSET  stpro_invhand
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_contact
ENDIF

DOS_MCB_DSTRY_ERR   EQU 41          ; DOS memory control blocks destroyed error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR DOS_ERRVAL
    DW  OFFSET  stpro_mcb
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_contact
ENDIF

DOS_INSUF_MEM_ERR   EQU 42          ; DOS insufficient memory error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR DOS_ERRVAL
    DW  OFFSET  stpro_mem
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_mem
ENDIF

DOS_INV_MEM_BLK_ERR EQU 43          ; DOS invalid memory block error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR DOS_ERRVAL
    DW  OFFSET  stpro_memblk
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_contact
ENDIF

NO_EXE_STACK_WARN   EQU 44          ; warning, no stack for .EXE file
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  WARNING_ERRVAL
    DW  OFFSET  stpro_nostack
    DW  OFFSET  stcause_possible
    DW  OFFSET  stsol_nostack
ENDIF

UNRES_EXT_WARN  EQU 45              ; warning, unresolved external
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  WARNING_ERRVAL
    DW  OFFSET  stpro_unres
    DW  OFFSET  stcause_possible
    DW  OFFSET  stsol_unres
ENDIF

MULT_DEF_SYM_WARN   EQU 46          ; warning, multiply defined symbol
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  WARNING_ERRVAL
    DW  OFFSET  stpro_multsym
    DW  OFFSET  stcause_possible
    DW  OFFSET  stsol_multsym
ENDIF

FIXUP_OVRFLW_WARN   EQU 47          ; warning, fixup overflow
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  WARNING_ERRVAL
    DW  OFFSET  stpro_fixup
    DW  OFFSET  stcause_possible
    DW  OFFSET  stsol_fixup
ENDIF

SEG_GRP_DECL_WARN   EQU 48          ; segment declared in more than one group
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  WARNING_ERRVAL
    DW  OFFSET  stpro_segdecl
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_segdecl
ENDIF

COM_ENTRY_ERR   EQU 49              ; COM file contains bad entry point
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_com_entry
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_com_entry
ENDIF

SELF_REL_FIX_ERR    EQU 50          ; intersegment self-relative fixup
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_selfrel
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

LIB_FORMAT_ERR  EQU 51              ; library format error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_libform
    DW  OFFSET  stcause_library
    DW  OFFSET  stsol_libform
ENDIF

FRAME_THRD_ERR  EQU 52              ; bad frame thread field value in FIXUPP record
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_inv_frthrd
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

FRAME_METH_ERR  EQU 53              ; bad frame method in FIXUPP record
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_inv_frmeth
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

TARGET_METH_ERR EQU 54              ; bad target method in FIXUPP record
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_inv_tarmeth
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

ENDFRAME_THRD_ERR   EQU 55          ; bad frame thread field value in MODEND record
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_inv_endframe
    DW  OFFSET  stcause_module
    DW  OFFSET  stsol_bad_rec
ENDIF

COMM_OVRFLW_ERR EQU 56              ; communal variable length overflow
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_comm_ovr
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_comm_ovr
ENDIF

DEF_SEG_OVRFLW_ERR  EQU 57          ; default data segment overflow (due to communals)
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_defseg_ovr
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_defseg_ovr
ENDIF

DISK_FULL_ERR   EQU 58              ; disk full error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_diskfull
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_diskfull
ENDIF

TMP_DISK_FULL_ERR   EQU 59          ; disk specified by TMP variable is full
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR NO_OFF_ERRVAL
    DW  OFFSET  stpro_diskfull
    DW  OFFSET  stcause_dos
    DW  OFFSET  stsol_tmpfull
ENDIF

IFNDEF JUNIOR

OVLSEG_LEN_ERR  EQU 60              ; overlaid segment size exceeds 64K
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR NOSPEC_ERRVAL
    DW  OFFSET  stpro_ovlseglen
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_ovlseglen
ENDIF

ROOTSEG_LEN_ERR EQU 61              ; overlay root segment size exceeds 64K
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR NOSPEC_ERRVAL
    DW  OFFSET  stpro_rootseglen
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_rootseglen
ENDIF

OVERLAY_COUNT_ERR   EQU 62          ; more than 16383 overlays
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR NOSPEC_ERRVAL
    DW  OFFSET  stpro_ovlmax
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_ovlmax
ENDIF

OVLCOM_CLASH_ERR    EQU 63          ; COM files clash with overlays
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_ovlcom
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_ovlcom
ENDIF

OVLPUB_COUNT_ERR    EQU 64          ; more than 4095 publics of overlay class
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR NOSPEC_ERRVAL
    DW  OFFSET  stpro_ovlpub
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_ovlpub
ENDIF

MISSING_OVLMGR_ERR  EQU 65          ; OVLMGR.OBJ missing
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_missing
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_missing
ENDIF

OVL_MOD_PUB_MAX_ERR EQU 66          ; More than 255 overlaid publics in one overlay
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_ovlmodpub
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_ovlmodpub
ENDIF

NONOVL_SEG_WARN EQU 67              ; warning, nonoverlayable segment reference
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  WARNING_ERRVAL
    DW  OFFSET  stpro_nonovlseg
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_nonovlsegsym
ENDIF

NONOVL_SYM_WARN EQU 68              ; warning, nonoverlayable symbol reference
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  WARNING_ERRVAL
    DW  OFFSET  stpro_nonovlsym
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_nonovlsegsym
ENDIF

NONCONT_OVLDATA_ERR EQU 69          ; noncontiguous overlay class segment binary data in overlay
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL
    DW  OFFSET  stpro_noncont
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_noncont
ENDIF

EMS_EMM_ERR EQU 70                  ; expanded memory manager error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR NOSPEC_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_ems
    DW  OFFSET  stcause_ems
    DW  OFFSET  stsol_ems
ENDIF

XMS_XMM_ERR EQU 71                  ; extended memory manager error
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR NOSPEC_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_xms
    DW  OFFSET  stcause_xms
    DW  OFFSET  stsol_xms
ENDIF

COMCONFLICT_ERR EQU 72              ; /c linker option conflicts with /ddl, /udl
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR COMMAND_ERRVAL
    DW  OFFSET  stpro_comcon
    DW  OFFSET  stcause_user
    DW  OFFSET  stsol_comcon
ENDIF

DDLMOD_COUNT_ERR    EQU 73          ; more than 1024 modules in DDL
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR NOSPEC_ERRVAL
    DW  OFFSET  stpro_ddlmod
    DW  OFFSET  stcause_user   
    DW  OFFSET  stsol_ddlmod
ENDIF

SYMBOLTOKEN_ERR	EQU 74          	; bad Clipper symbol token
IFDEF   ALLOCATE_ERROR_DATA         ; only allocate jumptable space once
    DW  FATAL_ERRVAL OR FILE_ERRVAL OR BIT8_ERRVAL
    DW  OFFSET  stpro_symtok
    DW  OFFSET  stcause_program
    DW  OFFSET  stsol_symtok
ENDIF

ELSE
    DW  15*4 DUP (0)                ; null the pointers
ENDIF

;*****************************************************************************

IFDEF   ALLOCATE_ERROR_DATA         ; only allocate message space once

; the actual feedback messages strings
; string text is preceded by a one byte value containing the length of the text

;********************************
;* message miscellaneous        *
;********************************

string_pro_len      DB  string_pro_stop-string_problem
string_problem      DB  CR,LF,'  Problem:  '
string_pro_stop =   $

string_sol_len      DB  string_sol_stop-string_solution
string_solution     DB  CR,LF,'  Solution: '
string_sol_stop =   $

string_sp_len       DB  string_sp_stop-string_spacer
string_spacer       DB  CR,LF,'            '
string_sp_stop  =   $

;********************************
;* messages for types of errors *
;********************************

sttyp_len0          DB  sttyp_0_stop-sttyp_fatal_internal
sttyp_fatal_internal    DB  CR,LF,'FATAL internal error in ',LINKER
sttyp_0_stop    =   $

sttyp_len1          DB  sttyp_1_stop-sttyp_fatal_comline
sttyp_fatal_comline DB  CR,LF,'FATAL error in ',LINKER,' command line'
sttyp_1_stop    =   $

sttyp_len2          DB  sttyp_2_stop-sttyp_fatal_unknown
sttyp_fatal_unknown DB  CR,LF,'FATAL unknown error'
sttyp_2_stop    =   $

sttyp_len2a         DB  sttyp_2a_stop-sttyp_fatal_error
sttyp_fatal_error   DB  CR,LF,'FATAL error'
sttyp_2a_stop   =   $

sttyp_len3          DB  sttyp_3_stop-sttyp_fatal_objmod
sttyp_fatal_objmod  DB  CR,LF,'FATAL error in '
sttyp_3_stop    =   $

sttyp_len3a         DB  sttyp_3a_stop-sttyp_offset
sttyp_offset        DB  ', offset '
sttyp_3a_stop   =   $

sttyp_len3b         DB  sttyp_3b_stop-sttyp_value
sttyp_value         DB  ', value '
sttyp_3b_stop   =   $

sttyp_len4          DB  sttyp_4_stop-sttyp_fatal_dos
sttyp_fatal_dos     DB  CR,LF,'FATAL DOS error'
sttyp_4_stop    =   $

sttyp_len5          DB  sttyp_5_stop-sttyp_fatal_dosmod
sttyp_fatal_dosmod  DB  CR,LF,'FATAL DOS error in '
sttyp_5_stop    =   $

sttyp_colon         DB  ':  '

sttyp_len6          DB  sttyp_6_stop-sttyp_warning
sttyp_warning       DB  CR,LF,'Warning in '
sttyp_6_stop    =   $

sttyp_len6a         DB  sttyp_6a_stop-sttyp_segment
sttyp_segment       DB  ', segment '
sttyp_6a_stop   =   $

sttyp_len6b         DB  sttyp_6b_stop-sttyp_symbol
sttyp_symbol        DB  ', symbol '
sttyp_6b_stop   =   $

sttyp_len7          DB  sttyp_7_stop-sttyp_module
sttyp_module        DB  ', module '
sttyp_7_stop    =   $

sttyp_len8          DB  sttyp_8_stop-sttyp_option
sttyp_option        DB  ', option /'
sttyp_8_stop    =   $

;********************************
;* messages for what problem is *
;********************************

stpro_len0          DB  stpro_0_stop-stpro_internal
stpro_internal      DB  'Internal error in ',LINKER,'.'
stpro_0_stop    =   $

IFNDEF JUNIOR
stpro_len1          DB  stpro_1_stop-stpro_left_paren
stpro_left_paren    DB  'Unmatched left parenthesis or bracket in link syntax.'
stpro_1_stop    =   $

stpro_len2          DB  stpro_2_stop-stpro_right_paren
stpro_right_paren   DB  'Unmatched right parenthesis or bracket in link syntax.'
stpro_2_stop    =   $

stpro_len3          DB  stpro_3_stop-stpro_nest_paren
stpro_nest_paren    DB  'Nested parentheses or brackets in link syntax.'
stpro_3_stop    =   $
ENDIF

stpro_len4          DB  stpro_4_stop-stpro_unknown
stpro_unknown       DB  'Unknown error.'
stpro_4_stop    =   $

stpro_len5          DB  stpro_5_stop-stpro_bad_option
stpro_bad_option    DB  'Invalid ',LINKER,' option or value. '
stpro_5_stop    =   $

stpro_len6          DB  stpro_6_stop-stpro_bad_rectype
stpro_bad_rectype   DB  'Invalid object record type. '
stpro_6_stop    =   $

stpro_len7          DB  stpro_7_stop-stpro_mult_theadr
;*** stpro_mult_theadr   DB  'More than one THEADR object record.'
stpro_mult_theadr   DB  0
stpro_7_stop    =   $

stpro_len8          DB  stpro_8_stop-stpro_grpdef_val
stpro_grpdef_val    DB  'GRPDEF index value too large.'
stpro_8_stop    =   $

stpro_len9          DB  stpro_9_stop-stpro_segdef_val
stpro_segdef_val    DB  'SEGDEF index value too large.'
stpro_9_stop    =   $

stpro_len10         DB  stpro_10_stop-stpro_unexpect_eof
stpro_unexpect_eof  DB  'Unexpected end of file.'
stpro_10_stop   =   $

stpro_len11         DB  stpro_11_stop-stpro_inv_dstype
stpro_inv_dstype    DB  'Invalid COMDEF data segment type value.'
stpro_11_stop   =   $

stpro_len12         DB  stpro_12_stop-stpro_inv_comfval
stpro_inv_comfval   DB  'Invalid COMDEF communal length field value.'
stpro_12_stop   =   $

stpro_len13         DB  stpro_13_stop-stpro_lnames_count
stpro_lnames_count  DB  'Too many LNAMES logical names in module.'
stpro_13_stop   =   $

stpro_len14         DB  stpro_14_stop-stpro_grpind_val
stpro_grpind_val    DB  'Invalid GRPDEF group name index value.'
stpro_14_stop   =   $

stpro_len15         DB  stpro_15_stop-stpro_segnam_ind
stpro_segnam_ind    DB  'Invalid SEGDEF segment name index value.'
stpro_15_stop   =   $

stpro_len16         DB  stpro_16_stop-stpro_classnam_ind
stpro_classnam_ind  DB  'Invalid SEGDEF class name index value.'
stpro_16_stop   =   $

stpro_len17         DB  stpro_17_stop-stpro_comfld_val
stpro_comfld_val    DB  'Invalid SEGDEF combine field value.'
stpro_17_stop   =   $

stpro_len18         DB  stpro_18_stop-stpro_seglen
stpro_seglen        DB  'Segment size exceeds 64K.'
stpro_18_stop   =   $

stpro_len19         DB  stpro_19_stop-stpro_no_objmod
stpro_no_objmod     DB  'No object modules specified in link syntax.'
stpro_19_stop   =   $

stpro_len20         DB  stpro_20_stop-stpro_stack_size
stpro_stack_size    DB  'Stack segment larger than 65534 bytes.'
stpro_20_stop   =   $

stpro_len21         DB  stpro_21_stop-stpro_com_stack
stpro_com_stack     DB  'Bad COM file format, program has stack segment.'
stpro_21_stop   =   $

stpro_len22         DB  stpro_22_stop-stpro_com_fixup
stpro_com_fixup     DB  'Bad COM file format, program has segment fixups.'
stpro_22_stop   =   $

stpro_len23         DB  stpro_23_stop-stpro_com_size
stpro_com_size      DB  'Bad COM file format, program larger than 64K.'
stpro_23_stop   =   $

stpro_len24         DB  stpro_24_stop-stpro_extdef_count
stpro_extdef_count  DB  'Too many external names in module.'
stpro_24_stop   =   $

stpro_len25         DB  stpro_25_stop-stpro_enddat
stpro_enddat        DB  "Invalid MODEND 'end dat' field value."
stpro_25_stop   =   $

stpro_len26         DB  stpro_26_stop-stpro_loc
stpro_loc           DB  "Invalid FIXUPP loc field value."
stpro_26_stop   =   $

stpro_len27         DB  stpro_27_stop-stpro_extdef_val
stpro_extdef_val    DB  'EXTDEF index value too large.'
stpro_27_stop   =   $

stpro_len28         DB  stpro_28_stop-stpro_inv_thrdmeth
stpro_inv_thrdmeth  DB  'Invalid FIXUPP thread method field value.'
stpro_28_stop   =   $

stpro_len29         DB  stpro_29_stop-stpro_bounds
stpro_bounds        DB  'Attempt to write data outside of program bounds.'
stpro_29_stop   =   $

stpro_len30         DB  stpro_30_stop-stpro_rel_count
stpro_rel_count     DB  'More than 32768 EXE file relocation table items.'
stpro_30_stop   =   $

stpro_len31         DB  stpro_31_stop-stpro_idata
stpro_idata         DB  'LIDATA iterated data block larger than 512 bytes.'
stpro_31_stop   =   $

stpro_len32         DB  stpro_32_stop-stpro_fix_data
stpro_fix_data      DB  'FIXUPP contains fixup field without a data record.'
stpro_32_stop   =   $

stpro_len33         DB  stpro_33_stop-stpro_rsp_line
stpro_rsp_line      DB  'Length of response file line exceeded 127 characters.'
stpro_33_stop   =   $

stpro_len34         DB  stpro_34_stop-stpro_rsp_nest
stpro_rsp_nest      DB  'Attempt to nest response files.'
stpro_34_stop   =   $

stpro_len35         DB  stpro_35_stop-stpro_seg_grp
stpro_seg_grp       DB  'PUBDEF group and segment index values conflict.'
stpro_35_stop   =   $

stpro_len36         DB  stpro_36_stop-stpro_filefnd
stpro_filefnd       DB  'DOS could not find a file needed by ',LINKER,'.'
stpro_36_stop   =   $

; feedback for 37 is same as 36

stpro_len38         DB  stpro_38_stop-stpro_outhand
stpro_outhand       DB  'Too many open files or handles.'
stpro_38_stop   =   $

stpro_len39         DB  stpro_39_stop-stpro_accden
stpro_accden        DB  'File access denied.'
stpro_39_stop   =   $

stpro_len40         DB  stpro_40_stop-stpro_invhand
stpro_invhand       DB  'Invalid file handle.'
stpro_40_stop   =   $

stpro_len41         DB  stpro_41_stop-stpro_mcb
stpro_mcb           DB  'DOS memory control blocks destroyed.'
stpro_41_stop   =   $

stpro_len42         DB  stpro_42_stop-stpro_mem
stpro_mem           DB  'Not enough memory to operate ',LINKER,'.'
stpro_42_stop   =   $

stpro_len43         DB  stpro_43_stop-stpro_memblk
stpro_memblk        DB  'Invalid DOS memory control block address.'
stpro_43_stop   =   $

stpro_len44         DB  stpro_44_stop-stpro_nostack
stpro_nostack       DB  'No stack segment was found for the EXE file.'
stpro_44_stop   =   $

stpro_len45         DB  stpro_45_stop-stpro_unres
stpro_unres         DB  'Externally declared symbol has no public definition.'
stpro_45_stop   =   $

stpro_len46         DB  stpro_46_stop-stpro_multsym
stpro_multsym       DB  'Symbol has more than one public definition.'
stpro_46_stop   =   $

stpro_len47         DB  stpro_47_stop-stpro_fixup
stpro_fixup         DB  'Fixup overflow.'
stpro_47_stop   =   $

stpro_len48         DB  stpro_48_stop-stpro_segdecl
stpro_segdecl       DB  'Segment was declared in more than one group.'
stpro_48_stop   =   $

stpro_len49         DB  stpro_49_stop-stpro_com_entry
stpro_com_entry     DB  'Bad COM file format, program has invalid entry point.'
stpro_49_stop   =   $

stpro_len50         DB  stpro_50_stop-stpro_selfrel
stpro_selfrel       DB  'Intersegment self-relative fixup in FIXUPP record.'
stpro_50_stop   =   $

stpro_len51         DB  stpro_51_stop-stpro_libform
stpro_libform       DB  'Bad library format.'
stpro_51_stop   =   $

stpro_len52         DB  stpro_52_stop-stpro_inv_frthrd
stpro_inv_frthrd    DB  'Invalid FIXUPP frame thread field.'
stpro_52_stop   =   $

stpro_len53         DB  stpro_53_stop-stpro_inv_frmeth
stpro_inv_frmeth    DB  'Invalid FIXUPP frame method field value.'
stpro_53_stop   =   $

stpro_len54         DB  stpro_54_stop-stpro_inv_tarmeth
stpro_inv_tarmeth   DB  'Invalid FIXUPP target method field value.'
stpro_54_stop   =   $

stpro_len55         DB  stpro_55_stop-stpro_inv_endframe
stpro_inv_endframe  DB  'Invalid MODEND frame thread field.'
stpro_55_stop   =   $

stpro_len56         DB  stpro_56_stop-stpro_comm_ovr
stpro_comm_ovr      DB  'COMDEF communal variable length overflow.'
stpro_56_stop   =   $

stpro_len57         DB  stpro_57_stop-stpro_defseg_ovr
stpro_defseg_ovr    DB  'Default data segment exceeds 64K.'
stpro_57_stop   =   $

stpro_len58         DB  stpro_58_stop-stpro_diskfull
stpro_diskfull      DB  'Insufficient disk space.'
stpro_58_stop   =   $

; feedback for 59 is same as 58

IFNDEF JUNIOR

stpro_len60         DB  stpro_60_stop-stpro_ovlseglen
stpro_ovlseglen     DB  'Overlaid segment size exceeds 64K.'
stpro_60_stop   =   $

stpro_len61         DB  stpro_61_stop-stpro_rootseglen
stpro_rootseglen    DB  'Overlay class root segment size exceeds 64K.'
stpro_61_stop   =   $

stpro_len62         DB  stpro_62_stop-stpro_ovlmax
stpro_ovlmax        DB  'More than 4095 overlaid segments.'
stpro_62_stop   =   $

stpro_len63         DB  stpro_63_stop-stpro_ovlcom
stpro_ovlcom        DB  '/c option specified with overlays.'
stpro_63_stop   =   $

stpro_len64         DB  stpro_64_stop-stpro_ovlpub
stpro_ovlpub        DB  'Count of overlaid publics exceeds 4095.'
stpro_64_stop   =   $

stpro_len65         DB  stpro_65_stop-stpro_missing
stpro_missing       DB  'Overlay manager module OVLMGR.OBJ missing or incorrect version.'
stpro_65_stop   =   $

stpro_len66         DB  stpro_66_stop-stpro_ovlmodpub
stpro_ovlmodpub     DB  'Count of overlaid publics in one overlay exceeds 255.'
stpro_66_stop   =   $

stpro_len67         DB  stpro_67_stop-stpro_nonovlseg
stpro_nonovlseg     DB  'Nonoverlayable segment reference, nonzero displacement.'
stpro_67_stop   =   $

stpro_len68         DB  stpro_68_stop-stpro_nonovlsym
stpro_nonovlsym     DB  'Nonoverlayable symbol reference, nonzero displacement.'
stpro_68_stop   =   $

stpro_len69         DB  stpro_69_stop-stpro_noncont
stpro_noncont       DB  'Overlaid module has noncontiguous overlay class code.'
stpro_69_stop   =   $

stpro_len70         DB  stpro_70_stop-stpro_ems
stpro_ems           DB  'Error when accessing expanded memory.'
stpro_70_stop   =   $

stpro_len71         DB  stpro_71_stop-stpro_xms
stpro_xms           DB  'Error when accessing extended memory.'
stpro_71_stop   =   $

stpro_len72         DB  stpro_72_stop-stpro_comcon
stpro_comcon        DB  '/c option conflicts with /ddl or /udl option.'
stpro_72_stop   =   $

stpro_len73         DB  stpro_73_stop-stpro_ddlmod
stpro_ddlmod        DB  'Too many modules in DDL file.'
stpro_73_stop   =   $

stpro_len74         DB  stpro_74_stop-stpro_symtok
stpro_symtok        DB  'Bad symbol table token in Clipper file.'
stpro_74_stop   =   $

ENDIF

;*******************************
;* messages for cause of error *
;*******************************

stcause_len0        DB  stcause_0_stop-stcause_machlink
stcause_machlink    DB  'This is an error in ',LINKER,'.'
stcause_0_stop  =   $

stcause_len1        DB  stcause_1_stop-stcause_user
stcause_user        DB  'This is a user error.'
stcause_1_stop  =   $

stcause_len2        DB  stcause_2_stop-stcause_module
stcause_module      DB  'This is an error in the module.'
stcause_2_stop  =   $

stcause_len3        DB  stcause_3_stop-stcause_program
stcause_program     DB  'This is an error in your program.'
stcause_3_stop  =   $

stcause_len4        DB  stcause_4_stop-stcause_dos
stcause_dos         DB  'This is an error returned by the operating system (DOS).'
stcause_4_stop  =   $

stcause_len5        DB  stcause_5_stop-stcause_possible
stcause_possible    DB  'This is possibly an error in your program.'
stcause_5_stop  =   $

stcause_len6        DB  stcause_6_stop-stcause_library
stcause_library     DB  'This is an error in the library.'
stcause_6_stop  =   $

stcause_len7        DB  stcause_7_stop-stcause_ems
stcause_ems         DB  'This is an error returned by the EMS expanded memory manager.'
stcause_7_stop  =   $

stcause_len8        DB  stcause_8_stop-stcause_xms
stcause_xms         DB  'This is an error returned by the XMS extended memory manager.'
stcause_8_stop  =   $

;***********************************
;* messages for possible solutions *
;***********************************

; first byte before message length first line is number of lines in solution

stsol_len0a         DB  2,stsol_0a_stop-stsol_contact
stsol_contact       DB  'Try to link using ',LINKER,' again.  If the error recurs,'
stsol_0a_stop   =   $
stsol_len0b         DB  stsol_0b_stop-stsol_contact1
stsol_contact1      DB  'carefully note the conditions and contact tech support.'
stsol_0b_stop   =   $

IFNDEF JUNIOR
stsol_len1a         DB  2,stsol_1a_stop-stsol_bal_paren
stsol_bal_paren     DB  'Correct the command line syntax to properly match the'
stsol_1a_stop   =   $
stsol_len1b         DB  stsol_1b_stop-stsol_bal_paren1
stsol_bal_paren1    DB  'parenthesis or bracket for overlays.'
stsol_1b_stop   =   $

stsol_len3a         DB  2,stsol_3a_stop-stsol_nest_paren
stsol_nest_paren    DB  'Remove or match the second left parenthesis or bracket'
stsol_3a_stop   =   $
stsol_len3b         DB  stsol_3b_stop-stsol_nest_paren2
stsol_nest_paren2   DB  'in a row that comes before a right parenthesis or bracket.'
stsol_3b_stop   =   $
ENDIF

stsol_len5          DB  1,stsol_5_stop-stsol_bad_option
stsol_bad_option    DB  'Remove or correct the ',LINKER,' option in the link syntax.'
stsol_5_stop    =   $

stsol_len6a         DB  3,stsol_6a_stop-stsol_bad_rec
stsol_bad_rec       DB  'Recreate the module by recompiling or reassembling.'
stsol_6a_stop   =   $
stsol_len6b         DB  stsol_6b_stop-stsol_bad_rec2
stsol_bad_rec2      DB  'If the error recurs, report it to the company that wrote'
stsol_6b_stop   =   $
stsol_len6c         DB  stsol_6c_stop-stsol_bad_rec3
stsol_bad_rec3      DB  'the software or supplied the module.'
stsol_6c_stop   =   $

stsol_len7a         DB  3,stsol_7a_stop-stsol_unexpect_eof
stsol_unexpect_eof  DB  'The file ended before the end of a record was reached.'
stsol_7a_stop   =   $
stsol_len7b         DB  stsol_7b_stop-stsol_unexpect_eof2
stsol_unexpect_eof2     DB  'This is usually due to file truncation caused by DOS error or'
stsol_7b_stop   =   $
stsol_len7c         DB  stsol_7c_stop-stsol_unexpect_eof3
stsol_unexpect_eof3     DB  'system reset.  Recreate the module and relink.'
stsol_7c_stop   =   $

stsol_len8a         DB  3,stsol_8a_stop-stsol_seglen
stsol_seglen        DB  'A segment contained more than 65,535 bytes of code or data.'
stsol_8a_stop   =   $
stsol_len8b         DB  stsol_8b_stop-stsol_seglen2
stsol_seglen2       DB  'Break the segment into smaller segments or otherwise reduce'
stsol_8b_stop   =   $
stsol_len8c         DB  stsol_8c_stop-stsol_seglen3
stsol_seglen3       DB  'the size of the segment, possibly by changing memory models.'
stsol_8c_stop   =   $

stsol_len9          DB  1,stsol_9_stop-stsol_no_objmod
stsol_no_objmod     DB  'Supply the name of at least one object module in the link syntax.'
stsol_9_stop    =   $

stsol_len10a        DB  2,stsol_10a_stop-stsol_stack_size
stsol_stack_size    DB  'The stack segment cannot be larger than 65,534 bytes.'
stsol_10a_stop  =   $
stsol_len10b        DB  stsol_10b_stop-stsol_stack_size2
stsol_stack_size2   DB  'Reduce the amount of memory allocated to the stack.'
stsol_10b_stop  =   $

stsol_len11a        DB  2,stsol_11a_stop-stsol_com_stack
stsol_com_stack     DB  'COM files cannot have a stack segment.'
stsol_11a_stop  =   $
stsol_len11b        DB  stsol_11b_stop-stsol_com_stack2
stsol_com_stack2    DB  'Remove the stack segment or use EXE file format.'
stsol_11b_stop  =   $

stsol_len12a        DB  2,stsol_12a_stop-stsol_com_fixup
stsol_com_fixup     DB  'COM files cannot have segment-relative fixups.'
stsol_12a_stop  =   $
stsol_len12b        DB  stsol_12b_stop-stsol_com_fixup2
stsol_com_fixup2    DB  'Use the EXE file format for your program.'
stsol_12b_stop  =   $

stsol_len13a        DB  2,stsol_13a_stop-stsol_com_size
stsol_com_size      DB  'COM files cannot be larger than 65536 bytes.'
stsol_13a_stop  =   $
stsol_len13b        DB  stsol_13b_stop-stsol_com_size2
stsol_com_size2     DB  'Use the EXE file format for your program.'
stsol_13b_stop  =   $

stsol_len14a        DB  2,stsol_14a_stop-stsol_extdef_count
stsol_extdef_count  DB  'Modules cannot contain more that 1023 external names.'
stsol_14a_stop  =   $
stsol_len14b        DB  stsol_14b_stop-stsol_extdef_count
stsol_extdef_count2 DB  'Remove some of the external references in the module.'
stsol_14b_stop  =   $

stsol_len15a        DB  3,stsol_15a_stop-stsol_rel_count
stsol_rel_count     DB  'Your program cannot contain a total of more than 32768'
stsol_15a_stop  =   $
stsol_len15b        DB  stsol_15b_stop-stsol_rel_count2
stsol_rel_count2    DB  'far calls, far jumps, or other far pointers. Remove or'
stsol_15b_stop  =   $
stsol_len15c        DB  stsol_15c_stop-stsol_rel_count3
stsol_rel_count3    DB  'replace some far references with near references.'
stsol_15c_stop  =   $

stsol_len16a        DB  2,stsol_16a_stop-stsol_rsp_line
stsol_rsp_line      DB  'Break the line in the response file into two lines.'
stsol_16a_stop  =   $
stsol_len16b        DB  stsol_16b_stop-stsol_rsp_line2
stsol_rsp_line2     DB  "Place a '+' at the end of the first line to continue it."
stsol_16b_stop  =   $

stsol_len17a        DB  2,stsol_17a_stop-stsol_rsp_nest
stsol_rsp_nest      DB  'A response file cannot be called from within another'
stsol_17a_stop  =   $
stsol_len17b        DB  stsol_17b_stop-stsol_rsp_nest2
stsol_rsp_nest2     DB  'response file.  Remove the nested response file.'
stsol_17b_stop  =   $

stsol_len18a        DB  2,stsol_18a_stop-stsol_filefnd
stsol_filefnd       DB  'This file could not be found.  Check that you have'
stsol_18a_stop  =   $
stsol_len18b        DB  stsol_18b_stop-stsol_filefnd2
stsol_filefnd2      DB  'typed the name and path of the file correctly.'
stsol_18b_stop  =   $

stsol_len19a        DB  3,stsol_19a_stop-stsol_outhand
stsol_outhand       DB  'Too many files are open at once.  Change the FILES='
stsol_19a_stop  =   $
stsol_len19b        DB  stsol_19b_stop-stsol_outhand2
stsol_outhand2      DB  'statement in your CONFIG.SYS file to allow more files'
stsol_19b_stop  =   $
stsol_len19c        DB  stsol_19c_stop-stsol_outhand3
stsol_outhand3      DB  'to be open at one time.'
stsol_19c_stop  =   $

stsol_len20a        DB  3,stsol_20a_stop-stsol_accden
stsol_accden        DB  'Access to this file was denied by DOS.  Change the'
stsol_20a_stop  =   $
stsol_len20b        DB  stsol_20b_stop-stsol_accden2
stsol_accden2       DB  'file attributes to allow ',LINKER,' to properly use'
stsol_20b_stop  =   $
stsol_len20c        DB  stsol_20c_stop-stsol_accden3
stsol_accden3       DB  'the file.'
stsol_20c_stop  =   $

stsol_len21a        DB  3,stsol_21a_stop-stsol_mem
stsol_mem           DB  'There is no more memory available for ',LINKER,' to use.'
stsol_21a_stop  =   $
stsol_len21b        DB  stsol_21b_stop-stsol_mem2
stsol_mem2          DB  'Reduce the memory in use by removing memory resident'
stsol_21b_stop  =   $
stsol_len21c        DB  stsol_21c_stop-stsol_mem3
stsol_mem3          DB  'programs or device drivers.'
stsol_21c_stop  =   $

stsol_len22a        DB  3,stsol_22a_stop-stsol_nostack
stsol_nostack       DB  'An EXE file, unlike a COM file, must internally setup'
stsol_22a_stop  =   $
stsol_len22b        DB  stsol_22b_stop-stsol_nostack2
stsol_nostack2      DB  'its own stack if it has no stack segment.  To create a'
stsol_22b_stop  =   $
stsol_len22c        DB  stsol_22c_stop-stsol_nostack3
stsol_nostack3      DB  'COM file, use the /c option of ',LINKER,'.'
stsol_22c_stop  =   $

stsol_len23a        DB  3,stsol_23a_stop-stsol_unres
stsol_unres         DB  'A symbol was declared as external in one or more'
stsol_23a_stop  =   $
stsol_len23b        DB  stsol_23b_stop-stsol_unres2
stsol_unres2        DB  'modules, but not publicly defined in any other module.'
stsol_23b_stop  =   $
stsol_len23c        DB  stsol_23c_stop-stsol_unres3
stsol_unres3        DB  'Remove or publicly define the symbol.'
stsol_23c_stop  =   $

stsol_len24a        DB  2,stsol_24a_stop-stsol_multsym
stsol_multsym       DB  'A symbol should not have more than one definition.'
stsol_24a_stop  =   $
stsol_len24b        DB  stsol_24b_stop-stsol_multsym2
stsol_multsym2      DB  'Remove the extra definition from your program.'
stsol_24b_stop  =   $

stsol_len25a        DB  5,stsol_25a_stop-stsol_fixup
stsol_fixup         DB  'A group was larger than 64K or the program contains a'
stsol_25a_stop  =   $
stsol_len25b        DB  stsol_25b_stop-stsol_fixup2
stsol_fixup2        DB  'near or short jump or call between different segments'
stsol_25b_stop  =   $
stsol_len25c        DB  stsol_25c_stop-stsol_fixup3
stsol_fixup3        DB  'or the name of a routine and a data item are the same.'
stsol_25c_stop  =   $
stsol_len25d        DB  stsol_25d_stop-stsol_fixup4
stsol_fixup4        DB  'Adjust the group size, or remove the intersegment near'
stsol_25d_stop  =   $
stsol_len25e        DB  stsol_25e_stop-stsol_fixup5
stsol_fixup5        DB  'jump or call, or change the data item.'
stsol_25e_stop  =   $

stsol_len26a        DB  2,stsol_26a_stop-stsol_segdecl
stsol_segdecl       DB  'A segment can only be a member of one group.  Remove its'
stsol_26a_stop  =   $
stsol_len26b        DB  stsol_26b_stop-stsol_segdecl2
stsol_segdecl2      DB  'declaration(s) in the other group(s).'
stsol_26b_stop  =   $

stsol_len27a        DB  3,stsol_27a_stop-stsol_com_entry
stsol_com_entry     DB  'COM files must have a starting address of 100h.'
stsol_27a_stop  =   $
stsol_len27b        DB  stsol_27b_stop-stsol_com_entry2
stsol_com_entry2    DB  'Change the starting address to 100h or use the'
stsol_27b_stop  =   $
stsol_len27c        DB  stsol_27c_stop-stsol_com_entry3
stsol_com_entry3    DB  'EXE file format for your program.'
stsol_27c_stop  =   $

stsol_len28a        DB  1,stsol_28a_stop-stsol_libform
stsol_libform       DB  'Use only standard Microsoft-compatible libraries.'
stsol_28a_stop  =   $

stsol_len29a        DB  3,stsol_29a_stop-stsol_comm_ovr
stsol_comm_ovr      DB  'A communal variable was too large.  Either reduce the'
stsol_29a_stop  =   $
stsol_len29b        DB  stsol_29b_stop-stsol_comm_ovr2
stsol_comm_ovr2     DB  'size of the variable, or if a near communal variable,'
stsol_29b_stop  =   $
stsol_len29c        DB  stsol_29c_stop-stsol_comm_ovr3
stsol_comm_ovr3     DB  'change it to a far communal variable.'
stsol_29c_stop  =   $

stsol_len30a        DB  3,stsol_30a_stop-stsol_defseg_ovr
stsol_defseg_ovr    DB  'The total size of near communal variables caused the'
stsol_30a_stop  =   $
stsol_len30b        DB  stsol_30b_stop-stsol_defseg_ovr2
stsol_defseg_ovr2   DB  'default data segment to exceed 64K.  Remove or change'
stsol_30b_stop  =   $
stsol_len30c        DB  stsol_30c_stop-stsol_defseg_ovr3
stsol_defseg_ovr3   DB  'some of the near communal variables to far variables.'
stsol_30c_stop  =   $

stsol_len31a        DB  3,stsol_31a_stop-stsol_diskfull
stsol_diskfull      DB  'There is not enough room on the disk to write the'
stsol_31a_stop  =   $
stsol_len31b        DB  stsol_31b_stop-stsol_diskfull2
stsol_diskfull2     DB  'executable file.  Either free up disk space by deleting'
stsol_31b_stop  =   $
stsol_len31c        DB  stsol_31c_stop-stsol_diskfull3
stsol_diskfull3     DB  'unnecessary files or use a different disk.'
stsol_31c_stop  =   $

stsol_len32a        DB  3,stsol_32a_stop-stsol_tmpfull
stsol_tmpfull       DB  'There is not enough room for the temporary file on the'
stsol_32a_stop  =   $
stsol_len32b        DB  stsol_32b_stop-stsol_tmpfull2
stsol_tmpfull2      DB  'disk specified by the TMP environment variable.  Free'
stsol_32b_stop  =   $
stsol_len32c        DB  stsol_32c_stop-stsol_tmpfull3
stsol_tmpfull3      DB  'up space on the disk or specify a different disk.'
stsol_32c_stop  =   $

IFNDEF JUNIOR

stsol_len33a        DB  4,stsol_33a_stop-stsol_ovlseglen
stsol_ovlseglen     DB  'An overlaid segment has increased in size beyond 64K'
stsol_33a_stop  =   $
stsol_len33b        DB  stsol_33b_stop-stsol_ovlseglen2
stsol_ovlseglen2    DB  'due to overlay manager allocations.  Break the overlaid'
stsol_33b_stop  =   $
stsol_len33c        DB  stsol_33c_stop-stsol_ovlseglen3
stsol_ovlseglen3    DB  'module into smaller modules or move some code into'
stsol_33c_stop  =   $
stsol_len33d        DB  stsol_33d_stop-stsol_ovlseglen4
stsol_ovlseglen4    DB  'another segment.'
stsol_33d_stop  =   $

stsol_len34a        DB  3,stsol_34a_stop-stsol_rootseglen
stsol_rootseglen    DB  'A root segment of overlay class has increased in size'
stsol_34a_stop  =   $
stsol_len34b        DB  stsol_34b_stop-stsol_rootseglen2
stsol_rootseglen2   DB  'beyond 64K due to overlay manager allocations.  Reduce'
stsol_34b_stop  =   $
stsol_len34c        DB  stsol_34c_stop-stsol_rootseglen3
stsol_rootseglen3   DB  'the segment size by placing some code in another segment.'
stsol_34c_stop  =   $

stsol_len35a        DB  3,stsol_35a_stop-stsol_ovlmax
stsol_ovlmax        DB  'There are too many overlaid segments in the program.'
stsol_35a_stop  =   $
stsol_len35b        DB  stsol_35b_stop-stsol_ovlmax2
stsol_ovlmax2       DB  'Combine some segments or remove one or more libraries'
stsol_35b_stop  =   $
stsol_len35c        DB  stsol_35c_stop-stsol_ovlmax3
stsol_ovlmax3       DB  'or object modules from overlays.'
stsol_35c_stop  =   $

stsol_len36a        DB  3,stsol_36a_stop-stsol_ovlcom
stsol_ovlcom        DB  'You cannot use overlays with COM files.  Either remove'
stsol_36a_stop  =   $
stsol_len36b        DB  stsol_36b_stop-stsol_ovlcom2
stsol_ovlcom2       DB  'the /c option and use an EXE file, or remove the'
stsol_36b_stop  =   $
stsol_len36c        DB  stsol_36c_stop-stsol_ovlcom3
stsol_ovlcom3       DB  'parentheses that specify overlay use.'
stsol_36c_stop  =   $

stsol_len37a        DB  2,stsol_37a_stop-stsol_ovlpub
stsol_ovlpub        DB  'There are too many global labels or symbols in overlays.'
stsol_37a_stop  =   $
stsol_len37b        DB  stsol_37b_stop-stsol_ovlpub2
stsol_ovlpub2       DB  'Decrease the number of global overlaid symbols.'
stsol_37b_stop  =   $

stsol_len38a        DB  3,stsol_38a_stop-stsol_missing
stsol_missing       DB  'When overlays are specified in the link syntax, the'
stsol_38a_stop  =   $
stsol_len38b        DB  stsol_38b_stop-stsol_missing2
stsol_missing2      DB  'proper version of the overlay manager module must be'
stsol_38b_stop  =   $
stsol_len38c        DB  stsol_38c_stop-stsol_missing3
stsol_missing3      DB  'included in the object file list.'
stsol_38c_stop  =   $

stsol_len39a        DB  3,stsol_39a_stop-stsol_ovlmodpub
stsol_ovlmodpub     DB  'There are too many public symbols of overlay class'
stsol_39a_stop  =   $
stsol_len39b        DB  stsol_39b_stop-stsol_ovlmodpub2
stsol_ovlmodpub2    DB  'used in an overlaid segment.  Decrease the count'
stsol_39b_stop  =   $
stsol_len39c        DB  stsol_39c_stop-stsol_ovlmodpub3
stsol_ovlmodpub3    DB  'of public symbols or split up the module.'
stsol_39c_stop  =   $

stsol_len40a        DB  5,stsol_40a_stop-stsol_nonovlsegsym
stsol_nonovlsegsym  DB  'A segment or symbol that was either overlaid or accessed'
stsol_40a_stop  =   $
stsol_len40b        DB  stsol_40b_stop-stsol_nonovlsegsym2
stsol_nonovlsegsym2 DB  'from an overlay was referenced in a manner incompatible'
stsol_40b_stop  =   $
stsol_len40c        DB  stsol_40c_stop-stsol_nonovlsegsym3
stsol_nonovlsegsym3 DB  'with the overlay manager.  To ensure proper program'
stsol_40c_stop  =   $
stsol_len40d        DB  stsol_40d_stop-stsol_nonovlsegsym4
stsol_nonovlsegsym4 DB  'operation, remove the segment or symbol from an overlay'
stsol_40d_stop  =   $
stsol_len40e        DB  stsol_40e_stop-stsol_nonovlsegsym5
stsol_nonovlsegsym5 DB  'or change the reference to it.'
stsol_40e_stop  =   $

stsol_len41a        DB  6,stsol_41a_stop-stsol_noncont
stsol_noncont       DB  'An overlaid module alternated code in an overlay class'
stsol_41a_stop  =   $
stsol_len41b        DB  stsol_41b_stop-stsol_noncont2
stsol_noncont2      DB  'segment with code or data from a different segment.  All'
stsol_41b_stop  =   $
stsol_len41c        DB  stsol_41c_stop-stsol_noncont3
stsol_noncont3      DB  'overlay class segment code within a module must be'
stsol_41c_stop  =   $
stsol_len41d        DB  stsol_41d_stop-stsol_noncont4
stsol_noncont4      DB  'declared in one place.  It cannot be split between any'
stsol_41d_stop  =   $
stsol_len41e        DB  stsol_41e_stop-stsol_noncont5
stsol_noncont5      DB  'other segments.  Remove the module from an overlay or'
stsol_41e_stop  =   $
stsol_len41f        DB  stsol_41f_stop-stsol_noncont6
stsol_noncont6      DB  'declare the code class segment in only one place.'
stsol_41f_stop  =   $

stsol_len42a        DB  4,stsol_42a_stop-stsol_ems
stsol_ems           DB  'An error occurred when ',LINKER,' accessed expanded memory'
stsol_42a_stop  =   $
stsol_len42b        DB  stsol_42b_stop-stsol_ems2
stsol_ems2          DB  '(EMS).  Check that your expanded memory manager supports'
stsol_42b_stop  =   $
stsol_len42c        DB  stsol_42c_stop-stsol_ems3
stsol_ems3          DB  'EMS 3.2 or greater and is working correctly, or turn off use'
stsol_42c_stop  =   $
stsol_len42d        DB  stsol_42d_stop-stsol_ems4
stsol_ems4          DB  'of EMS when linking by removing the /xp option.'
stsol_42d_stop  =   $

stsol_len43a        DB  4,stsol_43a_stop-stsol_xms
stsol_xms           DB  'An error occurred when ',LINKER,' accessed extended memory'
stsol_43a_stop  =   $
stsol_len43b        DB  stsol_43b_stop-stsol_xms2
stsol_xms2          DB  '(XMS).  Check that your extended memory manager is working'
stsol_43b_stop  =   $
stsol_len43c        DB  stsol_43c_stop-stsol_xms3
stsol_xms3          DB  'correctly or turn off use of XMS when linking by removing'
stsol_43c_stop  =   $
stsol_len43d        DB  stsol_43d_stop-stsol_xms4
stsol_xms4          DB  'the /xt option.'
stsol_43d_stop  =   $

stsol_len44a        DB  3,stsol_44a_stop-stsol_comcon
stsol_comcon        DB  'You cannot specify the /c (create COM file) option when'
stsol_44a_stop  =   $
stsol_len44b        DB  stsol_44b_stop-stsol_comcon2
stsol_comcon2       DB  'using the /ddl or /udl (dynamic library) options.  Remove'
stsol_44b_stop  =   $
stsol_len44c        DB  stsol_44c_stop-stsol_comcon3
stsol_comcon3       DB  'the /c option or do not use dynamic libraries.'
stsol_44c_stop  =   $

stsol_len45a        DB  2,stsol_45a_stop-stsol_ddlmod
stsol_ddlmod        DB  'The number of modules in the DDL file cannot exceed 1024.'
stsol_45a_stop  =   $
stsol_len45b        DB  stsol_45b_stop-stsol_ddlmod2
stsol_ddlmod2       DB  'Remove some of the modules specified for the DDL file.'
stsol_45b_stop  =   $

stsol_len46a        DB  5,stsol_46a_stop-stsol_symtok
stsol_symtok        DB  'During symbol table compression a bad symbol token was'
stsol_46a_stop  =   $
stsol_len46b        DB  stsol_46b_stop-stsol_symtok2
stsol_symtok2       DB  'encountered.  Check that you are not mixing different'
stsol_46b_stop  =   $
stsol_len46c        DB  stsol_46c_stop-stsol_symtok3
stsol_symtok3       DB  'versions of Clipper modules or do not have a corrupted'
stsol_46c_stop  =   $
stsol_len46d        DB  stsol_46d_stop-stsol_symtok4
stsol_symtok4       DB  'module.  If not, and the program works without the /sp'
stsol_46d_stop  =   $
stsol_len46e        DB  stsol_46e_stop-stsol_symtok5
stsol_symtok5       DB  'option, note the conditions and contact tech support.'
stsol_46e_stop  =   $

ENDIF

ENDIF
