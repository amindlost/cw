;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                 ;
; SYMPACWL   WP50.ASM  WarpLink symbol packing,  for Hyperkinetix ;
;                                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The following routines need to be modified for Clipper S87:
; get_weedbed (modified)
; copy_old_symbols (modified)
; copy_symbol  --> copy_symbol87
; brute --> brute87
; pcode_waltz -->pcode87_waltz
; copy_new_symbol --> copy_new_symbol87
; get_symbol_type --> get_symbol_type87
;
;  NEEDS these modules of Clipper.lib  (5.01) in the root:
;    CRT0         MAIN        INITEXIT        SYMSYS          WEEDBED
;    _astart      _main       __sysInit       __sym_init      __weed_eatr
;
;  Modifed to also handle S87 transparently. Determine during Weedbed search?
;                                   need CRT0, MAIN, SYMSYS, SEGJUNK in root.

title wp.asm
page 82,132
subttl

comment*

Symbol-table compression utility for Hyperkinetix

08/28/90 12:34pm modified to use simplified-segmentation and [bp] indexing

08/30/90 02:58pm modified to seek weedbed/weedend via dirct reads

see remainder of comments in zip-file headers

09/06/90 06:27pm appears to be working for non-overlayed files.  WP015.ZIP
09/07/90 06:32pm needed symbol-chunk fixup for plankheaders moved to
                        pcode_waltz() from brute(), and also added
                        0x9e to the list of tokens that reference symbols
                        WP016.ZIP

09/20/90 10:21pm WP023.ZIP  bugfix of first beta.  needed to have inheader.reloc
                        item_count decremented as well as the count in headbuffer
                        during kill_fixups.

09/26/90 10:57pm  tbl.equ modified (plus pcode_waltz) to put bogus code into
                        the table for identification purposes. The added bytes
                        spell "SOUNDVIEW DATA SYSTEMS" in morse code!

11/03/90 00:21am bug in fixup_header ignored the possibility that the compaction
                        gap might be greater than 64K. fixed.

11/08/90 11:46pm bug due to missed communication with Michael Devore - WarpLink
        overlay manager IGNORES overlay seg-fixup at offset 0000. These must
        also be ignored by WOVL.ASM processing.

        Also - check_di_name() added to delete target named files if they
        exist prior to calling function 56h to rename to them, since the dupe
        will make the rename function fail.

11/14/90 04:56pm   lines 822,823  overlay file name in ovlmgr wasn't null
                        terminated, could cause names like "my_file.ovll"
01/16/91 05:13pm   line 3284 must also accept hyphens in obj module name symbols

02/07/91 09:56am modified for clipper 5.0  but needs to check symbols for flag

04/20/91 10:50am waiting for R128 release code, but few mods made to bring up
                  filename handling, renaming to SymPacWL 1.15 level.  Also
                  fixesup symboltable vectors that point to code in the data
                  segment, which seems to happen with some UDF's with bad
                  segment/class names.

05/09/91 12:49pm  copy_symbol modified to treat any symbol with a nonzero
                   symbol_nulls field as unique  (STATIC$ and static func's)
                     (note: "2" value means it's an INIT function)
                   potential problems still include the unknown-purpose data
                   table at ___sympb and ___sympe, which are NOT updated by
                   SymPac. However, ___sympb contains segment addresses and
                   lengths of all of the symbol chunks, and so is obviously
                   WRONG once the symbols have been packed! However, no ASM
                   reference to the segment/public is found in all of the
                   LIB files. Go figure? Perhaps for the debugger???

10/23/91 05:47pm   reports that it is failing with > 4095 symbols after
                compaction. There is a test for that at line 1917 that
                apparently makes no sense!!! The copy_old_symbols routine
                is smart enough to handle tables > 64K, so it shouldn't
                be a problem, since the symbol addressing was all modified
                to handle large tables by segment addressing???

* endcomment


; equ's go here...

BUFFER_PARA     equ     0fffh
READ_SIZE       equ     0fff0h
;;BUFFER_PARA     equ     0280h           ;small buffer for testing
;;READ_SIZE       equ     BUFFER_PARA * 10h

VERBOSE         equ     TRUE            ;turn on the annunciators

INCLUDE wp50.equ              ;all of the screen color attributes, structures
jmatch          equ     jnc

; extrn's and publics go here...

public  mem_err, psp, go_home, errorlevel
public  argc, argv, env_seg, has_a_dot, bad_spec_flag, bad_spec_msg
public errmsg, exetail, eqetail, ovetail, read_segstart, seek_err, read_err
public inheader, outheader, warp_sig, wl_to_clipper, read_startup,is_it_clipper
public saveax, savedx, olddx, alphachars, leading0, scratchpad, bcds, ascii
public fseek, warp_codeseg, warp_dataseg
public copy_header, read_header, load_size, ltoa, div32,  ltrim
public clearbuff,  warpcode, warpdata
public warp_ovlmgr, infile, outfile, check_for_d, get_td
public copy_symbol,seg_off_seek, get_weedbed, weedbed, weedend, weedlength
public copy_old_symbols, fix_pcode, pcode_waltz, brute, temp, tempnext
public pcode_finished,buffstartptr_msw, buffstartptr_lsw, my_fseek
public code_fixups, gap, buffendptr_msw, buffendptr_lsw, straddle
public data_fixups, copy_data, kill_fixup, symbol_lo, symbol_hi
public demon1, demon2
public ovl_err,ovlopen_err, ovl_text, dos3, internal_overlay_flag
public savedate, savetime, gap, bigdx_err, cl_startup, pcode_err, write_err
public fatal_gap_err, open_inovl, open_outovl, cl_plankhead
public baselength_msw, baselength_lsw
public new_baselength_msw, new_baselength_lsw, close_files
public proc_ovl_msg, proc_xovl_msg, date_flag, ovlcount_msg
public bug1, bug2, readsym, ovlmgr_name, tempname
public kelly_stuff
;***public no_display_flag

public patch_ovlmgr, eqe_flag, ove_flag, samebase_flag
public real_ovlname, unreal_ovlname, rename_err, ovltail
public fixup_header, check_di_name, bigsym_out_msg
public dup_executable, dup_nonexecutable, tempfilename, tempfilename$
public cl87_flag, get_symbol_type87, copy_symbol87,copy_new_symbols87
public pcode87_waltz,brute87, cl87_plankhead

;---syscreen externals
extrn wpscreen:near
extrn showscreen:near
extrn screenbox2:near
extrn screencalc:near
extrn writes:near
extrn ScreenSeg:word
extrn dirdcvt:near
extrn dirtcvt:near
extrn dbuff:byte
extrn tbuff:byte
extrn wphelp:near
extrn sound:near
extrn init_video:near
extrn set_args:near
extrn show_infile:near
extrn show_outfile:near
extrn strcpy:near
extrn strcmp:near
extrn strlen:near
extrn strncpy:near
;***extrn kelly_screen:near

;IFDEF DEBUG
;extrn bytes_prn:near
;extrn readbytes_prn:near
;extrn buffstart_prn:near
;ENDIF

extrn   read_code:near
extrn   parse_filename:near
extrn   process_overlay:near
extrn   open_overlay:near
extrn   show_overlay:near
extrn   copyname:near

LOCALS

DOSSEG

;DGROUP      GROUP    DATA, NULL

.model small

NULL    segment para    public  'BEGDATA'
NULL    ends

.stack

;------------------------------------------------DATA
;DATA    segment para    public  '_DATA'
.data

dgroup_start  label  word
dgroup_segment  dw      0

;---error messages
bad_comm_msg    db      16,17,lwhite,red," Bad command line. Type 'SP' for help.         ",0
bad_spec_msg    db      16,17,lwhite,red," ERROR: Illegal character(s) in Filespec.      ",0
bad_argv_msg    db      16,17,lwhite,red," ERROR: Incorrect command line.                ",0
no_mem_msg      db      16,17,lwhite,red," ERROR: Inadequate memory to run SP.           ",0
open_msg        db      16,17,lwhite,red," ERROR: Unable to open target file.            ",0
seek_msg        db      16,17,lwhite,red," ERROR: Unable to set file pointer.            ",0
read_msg        db      16,17,lwhite,red," ERROR: Unable to read target file.            ",0
close_msg       db      16,17,lwhite,red," ERROR: Unable to close target file.           ",0
write_msg       db      16,17,lwhite,red," ERROR: Target may have been corrupted.        ",0
;comm_msg        db      16,17,lwhite,red," ERROR: Target is not a valid Clipper file.    ",0
comm_msg        db      16,17,lwhite,red," ERROR: Invalid Target or unknown Overlay Mgr. ",0
samefile_msg    db      16,17,lwhite,red," ERROR: Infile and Outfile must be different.  ",0
warperr_msg     db      16,17,lwhite,red," ERROR: Unknown WarpLink overlay manager.      ",0
bigsym_out_msg  db      16,17,lwhite,red," ERROR: Clipper limit of 4095 symbols exceeded!",0
bigsym_msg      db      16,17,lwhite,red," ERROR: Symbol Segment too large to process!   ",0
badsym_msg      db      16,17,lwhite,red," ERROR: Invalid Symbol encountered.            ",0
gap_msg         db      16,17,lwhite,red," ERROR: Invalid compaction calculation.        ",0
bigdx_msg       db      16,17,lwhite,red," ERROR: Invalid file-length calculation.       ",0
ovl_msg         db      16,17,lwhite,red," ERROR: Bad overlay name or path too long.     ",0
badcom_msg      db      16,17,lwhite,red," ERROR: Syntax error on command line.          ",0
ovlopen_msg     db      16,17,lwhite,red," ERROR: Unable to open overlay file.           ",0
pcode_msg       db      16,17,lwhite,red," ERROR: Procedure-code parsing error.          ",0
rename_msg      db      16,17,lwhite,red," ERROR: Unable to correctly rename file(s).    ",0
cl50_msg        db      16,17,lwhite,blue," Clipper 5 startup code detected.              ",0
cl87_msg        db      16,17,lwhite,blue," Clipper Summer '87 startup code detected.     ",0
noneed_msg      db      16,17,lwhite,red,"NOTE: No duplicate symbols! No file(s) created.",0
not_warp_msg    db      16,17,lwhite,blue,"NOT linked with WarpLink Overlay Manager.     ",0
warplink_msg    db      16,17,lwhite,blue,"Linked with WarpLink Overlay Manager.         ",0
blank_msg       db      16,17,lwhite,blue,"                                              ",0
good_msg        db      16,18,lwhite,blue,"Successful symbol-compaction.                 ",0
kellystuff_msg  db      16,18,lwhite,blue,"Renaming files...                             ",0
proc_ovl_msg    db      16,18,lwhite,blue,"Processing Internal Overlay #                 ",0
proc_xovl_msg   db      16,18,lwhite,blue,"Processing External Overlay #                 ",0
ovlcount_msg    db      16+29,18,lwhite,blue, 17 dup (" "),0
errorlevel      db      0
was_null        dw      0               ;scratch flag used by copy_old_symbols
cl87_flag       dw      0

;---target file data
tempfilename    db      50h dup (0)
tempfilename$   db      'SVD{$$$}.$$$',0
real_ovlname    db      40h dup (0)
unreal_ovlname  db      40h dup (0)
has_a_dot       dw      0               ;flag to set if filespec has a dot
exetail         db      '.EXE',0        ;default exe extension for infile
exktail         db      '.EXK',0        ;exe ext. for infile after processing
eqetail         db      '.EQE',0        ;default eqe extension for outfile
ovetail         db      '.OVE',0        ;default ove ext. for outfile overlay
;ovetail         db      '.OVK',0        ;default ove ext. for outfile overlay
ovktail         db      '.OVK',0        ;ext. for source OVL after processing
ovltail         db      '.OVL',0        ;ext. for OutFile ovl after processing
bad_spec_flag   dw      0               ;flag for bad filespec from show_infile
samebase_flag   dw      0               ;flag for same base names for In, Out
eqe_flag        dw      0               ;flag that an "eqe" extension was used
ove_flag        dw      0               ;flag that an "ove" extension was used
imagesize       dw      0               ;low word of load image calculated size
                dw      0               ;high word
out_len         dw      0               ;length of outfile spec


cl_startup      db      0B4h, 30h       ;MOV AH,30
                db      0CDh, 21h       ;INT 21
                db      3Ch, 02h        ;CMP AL,02
                db      73h, 02h        ;JNB 1156
                db      0CDh, 20h       ;INT 20
                db      0BFh            ;MOV DI,xxxx
;
; The Plankton Headers are different...
;               db  0b8h,xx xx          ; xx xx = Near Ptr to PCODE
;               db  0bbh,xx xx          ; xx xx = Symbol_segment
cl_plankhead    db  53h                 ; push  bx
                db  0bbh,00h,00h        ; mov   bx,0000
                db  53h                 ; push  bx
                db  0eh                 ; push  cs
                db  50h                 ; push  ax
                db  9ah                 ; call (FAR)

cl87_plankhead  db  55h                 ;push  bp
                db  8bh, 0ech           ;mov   bp,sp
                db  33h, 0dbh           ;xor   bx,bx
                db  0b8h, 00, 00        ;mov   ax,0000
                db  50h                 ;push  ax
                db  53h                 ;push  bx
                db  0b8h                ;mov   ax, (symbol_ptr)
                dw  00,00               ;(symbol_ptr)


headerbuffer    dw      0               ;buffer allocated for full header
weedbed         dw      0               ;segment address, above header
weedend         dw      0
new_weedend     dw      0               ;adjusted-down seg address
weedlength      dw      0               ; = symbol count
weedbed_ptr_hi  dw      0               ;file offsets (ABOVE header)
weedbed_ptr_lo  dw      0

;---fix_pcode data
temp            dw      0               ;used to hold si during fix_pcode()
tempnext        dw      0               ;used to hold si during fix_pcode()
buffstartptr_msw        dw      0
buffstartptr_lsw        dw      0
buffendptr_msw          dw      0
buffendptr_lsw          dw      0
bufferbytes             dw      0
gap                     dw      0
straddle                dw      0
tempax                  dw      0
tempdx                  dw      0
symbol_lo               dw      0
symbol_hi               dw      0
es_seg                  dw      0
baselength_msw          dw      0
baselength_lsw          dw      0
new_baselength_msw          dw      0
new_baselength_lsw          dw      0

nextptr_msw     dw      0               ;file offset (above header) for next read
nextptr_lsw     dw      0               ;
last_read       dw      0               ;# bytes from last read
symstart_msw    dw      0               ;symstart ptr above header
symstart_lsw    dw      0
pcode_finished  dw      0               ;flag to exit processing loop
dup_executable          dw      0       ;running count
dup_nonexecutable       dw      0


;---data structures
inheader  exeheader  <>                         ;structure for input exe hdr
outheader exeheader  <>                         ;structure for modified exe hdr
warpdata  warp_data  <>                         ;pattern - WarpLink OVLMGR data
warpcode  warp_code  <>                         ;pattern - WarpLink cseg data
infile    fileinfo   <>                         ;filespec, handle, buffer info
outfile   fileinfo   <>                         ;
symwindow  window    <16, 7,29,12,18,12,lwhite,blue,> ;initialize
procwindow window    <16,14,29,15,18,15,lwhite,blue,> ;initialize


warp_dataseg    dw      0                       ;save the warpdata segment, lo
                dw      0                       ;hi word
warp_codeseg    dw      0                       ;save the warpcode segment, lo
                dw      0                       ;hi word
warp_ovlmgr     dw      0                       ;flag = 1 if found
warp_sig        db      "FATAL Overlay Manager Error, Code "  ; at ds:001ah
mainstack       dw      0                       ;initial sp from .exe header
minalloc        dw      0                       ;min alloc (paragraphs) from hdr
scratchpad      db      12 dup (0)
internal_overlay_flag       dw      0           ;TRUE if internal overlay
ovlmgr_name     db      13 dup (0)              ;copied from OVLMGR data
;***kellyjoke       dw      FALSE           ;flag for kelly joke about program name
;***no_display_flag	DB		FALSE					; flag for no display of processing


;---argv data
argc            dw      0
argv            dd      0
psp             dw      0
env_seg         dw      0
dos3            dw      0


saveax          dw      0                ;used by asc2num
savedx          dw      0                ; ""
olddx           dw      0                ; ""

savedate        dw      0               ;save original file date/time
savetime        dw      0
date_flag       dw      0


; note--- the filename offset into the variable is target_msg+14 bytes
;                                                   
target_msg      db       29, 6,lwhite,blue," TARGET: ", 20 dup(" "), 0
ovl_text        db       32,10,lwhite,blue," OvlFile: ", 19 dup(" "), 0
symcount_msg    db      16,17,lwhite,blue,"InFile Symbol Count:        Duplicates:       ",0
tdstring        db      44,15,lwhite,blue, 17 dup(" "), 0
tch_msg         db      16,18,lwhite,blue,"                              (Date Preserved)",0
no_tch_msg      db      16,18,lwhite,blue,"                                              ",0
sizestring      db      44, 8,lwhite,blue, 19 dup(" "), 0
outsizestring   db      44,12,lwhite,blue, 19 dup(" "), 0
newsizestring   db      55, 5,lwhite,blue,"(           )", 0
parmstring      db      18,0 ,lwhite,blue, 13 dup (0)   ;for printing parms to screen
old_symlength   db      52, 9,lwhite,blue, 15 dup(" "), 0
new_symlength   db      52,13,lwhite,blue, 15 dup(" "), 0
mssg_read       db      16,18,lwhite,blue,"Reading                                       ",0
mssg_write      db      16,18,lwhite,blue,"                  Writing                     ",0
mssg_fixup      db      16,18,lwhite,blue,"                                 Fixups       ",0
mssg_blank      db      16,18,lwhite,blue,"                                              ",0
mssg_datacopy   db      16,18,lwhite,blue,"Copying Data Segments to Output file.         ",0

ovlnamebuff     db      22,17,lwhite,blue,14 dup(0)     ;buffer for ovl name
tempname        db      40h      dup (0)

;---number-conversion scratchpad
bcds            dw      0
ascii           db      "0123456789ABCDEF"
leading0        dw      0
alphachars      dw      0

INCLUDE tbl5.equ                 ;pcode table values

;DATA    ends

;------------------------------------------------CODE
;cseg    segment para    public  'CODE'
;        assume  cs:cseg,ds:DGROUP,es:DGROUP,ss:DGROUP

.code
wp       proc    far
wpstart:
        ;---check DOS version
        mov     dx,03                   ;assume DOS 3.x or better
        mov     ah,30h
        int     21h
        cmp     al,3
        jae     cd2                     ;anything >= 3.0 is fine
        cmp     al,2
        jae     cd1
        jmp     go_home
cd1:    mov     dx,02                   ;else must be 2.x
        cmp     ah,1
        jae     cd2
        jmp     go_home
cd2:

;---startup
        xor     bp,bp
        mov     ax,@data
        mov     ss,ax
        mov     sp,offset stack
        mov     ds,ax
        mov     dos3,dx                 ;save the DOS major number
        cld
        mov     bp,offset dgroup_start
        mov     psp,es
        mov     ax, es:[002ch]          ;PSP's ptr to environment seg
        mov     env_seg,ax
        mov     ax,sp
        add     ax,15
        mov     cl,4
        shr     ax,cl
        mov     bx,ss
        add     bx,ax
        sub     bx,psp
        mov     ah,4ah
        int     21h                     ;return the excess memory

;---initialize video, and parse command line - ds = DGROUP, es = PSP
        call    set_args                ;build argv array of ptrs
        call    init_video              ;color or mono? also clear the screen

;---draw the screen or show help
        cmp     argc,1                  ;arguments?
        ja     not_help
        call    wphelp                  ;display the help screen
        jmp     go_home2                ;quit with no chirp
not_help:
	cmp	argc,4
	jbe	@@nh1
	jmp     bad_command_err		;too many args passed
@@nh1:  call    show_infile             ;patch infile into screentext
        cmp     argc,2                  ;2 args means "SP5 target"
        jbe     @@nh2                   ;no outfile or flags

	;---there's an outfile and/or date flag too. Argc = 3 or 4
	call    check_for_d             ;carry clear means /d (or /!)
	jc      @@nh2
	dec	argc			;ignore the /d arg from now on
@@nh2:  call    show_outfile            ;sets bad_spec_flag if invalid file spec
call_wp:
        call    wpscreen                ;print initial screen with filenames
        cmp     bad_spec_flag,1         ;abort if bad filespec
        jne     goodspec
        mov     si,offset bad_spec_msg
        jmp     errmsg
goodspec:                               ;now make sure that names are different
        push    ds
        pop     es
        mov     di,offset outfile.filename
        mov     dx,out_len
        mov     si,offset infile.filename
        mov     bx,infile.spec_len
        call    strcmp                  ;see if they're the same
        jnz     not_same
        jmp     samefile_err            ;don't overwrite a file
not_same:
        mov     di,offset outfile.filename      ;set flag if basenames are ==
        mov     dx,outfile.spec_len
        sub     dx,4
        mov     si,offset infile.filename
        mov     bx,infile.spec_len
        sub     bx,4
        call    strcmp                  ;see if they're the same
        jnz     not_samebase
        mov     samebase_flag,TRUE
not_samebase:

;---open the input file
        call    open_infile
        jnc     ot1
        jmp     open_err
ot1:    mov     infile.filehandle, ax          ;save the handle

;---display the time/date stamp info and load-size
        call    read_header
        call    get_td
        mov     bx,offset inheader      ;infile header structure
        mov     si,offset sizestring    ;buffer to receive size ascii string
        call    load_size               ;struc in ds:bx, textbuffer in ds:si

;---is it a valid .exe file?
        cmp     inheader.signature, 'ZM'        ;exe signature
        je      valx
        jmp     valx_err
valx:   call    copy_header             ;copy inheader to outheader struc, file
        mov     ax,inheader.header_paragraphs
        mov     infile.header_paras,ax  ;save header length in struc infile
;---read startup code to buffer to search for ds store
        call    read_code               ;look for Clipper (and Warp OVLMGR)
        jnc     got_clipper             ;carry set = not a clipper file
        jmp     valx_err                ;"not a valid Clipper file" message
got_clipper:
        mov     ax,wptr [warpcode+0bh]    ;startup code is still in buffer
        mov     infile.datagroup,ax           ;save old_dgroup

;---do some direct reads to locate symbol table start/stop. use warpcode buffer.
        call    get_weedbed             ;symbols @ weedbed/weedend/weedlength
        mov     si,offset cl87_msg      ;announce which version we found,
        cmp     cl87_flag,1
        je      @@clver
        mov     si,offset cl50_msg
@@clver: call   showscreen

;---allocate symbol buffer and read entire symbol table to it.
        call    get_old_symbols

;---display and count the symbols and procs from infile.symseg buffer
        call    process_symbols         ;also do all the other dirty work

;---process WarpOverlay if present
        cmp     infile.ovlhandle,0      ;non-zero means there is an overlay
        je      fixdate
        call    process_overlay         ;files are still open!

;---if datepatch was requested, restore the date now
fixdate:
        mov     si,offset no_tch_msg       ;"date overwritten" message
        cmp     date_flag,TRUE
        jne     tch1
        mov     bx,outfile.filehandle
        mov     ax,5701h
        mov     cx,savetime
        mov     dx,savedate
        int     21h                     ;"touch" the dir date/time stamp
        mov     si,offset tch_msg
tch1:   call    showscreen              ;announce it

cu1:    mov     di,800h
        mov     cx,2000h
        call    sound                   ;chirp
        jmp     go_home

samefile_err:
        mov     si,offset samefile_msg    ;"cannot pack file into itself" text
        jmp     errmsg

valx_err:
        mov     si,offset comm_msg    ;"not valid Clipper file" text
        jmp     errmsg

bigsym_err:
        mov     si,offset bigsym_msg    ;"Symbol segment too big" text
        jmp     errmsg

symbol_err:
        mov     si,offset badsym_msg    ;"Illegal Symbol " text
        jmp     errmsg

warp_err:
        mov     si,offset warperr_msg   ;"Unknown WarpLink OVLMGR" text
        jmp     errmsg

bigdx_err:
        mov     si,offset bigdx_msg    ;"Illegal File Length calculation " text
        jmp     errmsg

mem_err:
        mov     si,offset no_mem_msg    ;"no memory" message text
        jmp     errmsg

ovl_err:
        mov     si,offset ovl_msg    ;"bad ovl or path too long" message text
        jmp     errmsg

ovlopen_err:
        mov     si,offset ovlopen_msg    ;"bad ovl open" message text
        jmp     errmsg

open_err:
        mov     si,offset open_msg    ;"unable to open target" message text
        jmp     errmsg

read_err:
        mov     si,offset read_msg    ;"unable to open target" message text
        jmp     errmsg

seek_err:
        mov     si,offset seek_msg    ;"unable to seek target" message text
        jmp     errmsg

write_err:
        mov     si,offset write_msg   ;"unable to re-write target" message text
        jmp     errmsg

pcode_err:
        mov     si,offset pcode_msg   ;"error parsing pcode" message text
        jmp     errmsg

rename_err:
        mov     si,offset rename_msg  ;"error renaming files" message text
        jmp     errmsg

final_err:
        mov     si,offset close_msg   ;"unable to close target" message text
        jmp     errmsg

fatal_gap_err:
        mov     si,offset gap_msg   ;"Fatal error - compaction" message text
        jmp     errmsg

bad_command_err:
        mov     si,offset badcom_msg   ;"Syntax error on command line" text
        jmp     errmsg

errmsg:
        call    showscreen              ;showscreen will set es
        mov     errorlevel,255          ;set errorlevel to -1
        mov     di,100h
        mov     cx,0b000h
        call    sound                   ;make some noise
        jmp     go_home2

go_home:
        mov     ax,infile.filehandle
        or      ax,outfile.filehandle
        je      go_home1
        call    kelly_stuff             ;play with file names if needed
        mov     si,offset good_msg
        call    showscreen
;***        cmp     kellyjoke, TRUE
;***        jne     go_home1
;***        call kelly_screen               ;Ugly-name message
go_home1:
        mov     di,1000h
        mov     cx,3000h
        call    sound                   ;chirp
        mov     di,0b00h
        mov     cx,3000h
        call    sound                   ;chirp
        mov     di,1000h
        mov     cx,3000h
        call    sound                   ;chirp
        mov     di,0b00h
        mov     cx,3000h
        call    sound                   ;chirp
go_home2:
        mov     cx,0b0ch
        cmp     ScreenSeg,0b000h
        je      gh2
        mov     cx,0607h
gh2:    mov     ah,01
        int     10h                     ;restore cursor size
        mov     ah,02h
        xor     bh,bh
        mov     dh,23
        mov     dl,0
        int     10h                     ;put cursor at 23,0 on exit
        mov     al,errorlevel
        mov     ah,4ch
        int     21h                     ;return to DOS

wp      endp

;  kelly_stuff()       fixup the file names as needed, patch the OVLMGR name
;                       for the ovl file if needed, and close the files
;
;
kelly_stuff    proc    near

        mov     ax,@data
        mov     ds,ax
        mov     es,ax
        mov     si,offset kellystuff_msg
        call    showscreen
        cmp     samebase_flag,TRUE
        jne     @@different
        jmp     @@same

@@different:
        cmp     warp_ovlmgr,TRUE        ;overlayed?
        je      @@00
        jmp     @@done
@@00:   cmp     internal_overlay_flag,TRUE
        jne     @@01                    ;jump if external
        mov     si,offset exetail       ;patch ovlmgr to look for base.exe
        jmp     @@02

@@01:   mov     si,offset ovltail       ;patch ovlmgr to look for base.ovl
@@02:   call    patch_ovlmgr

        call    close_files             ;finish up and leave

        ;---rename ovl file to new ovl name if external overlay
        cmp     internal_overlay_flag,TRUE
        jne     @@025
        jmp     @@done
@@025:  mov     si,offset outfile.filepath
        mov     di,offset tempname
        mov     cx,40h
        call    strcpy                       ;copy the outfile path
        mov     si,offset warpdata.ovl_name
        call    strcpy                       ;add the ovl name
        mov     di,offset tempname
        mov     dx,offset outfile.ovlname
        call    check_di_name                   ;look for dupes, delete
        mov     ah,56h
        int     21h
        jnc     @@003
        jmp     rename_err

        ;---if exk_name flag is set, rename the outfile to "exe"
@@003:  cmp     eqe_flag,TRUE
        jne     @@004
        mov     si,offset outfile.filename
        mov     di,offset tempname   ;build the new outfile.exe name
        mov     cx,40h
        call    copyname
        mov     si,offset exetail
        mov     cx,2
    rep movsw                   ;add the exe extension

        ;---don't try to rename if already equal
        mov     si,offset outfile.filename
        mov     di,offset tempname
        mov     bx,outfile.spec_len
        mov     dx,outfile.spec_len
        call    strcmp
        je      @@004                   ;if equal, don't rename or you'll crash

        ;---do the actual renaming of the outfile
        mov     di,offset tempname
        mov     dx,offset outfile.filename
        call    check_di_name                   ;look for dupes, delete
        mov     ah,56h
        int     21h
        jnc     @@004
        jmp     rename_err
@@004:  jmp     @@done

@@same:
        call    close_files             ;have to be closed to rename

        ;---rename the base file. swap the InFile and OutFile names
        mov     dx,offset outfile.filename
        mov     di,offset tempfilename
        call    check_di_name                   ;look for dupes, delete
        mov     ah,56h
        int     21h
        jnc     @@0
        jmp     rename_err

@@0:    mov     cx,40h
        mov     si,offset outfile.filename
        mov     di,offset tempname
        call    copyname                ;copies up to the "." or null
        mov     si,offset exktail
        call    strcpy

        mov     dx,offset infile.filename
        mov     di,offset tempname      ;rename original to oldfile.exk
        call    check_di_name                   ;look for dupes, delete
        mov     ah,56h
        int     21h
        jnc     @@1
        jmp     rename_err
@@1:    mov     dx,offset tempfilename
        mov     di,offset infile.filename
        cmp     eqe_flag,TRUE           ;use infile if we put in an "eqe"
        je      @@15
        mov     di,offset outfile.filename
@@15:   call    check_di_name                   ;look for dupes, delete
        mov     ah,56h
        int     21h
        jnc     @@2
        jmp     rename_err
@@2:    cmp     warp_ovlmgr,TRUE                ;overlayed file?
        je      @@3
        jmp     @@done
@@3:    cmp     internal_overlay_flag,TRUE
        jne     @@4                             ;external still needs work
        jmp     @@done

        ;---swap the overlay InFile and OutFile names
@@4:    mov     dx,offset outfile.ovlname
        mov     di,offset tempfilename          ;rename output ovl to temp
        call    check_di_name                   ;look for dupes, delete
        mov     ah,56h
        int     21h
        jnc     @@5
        jmp     rename_err
@@5:    mov     dx,offset infile.ovlname
        mov     si,offset outfile.ovlname       ;rename infile ovl to outfile.ovk
        mov     di,offset tempname
        mov     cx,40h
        call    copyname
        mov     si,offset ovktail               ;use .OVK instead of .OVE
        call    strcpy
        mov     di,offset tempname
        call    check_di_name                   ;look for dupes, delete
        mov     ah,56h
        int     21h
        jnc     @@6
        jmp     rename_err
@@6:    mov     dx,offset tempfilename
        mov     di,offset infile.ovlname        ;rename temp(the old outfile) to infile
        call    check_di_name                   ;look for dupes, delete
        mov     ah,56h
        int     21h
        jnc     @@done
        jmp     rename_err

@@done: ret
@@ret:  ret

kelly_stuff    endp


; check_di_name         pre-test before trying to rename files using
;                               DOS function 56h, which will fail if file
;                               name already exists. es:di points to the
;                               name to be tested. If found, the dupe is
;                               deleted. ds = es = DGROUP
;                               dx, di must be preserved!
;
check_di_name   proc    near

        push    dx
        push    di

        ;---delete the file
        mov     dx,di
        mov     ah,41h
        int     21h                     ;try to delete file with name at ds:dx
        jnc     @@ret
        cmp     ax,2
        je      @@ret                   ;return code 2 = "file not found"
        jmp     rename_err

@@ret:  pop     di
        pop     dx
        ret

check_di_name   endp


; patch_ovlmgr()    uses the data already in WarpData, and writes it back to
;                       the output file, after rewinding the filepointer,
;                       which was stored originally in warp_dataseg
;
patch_ovlmgr    proc    near

        ;---rewind the file pointer
        mov     dx,warp_dataseg
        mov     cx,warp_dataseg+02
        mov     ax,4200h
        mov     bx,outfile.filehandle
        int     21h
        jnc     @@0
        jmp     seek_err

        ;---patch the overlay name in the buffer at warpdata.ovl_name
@@0:    push    si
        mov     si,offset outfile.filebase
        mov     di,offset warpdata.ovl_name
        mov     cx,9
@@1:    lodsb
        or      al,al
        je      @@2
        cmp     al,"."
        je      @@2
        stosb
        loop    @@1
        jmp     rename_err      ;should never get here. Name too long

        ;---retrieve the pointer to the extension name
@@2:    pop     si
        lodsw
        stosw
        lodsw
        stosw                   ;move the 4 bytes ".EXE" or ".OVL"
        xor     al,al
        stosb                   ;remember to null-terminate it!

        ;---rewrite the buffer
        mov     ah,40h
        mov     bx,outfile.filehandle
        mov     dx,offset warpdata
        mov     cx,100h         ;the full-sized buffer
        int     21h             ;rewrite it.
        jnc     @@3
        jmp     write_err

@@3:    cmp     date_flag,TRUE
        jne     @@4
        mov     bx,outfile.filehandle
        mov     ax,5701h
        mov     cx,savetime
        mov     dx,savedate
        int     21h                     ;"touch" the dir date/time stamp

@@4:    ret

patch_ovlmgr    endp


;get_old_symbols proc    near    ;allocate and fill the old symbol table
;
;        mov     bx,infile.sym_count     ;each symbol gets 1 paragraph
;        mov     ah,48h
;        int     21h                     ;request allocation for new symbols
;        jnc     alloc2
;        jmp     mem_err                 ;"not enough memory"
;alloc2:
;        mov     outfile.symseg,ax           ;save the buffer
;        mov     bx,infile.sym_count
;        mov     ah,48h
;        int     21h                     ;request allocation for old symbols
;        jnc     alloc3
;        jmp     mem_err                 ;"not enough memory"
;alloc3:
;        mov     infile.symseg,ax           ;save the buffer
;        mov     ax,infile.sym_count         ;sym length in paragraphs
;        xor     dx,dx
;        mov     si,offset old_symlength ;line buffer with attributes, row, col
;        call    show_paras              ;show symbol table length on screen
;        mov     bx,offset infile
;        xor     dx,dx
;        mov     cx,weedbed              ;seg for _symstart
;        call    seg_off_seek
;        jnc     @@1
;        jmp     seek_err
;@@1:    mov     cx,infile.sym_count         ;sym length in paragraphs
;        shl     cx,1
;        shl     cx,1
;        shl     cx,1
;        shl     cx,1                    ;convert to bytes
;        jnc     @@2
;        jmp     bigsym_err
;@@2:    mov     ax,infile.symseg           ;the buffer address
;        push    ds
;        mov     ds,ax
;        xor     dx,dx
;        mov     ah,3fh
;        int     21h                     ;read symbol table to old_symseg:0000
;        pop     ds
;        jnc     @@4
;@@3:    jmp     read_err
;@@4:    cmp     ax,cx
;        jne     @@3
;        mov     ax,outfile.sym_count         ;sym length in paragraphs
;        xor     dx,dx
;        mov     si,offset new_symlength ;line buffer with attributes, row, col
;        call    show_paras              ;show symbol table length on screen
;        ret
;
;get_old_symbols endp



get_old_symbols proc    near    ;allocate and fill the old symbol table

        mov     bx,infile.sym_count     ;each symbol gets 1 paragraph
        mov     ah,48h
        int     21h                     ;request allocation for new symbols
        jnc     alloc2
        jmp     mem_err                 ;"not enough memory"
alloc2:
        mov     outfile.symseg,ax           ;save the buffer
        mov     bx,infile.sym_count
        mov     ah,48h
        int     21h                     ;request allocation for old symbols
        jnc     alloc3
        jmp     mem_err                 ;"not enough memory"
alloc3:
        mov     infile.symseg,ax           ;save the buffer
        mov     ax,infile.sym_count         ;sym length in paragraphs
        xor     dx,dx
        mov     si,offset old_symlength ;line buffer with attributes, row, col
        call    show_paras              ;show symbol table length on screen
        mov     bx,offset infile
        xor     dx,dx
        mov     cx,weedbed              ;seg for _symstart
        call    seg_off_seek
        jnc     @@1
        jmp     seek_err
@@1:    mov     cx,infile.sym_count     ;sym length in paragraphs
        push    ds
        mov     ax,infile.symseg
        mov     ds,ax                   ;aim at the buffer. DS ALTERED!

@@top:
        cmp     cx,0fffh                ;max single-read is fff0h bytes
        jbe     @@readall
        mov     bx,cx                   ;save the paragraph count in bx
        sub     bx,0fffh
        mov     cx,0fffh                ;maximum single read (paragraphs)
        call    readsym                 ;read (cx_paragraphs) bytes

@@bottom:
        mov   cx,bx                   ;remaining paragraphs to read
        jmp     @@top

@@readall:
        call    readsym

@@ret:  pop     ds
        mov     ax,outfile.sym_count         ;sym length in paragraphs
        xor     dx,dx
        mov     si,offset new_symlength ;line buffer with attributes, row, col
        call    show_paras              ;show symbol table length on screen
        ret

get_old_symbols endp


readsym         proc    near            ;cx = #paragraphs to read

        push    bx
        shl     cx,1
        shl     cx,1
        shl     cx,1
        shl     cx,1                    ;convert to #bytes to read
        mov     bx,ss:infile.filehandle
        xor     dx,dx
        mov     ah,3fh
        int     21h                     ;read symbol table chunk to ds:0000
        jnc     @@4
@@3:    mov     ax,@data
        mov     ds,ax
        jmp     read_err
@@4:    cmp     ax,cx
        jne     @@3
        shr     cx,1
        shr     cx,1
        shr     cx,1
        shr     cx,1                    ;convert back to paragraphs
        mov     ax,ds
        add     ax,cx
        mov     ds,ax                   ;bump bufferstart for next read
        pop     bx
@@ret:  ret

readsym         endp




get_td  proc    near
        mov     bx,infile.filehandle
        mov     ah,57h
        mov     al,00                   ;GET info
        int     21h                     ;dx = Date,  cx = Time
        jnc     gtd1
        jmp     read_err
gtd1:   mov     savetime,cx
        mov     savedate,dx
        mov     ax,dx                   ;date
        mov     bx,8                    ;buffer length
        mov     si,offset tdstring+4
        call    dirdcvt
        mov     ax,cx                   ;time
        mov     bx,6                    ;buffer length
        mov     si,offset tdstring+13
        call    dirtcvt
        mov     si,offset tdstring
        call    showscreen
        ret
get_td  endp

open_infile     proc    near   ;     call with  file structure in bx
        mov     bx,offset infile
        call    parse_filename          ;get basename and path

        ;---set up the tempfilename with a path (for later use in renaming)
        mov     si,offset infile.filepath
        mov     di,offset tempfilename
        mov     cx,40h
        call    strncpy                         ;copy the path. di -> null
        mov     si,offset tempfilename$
        call    strcpy                          ;add the filename

        ;---now try to open the infile
        mov     ah,3dh
        mov     al,0a0h                 ;read only
        cmp     dos3,3                  ;attribute differs for DOS 2 vs 3+
        jae     @@1
        mov     al,0h                   ;read only DOS2
@@1:    mov     dx,offset infile.filename         ;the filenamecpy'd argv[1]
        int     21h
        ret
open_infile      endp

open_inovl      proc    near   ;     call with  file structure in bx
        mov     ah,3dh
        mov     al,0a0h                 ;read only
        cmp     dos3,3                  ;attribute differs for DOS 2 vs 3+
        jae     @@1
        mov     al,0h                   ;read only DOS2
@@1:    mov     dx,offset infile.ovlname         ;the filenamecpy'd argv[1]
        int     21h
        ret
open_inovl       endp


open_outfile    proc    near            ;create/truncate-open
        mov     bx,offset outfile
        call    parse_filename          ;get basename and path
        mov     ah,3ch
        xor     cx,cx
        mov     dx,offset outfile.filename   ;the filenamecpy'd argv[1] argument
        int     21h
        ret
open_outfile     endp


open_outovl     proc    near            ;create/truncate-open
        mov     ah,3ch
        xor     cx,cx
        mov     dx,offset outfile.ovlname
        int     21h
        ret
open_outovl      endp

read_header     proc    near            ;read the target's header
        mov     ah,3fh
        mov     bx,infile.filehandle
        mov     cx,22h
        mov     dx,offset inheader      ;read to structure inheader
        int     21h
        jnc     @@1
        pop     cx                      ;fix the stack
        jmp     read_err

        ;---get the file length form the header numbers - for later use
@@1:    mov     ax,inheader.length512
        cmp     inheader.length_mod512,0
        je      @@2
        dec     ax
@@2:    mov     bx,512
        mul     bx              ;results in dx:ax
        add     ax,inheader.length_mod512
        adc     dx,0
        mov     baselength_msw,dx
        mov     baselength_lsw,ax

        ret
read_header     endp

copy_header     proc    near            ;copy target's header and write to file

        ;---copy the header structure to outheader
        push    ds
        pop     es
        mov     cx,22h
        mov     si, offset inheader
        mov     di, offset outheader
  rep   movsb
        ;---rewind infile to start of header
        mov     bx,infile.filehandle
        xor     cx,cx
        xor     dx,dx
        mov     ax,4200h
        int     21h             ;rewind to 00:00
        jnc     @@0
        jmp     seek_err
        ;---allocate a big enough buffer for the entire header
@@0:    mov     bx,outheader.header_paragraphs
        mov     ah,48h
        int     21h             ;allocate a buffer
        jnc     @@1
        jmp     seek_err
@@1:    mov     headerbuffer,ax
        ;---read the entire header now
        mov     ds,ax           ;point to the buffer with ds:dx
        xor     dx,dx
        mov     cx,es:outheader.header_paragraphs
        shl     cx,1
        shl     cx,1
        shl     cx,1
        shl     cx,1
        mov     bx,es:infile.filehandle
        mov     ah,03fh
        int     21h             ;read the header to buffer
        jnc     @@2
@@err:  push    es
        pop     ds
        jmp     read_err
@@2:    cmp     ax,cx           ;partial read is an error
        jne     @@err
;        ;---now write the header back out to the outfile
;        mov     cx,es:outheader.header_paragraphs
;        shl     cx,1
;        shl     cx,1
;        shl     cx,1
;        shl     cx,1
;        xor     dx,dx
;        mov     bx,es:outfile.filehandle
;        mov     ah,40h
;        int     21h
;        jc      @@err
        push    es
        pop     ds
        ret

copy_header     endp


;  close_files   uses non-zero value in filehandle struc as flag
;                    must check for root AND ovl files in InFile AND OutFile
;
close_files     proc    near                    ;close in- and out-files

        mov     ax,@data
        mov     ds,ax
        mov     bx,infile.filehandle
        or      bx,bx
        je      @@1
        mov     ah,3eh
        int     21h
        mov     infile.filehandle,0
        jnc     @@1
        pop     cx                      ;fix the stack
        jmp     final_err
@@1:
        mov     bx,outfile.filehandle
        or      bx,bx
        je      @@2
        mov     ah,3eh
        int     21h
        mov     outfile.filehandle,0
        jnc     @@2
        pop     cx                      ;fix the stack
        jmp     final_err
@@2:
        cmp     internal_overlay_flag,TRUE
        je      @@4
        mov     bx,infile.ovlhandle
        or      bx,bx
        je      @@3
        mov     ah,3eh
        int     21h
        mov     infile.ovlhandle,0
        jnc     @@3
        pop     cx                      ;fix the stack
        jmp     final_err
@@3:
        mov     bx,outfile.ovlhandle
        or      bx,bx
        je      @@4
        mov     ah,3eh
        int     21h
        mov     outfile.ovlhandle,0
        jnc     @@4
        pop     cx                      ;fix the stack
        jmp     final_err
@@4:    ret

close_files     endp


wl_to_clipper   proc    near    ;loads clipper startup.  ds = es = dgroup

        ;---search for far jump to find data address of Clipper offset vector
        mov     cx,250h                  ;arbitrary search limit
        mov     ax,0ff2eh                ;op codes for "cs:far jump" (reversed)
        mov     si,offset warpcode
@@0:    cmp     al,[si]                 ;ds:si is the read-buffer
        jne     @@01
        inc     si
        cmp     ah,[si]                 ;does the 2nd byte match?
        jne      @@1
        inc     si
        mov     bl,[si]
        dec     si
        cmp     bl,2eh                  ;third byte ok?
        je      @@2
        jne     @@1
@@01:   inc     si
@@1:    loop    @@0
        jmp     warp_err                ;didn't find "cs:far jump"
@@2:    add     si,2                    ;point it at the data offset value
        lodsw                           ;ax = warpCODE cs:offset for jump ptr
        mov     si,offset warpcode
        add     si,ax                   ;si points to vector now
        lodsw                           ;offset
        mov     dx,ax
        lodsw                           ;seg
        mov     cx,ax                   ;vector to Clipper startup now in cx:dx

        ;---rewind infile to point to Clipper startup
        mov     bx,offset infile
        call    seg_off_seek    ;aim at the ptr to __sym_init

        ;---read 100h bytes of Clipper startup to "warpcode" buffer
@@3:    mov     ah,3fh
        mov     dx,offset warpcode
        mov     bx,infile.filehandle
        mov     cx,100h
        int     21h
        jnc     @@4
        jmp     read_err

@@4:    ret

wl_to_clipper   endp


read_segstart   proc    near           ;read the first 250 bytes of startup
                                       ;  code to the buffer "warpcode"
                                       ; NOTE: does NOT add in the init_offset
                                       ;
;---fseek to the start of code segment
        xor     cx,cx
        mov     dx,inheader.init_cs
        add     dx,inheader.header_paragraphs
        call    fseek
        mov     warp_codeseg,ax
        mov     warp_codeseg+02,dx
        jnc     @@1
        pop     cx
        pop     cx                      ;fix the stack
        jmp     seek_err

;---read 250h bytes to buffer
@@1:    mov     bx,infile.filehandle
        mov     cx,250h
        mov     dx,offset warpcode
        mov     ah,3fh
        int     21h                     ;read 250h bytes
        jnc     @@2
        pop     cx
        pop     cx                      ;fix the stack
        jmp     read_err

@@2:    ret

read_segstart   endp


read_startup    proc    near           ;read the first 100 bytes of startup
                                       ;  code to the buffer "warpcode"
;---fseek to the start of code segment
        xor     cx,cx
        mov     dx,inheader.init_cs
        add     dx,inheader.header_paragraphs
        shl     dx,1
        rcl     cx,1
        shl     dx,1
        rcl     cx,1
        shl     dx,1
        rcl     cx,1
        shl     dx,1
        rcl     cx,1                     ;convert to byte offset in dx:cx
        add     dx,inheader.init_ip
        adc     cx,0
        mov     bx,infile.filehandle
        mov     ax,4200h
        int     21h                     ;fseek to start of init_cs segment
        jnc     @@1
        pop     cx
        pop     cx                      ;fix the stack
        jmp     seek_err

;---read 100h bytes to buffer
@@1:    mov     bx,infile.filehandle
        mov     cx,100h
        mov     dx,offset warpcode
        mov     ah,3fh
        int     21h                     ;read 100h bytes
        jnc     @@2
        pop     cx
        pop     cx                      ;fix the stack
        jmp     read_err

@@2:    ret

read_startup    endp




is_it_clipper   proc    near            ;is it clipper startup code in the
                                        ;  warpcode buffer?
        push    ds
        pop     es                      ;ds = es = DGROUP
        mov     di,offset warpcode
        mov     si,offset cl_startup
        mov     cx,11                   ;signature length
   repz cmpsb                           ;compare the signature with the buffer
        jz      @@0
        stc
        jc      @@1
@@0:    clc
@@1:    ret

is_it_clipper   endp

fseek   proc    near            ;arrive with dx:cx in paragraphs

        shl     dx,1
        rcl     cx,1            ;NOTE that offset was NOT added in!!
        shl     dx,1            ;    this routine for seg:0000 only
        rcl     cx,1
        shl     dx,1
        rcl     cx,1
        shl     dx,1
        rcl     cx,1                     ;convert to byte offset in dx:cx
        mov     bx,infile.filehandle
        mov     ax,4200h
        int     21h                     ;fseek to start of init_cs SEGMENT
        ret

fseek   endp


;
; get_weedbed()  uses Infile handle and WarpCode buffer to follow pointer
;                  chain from _astart to __sym_count to get segment addresses
;                  for __SYMSTART ("weedbed") and __SYMEND ("weedend").
;                  Call with Clipper startup in WarpCode buffer, starting with
;                  _astart at buffer offset 0.
;                  preserves ds = DGROUP, returns carry clear if successful.
;                There are two seperate routines, for 5.01 and S87
;
get_weedbed     proc    near

        ;---handle S87 and 5.01 differently
@@cl50: mov     bx,0a0h         ;_main far pointer is at _astart+0a0h
        mov     dx,wptr warpcode[bx]    ;offset
        mov     cx,wptr warpcode[bx+02] ;segment above .exe header for _main
        ;add     dx,01bh                 ;start reading at _main
        adc     cx,0                    ;point to FAR ptr to __sysInit
        mov     bx,offset infile
        call    seg_off_seek    ;aim at the ptr to _main
        jnc     @@1
        jmp     seek_err
@@1:    mov     dx,offset warpcode+0b0h  ;don't overwrite _astart yet!
        mov     cx,20h          ;read 4 bytes (far pointer to __sysInit)
        mov     ah,3fh
        mov     bx,infile.filehandle
        int     21h             ;read the _main segment ptr to __sysInit
        jnc     @@2
        jmp     read_err

        ;--- _main code is now in warpbuffer, starting at offset +0
@@2:    cmp     bptr [warpcode+0b5h],02   ;5.01 value at _main+05 is 08
        je      @@02                      ;remember - buffer now starts @ +0b0h
        cmp     bptr [warpcode+0b5h],08   ;S87 has a value of 02 @ _main+05
        jne     @@01                      ;if not 02 or 08, there's a problem
        jmp     @@cl87
@@01:   jmp     valx_err                  ;"not a valid target file"


@@02:   mov     dx,wptr warpcode+1bh+0b0h
        mov     cx,wptr warpcode+1dh+0b0h      ;ptr to __sysInit
        add     dx,088h                 ; 0beh in R109 version
        adc     cx,0
        mov     bx,offset infile
        call    seg_off_seek    ;aim at the offset ptr to __sym_init
        jnc     @@3
        jmp     seek_err
@@3:    mov     dx,offset warpcode
        mov     cx,5            ;read 5 bytes (offset, seg ptrs to __sym_init)
        mov     ah,3fh
        mov     bx,infile.filehandle
        int     21h             ;read the ptrs to __sym_init
        jnc     @@4
        jmp     read_err
@@4:    mov     dx,wptr warpcode        ;offset ptr to __sym_init
        mov     cx,wptr warpcode+2      ;seg ptr to __sym_init
        add     dx,0ch                  ;aim at FAR ptr to __weed_eatr
        adc     cx,0
        mov     bx,offset infile
        call    seg_off_seek    ;aim at the offset ptr to __sym_init
        jnc     @@45
        jmp     seek_err
@@45:   mov     dx,offset warpcode
        mov     cx,4            ;read 4 bytes (FAR ptr to __weed_eatr)
        mov     ah,3fh
        mov     bx,infile.filehandle
        int     21h             ;read the FAR ptr to __weed_eatr
        jnc     @@5
        jmp     read_err
@@5:    mov     dx,wptr warpcode        ;offset ptr to __weed_eatr
        mov     cx,wptr warpcode+2      ;seg ptr
        add     dx,28h                  ;point to WEEDBED
        adc     cx,0
        mov     weedbed_ptr_hi,cx       ;save pointer for later touch-up
        mov     weedbed_ptr_lo,dx
        sub     dx,03                   ;aim back down at WEEDEND
        sbb     cx,0
        mov     bx,offset infile
        call    seg_off_seek    ;aim at the WEEDEND value
        jnc     @@6
        jmp     seek_err
@@6:    mov     dx,offset warpcode
        mov     cx,5            ;read 5 bytes (WEEDEND,xx,WEEDSTART)
        mov     ah,3fh
        mov     bx,infile.filehandle
        int     21h             ;read the ptrs to __sym_init
        jnc     @@7
        jmp     read_err
@@7:    mov     bx,wptr warpcode        ;WEEDEND
        mov     weedend,bx
        mov     ax,wptr warpcode+03     ;WEEDBED
        mov     weedbed,ax
        sub     bx,ax
        mov     weedlength,bx           ;SYMBOL COUNT
        mov     infile.sym_count,bx

        ;---calculate symstart_msw, lsw pointers relative to top of header
        xor     dx,dx
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1            ;multiply by 16 to convert seg addr to bytes
        mov     symstart_lsw,ax
        mov     symstart_msw,dx
        clc
        jmp     @@ret

;---Clipper S87 weedbed handler:    need CRT0, MAIN, SYMSYS, SEGJUNK
@@cl87: mov     bx,0a0h         ;_main far pointer is at _astart+0a0h
        mov     dx,wptr warpcode[bx]    ;offset
        mov     cx,wptr warpcode[bx+02] ;segment above .exe header for _main
        sub     dx,7fh          ;ptr to __sym_init is 7fh bytes before _main
        sbb     cx,0
        mov     bx,offset infile
        call    seg_off_seek    ;aim at the ptr to __sym_init
        jnc     @@_1
        jmp     seek_err
@@_1:   mov     dx,offset warpcode
        mov     cx,4            ;read 4 bytes (far pointer to __sym_init)
        mov     ah,3fh
        mov     bx,infile.filehandle
        int     21h             ;read the _main segment ptr to __sym_init
        jnc     @@_2
        jmp     read_err
@@_2:   mov     dx,wptr warpcode
        mov     cx,wptr warpcode+2      ;ptr to __sym_init
        add     dx,0ch
        adc     cx,0
        mov     bx,offset infile
        call    seg_off_seek    ;aim at the far pointer to __sym_count
        jnc     @@_3
        jmp     seek_err
@@_3:   mov     dx,offset warpcode
        mov     cx,4            ;read 4 bytes (far pointer to __sym_count)
        mov     ah,3fh
        mov     bx,infile.filehandle
        int     21h             ;read the symseg segment ptr to __sym_count
        jnc     @@_4
        jmp     read_err
@@_4:   mov     dx,wptr warpcode
        mov     cx,wptr warpcode+2      ;ptr to __sym_count
        add     dx,0dh                  ;point to SYMSTART value
        adc     cx,0
        mov     weedbed_ptr_hi,cx       ;save pointer for later touch-up
        mov     weedbed_ptr_lo,dx
        mov     bx,offset infile
        call    seg_off_seek    ;aim at the far pointer into __sym_count
        jnc     @@_5
        jmp     seek_err
@@_5:   mov     dx,offset warpcode
        mov     cx,8            ;read 8 bytes weedbed and weedend values
        mov     ah,3fh
        mov     bx,infile.filehandle
        int     21h             ;read the segjunk segment
        jnc     @@_6
        jmp     read_err
@@_6:   mov     ax,wptr warpcode        ;WEEDBED
        mov     weedbed,ax
        mov     bx,wptr warpcode+06     ;WEEDEND
        mov     weedend,bx
        sub     bx,ax
        mov     weedlength,bx           ;SYMBOL COUNT
        mov     infile.sym_count,bx

        ;---calculate symstart_msw, lsw pointers relative to top of header
        xor     dx,dx
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1            ;multiply by 16 to convert seg addr to bytes
        mov     symstart_lsw,ax
        mov     symstart_msw,dx
        mov     cl87_flag,1     ;set the S87 flag
        clc
@@ret:  ret

get_weedbed     endp




;  seg_off_seek   a modified fseek, for use with fileinfo structures, and
;                       expects to be passed seg in cx, offset in dx, and
;                       bx should equal the DGROUP offset of the struc. Will
;                       add in the header_paragraphs value from fileinfo struc.
;                       Returns carry clear if successful. Returns dx:ax
;                       NOT adjusted for header length. (i.e. absolute)
seg_off_seek    proc    near

        xor     ax,ax
        add     cx,ss:[bx.header_paras]
        shl     cx,1
        rcl     ax,1
        shl     cx,1
        rcl     ax,1
        shl     cx,1
        rcl     ax,1
        shl     cx,1
        rcl     ax,1    ;convert paragraphs in cx to bytes in ax:cx
        add     dx,cx   ;low in dx now
        adc     ax,0
        mov     cx,ax   ;cx = high word now, including the header length
        mov     bx,ss:[bx.filehandle]
        mov     ax,4200h
        int     21h                     ;returns ABSOLUTE position in dx:ax
        ret

seg_off_seek    endp


load_size       proc     near           ;display load size from exe header
                                        ; use ltoa into (sizestring+30)
                                        ;call with struc in bx, target in si
                                        ;note that imagesize (dd) is NOT saved.
        push    si
        push    si
        mov     cx,32                   ;convert to paragraphs initially
        mov     ax,[bx.length512]
        cmp     bx.[length_mod512],0
        je      @@1
        dec     ax                      ;the wierd fixup if mod512 > 0
@@1:    mul     cx                      ;result in ax = paragraphs
        add     ax,[bx.minimum_alloc]
        adc     dx,0
        sub     ax,[bx.header_paragraphs]
        sbb     dx,0                    ;dx HAS to be 0 after the borrow
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1                      ;multiply by 16 to convert to bytes
        add     ax,[bx.length_mod512]
        adc     dx,0
        mov     imagesize,ax            ;save the calculated value
        mov     imagesize+02,dx
        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa
        pop     di
        add     di,4                    ;skip the string header info
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        pop     si                      ;called with target offset
        call    showscreen
        ret

load_size       endp

;
;  show_paras()   display dx:ax paragraphs as bytes via line at ds:si
;
show_paras      proc    near

        push    si
        push    si
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1                      ;multiply by 16 to convert to bytes
        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa                    ;convert it into scratchpad buffer
        pop     di                      ;restore original destination address
        add     di,4                    ;skip the string header info
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        pop     si                      ;called with target offset
        call    showscreen
        ret

show_paras      endp


;  show_numarg()  dedicated support function. Call with number in dx:ax,
;       and the screen row# in di. This function calls ltoa, ltrim, and
;       showscreen to print the number right-justified at screenrow,
;       (LAST_COL - number-length), using the atttributes in "parmstring"
;
show_numarg     proc    near

        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa                    ;returns length in ax
        push    ax                      ;save a copy of the string length
        mov     dx,di
        mov     di,offset parmstring
        xchg    dl,dh                   ;put row# into dh
;        mov     dl,LAST_COL             ; equ last column + 1 for arg numbers
        sub     dl,al                   ;adjust back to r-justify it
        mov     wptr [di],dx        ;set up the row and column addres s
        add     di,04                   ;point to the start of the text
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        mov     si,offset parmstring
        call    showscreen
        pop     cx                      ;return the length in cx
        ret

show_numarg     endp

ltrim   proc    near                    ;expects ds:si, es:di, and cx set

        mov     cx,11                   ;  skips leading 0's or spaces
@@1:    lodsb
        cmp     al,0
        je      @@1
        cmp     al,' '
        jne     @@2
        loop    @@1                     ;loop thru the leading whitespace

@@2:    stosb                           ;handle the rest of the digits now
        cmp     al,0
        je      @@3
        lodsb
        loop    @@2
@@3:    dec     di                      ;return pointing to final byte (null)
        ret

ltrim   endp

ltoa    proc    near                    ;dx:ax 32 bit int into ds:si ascii
                                        ;  string of radix cx. From Ray Duncan
                                        ; in PC Mag Feb 29, 2988 p.371. Calls
        call    clearbuff               ; "div32" also fram same article.
        add     si,11                   ; Ds:si must be at least 11 bytes long
        push    si
        or      dx,dx
        pushf
        jns     ltoa1
        not     dx
        not     ax
        add     ax,1
        adc     dx,0

ltoa1:  call    div32
        add     bl,'0'
        cmp     bl,'9'
        jle     ltoa2
        add     bl,'A' - '9' - 1

ltoa2:  dec     si
        mov     [si],bl
        mov     bx,ax
        or      bx,dx
        jnz     ltoa1
        popf
        jns     ltoa3
        dec     si
        mov     byte ptr [si],'-'

ltoa3:  pop     ax
        sub     ax,si
        ret

ltoa    endp

clearbuff       proc    near   ;clear out the buffer before ltoa conversion

        push    ax
        push    cx
        push    si
        mov     cx,11
        xor     al,al
@@1:    mov     byte ptr [si], al
        inc     si
        loop    @@1
        pop     si
        pop     cx
        pop     ax
        ret

clearbuff       endp



div32   proc    near            ;dx:ax = 32 bit dividend, divide by cx
                                ;  return dx:ax = quotient, bx = remainder
        jcxz    div1
        push    ax
        mov     ax,dx
        xor     dx,dx
        div     cx
        mov     bx,ax
        pop     ax
        div     cx
        xchg    bx,dx

div1:   ret

div32   endp


check_for_d     proc    near    ;see if argv[3] = "d" or "D" (flag to save date)
				;  argv[0] = SP5
				;  argv[1] = target file name
				;  argv[2] = output name/first argument
				;
				;  returns carry CLEAR if /d or /!  else carry set

        push    ds                      ;arrive with ds = DGROUP
        pop     es                      ;es changed. Save DGROUP in es
	mov	ax,argc			;argument count
	dec	ax
	shl	ax,1			;as an index into argv[]
        lds     si,argv                 ;ds changed to argv-array segment
        add     si,ax                   ;ptr to last argv[] = flag?
        mov     si,[si]                 ;ds:si now points to argv[last]
@@0:    lodsb                           ;get first byte of argv[last]
        cmp     al,"/"
        je      @@0
        cmp     al,"-"
        je      @@0
;***        cmp     al,"!"
;***        jne     @@00
;***        mov     ss:kellyjoke,TRUE
;***        push    es
;***        pop     ds
;***	clc
;***        jmp     @@ret

@@00:   and     al,0dfh                 ;uppercase it
        cmp     al,"D"                  ;is it the D flag?
        push    es
        pop     ds                      ;restore ds
        je      @@1
;***		cmp	al,'N'			; is it no display flag
;***		je	@@2
	stc
	jc	@@ret
        ;;jmp     bad_command_err         ;syntax error on command line
@@1:    mov     date_flag,TRUE      ;set the flag if needed
	clc
@@ret:  ret

;***@@2:    mov     no_display_flag,TRUE      ;set the flag if needed
;***	clc
;***	ret

check_for_d     endp

get_proc1       proc    near            ;read proc1 into appcode buffer
        ;---fseek to the start of code (i.e. skip the header)
        xor     cx,cx
        mov     dx,inheader.header_paragraphs
        call    fseek
        jnc     @@0
        jmp     seek_err

        ;---allocate buffer and read fff0h bytes to buffer "appcode"
@@0:    mov     bx,0fffh
        mov     ah,48h
        int     21h
        jnc     @@1
        jmp     mem_err

@@1:    mov     infile.appcodeseg,ax    ;save the seg address of buffer.
        mov     bx,infile.filehandle
        mov     cx,0fff0h
        xor     dx,dx
        mov     ds,ax
        mov     ah,3fh
        int     21h                     ;read 100h bytes
        jnc     @@2
        push    es
        pop     ds
        jmp     read_err                ;direct error-jump out of function!
@@2:    ret

get_proc1       endp


; process_symbols()  copy and compact the symbol table, copy code and data too
;
process_symbols proc    near

        call    copy_old_symbols        ;copy and compact the symbol table
        call    fix_symbol_vectors      ;adjust down any that point above WEEDEND

        ;---open the output file(s) only if there will be a compaction
        mov     ax,outfile.sym_count
        cmp     ax,infile.sym_count
        jb      @@0                             ;jump if there is compaction

        call    close_files
        mov     si,offset noneed_msg    ;else abort if there is no compaction
        jmp     errmsg
@@0:    cmp     ax, 0fffh       ;abort if too many symbols after compaction
;***        jb      @@_01                       ;10/23/91 06:00pm is this necessary???
;***        call    close_files                 ; it causes SP5 to crap out on big files
;***        mov     si,offset bigsym_out_msg    ; with over 4095 symbols
;***        jmp     errmsg                      ; post-compaction.

@@_01:  call    open_outfile
        jnc     @@1
        jmp     open_err
@@1:    mov     outfile.filehandle, ax          ;save the handle
        ;---now write the header back out to the outfile
        push    ds
        mov     ax,headerbuffer
        mov     ds,ax
        mov     cx,es:outheader.header_paragraphs
        shl     cx,1
        shl     cx,1
        shl     cx,1
        shl     cx,1
        xor     dx,dx
        mov     bx,es:outfile.filehandle
        mov     ah,40h
        int     21h
        pop     ds
        jnc     @@00
        jmp     write_err

@@00:   cmp     warp_ovlmgr,TRUE        ;skip this stuff if non-overlayed
        jne     @@01
        call    show_overlay            ;display the overlay name on the screen
        call    open_overlay            ;open the ovl file
@@01:
        ;---now get on with the processing
        call    fix_pcode               ;adjust symbol references
        cmp     cl87_flag[bp],1
        jne     @@__0
        call    copy_new_symbols87      ;write the new symbols to disk
        jmp     @@__1
@@__0:  call    copy_new_symbols        ;write the new symbols to disk
@@__1:  call    copy_data               ;write data segment using code buffer
        call    fixup_header            ;copy the altered header+fixups

        ret

process_symbols endp


;---adjust down any symbol vectors (that point above WEEDEND) by GAP paragraphs.
;
;   this code added 04/11/91 01:51am  because of wierd user file with executable
;      code in data area ABOVE Clipper.lib data!
fix_symbol_vectors      proc    near

        push    es
        mov     dx,ss:gap               ;the adjustment (paragraphs)
        mov     bx,ss:weedend           ;top of symbol table
        mov     ax,ss:outfile.symseg    ;segment address of buffer
        mov     cx,ss:outfile.sym_count ;the symbol count to check
        mov     es,ax                   ;es = new symbol buffer segment address
        or      cx,cx
        je      @@ret                   ;just in case. should never happen.
        mov     di,0eh                  ;vector high word is at offset +0eh
@@top:  cmp     wptr es:[di],bx
        jbe     @@low                   ;don't bother it if it's below weedend
        sub     wptr es:[di],dx         ;fix it if necessary

@@low:  mov     ax,es
        inc     ax                      ;aim es at the next symbol, using ax
        mov     es,ax
        loop    @@top

@@ret:  pop     es
        ret

fix_symbol_vectors      endp



fixup_header    proc    near

     ;---calculate length as original header-length - gaplenhi is
     ;     necessary because of internal overlays, which we need to NOT
     ;     count as part of the .exe base length.

        mov     dx,baselength_msw
        mov     ax,baselength_lsw
        mov     cx,gap                  ;#paragraphs of compaction
        xor     bx,bx                   ;convert gap paragraphs to bx:cx bytes
        shl     cx,1
        rcl     bx,1
        shl     cx,1
        rcl     bx,1
        shl     cx,1
        rcl     bx,1
        shl     cx,1
        rcl     bx,1
        sub     ax,cx                   ;subtract  dx:ax - bx:cx
        sbb     dx,bx                   ;new shortened length in dx:ax
        mov     new_baselength_msw,dx       ;save the shortened length
        mov     new_baselength_lsw,ax

        ;---now divide dx:ax by 512 and also get remainder
; MED 07/12/93
;***        mov     cx,512
;***        call    div32                   ;div dx:ax by cx. Returns dx:ax and bx
;***        or      dx,dx
;***        je      @@2
;***        jmp     bigdx_err

; MED 07/12/93
		mov	cx,512
		div	cx
		mov	bx,ax
		mov	ax,dx
		xor	dx,dx

@@2:    or      bx,bx                   ;any Length_mod512 remainder?
        je      @@3
        inc     ax
@@3:    mov     si,ax
        mov     di,bx                   ;now di  = remainder, si = Length512
        push    ds
        mov     bx,offset outfile
        call    rewind                  ;rewind the outfile
        mov     ax,headerbuffer         ;pointer to header buffer
        mov     ds,ax

        ;---ds:0 now points to header. Need to adjust load-image downwards
        xor     bx,bx
        mov     [bx.length_mod512],di
        mov     [bx.length512],si

        ;---now overwrite the header in the outfile with the altered buffer copy
        mov     cx,wptr [bx.header_paragraphs] ;read this from the header itself
        shl     cx,1
        shl     cx,1
        shl     cx,1
        shl     cx,1                    ;convert to header length in bytes
        mov     bx,ss:outfile.filehandle
        xor     dx,dx
        mov     ah,40h
        int     21h                     ;overwrite the header in outfile
        jnc     @@4
        pop     ds
        jmp     write_err
@@4:
        xor     bx,bx                   ;ds still points to header segment
        mov     cx,32                   ;convert to paragraphs initially
        mov     ax,[bx.length512]
        cmp     bx.[length_mod512],0
        je      @@5
        dec     ax                      ;the wierd fixup if mod512 > 0
@@5:    mul     cx                      ;result in ax = paragraphs
        add     ax,[bx.minimum_alloc]
        adc     dx,0
        sub     ax,[bx.header_paragraphs]
        sbb     dx,0                    ;dx HAS to be 0 after the borrow
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1                      ;multiply by 16 to convert to bytes
        add     ax,[bx.length_mod512]
        adc     dx,0                    ;result in dx:ax now

        pop     ds                      ;restore ds and fix the stack
        mov     imagesize,ax            ;save the calculated value
        mov     imagesize+02,dx
        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa
        mov     di,offset outsizestring
        add     di,4                    ;skip the string header info
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        mov     si,offset outsizestring ;called with target offset
        call    showscreen
        ret

fixup_header    endp

rewind  proc    near    ;call with bx = data structure. Assume ds = dgroup

        xor     cx,cx
        xor     dx,dx
        mov     bx,ss:bx.filehandle
        mov     ax,4200h
        int     21h
        ret

rewind  endp


my_fseek proc    near    ;call with cx:dx, returns dx:ax hard-coded for infile
                         ;  and bx pointing to file structure

        push    bx                      ;save a copy of the struc pointer
        xor     ax,ax
        mov     bx,ss:bx.header_paras
        shl     bx,1
        rcl     ax,1
        shl     bx,1
        rcl     ax,1
        shl     bx,1
        rcl     ax,1
        shl     bx,1
        rcl     ax,1                    ;header bytes in ax:bx
        add     dx,bx
        adc     cx,ax                   ;add header to file position in cx:dx
        pop     bx
        mov     bx,ss:bx.filehandle
        mov     ax,4200h
        int     21h                     ;return absolute offset in dx:ax
        ret

my_fseek endp


; my_ftell()  returns file ptr adjusted to exclude header paragraphs.
;
my_ftell   proc    near            ;call with handle in bx, header para's in ax

        push    cx
        push    ax              ;header paragraphs
        mov     ax,4201h        ;fseek relative to current location
        xor     cx,cx
        xor     dx,dx
        int     21h             ;returns ABSOLUTE current displacement in dx:ax
        pop     bx
        shl     bx,1
        shl     bx,1
        shl     bx,1
        shl     bx,1            ;header bytes now in bx
        sub     ax,bx
        sbb     dx,0            ;subtract the header length
        mov     nextptr_msw[bp],dx          ;save the locations. ss: override
        mov     nextptr_lsw[bp],ax
        pop     cx
        ret

my_ftell   endp


;
;  fix_pcode is the main loop that calls pcode_waltz to process the pcode.
;               ds = dgroup on entry, but becomes the pcode buffer segment
;               uses [bp] for automatic ss: overriding of all dgroup data
;
fix_pcode       proc    near

        ;---adjust weedend to reflect symbol compression
        mov     ax,weedend
        sub     ax,gap
        mov     new_weedend,ax

        ;---rewind
       ; mov     bx,offset infile        ;point to the data structure
       ; call    rewind
        mov     bx,offset infile
        xor     dx,dx
    ;    mov     cx,infile.header_paras
        xor     cx,cx
        call    seg_off_seek            ;aim at codestart
        mov     buffstartptr_msw,0      ;where the buffer starts in the file
        mov     buffstartptr_msw,0

        ;---allocate a code buffer
        IFDEF VERBOSE
        call    read_message
        ENDIF
        mov     bx,BUFFER_PARA
        mov     ah,48h
        int     21h                     ;allocate a 64K buffer
        jnc     @@1
        jmp     mem_err
@@1:
        mov     infile.appcodeseg,ax    ;record the buffer segment
        mov     bx,infile.filehandle

        ;---read first bufferful of code
        xor     bp,bp                   ;just in case
        mov     ds,ax
        mov     ah,3fh
        mov     cx,READ_SIZE            ;NORMALLY A BUFFERFUL.
        mov     dx,0
        int     21h                     ;read a chunk of code
        jnc     @@2
        mov     ax,@data
        mov     ds,ax
        jmp     read_err
@@2:
        mov     es:last_read,ax         ;save the number of bytes actually read
        mov     bx,es:infile.filehandle
        mov     ax,es:infile.header_paras
        call    my_ftell                ;sets nextptr_msw,_lsw for infile
        mov     ax,es:infile.symseg
        mov     es,ax
        xor     di,di                   ;set up old symbols in es:di

        ;---ds and es now altered! Enter proc-search loop
        xor     si,si                   ;start search at buffer:0000
@@seek0: cmp     cl87_flag[bp],1
        jne     @@__s0
        call    brute87                 ;carry = not_found, else ds:si = match
        jmp     @@__s1
@@__s0: call    brute                   ;carry = not_found, else ds:si = match
@@__s1: jmatch  @@3
        jmp     @@5
@@3:    mov     temp[bp],si             ;maybe use a register store later?
        add     si,7                    ;be sure we don't re-match the same one
@@seek1: cmp     cl87_flag[bp],1
        jne     @@__s2
        call    brute87                 ;carry = not_found, else ds:si = match
        jmp     @@__s3
@@__s2: call    brute                   ;carry = not_found, else ds:si = match
@@__s3: jmatch  @@4
        cmp     temp[bp],0              ;was first-match at start of buffer?
        je      @@4                     ;process it anyway if it was...
        jmp     @@6
@@4:    mov     tempnext[bp],si
        mov     si,temp[bp]             ;go back to the original match

        ;---now process the pcode in-situ in buffer
        cmp     cl87_flag[bp],1
        jne     @@_40
        call    pcode87_waltz           ;fix buffer at ds:si. es:di = symbols
        jmp     @@_41
@@_40:  call    pcode_waltz             ;fix buffer at ds:si. es:di = symbols

        ;---if valid content in tempnext, THAT becomes the current pointer
@@_41:  mov     ax,tempnext[bp]
        cmp     ax,0ffffh               ;-1 means NO next proc found
        je      @@5                     ;write the buffer, and read next chunk
        mov     temp[bp],ax             ;update this to current ptr store
        mov     si,ax
        jmp     @@seek0

@@5:
        ;---get here if search came up empty. write the buffer
        mov     cx,last_read[bp]
        cmp     pcode_finished[bp],TRUE
        je      @@501                   ;don't subtract on the last buffer
        sub     cx,0bh                  ;watch out for an overlap!!!
@@501:
        IFDEF VERBOSE
        call    fixup_message
        ENDIF
        call    code_fixups             ;fixup the code about to be written

        ;---write the buffer to disk
        IFDEF VERBOSE
        call    write_message
        ENDIF
        xor     dx,dx                   ;buffer starts at ds:0000
        mov     cx,last_read[bp]
        cmp     pcode_finished[bp],TRUE
        je      @@51                    ;don't subtract on the last buffer
        sub     cx,0bh                  ;watch out for an overlap!!!
        cmp     straddle[bp],FALSE
        je      @@51
        dec     cx                      ;just in case a pointer was split
@@51:   mov     bx,ss:outfile.filehandle
        mov     ah,40h
        int     21h
        jnc     @@55
        push    ss
        pop     ds
        jmp     read_err

        ;---infile fpointer is at start next chunk, but we need to back up 0bh
@@55:   xor     cx,cx
        xor     dx,dx
        cmp     pcode_finished[bp],TRUE ;don't subtract on last buffer
        je      @@5_5
        sub     dx,0bh
        sbb     cx,0                    ;set up for -0bh fpointer movement
        cmp     straddle[bp],FALSE
        je      @@5_5
        sub     dx,01                   ;in case a pointer was split
        sbb     cx,0
@@5_5:  mov     bx,ss:infile.filehandle
        mov     ax,4201h
        int     21h                     ;returns ABSOLUTE offset in dx:ax
        ;jnc     @@56
        jnc     @@7                     ;go do the read, fix ptrs, and loop
        push    ss
        pop     ds
        jmp     seek_err
        ;---now do the read to the buffer
;@@56:   ;mov     cx,READ_SIZE
        ;xor     dx,dx
        ;mov     bx,ss:infile.filehandle
        ;mov     ax,3fh
        ;int     21h
        ;jnc     @@8                     ;fix pointers and loop to process
        ;push    ss
        ;pop     ds
        ;jmp     read_err

@@6:
        ;---get here if second search failed to find a NEXT procedure
        ;---else write the buffer up to the start of current proc.
        IFDEF VERBOSE
        call    fixup_message
        ENDIF
        mov     cx,temp[bp]
        call    code_fixups             ;fixup the code about to be written

        ;---write the buffer to disk
        IFDEF VERBOSE
        call    write_message
        ENDIF
        xor     dx,dx                   ;buffer starts at ds:0000
        mov     cx,temp[bp]
        cmp     straddle[bp],FALSE
        je      @@61
        dec     cx                      ;in case a pointer was split
@@61:   mov     bx,ss:outfile.filehandle
        mov     ah,40h
        int     21h
        jnc     @@65
        push    ss
        pop     ds
        jmp     read_err

        ;---re-set infile pointer to start of current proc
@@65:   mov     bx,ss:infile.filehandle
        mov     ax,ss:infile.header_paras
        call    my_ftell                ;returns rel position in dx:ax
        sub     ax,ss:last_read
        sbb     dx,0                    ;calculate previous buffer start
        mov     cx,dx
        mov     dx,ax                   ;set up the regs properly
        add     dx,temp[bp]
        adc     cx,0                    ;buffer start offset for this proc
        cmp     straddle[bp],FALSE
        je      @@66
        sub     dx,01
        sbb     cx,0                    ;in case a pointer was split
@@66:   mov     bx,offset ss:infile
        call    my_fseek                ;allows for header. Returns ABS dx:ax
        jnc     @@7
        push    ss
        pop     ds
        jmp     seek_err

        ;---read bufferful/up to start of symbols (set flag if symbols reached)
        ;     subtract procstart from symstart to see if there's more than one
        ;     bufferful left to read before symbols.
@@7:    xor     si,si
        xor     cx,cx
        xor     bx,bx
        mov     bx,ss:infile.header_paras
        shl     bx,1
        rcl     cx,1
        shl     bx,1
        rcl     cx,1
        shl     bx,1
        rcl     cx,1
        shl     bx,1
        rcl     cx,1                    ;header bytes
        sub     ax,bx
        sbb     dx,cx                   ;subtr header from absolute file postn
@@75:   mov     cx,symstart_msw[bp]     ;procptr returned from fseek in dx:ax
        mov     bx,symstart_lsw[bp]
        mov     buffstartptr_msw[bp],dx ;file location where buffer starts
        mov     buffstartptr_lsw[bp],ax ;  relative to header
    ;IFDEF DEBUG
    ;    call    buffstart_prn           ;print the buffer start address
    ;ENDIF
        sub     bx,ax                   ;lsw
        sbb     cx,dx                   ;msw
        or      cx,cx                   ;over 64K ?
        jne     @@toobig
        cmp     bx,READ_SIZE            ;the buffer size in bytes
        ja      @@toobig
        cmp     bx,0
        jbe     @@ret
        mov     cx,bx                   ;read cx bytes
        mov     pcode_finished[bp],TRUE ;set flag to exit loop
        jmp     @@readit
@@toobig:                               ;just read a buffer full
        mov     cx,READ_SIZE
@@readit:
        IFDEF VERBOSE
        call    read_message
        ENDIF
        ;IFDEF DEBUG
        ;call    readbytes_prn           ;print the #bytes to read
        ;ENDIF
        xor     dx,dx                   ;read to ds:dx
        mov     bx,ss:infile.filehandle
        mov     ah,3fh
        int     21h
        jnc     @@8
        push    ss
        pop     ds
        jmp     read_err
@@8:    mov     last_read[bp],ax         ;save the number of bytes actually read
        mov     bx,ss:infile.filehandle
        mov     ax,ss:infile.header_paras
        call    my_ftell                ;sets nextptr_msw,_lsw for infile
        cmp     last_read[bp],0         ;have we reached symbols yet?
        je      @@ret                   ;then EXIT THE LOOP
        xor     si,si
        jmp     @@seek0


        ;---restore ds, and go home
@@ret:  mov     ax,@data
        mov     ds,ax
        mov     es,ax
        IFDEF VERBOSE
        call    blank_message
        ENDIF
        ret

fix_pcode       endp


;  code_fixups   scans the header relocation table for any addresses that
;                       convert to longs that would point within the portion
;                       of the buffer that is about to be written. Adjusts the
;                       segment reference that they point to, if it would fall
;                       higher than WEEDEND (i.e. top of symbols). The # bytes
;                       to write from the buffer is cx, and buffer starts at 0.
;
;   this routine returns carry set to indicate the need to shift buffer limit
;     by one byte! Also sets DGROUP flag "straddle" to indicate the same thing.
;

;
code_fixups     proc    near

        ;IFDEF DEBUG
        ;call    bytes_prn       ;print the # bytes to write
        ;ENDIF
        push    di
        push    si
        push    cx              ;#bytes in buffer to process
        push    es              ;the symbol buffer segment
        mov     straddle[bp],FALSE  ;clear the straddle flag
        or      cx,cx           ;trap zero-size buffer
        jne     @@0
        jmp     @@done
@@0:    mov     bufferbytes[bp],cx
        dec     cx
        xor     dx,dx
        add     cx,buffstartptr_lsw[bp]
        adc     dx,buffstartptr_msw[bp] ;calculate the top of the buffer
        mov     buffendptr_lsw[bp],cx
        mov     buffendptr_msw[bp],dx

        ;---set up the exe header
        mov     ax,headerbuffer[bp]     ;pointer to header buffer
        mov     es,ax
        mov     di,ss:inheader.first_reloc_offset  ;start here at es:di
BUG1:   mov     cx,ss:inheader.relocation_item_count  ;cx = item count
;        xor     bx,bx
;        mov     cx,[bx.relocation_item_count]  ;cx = item count

        ;---process all fixups in a loop
@@top:  mov     bx,wptr es:[di]
        mov     ax,wptr es:[di+02]      ;first relocation item

        ;---convert to a long displacement. This is ABOVE the header
        xor     dx,dx
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        add     ax,bx
        adc     dx,0                    ;the fixup pointer as a long in dx:ax

        ;---is dx:ax below or equal to buffendptr? If equal, set Straddle flag
        mov     tempax[bp],ax           ;save the values
        mov     tempdx[bp],dx
        sub     ax,buffendptr_lsw[bp]
        sbb     dx,buffendptr_msw[bp]
        jnc      @@get_next              ;jump if too high
        jnz     @@001
        mov     straddle[bp],TRUE       ;if straddling top of active buffer
@@001:
        mov     ax,tempax[bp]           ;retrieve the values
        mov     dx,tempdx[bp]
        ;---is dx:ax greater or equal to buffstartptr?
        sub     ax,buffstartptr_lsw[bp]
        sbb     dx,buffstartptr_msw[bp] ;subtract bufferstart from this ptr
        jc      @@get_next                ;can't be < 0
        or      dx,dx                   ;must be same msw value --> dx = 0
        jne     @@get_next
        cmp     ax,bufferbytes[bp]      ;ax is displacement above bufferstart
        jae     @@get_next                ;too high?

        ;---debugging
       ; mov     si,ax
       ; call    demon1

        ;---if we got here, the reloc ptr's target is within the active buffer
@@1:    mov     si,ax                   ;ax is the buffer offset value
        mov     ax,wptr [si]            ;seg value for fixup
         ;cmp     ax,new_weedend[bp]
        cmp     ax,weedend[bp]
         ;cmp     ax,weedbed[bp]
        jb      @@get_next                ;don't need to adjust if below symbols
        sub     ax,gap[bp]              ;"gap" is the amount of compaction
        jc      @@err                   ;must be positive value
        IFNDEF  NOFIXUPS
        mov     [si],wptr ax            ;rewrite the seg value reduced by GAP
        ENDIF

@@get_next:
        add     di,04                   ;point to next fixup item.
        loop    @@top                   ;process all the fixups

@@done: pop     es
        pop     cx
        pop     si
        pop     di
        clc
        cmp     straddle[bp],FALSE
        je      @@ret
        stc                             ;flag to extend buffer one byte
@@ret:  ret                             ;  to avod splitting a straddling ptr.

@@err:
        mov     ax,@data
        mov     ds,ax
        mov     es,ax
        jmp     fatal_gap_err

code_fixups     endp

demon1  proc    near    ;checks to see if this is the DGROUP fixup at _astart
        cmp     bptr [si-07],3ch
        jne     @@1
        cmp     bptr [si-06],02h
        jne     @@1
        cmp     bptr [si-05],73h
        jne     @@1
        cmp     bptr [si-04],02h
        jne     @@1
        cmp     bptr [si-01],0bfh
        jne     @@1
        call    demon2
@@1:    ret
demon1  endp

demon2  proc    near
        nop                     ;put a breakpoint here!
        ret
demon2  endp


read_message    proc    near
        push    si
        push    cx
        mov     si,offset ss:mssg_read
        call    showscreen
        mov     cx,3000                 ;duration
        mov     di,2000                 ;pitch
        call    sound
        pop     cx
        pop     si
        ret
read_message    endp

write_message    proc    near
        push    si
        push    cx
        mov     si,offset ss:mssg_write
        call    showscreen
        mov     cx,3000                 ;duration
        mov     di,800                 ;pitch
        call    sound
        pop     cx
        pop     si
        ret
write_message    endp

fixup_message    proc    near
        push    si
        push    cx
        mov     si,offset ss:mssg_fixup
        call    showscreen
        mov     cx,3000                 ;duration
        mov     di,1500                 ;pitch
        call    sound
        pop     cx
        pop     si
        ret
fixup_message    endp

blank_message    proc    near
        push    si
        mov     si,offset ss:mssg_blank
        call    showscreen
        pop     si
        ret
blank_message    endp


;
; brute()   brute-force search for proc headers in buffer at ds:si.
;           length to search is in dgroup:last_read
;           preserves es:di. returns ds:si pointing to start of plankton-header
;               if found, or else returns carry SET and si = ffff to indicate
;               no match.
;
;           Also... the pointer to symbols in the plankton-header must be
;               changed to point to weedbed for all procedures. At +0bh
;
brute   proc    near

        push    es
        push    di
        push    ds
        pop     es
        mov     di,si           ;aim es:di at the buffer
        mov     cx,last_read[bp]  ;automatic ss: override
        sub     cx,si           ;only search what's left of the buffer
@@top:  mov     al,[cl_plankhead][bp]     ;start of the stable signature
 repne  scasb
        je      @@1
        mov     si,0ffffh       ;"not found" flag
        stc                     ;not found. return carry set. si is unchanged.
        jc      @@done
@@1:    mov     al,es:[di]      ;di pointing to byte AFTER first matched chr
        cmp     al,[bp][cl_plankhead+01]
        jne     @@top
        cmp     bptr es:[di-04],0bbh  ;check the isolated lead-in bytes too
        jne     @@top
        cmp     bptr es:[di-07],0b8h  ;check the isolated lead-in bytes too
        jne     @@top
        mov     dx,di           ;temp save of di
        sub     di,01           ;point es:di to start of signature to match
        push    cx
        mov     cx,8            ;length of signature
        xor     bx,bx           ;index with bx
@@2:    mov     al,bptr es:[di]
        cmp     al,ss:[cl_plankhead][bx]    ;the char-by-char comparison
        jne     @@3
        inc     di
        inc     bx
        loop    @@2
        sub     di,0eh          ;if you get here, it was a match
        mov     si,di           ;return the matched offset in si
        ;---fix the pointer to symbols at offset +0bh
   ;     mov     ax,weedbed[bp]  ;_SYMSTART
   ;     mov     wptr [si+0bh],ax  ;make the alteration

        clc
        pop     cx
        jmp     @@done

@@3:    pop     cx
        mov     di,dx           ;restore from our bookmark
        jmp     @@top           ;loop to resume search

@@done: pop     di
        pop     es
        ret

brute   endp

brute87 proc    near

        push    es
        push    di
        push    ds
        pop     es
        mov     di,si           ;aim es:di at the buffer
        mov     cx,last_read[bp]  ;automatic ss: override
        sub     cx,si           ;only search what's left of the buffer
@@top:  mov     al,[cl87_plankhead+04][bp]     ;most unique character of signature
 repne  scasb
        je      @@1
        mov     si,0ffffh       ;"not found" flag
        stc                     ;not found. return carry set. si is unchanged.
        jc      @@done
@@1:    mov     al,es:[di]      ;di pointing to byte AFTER first matched chr
        cmp     al,[bp][cl87_plankhead+05]
        jne     @@top
        mov     dx,di           ;temp save of di
        sub     di,05           ;point es:di to start of signature to match
        push    cx
        mov     cx,11           ;length of signature
        xor     bx,bx           ;index with bx
@@2:    mov     al,bptr es:[di]
        cmp     al,ss:[cl87_plankhead][bx]    ;the char-by-char comparison
        jne     @@3
        inc     di
        inc     bx
        loop    @@2
        sub     di,11           ;if you get here, it was a match
        mov     si,di           ;return the matched offset in si
        ;---fix the pointer to symbols at offset +0bh
   ;     mov     ax,weedbed[bp]  ;_SYMSTART
   ;     mov     wptr [si+0bh],ax  ;make the alteration

        clc
        pop     cx
        jmp     @@done

@@3:    pop     cx
        mov     di,dx           ;restore from our bookmark
        jmp     @@top           ;loop to resume search

@@done: pop     di
        pop     es
        ret

brute87 endp


;
; pcode_waltz   steps through pcode, fixes symbol references.  Also adjusts
;                        the plankton header symbol-table pointer.
;
;               call with       ds:si pointing to plankton-header in code buffer
;                               es:0 = old symbol table (di = 0 = symbol[0])
;                               ss = DGROUP
;               returns         carry clear = success
;                                 ax = pcode length (bytes)
;                               carry set = error
;                                 ax = error offset (i.e. partial pcode length)
;
;
;               09/22/90 05:02pm  modified to use segment-addressing for symbols
;

pcode_waltz     proc    near    ;si+0bh = symbol pointer

        push    es
        mov     ax,wptr [si+04h] ;segment address for this chunk of symbols
        sub     ax,weedbed[bp]   ;seg addr for start of entire symbol table
        mov     bx,es
        add     ax,bx
        mov     es,ax           ;es now = segment address for this start
        mov     di,0            ;always 0 offset - Using segment addressing!
        mov     ax,weedbed[bp]  ;_SYMSTART
        mov     wptr [si+04h],ax  ;make the alteration
        add     si,16h          ;pcode starts 16h into the proc
        push    si              ;save the pcodestarting point
        xor     bp,bp
@@top:  xor     bx,bx
        mov     bl,[si]         ;pcode now in bl
        cmp     bl,60h          ;60h is the return opcode
        je      @@done
        cmp     bl,01h          ;asciiz opcode. followed by string length
        je      @@asciiz
        test    bl,11100000b    ;weed out most of the codes
        jne     @@no_need       ;jump if can't possibly need fixup
        cmp     bl,06h
        jb      @@no_need
        cmp     bl,13h
        jbe     @@fix_it

      ;  NAHHH! Didn't help!!!
      ;  cmp     bl,23h           ;"_0PUSHBL"
      ;  je      @@fix_it


                                ;all others fall through to here anyway
@@no_need:
        shl     bx,1            ;make it an offset into table of values
        mov     dx,si           ;save current pcode pointer
        mov     bx,ss: opcode_table[bx] ;use of [bp] forces ss: override
        xor     bh,bh
        add     si,bx
        cmp     si,dx
        je      @@bad_op        ;bad op codes have 0 values in table
        jmp     @@top           ;loop for next

@@bad_op:
        pop     cx              ;fix the stack
        stc
        jc      @@ret           ;return carry set for bad opcode

@@done: pop     ax              ;original offset was aved as "push si"
        sub     ax,si
        neg     ax
        inc     ax              ;the pcode length return-value
        pop     es              ;saved on entry
        clc
@@ret:  ret

@@fix_it:
        push    es              ;save symbol start
        push    ax
        mov     bx,[si+01]      ;old symbol reference into bx
        mov     ax,es
        add     ax,bx           ;16 bytes per symbol in symbol table
        mov     es,ax           ;bump the segment, keep di = 0
        xor     bx,bx           ;offset's been taken care of in the es register.
        mov     bx,es: [di][bx.symbol_nulls]  ;the corrected symbol value
     ;;   and     bh,00111111b    ;mask off the shifted type-bits
        mov     wptr [si+01],bx ;fix up the symbol reference
        add     si,03           ;bump pointer by three bytes
        pop     ax
        pop     es              ;restore symbol start segment address
        jmp     @@top

@@asciiz:
        xor     dx,dx           ;can't get byte ptr directly into si
        mov     dl,bptr [si+01] ;asciiz length
        add     si,dx
        add     si,3            ;skip the 01, length, and null bytes
        jmp     @@top

pcode_waltz     endp

pcode87_waltz     proc    near    ;si+0bh = symbol pointer

        push    es
        mov     ax,wptr [si+0bh] ;segment address for this chunk of symbols
        sub     ax,weedbed[bp]   ;seg addr for start of entire symbol table
        mov     bx,es
        add     ax,bx
        mov     es,ax           ;es now = segment address for this start
        mov     di,0            ;always 0 offset - Using segment addressing!
        mov     ax,weedbed[bp]  ;_SYMSTART
        mov     wptr [si+0bh],ax  ;make the alteration
        add     si,1dh          ;pcode starts 1dh into the proc
        push    si              ;save the pcodestarting point
        xor     bp,bp
@@top:  xor     bx,bx
        mov     bl,[si]         ;pcode now in bl
        cmp     bl,34h          ;34 is the return opcode
        je      @@done
        cmp     bl,97h          ;asciiz opcode. followed by string length
        je      @@asciiz
        test    bl,96h          ;weed out most of the codes
        jz      @@no_need       ;jump if can't possibly need fixup
        cmp     bl,96h
        je      @@fix_it
        cmp     bl,9ah
        je      @@fix_it
        cmp     bl,9ch
        je      @@fix_it
        cmp     bl,9eh
        je      @@fix_it
        cmp     bl,0d8h
        je      @@fix_it
                                ;all others fall through to here anyway
@@no_need:
        shl     bx,1            ;make it an offset into table of values
        mov     dx,si           ;save current pcode pointer
        mov     bx,ss: opcode87_table[bx] ;use of [bp] forces ss: override
        xor     bh,bh
        add     si,bx
        cmp     si,dx
        je      @@bad_op        ;bad op codes have 0 values in table
        jmp     @@top           ;loop for next

@@bad_op:
        pop     cx              ;fix the stack
        stc
        jc      @@ret           ;return carry set for bad opcode

@@done: pop     ax              ;original offset was aved as "push si"
        sub     ax,si
        neg     ax
        inc     ax              ;the pcode length return-value
        pop     es              ;saved on entry
        clc
@@ret:  ret

@@fix_it:
        push    es              ;save symbol start
        push    ax
        mov     bx,[si+01]      ;old symbol reference into bx
        mov     ax,es
        add     ax,bx           ;16 bytes per symbol in symbol table
        mov     es,ax           ;bump the segment, keep di = 0
        xor     bx,bx           ;offset's been taken care of in the es register.
        mov     bx,es: [di][bx.symbol_nulls]  ;the corrected symbol value
        mov     wptr [si+01],bx ;fix up the symbol reference
        add     si,03           ;bump pointer by three bytes
        pop     ax
        pop     es              ;restore symbol start segment address
        jmp     @@top

@@asciiz:
        xor     dx,dx           ;can't get byte ptr directly into si
        mov     dl,bptr [si+01] ;asciiz length
        add     si,dx
        add     si,3            ;skip the 97, length, and null bytes
        jmp     @@top

pcode87_waltz     endp


;
;  copy_new_symbols()   appends the new symbol table onto the output file
;                         without initially repositioning the pointer
;
copy_new_symbols        proc    near

        push    ds
        mov     cx,outfile.sym_count    ;symbol count
        mov     ax,outfile.symseg
        mov     ds,ax                   ;ds points to start of symbols

@@top:  cmp     cx,0fffh                ;max 0fffh paragraphs at once
        jbe     @@copyall               ;the loop exit!
        mov     bx,cx                   ;save the count in bx
        sub     bx,0fffh                ;adjust it for the upcoming write
        mov     cx,0fffh                ;so copy a 0fffh paragraph-sized chunk
        call    copysym
        cmp     ax,cx                   ;no excuse for an incomplete write!
        je      @@0
        mov     ax,@data
        mov     ds,ax
        jnc     @@0
        jmp     write_err

@@0:    mov     cx,bx                   ;remaining paragraph count
        jmp     @@top                   ;loop as necessary

@@copyall:
        call    copysym

@@ret:  pop     ds                      ;restore ds
        ret                             ;return after all copying completed

copy_new_symbols        endp


;
;  copy_new_symbols87()   appends the new symbol table onto the output file
;                         without initially repositioning the pointer
;
copy_new_symbols87        proc    near

        push    ds
        mov     cx,outfile.sym_count    ;symbol count
        mov     ax,outfile.symseg
        mov     ds,ax                   ;ds points to start of symbols

@@top:  cmp     cx,0fffh                ;max 0fffh paragraphs at once
        jbe     @@copyall               ;the loop exit!
        mov     bx,cx                   ;save the count in bx
        sub     bx,0fffh                ;adjust it for the upcoming write
        mov     cx,0fffh                ;so copy a 0fffh paragraph-sized chunk
        call    copysym
        cmp     ax,cx                   ;no excuse for an incomplete write!
        je      @@0                     ; BUG 3/7/91:  copysym was trashing ax
@@axcx: mov     ax,@data                ; this caused write_err if symbols > 64k
        mov     ds,ax
       ;;;; jnc     @@0                 ;BUG Set carry is checked in copysym.
        jmp     write_err

@@0:    mov     ax,ds                   ;we copy from ds:0000, so increase ds
        shr     cx,1                    ;  by the number of paragraphs we wrote
        shr     cx,1                    ;
        shr     cx,1                    ;convert cx back to PARAGRAPHS first!
        shr     cx,1                    ;
        add     ax,cx                   ;
        mov     ds,ax
        mov     cx,bx                   ;remaining paragraph count
        jmp     @@top                   ;loop as necessary

@@copyall:
        call    copysym
        cmp     ax,cx
        jne     @@axcx                  ;error if incomplete write

@@ret:  pop     ds                      ;restore ds
        ret                             ;return after all copying completed

copy_new_symbols87        endp



; copysym()    support function for copy_new_symbols()    preserves bx
;
copysym         proc    near    ;cx = # paragraphs to copy

        push    bx
        shl     cx,1                    ;convert paragraphs to bytes
        shl     cx,1
        shl     cx,1
        shl     cx,1
        mov     bx,ss:outfile.filehandle
        xor     dx,dx                   ;copy from ds:dx   (ds:0000)
        mov     ah,40h
        int     21h                     ;write the symbols
        jnc     @@0                     ;die if there was a DOS write error
        mov     ax,@data
        mov     ds,ax
        jmp     write_err
@@0:    pop     bx
        ret                             ;on return, ax == cx == # bytes written

copysym         endp



;
;  copy_data()  page-to-disk copy routine that uses code buffer RAM space.
;                  to copy from weedend through eof to the output file.
;               Note that in the even of an internal overlay, this will
;                  ALSO copy the UNPACKED overlay, which must be altered
;                  subsequently!  Any additional data appended to the end
;                  of the .exe file will also be transferred.
;
copy_data       proc    near

        ;---tell 'em we're here
        mov     si,offset mssg_datacopy
        call    showscreen

        ;---set the infile pointer to the end of symbols
        mov     bx,offset infile
        xor     dx,dx
        mov     cx,weedend              ;weedend is the _symend variable
        call    seg_off_seek            ;seeks above hdr, returns dx:ax ABSOLUTE
        jnc     @@top
        jmp     seek_err

        ;---read/write loop. tests for partial or 0 read as exit condition.
@@top:  mov     si,TRUE                 ;use si as the flag to exit loop
        mov     ax,ss:infile.appcodeseg    ;buffer segment allocated in fix_pcode()
        mov     ds,ax
@@0:    cmp     si,TRUE
        jne     @@done

        ;---get relative file position of the start of the buffer
        mov     bx,ss:infile.filehandle
        mov     ax,ss:infile.header_paras
        call    my_ftell                ;returns RELATIVE file position (long)
        mov     buffstartptr_msw[bp],dx ;accessed later by data_fixups
        mov     buffstartptr_lsw[bp],ax
        mov     bx,ss:infile.filehandle
        xor     dx,dx
        mov     cx,READ_SIZE
        mov     ah,3fh                  ;read to buffer
        int     21h                     ;returns #bytes read in ax
        jnc     @@01
        mov     ax,@data
        mov     ds,ax
        jmp     read_err

@@01:   or      ax,ax                   ;make sure its not 0 = 0!!!
        je      @@exit
        cmp     ax,cx                   ;are we done yet?
        je      @@1
@@exit: mov     si,FALSE
@@1:    mov     cx,ax                   ;#bytes to write = #bytes read

        call    data_fixups             ;cx = # active bytes in buffer @ ds:0
        jnc     @@15                    ;carry set means (straddle == TRUE)
        dec     cx
@@15:   xor     dx,dx
        mov     bx,ss:outfile.filehandle
        mov     ah,40h
        int     21h
        jnc     @@2
        mov     ax,@data
        mov     ds,ax
        jmp     write_err
@@2:    ;---if straddle[bp] == TRUE, back up infile ptr one byte
        cmp     straddle[bp],FALSE
        je      @@2_5                   ;no need for this if ptr is not split.
        xor     cx,cx
        xor     dx,dx
        sub     dx,01                   ;in case a pointer was split
        sbb     cx,0
        mov     bx,ss:infile.filehandle
        mov     ax,4201h                ;back up 1 byte if necessary
        int     21h                     ;returns ABSOLUTE offset in dx:ax
        jnc     @@2_5
        push    ss
        pop     ds
        jmp     seek_err
@@2_5:  jmp     @@0                     ;loop for more

@@done: mov     ax,@data
        mov     ds,ax
        ret

copy_data       endp

data_fixups     proc    near    ;functionally similar to code_fixups


        ;IFDEF DEBUG
        ;call    bytes_prn       ;print the # bytes to write
        ;ENDIF
        push    di
        push    si
        push    cx              ;#bytes in buffer to process
        push    es              ;the symbol buffer segment
        mov     straddle[bp],FALSE  ;clear the straddle flag
        or      cx,cx           ;trap zero-size buffer
        jne     @@0
        jmp     @@done
@@0:    mov     bufferbytes[bp],cx
        dec     cx
        xor     dx,dx
        add     cx,buffstartptr_lsw[bp]
        adc     dx,buffstartptr_msw[bp] ;calculate the top of the buffer
        mov     buffendptr_lsw[bp],cx
        mov     buffendptr_msw[bp],dx

        ;---set up the exe header
        mov     ax,headerbuffer[bp]     ;pointer to header buffer
        mov     es,ax
        mov     di,ss:inheader.first_reloc_offset  ;start here at es:di
        mov     cx,ss:inheader.relocation_item_count  ;cx = item count

        ;---process all fixups in a loop
@@top:  mov     bx,wptr es:[di]
        mov     ax,wptr es:[di+02]      ;first relocation item

        ;---convert to a long displacement. This is ABOVE the header
        xor     dx,dx
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        add     ax,bx
        adc     dx,0                    ;the fixup pointer as a long in dx:ax

;        ;---is dx:ax below or equal to buffendptr? If equal, set Straddle flag
;        cmp     dx,buffendptr_msw[bp] ;subtract bufferstart from this ptr
;        ja      @@get_next
;        cmp     ax,buffendptr_lsw[bp]
;        ja      @@get_next
;        jb      @@001
;        mov     straddle[bp],TRUE       ;if straddling top of active buffer
;@@001:


        ;---is dx:ax below or equal to buffendptr? If equal, set Straddle flag
        mov     tempax[bp],ax           ;save the values
        mov     tempdx[bp],dx
        sub     ax,buffendptr_lsw[bp]
        sbb     dx,buffendptr_msw[bp]
        jnc      @@get_next              ;jump if too high
        jnz     @@001
        mov     straddle[bp],TRUE       ;if straddling top of active buffer
@@001:
        mov     ax,tempax[bp]           ;retrieve the values
        mov     dx,tempdx[bp]

        ;---is dx:ax greater or equal to buffstartptr?
        sub     ax,buffstartptr_lsw[bp]
        sbb     dx,buffstartptr_msw[bp] ;subtract bufferstart from this ptr
        jc      @@get_next                ;can't be < 0
        or      dx,dx                   ;must be same msw value --> dx = 0
        jne     @@get_next
        cmp     ax,bufferbytes[bp]      ;ax is displacement above bufferstart
        jae     @@get_next                ;too high?

        ;---if we got here, the reloc ptr's target is within the active buffer
@@1:    mov     si,ax                   ;ax is the buffer offset value
        mov     ax,wptr [si]            ;seg value for fixup
        cmp     ax,weedend[bp]
        ;cmp     ax,weedbed[bp]
        jb      @@fix_ptr                 ;don't need to adjust if below symbols
        sub     ax,gap[bp]              ;"gap" is the amount of compaction
        jc      @@err                   ;must be positive value
        mov     [si],wptr ax            ;rewrite the seg value reduced by GAP
@@fix_ptr:
        ;---if in range, also need to fix the reloc pointer in the header
        mov     ax,gap[bp]              ;compaction, in paragraphs
        sub     wptr es:[di+02],ax      ;adjust the relocation pointer downward

@@get_next:
        add     di,04                   ;point to next fixup item.
        loop    @@top                   ;process all the fixups

@@done: pop     es
        pop     cx
        pop     si
        pop     di
        clc
        cmp     straddle[bp],FALSE
        je      @@ret
        stc                             ;flag to extend buffer one byte
@@ret:  ret                             ;  to avod splitting a straddling ptr.

@@err:
        mov     ax,@data
        mov     ds,ax
        mov     es,ax
        jmp     fatal_gap_err

data_fixups     endp



;
; copy_old_symbols()  displays in scrolling windows the symbols and procs from
;                     Infile's symbol table. Also makes a count of both. Proc
;                     names ALSO count/display as symbols. Uses "infile" struc
;                     Window is row7/col16 thru row12/col29. Sym at row12/col18
;
;                     Display the symbol count and dupe count
;
;                     outfile.sym_count is incremented via call to copy_symbol
;
copy_old_symbols        proc    near    ;ds = es = DGROUP

        mov     outfile.sym_count,0
        mov     infile.sym_count,1
        mov     infile.proc_count,1     ;because the first symbol is executable
        xor     bp,bp                   ;
        mov     dx,infile.symseg
        mov     ds,dx                   ;ds = Symbols
        xor     bx,bx
        cmp     cl87_flag[bp],1
        jne     @@_0
        call    get_symbol_type87       ;returns 0 = null, carry = invalid
        jmp     @@_1
@@_0:   call    get_symbol_type         ;returns 0 = null, carry = invalid
@@_1:   je      @@0
		jc	@@bad						; invalid

; non-null, not invalid, first symbol.  This can happen with Clipper 5.2
        mov     es:was_null,FALSE       ;clear the flag
		jmp	@@t1

@@bad:  jmp     @@badsym                ;first symbol should always be a null
@@0:    mov     es:was_null,TRUE        ;set the flag
        jmp     @@1                     ;first symbol should always be a null
@@top:
        mov     es:was_null,FALSE       ;clear the flag
        cmp     cl87_flag[bp],1
        jne     @@_2
        call    get_symbol_type87       ;returns 0 = null, carry = invalid
        jmp     @@_3
@@_2:   call    get_symbol_type         ;returns 0 = null, carry = invalid
@@_3:   jc      @@bad                   ;error if not valid
        jne     @@t1                    ;was it a blank?
        mov     es:was_null,TRUE        ;set the flag

@@t1:
;***		cmp	es:no_display_flag,TRUE		; see if should print symbol names
;***		je	@@med1
	   call    print_symbol            ;scroll window and print new symbol

@@med1:
        inc     es:infile.sym_count
        mov     ax,[bx.symbol_segptr]
        or      ax,[bx.symbol_offptr]     ;executable?
        jz      @@1
        inc     es:infile.proc_count
@@1:    inc     dx                      ;bump the segment
        cmp     cl87_flag[bp],1
        jne     @@_4
        call    copy_symbol87           ;compaction/copy. return set carry = dup
        jmp     @@_5
@@_4:   call    copy_symbol             ;compaction/copy. return set carry = dup
@@_5:   jnc     @@11
        inc     es:infile.dupe_count    ;count the dupes
@@11:   mov     [bx.symbol_nulls],ax    ;note the new value on the old sym table
        ;---see if we're done yet.
        mov     ds,dx
        mov     ax,es:infile.sym_count
        cmp     ax,es:weedlength
        jae     @@2                     ;jump out of loop if finished
        cmp     es:was_null,TRUE
        jne     @@15

;***		cmp	es:no_display_flag,TRUE		; see if should print symbol names
;***		je	@@med2
        call    print_module            ;display proc name if it follows a null

@@med2:
        inc     es:infile.module_count
@@15:   jmp     @@top                   ;loop for more symbols
@@2:    push    es
        pop     ds                      ;restore ds = DGROUP
        ;---display symbol and dupe count
        xor     dx,dx
        mov     ax,infile.sym_count
        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa
        mov     di,offset symcount_msg+25
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        mov     al," "
        stosb                           ;overwrite the null at end of string
        xor     dx,dx
        mov     ax,infile.dupe_count
        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa
        mov     di,offset symcount_msg+44
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        mov     si,offset symcount_msg  ;called with target offset
        call    showscreen
        mov     ax,outfile.sym_count         ;sym length in paragraphs
        xor     dx,dx
        mov     si,offset new_symlength ;line buffer with attributes, row, col
        call    show_paras              ;show symbol table length on screen
        mov     ax,infile.dupe_count
        mov     gap,ax                  ;the #paragraphs of compaction
        ret

@@badsym:
        push    es
        pop     ds
        jmp     symbol_err

copy_old_symbols        endp

; copy_symbol()  is the actual copy routine that transfers the symbol to the
;                  new buffer after searching to make sure it is unique. also
;                  updates the symbol_nulls field with the new symbol ordinal
;                  position in the compacted table for later transfer to the
;                  pcode references that must be fixed up. This number is
;                  returned in ax/carry clear. If carry = set, it was a dupe.
;
;                  also bumps the counter in OutFile.sym_count
;

copy_symbol     proc    near

        push    es              ;call with es = dgroup,  ds = symbol segment
        push    bx
        push    dx
        push    bp              ;use bp as counter

        xor     bp,bp           ;start with item 0
        mov     ax,es:outfile.symseg       ;segment address of buffer
        mov     cx,es:outfile.sym_count    ;the symbol count to search
        mov     es,ax           ;es = new symbol buffer segment address
        or      cx,cx
        je      @@2             ;first null symbol is special case

@@top:  xor     di,di
        xor     si,si
        mov     bx,12 		;include the symbol.nulls field too
        mov     dx,12		;   to preserve static symbols separately
        call    strcmp          ;preserves cx. returns zero flag set if a match
        jnz     @@1             ;jump around if not a match, continue looping
        xor     bx,bx
        test    [bx.symbol_nulls],0ffffh  ;is it  "STATICS$"  or static func ?
        jne     @@1             ;treat any non-zero value as a unique symbol

        ;--- symbols match, but must see if one/both are executable
        mov     ax,ds:symbol_offptr
        or      ax,ds:symbol_segptr ;see if the source symbol is executable?
        je      @@not_ex        ;jump taken if source symbol is NOT executable
        mov     ax,es:symbol_offptr   ;to get here, source IS executable
        or      ax,es:symbol_segptr ;see if the destination symbol is executable?
        jne     @@0             ;if both executable, it was a dupe
        je      @@1             ;a genuine mismatch
@@not_ex:
        inc     ss:dup_nonexecutable ;keep a running count  ***
        mov     ax,es:symbol_offptr   ;to get here, source IS NOT executable
        or      ax,es:symbol_segptr ;see if the destination symbol is executable?
        jne     @@1             ;a genuine mismatch
@@0:
        inc     ss:dup_executable ;keep a running count    ***
        call    kill_fixup      ;search the exe fixups for this vector, kill it
        mov     ax,bp           ;DUPE. return the count in ax
        pop     bp
        pop     dx
        pop     bx
        pop     es
        stc                     ;set flag for return as "duplicate"
        jmp     @@done          ;the RET
@@1:    mov     ax,es
        inc     ax
        mov     es,ax
        inc     bp
        loop    @@top
        ;---if we fall through to here, there were no matching symbols
@@2:
        call    fixup_fixup     ;the fixup vector may need alteration...
        xor     di,di
        xor     si,si
        mov     cx,8            ;words to move ds:si -> es:di
    rep movsw                   ;copy the symbol
        mov     ax,bp
        pop     bp
        pop     dx
        pop     bx
        pop     es
        inc     es:outfile.sym_count    ;bump the counter
        clc                             ;return flag for non-duplicate
@@done: ret

copy_symbol     endp


copy_symbol87     proc    near

        push    es              ;call with es = dgroup,  ds = symbol segment
        push    bx
        push    dx
        push    bp              ;use bp as counter

        xor     bp,bp           ;start with item 0
        mov     ax,es:outfile.symseg       ;segment address of buffer
        mov     cx,es:outfile.sym_count    ;the symbol count to search
        mov     es,ax           ;es = new symbol buffer segment address
        or      cx,cx
        je      @@2             ;first null symbol is special case

@@top:  xor     di,di
        xor     si,si
        mov     bx,10
        mov     dx,10
        call    strcmp          ;preserves cx. returns zero flag set if a match
        jnz     @@1             ;jump around if not a match, continue looping
        ;--- symbols match, but must see if one/both are executable
        mov     ax,ds:symbol_offptr
        or      ax,ds:symbol_segptr ;see if the source symbol is executable?
        je      @@not_ex        ;jump taken if source symbol is NOT executable
        mov     ax,es:symbol_offptr   ;to get here, source IS executable
        or      ax,es:symbol_segptr ;see if the destination symbol is executable?
        jne     @@0             ;if both executable, it was a dupe
        je      @@1             ;a genuine mismatch
@@not_ex:
        inc     ss:dup_nonexecutable ;keep a running count  ***
        mov     ax,es:symbol_offptr   ;to get here, source IS NOT executable
        or      ax,es:symbol_segptr ;see if the destination symbol is executable?
        jne     @@1             ;a genuine mismatch
@@0:
        inc     ss:dup_executable ;keep a running count    ***
        call    kill_fixup      ;search the exe fixups for this vector, kill it
        mov     ax,bp           ;DUPE. return the count in ax
        pop     bp
        pop     dx
        pop     bx
        pop     es
        stc                     ;set flag for return as "duplicate"
        jmp     @@done          ;the RET
@@1:    mov     ax,es
        inc     ax
        mov     es,ax
        inc     bp
        loop    @@top
        ;---if we fall through to here, there were no matching symbols
@@2:
        call    fixup_fixup     ;the fixup vector may need alteration...
        xor     di,di
        xor     si,si
        mov     cx,8            ;words to move ds:si -> es:di
    rep movsw                   ;copy the symbol
        mov     ax,bp
        pop     bp
        pop     dx
        pop     bx
        pop     es
        inc     es:outfile.sym_count    ;bump the counter
        clc                             ;return flag for non-duplicate
@@done: ret

copy_symbol87     endp




;
;  kill_fixup    searches exe header fixup table and removes vector for
;                  the symbol currently in buffer at ds:0. Also decrements
;                  the relocation_item_count in the header
;
kill_fixup      proc    near

        push    bp                      ;save the symbol-index value
        push    es
        push    si
        push    di
        push    cx
        xor     bp,bp

        ;---first, see if it's executable at ds:0ch, ds:0eh
        mov     si,0ch
        mov     ax,wptr [si]
        or      ax,wptr [si+02]
        jne      @@1
        jmp     @@ret                   ;skip this if it's an mvar dupe

        ;---calculate the old-symbol's actual segment address
@@1:    mov     ax,ds
        sub     ax,ss:infile.symseg     ;current - bufferstart segment
        add     ax,weedbed[bp]          ;dx = actual segment of this symbol
        xor     dx,dx
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1                    ;dx:ax = (long) offset of symbol
        add     ax,0eh
        adc     dx,0                    ;dx:ax = (long) offset of fixup site
        mov     symbol_lo[bp],ax
        mov     symbol_hi[bp],dx

        ;---set up the exe header
        mov     ax,headerbuffer[bp]     ;pointer to header buffer
        mov     es,ax
        xor     bx,bx
        mov     di,es:[bx.first_reloc_offset]  ;start here at es:di
        mov     cx,es:[bx.relocation_item_count]  ;cx = item count

        ;---process all fixups in a loop
@@top:  mov     ax,wptr es:[di+02]      ;first relocation item

        ;---convert vector to a long displacement. This is ABOVE the header
        xor     dx,dx
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        add     ax,wptr es:[di]
        adc     dx,0                    ;the fixup pointer as a long in dx:ax

        ;---do the comparison
        cmp     dx,symbol_hi[bp]
        jne     @@not_it
        cmp     ax,symbol_lo[bp]
        jne     @@not_it

        ;---if we got here, es:di points to the matching pointer (to be killed)
        mov     si,es:[bx.relocation_item_count]
        dec     si
        shl     si,1
        shl     si,1
        add     si,es:[bx.first_reloc_offset]   ;point to last dword vector

        ;---overwrite this vector with the last one
        mov     ax,es:[si]
        mov     es:[di],wptr ax
        mov     ax,es:[si+02]
        mov     es:[di+02],wptr ax

        ;---now zero-out the last vector and decrease the header count
        xor     ax,ax
        mov     es:[si],ax              ;zero out last one to make it obvious
        mov     es:[si+02],ax
        dec     es:[bx.relocation_item_count]      ;tell the header
BUG2:   dec     ss:inheader.relocation_item_count  ;maybe need to fix this too?
        jmp     @@ret                   ;can't be any others, right?

@@not_it:
        add     di,04                   ;point to next fixup item.
        loop    @@top                   ;process all the fixups

@@ret:
        pop     cx
        pop     di
        pop     si
        pop     es
        pop     bp
        ret

kill_fixup      endp



;
;  fixup_fixup    searches exe header fixup table and alters vector for
;                  the symbol currently in buffer at ds:0.
;
fixup_fixup      proc    near

        push    bp                      ;save the symbol-index value
        push    es
        push    si
        push    di
        push    cx
        xor     bp,bp
        mov     es_seg[bp],es           ;new-symbol's segment on entry

        ;---first, see if it's executable at ds:0ch, ds:0eh
        mov     si,0ch
        mov     ax,wptr [si]
        or      ax,wptr [si+02]
        jne      @@1
        jmp     @@ret                   ;skip this if it's an mvar dupe

        ;---calculate the old-symbol's actual segment address
@@1:    mov     ax,ds
        sub     ax,ss:infile.symseg     ;current - bufferstart segment
        add     ax,weedbed[bp]          ;dx = actual segment of this symbol
        xor     dx,dx
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1                    ;dx:ax = (long) offset of symbol
        add     ax,0eh
        adc     dx,0                    ;dx:ax = (long) offset of fixup site
        mov     symbol_lo[bp],ax
        mov     symbol_hi[bp],dx

        ;---set up the exe header
        mov     ax,headerbuffer[bp]     ;pointer to header buffer
        mov     es,ax
        xor     bx,bx
        mov     di,es:[bx.first_reloc_offset]  ;start here at es:di
        mov     cx,es:[bx.relocation_item_count]  ;cx = item count

        ;---process all fixups in a loop
@@top:  mov     ax,wptr es:[di+02]      ;first relocation item

        ;---convert vector to a long displacement. This is ABOVE the header
        xor     dx,dx
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        add     ax,wptr es:[di]
        adc     dx,0                    ;the fixup pointer as a long in dx:ax

        ;---do the comparison
        cmp     dx,symbol_hi[bp]
        jne     @@not_it
        cmp     ax,symbol_lo[bp]
        jne     @@not_it

        ;---if we got here, es:di points to the matching pointer for adjustment
        mov     ax,es_seg[bp]
        sub     ax,ss:outfile.symseg    ;calculate new-symbols file location
        add     ax,weedbed[bp]          ;ax now = paragraph address for fixup
        mov     es:[di],wptr 0eh             ;symbol fixups are all at offset 0eh
        mov     es:[di+02],ax

        jmp     @@ret                   ;can't be any others, right?

@@not_it:
        add     di,04                   ;point to next fixup item.
        loop    @@top                   ;process all the fixups

@@ret:
        pop     cx
        pop     di
        pop     si
        pop     es
        pop     bp
        ret

fixup_fixup      endp



; get_symbol_type()   call with ds:0 pointing to symbol. Returns zero flag set
;                       if null symbol (16 00's), carry set if invalid, and
;                       carry clear if apparently a valid Clipper symbol. Tests
;                       for ASCIIZ string in range A-Z 0-9 $_ for first up to
;                       10 chars. then 0 word at symbol_nulls. Saves regs.
;
get_symbol_type proc    near
        push    cx
        cmp     [bx.symbol_nulls],0       ;check the null word first
        jz      @@1
        cmp     [bx.symbol_nulls],0100h   ;is it a STATIC FUNC?
        jz      @@1
        cmp     [bx.symbol_nulls],0200h   ;is it  "STATICS$" ?
        jnz     @@invalid
@@1:    xor     si,si                   ;now check for a null word
        mov     cx,8
@@2:    lodsw
        or      ax,ax
        jnz     @@3
        loop    @@2
        clc
        jmp     @@5                     ;null symbol. return Z flag set
@@3:    xor     si,si                   ;else check for a valid ascii symbol
        mov     cx,10                   ;max length 10 characters
@@4:    lodsb                           ;get a letter into al
        cmp     al,0                    ;is it a null?
        jne     @@41
        cmp     cx,10                   ;is it the first character?
        jne     @@42
        je      @@invalid               ;can't be null 1st char if not all nulls
@@42:   lodsb                           ;else must all be zero's after ascii name
        cmp     al,0
        jne     @@invalid
        loop    @@42
        cmp     al,1                    ;force the zero flag to not be set
        clc
        jmp     @@5                     ;return home with carry and zero clear
@@41:   call    valid_char              ;return carry set if not valid
        jc      @@invalid
        loop    @@4
        jmp     @@5

@@invalid:
        stc
@@5:    pop     cx
        ret

get_symbol_type endp

get_symbol_type87 proc    near
        push    cx
        cmp     [bx.symbol_nulls],0       ;check the null word first
        jz      @@1
        jnz     @@invalid
@@1:    xor     si,si                   ;now check for a null word
        mov     cx,8
@@2:    lodsw
        or      ax,ax
        jnz     @@3
        loop    @@2
        clc
        jmp     @@5                     ;null symbol. return Z flag set
@@3:    xor     si,si                   ;else check for a valid ascii symbol
        mov     cx,10                   ;max length 10 characters
@@4:    lodsb                           ;get a letter into al
        cmp     al,0                    ;is it a null?
        jne     @@41
        cmp     cx,10                   ;is it the first character?
        jne     @@42
        je      @@invalid               ;can't be null 1st char if not all nulls
@@42:   lodsb                           ;else must all be zero's after ascii name
        cmp     al,0
        jne     @@invalid
        loop    @@42
        cmp     al,1                    ;force the zero flag to not be set
        clc
        jmp     @@5                     ;return home with carry and zero clear
@@41:   call    valid_char              ;return carry set if not valid
        jc      @@invalid
        loop    @@4
        jmp     @@5

@@invalid:
        stc
@@5:    pop     cx
        ret

get_symbol_type87 endp



; print_symbol()  scrolls the symbol window and prints current ds:0 symbol
;                       on bottom line of window.
;                       ds = symbol segment   es = dgroup
print_symbol    proc    near

        push    bx
        push    cx
        push    dx

        ;---dummy time delay for testing
;        mov     cx,2000h
;@@00:   push    bx
;        pop     bx
;        loop    @@00

        ;---scroll symwindow up one row
        mov     ax,0601h                ;UP, one row
        mov     bh,es:symwindow.textback
        shl     bh,1
        shl     bh,1
        shl     bh,1
        shl     bh,1
        mov     cl,es:symwindow.tcol
        mov     ch,es:symwindow.trow
        mov     dl,es:symwindow.bcol
        mov     dh,es:symwindow.brow
        int     10h

        ;---print the text to the now-vacant bottom row
        mov     cx,5                    ;max 10 chrs = 5 words
        mov     di,offset es:symwindow.textchrs
        xor     si,si                   ;copying from ds:0
   rep  movsw
        mov     si,offset es:symwindow.textcol
        call    showscreen              ;showscreen() takes care of ds
        pop     dx
        pop     cx
        pop     bx
        ret

print_symbol    endp

; print_module()  scrolls the symbol window and prints current ds:0 symbol
;                       on bottom line of window.
;                       ds = symbol segment   es = dgroup
print_module  proc    near

        push    bx
        push    cx
        push    dx

        ;---scroll procwindow up one row
        mov     ax,0601h                ;UP, one row
        mov     bh,es:procwindow.textback
        shl     bh,1
        shl     bh,1
        shl     bh,1
        shl     bh,1
        mov     cl,es:procwindow.tcol
        mov     ch,es:procwindow.trow
        mov     dl,es:procwindow.bcol
        mov     dh,es:procwindow.brow
        int     10h

        ;---print the text to the now-vacant bottom row
        mov     cx,5                    ;max 10 chrs = 5 words
        mov     di,offset es:procwindow.textchrs
        xor     si,si                   ;copying from ds:0
   rep  movsw
        mov     si,offset es:procwindow.textcol
        call    showscreen              ;showscreen() takes care of ds
        pop     dx
        pop     cx
        pop     bx
        ret

print_module  endp


; valid_char() validates character in al as a valid symbol-character
;               must be in range   A-Z 0-9 _$ or -
valid_char      proc    near

        cmp     al,"_"
        je      @@clc
        cmp     al,"-"
        je      @@clc
        cmp     al,"$"
        je      @@clc
        cmp     al,"0"
        jb      @@stc
        cmp     al,"9"
        jbe     @@clc
        and     al,0dfh
        cmp     al,"A"
        jb      @@stc
        cmp     al,"Z"
        jbe     @@clc
@@stc:  stc
        jmp     @@rtn
@@clc:  clc
@@rtn:  ret

valid_char      endp


end     wpstart