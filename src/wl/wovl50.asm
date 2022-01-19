;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                 ;
;  WOVL50   WarpPack  Overlay handler for WarpLink                ;
;                                                                 ;
;     Link with WP.ASM, WPSCREEN.ASM, TBL.EQU, WP.EQU             ;
;                                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The following routines need to be modified for Clipper 5.0:
; process_ovlfile
;

title wovl.asm
page 82,132
subttl

comment*

WarpLink overlay-support extensions for WarpPack

11/08/90 11:01pm  line 762  -  WarpLink ignores fixups at offset 0000!!

* endcomment


; equ's go here...

;; CATON   equ     TRUE       ;the no-fixup trap annunciator!

INCLUDE wp50.equ             ;all of the screen color attributes, structures

;---vpp externals
extrn   argv:dword
extrn   argc:word
extrn   psp:word
extrn   mem_err:near
extrn   env_seg:word
extrn   has_a_dot:word
extrn   outfile:word
extrn   infile:word
extrn   targetname:byte
extrn   errmsg:near
extrn   exetail:byte
extrn   eqetail:byte
extrn   bad_spec_flag:word
extrn   inheader:word
extrn   outheader:word
extrn   wmflags:word
extrn   wmoldvals:word
extrn   wmargs:word
extrn   saveax:word
extrn   savedx:word
extrn   olddx:word
extrn   alphachars:word
extrn   leading0:word
extrn   scratchpad:byte
extrn   bcds:word
extrn   ascii:byte
extrn   ltoa:near
extrn   ltrim:near
extrn   buffstartptr_msw:word
extrn   buffstartptr_lsw:word
extrn   read_segstart:near
extrn   wl_to_clipper:near
extrn   fseek:near
extrn   read_startup:near
extrn   is_it_clipper:near
extrn   warpcode:byte
extrn   warpdata:byte
extrn   read_err:near
extrn   seek_err:near
extrn   ovlopen_err:near
extrn   warp_sig:byte
extrn   warp_dataseg:word
extrn   warp_ovlmgr:byte
extrn   seg_off_seek:near
extrn   seek_err:near
extrn   read_err:near
extrn   ovl_err:near
extrn   read_segstart:near
extrn   warp_sig:byte
extrn   wl_to_clipper:near
extrn   read_startup:near
extrn   is_it_clipper:near
extrn   ovl_text:byte
extrn   dos3:word
extrn   internal_overlay_flag:word
extrn   savetime:word
extrn   savedate:word
extrn   write_err:near
extrn   buffstartptr_msw:word
extrn   buffstartptr_lsw:word
extrn   bigdx_err:near
extrn   cl_startup:byte
extrn   pcode_waltz:near
extrn   pcode_err:near
extrn   gap:word
extrn   fatal_gap_err:near
extrn   ovetail:byte
extrn   ovltail:byte
extrn   wl_to_clipper:near
extrn   cl_plankhead:byte
extrn   weedbed:word
extrn   weedend:word
extrn   baselength_msw:word
extrn   baselength_lsw:word
extrn   new_baselength_msw:word
extrn   new_baselength_lsw:word
extrn   proc_ovl_msg:byte
extrn   proc_xovl_msg:byte
extrn   date_flag:word
extrn   ovlcount_msg:byte
extrn   ove_flag:word
extrn   eqe_flag:word
extrn   tempname:byte
extrn   strnrcpy:near
extrn   cl87_flag:word
extrn   cl87_plankhead:byte
extrn   pcode87_waltz:near

;---vpscreen externals
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
extrn mem_err:near
extrn ovlmgr_name:byte
extrn real_ovlname:byte
extrn unreal_ovlname:byte
extrn rename_err:near

public read_code
public parse_filename
public show_overlay, open_overlay
public process_overlay, ovl_rewind
public ovlstart_msw, ovlstart_lsw
public process_ovlfile, get_ovlfile, fixup_ovlfile
public copyname, process_ovlfile87

LOCALS

DOSSEG

.model small

;------------------------------------------------DATA
.data

ovlstart_lsw    dw      0       ;internal overlays start after .exe content
ovlstart_msw    dw      0

overlay_count   dw      0       ;read header start to this word
ovlheader_seg   dw      0       ;segment address of allocated buffer

this_ovlproc_len        dw      0   ;temp store for current length

ovlproc_hdr     warp_ovlproc  <>  ;structure for start of ovl header

no_fixup_msg    db      16,3 ,lwhite,red," No Fix-up items... would have trashed memory! ",0

;------------------------------------------------CODE
.code

;  read_code()   Reads startup segment to check for WarpLink OVLMGR. If not
;       found, rereads startup segment:offset (cs:ip) to look for Clipper
;       signature. If it DID find OVLMGR, then follow its pointer and check
;       for Clipper signature. In either case, return with carry set (bad
;       file) or carry clear AND Clipper startup code in the buffer named
;       warpcode. Also uses buffer named warpdata.
;
;
;       Read_code() reads 250h bytes of startup code to buffer (warpcode)
;       and then searches for the first instance of "mov ax,xxx/ mov ds,ax"
;       which provides the key to locating the warpdata segment. It will
;       typically occur in the first 30 bytes of so of the code, which starts
;       around offset 37h (following after the cs-data). If unable to locate
;       this code, try to check for Clipper. If the code is located, the
;       data segment is read into the warpdata buffer. DOS errors are handled
;       with direct jmps to the error outlets. Finally, tee up the Clipper code
;       after verifying OVLMGR. Return carry clear if successful.
;
read_code       proc    near

        call    read_segstart           ;read first 100 bytes of seg @ cs:0
;---look for the sequence mov ax,xxxx / mov ds,ax
        ;                         B8 lo hi 8E D8
@@2:    mov     si,offset warpcode.wcode
        mov     di,offset warpcode
        add     di,250h                 ;the end of the buffer
@@2_1:  lodsb
        cmp     al,0b8h
        jne     @@3
        cmp     [si+02], wptr 0d88eh
        jne     @@3
        mov     dx,wptr [si]        ;data segment address
        jmp     @@4
@@3:    cmp     si,di                   ;are we done yet?
        jb      @@2_1
        stc
        jc      @@clip                  ;jump if it wasn't WarpLink OVLMGR

@@4:    xor     cx,cx
        add     dx,inheader.header_paragraphs
        call    fseek
        mov     warp_dataseg,ax
        mov     warp_dataseg+02,dx
        jnc     @@5
        pop     cx
        jmp     seek_err

;---read 100h bytes to databuffer
@@5:    mov     bx,infile.filehandle
        mov     cx,100h
        mov     dx,offset warpdata
        mov     ah,3fh
        int     21h                     ;read 100h bytes
        jnc     @@6
        pop     cx
        jmp     read_err

;---search in the data for the Warplink signature at offset 001ah (22h long)
@@6:    mov     si,offset warp_sig      ;ds = es = DGROUP
        mov     di,offset warpdata.warpsign
        mov     cx,22h
  repe  cmpsb                           ;return with carry set if mismatched
        jnz     @@clip                  ;jump if it IS NOT warplink OVLMGR

        ;---copy the overlay filename
        mov     di,offset ovlmgr_name
        mov     si,offset warpdata.ovl_name
        mov     cx,6
  rep   movsw                           ;max length = 12

        ;---if we found the WarpLink signature
        mov     warp_ovlmgr,TRUE        ;set the global flag
        mov     si,offset infile.filepath
        mov     di,offset infile.ovlname
        mov     cx,22h                  ;max length
@@7:    lodsb
        or      al,al
        je      @@8
        stosb                           ;copy the path first
        loop    @@7
@@8:
        mov     si,offset warpdata.ovl_name
@@80:   lodsb                           ;copy the overlay name
        or      al,al
        je      @@9
        stosb
        loop    @@80
@@9:    cmp     cx,0
        jae     @@10
        jmp     ovl_err                 ;bad overlay name or path too long
@@10:   call    set_overlayname         ;fix the outfile name for the overlay
        call    wl_to_clipper           ;read clipper startup from OVLMGR ptrs
        jmp     verify_cl
@@clip:
        call    read_startup            ;otherwise read first 100 bytes @ cs:ip
verify_cl:
        call    is_it_clipper           ;return carry set if NOT clipper
        ret

read_code       endp

; set_overlayname()    copies infile.ovlname but alters extension to OVE
;
set_overlayname proc    near
        ret
set_overlayname endp


; open_overlay()  try to open the overlay file, using the filespec
;                       in infile.ovlname and store handle in infile.ovlhandle
;                 first, check to see if it's the same name as the base name
;                       (i.e. INTERNAL OVERLAY) in which case don't OPEN
;                       anything, just stick the infile's handle into ovlhandle.
;
open_overlay    proc    near

        ;---aim at basename and ovlname, and compare them
        mov     si,offset infile.filebase       ;length in bx
        mov     di,offset warpdata.ovl_name     ;length in dx
        mov     dx,12                   ;max filespec base length = 12
        mov     bx,12
        call    strcmp                  ;returns 0 if equal names
        jne     @@10
        mov     ax,infile.filehandle    ;else use main InFile handle
        mov     internal_overlay_flag, TRUE ;set a dgroup flag, too
        jmp     @@5                     ;jump if INTERNAL overlay

        ;---if got here, it's an EXTERNAL overlay, and must be opened
@@10:   mov     si,offset infile.filepath  ;originally OUTfile.filepath
        mov     di,offset tempname
        mov     cx,40h  ;;;22h
        call    copyname                ;copy up to 22h letters
        mov     si,offset ovlmgr_name
        call    copyname                ;add the name to the path
        mov     si,offset ovltail
        call    strcpy                  ;add ".OVL"
        mov     cx,40h   ;;;22h
        mov     si,offset tempname
        mov     di,offset infile.ovlname
        call    strcpy
        mov     ah,3dh
        mov     al,0a0h                 ;read only
        cmp     dos3,3                  ;attribute differs for DOS 2 vs 3+
        jae     @@0
        mov     al,0h                   ;read only DOS2
@@0:    mov     dx,offset infile.ovlname         ;the filenamecpy'd argv[1]
        int     21h
        jnc     @@1
        jmp     ovlopen_err
@@1:    mov     infile.ovlhandle,ax

        ;---now truncate/create and open overlay OUTPUT file if EXTERNAL
        mov     si,offset infile.ovlname
        mov     di,offset outfile.ovlname
        call    copyname
@@3:    mov     si,offset ovetail       ;get here after basename finished
        mov     cx,5                    ;".OVE",0
    rep movsb
        mov     ove_flag,TRUE           ;keep a record...
        mov     dx,offset outfile.ovlname
        mov     ah,3ch
        xor     cx,cx                   ;read-write attribute
        int     21h
        jnc     @@4
        jmp     ovlopen_err
@@4:    mov     outfile.ovlhandle,ax    ;save file handle for output ovl file
        jmp     @@6

        ;---if INTERNAL, use the main OutFile handle for the overlay, too
@@5:    mov     ax,outfile.filehandle
        mov     outfile.ovlhandle,ax
        mov     ax,infile.filehandle
        mov     infile.ovlhandle,ax

@@6:    ret

open_overlay    endp

; copyname()    call with ds:si, and es:di set. assumes max length 22h
;                       or stops at either ascii-null OR "."
;
copyname        proc    near

@@0:    lodsb
        cmp     al,"."
        je      @@1
        cmp     al,0
        je      @@1
        stosb
        loop    @@0
        jmp     rename_err
@@1:    ret

copyname        endp


;  show_overlay()       displays overlay full spec on screen
;
;
show_overlay    proc    near

        mov     si,offset infile.ovlname
        mov     di,offset ovl_text+14
        mov     cx,21
        call    strnrcpy                ;copy the 20 rightmost chrs of string
        jnc     @@_0                    ;no carry means no truncation
        mov     bx,offset (ovl_text+14)        ;else patch in an arrow as a marker
        mov     bptr [bx],''
@@_0:   mov     al,' '
        mov     si,offset ovl_text
        call    showscreen
        ret

show_overlay    endp

; parse_filename()  Call with bx = file structure. Uses filename and
;                       filebase and filepath fields in the structure.
;                       For preparing to handle/detect overlay file.
;                       Expects ds = DGROUP.
;
parse_filename  proc    near

        push    bx
        push    es
        push    ds
        pop     es
        mov     cx,[bx.spec_len]          ;length of full filespec
        std                             ;work backwards from end of name
        mov     si,bx+filename
        add     si,cx                   ;aim at last letter
@@0:    lodsb
        cmp     al,"\"                  ;get out of loop if we found "\"
        je      @@1
        cmp     al,":"                  ;or if we found ":"
        je      @@1
        loop    @@0                     ;if we fall through, NO path information

        ;---no path... copy the basename
        mov     cx,[bx.spec_len]
        cld
        ;mov     si,bx+filename
        lea     si,[bx.filename]
        lea     di,[bx.filebase]
        push    si
   rep  movsb                           ;copy the base name; there is no path
        xor     al,al
        stosb

        ;---copy the actual base-name without extension, too
        pop     si
        lea     di,[bx.filename_only]
@@00:   lodsb
        cmp     al,"."
        je      @@000
        cmp     al,0
        je      @@000
        stosb
        jmp     @@00

@@000:  mov     al,0
        stosb                           ;null-terminate it
        jmp     @@2

        ;---copy the basename if there is also path information
@@1:    cld                             ;if here, there is some path information
        push    cx
        sub     cx,[bx.spec_len]
        neg     cx                      ;cx = basename length
        dec     cx
        add     si,02
        push    si                      ;save name start location
        lea     di,[bx.filebase]
   rep  movsb                           ;copy the base name - there is no path
        xor     al,al
        stosb

        ;---get the filename_only, too
        pop     si
        lea     di,[bx.filename_only]
@@10:   lodsb
        cmp     al,"."
        je      @@100
        cmp     al,0
        je      @@100
        stosb
        jmp     @@10
@@100:  mov     al,0                    ;null-terminate it.
        stosb
        ;---now copy the path information

        pop     cx
        inc     cx
        lea     di,[bx.filepath]
        mov     si,bx+filename
   rep  movsb                           ;copy the path name
        xor     al,al
        stosb

@@2:    pop     es
        pop     bx
        ret

parse_filename  endp


; process_overlay()    The actual overlay handler skeleton.
;
;
process_overlay proc    near

        cmp     internal_overlay_flag,TRUE
        je      @@0
        mov     si,offset proc_xovl_msg
        call    showscreen
        mov     ovlstart_lsw,0          ;external overlays start at offset 0
        mov     ovlstart_msw,0
        jmp     @@2

@@0:
        mov     si,offset proc_ovl_msg
        call    showscreen
        ;---internal overlay uses end of normal InFile as starting point
        mov     ax,inheader.length512   ;512-byte chunks
        cmp     inheader.length_mod512,0
        je      @@1
        dec     ax                      ;the weird fixup
@@1:    xor     dx,dx
        call    mul16                   ;left-shift dx:ax four times
        call    mul16                   ;now *256
        shl     ax,1
        rcl     dx,1                    ;now *512
        add     ax,inheader.length_mod512
        adc     dx,0                    ;dx:ax = starting offset for ovl file
        mov     ovlstart_lsw,ax         ;internal overlays start after base file
        mov     ovlstart_msw,dx

        ;---move pointer to calculated end of OutFile (Internal overlay)
        mov     bx,outfile.ovlhandle
        mov     cx,new_baselength_msw
        mov     dx,new_baselength_lsw
        mov     ax,4200h                ;move to end of base file
        int     21h
        jnc     @@2
        jmp     seek_err

        ;---rewind InFile to start of overlay file
@@2:    call    ovl_rewind              ;points to start of overlay code

        ;---get ovlheader length, allocate buffer, reread and copy it to OutFile
        call    get_ovlheader

        ;---load and process overlay items in a loop
        xor     cx,cx                   ;start with index 0
@@3:    push    cx
        call    show_ovl_number         ;add a little annunciator
        call    get_ovlfile             ;seek and load to buffer. ALTERS DS!!!
        cmp     cl87_flag[bp],1
        jne     @@__0
        call    process_ovlfile87       ;pcode_waltz, fixup-fixups, and write
        jmp     @@__1
@@__0:  call    process_ovlfile         ;pcode_waltz, fixup-fixups, and write
@@__1:  pop     cx
        inc     cx
        cmp     cx,overlay_count[bp]
        jb      @@3

        ;---touch up date if needed, and if external overlay
        mov     ax,@data
        mov     ds,ax
        mov     es,ax
        cmp     internal_overlay_flag,TRUE
        je      @@4                     ;no date-patch for internal overlay
        cmp     date_flag,TRUE
        jne     @@4                     ;jump if no date patch was requested
        mov     bx,outfile.ovlhandle
        mov     ax,5701h
        mov     cx,savetime
        mov     dx,savedate
        int     21h                     ;"touch" the dir date/time stamp
        jnc     @@4
        jmp     write_err
@@4:    ret

process_overlay endp


;  show_ovl_number()     Add a little screen activity so it doesn't look
;                          like it's locked up while processing overlays.
;                        On entry, cx = counter (0-biased), ds = dgroup
;
show_ovl_number proc    near

        push    cx
        push    es
        push    ds
        pop     es                      ;need ds and es = dgroup
        inc     cx                      ;now 1-biased
        mov     ax,cx
        xor     dx,dx
        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa
        mov     di,offset ovlcount_msg+04
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        mov     si,offset ovlcount_msg  ;called with target offset
        call    showscreen
        pop     es
        pop     cx
        ret

show_ovl_number endp

;  get_ovlfile()    tee up an overlay file into the *buffer "infile.appcodeseg"
;                   based on cx value as an index into ovlheader_seg buffer
;                       which holds the ovl header.
;                   set up buffstart_msw, lsw as pointers to start of the read
;                       and for use later to rewind for the re-write.
;                   NOTE: returns ds = appcodeseg
;
get_ovlfile     proc    near

        ;--- aim at the ovlproc defined by the cx index value into ovlheader_seg
        mov     ax,ovlheader_seg
        mov     si,cx
        shl     si,1
        shl     si,1            ;four bytes per entry
        add     si,02           ;allow for the initial overlay-count word
        push    ds
        mov     ds,ax           ;DS ALTERED!
        mov     dx,wptr [si]
        mov     cx,wptr[si+02]  ;item offsets into ovl file
        add     dx,ovlstart_lsw[bp]
        adc     cx,ovlstart_msw[bp] ;allow for internal overlay offset too
        mov     bx,ss:infile.ovlhandle
        mov     ax,4200h
        int     21h
        pop     ds
        jnc     @@00
        jmp     seek_err

@@00:
        ;---read start of ovlproc header to determine header+proc length
        mov     bx,infile.ovlhandle
        mov     dx,offset ovlproc_hdr  ;ovl proc header structure
        mov     cx,10
        mov     ah,3fh
        int     21h                     ;read the first 10 bytes
        jnc     @@0
        jmp     read_err

        ;---save bufferstart_msw, bufferstart_lsw (= start of fixups in hdr)
@@0:    mov     bx,infile.ovlhandle
        xor     dx,dx
        xor     cx,cx
        sub     dx,10
        sbb     cx,0
        mov     ax,4201h                ;back up to the start of this ovlproc
        int     21h                     ;returns absolute offset in dx:ax
        mov     buffstartptr_msw,dx
        mov     buffstartptr_lsw,ax        ;current location

        ;---read the procedure based on the length values in ovlproc_hdr
        clc
        mov     cx,ovlproc_hdr.codesize_paras  ;normalized paragraphs
        shl     cx,1
        shl     cx,1
        shl     cx,1
        shl     cx,1
        add     cx,10                   ;the 1st ten bytes we've already read
        mov     ax,ovlproc_hdr.fixup_count
        shl     ax,1                    ;2 bytes for each entry
        add     cx,ax
        jnc     @@1
        jmp     bigdx_err

@@1:    mov     this_ovlproc_len,cx     ;total byte count
        mov     bx,infile.ovlhandle
        xor     dx,dx
        mov     ax,infile.appcodeseg    ;the big code buffer segment
        mov     ds,ax                   ;Watch out!!!  ds altered
        mov     ah,3fh
        int     21h                     ;read the entire ovlproc chunk
        jnc     @@2
        mov     ax,@data
        mov     ds,ax
        jmp     read_err

@@2:    ret

get_ovlfile     endp


; process_ovlfile()   proc is now in appcodeseg buffer. IF it's Pcode, do
;                       the pcode-waltz. In any event, do the fixup-fixups.
;                     then rewind using buffstart_msw, lsw and re-write to
;                       OutFile using length in "this_ovlproc_len".
;                     NOTE: on entry, ds = appcodeseg code buffer
;                           on exit, ds is restored to DGROUP
process_ovlfile proc    near

        ;---is it Clippercode?
        mov     ax,@data
        mov     es,ax
        mov     bx,2
        mov     si,[bx]                 ;offset to overlay code at offset +02
        mov     dx,si                   ;save a copy
        add     si,06                   ;our signature doesn't start at top
        mov     di,offset ss:cl_plankhead  ;signature for plankton header
        mov     cx,8                    ;signature length
   repz cmpsb                           ;compare the signature with the buffer
        jz      @@00                    ;Clipper code header found
@@notcl: jmp     @@fixups                ;else skip the pcode_waltz routine

        ;---check the remaining two isolated bytes of the header
@@00:   mov     si,dx                   ;retrieve original pointer
        cmp     bptr [si],0b8h
        jne     @@notcl
        cmp     bptr [si+03],0bbh
        jne     @@notcl
        ;---pcode_waltz for symbol reassignment, if Clipper code
@@0:    mov     ax,es:infile.symseg
        mov     es,ax
        xor     di,di                   ;set up old symbols in es:di
        mov     si,dx                   ;retreive pointer to codestart
        call    pcode_waltz             ;ds:si pointing to plankton header start
        jnc     @@fixups
        push    es
        pop     ds
        jmp     pcode_err

        ;---check fixup targets and alter values > weedbed
@@fixups:
        mov     bx,08
        mov     cx,[bx]                 ;fixup count from header
        or      cx,cx
        jne     @@1                     ;make sure that there are fixups!
  IFDEF CATON
        push    ds                      ;else, report the finding!!!
        push    es
        push    ax
        push    bx
        push    cx
        push    dx
        push    di
        push    si

        mov     dx,@data
        mov     ds,dx
        mov     si,offset no_fixup_msg
        call    showscreen

        pop     si
        pop     di
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        pop     es
        pop     ds
  ENDIF
        jmp     @@15                    ;and skip the fixups...
@@1:    push    cx
        call    fixup_ovlfile           ;ds = es = code buffer. cx = counter
        pop     cx
        loop    @@1                     ;process all in a loop

        ;---write the buffer to OutFile
@@15:   xor     dx,dx
        mov     cx,this_ovlproc_len[bp]
        mov     bx,ss:outfile.ovlhandle
        mov     ah,40h
        int     21h                     ;write the altered buffer
        jnc     @@2
        mov     ax,@data
        mov     ds,ax
        jmp     write_err

@@2:    mov     ax,@data
        mov     ds,ax
        ret

process_ovlfile endp


; process_ovlfile87()   proc is now in appcodeseg buffer. IF it's Pcode, do
;                       the pcode-waltz. In any event, do the fixup-fixups.
;                     then rewind using buffstart_msw, lsw and re-write to
;                       OutFile using length in "this_ovlproc_len".
;                     NOTE: on entry, ds = appcodeseg code buffer
;                           on exit, ds is restored to DGROUP
process_ovlfile87 proc    near

        ;---is it Clippercode?
        mov     ax,@data
        mov     es,ax
        mov     bx,2
        mov     si,[bx]                 ;offset to overlay code at offset +02
        mov     dx,si                   ;save a copy
        mov     di,offset ss:cl87_plankhead  ;signature for plankton header
        mov     cx,11                   ;signature length
   repz cmpsb                           ;compare the signature with the buffer
        jz      @@0                     ;Clipper code header found
        jmp     @@fixups                ;else skip the pcode_waltz routine

        ;---pcode_waltz for symbol reassignment, if Clipper code
@@0:    mov     ax,es:infile.symseg
        mov     es,ax
        xor     di,di                   ;set up old symbols in es:di
        mov     si,dx                   ;retreive pointer to codestart
        call    pcode87_waltz             ;ds:si pointing to plankton header start
        jnc     @@fixups
        push    es
        pop     ds
        jmp     pcode_err

        ;---check fixup targets and alter values > weedbed
@@fixups:
        mov     bx,08
        mov     cx,[bx]                 ;fixup count from header
        or      cx,cx
        jne     @@1                     ;make sure that there are fixups!
  IFDEF CATON
        push    ds                      ;else, report the finding!!!
        push    es
        push    ax
        push    bx
        push    cx
        push    dx
        push    di
        push    si

        mov     dx,@data
        mov     ds,dx
        mov     si,offset no_fixup_msg
        call    showscreen

        pop     si
        pop     di
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        pop     es
        pop     ds
  ENDIF
        jmp     @@15                    ;and skip the fixups...
@@1:    push    cx
        call    fixup_ovlfile           ;ds = es = code buffer. cx = counter
        pop     cx
        loop    @@1                     ;process all in a loop

        ;---write the buffer to OutFile
@@15:   xor     dx,dx
        mov     cx,this_ovlproc_len[bp]
        mov     bx,ss:outfile.ovlhandle
        mov     ah,40h
        int     21h                     ;write the altered buffer
        jnc     @@2
        mov     ax,@data
        mov     ds,ax
        jmp     write_err

@@2:    mov     ax,@data
        mov     ds,ax
        ret

process_ovlfile87 endp



;  fixup_ovlfile()  call with ds = code buffer, cx = reloc item # to fixup
;                   adjusts downward any segment references above weedend[bp],
;                       by the amount of paragraphs in "gap[bp]"
fixup_ovlfile   proc    near

        mov     bx,0ah                  ;first reloc item offset in header
        dec     cx                      ;allow for 0-bias of array indexes
        shl     cx,1                    ;2 bytes per element
        add     bx,cx
        mov     si,wptr [bx]            ;offset of fixup location in ovl code

;bugfix 11/08/90 11:00pm
        cmp     si,00                   ;don't use fixups at offset 0000!!!
        je      @@0

        mov     bx,02
        add     si,wptr [bx]            ;offset of codestart in this ovl section
        mov     ax,[si]
        cmp     ax,weedbed[bp]          ;start of symbols
        jbe     @@0                     ;don't need to adjust if below symbols
        sub     ax,gap[bp]              ;"gap" is the amount of compaction
        jc      @@err                   ;must be positive value
        mov     [si],wptr ax            ;rewrite the seg value reduced by GAP

@@0:    clc
        ret

@@err:  mov     ax,@data
        mov     ds,ax
        jmp     fatal_gap_err

fixup_ovlfile   endp

; get_ovlheader()   loads start of ovlheader to determine length, then
;                       allocates a buffer and reads entire header to it.
;                   also - write the header to the overlay OutFile
;
get_ovlheader   proc    near

        mov     dx,offset overlay_count
        mov     bx,infile.ovlhandle
        mov     ah,3fh
        mov     cx,2                    ;read only the first two bytes of ovl
        int     21h
        jnc     @@3
        jmp     read_err
@@3:    call    ovl_rewind
        mov     bx,overlay_count
        shl     bx,1
        shl     bx,1                    ;four bytes for each entry
        add     bx,02                   ;include the first two bytes
        mov     si,bx                   ;save it for a moment
        add     bx,0fh                  ;normalized paragraphs
        shr     bx,1
        shr     bx,1
        shr     bx,1
        shr     bx,1                    ;convert to paragraphs needed for buffer
        mov     ah,48h
        int     21h
        jnc     @@4
        jmp     mem_err
@@4:    mov     ovlheader_seg,ax        ;save the buffer address
        push    ds
        mov     ds,ax                   ;NOTE ds altered!
        xor     dx,dx
        mov     cx,si                   ;the byte count to read
        mov     ah,3fh
        mov     bx,ss:infile.ovlhandle
        int     21h                     ;read the header data to buffer
        jnc     @@5
@@41:   pop     ds                      ;restore stack and ds
        jmp     read_err
@@5:    cmp     ax,cx                   ;full read?
        jne     @@41

        ;---append it to the outfile - cx = header length
        mov     bx,ss:outfile.ovlhandle
        xor     dx,dx                   ;write from ds:dx
        mov     ah,40h                  ;DOS write
        int     21h
        jnc     @@6
        pop     ds
        jmp     write_err

@@6:    pop     ds                      ;restore dgroup
        ret

get_ovlheader   endp


;  ovl_rewind()         Rewinds to start of overlay code, regardless of
;                         whether it is internal or external overlay.
;
ovl_rewind      proc    near

        mov     cx,ovlstart_msw
        mov     dx,ovlstart_lsw
        mov     bx,infile.ovlhandle
        mov     ax,4200h
        int     21h                     ;fseek
        jnc     @@0
        jmp     seek_err
@@0:    ret

ovl_rewind      endp


ftell   proc    near                    ;call with handle in bx

        xor     cx,cx
        xor     dx,dx
        mov     ax,4201h                ;mode 1 = relative jump
        int     21h
        jnc     @@0
        mov     ax,@data
        mov     ds,ax                   ;in case ds was altered
        jmp     seek_err
@@0:    ret                             ;return absolute position in dx:ax

ftell   endp



mul16   proc    near            ;returns dx:ax multiplied by 16

        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1
        shl     ax,1
        rcl     dx,1            ;convert to bytes
        ret

mul16   endp


end