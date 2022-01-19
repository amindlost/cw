title wpscreen.asm
page 82,132
subttl

comment*
        NOTE: 7/11/91  Make the version# 1.20 for release! Per Doug Amaral
        10/23/91 06:03pm small mod in wp50 line 1917 --> version 1.21

        SCREEN module for PACKRAT   (WP.ASM)   //  FOR CLIPPER 5.01
        08/22/90 09:44pm

        contains procedures:
                init_video
                wmscreen
                showscreen
                writes
                clearscreen
                syhelp
                screencalc
                screenbox2
                dirdcvt
                dirtcvt
                dcvt
                tcvt
                b2dec
                getctry
                sound
                set_args
                strskp_white
                strskp
                strcpy
                strcmp
                show_infile
                filenamecpy
                arg_parse

* endcomment

LOCALS

INCLUDE wp50.equ                      ;color EQU's for screen displays, structs

;JOKE            equ     TRUE

TEXTLINES       equ     23              ;lines of screen text to display
HELPLINES       equ     23;     11              ;lines of help text to display

CR              equ     13
LF              equ     10
SPACE_CHAR      equ     32
NULL_CHAR       equ     0
TAB_CHAR        equ     9

public  init_video, wpscreen, showscreen, writes, clearscreen, wphelp
public  screencalc, screenbox2, dirdcvt, dirtcvt, dcvt, tcvt, b2dec
public  getctry, sound, set_args, strskp_white, strskp, strcpy
public  ScreenSeg, show_infile, filenamecpy, show_infile, show_outfile
public  asc2num, num2asc, strncpy, strcmp, strlen
;*** public kelly_screen
;*** public  kelly1, kelly1_end
public	strnrcpy

;---debugging stuff
;IFDEF DEBUG
;public init_prn, asciiz_prn, bytes_prn, buffstart_prn, readbytes_prn, num2hex
;ENDIF

extrn   argv:dword
extrn   argc:word
extrn   psp:word
extrn   mem_err:near
extrn   env_seg:word
extrn   has_a_dot:word
extrn   outfile:word
extrn   infile:word
extrn   targetname:byte
extrn   bad_spec_msg:byte
extrn   errmsg:near
extrn   exetail:byte
extrn   eqetail:byte
extrn   eqe_flag:word
extrn   ove_flag:word
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
extrn   samebase_flag:word
extrn   tempname:byte

extrn   ltoa:near
extrn   ltrim:near
extrn   buffstartptr_msw:word
extrn   buffstartptr_lsw:word

;DGROUP      GROUP   DATA
;
;;------------------------------------------------CODE
;cseg    segment para    public  'CODE'
;        assume  cs:cseg,ds:DGROUP,es:DGROUP,ss:DGROUP
;
;;------------------------------------------------DATA
;data    segment para    public  '_DATA'

.model small

.data
;---screen data. Format is:  Col#, Row#, Foreground, Background, Asciiz

screentext      label   byte
d1      db       14,4 ,lwhite,blue,"ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ",0
d2      db       14,5 ,lwhite,blue,"ณ                                                 ณ",0
d3      db       14,6 ,lwhite,blue,"ณฤSymbolsฤฤฤฤฤฤฤฤยฤStatisticsฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ2.00ฤณ",0
d4      db       14,7 ,lwhite,blue,"ณ                ณ InFile:                        ณ",0
d5      db       14,8 ,lwhite,blue,"ณ                ณ Load Size:                     ณ",0
d6      db       14,9 ,lwhite,blue,"ณ                ณ Symbol Table Size:             ณ",0
d7      db       14,10,lwhite,blue,"ณ                ณ                                ณ",0
d8      db       14,11,lwhite,blue,"ณ                ณ OutFile:                       ณ",0
d9      db       14,12,lwhite,blue,"ณ                ณ Load Size:                     ณ",0
d10     db       14,13,lwhite,blue,"ณฤModulesฤฤฤฤฤฤฤฤด Symbol Table Size:             ณ",0
d11     db       14,14,lwhite,blue,"ณ                ณ                                ณ",0
d12     db       14,15,lwhite,blue,"ณ                ณ Time/date:                     ณ",0
d13     db       14,16,lwhite,blue,"ณฤMessagesฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤณ",0
d14     db       14,17,lwhite,blue,"ณ                                                 ณ",0
d15     db       14,18,lwhite,blue,"ณ                                                 ณ",0
d16     db       14,19,lwhite,blue,"ภฤฤฤ                                           ฤฤฤู",0

d17     db       17,5 ,yellow,blue,"   SP               for Clipper 5 and S'87    ",0
;d17     db       30,5 ,yellow,blue,"S y m P a c 5.01",0

d18     db       19,19,lcyan,blue,"  (C) Copyright 1993  Michael Devore     ",0
d19     db       16, 6,lmagenta,blue,"Symbols",0
d20     db       33, 6,lmagenta,blue,"Statistics",0
d21     db       16,13,lmagenta,blue,"Modules",0
d22     db       16,16,lmagenta,blue,"Messages",0
d23     db       10,20,lwhite,blue,"",0
d31     db       4 dup (0)                             ;(null)

help1   db      0, 1,black,white,"ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป",0
        db      0, 2,black,white,"บ                                                                          บ",0
        db      0, 3,black,white,"บ     SP -  The Clipper 5 and S'87 Symbol Packing Utility.                 บ",0
        db      0, 4,black,white,"บ                                                                          บ",0
        db      0, 5,black,white,"บ     Syntax:   SP  <InFile>  [<Outfile>]  [/d] [/n]                       บ",0
        db      0, 6,black,white,"บ                                                                          บ",0
        db      0, 7,black,white,"บ     SP assumes a file extension of .EXE for the InFile and OutFile       บ",0
        db      0, 8,black,white,"บ     unless a different one is specified. If no OutFile is listed,        บ",0
        db      0, 9,black,white,"บ     the basename and path of the InFile will be used, with .EXE as       บ",0
        db      0,10,black,white,"บ     the extension; following compaction, if the Infile and Outfile       บ",0
        db      0,11,black,white,"บ     names were the same, the InFile will receive an .EXK extension.      บ",0
        db      0,12,black,white,"บ                                                                          บ",0
        db      0,13,black,white,"บ     The external overlay file name is read directly from the overlay     บ",0
        db      0,14,black,white,"บ     manager code in InFile. If the InFile and OutFile names were the     บ",0
        db      0,15,black,white,"บ     same, the original overlay will receive an .OVK extension.           บ",0
        db      0,16,black,white,"บ                                                                          บ",0
        db      0,17,black,white,"บ     Use /d to preserve the original .EXE file time/date stamp.           บ",0
        db      0,18,black,white,"บ                                                                          บ",0
        db      0,19,black,white,"บ     Version 2.01, Copyright 1993, Michael Devore, All rights reserved.   บ",0
        db      0,20,black,white,"บ     (Original version SymPacWL written by Neil Kingsley)                 บ",0
        db      0,21,black,white,"บ                                                                          บ",0
        db      0,22,black,white,"ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ",0
        db      0

COMMENT #
IFDEF NONENCRYPTED
kelly1  db     26, 8,black,white,"ษอออออออออออออออออออออออออออออออออออออออออออออป",0
        db     26, 9,black,white,"บ      ***   D I S C L A I M E R   ***        บ",0
        db     26,10,black,white,"บ                                             บ",0
        db     26,11,black,white,"บ    I _TRIED_ to convince Kelly that this    บ",0
        db     26,12,black,white,"บ    name just didn't make it!! I really      บ",0
        db     26,13,black,white,"บ    tried. Believe me. Honest.               บ",0
        db     26,14,black,white,"บ                                             บ",0
        db     26,15,black,white,'บ    "SymPacWL".  Ugh!                        บ',0
        db     26,16,black,white,"บ                                             บ",0
        db     26,17,black,white,'บ    "Kelly," I said, "this name sucks        บ',0
        db     26,18,black,white,'บ    rocks..." But would he listen to me?     บ',0
        db     26,19,black,white,'บ    Noooooo!... He LIKES "SymPacWL"!         บ',0
        db     26,20,black,white,"บ                                             บ",0
        db     26,21,black,white,"บ    Go figure...                             บ",0
        db     26,22,black,white,"บ                                             บ",0
        db     26,23,black,white,"ศอออออออออออออออออออออออออออออออออออออออออออออผ",0
        db      0
kelly1_end  LABEL byte
ELSE

        INCLUDE kelljoke.inc                      ;the above gag text, encrypted

ENDIF
END COMMENT #

ScreenSeg       dw      0b800h
NumCols         db      ?
ActivePage      db      ?

;---date data from Ray Duncan's article, May 31 1988 PC Mag  (TD.ARC)

cbuff   db      34 dup (0)      ; current country info
dbuff   db      8  dup (' ')    ; date formatting buffer
tbuff   db      "     a     "    ; time formatting buffer

                                ; filled in by 'getctry'
doffs   dw      0               ; offset of ASCII day
moffs   dw      0               ; offset of ASCII month
yoffs   dw      0               ; offset of ASCII year

                                ; date format determined
                                ; by Int 21H Func. 38H
dtab    dw      mdy             ; 0 = USA format
        dw      dmy             ; 1 = Europe format
        dw      ymd             ; 2 = Japan format

mdy     dw      dbuff+3         ; USA: month day year
        dw      dbuff
        dw      dbuff+6

dmy     dw      dbuff           ; Europe: day month year
        dw      dbuff+3
        dw      dbuff+6

ymd     dw      dbuff+6         ; Japan: year month day
        dw      dbuff+3
        dw      dbuff

pm_flag dw      0               ;if flag set, add "pm" after time

;IFDEF DEBUG
;debug_msg       db      "WP Debugging - Active",0
;buffstart_msg   db      "File offset for start of code buffer:               ",0
;bytes_msg       db      "Number of bytes to write:                     ",0
;readbytes_msg   db      "Number of bytes to read:                      ",0
;ENDIF

;data    ends

.code

init_video      proc    near            ;Expects ds = DGROUP
;---initialize video. get VideoSeg
        push    es
        mov     ah,0fh
        int     10h                     ;ah = #col's, al = mode,  bh = page
        cmp     al,07h                  ;mono?
        jne     gotvideo
        mov     ScreenSeg,0b000h
gotvideo:
        mov     NumCols,ah       ;also save display width (# columns)
        mov     ActivePage,bh    ;save page (normally = 0)

;---kill the cursor
        mov     ah,01h
        mov     ch,14h                  ;starting line
        mov     cl,14h                  ;ending line
        int     10h                     ;reset cursor to illegal size

;IFDEF DEBUG
;;---initialize the printer if debugging
;        call    init_prn
;ENDIF

;---clear the screen to black
        call    clearscreen
        pop     es
        ret
init_video      endp


COMMENT #
kelly_screen    proc    near            ;assumes ds = dgroup

        ;---first decrypt it in-situ
        push    es
        mov     ax,@data
        mov     es,ax

IFNDEF NONENCRYPTED
        mov     si,offset kelly1
        mov     di,offset kelly1
@@top:  lodsb
        xor     al,"S"
        stosb
        cmp     si,offset kelly1_end
        jb      @@top
ENDIF

        ;---now show it on screen
        mov     cx,16                   ; line count
        mov     si,offset kelly1
@@0:    call    showscreen                      ;prints up to next null
        loop    @@0
        pop     es
        ret                                     ;ds, es unchanged on exit

kelly_screen    endp

END COMMENT #


wpscreen       proc    near             ;Expects ds = Dgroup, uses & restores es

        push    es                      ;save es
;;---draw a box @ 9,0,70,22,lwhite,blue
;        mov     ah,06h
;        xor     al,al                   ;entire window
;        mov     bh,blue                 ;backgnd attribute goes in high nibble
;        shl     bh,1
;        shl     bh,1
;        shl     bh,1
;        shl     bh,1                    ;put it in the high nibble
;        or      bh,lwhite               ;foregnd in low nibble
;        mov     ch,00
;        mov     cl,09
;        mov     dh,22
;        mov     dl,70
;        int     10h                     ;clear it
;
;;---add the line-drawing characters at the edge - use bg+/b attribute
;        mov     ax,ScreenSeg
;        mov     es,ax
;        mov     bh,blue                 ;backgnd attribute goes in high nibble
;        shl     bh,1
;        shl     bh,1
;        shl     bh,1
;        shl     bh,1                    ;put it in the high nibble
;        or      bh,lcyan                ;foregnd in low nibble
;        mov     ax,bx
;        mov     dl,00                   ;dl = row
;        mov     dh,09                   ;dh = col
;        mov     cl,22                   ;cl = height
;        mov     ch,60                   ;ch = width
;        call    screenbox2
;
;---now print all of the display lines
        mov     si,offset d1
ltop:   call    showscreen
        cmp     si,offset d31
        jle     ltop

;---ret
        pop     es
        ret

wpscreen        endp


;----------------------------
showscreen      proc    near    ;Col#, Row#, Foreground, Background, Asciiz
                                ;destroys si,di,dx,ax   preserves ds,es
        push    es
        push    ds
        mov     ax,DGROUP
        mov     ds,ax
        mov     ax,ScreenSeg
        mov     es,ax
        lodsw                   ;This is the set-up to call WRITES()
        mov     dh,al           ;column in dh
        mov     dl,ah           ;row in dl
        lodsw
        shl     ah,1            ;ah now has backgnd
        shl     ah,1            ;al is undefined
        shl     ah,1
        shl     ah,1
        or      ah,al           ;ah now has combined attribute byte
        call    writes          ;do the direct screenwrite
        pop     ds
        pop     es
        ret
showscreen      endp

;----------------------------
writes          proc    near

        call    screencalc      ;return screen offset in dx
wtop:   lodsb                   ;get a character from the source string
        cmp     al,0
        je      wdone
        stosw                   ;ah has the attribute
        jmp     wtop
wdone:  ret

writes          endp

;----------------------------

clearscreen     proc    near

        mov     ah,06h
        xor     al,al                   ;entire window
        mov     bh,07h                  ;white/black attribute
        xor     cx,cx
        mov     dh,24
        mov     dl,79
        int     10h                     ;clear it to black
        ret

clearscreen     endp


screencalc      proc    near    ;dl = row, dh = col   saves bx, cx, dx  sets di
        push    bx
        push    cx
        push    dx
        mov     ch,dh           ;save column# in ch
        xor     dh,dh           ;dl = row
        mov     cl,04
        shl     dx,cl           ;dl = row*16
        mov     bx,dx
        mov     cl,02
        shl     dx,cl           ;dl now = row*64
        add     dx,bx           ;dx now = row*80
        xchg    cl,ch           ;col now in cl
        xor     ch,ch
        add     dx,cx           ;dx = screen address
        shl     dx,1            ;double it for screen address
        mov     di,dx           ;return di set to screen offset
        pop     dx
        pop     cx
        pop     bx
        ret

screencalc      endp

;----------------------------

screenbox2      proc    near    ;draws double-box from dl,dh to dl + cl,dh + ch
                                ; using attribute in ah

;---draw top line
        mov     bx,cx           ;save a copy of the width in bl, height in bh
        mov     al,201          ;upper left double corner
        call    screencalc      ;returns with di pointing to top corner
        stosw                   ;put it on screen
        mov     al,205          ;double horiz bar
dline1: cmp     bh,0
        je      dline2
        stosw                   ;draw the line across the top
        dec     bh
        jmp     dline1
dline2: mov     al,187          ;top left double corner
        stosw
;---draw bottom line
        push    dx
        mov     bx,cx           ;save a copy of the width in bl, height in bh
        add     dl,cl
        mov     al,200          ;lower left double corner
        call    screencalc      ;returns with di pointing to top corner
        stosw                   ;put it on screen
        mov     al,205          ;double horiz bar
dline3: cmp     bh,0
        je      dline4
        stosw                   ;draw the line across the top
        dec     bh
        jmp     dline3
dline4: mov     al,188          ;lower left double corner
        stosw
        pop     dx              ;restore coordinates
;---draw left-side vertical line.
        push    dx
        mov     bx,cx           ;save a copy of the width in bl, height in bh
        mov     al,186          ;vertical double line
        call    screencalc      ;returns with di pointing to top corner
        add     di,02
dline5: add     di,160-2          ;same spot on next row
        dec     bl
        cmp     bl,1
        je      dline6
        stosw
        jmp     dline5
dline6:
        stosw                   ;put it on screen
        pop     dx
;---draw right-side vertical line.
        mov     bx,cx           ;save a copy of the width in bl, height in bh
        add     dh,ch           ;coordinate for top right corner
        add     dh,2
        mov     al,186          ;vertical double line
        call    screencalc      ;returns with di pointing to top corner
dline7: add     di,160-2          ;same spot on next row
        dec     bl
        cmp     bl,1
        je      dline8
        stosw
        jmp     dline7
dline8:
        stosw                   ;put it on screen
        ret
screenbox2      endp



;---time and date routines from Ray Duncan TD.ARC  (PC Mag)
;     modified to mimic DOS DIR reporting functions

dirdcvt proc    near            ; format directory date
                                ; AX    = directory date
                                ; BX    = length
                                ; DS:SI = buffer
                                ; preserves all registers

        push    ax              ; save registers
        push    bx
        push    cx
        push    dx

        mov     dx,ax           ; isolate months & days
        and     dx,01ffh
        mov     cl,3            ; position month
        shl     dx,cl
        shr     dl,cl           ; position day

        mov     cl,9            ; position year
        shr     ax,cl
        add     ax,1980
        mov     cx,ax

        call    dcvt            ; convert to ASCII

        pop     dx              ; restore registers
        pop     cx
        pop     bx
        pop     ax

        ret                     ; back to caller

dirdcvt endp


dirtcvt proc    near            ; format directory time
                                ; AX    = directory time
                                ; BX    = length
                                ; DS:SI = buffer
                                ; preserves all registers

        push    ax              ; save registers
        push    bx
        push    cx
        push    dx

        mov     dx,ax           ; isolate seconds field
        and     dx,1fh          ; and position it
        mov     cl,9            ; (includes seconds*2)
        shl     dx,cl

        mov     cl,3            ; position hours
        shr     ax,cl

        mov     cl,2            ; position minutes
        shr     al,cl
        mov     cx,ax

        call    tcvt            ; convert to ASCII

        pop     dx              ; restore registers
        pop     cx
        pop     bx
        pop     ax

        ret                     ; back to caller

dirtcvt endp


dcvt    proc    near            ; format ASCII date
                                ; BX    = length
                                ; CX    = year (1980+)
                                ; DH    = month (1-12)
                                ; DL    = day (1-31)
                                ; DS:SI = buffer
                                ; length clamped to 8
                                ; destroys AX BX CX DX

        cmp     bx,8            ; make sure length OK
        jle     dcvt1
        mov     bx,8            ; too long, use 8 max

dcvt1:  push    es              ; save registers
        push    di
        push    si
        push    bx

        call    getctry         ; get country info

        mov     si,moffs        ; convert month
        mov     al,dh
        call    b2dec
        cmp     dh,9
        jg      dcvt2
        mov     DGROUP:dbuff," "  ;suppress the leading zero
dcvt2:
        mov     si,doffs        ; convert day
        mov     al,dl
        call    b2dec

        mov     si,yoffs        ; convert year,
        sub     cx,1900         ; corrected to 80-99
        mov     al,cl
        call    b2dec

        mov     ax,ds           ; transfer ASCII date
        mov     es,ax           ; to caller's buffer
        mov     si,offset DGROUP:dbuff
        pop     cx              ; buffer length
        pop     di              ; buffer address
        push    di
        rep movsb               ; copy string

        pop     si              ; restore registers
        pop     di
        pop     es

        ret                     ; return to caller

dcvt    endp


tcvt    proc    near            ; format ASCII time
                                ; BX    = length
                                ; CH    = hour
                                ; CL    = minute
                                ; DH    = second
                                ; DL    = hundredths
                                ; DS:SI = buffer
                                ; length clamped to 11
                                ; destroys AX BX CX DX

        cmp     bx,11           ; make sure length OK
        jle     tcvt1
        mov     bx,11           ; too long, use 11 max

tcvt1:  push    es              ; save registers
        push    di
        push    si
        push    bx

        call    getctry         ; get country info

        ;---now convert hours (in ch) for 12-hour time (ch is 0 to 23)
        cmp     ch,12
        jl     tc1
        cmp     ch,12                   ;don't subtract anything if its 12:xx
        je      tc0
        sub     ch,12
tc0:    mov     DGROUP:pm_flag,1        ;set the flag to add "pm"
tc1:    mov     al,ch           ; convert hours
        mov     si,offset DGROUP:tbuff
        call    b2dec

        mov     al,cl           ; convert minutes
        add     si,3
        call    b2dec

        cmp     DGROUP:pm_flag,1
        jne     tc2
        mov     al,"p"
        mov     DGROUP:tbuff+5,al         ;overwrite "am" in tbuff

tc2:    cmp     byte ptr DGROUP:tbuff,"0"
        jne     tc3
        mov     byte ptr DGROUP:tbuff," "       ;kill the leading zero
tc3:
;---we don't want seconds or less in a dir-type listing
;        mov     al,dh           ; convert seconds
;        add     si,3
;        call    b2dec
;
;        mov     al,dl           ; convert hundredths
;        add     si,3
;        call    b2dec

        mov     ax,ds           ; transfer ASCII time
        mov     es,ax           ; to caller's buffer
        mov     si,offset DGROUP:tbuff
        pop     cx              ; buffer length
        pop     di              ; buffer address
        push    di
        rep movsb               ; copy string

        pop     si              ; restore registers
        pop     di
        pop     es

        ret                     ; return to caller

tcvt    endp


b2dec   proc    near            ; convert binary 0-99
                                ; to two ASCII digits.
                                ; AL    = value
                                ; DS:SI = storage address
                                ; destroys AX

        aam                     ; divide AL by 10 ->
                                ; AH = quot., AL = rem.

        add     ax,'00'         ; convert to ASCII
        xchg    ah,al
        mov     [si],ax         ; and store digits

        ret                     ; back to caller

b2dec   endp


getctry proc    near            ; get country information

        test    doffs,-1        ; did we already get info?
        jnz     getc4           ; if we did, just exit

        push    ax              ; save registers
        push    bx              ; (in case destroyed by
        push    cx              ;  Int 21H Function 38H)
        push    dx

        mov     ax,3000h        ; Fxn. 30h = get DOS vers.
        int     21h             ; transfer to MS-DOS

        or      al,al           ; is it version 1.x?
        jz      getc1           ; yes, jump

        push    ax              ; save MS-DOS version

        mov     ax,3800h        ; get current country info
        mov     dx,offset DGROUP:cbuff
        int     21h             ; transfer to MS-DOS

        pop     ax              ; restore MS-DOS version
        jc      getc1           ; jump if get cntry failed

        cmp     al,3            ; is it version 3.x?
        jne     getc2           ; jump if version 2

                                ; MS-DOS version 3...
        mov     al,cbuff+9      ; get decimal separator
        mov     bh,cbuff+11     ; get date separator
        mov     bl,cbuff+13     ; get time separator
        jmp     getc3

getc1:                          ; version 1.x or get
                                ; country info failed,
                                ; force date format=mdy
        mov     word ptr cbuff,0

getc2:                          ; versions 1.x and 2.x,
        mov     al,'.'          ; force decimal separator
        mov     bh,'/'          ; force date separator
        mov     bl,':'          ; force time separator

getc3:  mov     tbuff+8,al      ; store decimal separator

        mov     tbuff+2,bl      ; store time separators
       ;; mov     tbuff+5,bl

        mov     dbuff+2,bh      ; store date separators
        mov     dbuff+5,bh

                                ; set date field offsets
                                ; using country info
        mov     bx,word ptr cbuff
        shl     bx,1            ; date code*2=dtab index
        mov     bx,[bx+dtab]
        mov     ax,[bx]         ; offset for ASCII day
        mov     doffs,ax
        mov     ax,[bx+2]       ; offset for ASCII month
        mov     moffs,ax
        mov     ax,[bx+4]       ; offset for ASCII year
        mov     yoffs,ax

        pop     dx              ; restore registers
        pop     cx
        pop     bx
        pop     ax

getc4:  ret                     ; back to caller

getctry endp


sound   proc    near            ;SCANLON P. 241   di = Freq in hz
                                ;     cx = duration
        push    cx
        push    dx
        mov     al,0b6h
        out     43h,al
        mov     dx,14h
        ;;mov     ax,533h*896
        mov     ax,0fff0h
        div     di
        pop     dx
        out     42h,al
        mov     al,ah
        out     42h,al
        in      al,61h
        mov     ah,al
        or      al,3
        out     61h,al                  ;start it
snd0:   mov     dx,0ffffh
snd1:   dec     dx
        cmp     dx,0
        jg      snd1
        loop    snd0
        mov     al,ah
        out     61h,al                  ;turn it off
        pop     cx
        ret

sound   endp

;----------------------------

wphelp  proc    near                    ;display the help text.  ds = DGROUP

        call    clearscreen
        mov     cx,HELPLINES                    ; line count
        mov     si,offset DGROUP:help1
wmh1:   call    showscreen                      ;prints up to next null
        loop    wmh1
        ret                                     ;ds, es unchanged on exit

wphelp  endp


set_args        proc    near

        mov     bx,10h                  ;arrive with ds = DGROUP  es = PSP
        mov     ah,48h
        int     21h                     ;malloc some memory for argv array
        jnc     sa1
        jmp     mem_err
sa1:    mov     argc,1
        mov     word ptr argv,0
        mov     word ptr argv[2],ax     ;the allocated segment start address
        mov     es,ax
        xor     di,di
        mov     bx,psp                  ;save psp segment in bx
        push    bx                      ;save bx - the DOS call trashes it
        mov     ah,30h
        int     21h
        pop     bx                      ;restore bx (psp segment)
        cmp     al,3
        jnb     sa_1
        jmp     saa10                   ;jump if prior to DOS 3.x
sa_1:   mov     ds,env_seg              ;careful! ds altered!
        xor     si,si
saa1:   call    strskp
        lodsb
        cmp     al,NULL_CHAR
        jne     saa1
        lodsw
        call    strcpy                  ;use program name as argv[0]
        inc     di

saa2:   mov     ax,bx                   ;bx held the psp segment
        mov     ds,ax                   ;ds now = PSP segment
        mov     si,81h                  ;point to command tail
saa3:   call    strskp_white            ;skip whitespace
        cmp     al,CR
        je      saa7
        push    ds
        mov     cx,DGROUP
        mov     ds,cx
        inc     argc
        pop     ds
        jmp     saa5

saa4:   stosb
saa5:   lodsb
        cmp     al,SPACE_CHAR
        jbe     saa6
        cmp     al,','
        je      saa6
        cmp     al,'~'
        je      saa6
        jbe     saa4

saa6:   cmp     al,CR
        mov     al,NULL_CHAR
        stosb
        jne     saa3

saa7:   inc     di
        and     di,-2                   ;word align array of offsets  (??)
        mov     cx,DGROUP
        mov     ds,cx
        lds     si,argv
        push    ds
        mov     cx,DGROUP
        mov     ds,cx
        mov     word ptr argv,di
        mov     cx,argc
        pop     ds

saa8:   mov     ax,si
        stosw
        call    strskp
        loop    saa8
        clc
saa9:   mov     ax,DGROUP
        mov     ds,ax
        mov     ax,psp
        mov     es,ax                   ;return ds = DGROUP, es = PSP
        ret

saa10:  mov     ax,'C'
        stosw
        jmp     saa2

set_args        endp



;
; show_infile()  verifies filespec and copies target into screentext
;                       Sets bad_spec_flag if illegal DOS characters.
;                       Calls filenamecpy for this purpose.
;
show_infile     proc    near            ;ds = DGROUP, es = psp
        push    ds
        pop     es                      ;es changed!
        lds     si,argv                 ;ds changed!
        add     si,02h                  ;ptr to argv[1] = infile name
        mov     si,[si]
        mov     di,offset es:infile.filename   ;buffer for filename
        call    filenamecpy             ;check and copy, returns cx = length
        jnc     st1                     ;carry set = illegal filespec
        push    es
        pop     ds                      ;restore ds
        mov     bad_spec_flag,1
        jmp     st2                     ;return with error flag set
st1:    cmp     es:has_a_dot,1
        je      st00                    ;if no dot, we'll add ".EXE"
        mov     si,offset es:exetail
        call    estrcpy                 ;add default extension, all in e-seg
        add     cx,4                    ;keep the length counter accurate
st00:   push    es
        pop     ds                      ;ds now DGROUP again
        mov     infile.spec_len,cx
        mov     si,offset infile.filename    ;get ready to copy it onto  screentext
        mov     di,offset d4
        add     di,32
        mov     cx,21
        call    strnrcpy                ;copy the 20 rightmost chrs of string
        jnc     @@0                     ;no carry means no truncation
        mov     bx,offset (d4+32)        ;else patch in an arrow as a marker
        mov     bptr [bx],''
@@0:    mov     al,' '
        stosb                           ;add a final space character
st2:    ret                             ;return ds = DGROUP

show_infile     endp


;
; show_outfile()  verifies filespec and copies outfile into screentext
;                       Sets bad_spec_flag if illegal DOS characters.
;                       Calls filenamecpy for this purpose. Default extension
;                          is .EQE to avoid overwrite with infile

show_outfile     proc    near            ;ds = DGROUP, es = psp
        push    ds
        pop     es                      ;es changed!
        lds     si,argv                 ;ds changed!
        cmp     es:argc,3
        jae     @@0
        push    es                      ;if no outname, use infile base name
        pop     ds
        mov     si,offset infile.filename
        mov     di,offset tempname
        mov     cx,40h   ;;;22h
@@top:  lodsb
        cmp     al,"."
        je      @@t1
        cmp     al,0
        je      @@t1
        stosb
        loop    @@top
        mov     si,offset bad_spec_msg
        jmp     errmsg
@@t1:   mov     si,offset tempname      ;use the copy in tempname
        mov     samebase_flag,TRUE      ;by definition
        jmp     @@00

@@0:    add     si,04h                  ;ptr to argv[2] = outfile name
        mov     si,[si]
@@00:   mov     di,offset es:outfile.filename   ;buffer for filename
        mov     es:has_a_dot,FALSE      ;clear flag before calling filenamecopy
        call    filenamecpy             ;check and copy, returns cx = length
        jnc     @@1                     ;carry set = illegal filespec
        push    es
        pop     ds                      ;restore ds
        mov     bad_spec_flag,TRUE
        jmp     @@2                     ;return with error flag set
@@1:    cmp     es:has_a_dot,TRUE
        je      @@3                     ;if no dot, we'll add ".EQE"
        mov     si,offset es:exetail
        call    estrcpy                 ;add default extension, all in e-seg
        add     cx,4                    ;keep the length counter accurate
        mov     es:eqe_flag,TRUE        ;keep a record...
@@3:    push    es
        pop     ds                      ;ds now DGROUP again
        mov     outfile.spec_len,cx

        ;---see if they're identical
        mov     di,offset infile.filename
        mov     dx,infile.spec_len
        mov     si,offset outfile.filename
        mov     bx,outfile.spec_len
        call    strcmp                  ;returns zero-flag if equal
        jne     @@32

        ;---if identical, patch the outfile extension to .EQE
        mov     samebase_flag,TRUE      ;by definition
        mov     di,offset outfile.filename
        mov     cx,outfile.spec_len
        mov     al,"."
  repne scasb                           ;see if there's an extension
        dec     di                      ;point to the "."
        mov     si,offset eqetail
        mov     cx,2
   rep  movsw                           ;overwrite with ".EQE"
        mov     es:eqe_flag,TRUE        ;keep a record...

@@32:   mov     si,offset outfile.filename ;get ready to copy it onto screentext
        cmp     samebase_flag,TRUE
        jne     @@35
        mov     si,offset infile.filename
@@35:   mov     di,offset d8
        add     di,32
        mov     cx,21
        call    strnrcpy                ;copy the 21 rightmost chrs of string
        jnc     @@_0                    ;no carry means no truncation
        mov     bx,offset (d8+32)        ;else patch in an arrow as a marker
        mov     bptr [bx],''
@@_0:   mov     al,' '
        mov     al,' '
        stosb                           ;add a final space character
@@2:    ret                             ;return ds = DGROUP

show_outfile     endp


; filenamecpy() copies filespec from ds:si to es:di. Seg regs are not changed.
;       In addition, it upper-cases the filespec, and checks for DOS-illegal
;       characters. Carry is set on return if illegal characters. Cx returns
;       the length of the name copied. BX will be set nonzero if the filespec
;       contained path characters (: or \)
;
filenamecpy     proc    near            ;ds = argv seg, es = DGROUP

        mov     cx,0ffffh
        xor     bx,bx
fnc1:   lodsb
        cmp     al,0
        je      fnc10
        mov     ah,al                   ;save a copy in al
        and     ah,0dfh                 ;convert to upper case in ah
        cmp     ah,'A'
        jb      not_ltr
        cmp     ah,'Z'
        ja      not_ltr
        mov     al,ah
fnc2:   stosb                           ;save it if it's a letter
        loop    fnc1                    ;and loop for the next one
not_ltr:
        cmp     al,'0'                  ;is it a number?
        jb      not_num
        cmp     al,'9'
        ja      not_num
        jmp     fnc2                    ;use it if it's a valid number, too
not_num:
        cmp     al,21h
        jb      not_pun
        cmp     al,27h
        ja      not_pun
        cmp     al,22h
        je      fnc3                    ; 22h = " and is not valid
        jmp     fnc2                    ;use it if it's valid   !"#$%&'
not_pun:
        cmp     al,'@'
        je      fnc2                    ;OK to use @
        cmp     al,'-'
        je      fnc2                    ;OK to use -
        cmp     al,28h
        je      fnc2                    ;OK to use (
        cmp     al,29h
        je      fnc2                    ;OK to use )
        cmp     al,5fh
        je      fnc2                    ;OK to use _
        cmp     al,7bh
        je      fnc2                    ;OK to use {
        cmp     al,7dh
        je      fnc2                    ;OK to use }
        cmp     al,':'
        jne      fnc21                  ;set the flag in bx if there's a path
        or      bx,01
        jmp     fnc2                    ;but use it anyway
fnc21:  cmp     al,'\'
        jne      fnc22                  ;set the flag in bx if there's a path
        or      bx,01
        jmp     fnc2                    ;but use it anyway
fnc22:  cmp     al,'.'
        jne     fnc3                    ;mark it if we have a dot
        mov     es:has_a_dot,01         ;set the flag
        jmp     fnc2                    ;and use the dot
fnc3:   stc                             ;set carry = bad filespec character
        jmp     fnc12
fnc10:  pushf
        neg     cx
        dec     cx
        popf

fnc12:  ret

filenamecpy     endp



show_argv       proc    near
        call    strlen                  ;returns result in cx, preserves si
        add     di,55                   ;L-justify
        call    strcpy
        mov     al," "
        stosb
        ret
show_argv       endp


strskp_white   proc    near

        lodsb
        cmp     al,SPACE_CHAR
        je      strskp_white
        cmp     al,TAB_CHAR
        je      strskp_white
        dec     si
        ret
strskp_white   endp

strskp   proc    near

        lodsb
        cmp     al,0
        jne     strskp
        ret
strskp   endp


strcpy  proc    near            ;string copy
                                ;and return length in cx
        mov     cx,di           ;save the starting point
@@0:    lodsb
        stosb
        cmp     al,NULL_CHAR
        jne     @@0
        dec     di              ;di now points to the terminal null
        sub     cx,di
        neg     cx              ;return the length in cx
        clc                     ;the neg cx will set the carry
        ret
strcpy  endp

strncpy  proc    near            ;string copy of max length cx
                                ;and return length in cx
        push    bx
        mov     bx,di           ;save the starting point
@@0:    lodsb
        stosb
        dec     cx
        cmp     al,NULL_CHAR
        je      @@1
        cmp     cx,0
        ja      @@0
@@1:    dec     di              ;di now points to the terminal null
        sub     bx,di
        neg     bx              ;return the length in cx
        inc     bx
        xchg    bx,cx
        pop     bx
        clc                     ;the neg cx will set the carry
        ret
strncpy  endp

strnrcpy  proc    near          ;string copy up to max cx rightmost chrs.
                                ;return carry set if truncation occurred

        ;---find out how long the source string is
        push    dx              ;just in case
        xor     ax,ax           ;we'll use ah as truncation flag later
        mov     dx,cx           ;desired max length now in dx
        call    strlen          ;length returned in cx, preserves si
        cmp     cx,dx           ;cmp actual length with desired max
        jbe     @@0             ;just copy normally if not too long
        mov     ah,1            ;ah == 1 means string was truncated
        sub     cx,dx           ;else adjust the starting point
        add     si,cx
        mov     cx,dx           ;get max length back into cx and dx

        ;---now copy up to cx characters, or stop at terminating null
@@0:    lodsb
        stosb
        cmp     al,NULL_CHAR
        jne     @@0
        dec     di              ;di now points to the terminal null
        sub     cx,di
        neg     cx              ;return the length in cx
        pop     dx
        or      ah,ah           ;truncation flag set?
        je      @@no
        stc                     ;set the carry as a flag if there was truncation
        jc      @@ret
@@no:   clc                     ;neg cx will set the carry. Clear == no truncn
@@ret:  ret

strnrcpy  endp



strupcpy  proc    near          ;string copy and upper-case simultaneously
                                ;and return length in cx
        mov     cx,di           ;save the starting point
@@0:    lodsb
        cmp     al,'a'
        jb      @@1
        cmp     al,'z'
        ja      @@1
        and     al,0dfh
@@1:    stosb
        cmp     al,NULL_CHAR
        jne     @@0
        dec     di              ;di now points to the terminal null
        sub     cx,di
        neg     cx              ;return the length in cx
        clc                     ;the neg cx will set the carry
        ret
strupcpy  endp


strlen  proc    near                ;returns string length in cx, preserves si

        push    si
        mov     cx,0ffffh
@@0:    lodsb
        cmp     al,0
        je      @@1
        loop    @@0
@@1:    neg     cx
        dec     cx
        pop     si
        ret
strlen  endp

;
; strcmp()   call with ds:si (length in bx), es:di (length in dx)
;            returns Z = true if strings are equal just like STRCMP()
;               from PC MAG Ray Duncan Nov 15, 1988 p.345. modified to save cx
;
strcmp  proc    near

        push    cx
        mov     cx,dx
        cmp     bx,dx
        ja      scmp1
        mov     cx,bx
scmp1:
  repz  cmpsb
        pop     cx
        jz      scmp2
        ret
scmp2:
        sub     bx,dx
        ret

strcmp  endp


estrcpy  proc    near             ;copy within es      es:si -> es:di

        lods    byte ptr es:[si]        ;bumps si just like "lodsb"
        stosb
        cmp     al,NULL_CHAR
        jne     estrcpy
        dec     di
        ret
estrcpy  endp



; modified 08/01/90 03:07pm to use asciiz strings, max 5 chars long.

asc2num proc    near                    ;from Scanlon p. 150 modified
                                        ; string at ds:si. Returns value in ax
        cld
        xor     dx,dx
        mov     es:savedx,dx            ;initialize it to 0
        mov     es:olddx,dx             ;initialize it to 0
        mov     es:alphachars,dx        ;initialize it to 0
        mov     cx,5                    ;maximum of 5 digits
nchar:  call    dx10                    ;multiply total times ten
        jc      m_error                 ;out-of-range error
a2n1:   lodsb
        cmp     al,'0'
        jb      m_error
        cmp     al,'9'
        ja      m_error
        and     ax,0fh                  ;convert it to binary
        add     dx,ax
        jc      m_error                ;out-of-range error
        cmp     byte ptr [si],0
        je      @@1
        loop    nchar
@@1:    mov     ax,dx
        jmp     m_exit

m_error:
        stc                             ;invalid or out-of-range

m_exit: ret

asc2num endp




num2asc proc    near            ;number in ax, target for text at es:di
num2asc0:
        push    es
        push    ax
        push    ds
        pop     es
        cld                     ;from Scanlon page 144
        cmp     ax,0            ;"Default" setting?
        jne     n2a0
        mov     ax,6644h        ;"Df"  convert 0000 to "Dflt"
        stosw
        mov     ax,746ch        ;"lt"
        stosw
        pop     ax
        pop     es
        ret
num2asc00: push  es
        push    ax
        push    ds
        pop     es
n2a0:   cmp     ax,-1           ;convert "-1" to 0000
        jne     n2a1
        mov     ax,0
n2a1:   cmp     ax,9999
        jl      n2a3
        mov     ax,2a2ah        ;"**"  convert over-range to "****"
        stosw
        mov     ax,2a2ah        ;"**"
        stosw
      ;  ret
        jmp     nd4             ;pop twice and then exit
n2a3:   mov     DGROUP:leading0,01      ;initialize the flag
        call    b2bcdw
        mov     DGROUP:bcds,ax
        mov     cx,4
nxtdig: call    rotax4l
        cmp     al,"0"
        je      nd1
        mov     DGROUP:leading0,0    ;clear the flag if it's not a zero
        jmp     nd2
nd1:    cmp     DGROUP:leading0,1    ;if it's a "0", is it a leading 0?
        jne     nd2
        mov     al," "               ;suppress it if it's a leading zero
nd2:    stosb
        cmp     cx,2
        jg      nd3
        mov     DGROUP:leading0,0    ;show the last zero if 0000
nd3:    loop   nxtdig
nd4:    pop     ax
        pop     es
        ret
num2asc endp


ltrim_num       proc    near            ;call with ds:di = target, ax = number
        push    es
        push    ds
        pop     es                      ;aim ds and es at our data
        push    di                      ;save real destination
        mov     di,offset ds:scratchpad
        call    num2asc                 ;write it to the scratchpad
        mov     si,offset ds:scratchpad
        pop     di
        mov     cx,9                    ;max length
ltn1:   lodsb
        cmp     al,0
        je      ltn2
        cmp     al," "                  ;skip over leading spaces
        jne     ltn3
        loop    ltn1
ltn3:   stosb
        loop    ltn1
ltn2:   pop     es
        ret
ltrim_num       endp


rotax4l proc    near            ;from Scanlon page 144
        rol     DGROUP:bcds,1
        rol     DGROUP:bcds,1
        rol     DGROUP:bcds,1
        rol     DGROUP:bcds,1
        mov     ax,DGROUP:bcds
        push    bx
        and     al,0fh
        mov     bx,offset DGROUP:ascii
        xlat    ascii
        pop     bx
        ret
rotax4l endp

b2bcdw  proc    near            ;from Scanlon page 108
        xor     dx,dx
        mov     cx,1000
        div     cx
        xchg    ax,dx
        mov     cl,04
        shl     dx,cl
        mov     cl,100
        div     cl
        add     dl,al
        mov     cl,04
        shl     dx,cl
        xchg    al,ah
        xor     ah,ah
        mov     cl,10
        div     cl
        add     dl,al
        mov     cl,04
        shl     dx,cl
        add     dl,ah
        mov     ax,dx
        ret
b2bcdw  endp


dx10    proc    near

        mov     es:savedx,dx
        shl     dx,1
        shl     dx,1
        add     dx,es:savedx
        shl     dx,1
        ret

dx10    endp

;
;--------------------------Debugging routines-----------------------------
;
IFDEF DEBUGX

init_prn        proc    near

        mov     dx,0            ;LPT1
        mov     ah,01           ;initialize
        int     17h
        mov     si,offset ss:debug_msg
        call    asciiz_prn
        ret
init_prn        endp


asciiz_prn      proc    near    ;call with asciiz string at ds:si

        push    ds
        mov     ax,@data
        mov     ds,ax
        mov     dx,0            ;LPT1
@@0:    lodsb
        or      al,al           ;null terminator?
        je      @@crlf
        mov     ah,0
        int     17h             ;print the character
        test    ah,1
        jnz     @@err           ;timed out?
        jmp     @@0

@@crlf: mov     al,CR          ;CRLF ends the line
        mov     ah,0
        int     17h
        mov     al,LF
        mov     ah,0
        int     17h

@@err:  pop     ds
        ret

asciiz_prn      endp

buffstart_prn   proc    near    ;prints buffstart file address

        push    ds
        push    es
        push    si
        push    di
        push    ax
        push    bx
        push    cx
        push    dx
        mov     ax,@data
        mov     ds,ax
        mov     es,ax
        mov     dx,buffstartptr_msw
        mov     ax,buffstartptr_lsw
        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa
        mov     di,offset buffstart_msg+38
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        mov     si,offset buffstart_msg
        call    asciiz_prn
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        pop     di
        pop     si
        pop     es
        pop     ds
        ret

buffstart_prn   endp

bytes_prn   proc    near        ;prints number of bytes to write

        push    ds
        push    es
        push    si
        push    di
        push    ax
        push    bx
        push    cx
        push    dx
        mov     ax,@data
        mov     ds,ax
        mov     es,ax
        mov     dx,0
        mov     ax,cx                   ;call with #bytes in cx
        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa
        mov     di,offset bytes_msg+26
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        mov     si,offset bytes_msg
        call    asciiz_prn
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        pop     di
        pop     si
        pop     es
        pop     ds
        ret

bytes_prn   endp

readbytes_prn   proc    near        ;prints number of bytes to write

        push    ds
        push    es
        push    si
        push    di
        push    ax
        push    bx
        push    cx
        push    dx
        mov     ax,@data
        mov     ds,ax
        mov     es,ax
        mov     dx,0
        mov     ax,cx                   ;call with #bytes in cx
        mov     si,offset scratchpad
        mov     cx,10                   ;radix for ltoa conversion
        call    ltoa
        mov     di,offset readbytes_msg+26
        mov     si,offset scratchpad
        call    ltrim                   ;call with ds:si, es:di
        mov     si,offset readbytes_msg
        call    asciiz_prn
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        pop     di
        pop     si
        pop     es
        pop     ds
        ret

readbytes_prn   endp

num2hex         proc    near            ;call with number in

        ret
num2hex         endp


ENDIF


;cseg    ends
end

;eof