;*********************************************************************
;*   MLSUM.ASM                                                       *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          04/05/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   display linker command summary                                  *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlsum
PAGE    50,80

.MODEL  SMALL

;*****************************
;* Include files             *
;*****************************

INCLUDE MLEQUATE.INC
INCLUDE MLDATA.INC
     
;*****************************
;* Public declarations       *
;*****************************
     
PUBLIC  summary
     
;*****************************
;* Constant data             *
;*****************************

.CONST

sumlines    DB  CR,LF
            DB  'SYNTAX:  ',LINKERCAPS,' [options] object_files[,[program_file][,[map_file]'
            DB  CR,LF
            DB  '           [,[library_files]]]][;]'
            DB  CR,LF
            DB  CR,LF
            DB  '         (Items in brackets are optional)'
            DB  CR,LF
            DB  '                   or'
            DB  CR,LF
            DB  '         ',LINKERCAPS,' @response_file'
            DB  CR,LF
            DB  CR,LF
            DB  '<Press any key to continue....>'
sumlen1     EQU $-sumlines
sumlines2   DB  CR,LF
            DB  CR,LF
            DB  'NONOVERLAY OPTIONS:'
            DB  CR,LF
            DB  CR,LF
            DB  '/as:<size>     maximum program Allocation Space in paragraphs'
            DB  CR,LF
IFNDEF DEMO
            DB  '/b             Beep the speaker upon exit'
            DB  CR,LF
ENDIF
            DB  '/c             create COM file'
            DB  CR,LF
            DB  '/clpf          automatic Full link after failed CLiPper incremental link'
            DB  CR,LF
            DB  '/clpi          perform CLiPper Incremental link'
            DB  CR,LF
            DB  '/clpp:<size>   CLiPper incremental link segment Pad length in bytes'
            DB  CR,LF
IFNDEF DEMO
            DB  '/clps          use SMARTMEM.XXX CLiPper SmartMem functions'
            DB  CR,LF
            DB  '/d             use DOSSEG segment ordering'
            DB  CR,LF
            DB  '/ddl           create D-format Dynamic Library'
            DB  CR,LF
            DB  '/dm:<name>     DDL Manager file name'
            DB  CR,LF
            DB  '/em            use Extended dictionary in Microsoft-compatible library'
            DB  CR,LF
ENDIF
            DB  '/i             display link process Information'
            DB  CR,LF
IFNDEF DEMO
            DB  CR,LF
            DB  '<Press any key to continue....>'
sumlen2     EQU $-sumlines2
sumlines3	DB  CR,LF
            DB  CR,LF
ENDIF
            DB  '/m             create MAP file'
            DB  CR,LF
            DB  '/mx            create MAP file eXpanded version'
            DB  CR,LF
IFDEF DEMO
            DB  CR,LF
            DB  '<Press any key to continue....>'
sumlen2     EQU $-sumlines2
sumlines3	DB  CR,LF
            DB  CR,LF
ENDIF
            DB  '/nd            No Default libraries'
            DB  CR,LF
            DB  '/ql            perform QuickLinker link'
            DB  CR,LF
IFNDEF DEMO
            DB  '/s             Symbol names are case sensitive'
            DB  CR,LF
ENDIF
            DB  '/sp            Symbol table Pack for Clipper code'
            DB  CR,LF
            DB  '/st:<size>     program STack size in bytes'
            DB  CR,LF
            DB  '/tf:<name>     Temporary File name'
            DB  CR,LF
IFNDEF DEMO
            DB  '/udl:<name>    Use named D-format dynamic Library at runtime'
            DB  CR,LF
ENDIF
            DB  '/w0            Warnings generate an exit code of 0, instead of 1'
            DB  CR,LF
IFNDEF DEMO
            DB  '/wn            Warnings are Not displayed'
            DB  CR,LF
ENDIF
            DB  '/xp            use eXPanded memory (EMS) during link'
            DB  CR,LF
            DB  '/xt            use eXTended memory (XMS) during link'
            DB  CR,LF

            DB  CR,LF
            DB  '<Press any key to continue....>'
sumlen3     EQU $-sumlines3
sumlines4   DB  CR,LF
            DB  CR,LF
			DB	"OVERLAY OPTIONS:"
            DB  CR,LF
            DB  CR,LF
IFNDEF DEMO
            DB  '/cla           turn on CLArion overlay compatibility mode'
            DB  CR,LF
ENDIF
            DB  '/clp5[:<name>] automatically overlay CLiPper 5 compiled CLIPPER.LIB modules'
            DB  CR,LF
IFNDEF DEMO
            DB  "/oc:[.]<name>  Overlay Class name, leading '.' if exact match"
            DB  CR,LF
ENDIF
            DB  "/ohp:[-]<size> Overlay stasH in EMS exPanded memory, '-' leave free"
            DB  CR,LF
IFNDEF DEMO
            DB  "/ohp3:[-]<size>  Overlay stasH in EMS, forced version 3.0 compatibility"
            DB  CR,LF
ENDIF
            DB  "/oht:[-]<size> Overlay stasH in XMS exTended memory, '-' leave free"
            DB  CR,LF
            DB  '/oi            make Overlays Internal to EXE file'
            DB  CR,LF
            DB  '/ol:<count>    Overlay maximum Load count'
            DB  CR,LF
IFNDEF DEMO
            DB  '/on:<name>     Overlay file Name'
            DB  CR,LF
ENDIF
            DB  "/op:[-]<size>  Overlay Pool size in kilobytes, '-' leave free"
            DB  CR,LF
            DB  "/op:m          automatically use Overlay Pool Minimum required size"
            DB  CR,LF
            DB  "/orp           swap active Overlays at Runtime to EMS 4.0 exPanded memory"
            DB  CR,LF
            DB  "/ort           swap active Overlays at Runtime to exTended (XMS) memory"
            DB  CR,LF
IFNDEF DEMO
            DB  '/os:<size>     Overlay manager internal Stack size in kilobytes'
            DB  CR,LF
ENDIF
            DB  '/ou            attempt to place Overlay pool in Upper Memory Block (UMB)'
            DB  CR,LF
            DB  '/ox            place Overlay pool in eXpanded memory (EMS) page frame'
            DB  CR,LF
            DB  '/ox:e-var      Only use /OX if environment variable set to specified value'
            DB  CR,LF
            DB  '/r             Reload active overlays swapped out upon return at same address'
            DB  CR,LF

sumlen      EQU $-sumlines
sumlen4     EQU $-sumlines4

;*****************************
;* Code begins               *
;*****************************

.CODE

summary PROC
    mov ah,40h              ; write to file or device
    mov bx,STDOUT           ; write to standard output device
    mov dx,OFFSET DGROUP:sumlines   ; ds:dx == segment:offset of buffer area
    mov cx,sumlen1
    int 21h
    mov ah,8                ; character input without echo
    int 21h
    mov ah,40h              ; write to file or device
;***    mov bx,STDOUT           ; write to standard output device
    mov dx,OFFSET DGROUP:sumlines2
    mov cx,sumlen2          ; number of bytes to write
    int 21h

    mov ah,8                ; character input without echo
    int 21h
    mov ah,40h              ; write to file or device
;***    mov bx,STDOUT           ; write to standard output device
    mov dx,OFFSET DGROUP:sumlines3
    mov cx,sumlen3          ; number of bytes to write
    int 21h

    mov ah,8                ; character input without echo
    int 21h
    mov ah,40h              ; write to file or device
;***    mov bx,STDOUT           ; write to standard output device
    mov dx,OFFSET DGROUP:sumlines4
    mov cx,sumlen4          ; number of bytes to write
    int 21h
    ret
summary ENDP

END
