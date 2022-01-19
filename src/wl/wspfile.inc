;
;	wspfile.inc
;
;	Defines structure of WSP file.  Structures must match
;	those defined in wspfile.h.
;
;	D. Rifkind  8 Jan 91
;

; $Header: d:/warpspd/RCS/wspfile.inc 1.2 91/02/23 00:31:10 dave Exp Locker: dave $


FILEVER		equ	3		; File format version
PROFVER		equ	3		; Minimum profiler version


;
;	File header
;
;	This part of the header must be present for the profiler
;	to load the WSP file at runtime.  Its structure should
;	never change.
;

wspheader_t	struc
  wh_sig	db	4 dup (?)	; WSP signature
  wh_filever	dw	?		; File version number
  wh_profver	dw	?		; Minimum profiler version
  wh_hdrlen	dw	?		; Length of header
  wh_statlen	dd	?		; Stats table length
  wh_infolen	dd	?		; Info table length
wspheader_t	ends


;
;	Secondary header
;
;	This part of the header is version-dependant.  For
;	maximum compatibility, new information should only be
;	added to the end of the header.  Information added
;	elsewhere means that the minimum profiler version number
;	should change.
;

auxheader_t	struc
  ah_runfile	db	13 dup (?)	; Executable file name
  ah_nruns	dw	?		; Number of times run
  ah_clock	dw	?		; Clock multiplier
  ah_elapsed	dd	?		; Total elapsed time
  ah_runtime	dd	?		; Run time
  ah_proftime	dd	?		; Time lost to profiler
  ah_dostime	dd	?		; DOS/BIOS time
  ah_omtime	dd	?		; Overlay manager time
  ah_ovlcalls	dd	?		; Number of overlay calls
  ah_nloads	dd	?		; Number of overlay loads
  ah_nreloads	dd	?		; Number of overlay reloads
auxheader_t	ends


;
;	Statistics table header
;

statheader_t	struc
  sh_base	dw	?		; Address base segment
  sh_nsyms	dw	?		; Number of symbols
  sh_symptr	dw	?		; Offset to symbols
  sh_novsyms	dw	?		; Number of overlaid symbols
  sh_ovsymptr	dw	?		; Offset to overlaid symbols
  sh_novls	dw	?		; Number of overlays
  sh_ovlptr	dw	?		; Offset to overlays
statheader_t	ends


;
;	Statistics table entry
;
;	Times and counts are three-byte values.  24 bits is
;	enough for any reasonable program, and every byte saved
;	will help.
;

statentry_t	struc
  se_addr	dd	?		; entry address or offset/overlay
  se_flags	db	?		; Symbol flags
  se_saveop	db	?		; Save area for opcode
  se_ncalls	dd	?		; Number of calls
  se_fcntime	db	3 dup (?)	; Time in function
  se_cumtime	db	3 dup (?)	; Cumulative time
statentry_t	ends


;
;	Symbol flag bits
;

SF_MAGNO	equ	1Fh		; Overlay manager magic symbol number
SF_DATA		equ	20h		; Symbol is data address
SF_MAGIC	equ	40h		; Magic (overlay manager) symbol
SF_OVL		equ	80h		; Overlaid symbol


;
;	Overlay table entry
;

ovlentry_t	struc
  oe_nloads	dd	?		; Number of loads
  oe_nreloads	dd	?		; Number of reloads
ovlentry_t	ends


;
;	The remainder of this file is included for documentation
;	only, as the assembly portion should never have reason
;	to look at this part of the file.
;


;
;	Info table header
;

infoheader_t	struc
  ih_nmods	dw	?		; Number of modules
  ih_modptr	dw	?		; Offset to module table
  ih_strtablen	dd	?		; String table length
  ih_strtabptr	dw	?		; Offset to string table
infoheader_t	ends


;
;	Module table entry
;

modentry_t	struc
  me_nsyms	dw	?		; Number of symbols covered
modentry_t	ends


;	End of wspfile.h
