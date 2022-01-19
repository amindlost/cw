;*********************************************************************
;*   WLEQUATE.INC                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          05/14/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3f                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   General equates                                                 *
;*                                                                   *
;*********************************************************************

LINKER		EQU	"WL32"

; general use equates
STDIN	EQU 0			; standard input device
STDOUT	EQU 1			; standard output device
STDERR	EQU 2			; standard error device
BELL	EQU	7			; beep the speaker
CR		EQU	13			; carriage return
LF		EQU	10			; line feed
OFF		EQU	0
FALSE	EQU	0
ON		EQU	1
TRUE	EQU	1

MAXRSPFILENESTLEVEL	EQU	10
MAXOBJRECNAME	EQU	80h
LOWESTOBJRECTYPE	EQU	BLKDEF
HIGHESTOBJRECTYPE	EQU	MSLIBR

; flags used for response file translate lookups
FREEFORMATONLYFLAG	EQU	1
OBJNAMEFLAG	EQU	2
LIBNAMEFLAG	EQU	4
ISLIBFLAG	EQU	8
EXENAMEFLAG	EQU	10h
ADDOPTIONFLAG	EQU	20h
OPTIONFLAG	EQU	40h
FILEDELETEFLAG	EQU	80h
FLUSHLINEFLAG	EQU	100h
OBJADDFLAG	EQU	200h
LIBADDFLAG	EQU	400h
NOTCOMMANDFLAG	EQU	800h
FLUSHOPTIONFLAG	EQU	1000h
LIBSEARCHFLAG	EQU	2000h
OBJAPPENDFLAG	EQU	4000h
STACKABLEFLAG	EQU	8000h
EVARLINKFILEFLAG	EQU	10000h
FILEADDOBJFLAG	EQU	20000h
FILEADDLIBFLAG	EQU	40000h
PARAMETERFLAG	EQU	80000h

; option list equates
OPTLISTOFFTEXTPTR	EQU	0
OPTLISTOFFOPTPTR	EQU	2
OPTLISTOFFARGPTR	EQU	4
OPTLISTOFFARGFLAGS	EQU	6
OPTLISTOFFPARAMLOW	EQU	8
OPTLISTOFFPARAMHIGH	EQU	12
OPTLISTOFFEXTRAFLAG	EQU	16
OPTLISTSIZE		EQU	20

; object record type names
BLKDEF	EQU	 7ah		; non-Microsoft, ignore
BLKEND	EQU	 7ch		; non-Microsoft, ignore
DEBSYM	EQU	 7eh		; non-Microsoft, ignore

THEADR	EQU	 80h
LHEADR	EQU	 82h
COMENT	EQU	 88h		; sub-types exist
MODEND	EQU	 8ah
MODEND32	EQU	 8bh
EXTDEF	EQU	 8ch
TYPDEF	EQU	 8eh		; ignore
PUBDEF	EQU	 90h
PUBDEF32	EQU	 91h
LOCSYM	EQU	 92h		; non-Microsoft, ignore
LINNUM	EQU	 94h
LINNUM32	EQU	 95h
LNAMES	EQU	 96h
SEGDEF	EQU	 98h
SEGDEF32	EQU	99h
GRPDEF	EQU	 9ah
FIXUPP	EQU	 9ch
FIXUPP32	EQU	 9dh
LEDATA	EQU	 0a0h
LEDATA32	EQU	0a1h
LIDATA	EQU	 0a2h
LIDATA32	EQU	 0a3h
COMDEF	EQU	 0b0h
BAKPAT	EQU	0b2h
BAKPAT32	EQU	 0b3h
LEXTDEF1	EQU	 0b4h
LEXTDEF2	EQU	 0b5h
LPUBDEF	EQU 0b6h
LPUBDEF32	EQU 0b7h
LCOMDEF EQU	 0b8h
CEXTDEF	EQU	0bch
COMDAT	EQU	0c2h
COMDAT32	EQU	0c3h
LINSYM	EQU	0c4h
LINSYM32	EQU	0c5h
ALIAS	EQU	0c6h		; not supported
NBKPAT	EQU	0c8h
NBKPAT32	EQU	0c9h
LLNAMES	EQU	0cah

; COMENT record sub-types
COMENTLIBSEARCH1FLAG	EQU 81h	; default library search name type 1
COMENTLIBSEARCH2FLAG	EQU 9fh	; default library search name type 2
COMENTDOSSEGFLAG	EQU	9eh	; DOSSEG segment ordering
COMENTLINKPASSFLAG	EQU 0a2h	; linker pass two marker
COMENTLIBMODFLAG	EQU	0a3h	; library module name record
COMENTEXESTRFLAG	EQU	0a4h	; executable string record
COMENTWKEXTFLAG	EQU	0a8h	; weak extern
COMENTLZEXTFLAG	EQU	0a9h	; lazy extern
COMENTOMFEXT	EQU	0a0h	; OMF extensions, subtypes follow
COMENTPHARLAPFLAG	EQU	0aah	; special Phar Lap format module identifier
COMENTIMPDEF	EQU	1	; impdef (subtype of 0a0h)
COMENTEXPDEF	EQU	2	; expdef (subtypeof 0a0h)

; general library and object module record equates
MSLIBR	EQU	 0f0h		; MS library header record (aka MSLHED)
MSLHED	EQU	 0f0h		; MS library header record (aka MSLIBR)
MSEXTDICT	EQU 0f2h	; MS extended dictionary identifier byte
MSLEND	EQU	 0f1h		; MS library end record

; segment type info flag equates
GRANULARITY4K	EQU	(1 SHL 20)
CODESEGMENTTYPE	EQU	(0 SHL 21)
DATASEGMENTTYPE	EQU	(1 SHL 21)
STACKSEGMENTTYPE	EQU (2 SHL 21)
CONSTSEGMENTTYPE	EQU	(3 SHL 21)
FORCEDBITRESETFLAG	EQU	(1 SHL 25)
FORCEDBITSETFLAG	EQU	(1 SHL 26)
TRUEFLATSEGMENTFLAG		EQU	(1 SHL 27)

; object record equates
AFIELDOFACBP	EQU	0e0h
CFIELDOFACBP	EQU	1ch
BFIELDOFACBP	EQU	2
PFIELDOFACBP	EQU	1
COMMONSEGMENT	EQU	18h	; C field of segdef COMMON value

FIXUPSUBRECORD	EQU	80h	; first byte of fixupp record, flags fixup rather than thread
DBITOFTHREADDATA	EQU	40h
METHODOFTHREADDATA	EQU	1ch
TARGMETHOFTHREADDATA	EQU	0ch
THREADNUMOFTHREADDATA	EQU	3
FBITOFFIXDATA	EQU	80h
FRAMEFIELDOFFIXDATA	EQU	70h
TBITOFFIXDATA	EQU	8
PBITOFFIXDATA	EQU	4
TARGTFIELDOFFIXDATA	EQU	3
MBITOFLOCAT		EQU	40h
LOWORDERBYTELOC	EQU	0
OFFSETLOC	EQU	(1 SHL 2)
SEGMENTLOC	EQU	(2 SHL 2)
POINTERLOC	EQU	(3 SHL 2)
OFFSETLOC5	EQU	(5 SHL 2)
OFFSET32LOC	EQU	(9 SHL 2)
POINTER48LOC	EQU	(11 SHL 2)
OFFSET32LOC13	EQU	(13 SHL 2)
LOCFIELDOFLOCAT	EQU	3ch
STARTBITOFMODTYPE	EQU	40h

COMDATCONTINUEFLAG	EQU	1
COMDATITERATEDFLAG	EQU	2
COMDATLOCALFLAG		EQU	4
COMDATSELECTNOMATCH	EQU	0
COMDATSELECTPICKANY	EQU	10h
COMDATSELECTSAMESIZE	EQU	20h
COMDATSELECTEXACT	EQU	40h
COMDATALLOCEXPLICIT	EQU	0
COMDATALLOCFARCODE	EQU	1
COMDATALLOCFARDATA	EQU	2
COMDATALLOCCODE32	EQU	3
COMDATALLOCDATA32	EQU	4
COMDATALLOCTYPEFIELD	EQU	0fh
COMDATALIGNSEGDEF	EQU	0
COMDATALIGNBYTE		EQU	1
COMDATALIGNWORD		EQU	2
COMDATALIGNPARA		EQU	3
COMDATALIGN256		EQU	4
COMDATALIGNDWORD	EQU	5

EXPDEFORDINALBIT	EQU	80h

; 3P specific stuff
IMPDEFFIXUP16OFF	EQU	0
IMPDEFFIXUP32OFF	EQU	1
IMPDEFFIXUP16SEGOFF	EQU	2
IMPDEFFIXUP32SEGOFF	EQU	3
IMPDEFFIXUP16SEGONLY	EQU	4
IMPDEFSELFRELATIVEFLAG	EQU	40h
IMPDEFORDINALFLAG	EQU	80h

SIZEIOBUFFBLK	EQU	8000h
IOBUFFSYSVARSIZE	EQU	30h
FINALBLOCKFLAG	EQU	1
ISOS2MODFLAG	EQU	1000h
ISPHARLAPMODFLAG	EQU	2000h
ISCLIPPERMODFLAG	EQU	4000h
IGNOREBLOCKFLAG	EQU	8000h

; structure of system variables in each buffer header
IOBuffHeaderStruc	STRUC
	ibhsBlockSize	DW	?	; size of block, not counting sysvars
	ibhsChildPtr	DW	?	; pointer to child block, zero if none
	ibhsParentPtr	DW	?	; pointer to parent block, zero if none
	ibhsBlockID		DW	?	; count of block in chain, relative 0 (first==0)
	ibhsFileNamePtr	DD	?	; pointer to file name for i/o buffer
	ibhsModNamePtr	DD	?	; pointer to module name
	ibhsSegDefPtr	DD	?	; pointer to first individual segdef entry for module, if any (first module block only)
	ibhsPtrToGrpPtrs	DD	?	; pointer to base of group pointer table for module, if any (first module block only)
	ibhsPtrToSymPtrs	DD	?	; pointer to base of symbol pointer table for module, if any (first module block only)
	ibhsClipModPtr	DW	?	; pointer to clipper module entry
	ibhsSourceLinePtr	DW	?	; pointer to first source line block
	ibhsFlags	DW	?	; block flags
						; bit 0 set if final block
						; bit 13 set if Phar Lap format module (2nd pass)
						; bit 14 set if Clipper module
						; bit 15 set if ignore block (obj as lib)
IOBuffHeaderStruc	ENDS

PAGECOUNTLIBDICTBUFFBLK	EQU	63	; count of library pages in block, must be 2^n-1
LIBDICTBUFFSYSVARSIZE	EQU	512	; must be multiple of 512
SIZELIBDICTBUFFBLK	EQU	PAGECOUNTLIBDICTBUFFBLK*512+LIBDICTBUFFSYSVARSIZE

SIZELIBFLAGBLK	EQU	4096
USEALLMODULESFLAG	EQU	8000h

MODULEUSEDFLAG	EQU	1

; structure of system variables in each buffer header
LIBDictHeaderStruc	STRUC
	ldhsBlockSize	DW	?	; size of block, not counting sysvars
	ldhsChildPtr	DW	?	; pointer to child block, zero if none
	ldhsParentPtr	DW	?	; pointer to parent block, zero if none
	ldhsBlockID		DW	?	; count of block in chain, relative 0 (first==0)
	ldhsFileNamePtr	DD	?	; pointer to file name for dictionary buffer
	ldhsPagePtr		DW	?	; pointer to storage of pages
	ldhsPageOffPtr	DW	?	; pointer to storage of page offsets
	ldhsModSizePtr	DW	?	; pointer to storage of module sizes
	ldhsModBuffPtr	DW	?	; pointer to storage of module buffers
	ldhsModUsedCount	DW	?	; count of modules used in library
	ldhsModIDPtr	DW	?	; pointer to storage of used module IDs (based on ModuleCount)
	ldhsDictSize	DW	?	; size of library dictionary in 512-byte pages
	ldhsFlags	DW	?	; block flags
						; bit 0 set if one more modules used
LIBDictHeaderStruc	ENDS

MASTERSEGDEFSYSVARSIZE	EQU	10h
MAXCOUNTMASTERSEGDEFBLK	EQU	(4000h-MASTERSEGDEFSYSVARSIZE)/(SIZE MasterSegDefRecStruc)
SIZEMASTERSEGDEFBLK	EQU	(MAXCOUNTMASTERSEGDEFBLK*SIZE MasterSegDefRecStruc)+MASTERSEGDEFSYSVARSIZE

; structure of master segdef block
MasterSegDefBlkStruc	STRUC
	msdbCount	DW	?	; count of entries in block
	msdbNextPtr	DW	?	; pointer to next block, selector value
MasterSegDefBlkStruc	ENDS

COMMONSEGMENT	EQU	18h
STACKSEGMENT	EQU	14h
DWORDALIGNSEGMENT	EQU	0a0h
PAGEALIGNSEGMENT	EQU	80h
PARAALIGNSEGMENT	EQU	60h
WORDALIGNSEGMENT	EQU	40h
BYTEALIGNSEGMENT	EQU	20h
ABSOLUTESEGMENTFLAG	EQU	1
COMMONSEGMENTFLAG	EQU	2
STACKSEGMENTFLAG	EQU	4
GROUPMEMBERFLAG	EQU	8
SEGMENT32BITFLAG	EQU	10h
ASSOCIATEDDATAFLAG	EQU	20h
DEBUGTYPESSEGMENTFLAG	EQU	40h
DEBUGSYMBOLSSEGMENTFLAG	EQU	80h
DOSSEGFLAGBITS	EQU	700h
DOSSEGCODEFLAG	EQU	100h
DOSSEGNOTDGROUPFLAG	EQU	200h
DOSSEGBEGDATAFLAG	EQU	300h
DOSSEGOTHERDGROUPFLAG	EQU	400h
DOSSEGBSSFLAG	EQU	500h
DOSSEGSTACKFLAG	EQU	600h
CLARIONGLOBALDATAFLAG	EQU	800h
RESOLVEDSEGFLAG	EQU	8000h

; structure of master segdef record
MasterSegDefRecStruc	STRUC
	mssSegOffset	DD	?	; offset of segment from start of program/frame number if absolute
	mssSegLength	DD	?	; length of segment
	mssNamePtr		DD	?	; pointer to segment name
	mssClassPtr	DD	?	; pointer to class name of segment
	mssGroupPtr	DD	?	; pointer to owning group record, if any
	mssNextSegPtr	DD	?	; pointer to next segment in program image
	mssFirstIndSegPtr	DD	?	; pointer to first individual segment record of this segment
	mssLastIndSegPtr	DD	?	; pointer to last individual segment record of this segment
	mssHigherNamePtr	DD	?	; pointer to master segdef record with alphabetically higher name
	mssLowerNamePtr	DD	?	; pointer to master segdef record with alphabetically lower name
	mssSegmentID	DW	?	; segment identifier (0 -> number of nonabsolute segments-1)
	mssFlags	DW	?	; general flags
						; bit 0 set if absolute segment
						; bit 1 set if stack segment
						; bit 2 set if common segment
						; bit 3 set if member of a group
						; bit 4 set if 32-bit segment
						; bit 5 set if associated L?DATA record
						; bit 6 set if debugging segment $$TYPES
						; bit 7 set if debugging segment $$SYMBOLS
						; bit 8-10 used for DOSSEG type values
						; bit 11 set if CLARION global data segment
						; bit 15 set if resolved in resolution pass
	mssACBPByte	DB	?	; ACBP byte
	msspad		DB	?	; pad/reserved
MasterSegDefRecStruc	ENDS

INDSEGDEFSYSVARSIZE	EQU	10h
MAXCOUNTINDSEGDEFBLK	EQU	(4000h-INDSEGDEFSYSVARSIZE)/(SIZE IndSegDefRecStruc)
SIZEINDSEGDEFBLK	EQU	(MAXCOUNTINDSEGDEFBLK*SIZE IndSegDefRecStruc)+INDSEGDEFSYSVARSIZE

; structure of individual segdef block
IndSegDefBlkStruc	STRUC
	isdbCount	DW	?	; count of entries in block
	isdbNextPtr	DW	?	; pointer to next block, selector value
IndSegDefBlkStruc	ENDS

COMDATSEGMENTFLAG	EQU	4000h
CREATEDSEGMENTFLAG	EQU	8000h

; structure of individual segdef record
IndSegDefRecStruc	STRUC
	isdrMasterPtr	DD	?	; pointer to master segdef record
	isdrSegOffset	DD	?	; offset of individual segdef from segment start
	isdrSegLength	DD	?	; length of segment
	isdrNextIndSegPtr	DD	?	; pointer to next individual segment record
	isdrDataPtr	DD	?	; pointer to first associated L?DATA record, if any
	isdrModulePtr	DD	?	; pointer to obj base buffer or
						; pointer to owning COMDAT if COMDAT-based
	isdrFlags	DW	?	; general flags
						; bit 0 set if absolute segment
						; bit 1 set if stack segment
						; bit 2 set if common segment
						; bit 4 set if 32-bit segment
						; bit 5 set if associated L?DATA record
						; bit 14 set if created by COMDAT
						; bit 15 set if nonmodule-based, created during resolution
	isdrACBPByte	DB	?	; ACBP byte
	isdrpad		DB	?	; pad/reserved
IndSegDefRecStruc	ENDS

GRPPTRTABLESYSVARSIZE	EQU	10h
MAXCOUNTGRPPTRTABLEBLK	EQU	(1000h-GRPPTRTABLESYSVARSIZE)/4
SIZEGRPPTRTABLEBLK	EQU	(MAXCOUNTGRPPTRTABLEBLK*4)+GRPPTRTABLESYSVARSIZE

; structure of group pointer table block
GrpPtrTableBlkStruc	STRUC
	gptbCount	DW	?	; count of entries in block
	gptbNextPtr	DW	?	; pointer to next block, selector value
GrpPtrTableBlkStruc	ENDS

GRPDEFSYSVARSIZE	EQU	10h
MAXCOUNTGRPDEFBLK	EQU	(1000h-GRPDEFSYSVARSIZE)/(SIZE GrpDefRecStruc)
SIZEGRPDEFBLK	EQU	(MAXCOUNTGRPDEFBLK*SIZE GrpDefRecStruc)+GRPDEFSYSVARSIZE

; structure of grpdef block
GrpDefBlkStruc	STRUC
	gdbCount	DW	?	; count of entries in block
	gdbNextPtr	DW	?	; pointer to next block, selector value
GrpDefBlkStruc	ENDS

GRPOFFSETSETFLAG	EQU	1

; structure of grpdef record
GrpDefRecStruc	STRUC
	gdrGrpOffset	DD	?	; offset of group from start of program
	gdrGrpLen	DD	?	; length of group (all segments within group)
	gdrGrpNamePtr	DD	?	; pointer to group name
	gdrFirstSegPtr	DD	?	; pointer to first segment of group
	gdrGrpFlags	DW	?	; general flags
						; bit 0 set if group offset set
GrpDefRecStruc	ENDS

SYMPTRTABLESYSVARSIZE	EQU	10h
MAXCOUNTSYMPTRTABLEBLK	EQU	(4000h-SYMPTRTABLESYSVARSIZE)/4
SIZESYMPTRTABLEBLK	EQU	(MAXCOUNTSYMPTRTABLEBLK*4)+SYMPTRTABLESYSVARSIZE

; structure of symbol pointer table block
SymPtrTableBlkStruc	STRUC
	sptbCount	DW	?	; count of entries in block
	sptbNextPtr	DW	?	; pointer to next block, selector value
SymPtrTableBlkStruc	ENDS

RELOCENTRYSYSVARSIZE	EQU	10h
MAXCOUNTRELOCENTRYBLK	EQU	(4000h-RELOCENTRYSYSVARSIZE)/8
SIZERELOCENTRYBLK	EQU	(MAXCOUNTRELOCENTRYBLK*8)+RELOCENTRYSYSVARSIZE
; top half of block unused for 16-bit (EXE) fixups
;SIZE16RELOCENTRYBLK	EQU	(MAXCOUNTRELOCENTRYBLK*4)+RELOCENTRYSYSVARSIZE

; structure of relocation entries block
RelocEntryBlkStruc	STRUC
	rebCount	DW	?	; count of entries in block
	rebNextPtr	DW	?	; pointer to next block, selector value
RelocEntryBlkStruc	ENDS

PUBSYMSYSVARSIZE	EQU	10h
MAXCOUNTPUBSYMBLK	EQU	(4000h-PUBSYMSYSVARSIZE)/(SIZE PubSymRecStruc)
SIZEPUBSYMBLK	EQU	(MAXCOUNTPUBSYMBLK*SIZE PubSymRecStruc)+PUBSYMSYSVARSIZE

; structure of public symbol block
PubSymBlkStruc	STRUC
	psbCount	DW	?	; count of entries in block
	psbNextPtr	DW	?	; pointer to next block, selector value
PubSymBlkStruc	ENDS

ABSOLUTESYMBOLFLAG	EQU	1
GROUPRELSYMBOLFLAG	EQU	2
PUBLICSYMBOLFLAG	EQU	4
NEARCOMSYMBOLFLAG	EQU	8
FARCOMSYMBOLFLAG	EQU	10h
LOCALSYMBOLFLAG	EQU	20h
UNRESFEEDSYMBOLFLAG	EQU	40h
COMDATSYMBOLFLAG	EQU	80h
WEAKEXTERNFLAG		EQU	100h
;COULDBEWEAKFLAG		EQU	200h
PASS2COMDATFLAG		EQU	400h
COMDATCOMPLETEFLAG	EQU	800h
IMPORTSYMBOLFLAG	EQU	1000h
LAZYEXTERNFLAG		EQU	2000h

; structure of public symbol record
PubSymRecStruc	STRUC
	pssOffset	DD	?	; public symbol offset OR
						; length during pass 1 if communal (changed to offset at resolution)
	pssIndSegDefPtr	DD	?	; pointer to owning individual segdef entry OR
							; frame number if absolute symbol OR
							; if weak extern, pointer to default resolution public symbol record
							; OR impdef entry pointer if impdef symbol
	pssGrpDefPtr	DD	?	; pointer to owning group
	pssNamePtr	DD	?	; pointer to symbol name
	pssHigherNamePtr	DD	?	; pointer to symbol record with alphabetically higher name
	pssLowerNamePtr	DD	?	; pointer to symbol record with alphabetically lower name
	pssModuleCount	DD	?	; module count ID
	pssFlags	DW	?	; general flags
						; bit 0 set if absolute symbol, explicit or owning segment absolute
						; bit 1 set if group relative
						; bit 2 set if public
						; bit 3 set if near communal
						; bit 4 set if far communal
						; bit 5 set if local
						; bit 6 set if unresolved and feedback given
						; bit 7 set if COMDAT-based symbol
						; bit 8 set if weak extern
;						; bit 9 set if could be weak/lazy extern (not explicitly strong, not yet weak/lazy)
						; bit 10 set if COMDAT was pass2 processed
						; bit 11 set if COMDAT processing completed
						; bit 12 set if IMPORT symbol
						; bit 13 set if lazy extern
PubSymRecStruc	ENDS

COMDATSYSVARSIZE	EQU	10h
MAXCOUNTCOMDATBLK	EQU	(4000h-COMDATSYSVARSIZE)/(SIZE ComDatRecStruc)
SIZECOMDATBLK	EQU	(MAXCOUNTCOMDATBLK*SIZE ComDatRecStruc)+COMDATSYSVARSIZE

; structure of public symbol block
ComDatBlkStruc	STRUC
	cdbCount	DW	?	; count of entries in block
	cdbNextPtr	DW	?	; pointer to next block, selector value
ComDatBlkStruc	ENDS

SEGRELCOMDATFLAG	EQU	4
FARCODECOMDATFLAG	EQU	8
FARDATACOMDATFLAG	EQU	10h
LOCALCOMDATFLAG	EQU	20h

; structure of initialized communal data record
ComDatRecStruc	STRUC
	cdsLength	DD	?	; length of communal data
	cdsIndSegDefPtr	DD	?	; pointer to owning individual segdef entry OR
							; frame number if absolute symbol
	cdsCreatedSegDefPtr	DD	?	; pointer to created segdef entry for explicit comdats
	cdsGrpDefPtr	DD	?	; pointer to owning group
	cdsPubSymPtr	DD	?	; pointer to public symbol entry
	cdsRecordPtr	DD	?	; pointer to first comdat record instance
	cdsDataPtr	DD	?	; pointer to first data portion of record
	cdsFlags	DW	?	; general flags
						; bit 2 set if segment-specified relative
						; bit 3 set if far code
						; bit 4 set if far data
						; bit 5 set if local
	cdsRecordFlags	DB	?	; comdat record flags
	cdsAttributes	DB	?	; comdat attributes
	cdsAlign	DB	?	; comdat align type
	cdsPad		DB	?	; pad/reserved
ComDatRecStruc	ENDS

; structure of clipper module entry, followed by 5-byte per symbol entry info
ClipperModEntStruc	STRUC
	cmsModuleID	DD	?	; module ID
	cmsNextPtr	DW	?	; pointer to next Clipper module entry (selector value)
	cmsSymbolCount	DW	?	; count of symbols in module
	cmsPriorUnique	DW	?	; count of prior unique symbols
 	cmsSegDefIndex	DW	?	; SYMBOLS segdef index, relative 1
ClipperModEntStruc	ENDS

CLIPSYMSYSVARSIZE	EQU	10h
MAXCOUNTCLIPSYMBLK	EQU	(4000h-CLIPSYMSYSVARSIZE)/(SIZE ClipSymRecStruc)
SIZECLIPSYMBLK	EQU	(MAXCOUNTCLIPSYMBLK*SIZE ClipSymRecStruc)+CLIPSYMSYSVARSIZE

; structure of clipper symbol block
ClipSymBlkStruc	STRUC
	csbCount	DW	?	; count of entries in block
	csbNextPtr	DW	?	; pointer to next block, selector value
ClipSymBlkStruc	ENDS

; structure of clipper symbol record
ClipSymRecStruc	STRUC
	cssSymName	DB 10 DUP (?)	; symbol name
	cssSymFlags	DW	?	; symbol flags
	cssHighPtr	DD	?	; symbol high pointer
	cssLowPtr	DD	?	; symbol low pointer
	cssProcValue	DW	?	; symbol procedure value
	cssNonProcValue	DW	?	; symbol nonprocedure value
	cssFlags	DB	?	; general symbol flags
ClipSymRecStruc	ENDS

SOURCELINESYSVARSIZE	EQU	10h
MAXCOUNTSOURCELINEBLK	EQU	(1000h-SOURCELINESYSVARSIZE)/(SIZE SourceLineRecStruc)
SIZESOURCELINEBLK	EQU	(MAXCOUNTSOURCELINEBLK*SIZE SourceLineRecStruc)+SOURCELINESYSVARSIZE

; structure of source line block
SourceLineBlkStruc	STRUC
	ssbSegmentID	DW	?	; segment identifier (0 -> number of nonabsolute segments-1)
	ssbNextContPtr	DW	?	; pointer to next continuation block, selector value
	ssbNextSegPtr	DW	?	; pointer to next segment block, selector value
	ssbCount	DD	?	; count of entries in block
SourceLineBlkStruc	ENDS

; structure of source line record
SourceLineRecStruc	STRUC
	sssLineNumber	DD	?	; line number
	sssEIP		DD	?	; EIP value
SourceLineRecStruc	ENDS

SIZELEDATAPTRENTRY	EQU	10h	; size of LEDATA data entry

; structure of LEDATA data entries in pointer table
LEDATADataPtrStruc	STRUC
	ldpsSegmentIndex	DW	?
						DW	?	; reserved/padding
	ldpsDataStart	DD	?	; LEDATA start offset
	ldpsDataEnd		DD	?	; LEDATA end offset
	ldpsRecOffset	DW	?	; LEDATA record offset in i/o buffer for data bytes
	ldpsRecSel		DW	?	; LEDATA selector in i/o buffer for data bytes
LEDATADataPtrStruc	ENDS

EXPDEFSYSVARSIZE	EQU	10h
MAXCOUNTEXPDEFBLK	EQU	(4000h-EXPDEFSYSVARSIZE)/(SIZE EXPDEFRecStruc)
SIZEEXPDEFBLK	EQU	(MAXCOUNTEXPDEFBLK*SIZE EXPDEFRecStruc)+EXPDEFSYSVARSIZE

; structure of EXPDEF pointer block
EXPDEFBlkStruc	STRUC
	edbCount	DW	?	; count of entries in block
	edbNextPtr	DW	?	; pointer to next block, selector value
EXPDEFBlkStruc	ENDS

; structure of EXPDEF pointer record
EXPDEFRecStruc	STRUC
	edsExportedFlag	DB	?	; exported flag
	edsExportedNamePtr	DD	?	; pointer to exported name
	edsInternalNamePtr	DD	?	; pointer to internal name
	edsExportOrdinal	DW	?	; export ordinal
EXPDEFRecStruc	ENDS

IMPDEFSYSVARSIZE	EQU	10h
MAXCOUNTIMPDEFBLK	EQU	(4000h-IMPDEFSYSVARSIZE)/(SIZE IMPDEFRecStruc)
SIZEIMPDEFBLK	EQU	(MAXCOUNTIMPDEFBLK*SIZE IMPDEFRecStruc)+IMPDEFSYSVARSIZE

; structure of IMPDEF pointer block
IMPDEFBlkStruc	STRUC
	idbCount	DW	?	; count of entries in block
	idbNextPtr	DW	?	; pointer to next block, selector value
IMPDEFBlkStruc	ENDS

FLUSHIMPDEFFLAG	EQU	1
NEWMODULENAMEFLAG	EQU	2

; structure of IMPDEF pointer record
IMPDEFRecStruc	STRUC
	idsOrdinalFlag	DB	?	; ordinal flag
	idsInternalNamePtr	DD	?	; pointer to internal name
	idsModuleNamePtr	DD	?	; pointer to module name
	idsEntryIdentPtr	DD	?	; pointer to entry ident
	idsModuleNumber		DD	?	; module number
	idsFunctionNumber	DD	?	; function number/ordinal
	idsGeneralFlags	DB	?	; general bit-coded flags
							; bit 0, don't use this IMPDEF entry (resolved symbol)
							; bit 1, new use of module name
IMPDEFRecStruc	ENDS
