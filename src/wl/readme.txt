WARPLINK SOURCE FILE DOCUMENTATION


GENERAL AND LEGAL NOTES

As of November 4, 1999, Michael Devore surrendered all his
copyrights for WarpLink and released it to the public domain.
A few optional-use components of the WarpLink development files
retain their separate third party copyrights and are not part
of the release of the WarpLink software to public domain.

You may see several copyright notices within the source files.
When Michael Devore purchased the WarpLink property from
hyperkinetix in early 1993, copyrights for the files contained
in this distribution were assigned to Michael Devore as part of
the WarpLink property.  They were previously done as a work for
hire by other programmers, internally developed, or developed
on a per-copy royalty basis by Michael Devore.

There is no support or warranty whatsoever for the released
files.  Under no conditions will Michael Devore be held liable
for damages arising out of use of, or inability to use, the
WarpLink files. You may send Devore Software & Consulting e-mail
to freesource@devoresoftware.com with any questions, but those
questions will, of necessity, have lower priority than
paid customer/client inquiries and development works in
progress.  Such questions may be answered only after a long
delay or not at all. The small minority of individuals who may
feel that they deserve priority attention by calling, faxing, or
sending questions to other e-mail addresses will probably be
disappointed in the results, at best.

This is a true public domain release.  It is not Yet Another
Open Source licensing arrangement.  You may use the binary and
source files in whatever manner you desire, INCLUDING for
commercial purposes, without explicit credit or compensation to
Michael Devore.


COMMENTS ON THE SOURCE CODE

Most of the WarpLink code was written over the time period of
1989 through 1993. The operating system and programming
landscape was much different then, so this code probably needs
updates and modifications to work with later date software
versions.  Given the age, the programming team's learning
curves, the then cutting-edge technology, and fairly substantial
codebase involved, there doubtless exist operational errors and
examples of what is now considered poor code.  Please keep the
historic perspective in mind if you choose to use or study the
software and source code.

If you wish to rebuild the source files, rather than use the
current binary release, you are a bit on your own.  Someway,
somehow, I have successfully lost the batch files which build
binaries from the source files.  In most cases, this should not
matter. The majority of utilities are simple compile and link,
and the WarpLink executable is built the same way.  The linker
response file to build the WarpLink executable is included.

Two entities, specifically WarpWrap and DLLMGR files require an
extra step of building a binary image and incorporating it.
Specifics are left as an exercise for the reader (translation:
it's too long ago for me to remember exactly how it was done).
I could be wrong, but no source code strikes me as particularly
difficult to use to recreate the original binary, with possible
exception of the (little used) DDL managers.

The original files were assembled with TASM 2.0 or 2.5 in MASM
emulation, and Turbo C++ version 1.0 and 2.0.  A spot-check of
the assembly files shows that MASM 6.13 doesn't have a problem
compiling them.  In the rare cases where they may be an
incompatibility due to structure member naming or the like, it
should be simple to work around the incompatibility with a
slight modification to the assembly language structure or code.

Turbo C++ compiled utilities may be more problematic, unless you
have Turbo or Borland C, since some runtime specific library
routines were used for text output.  Still, those changes should
also be fairly minor if you have basic DOS text manipulation
capability with your compiler.

Finally, a naming note.  WarpLink was originally called Machlink
when it was in development before initial release.  That's why you
see all those MACHLINK and ML*.ASM files.


DISTRIBUTION FILES

Following is the full directory file list with a brief
description on what the file is for.

Main WarpLink executable files:

MACHLINK ASM        12,307  11-06-92  1:15p MACHLINK.ASM
MLCLEAN  ASM         3,232  01-21-91  4:51p MLCLEAN.ASM
MLCLIP   ASM       107,337  02-12-93  2:52a MLCLIP.ASM
MLCOMM   ASM        31,323  12-22-92 11:43a MLCOMM.ASM
MLCREDIT ASM         1,972  05-11-93  7:39a MLCREDIT.ASM
MLDATA   INC         6,523  12-21-92  5:00p MLDATA.INC
MLDDL1   ASM       134,811  04-17-93  4:04a MLDDL1.ASM
MLDDL2   ASM        23,842  04-04-93 11:55p MLDDL2.ASM
MLEQUATE INC        19,044  04-28-93  2:25a MLEQUATE.INC
MLERRMES INC        60,203  02-10-93  4:47p MLERRMES.INC
MLERROR  ASM        46,981  01-01-93  4:23p MLERROR.ASM
MLGETARG ASM         2,587  12-02-90  4:42a MLGETARG.ASM
MLGLOBAL INC        21,061  12-21-92  5:00p MLGLOBAL.INC
MLIMAGE1 ASM        65,156  02-13-93  4:12a MLIMAGE1.ASM
MLIMAGE2 ASM        63,391  12-31-92  3:02p MLIMAGE2.ASM
MLLIB1   ASM        54,094  02-04-93  2:52p MLLIB1.ASM
MLLIB2   ASM        23,944  12-31-92  4:02p MLLIB2.ASM
MLMAP    ASM        43,061  12-21-92  4:58p MLMAP.ASM
MLMEMORY ASM        27,712  11-08-92  9:16p MLMEMORY.ASM
MLOVL1   ASM        46,867  04-22-93  2:57p MLOVL1.ASM
MLOVLFIL ASM       100,353  12-31-92  4:37p MLOVLFIL.ASM
MLOVLRES ASM        49,103  12-22-92 11:16a MLOVLRES.ASM
MLPAROVL ASM        19,624  11-08-92 11:30p MLPAROVL.ASM
MLPARSE  ASM        92,960  04-17-93  4:04a MLPARSE.ASM
MLPASS1  ASM        10,501  09-28-92  2:00a MLPASS1.ASM
MLPASS1A ASM        74,942  05-10-93  7:39a MLPASS1A.ASM
MLPASS1B ASM        60,678  12-21-92  4:49p MLPASS1B.ASM
MLPASS1C ASM        13,485  07-11-91 12:46a MLPASS1C.ASM
MLPASS2  ASM        19,254  07-10-92  1:37p MLPASS2.ASM
MLPASS2A ASM        12,130  05-10-93  6:57a MLPASS2A.ASM
MLPASS2B ASM        41,753  05-10-93  7:03a MLPASS2B.ASM
MLPASS2C ASM        28,935  03-12-93  9:10a MLPASS2C.ASM
MLPASS2D ASM        57,571  03-12-93  9:39a MLPASS2D.ASM
MLPASS2E ASM        14,171  04-03-92  5:25a MLPASS2E.ASM
MLQUICK  ASM        17,520  01-01-93  4:22p MLQUICK.ASM
MLSETUP  ASM         7,049  01-01-93  4:23p MLSETUP.ASM
MLSHARED ASM        28,185  12-22-92  2:04p MLSHARED.ASM
MLSUM    ASM         8,385  04-06-93  8:35p MLSUM.ASM
MLSYMTOK INC        10,890  07-11-92  1:47p MLSYMTOK.INC

---

Linker response files, used to link WarpLink OBJ files to
final executable:

MS Link or Tlink
WARPLINK RSP           318  12-22-92 11:26a WARPLINK.RSP

WarpLink (builds itself)
WL       RSP           307  07-11-93  2:32a WL.RSP

--

DDLMGR data files

Clipper 5:
C5DDLMG1 ASM       168,568  04-21-93  2:09a C5DDLMG1.ASM
C5DDLMG2 ASM       202,780  04-21-93 11:18p C5DDLMG2.ASM

Clarion 2.1:
CNDDLMG1 ASM       193,094  04-24-93 10:39a CNDDLMG1.ASM
CNDDLMG2 ASM       203,469  04-24-93  9:08a CNDDLMG2.ASM

Standard (all other compatible languages)
DDLMGR1  ASM       157,548  04-21-93  2:10a DDLMGR1.ASM
DDLMGR2  ASM       200,499  04-21-93 11:13p DDLMGR2.ASM

?This may be used to generate the appropriate DDLMGR.DAT
file.  It is a two-step process.
LOADER   ASM        12,361  03-27-91 10:35p LOADER.ASM

---

Overlay manager files

Clipper 5
C5OVLMGR ASM       169,020  03-16-93 11:29a C5OVLMGR.ASM

Clarion 2.1
CNOVLMGR ASM       190,998  02-01-94  8:05p CNOVLMGR.ASM

Quicksilver
QSOVLMGR ASM       158,175  04-28-93  1:08a QSOVLMGR.ASM

Standard (all other languages)
OVLMGR   ASM       157,439  03-16-93 11:28a OVLMGR.ASM

Clarion 2.1, OEM version
LPM_VM   ASM       190,150  08-12-93 10:01a LPM_VM.ASM

Alternate Clarion 2.1, OEM version, so-called reciprocating
mode overlay manager
LPM_RM   ASM       179,647  08-12-93 10:03a LPM_RM.ASM

Debugging version of Clarion overlay manager
CNDEBUG  ASM       187,478  02-24-93 10:53p CNDEBUG.ASM

Used to output data from debugging version of overlay
managers
DUMPER   C           1,336  02-24-93 10:43p DUMPER.C

---

Utility files:

WarpConv utility
WARPCONV C          60,225  01-30-94  5:48p WARPCONV.C

WarpHog utility
WARPHOG  C           3,337  04-06-93  4:35a WARPHOG.C

WarpMod utility
WARPMOD  C          66,468  10-15-94  2:34a WARPMOD.C

WarpSpeed Profiler
CNWSPRUN C          36,582  04-13-93  4:14a CNWSPRUN.C
COMMON   C           4,914  02-13-91 10:20p COMMON.C
PROFILE  ASM        74,148  03-10-91  9:27p PROFILE.ASM
STRMATCH ASM         3,351  01-05-93  3:00p STRMATCH.ASM
WSPFILE  INC         3,292  02-23-91 12:31a WSPFILE.INC
WSPRUN   C          36,491  04-13-93  4:14a WSPRUN.C
WSPSTAT  C          29,866  04-13-93  4:14a WSPSTAT.C

Stand-alone Clipper symbol packer, SP.EXE
WOVL50   ASM        33,109  06-30-91  9:20p WOVL50.ASM
WP50     ASM       143,715  07-12-93  5:02p WP50.ASM
WPSCR50  ASM        53,623  07-12-93  5:04p WPSCR50.ASM

?Looks like a small utility to dump overlay file information
about Clarion overlay files
CNDUMP   C           4,747  04-26-93 12:17a CNDUMP.C

WarpWrap executable.  WRAPCODE.ASM must be assembled and then
a binary data image copied to end of WW.EXE compiled from WW.C.
WRAPCODE ASM        22,580  06-16-93 10:03p WRAPCODE.ASM
WW       C          20,387  03-18-96 11:13p WW.C

---

Example files:

ERRHAND  ASM         4,874  03-27-93  1:46p ERRHAND.ASM
ERRHAND1 C           4,937  03-27-93  1:30p ERRHAND1.C
ERRHAND2 C              83  10-31-90  1:36a ERRHAND2.C
SHOWEVAR ASM           975  11-11-93 10:34p SHOWEVAR.ASM

---

Unknown:

?Probably test or work files, but in case they are needed
somewhere, they were left them in.
WLEQUATE INC         2,843  05-17-93  4:54p WLEQUATE.INC
WLGLOBAL INC         1,365  05-17-93  4:56p WLGLOBAL.INC
WLSYSTEM ASM         7,488  05-17-93  4:56p WLSYSTEM.ASM

---
