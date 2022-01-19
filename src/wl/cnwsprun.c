/**********************************************************************
 *                                                                    *
 *   File:          cnwsprun.c                                        *
 *   By:            David Rifkind, modifications by Michael Devore    *
 *   Date:          23 Feb 91, 5 Jan 93--MED                          *
 *   Model:         Compact                                           *
 *   Version:       1.00                                              *
 *   Compiler:      Borland C++ 2.0                                   *
 *   Environment:   MS/PC-DOS 2.00+                                   *
 *                                                                    *
 *   WarpSpeed runtime module initialization, WSP file generator.     *
 *   Reads a WarpLink map file, produces initial statistics file,     *
 *   invokes profiler runtime module.                                 *
 *                                                                    *
 *   While this can also be compiled with MSC 5.1, some changes       *
 *   to the runtime module would be needed.                           *
 *                                                                    *
 **********************************************************************/

/* $Header: d:/warpspd/RCS/wsprun.c 1.3 91/03/11 04:13:14 dave Exp Locker: dave $ */

/*
     Include files
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dos.h>
#ifdef __TURBOC__
#include <dir.h>
#endif

#include "version.h"
#include "wspfile.h"
#include "common.h"
#include "compat.h"


/*
     Manifest constants
*/

#define SEGTABGRAN  10        /* allocation granularity of segment table */
#define SYMTABGRAN  50        /* allocation granularity of symbol table */
#define MODTABGRAN  10        /* allocation granularity of module table */
#define IGNTABGRAN  10        /* allocation granularity of ignore table */

#define MAXALLOC    0xFF80    /* maximum size of a malloc() block */

#define MAXSEGS     (MAXALLOC / sizeof(struct seginfo))
#define MAXSYMS     (MAXALLOC / sizeof(struct syminfo))
#define MAXMODS     (MAXALLOC / sizeof(struct modinfo))
#define MAXSTATS    ((0xFFFF - sizeof(struct statheader)) / sizeof(struct statentry))

#define NOADDR      (-1L)     /* returned by getaddr() on failure */

#define SF_SEG      0x100     /* symbol is segment name */


/*
     Structure definitions
*/

struct seginfo {              /* information about segment: */
     char *name;              /*   segment name */
     char *module;            /*   module name */
     char *file;              /*   file name */
     unsigned long addr;      /*   segment address */
     unsigned len;            /*   segment size */
     unsigned ovl;            /*   overlay number */
     int isomclass;           /*   overlay manager class */
};

struct syminfo {              /* information about symbol: */
     char *name;              /*   symbol name */
     char *called;            /*   artificial name */
     struct seginfo *seg;     /*   segment record */
     unsigned long addr;      /*   entry point address */
     int flags;               /*   symbol flags */
};

struct modinfo {              /* information about module: */
     char *module;            /*   module name */
     unsigned symn;           /*   first symbol number */
     unsigned nsyms;          /*   number of symbols covered */
};

struct ignore {               /* files/modules/symbols to ignore: */
     char type;               /*   'f', 'm', 's' */
     char *name;              /*   name to ignore */
};


/*
     Local variables
*/

char runfile[128];            /* executable file name */
char mapfile[128];            /* map file name */
char wspfile[128];            /* WSP file name */
char cmdline[128];            /* command tail following executable name */

unsigned actstack = 100;      /* activation stack size */
int casesense = 0;            /* case sensitivity */
int nolibs = 1;               /* automatically exclude libraries */
int segnames = 0;             /* include segment name symbols */
int ovlonly = 0;              /* overlay statistics only */
int trimunder = 0;            /* trim leading underscore */
int nodatecheck = 0;          /* don't check file dates */

struct seginfo *segtab;       /* segment table */
int nsegs = 0;
struct syminfo *symtab;       /* symbol table */
int nsyms = 0;
struct modinfo *modtab;       /* module table */
int nmods = 0;
struct ignore *ignoretab;     /* ignore table */
int nignore = 0;

char codeclass[] = "CLARION";
char omclass[] = "$$_OVL_MANAGER";


/*
     Table of overlay manager symbols

     These are names of symbols defined in the overlay manager
     which have special meaning to the profiler.  Ones marked
     with the "data" flag are data, not code.
*/

struct {
     char *name;              /* symbol name */
     int data;                /* data, not code */
} omnames[] = {
     { "$$_ovl_prof_call",         0 },
     { "$$_ovl_prof_return",       0 },
     { "$$_ovl_load_overlay",      0 },
     { "$$_ovl_jump_target",       1 },
     { "$$_ovl_destination_id",    1 },
     { "$$_ovl_call_target",       1 },
     { "$$_ovl_source_id",         1 },
     { "$$_ovl_lookup_table",      1 },
     { "$$_ovl_load_start",        1 },
     { "$$_ovl_load_end",          1 },
     { "$$_ovl_in_manager",        1 },
     { "$$_ovl_is_calling",        1 },
};
#define N_OMNAMES   (sizeof(omnames) / sizeof(*omnames))


/*
     Function prototypes
*/

int main(void);
void options(void);
void usage(void);
void makewsp(void);
int getcontinue(void);
void readmap(void);
int segtab_fcmp(const void *p1, const void *p2);
int symtab_fcmp(const void *p1, const void *p2);
struct seginfo *whichseg(unsigned long addr, unsigned ovl);
long scanfor(FILE *fp, char *fn, char *match);
char *getident(char *buf, int limit, char *text);
unsigned long getaddr(char *s, char **sp);
void excludesyms(void);
void makemodtab(void);
int strmatch(char *s, char *p, int nocase);

void far profile(char far *runfile, char far *cmdline,
     char far *wspfile, unsigned actstack);


/*
     main
*/

int main(void)
{
     fputs("WarpSpeed Execution Profiler Clarion version " VERSION "\n", stderr);
     fputs("Copyright 1993 Michael Devore.  All rights reserved.\n", stderr);
	 fputs("(Original version written by Dave Rifkind)\n\n",stderr);

     options();

     fixname(runfile, ".exe");
     makewsp();

     fflush(stdout);
     fflush(stderr);
     _restorezero();     /* restore vectors trapped by TC */
     profile(runfile, cmdline, wspfile, actstack);

     /* not reached */
     return 0;
}


/*
     options

     We parse the command tail from the PSP rather than C's argv
     so that the command line passed to the child will not have
     been messed up by argument parsing.

     This function tries to be all things to all people.  It
     allows both DOS and UNIX-style options, or any crazy combin-
     ation of the two you can think of.
*/

void options(void)
{
     char tail[128], ident[128];
     char *opt, *s, c, ic;

     s = getcmdtail(tail);

     for (;;) {
          while (isspace(*s)) s++;
          if (*s != switchar() && *s != '-')
               break;    /* end of options */
          opt = s++;
          while (*s != '\0' && !isspace(*s) && *s != switchar()) {
               switch (c = *s++) {
               case 'a': case 'A':      /* activation stack size */
                    if (*s == ':') s++;
                    while (isspace(*s)) s++;
                    if (!isdigit(*s))
                         error("Missing activation stack size");
                    actstack = (unsigned)strtoul(s, &s, 10);
                    /* size sanity checked in profile() */
                    break;
               case 'i': case 'I':      /* include file, module, symbol */
                    switch (ic = *s++) {
                    case 'f': case 'F':
                    case 'm': case 'M':
                    case 's': case 'S':
                         break;
                    default:
                         error("Invalid switch: should be /if, /im, or /is");
                    }
                    if (*s == ':') s++;
                    while (isspace(*s)) s++;
                    if (*s == '\0' || *s == switchar() || *s == '-')
                         error("Missing name following \"%c%c%c\"", *opt, c, ic);
                    s = fncopy(ident, s);
                    if (nignore % IGNTABGRAN == 0)
                         ignoretab = mustrealloc(ignoretab, (nignore + IGNTABGRAN) * sizeof(*ignoretab));
                    ignoretab[nignore].type = toupper(ic);
                    ignoretab[nignore].name = lookup(ident, 1);
                    nignore++;
                    break;
               case 'l': case 'L':      /* include libraries */
                    nolibs = 0;
                    break;
               case 'm': case 'M':      /* map file name */
                    if (*s == ':') s++;
                    while (isspace(*s)) s++;
                    if (*s == '\0' || *s == switchar() || *s == '-')
                         error("Missing map file name");
                    s = fncopy(mapfile, s);
                    break;
               case 'n': case 'N':      /* add segment name symbols */
                    segnames = 1;
                    break;
               case 'o': case 'O':      /* generate no timing stats */
                    ovlonly = 1;
                    break;
               case 's': case 'S':      /* symbol names case sensitive */
                    casesense = 1;
                    break;
               case 'u': case 'U':      /* remove leading _ from symbols */
                    trimunder = 1;
                    break;
               case 'w': case 'W':      /* WSP file name */
                    if (*s == ':') s++;
                    while (isspace(*s)) s++;
                    if (*s == '\0' || *s == switchar() || *s == '-')
                         error("Missing WSP file name");
                    s = fncopy(wspfile, s);
                    break;
               case 'x': case 'X':      /* exclude file, module, symbol */
                    switch (ic = *s++) {
                    case 'f': case 'F':
                    case 'm': case 'M':
                    case 's': case 'S':
                         break;
                    default:
                         error("Invalid switch: should be /xf, /xm, or /xs");
                    }
                    if (*s == ':') s++;
                    while (isspace(*s)) s++;
                    if (*s == '\0' || *s == switchar() || *s == '-')
                         error("Missing name following \"%c%c%c\"", *opt, c, ic);
                    s = fncopy(ident, s);
                    if (nignore % IGNTABGRAN == 0)
                         ignoretab = mustrealloc(ignoretab, (nignore + IGNTABGRAN) * sizeof(*ignoretab));
                    ignoretab[nignore].type = tolower(ic);
                    ignoretab[nignore].name = lookup(ident, 1);
                    nignore++;
                    break;
               case 'y': case 'Y':      /* yes, just do it */
                    nodatecheck = 1;
                    break;
               default:
                    error("Unknown switch \"%c%c\"", *opt, c);
               }
          }
     }

     if (*s == '\0') {
          /* missing executable name */
          usage();
          exit(0);
     }

     s = fncopy(runfile, s);  /* copy executable name */
     strcpy(cmdline, s);      /* remainder is program command line */
}


/*
     usage

     Display usage message.
*/

void usage(void)
{
     puts("Usage: wsprun [options] runfile[.exe] [arguments...]");
     puts("\nOptions:");
     puts("     /a:size        activation stack size");
     puts("     /if:file       include symbols in file (wildcards okay)");
     puts("     /im:module     include symbols in module (wildcards okay)");
     puts("     /is:symbol     include symbol (wildcards okay)");
     puts("     /l             include all libraries");
     puts("     /m:mapfile     override map file name");
     puts("     /n             include segment name symbols");
     puts("     /o             gather overlay statistics only");
     puts("     /s             symbol names case-sensitive");
     puts("     /u             trim leading underscore from symbol names");
     puts("     /w:wspfile     override WSP file name");
     puts("     /xf:file       exclude symbols in file (wildcards okay)");
     puts("     /xm:module     exclude symbols in module (wildcards okay)");
     puts("     /xs:symbol     exclude symbol (wildcards okay)");
     puts("     /y             okay if map file is out of date");
}


/*
     makewsp

     Build WSP file from map file.
*/

void makewsp(void)
{
     struct stat runst, mapst;          /* for checking file dates */
     struct wspheader wspheader;        /* main WSP file header */
     struct auxheader auxheader;        /* auxiliary WSP file header */
     struct statheader statheader;      /* statistics section header */
     struct statentry statentry;        /* one statistics entry */
     struct ovlentry ovlentry;          /* one overlay entry */
     struct infoheader infoheader;      /* info section header */
     struct modentry modentry;          /* one module entry */
     FILE *wfp;                         /* output file */
     char file[MAXFILE];                /* executable file base name */
     char ext[MAXEXT];                  /* executable file extension */
     int ovsym;                         /* first overlaid symbol */
     int symn, modn, segn;              /* symbol, module, segment index */
     char *name;                        /* symbol or module name */
     int namelen;                       /* length of name */
     long infotab;                      /* ftell() position of info header */
     long strtablen;                    /* length of string table */
     int maxovl;                        /* highest overlay number */

     /* Phase One: Check that files exist, make sure map file is
        not out of date (or the user knows it if it is). */

     /* This is convoluted because stat() returns a strange error
        number if the file doesn't exist.  Call access() first,
        just so that _doserrno will make sense. */
     if (access(runfile, 4) != 0 || stat(runfile, &runst) != 0 ||
               !(runst.st_mode & S_IFREG))
          error("%s: %s", runfile, doserror(_doserrno));

     if (mapfile[0] == '\0')
          newname(strcpy(mapfile, runfile), ".map");
     else
          fixname(mapfile, ".map");

     /* Same logic as above, when checking executable file. */
     if (access(runfile, 4) != 0 || stat(mapfile, &mapst) != 0 ||
               !(mapst.st_mode & S_IFREG))
          error("%s: %s", mapfile, doserror(_doserrno));

     if (runst.st_mtime > mapst.st_mtime) {
          fputs("Warning: Executable file is newer than map file\n", stderr);
          if (!getcontinue()) {
               fputs("Aborting...\n", stderr);
               exit(1);
          }
     }

     /* Phase Two: Read and process map file. */

     readmap();
     excludesyms();
     makemodtab();

     /* Phase Three: Generate WSP file. */

     if (nsyms > MAXSTATS)
          error("Too many symbols in map file");

     fputs("Writing WSP file...\n", stderr);

     if (wspfile[0] == '\0')
          newname(strcpy(wspfile, mapfile), ".wsp");
     else
          fixname(wspfile, ".wsp");

     if ((wfp = fopen(wspfile, "wb")) == NULL)
          error("Can't create %s: %s", wspfile, doserror(_doserrno));

     /* Need to know number of overlays.  Don't look at the symbol
        table, because we may have excluded lots of symbols.  Find
        highest-numbered segment record. */
     for (maxovl = 0, segn = 0; segn < nsegs; segn++) {
          if (segtab[segn].ovl > maxovl)
               maxovl = segtab[segn].ovl;
     }

     /* Write WSP file header.  Some fields are left blank; when
        we're done, we'll go back and rewrite it. */
     memcpy(wspheader.sig, "WSP\x1A", 4);
     wspheader.filever = FILEVER;
     wspheader.profver = PROFVER;
     wspheader.hdrlen = sizeof(wspheader) + sizeof(auxheader);
     wspheader.statlen = sizeof(statheader) + sizeof(statentry) * nsyms + sizeof(ovlentry) * maxovl;
     wspheader.infolen = 0;
     fwrite(&wspheader, sizeof(wspheader), 1, wfp);

     /* Write auxiliary header.  Various overall stats recorded
        here must be initially zero. */
     memset(&auxheader, 0, sizeof(auxheader));
     fnsplit(runfile, NULL, NULL, file, ext);
     fnmerge(auxheader.runfile, NULL, NULL, file, ext);
     auxheader.clock = 1;
     fwrite(&auxheader, sizeof(auxheader), 1, wfp);

     /* Find first overlaid symbol.  This divides the stats table
        into non-overlaid and overlaid parts. */
     for (ovsym = 0; ovsym < nsyms; ovsym++) {
          if (symtab[ovsym].seg->ovl != 0)
               break;
     }

     /* Write stats table header, after precalculating lengths. */
     statheader.base = 0;
     statheader.nsyms = ovsym;
     statheader.symptr = sizeof(statheader);
     statheader.novsyms = nsyms - ovsym;
     statheader.ovsymptr = sizeof(statheader) + sizeof(statentry) * ovsym;
     statheader.novls = maxovl;
     statheader.ovlptr = sizeof(statheader) + sizeof(statentry) * nsyms;
     fwrite(&statheader, sizeof(statheader), 1, wfp);

     /* Write stats table, one record per symbol. */
     memset(&statentry, 0, sizeof(statentry));
     for (symn = 0; symn < nsyms; symn++) {
          if ((statentry.flags = (char)symtab[symn].flags) & SF_OVL) {
               /* Overlaid symbols get segment number and offset. */
               statentry.ao.o.off = (unsigned)symtab[symn].addr;
               statentry.ao.o.ovl = symtab[symn].seg->ovl;
          } else {
               /* Non-overlaid symbols get full 32-bit address. */
               statentry.ao.addr = symtab[symn].addr;
          }
          fwrite(&statentry, sizeof(statentry), 1, wfp);
     }

     /* Write overlay table.  Nothing special, just one blank entry
        per overlay. */
     memset(&ovlentry, 0, sizeof(ovlentry));
     for (segn = 0; segn < maxovl; segn++)
          fwrite(&ovlentry, sizeof(ovlentry), 1, wfp);

     /* Now the info table header.  The info table consists of
        module records and strings.  Record the table position so
        it can be rewritten after we know the size of the string
        table. */
     infotab = ftell(wfp);
     infoheader.nmods = nmods;
     infoheader.modptr = sizeof(infoheader);
     infoheader.strtablen = 0;
     infoheader.strtabptr = sizeof(infoheader) + sizeof(modentry) * nmods;
     fwrite(&infoheader, sizeof(infoheader), 1, wfp);

     /* We've precalculated the module table, so all we do now is
        write it out. */
     for (modn = 0; modn < nmods; modn++) {
          modentry.nsyms = modtab[modn].nsyms;
          fwrite(&modentry, sizeof(modentry), 1, wfp);
     }

     /* And now, the string table.  One string for each symbol,
        and one for each module record. */
     strtablen = 0;
     for (symn = 0; symn < nsyms; symn++) {
          /* "called" not used yet; just preparation for an idea
             that hasn't yet come to light. */
          name = (symtab[symn].called == NULL) ?
               symtab[symn].name : symtab[symn].called;
          strtablen += (namelen = strlen(name) + 1);
          fwrite(name, namelen, 1, wfp);
     }
     for (modn = 0; modn < nmods; modn++) {
          strtablen += (namelen = strlen(modtab[modn].module) + 1);
          fwrite(modtab[modn].module, namelen, 1, wfp);
     }

     /* Rewrite the info table header with correct string table
        size. */
     fseek(wfp, infotab, SEEK_SET);
     infoheader.strtablen = strtablen;
     fwrite(&infoheader, sizeof(infoheader), 1, wfp);

     /* Rewrite the file header with correct info table size. */
     fseek(wfp, 0L, SEEK_SET);
     wspheader.infolen = sizeof(infoheader) + sizeof(modentry) * nmods +
          strtablen;
     fwrite(&wspheader, sizeof(wspheader), 1, wfp);

     fclose(wfp);
}


/*
     getcontinue

     Map file is out of date.  See if the user is sure he wants
     to go on.
*/

int getcontinue(void)
{
     int c, yes;

     if (nodatecheck)    /* /y switch */
          return 1;

     fputs("Continue? [yn](n) ", stderr);
     fflush(stderr);

     /* Anything but "yes" is "no". */
     yes = ((c = getchar()) == 'y' || c == 'Y');

     /* Purge remainder of input line. */
     while (c != EOF && c != '\n')
          c = getchar();

     return yes;
}


/*
     readmap

     Read the map file, loading its contents into tables.
*/

void readmap(void)
{
     FILE *mfp;
     long lineno = 0;
     char line[256];
     char ident[128];
     char class[32];
     int isomclass;
     char base[MAXFILE];
     char ext[MAXEXT];
     int alloc;
     char *name;
     char *module;
     char *file;
     char *tok;
     unsigned long addr;
     unsigned len;
     unsigned ovl;
     struct seginfo *seg;
     int flags;
     int i, j;

     fputs("Reading map file...\n", stderr);

     if ((mfp = fopen(mapfile, "rb")) == NULL)
          error("Can't open %s: %s", mapfile, doserror(_doserrno));
     setvbuf(mfp, NULL, _IOFBF, 0x1000);

     /* Find and read the expanded segment map.
     */

     lineno += scanfor(mfp, mapfile, "Detailed Segment Map");
     fgets(line, sizeof(line), mfp);    /* skip header line */
     lineno++;

     while (fgets(line, sizeof(line), mfp) != NULL) {

          lineno++;

          /* It's impossible for a valid line to be shorter than
             86 characters, so let's just check now, shall we? */
          if (strlen(line) < 86)
               break;

          /* Name field: minimum width 16, may extend into
             overlay number field if overlay number is zero.
             Just hold onto the name for now, because if the
             class isn't CODE we don't want to store it. */
          tok = getident(ident, sizeof(ident), line);
          if (ident[0] == '\0')
               break;

          /* Overlay field blank if zero, may be omitted if name
             too long and overlay number zero.  Easiest way to
             check is to look for an address next, and assume
             it's an overlay number if malformed address. */
          ovl = 0;
          if (strlen(ident) <= 16) {
               if (isxdigit(line[17]))
                    ovl = (unsigned)strtoul(line + 17, NULL, 16);
               addr = getaddr(line + 22, &tok);
          } else if (strlen(ident) <= 21) {
               if (isxdigit(*(tok + 1)))
                    ovl = (unsigned)strtoul(tok + 1, &tok, 16);
               else
                    tok = line + 21;
               addr = getaddr(tok + 1, &tok);
          } else if ((addr = getaddr(tok + 1, &tok)) == NOADDR) {
               ovl = (unsigned)strtoul(tok + 1, NULL, 16);
               addr = getaddr(tok + 6, &tok);
          }

          /* Length field starts two characters after address. */
          len = (unsigned)strtoul(tok + 2, NULL, 16);

          /* Now skip 22 characters worth of fixed-width fields
             to get the class name.  Phew.  We're not interested
             unless the class is CLARION with _CODE segment name prefix
			 or class is $$_OVL_MANAGER. */
          tok = getident(class, sizeof(class), tok + 22) + 1;
          if (strcmp(class, codeclass) == 0 && strncmp(line,"_CODE",5) == 0)
               isomclass = 0;
          else if (strcmp(class, omclass) == 0)
               isomclass = 1;
          else
               continue;

          if (ovlonly && !isomclass && ovl == 0)
               continue;

          name = lookup(ident, 1);

          /* Now we're into minimum-starting-position fields.
             These fields may be shorter than expected if the
             preceding text was long enough to push the starting
             position out.  Group name first, position 62.  As
             it happens, we don't care about the group name. */
          if (tok - line < 62) tok = line + 62;
          tok = getident(ident, sizeof(ident), tok) + 1;

          /* Next is module name, starting offset 72.  We strip
             off the stuff we don't care about to get the base
             name, minus drive and directory. */
          if (tok - line < 72) tok = line + 72;
          tok = getident(ident, sizeof(ident), tok) + 1;
          fnsplit(ident, NULL, NULL, base, ext);
          fnmerge(ident, NULL, NULL, base, ext);
          module = lookup(ident, 1);

          /* Last comes the file name, starting offset 85.  We
             strip off the stuff we don't care about to get the
             base name, minus drive and directory, and convert
             to upper case. */
          if (tok - line < 85) tok = line + 85;
          tok = getident(ident, sizeof(ident), tok) + 1;
          fnsplit(ident, NULL, NULL, base, ext);
          fnmerge(ident, NULL, NULL, base, ext);
          strupr(ident);
          file = lookup(ident, 1);

          /* Jolly.  Now stuff all that garbage into a segment
             record. */

          if (nsegs == MAXSEGS)
               error("Too many code segments in map file");

          if (nsegs % SEGTABGRAN == 0) {
               if ((alloc = nsegs + SEGTABGRAN) > MAXSEGS)
                    alloc = MAXSEGS;
               segtab = mustrealloc(segtab, alloc * sizeof(*segtab));
          }

          segtab[nsegs].name = name;
          segtab[nsegs].module = module;
          segtab[nsegs].file = file;
          segtab[nsegs].addr = addr;
          segtab[nsegs].len = len;
          segtab[nsegs].ovl = ovl;
          segtab[nsegs].isomclass = isomclass;
          nsegs++;
     }

     /* Now sort the segment table.  Assume that segments never
        overlap, otherwise we'll never get anything done. */
     qsort(segtab, nsegs, sizeof(*segtab), segtab_fcmp);

     if (segnames && nsegs > 0) {

          alloc = nsegs + SYMTABGRAN - nsegs % SYMTABGRAN;
          symtab = mustalloc(alloc * sizeof(*symtab));

          for (i = j = 0; i < nsegs; i++) {
               if (segtab[i].isomclass)
                    continue;
               symtab[j].name = segtab[i].name;
               symtab[j].called = NULL;
               symtab[j].seg = &segtab[i];
               symtab[j].addr = segtab[i].addr;
               symtab[j].flags = SF_SEG | ((segtab[i].ovl != 0) ? SF_OVL : 0);
               if (ovlonly)
                    symtab[j].flags |= SF_DATA;
               j++;
          }

          nsyms = j;
     }

     /* Find and read the symbol table. */

     lineno += scanfor(mfp, mapfile, " Address   Status   Symbol Name");

     while (fgets(line, sizeof(line), mfp) != NULL) {

          lineno++;

          /* This section is easy--just address, overlay flag
             (or other stuff we don't care about), and name. */

          if ((addr = getaddr(line, &tok)) == NOADDR)
               break;
          if ((tok = strtok(tok, " \t\r\n")) == NULL)
               break;

          /* Only want resident code or overlaid symbols. */
          if (strcmp(tok, "Res") == 0)
               ovl = 0;
          else if (memcmp(tok, "Ovl", 3) == 0)
               ovl = (unsigned)strtoul(tok + 3, NULL, 16);
          else
               continue;

          /* Addresses that aren't in interesting segments are
             discarded here. */

          if ((seg = whichseg(addr, ovl)) == NULL)
               continue;

          if ((tok = strtok(NULL, " \t\r\n")) == NULL)
               break;
          name = lookup((trimunder && *tok == '_') ? tok + 1 : tok, 1);

          if (seg->isomclass) {
               for (i = 0; i < N_OMNAMES; i++) {
                    if (stricmp(omnames[i].name, name) == 0)
                         break;
               }
               if (i == N_OMNAMES)
                    continue;
               flags = SF_MAGIC | (omnames[i].data ? SF_DATA : 0) | (i & SF_MAGNO);
          } else {
               flags = (seg->ovl != 0) ? SF_OVL : 0;
               if (ovlonly)
                    flags |= SF_DATA;
          }

          if (nsyms == MAXSYMS)
               error("Too many symbols in map file");

          if (nsyms % SYMTABGRAN == 0) {
               if ((alloc = nsyms + SYMTABGRAN) > MAXSYMS)
                    alloc = MAXSYMS;
               symtab = mustrealloc(symtab, alloc * sizeof(*symtab));
          }

          symtab[nsyms].name = name;
          symtab[nsyms].called = NULL;
          symtab[nsyms].seg = seg;
          symtab[nsyms].addr = addr;
          symtab[nsyms].flags = flags;
          nsyms++;
     }

     qsort(symtab, nsyms, sizeof(*symtab), symtab_fcmp);

     if (ferror(mfp))
          error("Can't read %s: %s", mapfile, doserror(_doserrno));

     fclose(mfp);
}


/*
     segtab_fcmp

     Comparison function for sorting the segment table.  First
     resident segments, in address order, then overlaid segments
     by overlay number.  Note the grotesque contortions to avoid
     numeric overflow.
*/

int segtab_fcmp(const void *p1, const void *p2)
{
     if (((struct seginfo *)p1)->ovl < ((struct seginfo *)p2)->ovl)
          return -1;
     else if (((struct seginfo *)p1)->ovl > ((struct seginfo *)p2)->ovl)
          return 1;
     else if (((struct seginfo *)p1)->addr < ((struct seginfo *)p2)->addr)
          return -1;
     else if (((struct seginfo *)p1)->addr > ((struct seginfo *)p2)->addr)
          return 1;
     else
          return 0;
}


/*
     symtab_fcmp

     Comparison function for sorting the symbol table.  Like the
     segment table, it's sorted by overlay number and address.
     If there are both segment-name and normal symbols for the
     same address, the segment-name symbol sorts first so that
     it will be deleted when we go get rid of duplicates.
*/

int symtab_fcmp(const void *p1, const void *p2)
{
     if (((struct syminfo *)p1)->seg->ovl < ((struct syminfo *)p2)->seg->ovl)
          return -1;
     else if (((struct syminfo *)p1)->seg->ovl > ((struct syminfo *)p2)->seg->ovl)
          return 1;
     else if (((struct syminfo *)p1)->addr < ((struct syminfo *)p2)->addr)
          return -1;
     else if (((struct syminfo *)p1)->addr > ((struct syminfo *)p2)->addr)
          return 1;
     else if ((((struct syminfo *)p1)->flags ^ ((struct syminfo *)p2)->flags) & SF_SEG)
          return (((struct syminfo *)p1)->flags & SF_SEG) ? -1 : 1;
     else
          return 0;
}


/*
     whichseg

     Find the record for the segment containing a given address.
*/

struct seginfo *whichseg(unsigned long addr, unsigned ovl)
{
     int left, right, segn;
     struct seginfo *seg;

     left = 0;
     right = nsegs - 1;

     while (left <= right) {
          seg = &segtab[segn = (left + right) / 2];
          if (seg->ovl > ovl || (seg->ovl == ovl && seg->addr > addr))
               right = segn - 1;
          else if (seg->ovl < ovl || (seg->ovl == ovl && seg->addr + seg->len <= addr))
               left = segn + 1;
          else
               return seg;
     }

     return NULL;
}


/*
     scanfor

     Scan input file (the map file) looking for specified header
     line.  Returns the number of lines read.
*/

long scanfor(FILE *fp, char *fn, char *match)
{
     int matchlen = strlen(match);
     char line[128];
     long count = 0;

     while (fgets(line, sizeof(line), fp) != NULL) {
          if (strchr(line, '\n') != NULL)
               count++;
          if (strncmp(line, match, matchlen) == 0)
               break;
     }

     if (ferror(fp))
          error("Can't read %s: %s", fn, doserror(_doserrno));
     if (feof(fp))
          error("%s is not a valid WarpLink expanded map file", fn);

     return count;
}


/*
     getident

     Retrieve an identifier from the input line, starting at the
     specified position and continuing up to any white space.
     Leading spaces are not stripped, so if the first character
     is blank, the result will be empty.  Returns a pointer to
     the terminating character.  "limit" is the maximum length
     of the identifier, counting the final null character.
*/

char *getident(char *buf, int limit, char *text)
{
     while (*text != '\0' && !isspace(*text)) {
          if (limit > 1) {
               *buf++ = *text;
               limit--;
          }
          text++;
     }
     *buf = '\0';

     return text;
}


/*
     getaddr

     Convert a string in segment:offset form into an address.
     Returns NOADDR on malformed address.  Like strtol(),
     optionally passes back a pointer to the next character
     ONLY on success.
*/

unsigned long getaddr(char *s, char **sp)
{
     unsigned seg, off;

     if (!isxdigit(*s))
          return NOADDR;
     seg = (unsigned)strtoul(s, &s, 16);

     if (*s != ':' || (s++, !isxdigit(*s)))
          return NOADDR;
     off = (unsigned)strtoul(s, &s, 16);

     if (sp != NULL) *sp = s;
     return ((unsigned long)seg << 16) + off;
}


/*
     excludesyms
*/

void excludesyms(void)
{
     int i, j, k;
     int include;

     for (i = j = 0; i < nsyms; i++) {
          if (i + 1 == nsyms || symtab[i].addr != symtab[i + 1].addr ||
                    symtab[i].seg->ovl != symtab[i + 1].seg->ovl) {
               include = nolibs ?
                    !strmatch(symtab[i].seg->file, "*.lib", 1) : 1;
               for (k = 0; k < nignore; k++) {
                    switch (ignoretab[k].type) {
                    case 'f': case 'F':
                         if (strmatch(symtab[i].seg->file,
                                   ignoretab[k].name, 1))
                              include = isupper(ignoretab[k].type);
                         break;
                    case 'm': case 'M':
                         if (strmatch(symtab[i].seg->module,
                                   ignoretab[k].name, 1))
                              include = isupper(ignoretab[k].type);
                         break;
                    case 's': case 'S':
                         if (strmatch(symtab[i].name,
                                   ignoretab[k].name, !casesense))
                              include = isupper(ignoretab[k].type);
                         break;
                    }
               }
               if (include)
                    symtab[j++] = symtab[i];
          }
     }

     nsyms = j;
}


/*
     makemodtab

     Build the module table from the symbol table.
*/

void makemodtab(void)
{
     int symn;
     int alloc;

     for (symn = 0; symn < nsyms; ) {

          if (nmods == MAXMODS)
               error("Too many module records");  /* hardly likely */

          if (nmods % MODTABGRAN == 0) {
               if ((alloc = nmods + MODTABGRAN) > MAXMODS)
                    alloc = MAXMODS;
               modtab = mustrealloc(modtab, alloc * sizeof(*modtab));
          }

          modtab[nmods].module = symtab[symn].seg->module;
          modtab[nmods].symn = symn;

          while (++symn < nsyms) {
               if (symtab[symn].seg->module != modtab[nmods].module)
                    break;
          }

          modtab[nmods].nsyms = symn - modtab[nmods].symn;
          nmods++;
     }
}


/* end of wsprun.c */
