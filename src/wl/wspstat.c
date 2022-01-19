/**********************************************************************
 *                                                                    *
 *   File:          wspstat.c                                         *
 *   By:            David Rifkind                                     *
 *   Date:          28 Jan 91                                         *
 *   Model:         Compact                                           *
 *   Version:       1.00                                              *
 *   Compiler:      Turbo C 2.0/Turbo C++ 1.0                         *
 *   Environment:   MS/PC-DOS 2.00+                                   *
 *                                                                    *
 *   WarpSpeed profiler statistics output module.                     *
 *                                                                    *
 **********************************************************************/

/* $Header: d:/warpspd/RCS/wspstat.c 1.3 91/03/11 04:13:34 dave Exp Locker: dave $ */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#ifdef __TURBOC__
#include <dir.h>
#endif

#include "version.h"
#include "wspfile.h"
#include "common.h"
#include "compat.h"


/******************************
 *   Manifest constants       *
 ******************************/

#define MAXALLOC    0xFF80    /* maximum size of a malloc() block */

#define SORT_TIME   1         /* sort by function time */
#define SORT_CALLS  2         /* sort by number of calls */
#define SORT_AVG    3         /* sort by average time */
#define SORT_CUM    4         /* sort by cumulative time */

#define MAXHDR      2         /* maximum lines of header */


/******************************
 *   Structure definitions    *
 ******************************/

struct dbfhdr {               /* DBF header structure: */
     unsigned char version;   /*   file version */
     unsigned char update[3]; /*   last update (not used) */
     long nrecs;              /*   number of records */
     int hdrlen;              /*   length of header */
     int reclen;              /*   length of record */
     char reserved[20];       /*   reserved */
};

struct rechdr {               /* DBF record header: */
     char name[11];           /*   field name */
     char type;               /*   field type */
     long daddr;              /*   internal use */
     unsigned char fldlen;    /*   field length */
     unsigned char flddec;    /*   field decimals */
     char reserved[14];       /*   reserved */
};


/******************************
 *   Local variables          *
 ******************************/

char wspfile[128];            /* WSP file name */
char profile[128];            /* profile file name (no pun intended) */

FILE *pfp;                    /* output file pointer */

struct wspheader wspheader;   /* WSP file header */
struct auxheader auxheader;   /* WSP file auxiliary header */
struct statheader statheader; /* WSP file statistics block header */
struct infoheader infoheader; /* WSP file info block header */
struct statentry *stattab;    /* statistics table */
int *statptr;                 /* sorted statistics table */
int totsyms;                  /* total number of symbols */
struct ovlentry *ovltab;      /* overlays table */
struct modentry *modtab;      /* modules table */
char **strtab;                /* string table */

int showtimes = 1;            /* show timing statistics */
int showovls = 1;             /* show overlay statistics */
int showmods = 0;             /* show module names */
int showcalls = 1;            /* show number of calls */
int showfcntime = 1;          /* show function time */
int showaverage = 0;          /* show average time per call */
int showcumtime = 0;          /* show cumulative time */
int showpercent = 0;          /* show percentage of total */
int showzero = 0;             /* show never-called functions */
int tostd = 0;                /* send output to stdout */
int todbf = 0;                /* generate DBF file */
int symwidth = 16;            /* symbol name width */
int sortby = SORT_TIME;       /* sort criterion */
int leftmgn = 0;              /* left margin */
int pagelen = 66;             /* page length */

int fmtcol = 0;               /* output column including indent */
int curline = 0;              /* output line number (top line = 1) */
int curpage = 0;              /* output page number */

char hdrlines[MAXHDR][256];   /* page header */
int nhdrlines = 0;            /* number of lines of header defined */

struct dbfhdr dbfhdr = {      /* DBF file header */
     3, { 0, 0, 0 }, 0L, 0, 0
};

struct rechdr fields[] = {    /* DBF field definitions */
     { "NAME",      'C',      0L,       0,        0 },
     { "MODULE",    'C',      0L,       12,       0 },
     { "RECTYPE",   'C',      0L,       1,        0 },
     { "NCALLS",    'N',      0L,       8,        0 },
     { "FCNTIME",   'N',      0L,       10,       3 },
     { "CUMTIME",   'N',      0L,       10,       3 },
};
#define N_FIELDS    (sizeof(fields) / sizeof(*fields))


/******************************
 *   Function prototypes      *
 ******************************/

int main(void);
void options(void);
void usage(void);
void readwsp(void);
void wsperror(unsigned nread);
void writedbf(void);
void writerec(char *name, char *mod, char type, long ncalls, long fcntime, long cumtime);
void writeheader(void);
void writestats(void);
int stat_fcmp(const void *p1, const void *p2);
void writeovlstats(void);
long b24(char *p24);
double seconds(long ticks);
int pprintf(char *fmt, ...);
void pputs(char *s);
void pputc(int c);
void eject(void);
void outc(int c);


/********************
 *   main           *
 ********************/

int main(void)
{
     fputs("WarpSpeed Profiler Statistics Generator version " VERSION "\n", stderr);
     fputs("Copyright 1993 Michael Devore.  All rights reserved.\n", stderr);
	 fputs("(Original version written by Dave Rifkind)\n\n",stderr);

     options();     /* process options */

     readwsp();     /* read WSP file */
     if (auxheader.nruns == 0)
          error("No timing data in file");

     if (tostd && !todbf) {
          /* Output to stdout (not allowed for DBFs). */
          pfp = stdout;
          fputs("Writing statistics to standard output...\n", stderr);
     } else {
          /* Output to file.  Generate default filename if not
             specified on command line. */
          if (profile[0] == '\0')
               newname(strcpy(profile, wspfile), todbf ? ".dbf" : ".wss");
          else
               fixname(profile, todbf ? ".dbf" : ".wss");
          if ((pfp = fopen(profile, todbf ? "wb" : "w")) == NULL)
               error("Can't open %s: %s", profile, doserror(_doserrno));
          fprintf(stderr, "Writing statistics to %s...\n", profile);
     }

     if (todbf) {
          writedbf();              /* output DBF */
     } else {
          writeheader();           /* overall statistics */
          if (showtimes)
               writestats();       /* by-function statistics */
          if (showovls)
               writeovlstats();    /* overlay load/reload statistics */
          if (curline > 0 && pagelen > 0)
               eject();            /* finish off page */
     }

     if (pfp != stdout) {
          _doserrno = 0;
          if (fclose(pfp) == EOF)
               error("Can't write %s: %s", profile, _doserrno ? doserror(_doserrno) : "Disk full");
     }

     return 0;
}


/********************
 *   options        *
 ********************

     We parse the command tail from the PSP rather than C's argv
     so that the command line passed to the child will not have
     been messed up by argument parsing.

     This function tries to be all things to all people.  It
     allows both DOS and UNIX-style options, or any crazy combin-
     ation of the two you can think of.
*/

void options(void)
{
     char tail[128];
     char *opt, *s, c;

     s = getcmdtail(tail);

     for (;;) {
          while (isspace(*s)) s++;
          if (*s == '\0')     /* end of command line */
               break;
          if (*s == switchar() || *s == '-') {
               /* Got a switch character, now process switch. */
               opt = s++;
               while (*s != '\0' && !isspace(*s) && *s != switchar()) {
                    switch (c = *s++) {
                    case 'a': case 'A':      /* report average times */
                         showaverage = 1;
                         break;
                    case 'c': case 'C':      /* output to console */
                         tostd = 1;
                         break;
                    case 'd': case 'D':      /* output to DBF */
                         todbf = 1;
                         break;
                    case 'l': case 'L':      /* left margin */
                         switch (*s++) {
                         case 'm': case 'M':
                              if (*s == ':') s++;
                              while (isspace(*s)) s++;
                              if (!isdigit(*s))
                                   error("Missing left margin");
                              leftmgn = (int)strtol(s, &s, 10);
                              if (leftmgn < 0 || leftmgn > 80)
                                   error("Invalid left margin");
                              break;
                         default:
                              error("Invalid switch: should be /lm");
                         }
                         break;
                    case 'm': case 'M':      /* include module names */
                         showmods = 1;
                         break;
                    case 'n': case 'N':      /* symbol name width */
                         if (*s == ':') s++;
                         while (isspace(*s)) s++;
                         if (!isdigit(*s))
                              error("Missing symbol name width");
                         symwidth = (int)strtol(s, &s, 10);
                         if (symwidth < 4)
                              error("Symbol name width too small (minimum 4)");
                         else if (symwidth > 128)
                              error("Symbol name width too large (maximum 128)");
                         break;
                    case 'o': case 'O':      /* overlay stats only */
                         showtimes = 0;
                         showovls = 1;
                         break;
                    case 'p': case 'P':      /* either /p or /pl */
                         if (*s == 'l' || *s == 'L') {
                              if (*++s == ':') s++;
                              while (isspace(*s)) s++;
                              if (!isdigit(*s))
                                   error("Missing page length");
                              pagelen = (int)strtol(s, &s, 10);
                              if (pagelen < 0 || (pagelen > 0 && pagelen < 10))
                                   error("Invalid page length");
                         } else {
                              showpercent = 1;
                         }
                         break;
                    case 's': case 'S':
                         switch (*s++) {
                         case 'a': case 'A':
                              sortby = SORT_AVG;
                              break;
                         case 'c': case 'C':
                              sortby = SORT_CALLS;
                              break;
                         case 'u': case 'U':
                              sortby = SORT_CUM;
                              break;
                         default:
                              error("Invalid switch: should be /sa, /sc, or /su");
                         }
                         break;
                    case 't': case 'T':
                         showtimes = 1;
                         showovls = 0;
                         break;
                    case 'u': case 'U':
                         showcumtime = 1;
                         break;
                    case 'z': case 'Z':
                         showzero = 1;
                         break;
                    default:
                         error("Unknown switch \"%c%c\"", *opt, c);
                    }
               }
          } else if (wspfile[0] == '\0') {
               s = fncopy(wspfile, s);
          } else if (profile[0] == '\0') {
               s = fncopy(profile, s);
          }
     }

     if (wspfile[0] == '\0') {
          usage();
          exit(0);
     }
}


/*
     usage
*/

void usage(void)
{
     puts("Usage: wspstat [options] wspfile[.wsp] output[.wss]");
     puts("\nOptions:");
     puts("     /a             report average times");
     puts("     /c             output to console (standard output)");
     puts("     /d             output to DBF file");
     puts("     /lm:margin     set report left margin (default 0)");
     puts("     /m             include module names");
     puts("     /n:width       width of \"Name\" column (default 16)");
     puts("     /o             overlay statistics only");
     puts("     /p             report percentages of run time");
     puts("     /pl:length     set report page length (default 66)");
     puts("     /sa            sort by average times");
     puts("     /sc            sort by numbers of calls");
     puts("     /su            sort by cumulative times");
     puts("     /t             timing statistics only");
     puts("     /u             report cumulative times");
     puts("     /z             include never-called functions");
}


/*
     readwsp
*/

void readwsp(void)
{
     FILE *wfp;
     int symn;
     unsigned nread;
     int nstrings, strn;
     long remain;
     unsigned blklen, extra;
     char *block, *lastp, *nextp;

     fputs("Reading WSP file...\n", stderr);

     fixname(wspfile, ".wsp");
     if ((wfp = fopen(wspfile, "rb")) == NULL)
          error("Can't open %s: %s", wspfile, doserror(_doserrno));

     if ((nread = fread(&wspheader, sizeof(wspheader), 1, wfp)) != 1 ||
               memcmp(wspheader.sig, "WSP\x1A", 4) != 0)
          wsperror(nread);

     if (wspheader.filever < PROFVER || wspheader.profver > FILEVER)
          error("%s: Incompatible file format", wspfile);

     if ((nread = fread(&auxheader, sizeof(auxheader), 1, wfp)) != 1)
          wsperror(nread);

     if ((nread = fread(&statheader, sizeof(statheader), 1, wfp)) != 1)
          wsperror(nread);

     if ((totsyms = statheader.nsyms + statheader.novsyms) > 0) {
          stattab = mustalloc(totsyms * sizeof(*stattab));
          statptr = mustalloc(totsyms * sizeof(*statptr));
          if ((nread = fread(stattab, sizeof(*stattab), totsyms, wfp)) != totsyms)
               wsperror(nread);
          for (symn = 0; symn < totsyms; symn++)
               statptr[symn] = symn;
     }

     if (statheader.novls > 0) {
          ovltab = mustalloc(statheader.novls * sizeof(*ovltab));
          if ((nread = fread(ovltab, sizeof(*ovltab), statheader.novls, wfp)) != statheader.novls)
               wsperror(nread);
     }

     if ((nread = fread(&infoheader, sizeof(infoheader), 1, wfp)) != 1)
          wsperror(nread);

     if (infoheader.nmods > 0) {
          modtab = mustalloc(infoheader.nmods * sizeof(*modtab));
          if ((nread = fread(modtab, sizeof(*modtab), infoheader.nmods, wfp)) != infoheader.nmods)
               wsperror(nread);
     }

     nstrings = totsyms + infoheader.nmods;
     strtab = mustalloc(nstrings * sizeof(*strtab));

     strn = 0;
     remain = infoheader.strtablen;
     extra = 0;

     while (remain > 0 && strn < nstrings) {
          blklen = (remain + extra > MAXALLOC) ? MAXALLOC : remain + extra;
          block = mustalloc(blklen);
          if (extra > 0) memcpy(block, lastp, extra);
          if ((nread = fread(block + extra, 1, blklen - extra, wfp)) != blklen - extra)
               wsperror(nread);
          remain -= nread;
          lastp = block;
          while (strn < nstrings) {
               strtab[strn] = lastp;
               extra = block + blklen - lastp;
               if ((nextp = memchr(lastp, '\0', extra)) == NULL)
                    break;
               lastp = nextp + 1;
               strn++;
          }
     }

     if (remain > 0 || strn < nstrings)
          wsperror(nread);

     fclose(wfp);
}


/*
     wsperror
*/

void wsperror(unsigned nread)
{
     error("%s: %s", wspfile, (nread == -1U) ?
          doserror(_doserrno) : "Invalid format");
}


/*
     writedbf
*/

void writedbf(void)
{
     int i, symn;
     int modn, symcount;

     dbfhdr.hdrlen = sizeof(dbfhdr) + sizeof(fields) + 2;
     dbfhdr.reclen = 1;
     fields[0].fldlen = (char)symwidth;

     for (i = 0; i < N_FIELDS; i++)
          dbfhdr.reclen += fields[i].fldlen;

     fwrite(&dbfhdr, sizeof(dbfhdr), 1, pfp);
     fwrite(fields, sizeof(*fields), N_FIELDS, pfp);
     fwrite("\r\0", 1, 2, pfp);

     writerec("Elapsed Time", "", 'E', 1L, auxheader.elapsed, auxheader.elapsed);
     writerec("Run Time", "", 'R', 1L, auxheader.runtime, auxheader.runtime);
     writerec("Profiler Time", "", 'P', 1L, auxheader.proftime, auxheader.proftime);
     writerec("DOS/BIOS Time", "", 'D', 1L, auxheader.dostime, auxheader.dostime);
     writerec("Overlay Manager", "", 'O', auxheader.ovlcalls, auxheader.omtime, auxheader.omtime);

     for (symn = 0; symn < totsyms; symn++) {
          if (stattab[symn].flags & SF_MAGIC)
               continue;
          if (!showzero && stattab[symn].ncalls == 0)
               continue;
          for (modn = symcount = 0; modn < infoheader.nmods; modn++) {
               if ((symcount += modtab[modn].nsyms) > symn)
                    break;
          }
          writerec(strtab[symn], strtab[totsyms + modn], ' ',
               stattab[symn].ncalls, b24(stattab[symn].fcntime),
               b24(stattab[symn].cumtime));
     }

     rewind(pfp);
     fwrite(&dbfhdr, sizeof(dbfhdr), 1, pfp);
}


/*
     writerec
*/

void writerec(char *name, char *mod, char type, long ncalls, long fcntime, long cumtime)
{
     fwrite(" ", 1, 1, pfp);
     fprintf(pfp, "%-*.*s", symwidth, symwidth, name);
     fprintf(pfp, "%-12.12s", mod);
     fwrite(&type, 1, 1, pfp);
     fprintf(pfp, "%8lu", ncalls);
     fprintf(pfp, "%10.3f", seconds(fcntime));
     fprintf(pfp, "%10.3f", seconds(cumtime));
     dbfhdr.nrecs++;
}


/*
     writeheader
*/

void writeheader(void)
{
     if (pagelen == 0)
          pprintf("Profile data for %s\n\n", auxheader.runfile);
     if (auxheader.nruns > 1)
          pprintf("Cumulative timings for %u runs\n\n", auxheader.nruns);

     pprintf("Elapsed time:      %10.3f sec.\n", seconds(auxheader.elapsed));
     pprintf("Execution time:    %10.3f sec.\n", seconds(auxheader.runtime));
     pprintf("DOS/BIOS time:     %10.3f sec.\n", seconds(auxheader.dostime));
     pprintf("Profiler overhead: %10.3f sec.\n", seconds(auxheader.proftime));

     if (statheader.novsyms != 0) {
          pprintf("Overlay manager:   %10.3f sec.\n", seconds(auxheader.omtime));
          pprintf("\nOverlay loads:   %8lu\n", auxheader.nloads);
          pprintf("Overlay reloads: %8lu\n", auxheader.nreloads);
          pprintf("Overlay calls:   %8lu\n", auxheader.ovlcalls);
     }
}


/*
     writestats
*/

void writestats(void)
{
     int i, hdr;
     int symn, statn;
     int modn, symcount;
     long tottime;

     if (totsyms == 0)
          return;
     if (!showzero) {
          for (statn = 0; statn < totsyms; statn++) {
               if (stattab[statn].ncalls != 0)
                    break;
          }
          if (statn == totsyms)
               return;
     }

     hdr = nhdrlines = 0;
     if (pagelen > 0 && curline > 0 && pagelen - curline < 6)
          eject();
     else if (pagelen == 0 || curline > 0)
          pputc('\n');
     pputs("Timing Statistics:\n\n");

     sprintf(hdrlines[hdr], "%-*s", symwidth, "Name");
     if (showmods)            strcat(hdrlines[hdr], " Module      ");
     if (showcalls)           strcat(hdrlines[hdr], "  # Calls");
     if (showfcntime) {
          strcat(hdrlines[hdr], " Exec. Time");
          if (showpercent)    strcat(hdrlines[hdr], "    %");
     }
     if (showaverage)         strcat(hdrlines[hdr], "  Average Time");
     if (showcumtime) {
          strcat(hdrlines[hdr], "  Cum. Time");
          if (showpercent)    strcat(hdrlines[hdr], "    %");
     }
     hdr++;

     for (i = 0; i < symwidth; i++)
          hdrlines[hdr][i] = '-';
     hdrlines[hdr][i] = '\0';
     if (showmods)            strcat(hdrlines[hdr], " ------------");
     if (showcalls)           strcat(hdrlines[hdr], " --------");
     if (showfcntime) {
          strcat(hdrlines[hdr], " ----------");
          if (showpercent)    strcat(hdrlines[hdr], " ----");
     }
     if (showaverage)         strcat(hdrlines[hdr], " -------------");
     if (showcumtime) {
          strcat(hdrlines[hdr], " ----------");
          if (showpercent)    strcat(hdrlines[hdr], " ----");
     }
     hdr++;

     nhdrlines = hdr;

     for (hdr = 0; hdr < nhdrlines; hdr++)
          pprintf("%s\n", hdrlines[hdr]);

     tottime = auxheader.runtime - auxheader.omtime;

     qsort(statptr, totsyms, sizeof(*statptr), stat_fcmp);

     for (symn = 0; symn < totsyms; symn++) {
          statn = statptr[symn];
          if (stattab[statn].flags & SF_MAGIC)
               continue;
          if (!showzero && stattab[statn].ncalls == 0)
               continue;
          pprintf("%-*.*s", symwidth, symwidth, strtab[statn]);
          if (showmods) {
               for (modn = symcount = 0; modn < infoheader.nmods; modn++) {
                    if ((symcount += modtab[modn].nsyms) > statn)
                         break;
               }
               pprintf(" %-12s", strtab[totsyms + modn]);
          }
          if (showcalls)
               pprintf(" %8lu", stattab[statn].ncalls);
          if (showfcntime) {
               pprintf(" %10.3f", seconds(b24(stattab[statn].fcntime)));
               if (showpercent)
                    pprintf(" %3d\%", (int)((b24(stattab[statn].fcntime) * 200 + 1) / (tottime * 2)));
          }
          if (showaverage) {
               if (stattab[statn].ncalls != 0)
                    pprintf(" %13.6f", seconds(b24(stattab[statn].fcntime)) / stattab[statn].ncalls);
               else
                    pputs("        ------");
          }
          if (showcumtime) {
               pprintf(" %10.3f", seconds(b24(stattab[statn].cumtime)));
               if (showpercent)
                    pprintf(" %3d\%", (int)((b24(stattab[statn].cumtime) * 200 + 1) / (tottime * 2)));
          }
          pputc('\n');
     }

     nhdrlines = 0;
}


/*
     stat_fcmp
*/

int stat_fcmp(const void *p1, const void *p2)
{
     struct statentry *sp1 = &stattab[*(int *)p1];
     struct statentry *sp2 = &stattab[*(int *)p2];
     double avg1, avg2;

     switch (sortby) {
     case SORT_TIME:
          if (b24(sp1->fcntime) > b24(sp2->fcntime))
               return -1;
          if (b24(sp1->fcntime) < b24(sp2->fcntime))
               return 1;
          if (sp1->ncalls > sp2->ncalls)
               return -1;
          if (sp1->ncalls < sp2->ncalls)
               return 1;
          break;
     case SORT_CALLS:
          if (sp1->ncalls > sp2->ncalls)
               return -1;
          if (sp1->ncalls < sp2->ncalls)
               return 1;
          if (b24(sp1->fcntime) > b24(sp2->fcntime))
               return -1;
          if (b24(sp1->fcntime) < b24(sp2->fcntime))
               return 1;
          break;
     case SORT_AVG:
          avg1 = avg2 = 0.0;
          if (sp1->ncalls != 0)
               avg1 = (double)b24(sp1->fcntime) / (double)sp1->ncalls;
          if (sp2->ncalls != 0)
               avg2 = (double)b24(sp2->fcntime) / (double)sp2->ncalls;
          if (avg1 > avg2)
               return -1;
          if (avg1 < avg2)
               return 1;
          if (sp1->ncalls > sp2->ncalls)
               return -1;
          if (sp1->ncalls < sp2->ncalls)
               return 1;
          break;
     case SORT_CUM:
          if (b24(sp1->cumtime) > b24(sp2->cumtime))
               return -1;
          if (b24(sp1->cumtime) < b24(sp2->cumtime))
               return 1;
          if (sp1->ncalls > sp2->ncalls)
               return -1;
          if (sp1->ncalls < sp2->ncalls)
               return 1;
          break;
     }

     return stricmp(strtab[*(int *)p1], strtab[*(int *)p2]);
}


/*
     writeovlstats
*/

void writeovlstats(void)
{
     int i, hdr;
     int ovln, symn, modn, symcount;

     if (statheader.novls == 0)
          return;
     if (!showzero) {
          for (ovln = 0; ovln < statheader.novls; ovln++) {
               if (ovltab[ovln].nloads != 0)
                    break;
          }
          if (ovln == statheader.novls)
               return;
     }

     hdr = nhdrlines = 0;
     if (pagelen > 0 && curline > 0 && pagelen - curline < 6)
          eject();
     else if (pagelen == 0 || curline > 0)
          pputc('\n');
     pputs("Overlay Statistics:\n\n");

     sprintf(hdrlines[hdr], "Ovl# %-*s", symwidth, "Name");
     if (showmods)
          strcat(hdrlines[hdr], " Module      ");
     strcat(hdrlines[hdr], "    Loads  Reloads");
     hdr++;

     strcpy(hdrlines[hdr], "---- ");
     for (i = 0; i < symwidth; i++)
          hdrlines[hdr][i + 5] = '-';
     hdrlines[hdr][i + 5] = '\0';
     if (showmods)
          strcat(hdrlines[hdr], " ------------");
     strcat(hdrlines[hdr], " -------- --------");
     hdr++;

     nhdrlines = hdr;

     for (hdr = 0; hdr < nhdrlines; hdr++)
          pprintf("%s\n", hdrlines[hdr]);

     symn = statheader.nsyms;
     for (ovln = 0; ovln < statheader.novls; ovln++) {
          if (!showzero && ovltab[ovln].nloads == 0)
               continue;
          pprintf("%04X", ovln + 1);
          while (symn < totsyms && stattab[symn].ao.o.ovl <= ovln)
               symn++;
          if (symn < totsyms && stattab[symn].ao.o.ovl == ovln + 1) {
               pprintf(" %-*.*s", symwidth, symwidth, strtab[symn]);
               if (showmods) {
                    for (modn = symcount = 0; modn < infoheader.nmods; modn++) {
                         if ((symcount += modtab[modn].nsyms) > symn)
                              break;
                    }
                    pprintf(" %-12s", strtab[totsyms + modn]);
               }
          } else {
               pprintf(" %*s", symwidth, "");
               if (showmods)
                    pputs("             ");
          }
          pprintf(" %8lu %8lu\n", ovltab[ovln].nloads, ovltab[ovln].nreloads);
          while (++symn < totsyms && stattab[symn].ao.o.ovl == ovln + 1) {
               pprintf("     %-*.*s", symwidth, symwidth, strtab[symn]);
               if (showmods) {
                    for (modn = symcount = 0; modn < infoheader.nmods; modn++) {
                         if ((symcount += modtab[modn].nsyms) > symn)
                              break;
                    }
                    pprintf(" %-12s", strtab[totsyms + modn]);
               }
               pputc('\n');
          }
     }
}


/*
     b24
*/

long b24(char *p24)
{
     return *(long *)p24 & 0xFFFFFFL;
}


/*
     seconds
*/

double seconds(long ticks)
{
     return (double)ticks / (18.20648193 * auxheader.clock);
}


/*
     pprintf
*/

int pprintf(char *fmt, ...)
{
     va_list ap;
     int nchars;
     char line[256];

     va_start(ap, fmt);
     nchars = vsprintf(line, fmt, ap);
     va_end(ap);
     pputs(line);

     return nchars;
}


/*
     pputs
*/

void pputs(char *s)
{
     while (*s != '\0')
          pputc(*s++);
}


/*
     pputc
*/

void pputc(int c)
{
     int i;

     if (curline == 0 && pagelen > 0) {
          curline = 1;
          curpage++;
          pprintf("\nProfile data for %s - page %d\n\n", auxheader.runfile, curpage);
          for (i = 0; i < nhdrlines; i++) {
               pputs(hdrlines[i]);
               pputc('\n');
          }
     }

     switch (c) {
     case '\n':
          outc('\n');
          fmtcol = 0;
          curline++;
          break;
     default:
          if (fmtcol == 0) {
               for (i = 0; i < leftmgn; i++)
                    outc(' ');
               fmtcol = 1;
          }
          outc(c);
          fmtcol++;
     }

     if (pagelen > 0 && curline > pagelen - 1)
          eject();
}


/*
     eject
*/

void eject(void)
{
     if (fmtcol > 0)
          outc('\r');
     outc('\f');
     fmtcol = curline = 0;
}


/*
     outc
*/

void outc(int c)
{
     _doserrno = 0;
     if (fputc(c, pfp) == EOF) {
          error("Can't write %s: %s", tostd ? "standard output" : profile,
               _doserrno ? doserror(_doserrno) : "Disk full");
     }
}


/* end of wspstat.c */
