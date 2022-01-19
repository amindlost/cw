/*
     common.c

     Functions common to wsprun and wspstat.

     D. Rifkind  28 Jan 91
*/

/* $Header: d:/warpspd/RCS/common.c 1.1 91/02/13 22:20:29 dave Exp Locker: dave $ */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <dos.h>
#ifdef __TURBOC__
#include <dir.h>
#endif

#include "common.h"
#include "compat.h"


/*
     Manifest constants
*/

#define NAMETABSIZE 101       /* size of name hash table */


/*
     Structure definitions
*/

struct name {                 /* name table entry: */
     struct name *next;       /*   next name in hash chain */
     char name[1];            /*   text string */
};


/*
     Local variables
*/

static struct name *nametab[NAMETABSIZE];


/*
     Error messages

     This is duplicated in the resident part, but I don't want
     to make the parts too interdependent.
*/

static char *dos_messages[] = {
     "Error 0",
     "Function number invalid",
     "File not found",
     "Path not found",
     "Too many open files",
     "Access denied",
     "Handle invalid",
     "Memory control blocks destroyed",
     "Insufficient memory",
     "Memory block address invalid",
     "Environment invalid",
     "Format invalid",
     "Access code invalid",
     "Data invalid",
     "Unknown unit",
     "Disk drive invalid",
     "Attempted to remove current directory",
     "Not same device",
     "No more files",
};
#define DOS_NMSG    (sizeof(dos_messages) / sizeof(*dos_messages))


/*
     _setargv

     Including this dummy function prevents TC or MSC from
     including the real argument parsing routines from the
     library.  Since we don't use them, this is a minor space
     saver.
*/

void _setargv(void)
{
     return;
}


/*
     getcmdtail

     Retrieve our command tail from the PSP.
*/

char *getcmdtail(char *buf)
{
     char far *tail = MK_FP(_psp, 0x81);
     char *p = buf;

     while (*tail != '\x0D')
          *p++ = *tail++;
     *p = '\0';

     return buf;
}


/*
     switchar

     Returns the DOS switch character.
*/

char switchar(void)
{
     static int sw = -1;
     union REGS regs;

     if (sw < 0) {
          regs.x.ax = 0x3700;
          int86(0x21, &regs, &regs);
          sw = regs.h.dl;
     }

     return (char)sw;
}


/*
     fncopy

     Copy a file name, up to a possible terminating slash (which
     starts a switch or option).  Returns a pointer to the slash
     or space or null terminating the source.
*/

char *fncopy(char *dest, char *src)
{
     char *end;

     for (end = src; *end != '\0' && !isspace(*end) && *end != switchar(); end++)
          ;

     memcpy(dest, src, end - src);
     dest[end - src] = '\0';

     return end;
}


/*
     lookup

     Look up a name in the names table.  Optionally add it if
     not found.
*/

char *lookup(char *name, int add)
{
     char *p;
     unsigned int hash = 0;
     struct name *np;

     for (p = name; *p != '\0'; p++)
          hash = hash * 9 + *p;
     hash %= NAMETABSIZE;

     for (np = nametab[hash]; np != NULL; np = np->next) {
          if (strcmp(name, np->name) == 0)
               break;
     }

     if (np == NULL && add) {
          np = mustalloc(sizeof(*np) + strlen(name));
          strcpy(np->name, name);
          np->next = nametab[hash];
          nametab[hash] = np;
     }

     return (np == NULL) ? NULL : np->name;
}


/*
     fixname

     Normalize a file name, adding default extension if none was
     present.
*/

char *fixname(char *fn, char *defext)
{
     char drive[MAXDRIVE], dir[MAXDIR], name[MAXFILE], ext[MAXEXT];

     fnsplit(fn, drive, dir, name, ext);
     fnmerge(fn, drive, dir, name, (*ext == '\0') ? defext : ext);
     strupr(fn);

     return fn;
}


/*
     newname

     Like fixname(), but changes the extension.
*/

char *newname(char *fn, char *newext)
{
     char drive[MAXDRIVE], dir[MAXDIR], name[MAXFILE];

     fnsplit(fn, drive, dir, name, NULL);
     fnmerge(fn, drive, dir, name, newext);
     strupr(fn);

     return fn;
}


/*
     mustrealloc
*/

void *mustrealloc(void *p, unsigned size)
{
     if ((p = realloc(p, size)) == NULL)
          error("Out of memory");
     return p;
}


/*
     doserror
*/

char *doserror(int err)
{
     static char buf[40];

     if (err < 0 || err >= DOS_NMSG) {
          sprintf(buf, "DOS error code %d", err);
          return buf;
     } else {
          return dos_messages[err];
     }
}


/*
     error
*/

void error(char *fmt, ...)
{
     va_list ap;

     va_start(ap, fmt);
     vfprintf(stderr, fmt, ap);
     va_end(ap);
     fputc('\n', stderr);

     exit(1);
}


/* end of common.c */
