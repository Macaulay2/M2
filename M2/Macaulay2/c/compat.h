/*		Copyright 1993 by Daniel R. Grayson		*/

#ifndef __cplusplus
#undef bool
typedef char bool;
#endif

extern char posfmt[];
extern char errfmt[];
extern char errfmtnc[];

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define OKAY 0

#define ERROR (-1)

#if defined(__ppc__) && defined(__MACH__)
/* This is how we identify MacOS X, and Darwin */
#define __DARWIN__ 1
#endif

#include <M2/gc-include.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYSTYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_TYPES_H
#include <types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ctype.h>

#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif

#include <stdio.h>
#include <fcntl.h>

#include <stdarg.h>

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
