/*		Copyright 1993 by Daniel R. Grayson		*/

typedef char bool;

extern char errfmt[];
extern char errfmtnc[];

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define ERROR (-1)

#define GRAIN 8

#define HAS_UNISTD_H
#define HAS_OPEN_MODE
#define HAS_SYSTYPES_H
#define VA_START_HAS_TWO_ARGS

#ifdef _WIN32
#define __WIN32__
/* This is for Microsoft's Visual C/C++ compiler, version 4, /Za */
#include <direct.h>
#include <io.h>
#define getcwd _getcwd
#define read _read
#define open _open
#define close _close
#undef HAS_UNISTD_H
#define alloca _alloca
#define  O_BINARY _O_BINARY
#define	 O_CREAT _O_CREAT
#define	 O_WRONLY _O_WRONLY
#define	 O_TRUNC _O_TRUNC
#endif

#ifdef MPWC
/* This is for MPW's C compiler on the Macintosh, with compile time
   switch "-model far". */
#undef HAS_UNISTD_H
#undef HAS_OPEN_MODE
#undef HAS_SYSTYPES_H
#define HAS_TYPES_H
#endif

#ifdef HAS_UNISTD_H
#include <unistd.h>
#endif

#if !defined(__MWERKS__)
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <memory.h>
#include <stdio.h>
#include <fcntl.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#if 0
#ifdef HAS_SYSTYPES_H
#include <sys/types.h>
#endif

#ifdef HAS_TYPES_H
#include <types.h>
#endif
#endif

#include <fcntl.h>

#if 0
int write(int, char *, int);
int read(int, char *, int);
int open(const char *, int, ...);
#endif

#ifdef __STDC__
int close(int);
int fputs(const char *,FILE *); /* needed  for sunos 4.1 */
int fprintf(FILE *,const char *,...); /* needed  for sunos 4.1 */
int puts(const char *);		
int vfprintf(FILE *,const char *,va_list);/* needed  for sunos 4.1 */
int fflush(FILE *);/* needed  for sunos 4.1 */
#if !defined(__alpha) && !defined(_WIN32)
int _flsbuf(unsigned int, FILE *);/* needed  for sunos 4.1 */
#endif
int printf(const char *,...);
#endif

#if 0
int stat(const char *, struct stat *);
char *index (char *, int);
char *intToString(int);
int vprintf(const char *,va_list);
FILE *freopen(const char *,const char *, FILE *);
int fscanf(FILE *, const char *,...);
char *fgets(char *, int, FILE *);
int fclose(FILE *);
int sscanf(const char *, const char *, ...);
void exit(int);
#endif

#define GaCo 1

#if GaCo
#include <gc.h>
#undef malloc
#define malloc(n) GC_malloc(n)
#define free(n)
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif
