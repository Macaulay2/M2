
#include "../../Makeconf.h"	/* VERSION, MP, FACTORY, GCMALLOC, and DUMPDATA are defined here */

#include <stdio.h>

#ifdef SOCKS
#include <socks.h>
#endif

#undef _POSIX_THREAD_SAFE_FUNCTIONS
#undef _REENTRANT

#if defined(_WIN32) && !defined(__CYGWIN32__)
#define alloca _alloca
#endif

#if defined(__DJGPP__) || defined(_WIN32)
#define NEWLINE "\r\n"
#elif defined(__MWERKS__) && !defined(__BUILDING_MPW__)
#define NEWLINE "\r"
#else
#define NEWLINE "\n"
#endif

extern char newline[];

#ifdef FACTORY
extern char *libfac_version;
#endif

#include "../c/compat.h"

#undef malloc
#undef free
#include <gmp.h>

#ifdef HAS_UNISTD_H
#include <unistd.h>
#endif

#if !defined(__MWERKS__)
#include <sys/types.h>
#include <sys/stat.h>
#endif

#if defined(__MWERKS__)
#elif defined(_WIN32) && !defined(__CYGWIN32__)
#else
#include <sys/time.h>
#include <sys/wait.h>
#endif

#include <signal.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <math.h>

#if defined(_WIN32) && !defined(__CYGWIN32__)
#else
#include <alloca.h>
#endif

#include <setjmp.h>

#ifdef __CYGWIN32__
#define HAVE_SOCKETS TRUE
#include <sys/ioctl.h>		/* just for window width */
#include <termios.h>		/* just for window width */
#include <sys/mman.h>		/* needed for mmap() */
#include <sys/socket.h>		/* needed for args to socket(), bind() */
#include <netdb.h>     	    	/* needed for gethostbyname() */
#include <netinet/in.h>	    	/* needed for struct sockaddr_in */
#include <arpa/inet.h>	   	/* needed for inet_addr() */
#elif defined(__DJGPP__) || defined(__MWERKS__) || defined(_WIN32) && !defined(__CYGWIN32__)
#define HAVE_SOCKETS FALSE
#else
#define HAVE_SOCKETS TRUE
#include <sys/ioctl.h>		/* just for window width */
#include <termios.h>		/* just for window width */
#include <sys/mman.h>		/* needed for mmap() */
#include <sys/socket.h>		/* needed for args to socket(), bind() */
#include <netdb.h>     	    	/* needed for gethostbyname() */
#include <netinet/in.h>	    	/* needed for struct sockaddr_in */
#include <arpa/inet.h>	   	/* needed for inet_addr() */
#endif

#ifdef __DJGPP__
#include <fcntl.h>
#include <libc/dosio.h>
#endif

#if defined(_WIN32) && !defined(__CYGWIN32__)
#include <io.h>
#endif

#ifdef includeX11
#include <X11/Xlib.h>
#endif

#include "compat.h"

#ifndef O_BINARY
#define O_BINARY 0		/* for non msdos systems */
#endif

#define PAGESIZE getpagesize()
#define RUP(x) ((((x) + PAGESIZE - 1) / PAGESIZE) * PAGESIZE)

void *sbrk();		/* not really ansi standard, sigh */

#ifdef MP
#define link _link
#include <MP.h>
#undef link
#endif

#if defined(__NeXT__)
 /* on the NeXT Step i386 machine, brk always returns -1, and doesn't work. */
#   define brk(p) (int)sbrk(p-sbrk(0))
#else
int brk();		/* not really ansi standard, sigh */
#endif

#define STDIN 0
#define STDOUT 1
#define STDERR 2

#undef ERROR
#define ERROR (-1)      /* in Windows NT there is a file that sets ERROR to 0 */


typedef struct M2_string_struct {
     unsigned int len;
     char array[1];
     } *M2_string;

extern M2_string tostring(char *);

extern M2_string system_newline;
extern char *tocharstar(M2_string);

typedef char M2_bool;

typedef struct {
     unsigned int len;
     int array[1];
     } *M2_arrayint;

typedef struct {
     unsigned int len;
     M2_string array[1];
     } *M2_stringarray;

extern char **tocharstarstar(M2_stringarray);
extern M2_stringarray tostrings(int,char **);

#define sizeofarray(s,len) (sizeof(*s) - sizeof(s->array) \
     + (len)*sizeof(s->array[0]))

#ifdef GCMALLOC

void *GC_malloc1 (size_t size_in_bytes);
void *GC_realloc3 (void *s, size_t old, size_t new);
void GC_free2 (void *s, size_t old);

#else

void *malloc1 (size_t size_in_bytes);
void *realloc3 (void *s, size_t old, size_t new);
void free2 (void *s, size_t old);

#endif

void trap();
char *getmem(unsigned int);

#if defined(__STDC__) || defined(_WIN32) && !defined(__CYGWIN32__)
extern void fatal(char *s,...);
#else
extern void fatal( va_alist  );
#endif


extern struct FINAL {
     void (*final)();
     struct FINAL *next;
     } *final_list, *pre_final_list;

extern char current_date[];
extern char current_time[];
