#include <stdio.h>
#include <stdlib.h>
#include <limits.h>		/* to get PATH_MAX */

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

#ifdef FACTORY
extern char *libfac_version;
#endif

#include "../c/compat.h"

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

#include <stddef.h>
#include <signal.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <math.h>

#if HAVE_ALLOCA_H
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


#ifndef O_BINARY
#define O_BINARY 0		/* for non msdos systems */
#endif

#ifndef __DARWIN__
#ifndef PAGESIZE
#if !defined(__linux__) && !defined(__osf__) && !defined(__FreeBSD__) && !defined(__hpux__)
extern size_t getpagesize();
#endif
#define PAGESIZE getpagesize()
#endif
#endif /* __DARWIN__ */

#define RUP(x) ((((x) + PAGESIZE - 1) / PAGESIZE) * PAGESIZE)

#if !defined(__FreeBSD__) && !defined(__DARWIN__)
void *sbrk();		/* not really ansi standard, sigh */
#endif

#ifdef MP
#define link _link
#include <MP.h>
#undef link
#endif


#include "M2types.h"
#include "../e/engine.h"
#include "config.h"
#include "compat.h"		/* same include files seen by *.c files produced from *.d files */
#include "targettypes.h"



#if defined(__NeXT__)
 /* on the NeXT Step i386 machine, brk always returns -1, and doesn't work. */
#   define brk(p) (int)sbrk(p-sbrk(0))
#elif !defined(__FreeBSD__) && !defined(__DARWIN__)
int brk();		/* not really ansi standard, sigh */
#endif

#define STDIN 0
#define STDOUT 1
#define STDERR 2

#undef ERROR
#define ERROR (-1)      /* in Windows NT there is a file that sets ERROR to 0 */

#define sizeofarray(s,len) (sizeof(*s) - sizeof(s->array) + (len)*sizeof(s->array[0]))

extern unsigned GC_version;		/* in libgc.a */

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
extern char const *system_strerror();
extern int system_errno();
extern char *progname;

#ifndef NO_GNU_GET_LIBC_VERSION
extern char *gnu_get_libc_version();
#endif

#include "../dumpdata/dumpdata.h"

#ifdef FACTORY
extern int libfac_interruptflag;
#endif

extern M2_bool tokens_stopIfError;

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
