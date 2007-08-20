#include "config.h"
#ifndef M2_CONFIG_H
#error "M2_CONFIG_H not defined, perhaps the wrong file named config.h was loaded"
#endif

#include <stdio.h>

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <limits.h>		/* to get PATH_MAX */

#undef _POSIX_THREAD_SAFE_FUNCTIONS
#undef _REENTRANT

#if WITH_NEWLINE_CRLF
#define NEWLINE "\r\n"
#elif WITH_NEWLINE_CR
#define NEWLINE "\r"
#else
#define NEWLINE "\n"
#endif

extern char *libfac_version;

#include "../c/compat.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#if HAVE_MEMORY_H
#include <memory.h>
#endif

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include <stddef.h>
#include <signal.h>
#include <time.h>
#include <utime.h>
#include <errno.h>

#if HAVE_STRING_H
#include <string.h>
#elif HAVE_STRINGS_H
#include <strings.h>
#endif

#include <math.h>

#if HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include <setjmp.h>

#if HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>		/* just for window width */
#endif
#if HAVE_TERMIOS_H
#include <termios.h>		/* just for window width */
#endif
#if HAVE_SYS_MMAN_H
#include <sys/mman.h>		/* needed for mmap() */
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>		/* needed for args to socket(), bind() */
#endif
#if HAVE_NETDB_H
#include <netdb.h>     	    	/* needed for gethostbyname() */
#endif
#if HAVE_NETINET_IN_H
#include <netinet/in.h>	    	/* needed for struct sockaddr_in */
#endif
#if HAVE_ARPA_INET_H
#include <arpa/inet.h>	   	/* needed for inet_addr() */
#endif

#if HAVE_IO_H
#include <io.h>
#endif

#ifdef includeX11
#include <X11/Xlib.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0		/* for non msdos systems */
#endif

#ifndef PAGESIZE
#define PAGESIZE getpagesize()
#endif
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
#include "compat.h"		/* same include files seen by *.c files produced from *.d files */

#define STDIN 0
#define STDOUT 1
#define STDERR 2

#undef ERROR
#define ERROR (-1)      /* in Windows NT there is a file that sets ERROR to 0 */

#define sizeofarray(s,len) (sizeof(*s) - sizeof(s->array) + (len)*sizeof(s->array[0]))

extern unsigned GC_version;		/* in libgc.a */

#if defined(__STDC__)
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

#if HAVE_GNU_GET_LIBC_VERSION
extern char *gnu_get_libc_version();
#endif

#include "../dumpdata/dumpdata.h"

extern int libfac_interruptflag;
extern M2_bool tokens_stopIfError;

extern sigjmp_buf interrupt_jump; /* set this jump and the flag below if the handler should always jump; e.g., for interrupting a slow 3rd party or system library routine */
extern bool interrupt_jump_set;

extern M2_bool system_interruptedFlag;

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
