#include <M2/config.h>
#ifndef M2_CONFIG_H
#error "M2_CONFIG_H not defined, perhaps the wrong file named config.h was loaded"
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
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

#include "../c/compat.h"

/* set this jump and the flag below if the handler should always jump;
   e.g., for interrupting a slow 3rd party or system library routine */
#include <setjmp.h>
#ifdef _POSIX_C_SOURCE
# define JMPBUF sigjmp_buf
# define SETJMP(env) sigsetjmp(env, TRUE)
# define LONGJUMP(env) siglongjmp(env, 1)
#else
# define JMPBUF jmp_buf
# define SETJMP(env) setjmp(env)
# define LONGJUMP(env) longjmp(env, 1)
#endif

struct ArgCell
{
  int argc, envc;
  /* const */ char * /* const */ * argv;
  /* const */ char * /* const */ * envp;
};

struct JumpCell
{
  JMPBUF addr;
  bool is_set;
};

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include <stddef.h>
#include <signal.h>
#include <time.h>
#include <utime.h>
#include <errno.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include <math.h>

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>		/* just for window width */
#endif
#ifdef HAVE_TERMIOS_H
#include <termios.h>		/* just for window width */
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>		/* needed for mmap() */
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>		/* needed for args to socket(), bind() */
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>     	    	/* needed for gethostbyname() */
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>	    	/* needed for struct sockaddr_in */
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>	   	/* needed for inet_addr() */
#endif

#ifdef HAVE_IO_H
#include <io.h>
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

#define STDIN 0
#define STDOUT 1
#define STDERR 2

#undef ERROR
#define ERROR (-1)      /* in Windows NT there is a file that sets ERROR to 0 */

#define FATAL(s) fatal("%s:%d: fatal error: %s",__FILE__,__LINE__,s)

extern char current_date[];
extern char current_time[];
extern int system_errno();

#ifdef WITH_PYTHON
#include <Python.h>
#endif

/*
// Local Variables:
// compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
