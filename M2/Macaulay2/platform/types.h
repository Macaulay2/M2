#include <stdio.h>
#include <stdlib.h>
#include <limits.h>		/* to get PATH_MAX */

//#include "../c/compat.h"

#include <memory.h>
#include <stddef.h>
#include <signal.h>
#include <time.h>
#include <utime.h>
#include <errno.h>
#include <string.h>
#include <math.h>

#include <gc.h>

#ifndef O_BINARY
#define O_BINARY 0		/* for non msdos systems */
#endif

#define STDIN 0
#define STDOUT 1
#define STDERR 2

#define FALSE 0
#define TRUE 1

//#include <scc-core.h>

extern int system_errno();
extern char *progname;


extern const char *libfac_version;

/*
// Local Variables:
// compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
