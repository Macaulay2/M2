/* this file contains all initializations needed to get going, even if Macaulay2_main() is not called at all */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include <M2/config.h>
/* these two macros affect GC_INIT below */
#define GC_FREE_SPACE_DIVISOR 12
#define GC_INITIAL_HEAP_SIZE 70000000
#include <M2/gc-include.h>
#include <string.h>
#include "M2inits.h"
#include "M2mem.h"
#include "debug.h"
#define TRUE 1
#define FALSE 0

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
