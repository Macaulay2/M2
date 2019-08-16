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

/* to get IM2_initialize() : */
#include "engine.h"

const char *progname;
void arginits(int argc, char **argv) { progname = argv[0]; }

static void init_gc(void) {
     GC_INIT();
     }

void M2inits(void) {
  static int done = 0;
  if (!done) {
    done = 1;
#   ifndef NDEBUG
    trap();			/* we call trap() once so variables (such as trapset) can be set */
#   endif
    init_gc();
    IM2_initialize();
  }
}

void scc_core_prepare() {
  M2inits();
}

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
