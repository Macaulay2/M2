#include <stdio.h>
#include <gc.h>
#include "../../include/config.h"
#include "debug.h"

void trap(void) {}
void *trapaddr = (void *)1;
void trapchk(void *p) { if (p == trapaddr) trap(); }

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
