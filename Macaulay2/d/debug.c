#include <stdio.h>
#include <gc.h>
#include "memdebug.h"
#include "config.h"
#include "debug.h"

void trap(void) {}
void *trapaddr = (void *)1;
int trapcount = 0;
int trapset = 0;
void trapchk(void *p) { trapcount++; if (trapcount == trapset || p == trapaddr) trap(); }

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
