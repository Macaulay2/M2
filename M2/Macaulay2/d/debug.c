#include <M2/gc-include.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include <M2/config.h>
#include <stdint.h>

#ifndef NDEBUG

#include "debug.h"
#include <mpfr.h>
#include <mpfi.h>
#include <string.h>

void *trapaddr = (void *)1;
int trapcount = 0;
int trapset = 0;
size_t trapsize = (size_t)-1;

void trap(void) {}		/* I used to be concerned that this function would get optimized away, but it isn't static ... */

void *pointers[10];		/* during debugging we can put pointers here, visible to the garbage collector */
void trapchk(void *p) { 
     trapcount++;
     if (trapcount == trapset || p == trapaddr || p == (void *)~(intptr_t)trapaddr) trap();
}
void trapchk_size(size_t n) { 
     trapcount++;
     if (trapcount == trapset || trapsize == n) trap();
}

#define STDERR 2
int badBlock() {
     char buf[120];
     sprintf(buf,"%s:%d: internal error: smashed block in memory block allocator\n",__FILE__,__LINE__);
     if (write(STDERR,buf,strlen(buf))) abort();
     abort();
}

#if 0
/* GC_check_annotated_obj is no longer globally defined */
extern unsigned int GC_debug_header_size;
extern void *GC_check_annotated_obj(void *); /* returns NULL or pointer to clobbered debug header location */
#endif

#endif /* NDEBUG */

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
