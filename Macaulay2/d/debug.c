#include <stdio.h>
#include <gc.h>
#include "memdebug.h"
#include "config.h"
#include "debug.h"
#include <gmp.h>

void *trapaddr = (void *)1;
void trap(void) { }
int trapcount = 0;
int trapset = 0;
void trapchk(void *p) { trapcount++; if (trapcount == trapset || p == trapaddr || (void *)((int)p+1) == trapaddr) trap(); }

#if GC_DEBUG
extern unsigned int GC_debug_header_size;
extern void *GC_check_annotated_obj(void *); /* returns NULL or pointer to clobbered debug header location */
void GC_check(void *p) {
  void *q = p - GC_debug_header_size;
  if (NULL != GC_check_annotated_obj(q)) trap();
}
void gmp_GC_check(void *p) {
  extern void *GC_malloc_function(size_t);
  //  if (__gmp_allocate_func == GC_malloc_function) 
    GC_check(p);
}
#endif

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
