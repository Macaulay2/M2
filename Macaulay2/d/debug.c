#include <stdio.h>
#include <gc.h>
#include "../../include/config.h"
#include "debug.h"

void trap(void) {}
void *trapaddr = (void *)1;
void trapchk(void *p) { if (p == trapaddr) trap(); }

/* these are replacements for the standard routines */

#if 0

    void *malloc(int n) { 
      void *p = GC_MALLOC_UNCOLLECTABLE(n);
      trapchk(p);
      return p;
    }
    void *calloc(int m, int n) { 
      void *p = GC_MALLOC_UNCOLLECTABLE(m*n);
      trapchk(p);
      return p;
    }
    void free(void *p) { 
      trapchk(p);
      GC_FREE(p);
    }
    void *realloc(void *p, int size) { 
      trapchk(p);
      p = GC_REALLOC(p,size);
      trapchk(p);
      return p;
    }

#endif

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
