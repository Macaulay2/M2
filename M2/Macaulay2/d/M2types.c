#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "M2mem.h"

#if 0

const char *nullstringer(const char *s) {
  return *s ? s : NULL;
}

M2_arrayint toarrayint(int n,int *p)
{
  M2_arrayint z = (M2_arrayint)getmem_atomic(sizeofarray(z,n));
  z->len = n;
  memcpy(z->array,p,n * sizeof(int));
  return z;
}




#endif

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
