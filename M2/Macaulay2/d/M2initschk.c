#include <stdio.h>
#include "M2types.h"
#include "M2inits.h"

void M2initschk(void) __attribute__ ((constructor));
void M2initschk(void) {

     if (!M2inits_run) {
       fprintf(stderr,"internal error: constructor M2inits() failed to run soon enough\n");
       exit(1);
     }

}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
