#include <stdio.h>
#include "M2types.h"

void M2initschk(void) __attribute__ ((constructor));
void M2initschk(void) {

     if (!M2inits_run) {
       fprintf(stderr,"internal error: constructor M2inits() failed to run soon enough\n");
       exit(1);
     }

}
