#include "M2types.h"
#include "M2inits.h"
void M2inits2(void) __attribute__ ((constructor));
void M2inits2(void) { M2inits(); }
