#include "M2types.h"
#include "M2inits.h"
void M2inits1(void) __attribute__ ((constructor));
void M2inits1(void) { M2inits(); }
