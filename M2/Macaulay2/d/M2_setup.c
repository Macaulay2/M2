#include "M2inits.h"
static void init(void) __attribute__ ((constructor));
static void init(void) { M2_setup(); }
