#include "M2inits.h"
static void init(void) __attribute__ ((constructor)) __attribute__ ((destructor));;
static void init(void) { factory_setup(); }
