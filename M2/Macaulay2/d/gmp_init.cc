#include <gc.h>
#include "gmp_init.h"

void initializeGMP(void);
extern "C" void factory_gmp_init() { 
#ifdef FACTORY
  initializeGMP(); 		// factory's initialization routine for gmp memory functions
#endif
}
