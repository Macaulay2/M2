#include <gc.h>
#include "memdebug.h"
#include "gmp_init.h"
#include "config.h"

void initializeGMP(void);
extern "C" void factory_gmp_init() { 
#ifdef FACTORY
  initializeGMP(); 		// factory's initialization routine for gmp memory functions
#endif
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
