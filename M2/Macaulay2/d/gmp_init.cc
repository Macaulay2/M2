#include "config.h"
#include <gc/gc.h>

#include "gmp_init.h"

extern int initializeGMP();

#if 0
// we could override factory's routine so it doesn't install any gmp memory allocation functions
int initializeGMP() { return 1; }
#endif

extern "C" void initializeGMP_Cwrapper() { 
  initializeGMP(); 		// factory's initialization routine for gmp memory functions
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
