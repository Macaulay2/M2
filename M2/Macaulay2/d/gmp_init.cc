#include <M2/config.h>
#include <M2/gc-include.h>

#include "gmp_init.h"
#include "M2inits.h"

//extern int initializeGMP();

 int initializeGMP()
{
   return 1;
}

#if 0
// we could override factory's routine so it doesn't install any gmp memory allocation functions
int initializeGMP() { return 1; }
#endif

extern "C" void initializeGMP_Cwrapper() { 
  initializeGMP(); 		// factory's initialization routine for gmp memory functions
}

/*
// Local Variables:
// compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
