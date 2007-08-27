#include "config.h"
#include <gc/gc.h>

#include "gmp_init.h"
#include "M2inits.h"

extern int initializeGMP();

#if 0
// we could override factory's routine so it doesn't install any gmp memory allocation functions
int initializeGMP() { return 1; }
#endif

extern "C" void initializeGMP_Cwrapper() { 
  initializeGMP(); 		// factory's initialization routine for gmp memory functions
}

void gmp_init_nothing () {
     factory_init1();		// just to make sure factory_init1.o gets linked in
     factory_init2();		// just to make sure factory_init2.o gets linked in
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
