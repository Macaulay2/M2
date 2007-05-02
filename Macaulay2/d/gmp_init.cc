#include "config.h"
#if defined(HAVE_GC_GC_H)
#include <gc/gc.h>
#elif defined(HAVE_GC_H)
#include <gc.h>
#else
#error missing include file gc.h
#endif

#include "gmp_init.h"

extern int initializeGMP();

#if 0
// we could override factory's routine so it doesn't install any gmp memory allocation functions
int initializeGMP() { return 1; }
#endif

extern "C" void initializeGMP_Cwrapper() { 
#if FACTORY
  initializeGMP(); 		// factory's initialization routine for gmp memory functions
#endif
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
