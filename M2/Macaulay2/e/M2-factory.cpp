#include "config.h"

#if FACTORY
// #define NOSTREAMIO
#include <factory.h>		// to get definition of factoryseed()
#endif

extern "C" void actors5_setFactorySeed(int s) { 
#if FACTORY
  factoryseed(s);
#endif
}

void factory_setup_1() {
#if FACTORY
  On(SW_USE_NTL);		/* tell factory to use NTL */
#endif
}

#if FACTORY
#define Matrix MaTrIx
// #define NOSTREAMIO
#include <factor.h>		// from Messollen's libfac
#undef Matrix
#include <templates/ftmpl_list.cc>
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
