#include "config.h"

#ifdef FACTORY
#include <factory.h>		// to get definition of factoryseed()
#endif

extern "C" void actors5_setFactorySeed(int s) { 
#ifdef FACTORY
  factoryseed(s);
#endif
}

void factory_setup() {
#ifdef FACTORY
  On(SW_USE_NTL);		/* tell factory to use NTL */
#endif
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
