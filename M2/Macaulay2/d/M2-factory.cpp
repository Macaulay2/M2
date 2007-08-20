#include "config.h"
#include <factory.h>		// to get definition of factoryseed()

// extern "C" void actors5_setFactorySeed(int s) { 
//   factoryseed(s);
// }

void factory_setup_1() {
  On(SW_USE_NTL);		/* tell factory to use NTL */
}

#define Matrix MaTrIx
#include <factor.h>		// from Messollen's libfac
#undef Matrix
#include <templates/ftmpl_list.cc>

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
