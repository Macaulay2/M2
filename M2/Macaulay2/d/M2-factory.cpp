#include <M2/config.h>
#define Matrix MaTrIx
#include <factory/factory.h>		// to get definition of factoryseed()
#undef Matrix
// extern "C" void actors5_setFactorySeed(int s) { 
//   factoryseed(s);
// }

void factory_setup_1() {
  On(SW_USE_NTL);		/* tell factory to use NTL */
}





// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
