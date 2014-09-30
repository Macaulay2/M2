#include <M2/config.h>
#ifdef HAVE_STDDEF_H
  /* this prevents a problem in Mac OS X, where 'cstddef' is loaded before 'stddef.h', and it causes a problem */
  #include <stddef.h>
#endif
#include <factory/factory.h>		// to get definition of factoryseed()

// extern "C" void actors5_setFactorySeed(int s) { 
//   factoryseed(s);
// }

void factory_setup_1() {
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
