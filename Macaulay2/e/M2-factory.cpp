#include <factory.h>		// to get definition of factoryseed()

extern "C" {

  void actors5_setFactorySeed(int s) { factoryseed(s); }


}

void factory_setup() {
  On(SW_USE_NTL);		/* tell factory to use NTL */
}
