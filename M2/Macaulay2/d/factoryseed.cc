#include <factory.h>		// to get definition of factoryseed()

extern "C" {
  void actors5_setFactorySeed(int s) { factoryseed(s); }
}
