#include "config.h"
#ifdef FACTORY
#include <factor.h>		/* to get version number of libfac */
#endif

extern "C" {
  const char *get_libfac_version() { 
#ifdef FACTORY
    return libfac_version; 
#else
    return "library 'libfac' not installed";
#endif
  }
}

//
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d"
// End:
//
