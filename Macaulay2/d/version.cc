#include "config.h"
#include <factor.h>		/* to get version number of libfac */

extern "C" {
  const char *get_libfac_version() { 
    return libfac_version; 
  }
}

//
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d"
// End:
//
