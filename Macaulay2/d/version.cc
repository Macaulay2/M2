#include <factor.h>		/* to get version number of libfac */

extern "C" {
  const char *get_libfac_version() { return libfac_version; }
}
