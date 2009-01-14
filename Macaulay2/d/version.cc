#include <M2/config.h>
#include <factor.h>		/* to get version number of libfac */
#undef ASSERT
extern "C" {
  const char *get_libfac_version() { 
    return libfac_version; 
  }
}
#ifdef HAVE_FROBBY
#include <stdinc.h>		// to get version number of frobby
#undef ASSERT
extern "C" {
  const char *get_frobby_version() { 
    return constants::version; 
  }
}
#else
extern "C" {
  const char *get_frobby_version() { 
    return "not present"; 
  }
}
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d"
// End:
