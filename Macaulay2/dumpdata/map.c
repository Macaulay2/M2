#if defined(__linux__)
#include "map-linux.c"
#elif defined(__sparc__) && defined(__sun__) && defined(__svr4__)
#include "map-solaris.c"
#elif defined(__alpha__) && defined(__osf__)
#include "map-solaris.c"
#elif defined(__FreeBSD__)
#include "map-freebsd.c"
#elif defined(__POWERPC__) && defined(__MACH__) && defined(__APPLE__)
#include "map-macosx.c"
#else
#include "map-notimplemented.c"
#endif

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
