#if defined(__linux__)
#include "map-linux.c"
#elif defined(__sparc__) && defined(__sun__) && defined(__svr4__)
#include "map-solaris.c"
#endif
