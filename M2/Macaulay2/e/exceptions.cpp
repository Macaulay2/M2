#include "exceptions.hpp"

#if 0
static void f(void) __attribute__ ((constructor));
static void f(void) {
     std::set_terminate (__gnu_cxx::__verbose_terminate_handler);
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
