#include "pi.h"

#ifdef DEBUG
void trap () {}
#endif

typedef uint32_t T;
typedef uint32_t U;
typedef pui<T, U, 7, UNSIGNED> A;
template class pui<T, U, 7, UNSIGNED>;
int main() {
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e pi-demangled.s pi && $M2BUILDDIR/Macaulay2/e/pi"
// End:
