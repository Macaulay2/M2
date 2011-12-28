#include "overflow.hpp"

namespace safe {
     void ov(const char *msg) { throw(exc::overflow_error(msg)); }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e overflow.ss"
// indent-tabs-mode: nil
// End:
