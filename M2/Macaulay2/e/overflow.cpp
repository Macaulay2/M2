#include "overflow.hpp"
#include <string>

namespace safe {
     void ov(const char *msg) { 
       const std::string s(msg);
       throw(exc::overflow_error(s)); }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e overflow.ss"
// indent-tabs-mode: nil
// End:
