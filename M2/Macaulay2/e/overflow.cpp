#include "overflow.hpp"

namespace safe {
     void ov(const char *msg) { throw(exc::overflow_error(msg)); }
     void dz(const char *msg) { throw(exc::division_by_zero_error(msg)); }
}
