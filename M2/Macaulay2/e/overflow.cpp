#include "overflow.hpp"

namespace safe {
     void ov(const char *msg) { throw(exc::overflow_error(msg)); }
}
