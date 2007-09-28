#include "pm.hpp"
#include "exceptions.hpp"

template <typename EXP, typename BIN>
void pm<EXP,BIN>::signal_overflow(char *msg) { throw(exc::overflow_error(msg)); }

#if 0

void pm::mult(BIN *dest, BIN *x, BIN *y) {
    int i = numbins;
    BIN carries = 0;
    while (i-- > 0) carries |= x[i] ^ y[i] ^ identity[i] ^ (dest[i] = x[i] + y[i] - identity[i]);
    if expect_false (carries ^ overflow_bits) signal_overflow();
    ...
      }

#endif

// Local Variables:
// compile-command: "cd $M2BUILDDIR/Macaulay2/e && make pm.o"
// End:
