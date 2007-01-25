#include "pi.h"

int64_t (*g)(int64_t *, int64_t *, int) = pui<int64_t,int,7>::cmp_lex;

template class pui<int32_t,int,7>;
template class pui<int32_t,int,8>;
template class puiv<int32_t,int,7,10>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e pi-demangled.s"
// End:
