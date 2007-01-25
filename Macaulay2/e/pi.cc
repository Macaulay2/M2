#include "pi.h"

int (*g)(int64_t *, int64_t *, int) = pui<int64_t,int,7,UNSIGNED>::cmp_lex;

template class pui<uint32_t,int,7,UNSIGNED>;
template class pui<uint32_t,int,32,UNSIGNED>;
template class puiv<uint32_t,int,7,10,UNSIGNED>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e pi-demangled.s"
// End:
