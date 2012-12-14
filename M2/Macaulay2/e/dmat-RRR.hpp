#include "dmat.hpp"

namespace LUDecompositionRRR
{
  size_t rankRRR(const DMat<M2::ARingRRR> &M) { return 42; }
  void determinantRRR(const DMat<M2::ARingRRR> &M, M2::ARingRRR::elem &result) { M.ring().set_zero(result); }
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
