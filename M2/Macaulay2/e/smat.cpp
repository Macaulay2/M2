// Copyright 2005  Michael E. Stillman

#if 0
#include "coeffrings.hpp"
#include "aring-zz-gmp.hpp"
#include "ZZp.hpp"
#include "smat.hpp"
#include "mat.hpp"

#include "aring-zzp.hpp"
#include "aring-RRR.hpp"
#include "aring-RR.hpp"
#include "aring-CCC.hpp"
#include "aring-gf-givaro.hpp"
#include "aring-m2-gf.hpp"
#include "aring-tower.hpp"

#include "aring-zzp-ffpack.hpp"
#include "aring-zz-flint.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-qq.hpp"
template class SMat<M2::ARingZZGMP>;
template class SMat<M2::ARingZZp>;

template class SMat<M2::ARingQQ>;

#ifdef HAVE_FLINT
template class SMat<M2::ARingZZpFlint>;
template class SMat<M2::ARingQQFlint>;
template class SMat<M2::ARingZZ>;
#endif // HAVE_FLINT

template class SMat<M2::ARingTower>;
template class SMat<M2::ARingZZpFFPACK>;
template class SMat<M2::ARingGFGivaro>;
template class SMat<M2::ARingGFM2>;

template class SMat<CoefficientRingRRR>;
template class SMat<CoefficientRingCCC>;
template class SMat<CoefficientRingR>;
template class SMat<M2::ARingRRR>;
template class SMat<M2::ARingRR>;
template class SMat<M2::ARingCCC>;
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
