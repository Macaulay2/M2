// Copyright 2014 Michael E. Stillman

#include "res-f4.hpp"
#include "gausser.hpp"

F4Res::F4Res(
             F4Mem* Mem,
             const Gausser* KK0,
             const MonomialInfo* MI,
             int max_level
             )
  : mFrame(*MI,max_level),
    mGausser(KK0),
    mMonoid(MI),
    mMem(Mem),
    mMaxLevel(max_level)
{
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

