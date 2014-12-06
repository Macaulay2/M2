// Copyright 2014 Michael E. Stillman

#ifndef _res_f4_hpp_
#define _res_f4_hpp_

#include "res-f4-types.hpp"
#include "f4-mem.hpp"
#include "res-schreyer-frame.hpp"

#include <assert.h>
#define M2_ASSERT assert

class Gausser;
class MonomialInfo;

/////////////////////////////////////////////////////////////////////////////

class F4Res
{
public:
  F4Res(
        F4Mem* Mem,
        const Gausser* KK0,
        const MonomialInfo* MI,
        int max_level
       );

  ~F4Res() {
    delete mMem;
  }

  SchreyerFrame& frame() { return mFrame; }

  M2_arrayint getBetti(int type) const;
  
private:
  SchreyerFrame mFrame;

  const Gausser* mGausser;
  const MonomialInfo* mMonoid;
  const F4Mem* mMem; // Used for what TODO?
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
