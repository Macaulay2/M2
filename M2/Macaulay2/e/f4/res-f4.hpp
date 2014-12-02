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
class FreeModule;

/////////////////////////////////////////////////////////////////////////////

class F4Monomial;  // contains a packed monomial and component
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

  void computeLevel(int level); // level >= 2

  void startLevel0(long nComponents);
  void insertLevel0(long degree, 
                    const F4Monomial& monom);  // we copy the monomial info into level 0 frame

  void startLevel1(long nComponents);
  void insertLevel1(long degree, 
                    const F4Monomial& leadMonomial, 
                    Polynomial&& tail);  // we take ownership of this object
private:
  SchreyerFrame mFrame;

  const Gausser* mGausser;
  const MonomialInfo* mMonoid;
  const F4Mem* mMem;
  int mMaxLevel;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
