// Copyright 2014 Michael E. Stillman

#ifndef _res_f4_hpp_
#define _res_f4_hpp_

#include "res-f4-types.hpp"

#include <vector>

class Gausser;
class MonomialInfo;
class FreeModule;

/////////////////////////////////////////////////////////////////////////////

class F4Res
{
public:
  F4Res(const Gausser& KK0,
        const MonomialInfo& MI,
        const FreeModule& F, // used for debugging only...
        bool use_maxlevel,
        int maxlevel,
        bool use_maxdegree,
        int maxdegree,
        int strategy // unused so far
       );
  ~F4Res() {}

  void setLevel0();
  void setLevel1(); // these functions 

  void computeLevel(int level); // level >= 2
private:
  Frame mFrame;

  const Gausser& mGausser;
  const MonomialInfo& mMonoid;
  const FreeModule& mFreeModule;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
