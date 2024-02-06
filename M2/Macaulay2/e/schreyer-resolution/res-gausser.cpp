// Copyright 2005-2017 Michael E. Stillman.

#include "schreyer-resolution/res-gausser.hpp"
#include "error.h"                                        // for ERROR
#include "ring.hpp"                                       // for Ring
#include "schreyer-resolution/res-gausser-QQ-hybrid.hpp"  // for ResGausserQ...
#include "schreyer-resolution/res-gausser-QQ.hpp"         // for ResGausserQQ
#include "schreyer-resolution/res-gausser-ZZp.hpp"        // for ResGausserZZp

ResGausser* ResGausser::newResGausser(const Ring* K1)
{
  if (K1->isFinitePrimeField())
    {
      auto p = K1->characteristic();
      if (p > 32767)
        {
          ERROR(
              "currently, res(...,FastNonminimal=>true) requires finite prime "
              "fields with p < 32767");
          return nullptr;
        }
      return new ResGausserZZp(K1);
    }
  if (K1->is_QQ())
    {
      // return new ResGausserQQHybrid(K1, 1000, 32003, 32009);
      //      return new ResGausserQQHybrid(K1, 1000, 1073741891, 1073741909);
      return new ResGausserQQHybrid(K1, 1000, 32003, 1073741909);
    }
  if (K1->is_QQ())
    {
      return new ResGausserQQ(K1, 32003);
    }
  ERROR(
      "currently, res(...,FastNonminimal=>true) requires finite prime fields "
      "or funny QQ");
  return nullptr;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
