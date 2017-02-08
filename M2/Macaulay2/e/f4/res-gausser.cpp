// Copyright 2005-2017 Michael E. Stillman.

#include "res-gausser.hpp"
#include "res-gausser-ZZp.hpp"

ResGausser *ResGausser::newResGausser(const Ring* K1)
{
  if (!K1->isFinitePrimeField())
    {
      ERROR("currently, res(...,FastNonminimal=>true) requires finite prime fields");
      return nullptr;
    }
  auto p = K1->characteristic();
  if (p > 32767)
    {
      ERROR("currently, res(...,FastNonminimal=>true) requires finite prime fields with p < 32767");
      return nullptr;
    }
  return new ResGausserZZp(K1);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
