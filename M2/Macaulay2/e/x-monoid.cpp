// (c) 1994-2002 Michael E. Stillman

#include "monoid.hpp"
#include "engine.h"
#include "ring.hpp"

Monoid *IM2_Monoid_trivial()
{
  return Monoid::get_trivial_monoid(); // Set up in IM2_initialize()
}

engine_RawMonoidOrNull IM2_Monoid_make(const MonomialOrdering *mo,
                              M2_ArrayString names,
                              const Ring *deg_ring,
                              M2_arrayint degs,
                              M2_arrayint hefts)
{
  const PolynomialRing *P = deg_ring->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  return Monoid::create(mo,names,P,degs,hefts);
}

unsigned long IM2_Monoid_hash(const Monoid *M)
{
  return M->get_hash_value();
}

M2_string IM2_Monoid_to_string(const Monoid *M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
