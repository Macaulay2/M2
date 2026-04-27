// (c) 1994-2002 Michael E. Stillman

#include "interface/monoid.h"

#include "buffer.hpp"
#include "error.h"
#include "monoid.hpp"
#include "ring.hpp"
#include "util.hpp"

const Monoid* rawTrivialMonoid()
{
  return Monoid::get_trivial_monoid();  // Set up in IM2_initialize()
}

const Monoid* /* or Null */ rawMonoid(const MonomialOrdering* mo,
                                      const Ring* deg_ring,
                                      M2_ArrayString names,
                                      M2_arrayint degs,
                                      M2_arrayint hefts)
{
  const auto P = deg_ring->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  return Monoid::create(mo,
                        P,
                        M2_ArrayString_to_stdvector(names),
                        M2_arrayint_to_stdvector<int>(degs),
                        M2_arrayint_to_stdvector<int>(hefts));
}

int rawMonoidNumberOfBlocks(const Monoid* M) { return M->num_parts(); }

unsigned int rawMonoidHash(const Monoid* M) { return M->hash(); }

M2_string rawMonoidToString(const Monoid* M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

M2_arrayint to_degree_vector(const Monoid* M, const_monomial d)
{
  auto result = M2_makearrayint(M->n_vars());
  M->to_expvector(d, result->array);
  return result;
}

// Local Variables:
// indent-tabs-mode: nil
// End:
