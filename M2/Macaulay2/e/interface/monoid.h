#ifndef _monoid_h_
#  define _monoid_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Ring;
class Monoid;
class MonomialOrdering;
#  else
typedef struct Ring Ring;
typedef struct Monoid Monoid;
typedef struct MonomialOrdering MonomialOrdering;
#  endif

/**
   Monoid interface routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

/* rawMonoid(): always returns the same object */
const Monoid *rawTrivialMonoid();

/* rawMonoid(mo, deg_ring, names, degrees, heftvec) */
const Monoid * /* or Null */ rawMonoid(const MonomialOrdering *mo,
                                       const Ring *deg_ring,
                                       M2_ArrayString names,
                                       M2_arrayint degrees,
                                       M2_arrayint heftvec);
/* This function will return NULL if the monomial order cannot be handled
   currently, if the first components for each degree are not all
   positive, or under various other "internal" error conditions */

unsigned int rawMonoidHash(const Monoid *M);
/* drg: connected hash */
/* Assigned sequentially */

M2_string IM2_Monoid_to_string(const Monoid *M);
/* drg: connected */
/* For debugging purposes */

int rawMonoidNumberOfBlocks(const Monoid *M);
/* connected rawMonoidNumberOfBlocks */

#  if defined(__cplusplus)
}
#  endif

#endif /* _monoid_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
