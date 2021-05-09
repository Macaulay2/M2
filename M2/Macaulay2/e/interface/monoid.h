#ifndef _monoid_h_
#  define _monoid_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Monoid;
class MonomialOrdering;
#  else
typedef struct Monoid Monoid;
typedef struct MonomialOrdering MonomialOrdering;
#  endif

/**
   Monoid interface routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

Monoid *IM2_Monoid_trivial();
/* drg: connected rawMonoid*/
/* Always returns the same object */

engine_RawMonoidOrNull IM2_Monoid_make(const MonomialOrdering *mo,
                                       M2_ArrayString names,
                                       const Ring *DegreeRing,
                                       M2_arrayint degs,
                                       M2_arrayint hefts);
/* drg: connected rawMonoid*/
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
