#ifndef _monoid_h_
#  define _monoid_h_

#  include "engine-includes.hpp"

// TODO: make this unnecessary
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

/* integer types to be used for degrees and exponents */
typedef int32_t deg_t;
typedef const deg_t *const_monomial;

/* rawMonoid(): always returns the same object */
const Monoid *rawTrivialMonoid();

/* rawMonoid(mo, deg_ring, names, degrees, heftvec) */
const Monoid * /* or Null */ rawMonoid(const MonomialOrdering *mo,
                                       const Ring *deg_ring,
                                       M2_ArrayString names,
                                       M2_arrayint degrees,
                                       M2_arrayint heftvec);
// TODO: this is wrong and needs to be updated
/* This function will return NULL if the monomial order cannot be handled
   currently, if the first components for each degree are not all
   positive, or under various other "internal" error conditions */

int rawMonoidNumberOfBlocks(const Monoid *M);

/* connected to the interpreter in d/basic.d */
unsigned int rawMonoidHash(const Monoid *M);
/* Assigned sequentially */

/* connected to the interpreter in d/actors4.d */
M2_string rawMonoidToString(const Monoid *M);
/* For debugging purposes */

/* Service routine for converting degree monomials to int arrays.
   To be used only by other interface routines. */
M2_arrayint to_degree_vector(const Monoid *M, const_monomial d);

#  if defined(__cplusplus)
}
#  endif

#endif /* _monoid_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
