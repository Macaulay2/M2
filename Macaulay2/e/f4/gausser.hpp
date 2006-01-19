#ifndef __gausser_h_
#define __gausser_h_

#include "../ring.hpp"

typedef void *F4CoefficientArray;

class Gausser : public our_new_delete
{
public:
  Gausser(const Ring *K);
  ~Gausser() {}

  F4CoefficientArray from_ringelem_array(int len, ring_elem *elems) const;

  void to_ringelem_array(int len, F4CoefficientArray, ring_elem *result) const;

  // other routines:
  void remove_array(F4CoefficientArray a) const;

  // reduce mod p. (QQ --> ZZ/p) (place in double's??)

  // other reductions

  // combine them (ZZ/p1, ..., ZZ/pn --> QQ)

  // Hensel lift routine?

  // evaluation of multivariate poly's or fcn's.
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
