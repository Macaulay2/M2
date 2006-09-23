// Copyright 2005 Michael E. Stillman.

#include "gausser.hpp"

Gausser *Gausser::newGausser(const Ring *K)
{
  const Z_mod *Kp = K->cast_to_Z_mod();
  if (Kp != 0)
    return new Gausser(Kp);
  return 0;
}

Gausser::Gausser(const Z_mod *K0)
  : typ(ZZp), K(K0), Kp(K0->get_CoeffRing())
{
}

F4CoefficientArray Gausser::from_ringelem_array(int len, ring_elem *elems) const
{
  int i;
  switch (typ) {
  case ZZp:
    int *result = newarray_atomic(int, len);
    for (i=0; i<len; i++)
      result[i] = elems[i].int_val;
    return result;
  };
  return 0;
}

void Gausser::to_ringelem_array(int len, F4CoefficientArray F, ring_elem *result) const
{
  int *elems = static_cast<int *>(F);
  int i;
  switch (typ) {
  case ZZp:
    for (i=0; i<len; i++)
      result[i].int_val = elems[i];
  };
}

void Gausser::remove_array(F4CoefficientArray a) const
{
  deletearray(a);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
