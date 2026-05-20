// Copyright 2011 Michael E. Stillman

#ifndef _schurSn_hh_
#define _schurSn_hh_

#include "schur2.hpp"

class SchurSnRing : public SchurRing2
{
 public:
  SchurSnRing(const Ring *A, int n = -1);

  static SchurSnRing *create(const Ring *A, int n = -1);

  virtual const SchurSnRing *cast_to_SchurSnRing() const { return this; }
  virtual SchurSnRing *cast_to_SchurSnRing() { return this; }
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
  ring_elem tensor_mult(const ring_elem f, const ring_elem g) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
