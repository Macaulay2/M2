// Copyright 2004 Michael E. Stillman

#ifndef __MonomialOps_h_
#define __MonomialOps_h_

#include "MonomialSet.hpp"

class MonomialOps
{
 public:
  // For monomial operations which don't use memory allocation
  // see monoms.h

  // The resulting monomial is stored into B
  static uninterned_monomial mult(MemoryBlock &B, monomial a, monomial b);
  static uninterned_monomial quotient(MemoryBlock &B, monomial a, monomial b);
  static uninterned_monomial lcm(MemoryBlock &B, monomial a, monomial b);

  // The resulting monomial is interned in the hash table H
  static monomial mult(MonomialSet *H, monomial a, monomial b);
  static monomial quotient(MonomialSet *H, monomial a, monomial b);
  static monomial lcm(MonomialSet *H, monomial a, monomial b);
};
#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
