// Copyright 2005  Michael E. Stillman

#ifndef _monsort_h_
#define _monsort_h_

#include "moninfo.hpp"

class MonomialSorter
{
  typedef MonomialInfo::monomial monomial;
  MonomialInfo *M;
  monomial *monoms;
  long len;

  long sort_partition(long lo, long hi);
  void sort(long lo, long hi);

  MonomialSorter(MonomialInfo *M0, monomial *monoms0, long len0);
  ~MonomialSorter();
public:
  static void sortMonomials(MonomialInfo *M0, monomial *monoms0, long len0);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
