// Copyright 2005  Michael E. Stillman

#include "monsort.hpp"

long MonomialSorter::sort_partition(long lo, long hi)
{
  monomial pivot = monoms[lo];
  long i = lo-1;
  long j = hi+1;
  for (;;)
    {
      do { j--; }
      while (M->compare_grevlex(monoms[j], pivot) < 0);
      do { i++; }
      while (M->compare_grevlex(monoms[i], pivot) > 0);

      if (i < j)
	{
	  monomial tmp = monoms[j];
	  monoms[j] = monoms[i];
	  monoms[i] = tmp;
	}
      else
	return j;
    }
}

void MonomialSorter::sort(long lo, long hi)
{
  if (lo < hi)
    {
      long q = sort_partition(lo, hi);
      sort(lo, q);
      sort(q+1, hi);
    }
}

MonomialSorter::MonomialSorter(MonomialInfo *M0, monomial *monoms0, long len0)
  : M(M0),
    monoms(monoms0),
    len(len0)
{
}

MonomialSorter::~MonomialSorter()
{
}

void MonomialSorter::sortMonomials(MonomialInfo *M0, monomial *monoms0, long len0)
{
  MonomialSorter S(M0,monoms0,len0);
  S.sort(0,len0-1);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
