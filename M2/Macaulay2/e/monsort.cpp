// Copyright 2005  Michael E. Stillman

#include "monsort.hpp"

#if 0
#include "moninfo.hpp"

// Here is an example use of QuickSorter.

class MSorter
{
 public:
  typedef MonomialInfo::value monomial;
  typedef long value;

 private:
  MonomialInfo *M;
  monomial *monoms;
  long *indices;
  long len;
  long ncmps;

 public:
  int compare(value a, value b)
  {
    ncmps++;
    return M->compare_grevlex(monoms[a], monoms[b]);
  }

  MSorter(MonomialInfo *M0, monomial *monoms0, long *indices0, long len0)
      : M(M0), monoms(monoms0), indices(indices0), len(len0), ncmps(0)
  {
  }

  ~MSorter() {}
  void sort()
  {
    QuickSorter<MSorter>::sort(this, indices, len);
    fprintf(stderr, "sort: nelements %ld ncompares %ld\n", len, ncmps);
  }

  static void sort(MonomialInfo *M0,
                   monomial *monoms0,
                   long *indices0,
                   long len0)
  {
    // monoms0 is an array 0..len0-1 of monomials
    // indices0 is an array 0..len0-1, starts at the identity permutation
    // monoms0 is not modified.  Only the permutation to place the
    // elements in descending monomial order is modified.
    MSorter S(M0, monoms0, indices0, len0);
    S.sort();
  }
};

template class QuickSorter<MSorter>;
#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
