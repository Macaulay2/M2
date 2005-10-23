// Copyright 2005  Michael E. Stillman

#ifndef _monsort_h_
#define _monsort_h_

#include "moninfo.hpp"

template<typename Sorter>
// Sorter S, S::value_type
// int S.compare(S::value_type a, S::value_type b)
class QuickSorter
{
  typedef typename Sorter::value value;
  Sorter *M;
  value *elems;
  long len;

  long sort_partition(long lo, long hi);
  void sort(long lo, long hi);
  void sort2(long lo, long hi);

  QuickSorter(Sorter *M0, value *elems0, long len0)
    : M(M0), elems(elems0), len(len0) {}
  ~QuickSorter() {}
public:
  static void sort(Sorter *M0, value *elems0, long len0);
};

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
    ncmps ++;
    return M->compare_grevlex(monoms[a],monoms[b]);
  }

  MSorter(MonomialInfo *M0, monomial *monoms0, long *indices0, long len0)
    : M(M0), monoms(monoms0), indices(indices0), len(len0), ncmps(0) {}

  ~MSorter() {} 

  void sort() { 
    QuickSorter<MSorter>::sort(this,indices,len); 
    fprintf(stderr, "sort: nelements %ld ncompares %ld\n",len,ncmps);
  }

  static void sort(MonomialInfo *M0, monomial *monoms0, long *indices0, long len0) {
    MSorter S(M0,monoms0,indices0,len0);
    S.sort();
  }

};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
