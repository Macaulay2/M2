// Copyright 2005  Michael E. Stillman

#include "monsort.hpp"

template<typename Sorter>
long QuickSorter<Sorter>::sort_partition(long lo, long hi)
{
  value pivot = elems[lo];
  long i = lo-1;
  long j = hi+1;
  for (;;)
    {
      do { j--; }
      while (M->compare(elems[j], pivot) < 0);
      do { i++; }
      while (M->compare(elems[i], pivot) > 0);

      if (i < j)
	{
	  value tmp = elems[j];
	  elems[j] = elems[i];
	  elems[i] = tmp;
	}
      else
	return j;
    }
}

template<typename Sorter>
void QuickSorter<Sorter>::sort(long lo, long hi)
{
  if (lo < hi)
    {
      long q = sort_partition(lo, hi);
      sort(lo, q);
      sort(q+1, hi);
    }
}


/*****   macros create functional code   *****/
#define pivot_index() (begin+(end-begin)/2)
#define swap(a,b,t) ((t)=(a),(a)=(b),(b)=(t))

template<typename Sorter>
void QuickSorter<Sorter>::sort2(long begin, long end) {
   /*** Use of static here will reduce memory footprint, but will make it thread-unsafe ***/
   static long pivot;
   static long t;     /* temporary variable for swap */
   if (end > begin) {
      long l = begin + 1;
      long r = end;
      swap(elems[begin], elems[pivot_index()], t); /*** choose arbitrary pivot ***/
      pivot = elems[begin];
      while(l < r) {
         if (M->compare(elems[l],pivot) <= 0) {
            l++;
         } else {
            while(l < --r && M->compare(elems[r], pivot) >= 0)/*** skip superfluous swaps ***/
               ;
            swap(elems[l], elems[r], t); 
         }
      }
      l--;
      swap(elems[begin], elems[l], t);
      sort2(begin, l);
      sort2(r, end);
   }
}

#undef swap
#undef pivot_index


template<typename Sorter>
void QuickSorter<Sorter>::sort(Sorter *M0, value *elems0, long len0)
{
  QuickSorter S(M0,elems0,len0);
  S.sort2(0,len0-1);
}

template class QuickSorter<MSorter>;
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
