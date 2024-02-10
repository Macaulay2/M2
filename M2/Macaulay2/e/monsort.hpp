// Copyright 2005  Michael E. Stillman

#ifndef _monsort_h_
#define _monsort_h_

#include <cstdio>
#include <cstdlib>
#include "newdelete.hpp"

#if !defined(SAFEC_EXPORTS)
#include <engine-exports.h>
#endif

template <typename Sorter>
// Sorter S, needs to define:
//   typename S::value
//   int S.compare(S::value a, S::value b)
// such that: if a<b, result is <0
//               a==b, result is ==0
//               a>b, result is >0
class QuickSorter
{
  typedef typename Sorter::value value;
  Sorter *M;
  value *elems;
  long len;
  long maxdepth;

  long sort_partition(long lo, long hi);
  void sort(long lo, long hi);

  void sort2(long lo, long hi);

  void sort2depth(long lo, long hi, long depth);

  void sortC();
  void sortD();  // heap sort

  QuickSorter(Sorter *M0, value *elems0, long len0)
      : M(M0), elems(elems0), len(len0), maxdepth(0)
  {
  }
  ~QuickSorter() {}
 public:
  static void sort(Sorter *M0, value *elems0, long len0);
};

///////////////////////////
// Algorithm A ////////////
// Quicksort, recursive version, not used now?
///////////////////////////

template <typename Sorter>
long QuickSorter<Sorter>::sort_partition(long lo, long hi)
{
  value pivot = elems[lo];
  long i = lo - 1;
  long j = hi + 1;
  for (;;)
    {
      do
        {
          j--;
        }
      while (M->compare(elems[j], pivot) < 0);
      do
        {
          i++;
        }
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

template <typename Sorter>
void QuickSorter<Sorter>::sort(long lo, long hi)
{
  if (lo < hi)
    {
      long q = sort_partition(lo, hi);
      sort(lo, q);
      sort(q + 1, hi);
    }
}

/////////////////////////
// Quicksort, also recursive, better pivot choices
// But: is very poor if all elements are the same
/////////////////////////

/*****   macros create functional code   *****/
#define pivot_index() (begin + (end - begin) / 2)
#define swap(a, b, t) ((t) = (a), (a) = (b), (b) = (t))

template <typename Sorter>
void QuickSorter<Sorter>::sort2(long begin, long end)
{
  value pivot;
  value t; /* temporary variable for swap */
  if (end > begin)
    {
      long l = begin + 1;
      long r = end;
      swap(elems[begin],
           elems[pivot_index()],
           t); /*** choose arbitrary pivot ***/
      pivot = elems[begin];
      while (l < r)
        {
          if (M->compare(elems[l], pivot) <= 0)
            {
              l++;
            }
          else
            {
              while (l < --r &&
                     M->compare(elems[r], pivot) >=
                         0) /*** skip superfluous swaps ***/
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

///////////////////////
// Same as sort2, except depth information is kept
///////////////////////

template <typename Sorter>
void QuickSorter<Sorter>::sort2depth(long begin, long end, long depth)
{
  value pivot;
  value t; /* temporary variable for swap */
  if (depth > maxdepth) maxdepth = depth;
  if (end > begin)
    {
      long l = begin + 1;
      long r = end;
      swap(elems[begin],
           elems[pivot_index()],
           t); /*** choose arbitrary pivot ***/
      pivot = elems[begin];
      while (l < r)
        {
          if (M->compare(elems[l], pivot) <= 0)
            {
              l++;
            }
          else
            {
              while (l < --r &&
                     M->compare(elems[r], pivot) >=
                         0) /*** skip superfluous swaps ***/
                ;
              swap(elems[l], elems[r], t);
            }
        }
      l--;
      swap(elems[begin], elems[l], t);
      sort2depth(begin, l, depth + 1);
      sort2depth(r, end, depth + 1);
    }
}

#undef swap
#undef pivot_index

///////////////////////
// Non-recursive quicksort, cribbed from numerical recipes
///////////////////////

#define SWAP(a, b) \
  temp = (a);      \
  (a) = (b);       \
  (b) = temp;
#define THRESH 6
#define NSTACK 50

template <typename Sorter>
void QuickSorter<Sorter>::sortC()
// (unsigned long n, float arr[])
// array is: elems
// length is: len
{
  unsigned long i, ir = len, j, k, l = 1, *istack;
  int jstack = 0;
  value a, temp;
  long ncmps;
  int maxstack = 0;
  ncmps = 0;
  istack = newarray_atomic(unsigned long, NSTACK);
  for (;;)
    {
      if (ir - l < THRESH)
        {
          for (j = l + 1; j <= ir; j++)
            {
              a = elems[j];
              for (i = j - 1; i >= l; i--)
                {
                  ncmps++;
                  if (M->compare(elems[i], a) <= 0) break;
                  elems[i + 1] = elems[i];
                }
              elems[i + 1] = a;
            }
          if (jstack == 0) break;
          ir = istack[jstack--];
          l = istack[jstack--];
        }
      else
        {
          k = (l + ir) >> 1;
          SWAP(elems[k], elems[l + 1])
          ncmps++;
          if (M->compare(elems[l], elems[ir]) > 0)
            {
              SWAP(elems[l], elems[ir])
            }
          ncmps++;
          if (M->compare(elems[l + 1], elems[ir]) > 0)
            {
              SWAP(elems[l + 1], elems[ir])
            }
          ncmps++;
          if (M->compare(elems[l], elems[l + 1]) > 0)
            {
              SWAP(elems[l], elems[l + 1])
            }
          i = l + 1;
          j = ir;
          a = elems[l + 1];
          for (;;)
            {
              do
                i++;
              while (ncmps++, M->compare(elems[i], a) < 0);
              do
                j--;
              while (ncmps++, M->compare(elems[j], a) > 0);
              if (j < i) break;
              SWAP(elems[i], elems[j]);
            }
          elems[l + 1] = elems[j];
          elems[j] = a;
          jstack += 2;
          if (jstack > maxstack) maxstack = jstack;
          if (jstack > NSTACK)
            {
              fprintf(stderr, "NSTACK too small in sort.\n");
              exit(0);
            }
          if (ir - i + 1 >= j - l)
            {
              istack[jstack] = ir;
              istack[jstack - 1] = i;
              ir = j - 1;
            }
          else
            {
              istack[jstack] = j - 1;
              istack[jstack - 1] = l;
              l = i;
            }
        }
    }
  freemem(istack);
  fprintf(stderr,
          "quicksort: len = %ld ncmps = %ld 2*depth = %d\n",
          len,
          ncmps,
          maxstack);
}
#undef THRESH
#undef NSTACK
#undef SWAP

//////////////////////////////////////
// sortD: heap sort
//////////////////////////////////////

template <typename Sorter>
void QuickSorter<Sorter>::sortD()
{
  unsigned long i, ir, j, l;
  value rra;
  long ncmps;

  ncmps = 0;
  if (len < 2) return;
  l = (len >> 1) + 1;
  ir = len;
  for (;;)
    {
      if (l > 1)
        {
          rra = elems[--l];
        }
      else
        {
          rra = elems[ir];
          elems[ir] = elems[1];
          if (--ir == 1)
            {
              elems[1] = rra;
              break;
            }
        }
      i = l;
      j = l + l;
      while (j <= ir)
        {
          if (j < ir)
            {
              ncmps++;
              if (M->compare(elems[j], elems[j + 1]) < 0) j++;
            }
          ncmps++;
          if (M->compare(rra, elems[j]) < 0)
            {
              elems[i] = elems[j];
              i = j;
              j <<= 1;
            }
          else
            j = ir + 1;
        }
      elems[i] = rra;
    }
  fprintf(stderr, "hpsort: %ld, %ld\n", len, ncmps);
}

//////////////////////////////////////
#include <ctime>

template <typename Sorter>
void QuickSorter<Sorter>::sort(Sorter *M0, value *elems0, long len0)
{
  QuickSorter S(M0, elems0, len0);
  int typ = 1;
  clock_t begin_time = clock();

  switch (typ)
    {
      case 1:
        S.sort2(0, len0);
        break;
      case 2:
        S.sort2depth(0, len0, 1);
        break;

      case 3:
        S.sortC();
        break;

      case 4:
        S.sortD();
        break;
    }

  clock_t end_time = clock();
  long double nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;

  if (M2_gbTrace >= 4)
    fprintf(
        stderr, "sort: len %ld depth %ld time %Lf\n", len0, S.maxdepth, nsecs);
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
