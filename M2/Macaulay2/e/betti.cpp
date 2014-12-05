#include "betti.hpp"

#include <iostream>

BettiDisplay::BettiDisplay(int lodegree, int hidegree, int hilen)
  : mLoDegree(lodegree),
    mHiDegree(hidegree),
    mHiLength(hilen),
    mNLevels(hilen+1)
{
  mValues = newarray_atomic_clear(int,(hidegree-lodegree+1)*mNLevels);
}

BettiDisplay::~BettiDisplay()
{
  deletearray(mValues);
  mValues = 0;
}

int& BettiDisplay::entry(int deg, int lev)
{
  if (deg < mLoDegree 
      or deg > mHiDegree
      or lev < 0
      or lev > mHiLength)
    {
      std::cout << "Internal error: (" << deg << "," << lev << ") out of range" << std::endl;
      exit(1);
    }
  return mValues[lev + mNLevels*(deg-mLoDegree)];
}

M2_arrayint BettiDisplay::getBetti()
{
  return betti_make(mLoDegree, mHiDegree, mHiLength, mValues);
}

void BettiDisplay::displayBetti(buffer& o)
{
  M2_arrayint b = getBetti();
  betti_display(o, b);
}

M2_arrayint BettiDisplay::betti_make(int lo, int hi, int len, int *bettis)
{
  int d, lev;
  int hi1 = hi+1;
  int len1 = len+1;

  // Reset 'hi1' to reflect the top degree that occurs
  for (d=hi; d >= lo; d--)
    {
      for (lev=0; lev<=len; lev++)
        if (bettis[lev+(len+1)*(d-lo)] > 0)
          {
            hi1 = d;
            break;
          }
      if (hi1 <= hi) break;
    }
  if (hi1 > hi) hi1 = hi;

  // Reset 'len1' to reflect the top level that occurs
  for (lev=len; lev>=0; lev--)
    {
      for (d=lo; d<=hi1; d++)
        if (bettis[lev+(len+1)*(d-lo)] > 0)
          {
            len1 = lev;
            break;
          }
      if (len1 <= len) break;
    }
  if (len1 > len) len1 = len;

  int totallen = (hi1-lo+1)*(len1+1);
  M2_arrayint result = M2_makearrayint(3 + totallen);

  result->array[0] = lo;
  result->array[1] = hi1;
  result->array[2] = len1;

  int next = 3;
  for (d=lo; d<=hi1; d++)
    for (lev=0; lev<=len1; lev++)
      result->array[next++] = bettis[lev+(len+1)*(d-lo)];

  return result;
}

void BettiDisplay::betti_display(buffer &o, M2_arrayint ar)
{
  int *a = ar->array;
  int total_sum = 0;
  int lo = a[0];
  int hi = a[1];
  int len = a[2]+1;
  o << "total  ";
  for (int lev=0; lev<len; lev++)
    {
      int sum = 0;
      for (int d=lo; d<=hi; d++)
        sum += a[len*(d-lo)+lev+3];
      total_sum += sum;
      o.put(sum, 6);
      o << ' ';
    }
  o << " [" << total_sum << "]" << newline;
  for (int d=lo; d<=hi; d++)
    {
      o.put(d, 5);
      o << ": ";
      for (int lev=0; lev<len; lev++)
        {
          int c = a[len*(d-lo) + lev + 3];
          if (c != 0)
            o.put(c, 6);
          else
            o << "     -";
          o << " ";
        }
      o << newline;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
