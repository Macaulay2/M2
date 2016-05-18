#include "betti.hpp"

#include <iomanip>

BettiDisplay::BettiDisplay()
  : mLoDegree(0),
    mHiDegree(0),
    mHiLength(0),
    mNLevels(0)
{
  mValues = 0;
}

BettiDisplay::BettiDisplay(int lodegree, int hidegree, int hilen)
  : mLoDegree(lodegree),
    mHiDegree(hidegree),
    mHiLength(hilen),
    mNLevels(hilen+1)
{
  int nelems = (hidegree-lodegree+1)*mNLevels;
  mValues = new int[nelems];
  for (int i=0; i<nelems; i++) mValues[i] = 0;
}

BettiDisplay::BettiDisplay(const BettiDisplay& B)
  : mLoDegree(B.mLoDegree),
    mHiDegree(B.mHiDegree),
    mHiLength(B.mHiLength),
    mNLevels(B.mNLevels)
{
  int nelems = (mHiDegree - mLoDegree + 1) * mNLevels;
  mValues = new int[nelems];
  for (int i=0; i<nelems; i++)
    mValues[i] = B.mValues[i];
}

void BettiDisplay::swap(BettiDisplay& B)
{
  std::swap(mLoDegree, B.mLoDegree);
  std::swap(mHiDegree, B.mHiDegree);
  std::swap(mHiLength, B.mHiLength);
  std::swap(mNLevels, B.mNLevels);
  std::swap(mValues, B.mValues);
}

BettiDisplay& BettiDisplay::operator=(const BettiDisplay& B)
{
  if (this != &B)
    {
      BettiDisplay b(B); 
      swap(b);
    }
  return *this;
}

BettiDisplay::~BettiDisplay()
{
  delete [] mValues;
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

std::vector<int> BettiDisplay::getBetti() const
{
  return betti_make(mLoDegree, mHiDegree, mHiLength, mValues);
}

void BettiDisplay::displayBetti(std::ostream& o) const
{
  betti_display(o, getBetti());
}

void BettiDisplay::output() const
{
  displayBetti(std::cout);
  std::cout << std::endl;
}

std::vector<int> BettiDisplay::betti_make(int lo, int hi, int len, int *bettis)
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
  std::vector<int> result;
  result.resize(3 + totallen);

  result[0] = lo;
  result[1] = hi1;
  result[2] = len1;

  int next = 3;
  for (d=lo; d<=hi1; d++)
    for (lev=0; lev<=len1; lev++)
      result[next++] = bettis[lev+(len+1)*(d-lo)];

  return result;
}

void BettiDisplay::betti_display(std::ostream &o, const std::vector<int>& a)
{
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
      o << std::setw(6) << sum;
      o << ' ';
    }
  o << " [" << total_sum << "]" << std::endl;
  for (int d=lo; d<=hi; d++)
    {
      o << std::setw(5) << d;
      o << ": ";
      for (int lev=0; lev<len; lev++)
        {
          int c = a[len*(d-lo) + lev + 3];
          if (c != 0)
            o << std::setw(6) << c;
          else
            o << "     -";
          o << " ";
        }
      o << std::endl;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
