// Copyright 1997 by Michael E. Stillman

#include "comb.hpp"
#include "text-io.hpp"
#include <ostream>

comb::comb(int n, int d)
{
  expand(n,d);
}

typedef unsigned long int ulong;

inline ulong range_safe_add(ulong a, ulong b)
{
  ulong c = a + b;
  if (c < a)
    {
      emit_line("ulong integer addition overflow");
      exit(-1);
    }
  return c;
}

void comb::expand(int nn, int dd)
{
  int n,d;

  for (d = tab.length(); d<=dd; d++)
    tab[d][0] = 0;
  for (n = tab[0].length(); n<=nn; n++)
    tab[0][n] = 1;
  tab[0][0] = 1;

  for(d = 1; d<=dd; d++)
    for(n = tab[d].length(); n<=nn; n++)
      tab[d][n] = range_safe_add(tab[d][n-1],tab[d-1][n-1]);
}

void comb::text_out(buffer &o)
{
  o << "length = " << tab.length() << newline;
  for(int i=0; i<tab.length(); i++)
    {
      o << '[' << tab[i].length() << "] ";
      for(int j=0; j < tab[i].length(); j++)
        o << tab[i][j] << ' ';
      o << newline;
    }
}

int comb::encode(int *a, int p)
{
  int result = 0;

  if (p > 0) expand(a[p-1],p);
  for (int i=0; i<p; i++)
    result += binom(a[i],i+1);

  return result;
}

void comb::decode(int b, int *result, int p)
{
  int tmp = b;
  expand(0,p);
  int bot = 0, len = d_len(p) - 1, bit, i;

  while (binom(len++,p) <= tmp)
    expand(len,p);

  for (i=p; i>0; i--)
    {
      while(bit = len % 2, len >>= 1)
        {
          if (binom(bot+len, i) <= tmp)
            {
              bot += len;
              len += bit;
            }
        }
      result[i-1] = bot;
      tmp -= binom(bot, i);
      len = bot+1;
      bot = 0;
    }
}

bool comb::increment(int p, int n, int *s)
{
  for (int i=0; i<p; i++)
    {
      // Attempt to increment this one element
      if ((i < p-1 && s[i]+1 < s[i+1])
          || (i == p-1 && s[i]+1 < n))
        {
          s[i]++;
          for (int j=0; j<i; j++)
            s[j] = j;
          return true;
        }
    }
  return false;
}

bool comb::valid_subset(int p, int n, const int *s)
{
  // Return 1 iff s is a valid p-subset of 0..n-1.
  if (s[0] < 0 || s[0] > n-p) return false;
  for (int i=1; i<p; i++)
    if (s[i] <= s[i-1] || s[i] > n-p+i)
      return false;
  return true;
}

int comb::mult_subsets(int p, const int *s, int q, const int *t, int *&result)
{
  // Places the sorted value of [s0..s(p-1),t0..t(p-1)]
  // int 'result', and returns the sign of the permutation
  // required.  0 is returned if s and t have a common
  // element, in which case, the value in 'result'
  // is undefined.

  int a = 0;
  int b = 0;
  int c = 0;
  int sign = 0;
  if (p == 0 && q == 0) return 1;
  for (;;)
    {
      if (a >= p)
        {
          while (b < q)
            result[c++] = t[b++];
          break;
        }
      else if (b >= q)
        {
          while (a < p)
            result[c++] = s[a++];
          break;
        }
      if (s[a] > t[b])
        {
          sign += p-a;
          result[c++] = t[b++];
        }
      else if (s[a] < t[b])
        {
          result[c++] = s[a++];
        }
      else return 0;
    }
  if ((sign % 2) == 0) return 1;
  return -1;
}

void pcombtab(comb *C)
{
  printf("comb tab has length %d\n", C->tab.length());
  for (int i=0; i<C->tab.length(); i++)
    {
      printf("[%d] ", C->tab[i].length());
      for (int j=0; j<C->tab[i].length(); j++)
        printf("%d ",C->tab[i][j]);
      printf("\n");
    }
}

Subsets::Subsets(size_t n, size_t p)
  : mNumElements(n),
    mMaxSubsetSize(p)
{
  assert(p <= n);
  // we also need to assert that all values placed are size_t without overflow
  mTable = newarray(size_t *, p+1);
  for (size_t i=0; i<=p; i++)
    mTable[i] = newarray_atomic(size_t, n+1);

  // Now fill in the table completely
  for (size_t i=1; i<=mMaxSubsetSize; i++)
    mTable[i][0] = 0;
  for (size_t j=0; j<=mNumElements; j++)
    mTable[0][j] = 1;
  for (size_t i=1; i<=mMaxSubsetSize; i++)
    for (size_t j=1; j<=mNumElements; j++)
      mTable[i][j] = range_safe_add(mTable[i][j-1], mTable[i-1][j-1]);
}

Subsets::~Subsets()
{
  for (size_t i=0; i<=mMaxSubsetSize; i++)
    deletearray(mTable[i]);
  deletearray(mTable);
}

bool Subsets::isValid(const Subset &a)
{
  if (a.size() > mMaxSubsetSize) return false;
  if (a.size() == 0) return true;
  if (a[a.size()-1] > mNumElements) return false;
  for (size_t i=1; i<a.size(); i++)
    if (a[i] <= a[i-1]) return false;
  return true;
}

size_t Subsets::encode(const Subset &a)
{
  // Subsets should be an ascending sequence of ints, all in the range 0..mNumElements-1
  assert(a.size() <= mMaxSubsetSize);
  assert(isValid(a));

  size_t result = 0;

  for (size_t i=0; i<a.size(); i++)
    result += binom(a[i], i+1);

  return result;
}

size_t Subsets::encodeBoundary(size_t e, const Subset &a)
// Take out the e-th element of a (e=0..a.size()-1), and then encode that a.size()-1 subset.
{
  assert(a.size() <= mMaxSubsetSize);
  assert(isValid(a));

  size_t result = 0;

  for (size_t i=0; i<e; i++)
    result += binom(a[i], i+1);

  for (size_t i=e+1; i<a.size(); i++)
    result += binom(a[i], i);

  return result;
}

void Subsets::decode(size_t val, Subset &result)
{
  size_t tmp = val;
  size_t subsetSize = result.size();
  assert(val <= binom(mNumElements, subsetSize));

  size_t len = mNumElements;

  for (size_t i=subsetSize; i>0; i--)
    {
      size_t bit;
      size_t bot = 0;
      while(bit = len % 2, len >>= 1)
        {
          if (binom(bot+len, i) <= tmp)
            {
              bot += len;
              len += bit;
            }
        }
      result[i-1] = bot;
      tmp -= binom(bot, i);
      len = bot+1;
    }

  assert(isValid(result));
}

bool Subsets::increment(size_t n, Subset &s)
{
  size_t p = s.size();
  for (size_t i=0; i<p; i++)
    {
      // Attempt to increment this one element
      if ((i < p-1 && s[i]+1 < s[i+1])
          || (i == p-1 && s[i]+1 < n))
        {
          s[i]++;
          for (size_t j=0; j<i; j++)
            s[j] = j;
          return true;
        }
    }
  return false;
}

int Subsets::concatenateSubsets(const Subset &s,
                                const Subset &t,
                                Subset &result)
{
  size_t p = s.size();
  size_t q = t.size();
  M2_ASSERT(p+q == result.size());
  size_t a = 0;
  size_t b = 0;
  size_t c = 0;
  size_t sign = 0;
  if (p == 0 && q == 0) return 1;
  for (;;)
    {
      if (a >= p)
        {
          while (b < q)
            result[c++] = t[b++];
          break;
        }
      else if (b >= q)
        {
          while (a < p)
            result[c++] = s[a++];
          break;
        }
      if (s[a] > t[b])
        {
          sign += p-a;
          result[c++] = t[b++];
        }
      else if (s[a] < t[b])
        {
          result[c++] = s[a++];
        }
      else return 0;
    }
  if ((sign % 2) == 0) return 1;
  return -1;
}

void Subsets::show(std::ostream &o, const Subset &a)
{
  o << "[";
  for (size_t i=0; i<a.size(); i++)
    {
      if (i > 0) o << ",";
      o << a[i];
    }
  o << "]";
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
