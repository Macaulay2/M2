// Copyright 1997-2013 by Michael E. Stillman

#include "comb.hpp"
#include "text-io.hpp"
#include <ostream>

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

Subsets::Subsets(size_t n, size_t p) : mNumElements(n), mMaxSubsetSize(p)
{
  assert(p <= n);
  // we also need to assert that all values placed are size_t without overflow
  mTable = newarray(size_t *, p + 1);
  for (size_t i = 0; i <= p; i++) mTable[i] = newarray_atomic(size_t, n + 1);

  // Now fill in the table completely
  for (size_t i = 1; i <= mMaxSubsetSize; i++) mTable[i][0] = 0;
  for (size_t j = 0; j <= mNumElements; j++) mTable[0][j] = 1;
  for (size_t i = 1; i <= mMaxSubsetSize; i++)
    for (size_t j = 1; j <= mNumElements; j++)
      mTable[i][j] = range_safe_add(mTable[i][j - 1], mTable[i - 1][j - 1]);
}

Subsets::~Subsets()
{
  for (size_t i = 0; i <= mMaxSubsetSize; i++) freemem(mTable[i]);
  freemem(mTable);
}

bool Subsets::isValid(const Subset &a)
{
  if (a.size() == 0) return true;
  return isValid(mNumElements, a.size(), &(a[0]));
}

bool Subsets::isValid(size_t nElements, size_t subsetSize, const size_t *a)
{
  if (subsetSize > nElements) return false;
  if (subsetSize == 0) return true;
  if (a[subsetSize - 1] > nElements) return false;
  for (size_t i = 1; i < subsetSize; i++)
    if (a[i] <= a[i - 1]) return false;
  return true;
}
bool Subsets::isValid(size_t nElements, size_t subsetSize, const int *a)
{
  if (subsetSize > nElements) return false;
  if (subsetSize == 0) return true;
  if (a[subsetSize - 1] > nElements) return false;
  for (size_t i = 1; i < subsetSize; i++)
    if (a[i] <= a[i - 1]) return false;
  return true;
}

size_t Subsets::encode(const Subset &a)
{
  // Subsets should be an ascending sequence of ints, all in the range
  // 0..mNumElements-1
  assert(a.size() <= mMaxSubsetSize);
  assert(isValid(a));

  size_t result = 0;

  for (size_t i = 0; i < a.size(); i++) result += binom(a[i], i + 1);

  return result;
}

size_t Subsets::encodeBoundary(size_t e, const Subset &a)
// Take out the e-th element of a (e=0..a.size()-1), and then encode that
// a.size()-1 subset.
{
  assert(a.size() <= mMaxSubsetSize);
  assert(isValid(a));

  size_t result = 0;

  for (size_t i = 0; i < e; i++) result += binom(a[i], i + 1);

  for (size_t i = e + 1; i < a.size(); i++) result += binom(a[i], i);

  return result;
}

void Subsets::decode(size_t val, Subset &result)
{
  size_t tmp = val;
  size_t subsetSize = result.size();
  assert(val <= binom(mNumElements, subsetSize));

  size_t len = mNumElements;

  for (size_t i = subsetSize; i > 0; i--)
    {
      size_t bit;
      size_t bot = 0;
      while (bit = len % 2, len >>= 1)
        {
          if (binom(bot + len, i) <= tmp)
            {
              bot += len;
              len += bit;
            }
        }
      result[i - 1] = bot;
      tmp -= binom(bot, i);
      len = bot + 1;
    }

  assert(isValid(result));
}

bool Subsets::increment(size_t n, Subset &s)
{
  if (s.size() == 0) return false;
  return increment(n, s.size(), &(s[0]));
#if 0
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
#endif
}

bool Subsets::increment(size_t n, size_t subset_size, size_t *subset)
{
  size_t p = subset_size;
  for (size_t i = 0; i < p; i++)
    {
      // Attempt to increment this one element
      if ((i < p - 1 && subset[i] + 1 < subset[i + 1]) ||
          (i == p - 1 && subset[i] + 1 < n))
        {
          subset[i]++;
          for (size_t j = 0; j < i; j++) subset[j] = j;
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
  assert(p + q == result.size());
  size_t a = 0;
  size_t b = 0;
  size_t c = 0;
  size_t sign = 0;
  if (p == 0 && q == 0) return 1;
  for (;;)
    {
      if (a >= p)
        {
          while (b < q) result[c++] = t[b++];
          break;
        }
      else if (b >= q)
        {
          while (a < p) result[c++] = s[a++];
          break;
        }
      if (s[a] > t[b])
        {
          sign += p - a;
          result[c++] = t[b++];
        }
      else if (s[a] < t[b])
        {
          result[c++] = s[a++];
        }
      else
        return 0;
    }
  if ((sign % 2) == 0) return 1;
  return -1;
}

void Subsets::show(std::ostream &o, const Subset &a)
{
  o << "[";
  for (size_t i = 0; i < a.size(); i++)
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
