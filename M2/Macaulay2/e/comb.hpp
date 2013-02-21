// Copyright 1997 by Michael E. Stillman
#ifndef _comb_hh_
#define _comb_hh_

#include "style.hpp"
#include "array.hpp"
#include <vector>
#include <iosfwd>
#define M2_ASSERT assert

class comb
     // Encoding, decoding of elements [i0,...,i(p-1)] to ord value.
{
// tab[d][n] is n choose d
// tab[0][n] == 1, tab[d][0] == 0, tab[0][0] = 1.
// tab[d][n] == tab[d][n-1] + tab[d-1][n-1]

  void expand(int n, int d);

  int d_len(int n)
    {
      int i = tab.rawelem(n).length();
      if (i == 0)
        {
          expand(n,0);
          i = tab.rawelem(n).length();
        }
      return i;
    }

  void text_out(buffer &o);
public:
  array_class<array<int> > tab;

  comb(int n, int d);
  // insure that all numbers in (n choose d) and (n choose (d-1)) can be encoded/decoded.
  // The table can be built larger than this.

  ~comb() {}

  int binom(int n, int p)
    {
      if (p < 0) return 0;
      if (p >= tab.length())
        expand(n,p);
      else if (n >= tab[p].length())
        expand(n,p);
      return tab[p][n];
    }
  int encode(int *a, int p);
  void decode(int c, int *a, int p);

  // The following static functions do not access 'tab'
  static bool increment(int p, int n, int *s);
  static bool valid_subset(int p, int n, const int *s);
  static int mult_subsets(int p, const int *s,
                          int q, const int *t, int *&result);
};

// Class: Subsets
// Manipulate p-subsets of 0..n-1, for fixed n, but possibly several p.
//
// Encoding and decoding is done as follows:
// Consider e.g. 3-subsets:
// These are
//   0 = 0 1 2
//   1 = 0 1 3
//   2 = 0 2 3
//   3 = 1 2 3
//   4 = 0 1 4
//   5 = 0 2 4
//   6 = 0 3 4
//   7 = 1 2 4
//   8 = 1 3 4
//   9 = 2 3 4
//   ...  (if there are more than 5 elements in the set).  
// Notice that the encoded value does not depend on the number of elements
// [a0, a1, ..., a(p-1)] becomes
// the integer binomial(a0,1) + binomial(a1,2) + binomial(a2,3) + ...
// example: 10 = 0 1 5.  This is binomial(0,1) + binomial(1,2) + binomial(5,3) = 10
// example: 9 = 2 3 4.  9 == 2 + binomial(3,2) + binomial(4,3) = 2 + 3 + 4

typedef std::vector<size_t> Subset;

class Subsets
{
public:
  // Create an object which allows to encode and decode
  // q-subsets of a (size n) set, for all q <= p.
  // Throws an exception if the result will not fit into a size_t.
  Subsets(size_t n, size_t p);

  ~Subsets();

  bool isValid(const Subset &a);

  size_t encode(const Subset &a);  // an assertion failure if a.size() is > p, 

  // encode the subset obtained by removing the
  // element of a at index 'index',
  size_t encodeBoundary(size_t index, const Subset &a);  

  void decode(size_t val, Subset &result);

  static void show(std::ostream &o, const Subset &a);

  // Take a p-subset s of 0..n-1 and change it to be the next one in the above mentioned order
  static bool increment(size_t n, Subset &s);

  // Places the sorted value of [s0..s(p-1),t0..t(q-1)]
  // (where p=size of s, q = size of t)
  // into 'result', and returns the sign of the permutation
  // required.  0 is returned if s and t have a common
  // element, in which case, the value in 'result'
  // is undefined.
  // Note: The size of result is not changed, and it needs to be
  // s.size() + t.size().
  static int concatenateSubsets(const Subset &s,
                                const Subset &t,
                                Subset &result);
private:
  size_t binom(size_t n, size_t p)
  {
    assert(n <= mNumElements);
    assert(p <= mMaxSubsetSize);
    return mTable[p][n];
  }

  size_t **mTable;
  size_t mNumElements; // number of elements in the set: the elements are 0..mNumElements-1
  size_t mMaxSubsetSize; // This table can only handle subsets up to this size.
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
