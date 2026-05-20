// Copyright 1997 by Michael E. Stillman
#ifndef _comb_hh_
#define _comb_hh_

/**
 * @file comb.hpp
 * @brief `Subsets` --- combinatorial-number-system encoding of `p`-subsets of `{0,...,n-1}`.
 *
 * `Subsets` enumerates and encodes `q`-element subsets of
 * `{0, 1, ..., n - 1}` (for any `q <= p`) as a single integer index
 * via the standard combinatorial number system. The constructor
 * `Subsets(n, p)` precomputes a binomial table; `encode(a)` returns
 * the index of a sorted `Subset`, `decode(i, a)` writes the `i`-th
 * subset back into `a`, and the static `increment(n, s)` advances
 * `s` to the next subset in canonical order. `encodeBoundary` and
 * `concatenateSubsets` cover the face-map and shuffle operations
 * needed for exterior-power signs. The encoding is stable under
 * extension of the ambient set --- the index of a subset depends
 * only on its element list, not on `n` --- so adding new ambient
 * elements appends new indices without renumbering existing ones.
 *
 * Engine consumers are the exterior-power constructions on free
 * modules in `freemod.cpp` and `schorder.cpp`, and the minor /
 * pfaffian routines in `det.cpp`, `matrix.cpp`, and `pfaff.cpp`,
 * which iterate over row- and column-subsets via `Subsets::increment`.
 */

#include <cassert>
#include <vector>
#include <iosfwd>

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
// example: 10 = 0 1 5.  This is binomial(0,1) + binomial(1,2) + binomial(5,3) =
// 10
// example: 9 = 2 3 4.  9 == 2 + binomial(3,2) + binomial(4,3) = 2 + 3 + 4

typedef std::vector<size_t> Subset;

/**
 * @brief Bijective integer encoding of `q`-subsets of `{0, ..., n-1}` via
 * `binomial(a_0, 1) + binomial(a_1, 2) + ... + binomial(a_{q-1}, q)`.
 *
 * @details Constructed with maximum bounds `(n, p)`; supports encoding and
 * decoding of `q`-subsets for all `q <= p`. The encoding does not
 * depend on `n`, so the same index space scales as the underlying
 * set grows. Throws if a result would not fit in `size_t`. Used
 * wherever the engine enumerates exterior-style subsets without
 * allocating an explicit table.
 */
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

  // Take a p-subset s of 0..n-1 and change it to be the next one in the above
  // mentioned order
  static bool increment(size_t n, Subset &s);

  static bool increment(size_t n, size_t subset_size, size_t *subset);

  static bool isValid(size_t nElements, size_t subsetSize, const size_t *a);
  static bool isValid(size_t nElements, size_t subsetSize, const int *a);

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
  size_t mNumElements;    // number of elements in the set: the elements are
                          // 0..mNumElements-1
  size_t mMaxSubsetSize;  // This table can only handle subsets up to this size.
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
