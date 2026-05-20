/* Copyright 2005-2021, Michael E. Stillman */

#ifndef _F4types_h_
#define _F4types_h_

/**
 * @file f4/f4-types.hpp
 * @brief Shared type vocabulary used across the F4 engine.
 *
 * Declares the inner-loop record types the F4 algorithm
 * shares. Polynomial form: `GBF4Polynomial` (length + opaque
 * `ElementArray coeffs` + flat `monomial_word* monoms` buffer)
 * is what each basis element stores. Basis side: `gbelem`
 * wraps a `GBF4Polynomial` with `deg`, `alpha` (homogenising
 * degree), and a `gbelem_type` tag --- `ELEM_IN_RING` /
 * `ELEM_POSSIBLE_MINGEN` / `ELEM_MIN_GB` / `ELEM_NON_MIN_GB`
 * --- and `gb_array = std::vector<gbelem*>` is the basis list.
 * Pair side: `pre_spair` is the proto-pair staged before full
 * materialisation; `spair` carries `(type, deg, i, j,
 * monomial_word* lcm)` where the lcm is a pointer to an
 * externally-allocated `MemoryBlock` slot (the file also
 * defines a vestigial `sizeofspair(s, len)` macro from an
 * older design where `lcm` was a flexible array member --- the
 * only call site is now commented out in `f4-spairs.cpp`). Two
 * pair-type enums coexist: the legacy `spair_type` (with
 * `F4_SPAIR_GCD_ZZ`, `F4_SPAIR_RING`, `F4_SPAIR_SKEW`,
 * `F4_SPAIR_GEN`, `F4_SPAIR_ELEM`, `F4_SPAIR_SPAIR`) and the
 * modern `enum class SPairType { SPair, Generator, Retired }`
 * with a TODO to absorb the remaining cases.
 *
 * Macaulay matrix: `coefficient_matrix` is a `(row_array,
 * column_array)` pair; each `row_elem` holds an opaque
 * `ElementArray coeffs` plus a `comps[len]` index array (`new[]`-
 * allocated, no longer arena-backed), and each `column_elem`
 * tracks the pivoting `head` row. The transient
 * `sparse_row` mirrors `row_elem` with the `comps` allocated
 * from a memory block. The sorters --- `ColumnsSorter` (sorts
 * column indices by `MonomialInfo::compare` on the column
 * monomials), `GBSorter` (sorts basis indices by leading
 * monomial), `PreSPairSorter` (sorts pre-pairs by varpower
 * quotient) and `SPairCompare` (degree first, `i` tiebreak,
 * driving `f4-spairs.hpp`'s priority queue) --- all carry
 * static comparison counters. `MonomialLookupTable` is the
 * `F4MonomialLookupTableT<int32_t>` instantiation.
 *
 * Pulls together the encoded-monomial layer (`moninfo.hpp`,
 * `varpower-monomial.hpp`), the lookup-table type
 * (`f4-monlookup.hpp`), and the templated coefficient
 * arithmetic (`VectorArithmetic.hpp`). Counterpart to the new
 * F4 engine's `gb-f4/MonomialTypes.hpp`.
 *
 * @see f4.hpp
 * @see f4-spairs.hpp
 * @see moninfo.hpp
 */


#include <climits>                   // for INT_MIN
#include "VectorArithmetic.hpp"      // for ElementArray
#include "f4/f4-monlookup.hpp"       // for F4MonomialLookupTableT
#include "f4/moninfo.hpp"            // for MonomialInfo, monomial_word, pac...
#include "f4/varpower-monomial.hpp"  // for varpower_monomials, varpower_mon...
#include "newdelete.hpp"             // for our_new_delete, VECTOR, (gc_allocator)
#include "style.hpp"                 // for LT

#define sizeofspair(s, len) \
  (sizeof(*s) - sizeof(s->lcm) + (len) * sizeof(s->lcm[0]))

enum gbelem_type {
  ELEM_IN_RING,          // These are ring elements
  ELEM_POSSIBLE_MINGEN,  // These are min GB elements which might
                         // also be min gens, in the graded case,
                         // they ARE minimal generators
  ELEM_MIN_GB,           // These are elements which are minimal GB elements
  ELEM_NON_MIN_GB        // These are elements which are not minimal GB elements
};

enum spair_type {
  F4_SPAIR_SPAIR,        // arising from an honest spair
  F4_SPAIR_GCD_ZZ,       // for gbs over the integers
  F4_SPAIR_RING,         // an spair between a generator and a gen of the defining ideal
  F4_SPAIR_SKEW,         // from exterior variables times a monomial
  F4_SPAIR_GEN,          // a generator of the defining ideal
  F4_SPAIR_ELEM          // 
};

enum class SPairType {
  SPair,
  Generator,
  Retired
  // later we would also like GCDZZ, Ring, Skew to handle those cases as well
};

/**
 * @brief Compact polynomial layout used inside the F4 GB engine.
 *
 * @details `len` is the number of terms, `coeffs` is the parallel
 * `ElementArray` of coefficients, and `monoms` is a flat buffer of
 * `monomial_word`s with every term's monomial laid out contiguously.
 * Owned by an enclosing memory block; no destructor needed.
 */
struct GBF4Polynomial
{
  int len;
  ElementArray coeffs;
  monomial_word *monoms;  // This is all of the monomials written contiguously
};

struct pre_spair 
{
  //enum spair_type type;
  SPairType type;
  int deg1;  // simple degree of quot
  varpower_monomial quot;
  int j;
  bool are_disjoint;
};

struct spair
{
public:
  SPairType type;
  int deg; /* sugar degree of this spair */
  int i;
  int j;
  monomial_word* lcm;  // pointer to a monomial space
  
  spair() : type(SPairType::Retired),deg(INT_MIN),i(-1),j(-1),lcm(nullptr) {}
  spair(SPairType t, int deg0, int i0, int j0, monomial_word* lcm0) :
    type(t), deg(deg0), i(i0), j(j0), lcm(lcm0) {}
};

struct gbelem
{
  GBF4Polynomial f;
  int deg;
  int alpha;  // the homogenizing degree
  gbelem_type minlevel;
};

typedef std::vector<gbelem *> gb_array;

struct sparse_row
{
  int len;
  ElementArray coeffs;
  int *comps; // of length len, allocated in a memory block.
};

struct row_elem
{
  // Header information
  packed_monomial monom; // pointer, allocated monomial in a memory block
  int elem;

  // The polynomial itself
  int len;
  ElementArray coeffs;
  int *comps; // of length len, no longer allocated in a memory block, but with new[]
};

struct column_elem
{
  packed_monomial monom; // pointer, allocated monomial in a memory block
  int head;  // which row is being used as a pivot for this column.
             // -1 means none, -2 means not set yet
};

struct coefficient_matrix
{
  typedef std::vector<row_elem> row_array;
  typedef std::vector<column_elem> column_array;

  row_array rows;
  column_array columns;
};

typedef int (MonomialInfo::*CompareFunction)(const monomial_word *,
                                             const monomial_word *) const;

/**
 * @brief Comparator that orders Macaulay-matrix column indices by the
 * monomial each column represents, using the ambient `MonomialInfo`.
 *
 * @details Fed to `std::sort` so the F4 matrix builder can put the columns
 * into the monoid's order before reduction. Keeps two static
 * counters (`ncmps`, `ncmps0`) to let profiling code report how
 * many comparisons the sort took.
 */
class ColumnsSorter
{
 public:
  typedef MonomialInfo::value monomial;
  typedef int value;

 private:
  const MonomialInfo *M;
  const coefficient_matrix::column_array &cols;

  static long ncmps;
  static long ncmps0;
 public:
  int compare(value a, value b)
  {
    //    ncmps ++;
    return M->compare(cols[a].monom, cols[b].monom);
  }

  bool operator()(value a, value b)
  {
    int newret = M->compare(cols[a].monom, cols[b].monom);
    return (newret == GT);
  }

  ColumnsSorter(const MonomialInfo *M0, const coefficient_matrix *mat0)
      : M(M0),
        cols(mat0->columns)
  {
  }

  long ncomparisons() const { return ncmps; }
  long ncomparisons0() const { return ncmps0; }
  void reset_ncomparisons()
  {
    ncmps0 = 0;
    ncmps = 0;
  }

  ~ColumnsSorter() {}
};

/**
 * @brief Comparator that orders indices into the current GB array (`gb_array`)
 * by each `gbelem`'s leading monomial, in increasing order.
 *
 * @details `operator()` returns true when `gb[a]`'s leading monomial is
 * strictly less than `gb[b]`'s under `MonomialInfo::compare`. Like
 * `ColumnsSorter` it keeps `ncmps` / `ncmps0` profiling counters.
 */
class GBSorter
{
 public:
  typedef MonomialInfo::value monomial;
  typedef int value;

 private:
  const MonomialInfo *M;
  const gb_array& gb;

  static long ncmps;
  static long ncmps0;
 public:
  int compare(value a, value b)
  {
    //    ncmps ++;
    return M->compare(gb[a]->f.monoms, gb[b]->f.monoms);
  }

  bool operator()(value a, value b)
  {
    return (M->compare(gb[a]->f.monoms, gb[b]->f.monoms) == LT); // LT: sort in increasing order...
  }

  GBSorter(const MonomialInfo *M0, const gb_array& gb0)
      : M(M0),
        gb(gb0)
  {
  }

  long ncomparisons() const { return ncmps; }
  long ncomparisons0() const { return ncmps0; }
  void reset_ncomparisons()
  {
    ncmps0 = 0;
    ncmps = 0;
  }

  ~GBSorter() {}
};

/**
 * @brief Comparator that orders `pre_spair*` pointers by the `quot`
 * varpower monomial of each pre-S-pair.
 *
 * @details Uses `varpower_monomials::compare` directly and keeps a single
 * `ncmps` profiling counter. Applied during S-pair generation,
 * before the pre-pairs are promoted to full `spair`s.
 */
class PreSPairSorter
{
 public:
  typedef pre_spair *value;

 private:
  static long ncmps;

 public:
  int compare(value a, value b)
  {
    ncmps++;
    return varpower_monomials::compare(a->quot, b->quot);
  }

  bool operator()(value a, value b)
  {
    ncmps++;
    return varpower_monomials::compare(a->quot, b->quot) == LT;
  }

  PreSPairSorter() {}
  void reset_ncomparisons() { ncmps = 0; }
  long ncomparisons() const { return ncmps; }
  ~PreSPairSorter() {}
};

/**
 * @brief Comparator on indices into an `spair` table, ordering by sugar
 * degree then by the larger of the two parent indices.
 *
 * @details Drives the S-pair queue inside the F4 algorithm: largest sugar
 * degree first, then largest `i`. `operator()(s, t)` returns true
 * when `s` should come before `t` under that order, so it's wired
 * straight into `std::priority_queue` and friends.
 */
class SPairCompare
{
public:
  SPairCompare(const std::vector<spair> &spairs) : mSPairs(spairs) { }

public:
  bool operator()(size_t s, size_t t) const
  {
    const spair& a = mSPairs[s];
    const spair& b = mSPairs[t];
    if (a.deg > b.deg) return true;
    if (a.deg < b.deg) return false;
    if (a.i > b.i) return true;
    return false;
  }

private:
  const std::vector<spair>& mSPairs;
};

typedef F4MonomialLookupTableT<int32_t> MonomialLookupTable;

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
