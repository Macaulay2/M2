/* Copyright 2005, Michael E. Stillman */

#ifndef _F4types_h_
#define _F4types_h_

#include <memory>

#include "engine-includes.hpp"
#include "f4-monlookup.hpp"
#include "moninfo.hpp"
#include "varpower-monomial.hpp"

#define sizeofspair(s, len) \
  (sizeof(*s) - sizeof(s->lcm) + (len) * sizeof(s->lcm[0]))

// Coefficients.  The implementation of arrays of coeffs
// is done as a private array.  Note that the length is
// not encoded: keep that length separately.
typedef void *F4CoefficientArray;

enum gbelem_type {
  ELEM_IN_RING,          // These are ring elements
  ELEM_POSSIBLE_MINGEN,  // These are min GB elements which might
                         // also be min gens, in the graded case,
                         // they ARE minimal generators
  ELEM_MIN_GB,           // These are elements which are minimal GB elements
  ELEM_NON_MIN_GB        // These are elements which are not minimal GB elements
};

enum spair_type {
  F4_SPAIR_SPAIR,
  F4_SPAIR_GCD_ZZ,
  F4_SPAIR_RING,
  F4_SPAIR_SKEW,
  F4_SPAIR_GEN,
  F4_SPAIR_ELEM
};

struct poly : public our_new_delete
{
  int len;
  F4CoefficientArray coeffs;
  monomial_word *monoms;  // This is all of the monomials written contiguously
};

struct pre_spair : public our_new_delete
{
  enum spair_type type;
  int deg1;  // simple degree of quot
  varpower_monomial quot;
  int j;
  bool are_disjoint;
};

struct spair : public our_new_delete
{
  spair *next;
  spair_type type;
  int deg; /* sugar degree of this spair */
  int i;
  int j;
  monomial_word lcm[1];  // packed_monomial
};

struct gbelem : public our_new_delete
{
  poly f;
  int deg;
  int alpha;  // the homogenizing degree
  gbelem_type minlevel;
};

typedef VECTOR(gbelem *) gb_array;

struct sparse_row : public our_new_delete
{
  int len;
  F4CoefficientArray coeffs;
  int *comps;
};

struct row_elem : public our_new_delete
{
  // Header information
  packed_monomial monom;
  int elem;

  // The polynomial itself
  int len;
  F4CoefficientArray coeffs;
  int *comps;
};

struct column_elem : public our_new_delete
{
  packed_monomial monom;
  int head;  // which row is being used as a pivot for this column.
             // -1 means none, -2 means not set yet
};

struct coefficient_matrix : public our_new_delete
{
  typedef VECTOR(row_elem) row_array;
  typedef VECTOR(column_elem) column_array;

  row_array rows;
  column_array columns;
};

typedef int (MonomialInfo::*CompareFunction)(const monomial_word *,
                                             const monomial_word *) const;

class ColumnsSorter
{
 public:
  typedef MonomialInfo::value monomial;
  typedef int value;

 private:
  const MonomialInfo *M;
  //  const coefficient_matrix *mat;
  const coefficient_matrix::column_array &cols;

  static long ncmps;
  static long ncmps0;
  //  int (MonomialInfo::*compareFcn)(const monomial_word *, const monomial_word
  //  *) const;
 public:
  int compare(value a, value b)
  {
    //    ncmps ++;
    return M->compare_grevlex(cols[a].monom, cols[b].monom);
    // return (M->*(M->compare))(mat->columns[a].monom,mat->columns[b].monom);

    //    return (M->*compareFcn)(col[a].monom,col[b].monom);
    //    return (M->*compareFcn)(mat->columns[a].monom,mat->columns[b].monom);
    //    return
    //    M->compare_grevlex(mat->columns[a].monom,mat->columns[b].monom);
  }

  bool operator()(value a, value b)
  {
    // ncmps0 ++;
    return (M->compare_grevlex(cols[a].monom, cols[b].monom) == LT);
    //    return (M->*(M->compare))(mat->columns[a].monom,mat->columns[b].monom)
    //    == LT;
  }

  ColumnsSorter(const MonomialInfo *M0, const coefficient_matrix *mat0)
      : M(M0),
        /* mat(mat0), */ cols(
            mat0->columns) /* , compareFcn(&MonomialInfo::compare_grevlex) */
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

typedef F4MonomialLookupTableT<int32_t> MonomialLookupTable;

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
