/* Copyright 2005, Michael E. Stillman */

#ifndef _F4types_h_
#define _F4types_h_

#include <vector>
#include <stdint.h>

#include "../newdelete.hpp"
#include "../engine.h"
#include "../stop.hpp"
#include "f4monlookup.hpp"

#define VECTOR(T) std::vector< T, gc_allocator< T > >
#define sizeofspair(s,len) (sizeof(*s) - sizeof(s->lcm) + (len)*sizeof(s->lcm[0]))

extern char system_interruptedFlag;
extern int gbTrace;

#include "varpower_monomial.hpp"
#include "ntuple_monomial.hpp"
#include "moninfo.hpp"

// Coefficients.  The implementation of arrays of coeffs
// is done as a private array.  Note that the length is
// not encoded: keep that length separately.
typedef void *F4CoefficientArray;

enum gbelem_type { 
  ELEM_IN_RING,  // These are ring elements
  ELEM_POSSIBLE_MINGEN,   // These are min GB elements which might 
                          // also be min gens, in the graded case, 
                          // they ARE minimal generators
  ELEM_MIN_GB,    // These are elements which are minimal GB elements
  ELEM_NON_MIN_GB // These are elements which are not minimal GB elements
};

enum spair_type {
  F4_SPAIR_SPAIR,
  F4_SPAIR_GCD_ZZ,
  F4_SPAIR_RING,
  F4_SPAIR_SKEW,
  F4_SPAIR_GEN,
  F4_SPAIR_ELEM
};

struct poly : public our_new_delete {
  int len;
  F4CoefficientArray coeffs;
  monomial_word *monoms; // This is all of the monomials written contiguously
};

struct pre_spair : public our_new_delete {
  enum spair_type type;
  int deg1; // simple degree of quot
  varpower_monomial quot;
  int j;
  bool are_disjoint;
};

struct spair : public our_new_delete {
  spair * next;
  spair_type type;
  int deg; /* sugar degree of this spair */
  int i;
  int j;
  monomial_word lcm[1]; // packed_monomial
};

struct gbelem : public our_new_delete {
  poly f;
  int deg;
  int alpha; // the homogenizing degree
  gbelem_type minlevel;
};

class gb_array : public VECTOR(gbelem *) {};

struct sparse_row : public our_new_delete {
  int len;
  F4CoefficientArray coeffs;
  int *comps;
};

struct row_elem : public our_new_delete {
  // Header information
  packed_monomial monom;
  int elem;
  
  // The polynomial itself
  int len;
  F4CoefficientArray coeffs;
  int *comps;
};

struct column_elem : public our_new_delete {
  packed_monomial monom;
  int degree;
  int gb_divisor; // -1 if none, otherwise >= 0.
  int head; // which row is being used as a pivot for this column
  int ord; // Set before doing LU decomposition
};

struct coefficient_matrix : public our_new_delete {
  typedef VECTOR(row_elem) row_array;
  typedef VECTOR(column_elem) column_array;
  
  row_array rows;
  column_array columns;
  
  VECTOR(int) column_order; // Inverse to ord values
};

class ColumnsSorter
{
public:
  typedef MonomialInfo::value monomial;
  typedef long value;
private:
  const MonomialInfo *M;
  const coefficient_matrix *mat;
  long ncmps;
public:
  int compare(value a, value b)
  {
    ncmps ++;
    return M->compare_grevlex(mat->columns[a].monom,mat->columns[b].monom);
  }

  ColumnsSorter(const MonomialInfo *M0, const coefficient_matrix *mat0)
    : M(M0), mat(mat0), ncmps(0) {}

  long ncomparisons() const { return ncmps; }
  
  ~ColumnsSorter() {} 
};

class PreSPairSorter
{
public:
  typedef pre_spair * value;
private:
  long ncmps;
public:
  int compare(value a, value b)
  {
    ncmps ++;
    return varpower_monomials::compare(a->quot, b->quot);
  }

  PreSPairSorter()
    : ncmps(0) {}

  long ncomparisons() const { return ncmps; }
  
  ~PreSPairSorter() {} 
};

typedef F4MonomialLookupTableT<int32_t> MonomialLookupTable;

template <typename MonInfo> class MonomialHashTable;

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
