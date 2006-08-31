/* Copyright 2005, Michael E. Stillman */

#ifndef _F4types_h_
#define _F4types_h_

#include "../newdelete.hpp"
#include "../engine.h"
#include "../stop.hpp"
#include <vector>
#include "montable.hpp"

#define VECTOR(T) std::vector< T, gc_allocator< T > >
extern char system_interruptedFlag;
extern int gbTrace;

// int32_t is defined in stdint.h
#include <stdint.h>

// The various kinds of monomials

#include "varpower_monomial.hpp"
#include "ntuple_monomial.hpp"
#include "moninfo.hpp"

// Coefficients.  The implementation of arrays of coeffs
// is done as a private array.  Note that the length is
// not encoded: keep that length separately.
typedef void *F4CoefficientArray;

// routines needed for coefficient arrays:
// ring_elem_to_coeff_array(K, len, ringelemarray, CoefficientArray)
// coeff_to_ringelem_array(K,  len, ringelemarray, CoefficientArray)
// And then we will need in gauss() routine to do arithmetic
// on them.  However, this can be handled on a ring by ring basis.

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
  monomial_word *monom_space; // This is all of the monomials written contiguously
};

struct spair : public our_new_delete {
  spair * next;
  spair_type type;
  int deg; /* sugar degree of this spair */
  packed_monomial lcm;
  union {
    struct {
      packed_monomial first_monom;
      int first_gb_num;
      packed_monomial second_monom;
      int second_gb_num;
    } spair;
    struct {
      int column; // original column
    } poly;
  } s;
};

struct gbelem : public our_new_delete {
  poly f;
  int deg;
  int alpha; // the homogenizing degree
  gbelem_type minlevel;
};

class gb_array : public VECTOR(gbelem *) {};

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
  //  typedef std::map<packed_monomial, int> monomial_map;
  typedef VECTOR(row_elem) row_array;
  typedef VECTOR(column_elem) column_array;
  
  row_array rows;
  column_array columns;
  // monomial_map H0; // Hash table (well...  sort of) of
  // monomial --> int (column)
  
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

typedef MonomialLookupTableT<int32_t> MonomialLookupTable;

template <typename MonInfo> class MonomialHashTable;

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
