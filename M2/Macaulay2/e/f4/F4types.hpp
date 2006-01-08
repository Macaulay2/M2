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

// int8, int16, int32, int64, uint8, uint16, uint32, uint64
// are all defined in "../../util/targettypes.h"

// The various kinds of monomials

#include "varpower_monomial.hpp"
#include "ntuple_monomial.hpp"
#include "moninfo.hpp"

// Coefficients.  The implementation of arrays of coeffs
// is done as a private array.  Note that the length is
// not encoded: keep that length separately.
typedef void *CoefficientArray;

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
  
template <typename CoeffRing>
class F4types
{
public:
  typedef typename CoeffRing::elem COEFF_TYPE;
  
  // Main types

  struct poly : public our_new_delete {
    int len;
    COEFF_TYPE *coeffs;
    packed_monomial *monoms;  // Component is in the monomial.  Where?
    monomial_word *monom_space;
  };

  struct gbelem : public our_new_delete {
    poly f;
    int deg;
    int alpha; // the homogenizing degree
    unsigned char is_minimal;
    gbelem_type minlevel;
  };

  class gb_array : public VECTOR(gbelem *) {};

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

  struct row_elem : public our_new_delete {
    // Header information
    packed_monomial monom;
    int elem;

    // The polynomial itself
    int len;
    COEFF_TYPE *coeffs;
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

};

template <typename MonInfo> class MonomialHashTable;

#define INCLUDE_F4_TYPES \
  typedef typename CoeffRing::ring_type RingType; \
  typedef typename CoeffRing::elem elem; \
  typedef typename CoeffRing::elem COEFF_TYPE; \
  typedef typename F4types<CoeffRing>::poly poly; \
  typedef typename F4types<CoeffRing>::gbelem gbelem; \
  typedef typename F4types<CoeffRing>::spair spair; \
  typedef typename F4types<CoeffRing>::gb_array gb_array; \
  typedef typename F4types<CoeffRing>::row_elem row_elem; \
  typedef typename F4types<CoeffRing>::column_elem column_elem; \
  typedef typename F4types<CoeffRing>::coefficient_matrix coefficient_matrix; \
  typedef MonomialLookupTable<packed_monomial> MonomialLookupTable; \
  

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
