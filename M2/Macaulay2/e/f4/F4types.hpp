/* Copyright 2005, Michael E. Stillman */

#ifndef _F4types_h_
#define _F4types_h_

#include "../newdelete.hpp"
#include "../../d/M2types.h"
#include <vector>
#define VECTOR(T) std::vector< T, gc_allocator< T > >

template <typename CoeffRing, typename MonInfo>
class F4types
{
public:
  typedef typename CoeffRing::elem COEFF_TYPE;
  
  // Main types
  typedef long * packed_monomial;

  struct poly : public our_new_delete {
    int len;
    COEFF_TYPE *coeffs;
    packed_monomial *monoms;  // Component is in the monomial.  Where?
    long *monom_space;
  };

  enum gbelem_type { 
    ELEM_IN_RING,  // These are ring elements
    ELEM_POSSIBLE_MINGEN,   // These are min GB elements which might 
			    // also be min gens, in the graded case, 
                            // they ARE minimal generators
    ELEM_MIN_GB,    // These are elements which are minimal GB elements
    ELEM_NON_MIN_GB // These are elements which are not minimal GB elements
  };

  struct gbelem : public our_new_delete {
    poly f;
    int deg;
    int alpha; // the homogenizing degree
    unsigned char is_minimal;
    gbelem_type minlevel;
  };

  class gb_array : public VECTOR(gbelem *) {};

  enum spair_type {
    SPAIR_SPAIR,
    SPAIR_GCD_ZZ,
    SPAIR_RING,
    SPAIR_SKEW,
    SPAIR_GEN,
    SPAIR_ELEM
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

class MonomialLookupTable;
template <typename MonInfo> class MonomialHashTable;
class SPairSet;

#define INCLUDE_F4_TYPES \
  typedef long * packed_monomial; \
  typedef typename CoeffRing::ring_type RingType; \
  typedef typename CoeffRing::elem elem; \
  typedef typename CoeffRing::elem COEFF_TYPE; \
  typedef typename F4types<CoeffRing,MonInfo>::poly poly; \
  typedef typename F4types<CoeffRing,MonInfo>::gbelem gbelem; \
  typedef typename F4types<CoeffRing,MonInfo>::spair spair; \
  typedef typename F4types<CoeffRing,MonInfo>::gb_array gb_array; \
  typedef typename F4types<CoeffRing,MonInfo>::row_elem row_elem; \
  typedef typename F4types<CoeffRing,MonInfo>::column_elem column_elem; \
  typedef typename F4types<CoeffRing,MonInfo>::coefficient_matrix coefficient_matrix; \
  

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
