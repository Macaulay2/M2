/* Copyright 2005, Michael E. Stillman */

#ifndef _lingb_h_
#define _lingb_h_

#include <map>
#include <vector>
#include "../comp_gb.hpp"
#include "../poly.hpp"
#include "../coeffrings.hpp"
#include "MonomialSet.hpp"
#include "SPairSet.hpp"

// This should be either DMat or SMat
#define MATTYPE DMat

#define VECTOR(T) std::vector< T, gc_allocator< T > >

class MonomialLookupTable;
typedef int * monomial;

template<typename COEFF_TYPE>
struct mypoly : public our_new_delete 
{
  int len;
  COEFF_TYPE *coeffs;
  monomial *monoms;
};

enum gbelem_type { 
  ELEM_IN_RING,  // These are ring elements
  ELEM_POSSIBLE_MINGEN,   // These are min GB elements which might 
			  // also be min gens, in the graded case, 
                          // they ARE minimal generators
  ELEM_MIN_GB,    // These are elements which are minimal GB elements
  ELEM_NON_MIN_GB // These are elements which are not minimal GB elements
};

template<typename COEFF_TYPE>
struct mygbelem : public our_new_delete
{
  mypoly<COEFF_TYPE> f;
  int deg;
  int alpha; // the homogenizing degree
  unsigned char is_minimal;
  gbelem_type minlevel;
};

template<typename COEFF_TYPE>
class gb_array : public VECTOR(mygbelem<COEFF_TYPE> *)
{
};

template<typename COEFF_TYPE>
struct row_elem : public our_new_delete
{
  // Header information
  monomial monom;
  int elem;

  // The polynomial itself
  int len;
  COEFF_TYPE *coeffs;
  int *comps;
};

struct column_elem : public our_new_delete
{
  monomial monom;
  int degree;
  int gb_divisor; // -1 if none, otherwise >= 0.
  int head; // which row is being used as a pivot for this column
  int ord; // Set before doing LU decomposition
};

template<typename COEFF_TYPE>
struct coefficient_matrix : public our_new_delete 
{
  typedef std::map<monomial, int> monomial_map;
  typedef row_elem<COEFF_TYPE> row_elem;
  typedef VECTOR(row_elem) row_array;
  typedef VECTOR(column_elem) column_array;

  row_array rows;
  column_array columns;
  monomial_map H0; // Hash table (well...  sort of) of
                         // monomial --> int (column)

  VECTOR(int) column_order; // Inverse to ord values
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
