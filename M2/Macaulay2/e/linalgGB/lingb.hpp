/* Copyright 2005, Michael E. Stillman */

#ifndef _lingb_h_
#define _lingb_h_

#include "../comp_gb.hpp"
#include "../poly.hpp"
#include <map>
#include <vector>
#include "MonomialSet.h"
#include "SPairSet.h"
#include "../coeffrings.hpp"

class MonomialLookupTable;
typedef int * monomial;

template<typename COEFF_TYPE>
struct mypoly : public our_new_delete 
{
  int len;
  COEFF_TYPE *coeffs;
  monomial *monoms;
};

#if 0
class CoefficientRingZZp
{
  int p;
public:
  typedef int elem;

  CoefficientRingZZp(int p0) : p(p0) {}

  void init_set(elem *result, elem *a) const { *result = *a; }
  void set_zero(elem &result) const { result = 0; }
  bool is_zero(elem &result) const { return result == 0; }
  void invert(elem &result, elem &a) const;
  void subtract_multiple(elem &result, elem a, elem b);
    // result -= a*b
};
#endif

enum gbelem_type { 
  ELEM_IN_RING,  // These are ring elements
  ELEM_POSSIBLE_MINGEN,   // These are min GB elements which might also be min gens
  // In the graded case, they ARE minimal generators
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
class mygb_array : public std::vector<mygbelem<COEFF_TYPE> *, 
                    gc_allocator<mygbelem<COEFF_TYPE> *> >
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
  typedef std::vector<row_elem, gc_allocator<row_elem> > row_array;
  typedef std::vector<column_elem, gc_allocator<column_elem> > column_array;
  row_array rows;
  column_array columns;
  monomial_map H0; // Hash table (well...  sort of) of
                         // monomial --> int (column)
  std::vector<int> column_order;
};

template<typename CoefficientRing>
class M2Interface
{
  typedef typename CoefficientRing::elem COEFF_TYPE;
  typedef mypoly<COEFF_TYPE> poly;
  typedef mygbelem<COEFF_TYPE> gbelem;
  typedef mygb_array<COEFF_TYPE> gb_array;


public:
  static void poly_set_degrees(CoefficientRing *K, 
			       const M2_arrayint wts,
			       const poly &f,
			       int &deg, 
			       int &alpha);
  
  static void from_M2_vec(CoefficientRing *K, 
			  MonomialSet *H, 
			  const FreeModule *F,
			  vec v,
			  mypoly<COEFF_TYPE> &poly);

  static vec to_M2_vec(CoefficientRing *K, 
		       poly &f,
		       const FreeModule *F);

  static void from_M2_matrix(CoefficientRing *K, 
			     const Matrix *m, 
			     MonomialSet *H,
			     M2_arrayint wts,
			     gb_array &result_polys);
  
  static Matrix *to_M2_matrix(CoefficientRing *K, 
			      gb_array &polys, 
			      const FreeModule *F);

  static MutableMatrix *to_M2_MutableMatrix(const Ring *K,
					    coefficient_matrix<COEFF_TYPE> *mat);
};


GBComputation *createLinearAlgebraGB(const Matrix *m,
				     M2_bool collect_syz,
				     int n_rows_to_keep,
				     M2_arrayint gb_weights,
				     int strategy,
				     M2_bool use_max_degree,
				     int max_degree);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
