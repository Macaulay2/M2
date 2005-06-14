#ifndef __interface_h_
#define __interface_h_

#include "lingb.hpp"

template<typename CoeffRing>
class M2Interface
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;

  typedef typename CoeffRing::elem COEFF_TYPE;
  typedef mypoly<COEFF_TYPE> poly;
  typedef mygbelem<COEFF_TYPE> gbelem;
  typedef gb_array<COEFF_TYPE> gb_array;


public:
  static void poly_set_degrees(CoeffRing *K, 
			       const M2_arrayint wts,
			       const poly &f,
			       int &deg, 
			       int &alpha);
  
  static void from_M2_vec(CoeffRing *K, 
			  MonomialSet *H, 
			  const FreeModule *F,
			  vec v,
			  mypoly<COEFF_TYPE> &poly);

  static vec to_M2_vec(CoeffRing *K, 
		       poly &f,
		       const FreeModule *F);

  static void from_M2_matrix(CoeffRing *K, 
			     const Matrix *m, 
			     MonomialSet *H,
			     M2_arrayint wts,
			     gb_array &result_polys);
  
  static Matrix *to_M2_matrix(CoeffRing *K, 
			      gb_array &polys, 
			      const FreeModule *F);

  static MutableMatrixXXX *to_M2_MutableMatrix(const RingType *K,
					       coefficient_matrix<COEFF_TYPE> *mat);

  // This will change to SMat:
  static MATTYPE<CoeffRing> *to_M2_Mat(const RingType *K,
				    coefficient_matrix<COEFF_TYPE> *mat);
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:


