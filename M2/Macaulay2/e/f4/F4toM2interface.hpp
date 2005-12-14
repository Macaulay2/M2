#ifndef __F4toM2interface_h_
#define __F4toM2interface_h_

#include "F4types.hpp"

template<typename CoeffRing, typename MonInfo>
class F4toM2Interface
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;
  typedef typename CoeffRing::elem COEFF_TYPE;

  typedef typename MonInfo::monomial packed_monomial;
  typedef mypoly<COEFF_TYPE> poly;
  typedef mygbelem<COEFF_TYPE> gbelem;
  typedef gb_array<COEFF_TYPE> gb_array;

public:
  static void poly_set_degrees(const CoeffRing *K,
			       const MonInfo *MI,
			       const M2_arrayint wts,
			       const poly &f,
			       int &deg, 
			       int &alpha);
  
  static void from_M2_vec(const CoeffRing *K,
  			  const MonInfo *MI,
			  const FreeModule *F,
			  vec v,
			  poly &result);

  static vec to_M2_vec(const CoeffRing *K, 
  		       const MonInfo *MI,
		       const poly &f,
		       const FreeModule *F);

  static void from_M2_matrix(const CoeffRing *K, 
  			     const MonInfo *MI,
			     const Matrix *m, 
			     M2_arrayint wts,
			     gb_array &result_polys);
  
  static Matrix *to_M2_matrix(const CoeffRing *K, 
			      const MonInfo *MI,
			      gb_array &polys, 
			      const FreeModule *F);

  static MutableMatrix *to_M2_MutableMatrix(const RingType *K,
					    coefficient_matrix<COEFF_TYPE> *mat);

};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:


