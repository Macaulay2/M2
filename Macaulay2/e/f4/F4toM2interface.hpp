#ifndef __F4toM2interface_h_
#define __F4toM2interface_h_

#include "F4types.hpp"

#define MATTYPE DMat

template<typename CoeffRing>
class F4toM2Interface
{
  INCLUDE_F4_TYPES;
public:
  static void poly_set_degrees(const CoeffRing *K,
			       const MonomialInfo *MI,
			       const M2_arrayint wts,
			       const poly &f,
			       int &deg, 
			       int &alpha);
  
  static void from_M2_vec(const CoeffRing *K,
  			  const MonomialInfo *MI,
			  const FreeModule *F,
			  vec v,
			  poly &result);

  static vec to_M2_vec(const CoeffRing *K, 
  		       const MonomialInfo *MI,
		       const poly &f,
		       const FreeModule *F);

  static void from_M2_matrix(const CoeffRing *K, 
  			     const MonomialInfo *MI,
			     const Matrix *m, 
			     M2_arrayint wts,
			     gb_array &result_polys);
  
  static Matrix *to_M2_matrix(const CoeffRing *K, 
			      const MonomialInfo *MI,
			      gb_array &polys, 
			      const FreeModule *F);

  static MutableMatrix *to_M2_MutableMatrix(const RingType *K,
					    coefficient_matrix *mat);

};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:


