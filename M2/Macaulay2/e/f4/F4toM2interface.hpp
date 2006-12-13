#ifndef __F4toM2interface_h_
#define __F4toM2interface_h_

#include "F4types.hpp"
#include "gausser.hpp"
#define MATTYPE DMat

class F4toM2Interface
{
public:
  static void poly_set_degrees(const Gausser *KK,
			       const MonomialInfo *MI,
			       const M2_arrayint wts,
			       const poly &f,
			       int &deg, 
			       int &alpha);
  
  static void from_M2_vec(const Gausser *KK,
  			  const MonomialInfo *MI,
			  const FreeModule *F,
			  vec v,
			  poly &result);

  static vec to_M2_vec(const Gausser *KK, 
  		       const MonomialInfo *MI,
		       const poly &f,
		       const FreeModule *F);

  static void from_M2_matrix(const Gausser *KK, 
  			     const MonomialInfo *MI,
			     const Matrix *m, 
			     M2_arrayint wts,
			     gb_array &result_polys);
  
  static Matrix *to_M2_matrix(const Gausser *KK, 
			      const MonomialInfo *MI,
			      gb_array &polys, 
			      const FreeModule *F);

  static MutableMatrix *to_M2_MutableMatrix(const Gausser *KK,
					    coefficient_matrix *mat,
					    gb_array &gens,
					    gb_array &gb);

};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
