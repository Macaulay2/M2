#ifndef __interface_h_
#define __interface_h_

#include "Monomials.h"
#include "MonomialSet.h"
#include "../freemod.hpp"
#include <vector>

class Matrix;

typedef int COEFF_TYPE;

struct poly {
  int len;
  COEFF_TYPE *coeffs;
  monomial *monoms; // points into either S, or into a set Sd
  
  // One must be careful about adding two polynomials, since we
  // need to have an order on the monomials in this case
};

void from_M2_vec(MonomialSet *H,
		 const FreeModule *F, 
		 vec v, 
		 poly &result);

vec to_M2_vec(poly &f,
	      const FreeModule *F);

void from_M2_matrix(const Matrix *m, 
		    MonomialSet *H,
		    std::vector<poly,gc_alloc> &result_polys);

Matrix *to_M2_matrix(std::vector<poly,gc_alloc> &polys, const FreeModule *F);

void spair_testing(MonomialSet *H,
		   std::vector<poly,gc_alloc> &result_polys);

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
