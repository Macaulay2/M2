#ifndef M2_INTERFACE_CONE_H_
#define M2_INTERFACE_CONE_H_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Matrix;
#  else
typedef struct Matrix Matrix;
#  endif

/**
   Cone interface routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

/**************************************************/
/**** Cone routines (via Normaliz) ****************/
/**************************************************/

const Matrix /* or null */ *rawFourierMotzkin(const Matrix *C);

const Matrix /* or null */ *rawHilbertBasis(const Matrix *C);

const Matrix /* or null */ *rawGVInvariants(M2_arrayint a,
                                            M2_arrayint b,
                                            M2_arrayint c,
                                            M2_arrayint d,
                                            M2_arrayint e,
                                            M2_arrayint f,
                                            M2_arrayint g);
  
#  if defined(__cplusplus)
}
#  endif

#endif /* _cone_h_ */
