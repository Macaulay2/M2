#ifndef _cone_h_
#  define _cone_h_

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

const Matrix /* or null */ *rawHilbertBasis(const Matrix *C);

#  if defined(__cplusplus)
}
#  endif

#endif /* _cone_h_ */
