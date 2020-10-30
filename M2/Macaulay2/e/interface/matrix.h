#ifndef _matrix_h_
#  define _matrix_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Matrix;
class FreeModule;
#  else
typedef struct Matrix Matrix;
typedef struct FreeModule FreeModule;
#  endif

/**
   Matrix routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

const Matrix /* or null */ *IM2_Matrix_promote(const FreeModule *newTarget,
                                               const Matrix *f);
/* connected to rawPromote*/

const Matrix /* or null */ *IM2_Matrix_lift(int *success_return,
                                            const FreeModule *newTarget,
                                            const Matrix *f);
/* connected to rawLift */
// returns null if lifting not possible

#  if defined(__cplusplus)
}
#  endif

#endif /* _matrix_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
