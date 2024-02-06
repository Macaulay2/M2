#ifndef _ringmap_h_
#  define _ringmap_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class FreeModule;
class Matrix;
class MutableMatrix;
class Ring;
class RingElement;
class RingMap;
#  else
typedef struct FreeModule FreeModule;
typedef struct Matrix Matrix;
typedef struct MutableMatrix MutableMatrix;
typedef struct Ring Ring;
typedef struct RingElement RingElement;
typedef struct RingMap RingMap;
#  endif

/**
   RingMap interface routines

   My plan, Dan, is to make changes to how ring maps are constructed, in the
   case when we have rings over polynomials rings (including galois field bases)
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

const Ring *IM2_RingMap_target(const RingMap *F); /* drg: connected rawTarget*/

M2_string IM2_RingMap_to_string(const RingMap *F); /* drg: connected */

unsigned int rawRingMapHash(const RingMap *F); /* TODO */

M2_bool IM2_RingMap_is_equal(const RingMap *, const RingMap *);
/* drg: connected === */

const RingMap *IM2_RingMap_make(const Matrix *M, const Ring *base); /* TODO */

const RingMap *IM2_RingMap_make1(const Matrix *M);
/* drg: connected rawRingMap */
/* WARNING: I want to change the interface to this routine */

const RingElement /* or null */ *IM2_RingMap_eval_ringelem(
    const RingMap *F,
    const RingElement *a);
/* drg: connected rawRingMapEval*/

const Matrix /* or null */ *IM2_RingMap_eval_matrix(const RingMap *F,
                                                    const FreeModule *newTarget,
                                                    const Matrix *M);
/* drg: connected rawRingMapEval*/

MutableMatrix /* or null */ *rawRingMapEvalMutableMatrix(
    const RingMap *F,
    const MutableMatrix *M);
/* drg: connected rawRingMapEval*/

#  if defined(__cplusplus)
}
#  endif

#endif /* _ringmap_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
