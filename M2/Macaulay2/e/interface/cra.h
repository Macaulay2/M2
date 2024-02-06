#ifndef _cra_h_
#  define _cra_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Matrix;
class Ring;
class RingElement;
#  else
typedef struct Matrix Matrix;
typedef struct Ring Ring;
typedef struct RingElement RingElement;
#  endif

/**
   Chinese remainder and rational reconstruction interface routines 
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

const RingElement *rawRingElementCRA(const RingElement *f,
                                     const RingElement *g,
                                     mpz_srcptr m,
                                     mpz_srcptr n);

const Matrix *rawMatrixCRA(const Matrix *f,
                           const Matrix *g,
                           mpz_srcptr m,
                           mpz_srcptr n);

// f should be an element in the polynomial ring R (over ZZ).
// RQ should be the same ring as R, but with rational coefficients
const RingElement *rawRingElementRatConversion(const RingElement *f,
                                               mpz_srcptr m,
                                               const Ring *RQ);

// f should be a matrix in the polynomial ring R (over ZZ).
// RQ should be the same ring as R, but with rational coefficients
const Matrix *rawMatrixRatConversion(const Matrix *f,
                                     mpz_srcptr m,
                                     const Ring *RQ);

#  if defined(__cplusplus)
}
#  endif

#endif /* _cra_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
