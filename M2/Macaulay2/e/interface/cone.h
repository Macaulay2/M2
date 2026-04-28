#ifndef M2_INTERFACE_CONE_H_
#define M2_INTERFACE_CONE_H_

#  include "engine-includes.hpp"
#include "m2-types.h"
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

MutableMatrix /* or null */ *rawGVInvariants(M2_arrayint a,
                                            M2_arrayint b,
                                            M2_arrayint c,
                                            M2_arrayint d,
                                            M2_arrayint e,
                                            M2_arrayint f,
                                            M2_arrayint g);

// Enumerate integer vectors x of length d = #cols(A) satisfying
//   A * x <= b  (componentwise) and  |x_i| <= B  for all i.
// Returns a MutableMatrix over ZZ with d rows and one column per
// lattice point.
MutableMatrix /* or null */ *rawLatticePoints(const Matrix *A,
                                              M2_arrayint b,
                                              int B,
                                              long max_N_out,
                                              long max_N_nodes);


MutableMatrix /* or null */ *rawConeInteriorPoint(const Matrix *A);
  
#  if defined(__cplusplus)
}
#  endif

#endif /* _cone_h_ */
