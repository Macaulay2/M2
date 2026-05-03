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
// Both A and b are matrices over ZZ; b must be a column matrix with
// n_rows(A) rows and 1 column. All entries of A and b must fit in a
// C int (the underlying box_enum operates on int).
// Returns a MutableMatrix over ZZ with d rows and one column per
// lattice point.
MutableMatrix /* or null */ *rawLatticePoints(const Matrix *A,
                                              const Matrix *b,
                                              int B,
                                              long max_N_out,
                                              long max_N_nodes);

// Enumerate ALL integer vectors x of length d = #cols(A) satisfying
//   A * x <= b  (componentwise),
// using libnormaliz. The polyhedron must be bounded (otherwise an error
// is reported). Both A and b are matrices over ZZ; b must be a column
// matrix with n_rows(A) rows and 1 column. Big-integer entries are
// supported (no fits-in-int restriction).
// Returns a MutableMatrix over ZZ with d rows and one column per
// lattice point.
MutableMatrix /* or null */ *rawLatticePointsNormaliz(const Matrix *A,
                                                      const Matrix *b);


MutableMatrix /* or null */ *rawConeInteriorPoint(const Matrix *A);
  
#  if defined(__cplusplus)
}
#  endif

#endif /* _cone_h_ */
