#ifndef __F4toM2interface_h_
#define __F4toM2interface_h_

#include "engine-exports.h"  // for M2_arrayint
#include "f4/f4-types.hpp"   // for gb_array, GBF4Polynomial (ptr only), coefficient_m...
#include "ringelem.hpp"      // for vec

class FreeModule;
class VectorArithmetic;
class Matrix;
class MonomialInfo;
class MutableMatrix;

class F4toM2Interface
{
 public:
  static void poly_set_degrees(const VectorArithmetic* VA,
                               const MonomialInfo *MI,
                               const M2_arrayint wts,
                               const GBF4Polynomial &f,
                               int &deg,
                               int &alpha);

  static void from_M2_vec(const VectorArithmetic* VA,
                          const MonomialInfo *MI,
                          const FreeModule *F,
                          vec v,
                          GBF4Polynomial &result);

  static vec to_M2_vec(const VectorArithmetic* VA,
                       const MonomialInfo *MI,
                       const GBF4Polynomial &f,
                       const FreeModule *F);

  static void from_M2_matrix(const VectorArithmetic* VA,
                             const MonomialInfo *MI,
                             const Matrix *m,
                             M2_arrayint wts,
                             gb_array &result_polys);

  static Matrix *to_M2_matrix(const VectorArithmetic* VA,
                              const MonomialInfo *MI,
                              gb_array &polys,
                              const FreeModule *F);

  static MutableMatrix *to_M2_MutableMatrix(const VectorArithmetic* VA,
                                            coefficient_matrix *mat,
                                            gb_array &gens,
                                            gb_array &gb);
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  indent-tabs-mode: nil
//  End:
