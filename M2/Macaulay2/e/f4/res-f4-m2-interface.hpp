#ifndef __res_f4_to_m2_interface_hpp_
#define __res_f4_to_m2_interface_hpp_

#include "../engine-includes.hpp"

#include "res-gausser.hpp"
#include "res-f4-types.hpp"

class MonomialInfo;
class ResGausser;
class Polynomial; // vector in a free module

class ResF4toM2Interface
{
public:
  static void poly_set_degrees(const ResGausser *KK,
                               const MonomialInfo *MI,
                               const M2_arrayint wts,
                               const poly &f,
                               int &deg,
                               int &alpha);

  static void from_M2_vec(const ResGausser *KK,
                          const MonomialInfo *MI,
                          const FreeModule *F,
                          vec v,
                          poly &result);

  static vec to_M2_vec(const ResGausser *KK,
                       const MonomialInfo *MI,
                       const poly &f,
                       const FreeModule *F);

#if 0
  static void from_M2_matrix(const ResGausser *KK,
                             const MonomialInfo *MI,
                             const Matrix *m,
                             M2_arrayint wts,
                             gb_array &result_polys);

  static Matrix *to_M2_matrix(const ResGausser *KK,
                              const MonomialInfo *MI,
                              gb_array &polys,
                              const FreeModule *F);

  static MutableMatrix *to_M2_MutableMatrix(const ResGausser *KK,
                                            coefficient_matrix *mat,
                                            gb_array &gens,
                                            gb_array &gb);
#endif
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
