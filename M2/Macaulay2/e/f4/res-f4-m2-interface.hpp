#ifndef __res_f4_to_m2_interface_hpp_
#define __res_f4_to_m2_interface_hpp_

#include "../engine-includes.hpp"

#include "res-schreyer-frame.hpp"
#include "res-gausser.hpp"
#include "res-poly-ring.hpp"

class MonomialInfo;
class ResGausser;
class Polynomial; // vector in a free module

class ResF4toM2Interface
{
public:
  static void poly_set_degrees(const ResPolyRing& R,
                               const M2_arrayint wts,
                               const poly &f,
                               int &deg,
                               int &alpha);

  static void from_M2_vec(const ResPolyRing& R,
                          const FreeModule *F,
                          vec v,
                          poly &result);

  static vec to_M2_vec(const ResPolyRing& R,
                       const poly &f,
                       const FreeModule *F);

  static Matrix *to_M2_matrix(SchreyerFrame& C,
                              int lev,
                              const FreeModule *F);

  static MutableMatrix* to_M2_MutableMatrix(SchreyerFrame& C,
                                            int lev,
                                            int degree);
  
#if 0
  static void from_M2_matrix(const ResPolyRing& R,
                             const Matrix *m,
                             M2_arrayint wts,
                             gb_array &result_polys);

  static Matrix *to_M2_matrix(const ResPolyRing& R,
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
