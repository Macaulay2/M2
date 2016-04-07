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

  static MutableMatrix* to_M2_MutableMatrix(
                                            const Ring* K, // should be a ZZ/p ring.
                                            SchreyerFrame& C,
                                            int lev,
                                            int degree);
  
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
